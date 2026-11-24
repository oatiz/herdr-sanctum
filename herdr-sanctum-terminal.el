;;; herdr-sanctum-terminal.el --- Pluggable terminal backends for herdr-sanctum -*- lexical-binding: t; -*-

;; Copyright (C) 2026 oatiz

;; Author: oatiz <iamoatiz@gmail.com>
;; URL: https://github.com/oatiz/herdr-sanctum
;; Keywords: terminals, tools

;; This file is part of herdr-sanctum.
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Terminal emulator abstraction for hosting the `herdr' TUI inside Emacs.
;; Ships with a `ghostel' backend and allows registering custom backends
;; (for example `eat', `vterm', or user-defined adapters) via
;; `herdr-sanctum-register-terminal-backend'.

;;; Code:

(require 'face-remap)
(require 'subr-x)

(defvar herdr-sanctum-buffer-name "*sanctum*")

(declare-function ghostel-exec "ghostel" (buffer program &rest args))
(declare-function ghostel--adjust-size "ghostel" (window &optional force))
(defvar ghostel--input-mode)
(defvar ghostel--process)
(defvar ghostel-buffer-name-function)
(defvar ghostel-detect-password-prompts)
(defvar ghostel-window-padding-balance)

(defgroup herdr-sanctum-terminal nil
  "Terminal emulator backend settings for `herdr-sanctum'."
  :group 'herdr-sanctum
  :prefix "herdr-sanctum-")

(defcustom herdr-sanctum-program "herdr"
  "Executable name or path of the Herdr TUI binary."
  :type 'string
  :group 'herdr-sanctum-terminal)

(defcustom herdr-sanctum-program-args nil
  "Optional extra command-line arguments passed to `herdr-sanctum-program'."
  :type '(repeat string)
  :group 'herdr-sanctum-terminal)

(defcustom herdr-sanctum-terminal-backend 'ghostel
  "Terminal emulator backend used to host `herdr' inside `*sanctum*'.
May be a registered backend symbol (see `herdr-sanctum-terminal-backends')
or a backend plist directly with keys:
  `:start'        -- `(BUFFER PROGRAM ARGS)' start or attach Herdr in BUFFER
  `:live-p'       -- `(BUFFER)' non-nil when the terminal process is live
  `:resize'       -- `(WINDOW)' optional forced PTY resize after layout change
  `:setup'        -- `(BUFFER)' optional buffer-local tweaks after start
  `:mode-banner'  -- `(BUFFER)' optional header-line banner override string
  `:available-p'  -- `()' optional check used by `herdr-sanctum-doctor'."
  :type '(choice (symbol :tag "Registered backend")
                 (plist :tag "Custom backend plist"))
  :group 'herdr-sanctum-terminal)

(defcustom herdr-sanctum-terminal-line-spacing 0.12
  "Buffer-local `line-spacing' used in the `*sanctum*' terminal buffer."
  :type '(choice (const :tag "Default" nil) number)
  :group 'herdr-sanctum-terminal)

(defcustom herdr-sanctum-terminal-font-height nil
  "Optional `:height' remapping applied to `*sanctum*' terminal body text.
When nil, keep the active terminal mode's default height."
  :type '(choice (const :tag "Default" nil)
                 (integer :tag "Absolute 1/10pt")
                 (float :tag "Relative scale"))
  :group 'herdr-sanctum-terminal
  :set (lambda (sym val)
         (set-default sym val)
         (when (and (featurep 'herdr-sanctum-terminal)
                    (fboundp 'herdr-sanctum-terminal--apply-font-height))
           (when-let* ((buf (get-buffer herdr-sanctum-buffer-name)))
             (herdr-sanctum-terminal--apply-font-height buf)
             (when (and (fboundp 'herdr-sanctum-terminal-live-p)
                        (fboundp 'herdr-sanctum-terminal-resize)
                        (herdr-sanctum-terminal-live-p buf))
               (herdr-sanctum-terminal-resize))))))

(defvar herdr-sanctum-terminal-start-hook nil
  "Hook run inside `*sanctum*' after `herdr-sanctum-terminal-start' finishes.")

(defvar-local herdr-sanctum-terminal--font-cookie nil
  "Face-remapping cookie for `herdr-sanctum-terminal-font-height'.")

(defun herdr-sanctum-terminal--apply-font-height (buffer)
  "Synchronize `default' font-height remapping in terminal BUFFER.
Uses `face-remap-add-relative' and `face-remap-remove-relative' with
`herdr-sanctum-terminal--font-cookie' so pre-existing `default' remappings
(such as `ghostel-default', `buffer-face-mode', or `text-scale-mode') are
preserved when `herdr-sanctum-terminal-font-height' is nil or reset."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when herdr-sanctum-terminal--font-cookie
        (face-remap-remove-relative herdr-sanctum-terminal--font-cookie)
        (setq herdr-sanctum-terminal--font-cookie nil))
      (when herdr-sanctum-terminal-font-height
        (setq herdr-sanctum-terminal--font-cookie
              (face-remap-add-relative 'default
                                       :height herdr-sanctum-terminal-font-height))))))

(defvar herdr-sanctum-terminal-backends nil
  "Alist of `(NAME . PLIST)' registered Sanctum terminal backends.")

(defun herdr-sanctum-register-terminal-backend (name &rest spec)
  "Register terminal backend NAME with plist SPEC.
SPEC keys:
  `:start'       -- required `(BUFFER PROGRAM ARGS)'
  `:live-p'      -- required `(BUFFER)'
  `:resize'      -- optional `(WINDOW)'
  `:setup'       -- optional `(BUFFER)'
  `:mode-banner' -- optional `(BUFFER)'
  `:available-p' -- optional `()'."
  (unless (and (plist-get spec :start) (plist-get spec :live-p))
    (error "Sanctum terminal backend %S requires :start and :live-p" name))
  (setf (alist-get name herdr-sanctum-terminal-backends) spec)
  name)

(defun herdr-sanctum-terminal-resolve-backend (&optional noerror)
  "Return the active terminal backend plist.
Validates that the backend provides `:start' and `:live-p' functions.
Signal a `user-error' on invalid configuration unless NOERROR is non-nil."
  (let ((backend
         (cond
          ((and (listp herdr-sanctum-terminal-backend)
                (keywordp (car-safe herdr-sanctum-terminal-backend)))
           herdr-sanctum-terminal-backend)
          ((symbolp herdr-sanctum-terminal-backend)
           (or (alist-get herdr-sanctum-terminal-backend
                          herdr-sanctum-terminal-backends)
               (unless noerror
                 (user-error "Unknown Sanctum terminal backend: %s"
                             herdr-sanctum-terminal-backend))))
          ((not noerror)
           (user-error "Invalid `herdr-sanctum-terminal-backend': %S"
                       herdr-sanctum-terminal-backend)))))
    (when backend
      (if (and (proper-list-p backend)
               (functionp (plist-get backend :start))
               (functionp (plist-get backend :live-p)))
          backend
        (unless noerror
          (user-error
           "Sanctum terminal backend %S requires `:start' and `:live-p' functions"
           herdr-sanctum-terminal-backend))))))

(defun herdr-sanctum-terminal-live-p (&optional buffer)
  "Return non-nil when BUFFER (default `*sanctum*') has a live terminal process."
  (when-let* ((buf (or buffer (get-buffer herdr-sanctum-buffer-name)))
              ((buffer-live-p buf))
              (backend (herdr-sanctum-terminal-resolve-backend t))
              (live-fn (plist-get backend :live-p)))
    (and (functionp live-fn) (funcall live-fn buf))))

(defun herdr-sanctum-in-sanctum-p ()
  "Return non-nil when the selected buffer is the live `*sanctum*' terminal."
  (and (equal (buffer-name) herdr-sanctum-buffer-name)
       (herdr-sanctum-terminal-live-p (current-buffer))))

(defun herdr-sanctum-terminal-validate-start (&optional buffer)
  "Validate backend and executable before mutating layout for BUFFER.
Return `(BACKEND . PROGRAM)' where PROGRAM is nil when BUFFER already has a
live terminal process."
  (let* ((backend (herdr-sanctum-terminal-resolve-backend))
         (live-fn (plist-get backend :live-p))
         (live (and buffer (buffer-live-p buffer)
                    (functionp live-fn)
                    (funcall live-fn buffer)))
         (program nil))
    (unless live
      (let ((avail-fn (plist-get backend :available-p)))
        (when (and (functionp avail-fn) (not (funcall avail-fn)))
          (user-error "Sanctum terminal backend `%s' is unavailable"
                      herdr-sanctum-terminal-backend)))
      (setq program (or (executable-find herdr-sanctum-program)
                        (user-error "Cannot find %s in exec-path"
                                    herdr-sanctum-program))))
    (cons backend program)))

(defun herdr-sanctum-terminal-start (buffer &optional resolved)
  "Ensure `herdr-sanctum-program' is running inside BUFFER via the active backend.
Optional RESOLVED is a `(BACKEND . PROGRAM)' cons returned by
`herdr-sanctum-terminal-validate-start' to avoid redundant resolution."
  (pcase-let* ((`(,backend . ,program)
                (or resolved (herdr-sanctum-terminal-validate-start buffer)))
               (start-fn (plist-get backend :start))
               (live-fn (plist-get backend :live-p))
               (setup-fn (plist-get backend :setup)))
    (when (and program
               (not (and (functionp live-fn) (funcall live-fn buffer))))
      (funcall start-fn buffer program herdr-sanctum-program-args))
    (when (functionp setup-fn)
      (funcall setup-fn buffer))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (setq-local line-spacing herdr-sanctum-terminal-line-spacing)
        (herdr-sanctum-terminal--apply-font-height buffer)
        (run-hooks 'herdr-sanctum-terminal-start-hook)))
    buffer))

(defun herdr-sanctum-terminal-resize (&optional window)
  "Notify the active terminal backend to sync PTY dimensions for WINDOW."
  (when-let* ((win (or window
                       (get-buffer-window herdr-sanctum-buffer-name (selected-frame))))
              ((window-live-p win))
              (backend (herdr-sanctum-terminal-resolve-backend t))
              (resize-fn (plist-get backend :resize))
              ((functionp resize-fn)))
    (funcall resize-fn win)))

(defun herdr-sanctum-terminal-mode-banner (&optional buffer)
  "Return an optional header-line mode banner for BUFFER from the active backend.
Returns nil gracefully when the terminal backend is invalid or unconfigured."
  (when-let* ((buf (or buffer (current-buffer)))
              (backend (herdr-sanctum-terminal-resolve-backend t))
              (banner-fn (plist-get backend :mode-banner))
              ((functionp banner-fn)))
    (funcall banner-fn buf)))

;;;###autoload
(defun herdr-sanctum-focus-terminal ()
  "Return focus to the `*sanctum*' window on the current frame if visible."
  (interactive)
  (when-let* ((win (get-buffer-window herdr-sanctum-buffer-name (selected-frame))))
    (select-window win)))

;;; Built-in Ghostel Backend

(defun herdr-sanctum-ghostel--start (buffer program args)
  "Start PROGRAM with ARGS in BUFFER using `ghostel'."
  (unless (require 'ghostel nil t)
    (user-error "The `ghostel' package is required for the ghostel terminal backend"))
  (apply #'ghostel-exec buffer program args))

(defun herdr-sanctum-ghostel--live-p (buffer)
  "Return non-nil when BUFFER is a live `ghostel-mode' terminal."
  (with-current-buffer buffer
    (and (derived-mode-p 'ghostel-mode)
         (boundp 'ghostel--process)
         (process-live-p ghostel--process))))

(defun herdr-sanctum-ghostel--setup (buffer)
  "Apply Ghostel-specific buffer-local tuning for Herdr in BUFFER."
  (with-current-buffer buffer
    (setq-local ghostel-buffer-name-function nil
                ghostel-detect-password-prompts nil
                ghostel-window-padding-balance 'top)))

(defun herdr-sanctum-ghostel--resize (window)
  "Force Ghostel to recalculate PTY size for WINDOW."
  (when (fboundp 'ghostel--adjust-size)
    (ghostel--adjust-size window t)))

(defconst herdr-sanctum-ghostel--banner-copy
  (propertize " COPY  h/j/k/l · w/x/y copy · / search · RET link · q/i exit "
              'face 'herdr-sanctum-banner-info)
  "Pre-allocated header-line banner for Ghostel `copy' input mode.")

(defconst herdr-sanctum-ghostel--banner-emacs
  (propertize " EMACS (live)  h/j/k/l · w/x/y copy · / search · RET link · q/i exit "
              'face 'herdr-sanctum-banner-info)
  "Pre-allocated header-line banner for Ghostel `emacs' input mode.")

(defconst herdr-sanctum-ghostel--banner-char
  (propertize " CHAR (raw keys)  M-RET exit "
              'face 'herdr-sanctum-banner-warn)
  "Pre-allocated header-line banner for Ghostel `char' input mode.")

(defun herdr-sanctum-ghostel--mode-banner (buffer)
  "Return a header-line banner for non-semi-char Ghostel modes in BUFFER."
  (with-current-buffer buffer
    (pcase (bound-and-true-p ghostel--input-mode)
      ('copy herdr-sanctum-ghostel--banner-copy)
      ('emacs herdr-sanctum-ghostel--banner-emacs)
      ('char herdr-sanctum-ghostel--banner-char)
      (_ nil))))

(herdr-sanctum-register-terminal-backend
 'ghostel
 :start #'herdr-sanctum-ghostel--start
 :live-p #'herdr-sanctum-ghostel--live-p
 :setup #'herdr-sanctum-ghostel--setup
 :resize #'herdr-sanctum-ghostel--resize
 :mode-banner #'herdr-sanctum-ghostel--mode-banner
 :available-p (lambda () (or (featurep 'ghostel) (locate-library "ghostel"))))

(provide 'herdr-sanctum-terminal)
;;; herdr-sanctum-terminal.el ends here
