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

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'herdr-sanctum-rpc)
(require 'herdr-sanctum-theme)

(declare-function ghostel-exec "ghostel" (buffer program &rest args))
(declare-function ghostel--adjust-size "ghostel" (window &optional force))
(declare-function ghostel-window-size "ghostel" ())
(defvar ghostel--input-mode)
(defvar ghostel--process)
(defvar ghostel--term-cols)
(defvar ghostel-buffer-name-function)
(defvar ghostel-detect-password-prompts)
(defvar ghostel-window-padding-balance)

(defcustom herdr-sanctum-program "herdr"
  "Executable name or path of the Herdr TUI binary."
  :type 'string
  :group 'herdr-sanctum)

(defcustom herdr-sanctum-program-args nil
  "Optional extra command-line arguments passed to `herdr-sanctum-program'."
  :type '(repeat string)
  :group 'herdr-sanctum)

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
  :group 'herdr-sanctum)

(defcustom herdr-sanctum-terminal-line-spacing 0.12
  "Buffer-local `line-spacing' used in the `*sanctum*' terminal buffer."
  :type '(choice (const :tag "Default" nil) number)
  :group 'herdr-sanctum)

(defcustom herdr-sanctum-open-outside-function nil
  "Function `(FN DIR)' used when opening files or Git views from Sanctum.
When nil, simply call FN with `default-directory' bound to DIR.
Set this to switch tabs or frames before opening code, Magit, or diffs."
  :type '(choice (const :tag "Default (current context)" nil)
                 (function :tag "Custom dispatcher"))
  :group 'herdr-sanctum)

(defcustom herdr-sanctum-ghostel-inhibit-transient-resize nil
  "When non-nil, advise `ghostel--adjust-size' to skip row-only resizes while
a minibuffer, which-key, or transient popup is visible over `*sanctum*'.
Disabled by default so Sanctum does not patch Ghostel internals unless opted in."
  :type 'boolean
  :group 'herdr-sanctum
  :set (lambda (sym val)
         (set-default sym val)
         (when (fboundp 'herdr-sanctum-ghostel--sync-resize-guard)
           (herdr-sanctum-ghostel--sync-resize-guard))))

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

(defun herdr-sanctum--resolve-terminal-backend ()
  "Return the active terminal backend plist or signal a `user-error'."
  (cond
   ((and (listp herdr-sanctum-terminal-backend)
         (keywordp (car herdr-sanctum-terminal-backend)))
    herdr-sanctum-terminal-backend)
   ((symbolp herdr-sanctum-terminal-backend)
    (or (alist-get herdr-sanctum-terminal-backend herdr-sanctum-terminal-backends)
        (user-error "Unknown Sanctum terminal backend: %s"
                    herdr-sanctum-terminal-backend)))
   (t
    (user-error "Invalid `herdr-sanctum-terminal-backend': %S"
                herdr-sanctum-terminal-backend))))

(defun herdr-sanctum-terminal-live-p (&optional buffer)
  "Return non-nil when BUFFER (default `*sanctum*') has a live terminal process."
  (when-let* ((buf (or buffer (get-buffer herdr-sanctum-buffer-name))))
    (when (buffer-live-p buf)
      (let ((live-fn (plist-get (herdr-sanctum--resolve-terminal-backend) :live-p)))
        (and (functionp live-fn) (funcall live-fn buf))))))

(defun herdr-sanctum-in-sanctum-p ()
  "Return non-nil when the selected buffer is the live `*sanctum*' terminal."
  (and (equal (buffer-name) herdr-sanctum-buffer-name)
       (herdr-sanctum-terminal-live-p (current-buffer))))

(defun herdr-sanctum-terminal-start (buffer)
  "Ensure `herdr-sanctum-program' is running inside BUFFER via the active backend."
  (let* ((backend (herdr-sanctum--resolve-terminal-backend))
         (start-fn (plist-get backend :start))
         (setup-fn (plist-get backend :setup))
         (program (or (executable-find herdr-sanctum-program)
                      (user-error "Cannot find %s in exec-path"
                                  herdr-sanctum-program))))
    (unless (herdr-sanctum-terminal-live-p buffer)
      (funcall start-fn buffer program herdr-sanctum-program-args))
    (when (functionp setup-fn)
      (funcall setup-fn buffer))
    buffer))

(defun herdr-sanctum-terminal-resize (&optional window)
  "Notify the active terminal backend to sync PTY dimensions for WINDOW."
  (when-let* ((win (or window
                       (get-buffer-window herdr-sanctum-buffer-name (selected-frame)))))
    (when (window-live-p win)
      (when-let* ((resize-fn (plist-get (herdr-sanctum--resolve-terminal-backend) :resize)))
        (funcall resize-fn win)))))

(defun herdr-sanctum-terminal-mode-banner (&optional buffer)
  "Return an optional header-line mode banner for BUFFER from the active backend."
  (when-let* ((buf (or buffer (current-buffer)))
              (banner-fn (plist-get (herdr-sanctum--resolve-terminal-backend) :mode-banner)))
    (funcall banner-fn buf)))

;;;###autoload
(defun herdr-sanctum-focus-terminal ()
  "Return focus to the `*sanctum*' window on the current frame if visible."
  (interactive)
  (when-let* ((win (get-buffer-window herdr-sanctum-buffer-name (selected-frame))))
    (select-window win)))

(defun herdr-sanctum-open-outside (fn &optional dir)
  "Run FN in DIR (default focused Herdr directory), respecting outside routing."
  (let ((target-dir (or dir (herdr-sanctum-focused-dir) default-directory)))
    (if (functionp herdr-sanctum-open-outside-function)
        (funcall herdr-sanctum-open-outside-function fn target-dir)
      (let ((default-directory target-dir))
        (funcall fn)))))

;;; Built-in Ghostel Backend

(defun herdr-sanctum-ghostel--start (buffer program args)
  "Start PROGRAM with ARGS in BUFFER using `ghostel'."
  (unless (require 'ghostel nil t)
    (user-error "The `ghostel' package is required for the ghostel terminal backend"))
  (if args
      (apply #'ghostel-exec buffer program args)
    (ghostel-exec buffer program)))

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

(defun herdr-sanctum-ghostel--mode-banner (buffer)
  "Return a header-line banner for non-semi-char Ghostel modes in BUFFER."
  (with-current-buffer buffer
    (pcase (bound-and-true-p ghostel--input-mode)
      ('copy
       (propertize " COPY  h/j/k/l · w/x/y copy · / search · RET link · q/i exit "
                   'face 'herdr-sanctum-banner-info))
      ('emacs
       (propertize " EMACS (live)  h/j/k/l · w/x/y copy · / search · RET link · q/i exit "
                   'face 'herdr-sanctum-banner-info))
      ('char
       (propertize " CHAR (raw keys)  M-RET exit "
                   'face 'herdr-sanctum-banner-warn))
      (_ nil))))

(defun herdr-sanctum-ghostel--transient-popup-visible-p (frame)
  "Return non-nil when FRAME shows a minibuffer, which-key, or transient popup."
  (or (and (active-minibuffer-window)
           (eq (window-frame (active-minibuffer-window)) frame))
      (seq-some (lambda (name)
                  (when-let* ((buf (get-buffer name)))
                    (get-buffer-window buf frame)))
                '(" *which-key*" " *transient*"))))

(defun herdr-sanctum-ghostel--inhibit-minibuffer-resize (orig-fn window &optional force)
  "Skip row-only resizes of `*sanctum*' while a transient bottom popup is open."
  (if (and (not force)
           (window-live-p window)
           (equal (buffer-name (window-buffer window)) herdr-sanctum-buffer-name)
           (herdr-sanctum-ghostel--transient-popup-visible-p (window-frame window))
           (with-current-buffer (window-buffer window)
             (when-let* ((size (and (fboundp 'ghostel-window-size)
                                    (ghostel-window-size))))
               (eql (car size) (bound-and-true-p ghostel--term-cols)))))
      nil
    (funcall orig-fn window force)))

(defun herdr-sanctum-ghostel--sync-resize-guard ()
  "Install or remove Ghostel's transient-popup resize guard."
  (if herdr-sanctum-ghostel-inhibit-transient-resize
      (with-eval-after-load 'ghostel
        (advice-add 'ghostel--adjust-size :around
                    #'herdr-sanctum-ghostel--inhibit-minibuffer-resize))
    (when (fboundp 'ghostel--adjust-size)
      (advice-remove 'ghostel--adjust-size
                     #'herdr-sanctum-ghostel--inhibit-minibuffer-resize))))

(herdr-sanctum-register-terminal-backend
 'ghostel
 :start #'herdr-sanctum-ghostel--start
 :live-p #'herdr-sanctum-ghostel--live-p
 :setup #'herdr-sanctum-ghostel--setup
 :resize #'herdr-sanctum-ghostel--resize
 :mode-banner #'herdr-sanctum-ghostel--mode-banner
 :available-p (lambda () (or (featurep 'ghostel) (locate-library "ghostel"))))

(herdr-sanctum-ghostel--sync-resize-guard)

(provide 'herdr-sanctum-terminal)
;;; herdr-sanctum-terminal.el ends here
