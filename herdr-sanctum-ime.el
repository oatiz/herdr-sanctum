;;; herdr-sanctum-ime.el --- Per-pane input method sync for herdr-sanctum -*- lexical-binding: t; -*-

;; Copyright (C) 2026 oatiz

;; Author: oatiz <iamoatiz@gmail.com>
;; URL: https://github.com/oatiz/herdr-sanctum
;; Keywords: i18n, tools, terminals

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

;; Optional global minor mode (`herdr-sanctum-ime-mode') that switches Emacs's
;; input method (such as `rime', `pyim', or any built-in input method) per
;; Herdr pane inside `*sanctum*'.  Agent panes default to IME active while
;; shell panes default to ASCII, manual toggles are remembered per pane, and
;; overrides reset when a pane transitions between agent and shell.

;;; Code:

(require 'subr-x)
(require 'herdr-sanctum-rpc)

(defgroup herdr-sanctum-ime nil
  "Per-pane input method synchronization for `herdr-sanctum'."
  :group 'herdr-sanctum
  :prefix "herdr-sanctum-ime-")

(defcustom herdr-sanctum-ime-input-method nil
  "Input method name managed per Herdr pane.
When nil, use `default-input-method' (for example \"rime\")."
  :type '(choice (const :tag "Use default-input-method" nil)
                 (string :tag "Input method name"))
  :group 'herdr-sanctum-ime)

(defcustom herdr-sanctum-ime-agent-default t
  "When non-nil, enable the input method by default on Herdr agent panes."
  :type 'boolean
  :group 'herdr-sanctum-ime)

(defcustom herdr-sanctum-ime-shell-default nil
  "When non-nil, enable the input method by default on Herdr shell panes."
  :type 'boolean
  :group 'herdr-sanctum-ime)

(defvar herdr-sanctum-ime--panes (make-hash-table :test #'equal)
  "Map `pane_id' -> `(KIND . PREF)' tracked for `*sanctum*' IME sync.
KIND is `agent', `shell', or nil; PREF is `on', `off', or nil.")

(defvar herdr-sanctum-ime--focused-pane-id nil
  "Herdr `pane_id' currently mirrored by `*sanctum*' input method state.")

(defvar herdr-sanctum-ime--syncing nil
  "Non-nil while `herdr-sanctum-ime-sync-pane' is switching input method.")

(defun herdr-sanctum-ime--method ()
  "Return the resolved input method name string, or nil."
  (or herdr-sanctum-ime-input-method default-input-method))

(defun herdr-sanctum-ime--available-p ()
  "Return non-nil when automatic per-pane IME switching should run."
  (let ((method (herdr-sanctum-ime--method)))
    (and (bound-and-true-p herdr-sanctum-ime-mode)
         (stringp method)
         (not (string-empty-p method))
         (assoc method input-method-alist))))

(defun herdr-sanctum-ime--record-pref (pref)
  "Remember manual IME PREF (`on' or `off') for the focused Herdr pane."
  (when (and (not herdr-sanctum-ime--syncing)
             herdr-sanctum-ime--focused-pane-id
             (equal (buffer-name) herdr-sanctum-buffer-name)
             (equal current-input-method (herdr-sanctum-ime--method)))
    (let* ((pane-id herdr-sanctum-ime--focused-pane-id)
           (entry (gethash pane-id herdr-sanctum-ime--panes)))
      (puthash pane-id (cons (car entry) pref) herdr-sanctum-ime--panes))))

(defun herdr-sanctum-ime--record-activate ()
  "Remember manual IME activation for the focused Herdr pane."
  (herdr-sanctum-ime--record-pref 'on))

(defun herdr-sanctum-ime--record-deactivate ()
  "Remember manual IME deactivation for the focused Herdr pane."
  (herdr-sanctum-ime--record-pref 'off))

(defun herdr-sanctum-ime--pane-kind (pane)
  "Return `agent' when PANE identifies an agent, otherwise `shell'.
Checks `agent' first and falls back to `display_agent' when empty."
  (let ((agent (alist-get 'agent pane))
        (display-agent (alist-get 'display_agent pane)))
    (if (or (and (stringp agent) (not (string-empty-p agent)))
            (and (stringp display-agent) (not (string-empty-p display-agent))))
        'agent
      'shell)))

(defun herdr-sanctum-ime--update-pane-kind (pane-id kind)
  "Record KIND (`agent' or `shell') for PANE-ID.
Clear any manual override when the pane transitions between shell and agent."
  (when (stringp pane-id)
    (let* ((entry (gethash pane-id herdr-sanctum-ime--panes))
           (prev-kind (car entry))
           (pref (if (and prev-kind (not (eq prev-kind kind)))
                     nil
                   (cdr entry))))
      (puthash pane-id (cons kind pref) herdr-sanctum-ime--panes))))

(defun herdr-sanctum-ime-sync-pane (pane-id)
  "Sync `*sanctum*' input method to PANE-ID's remembered or default preference."
  (if (not (stringp pane-id))
      (setq herdr-sanctum-ime--focused-pane-id nil)
    (setq herdr-sanctum-ime--focused-pane-id pane-id)
    (when-let* ((buf (get-buffer herdr-sanctum-buffer-name)))
      (with-current-buffer buf
        (when (herdr-sanctum-ime--available-p)
          (let* ((method (herdr-sanctum-ime--method))
                 (entry (gethash pane-id herdr-sanctum-ime--panes))
                 (is-agent (eq (car entry) 'agent))
                 (pref (cdr entry))
                 (want-on (if pref
                              (eq pref 'on)
                            (if is-agent
                                herdr-sanctum-ime-agent-default
                              herdr-sanctum-ime-shell-default)))
                 (is-on (equal current-input-method method)))
            (when (and herdr-sanctum-ime-input-method
                       (not (equal default-input-method method)))
              (setq-local default-input-method method))
            (unless (eq want-on is-on)
              (condition-case err
                  (let ((herdr-sanctum-ime--syncing t))
                    (if want-on
                        (activate-input-method method)
                      (deactivate-input-method)))
                (error
                 (message "Sanctum IME sync failed: %s" (error-message-string err))))
              (force-mode-line-update))))))))

(defun herdr-sanctum-ime--on-snapshot (snapshot)
  "Reconcile tracked pane kinds from SNAPSHOT, prune closed panes, and sync focus."
  (let ((panes (alist-get 'panes snapshot))
        (live (make-hash-table :test #'equal))
        stale)
    (dolist (pane panes)
      (when-let* ((pane-id (alist-get 'pane_id pane)))
        (puthash pane-id t live)
        (herdr-sanctum-ime--update-pane-kind
         pane-id (herdr-sanctum-ime--pane-kind pane))))
    (maphash (lambda (pane-id _v)
               (unless (gethash pane-id live)
                 (push pane-id stale)))
             herdr-sanctum-ime--panes)
    (dolist (pane-id stale)
      (remhash pane-id herdr-sanctum-ime--panes))
    (herdr-sanctum-ime-sync-pane (alist-get 'focused_pane_id snapshot))))

(defun herdr-sanctum-ime--on-event (kind data)
  "Handle Herdr event KIND with DATA for low-latency IME synchronization."
  (pcase kind
    ("pane_focused"
     (when-let* ((pane-id (alist-get 'pane_id data)))
       (herdr-sanctum-ime-sync-pane pane-id)))
    ("pane_created"
     (when-let* ((pane (alist-get 'pane data))
                 (pane-id (alist-get 'pane_id pane)))
       (herdr-sanctum-ime--update-pane-kind
        pane-id (herdr-sanctum-ime--pane-kind pane))))
    ("pane_agent_detected"
     (when-let* ((pane-id (alist-get 'pane_id data)))
       (let* ((agent (alist-get 'agent data))
              (released (alist-get 'released data))
              (kind (if (and (stringp agent)
                             (not (string-empty-p agent))
                             (not released))
                        'agent
                      'shell)))
         (herdr-sanctum-ime--update-pane-kind pane-id kind)
         (when (equal pane-id herdr-sanctum-ime--focused-pane-id)
           (herdr-sanctum-ime-sync-pane pane-id)))))
    ((or "pane_closed" "pane_exited")
     (when-let* ((pane-id (alist-get 'pane_id data)))
       (remhash pane-id herdr-sanctum-ime--panes)
       (when (equal pane-id herdr-sanctum-ime--focused-pane-id)
         (setq herdr-sanctum-ime--focused-pane-id nil))))))

(defun herdr-sanctum-ime--on-terminal-start ()
  "Sync input method into `*sanctum*' after terminal startup or restart."
  (if herdr-sanctum-ime--focused-pane-id
      (herdr-sanctum-ime-sync-pane herdr-sanctum-ime--focused-pane-id)
    (when-let* ((snap (herdr-sanctum-snapshot)))
      (herdr-sanctum-ime--on-snapshot snap))))

(defun herdr-sanctum-ime--clear ()
  "Reset tracked per-pane IME state."
  (clrhash herdr-sanctum-ime--panes)
  (setq herdr-sanctum-ime--focused-pane-id nil))

;;;###autoload
(define-minor-mode herdr-sanctum-ime-mode
  "Toggle automatic per-pane input method synchronization in `*sanctum*'."
  :global t
  :group 'herdr-sanctum-ime
  (if herdr-sanctum-ime-mode
      (progn
        (add-hook 'input-method-activate-hook #'herdr-sanctum-ime--record-activate)
        (add-hook 'input-method-deactivate-hook #'herdr-sanctum-ime--record-deactivate)
        (add-hook 'herdr-sanctum-event-hook #'herdr-sanctum-ime--on-event)
        (add-hook 'herdr-sanctum-snapshot-hook #'herdr-sanctum-ime--on-snapshot)
        (add-hook 'herdr-sanctum-disconnect-hook #'herdr-sanctum-ime--clear)
        (add-hook 'herdr-sanctum-terminal-start-hook #'herdr-sanctum-ime--on-terminal-start)
        (when-let* ((snap (herdr-sanctum-snapshot)))
          (herdr-sanctum-ime--on-snapshot snap)))
    (remove-hook 'input-method-activate-hook #'herdr-sanctum-ime--record-activate)
    (remove-hook 'input-method-deactivate-hook #'herdr-sanctum-ime--record-deactivate)
    (remove-hook 'herdr-sanctum-event-hook #'herdr-sanctum-ime--on-event)
    (remove-hook 'herdr-sanctum-snapshot-hook #'herdr-sanctum-ime--on-snapshot)
    (remove-hook 'herdr-sanctum-disconnect-hook #'herdr-sanctum-ime--clear)
    (remove-hook 'herdr-sanctum-terminal-start-hook #'herdr-sanctum-ime--on-terminal-start)
    (herdr-sanctum-ime--clear)))

(provide 'herdr-sanctum-ime)
;;; herdr-sanctum-ime.el ends here
