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

(require 'cl-lib)
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

(defcustom herdr-sanctum-ime-default-function nil
  "Optional function `(PANE SNAPSHOT)' resolving default IME behavior per pane.
Called before falling back to `herdr-sanctum-ime-agent-default' and
`herdr-sanctum-ime-shell-default'.  Supported return values:
  `on' or t      -- enable the configured input method
  `off' or nil   -- disable the input method
  non-empty string -- activate that specific input method for this pane
  `default'      -- fall back to `herdr-sanctum-ime-agent-default' /
                    `herdr-sanctum-ime-shell-default'."
  :type '(choice (const :tag "Use agent/shell defaults" nil)
                 function)
  :group 'herdr-sanctum-ime)

(defcustom herdr-sanctum-ime-activate-function nil
  "Optional external OS IME activation callback called with `(METHOD)'.
When both this and `herdr-sanctum-ime-deactivate-function' are non-nil,
invoke them instead of Emacs's `activate-input-method' and
`deactivate-input-method' (for integrating `sis', `macism', `fcitx5-remote',
and similar OS-level IME controllers)."
  :type '(choice (const :tag "Use Emacs input method" nil)
                 function)
  :group 'herdr-sanctum-ime)

(defcustom herdr-sanctum-ime-deactivate-function nil
  "Optional external OS IME deactivation callback called with no arguments.
Used together with `herdr-sanctum-ime-activate-function'."
  :type '(choice (const :tag "Use Emacs input method" nil)
                 function)
  :group 'herdr-sanctum-ime)

(defcustom herdr-sanctum-ime-active-p-function nil
  "Optional zero-argument predicate returning non-nil when OS IME is active.
When nil and external activate/deactivate callbacks are configured,
`herdr-sanctum-ime--external-active' tracks the last synced boolean state."
  :type '(choice (const :tag "Track synced state" nil)
                 function)
  :group 'herdr-sanctum-ime)

(defcustom herdr-sanctum-ime-change-hook nil
  "Hook run whenever Sanctum IME state changes.
Fired when `herdr-sanctum-ime-mode' toggles, when `herdr-sanctum-ime-sync-pane'
switches state, or when Emacs's input method activate/deactivate hook fires
inside `*sanctum*'."
  :type 'hook
  :group 'herdr-sanctum-ime)

(defvar herdr-sanctum-ime--panes (make-hash-table :test #'equal)
  "Map `pane_id' -> `(KIND . PREF)' tracked for `*sanctum*' IME sync.
KIND is `agent', `shell', or nil; PREF is `on', `off', or nil.")

(defvar herdr-sanctum-ime--pane-data (make-hash-table :test #'equal)
  "Map `pane_id' -> latest Herdr pane alist for default IME resolution.")

(defvar herdr-sanctum-ime--last-snapshot nil
  "Latest Herdr snapshot alist seen by `herdr-sanctum-ime'.")

(defvar herdr-sanctum-ime--focused-pane-id nil
  "Herdr `pane_id' currently mirrored by `*sanctum*' input method state.")

(defvar herdr-sanctum-ime--syncing nil
  "Non-nil while `herdr-sanctum-ime-sync-pane' is switching input method.")

(defvar herdr-sanctum-ime--deactivating nil
  "Non-nil while `input-method-deactivate-hook' is firing in `*sanctum*'.")

(defvar herdr-sanctum-ime--external-active nil
  "Last synced boolean active state for external OS IME callbacks.")

(defvar herdr-sanctum-ime--last-method nil
  "Last input method name activated by `herdr-sanctum-ime'.")

(defun herdr-sanctum-ime--method ()
  "Return the resolved input method name string, or nil."
  (or herdr-sanctum-ime-input-method default-input-method))

(defun herdr-sanctum-ime--external-p ()
  "Return non-nil when external OS IME callbacks are configured."
  (and (functionp herdr-sanctum-ime-activate-function)
       (functionp herdr-sanctum-ime-deactivate-function)))

(defun herdr-sanctum-ime--external-active-p ()
  "Return non-nil when the external OS IME is currently active."
  (if (functionp herdr-sanctum-ime-active-p-function)
      (and (condition-case err
               (funcall herdr-sanctum-ime-active-p-function)
             (error
              (message "Sanctum IME active check failed: %s"
                       (error-message-string err))
              herdr-sanctum-ime--external-active))
           t)
    (and herdr-sanctum-ime--external-active t)))

(defun herdr-sanctum-ime--available-p (&optional method)
  "Return non-nil when automatic per-pane IME switching should run.
When optional METHOD is non-nil, validate it instead of the default method."
  (and (bound-and-true-p herdr-sanctum-ime-mode)
       (or (herdr-sanctum-ime--external-p)
           (let ((m (or method (herdr-sanctum-ime--method))))
             (and (stringp m)
                  (not (string-empty-p m))
                  (assoc m input-method-alist))))))

(defun herdr-sanctum-ime--pane-for-id (pane-id snapshot)
  "Return the Herdr pane alist for PANE-ID from cache or SNAPSHOT."
  (or (gethash pane-id herdr-sanctum-ime--pane-data)
      (cl-find-if (lambda (p) (equal (alist-get 'pane_id p) pane-id))
                  (alist-get 'panes snapshot))))

(defun herdr-sanctum-ime--resolve-default (pane-id entry)
  "Return `(WANT-ON . METHOD)' default IME specification for PANE-ID.
ENTRY is the `(KIND . PREF)' cons stored in `herdr-sanctum-ime--panes'."
  (let* ((global-method (herdr-sanctum-ime--method))
         (is-agent (eq (car entry) 'agent))
         (fallback (cons (and (if is-agent
                                  herdr-sanctum-ime-agent-default
                                herdr-sanctum-ime-shell-default)
                              t)
                         global-method)))
    (if (functionp herdr-sanctum-ime-default-function)
        (let* ((snapshot (or herdr-sanctum-ime--last-snapshot
                             (herdr-sanctum-snapshot)))
               (pane (herdr-sanctum-ime--pane-for-id pane-id snapshot))
               (res (funcall herdr-sanctum-ime-default-function pane snapshot)))
          (cond
           ((eq res 'default) fallback)
           ((or (eq res 'on) (eq res t)) (cons t global-method))
           ((or (eq res 'off) (null res)) (cons nil global-method))
           ((and (stringp res) (not (string-empty-p res))) (cons t res))
           (t fallback)))
      fallback)))

(defun herdr-sanctum-ime-indicator-string (&optional buffer)
  "Return a compact IME status indicator string, or nil when disabled.
When `herdr-sanctum-ime-mode' is enabled, returns `current-input-method-title'
when an Emacs input method is active in BUFFER (defaulting to `*sanctum*'),
\"IME\" when an external OS IME is active, or \"EN\" when inactive."
  (when (bound-and-true-p herdr-sanctum-ime-mode)
    (if (herdr-sanctum-ime--external-p)
        (if (herdr-sanctum-ime--external-active-p) "IME" "EN")
      (let ((buf (if buffer
                     (get-buffer buffer)
                   (or (get-buffer herdr-sanctum-buffer-name)
                       (current-buffer)))))
        (if (and (buffer-live-p buf)
                 (not (and herdr-sanctum-ime--deactivating
                           (eq buf (current-buffer))))
                 (buffer-local-value 'current-input-method buf))
            (let ((title (buffer-local-value 'current-input-method-title buf)))
              (if (and (stringp title) (not (string-empty-p title)))
                  title
                "IME"))
          "EN")))))

(defun herdr-sanctum-ime--record-pref (pref)
  "Remember manual IME PREF (`on' or `off') for the focused Herdr pane."
  (when (and (not herdr-sanctum-ime--syncing)
             (equal (buffer-name) herdr-sanctum-buffer-name))
    (when (and (not (herdr-sanctum-ime--external-p))
               herdr-sanctum-ime--focused-pane-id
               current-input-method)
      (let* ((pane-id herdr-sanctum-ime--focused-pane-id)
             (entry (gethash pane-id herdr-sanctum-ime--panes))
             (pane-method (cdr (herdr-sanctum-ime--resolve-default pane-id entry))))
        (when (or (equal current-input-method pane-method)
                  (equal current-input-method (herdr-sanctum-ime--method))
                  (and herdr-sanctum-ime--last-method
                       (equal current-input-method herdr-sanctum-ime--last-method)))
          (puthash pane-id (cons (car entry) pref) herdr-sanctum-ime--panes)
          (setq herdr-sanctum-ime--last-method
                (when (eq pref 'on) current-input-method)))))
    (let ((herdr-sanctum-ime--deactivating (eq pref 'off)))
      (run-hooks 'herdr-sanctum-ime-change-hook))))

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
  "Sync `*sanctum*' input method to PANE-ID's remembered or default preference.
Return non-nil when input method state switched."
  (if (not (stringp pane-id))
      (progn
        (setq herdr-sanctum-ime--focused-pane-id nil)
        nil)
    (setq herdr-sanctum-ime--focused-pane-id pane-id)
    (when (bound-and-true-p herdr-sanctum-ime-mode)
      (when-let* ((buf (get-buffer herdr-sanctum-buffer-name)))
        (with-current-buffer buf
          (let* ((external (herdr-sanctum-ime--external-p))
                 (entry (gethash pane-id herdr-sanctum-ime--panes))
                 (pref (cdr entry))
                 (default-spec (herdr-sanctum-ime--resolve-default pane-id entry))
                 (target-method (cdr default-spec))
                 (want-on (and (if pref (eq pref 'on) (car default-spec)) t))
                 (changed nil))
            (cond
             (external
              (let* ((is-on (herdr-sanctum-ime--external-active-p))
                     (need-switch
                      (or (not (eq want-on is-on))
                          (and want-on
                               (stringp target-method)
                               (not (string-empty-p target-method))
                               (not (equal target-method
                                           herdr-sanctum-ime--last-method))))))
                (when need-switch
                  (condition-case err
                      (let ((herdr-sanctum-ime--syncing t))
                        (if want-on
                            (funcall herdr-sanctum-ime-activate-function target-method)
                          (funcall herdr-sanctum-ime-deactivate-function))
                        (setq herdr-sanctum-ime--external-active want-on
                              herdr-sanctum-ime--last-method (when want-on target-method)
                              changed t))
                    (error
                     (message "Sanctum IME sync failed: %s"
                              (error-message-string err)))))))
             ((or (herdr-sanctum-ime--available-p target-method)
                  (and (not want-on)
                       current-input-method
                       (or (equal current-input-method herdr-sanctum-ime--last-method)
                           (functionp herdr-sanctum-ime-default-function))))
              (when (and herdr-sanctum-ime-input-method
                         (stringp target-method)
                         (not (string-empty-p target-method))
                         (not (equal default-input-method target-method)))
                (setq-local default-input-method target-method))
              (let ((is-on (if want-on
                               (equal current-input-method target-method)
                             (and current-input-method
                                  (or (equal current-input-method target-method)
                                      (equal current-input-method
                                             herdr-sanctum-ime--last-method)
                                      (functionp herdr-sanctum-ime-default-function))
                                  t))))
                (unless (eq want-on is-on)
                  (condition-case err
                      (let ((herdr-sanctum-ime--syncing t))
                        (if want-on
                            (activate-input-method target-method)
                          (deactivate-input-method))
                        (setq herdr-sanctum-ime--last-method (when want-on target-method)
                              changed t))
                    (error
                     (message "Sanctum IME sync failed: %s"
                              (error-message-string err))))))))
            (when changed
              (force-mode-line-update)
              (run-hooks 'herdr-sanctum-ime-change-hook))
            changed))))))

(defun herdr-sanctum-ime--on-snapshot (snapshot)
  "Reconcile tracked pane kinds from SNAPSHOT, prune closed panes, and sync focus.
Return non-nil when input method state switched."
  (setq herdr-sanctum-ime--last-snapshot snapshot)
  (let ((panes (alist-get 'panes snapshot))
        (live (make-hash-table :test #'equal)))
    (dolist (pane panes)
      (when-let* ((pane-id (alist-get 'pane_id pane)))
        (puthash pane-id t live)
        (puthash pane-id pane herdr-sanctum-ime--pane-data)
        (herdr-sanctum-ime--update-pane-kind
         pane-id (herdr-sanctum-ime--pane-kind pane))))
    (dolist (tbl (list herdr-sanctum-ime--panes herdr-sanctum-ime--pane-data))
      (let (stale)
        (maphash (lambda (pane-id _v)
                   (unless (gethash pane-id live)
                     (push pane-id stale)))
                 tbl)
        (dolist (pane-id stale)
          (remhash pane-id tbl))))
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
       (puthash pane-id pane herdr-sanctum-ime--pane-data)
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
                      'shell))
              (existing (gethash pane-id herdr-sanctum-ime--pane-data))
              (updated (assq-delete-all
                        'display_agent
                        (assq-delete-all 'agent (copy-alist existing)))))
         (unless (alist-get 'pane_id updated)
           (push (cons 'pane_id pane-id) updated))
         (when (eq kind 'agent)
           (push (cons 'agent agent) updated))
         (puthash pane-id updated herdr-sanctum-ime--pane-data)
         (herdr-sanctum-ime--update-pane-kind pane-id kind)
         (when (equal pane-id herdr-sanctum-ime--focused-pane-id)
           (herdr-sanctum-ime-sync-pane pane-id)))))
    ((or "pane_closed" "pane_exited")
     (when-let* ((pane-id (alist-get 'pane_id data)))
       (remhash pane-id herdr-sanctum-ime--panes)
       (remhash pane-id herdr-sanctum-ime--pane-data)
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
  (clrhash herdr-sanctum-ime--pane-data)
  (setq herdr-sanctum-ime--last-snapshot nil
        herdr-sanctum-ime--focused-pane-id nil
        herdr-sanctum-ime--external-active nil
        herdr-sanctum-ime--last-method nil))

;;;###autoload
(define-minor-mode herdr-sanctum-ime-mode
  "Toggle automatic per-pane input method synchronization in `*sanctum*'."
  :global t
  :group 'herdr-sanctum-ime
  (let ((changed nil))
    (if herdr-sanctum-ime-mode
        (progn
          (add-hook 'input-method-activate-hook #'herdr-sanctum-ime--record-activate)
          (add-hook 'input-method-deactivate-hook #'herdr-sanctum-ime--record-deactivate)
          (add-hook 'herdr-sanctum-event-hook #'herdr-sanctum-ime--on-event)
          (add-hook 'herdr-sanctum-snapshot-hook #'herdr-sanctum-ime--on-snapshot)
          (add-hook 'herdr-sanctum-disconnect-hook #'herdr-sanctum-ime--clear)
          (add-hook 'herdr-sanctum-terminal-start-hook #'herdr-sanctum-ime--on-terminal-start)
          (when-let* ((snap (herdr-sanctum-snapshot)))
            (setq changed (herdr-sanctum-ime--on-snapshot snap))))
      (remove-hook 'input-method-activate-hook #'herdr-sanctum-ime--record-activate)
      (remove-hook 'input-method-deactivate-hook #'herdr-sanctum-ime--record-deactivate)
      (remove-hook 'herdr-sanctum-event-hook #'herdr-sanctum-ime--on-event)
      (remove-hook 'herdr-sanctum-snapshot-hook #'herdr-sanctum-ime--on-snapshot)
      (remove-hook 'herdr-sanctum-disconnect-hook #'herdr-sanctum-ime--clear)
      (remove-hook 'herdr-sanctum-terminal-start-hook #'herdr-sanctum-ime--on-terminal-start)
      (herdr-sanctum-ime--clear))
    (unless changed
      (run-hooks 'herdr-sanctum-ime-change-hook))))

(provide 'herdr-sanctum-ime)
;;; herdr-sanctum-ime.el ends here
