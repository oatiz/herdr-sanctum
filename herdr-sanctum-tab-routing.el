;;; herdr-sanctum-tab-routing.el --- Declarative tab routing out of *sanctum* -*- lexical-binding: t; -*-

;; Copyright (C) 2026 oatiz

;; Author: oatiz <iamoatiz@gmail.com>
;; URL: https://github.com/oatiz/herdr-sanctum
;; Keywords: frames, convenience, tools

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

;; Optional global minor mode (`herdr-sanctum-tab-routing-mode') keeping
;; `*sanctum*' dedicated and unsplit by routing buffers opened from it into
;; designated `tab-bar' tabs based on declarative rules.
;;
;; Example:
;;   (setq herdr-sanctum-tab-routing-rules
;;         '(("Atlas" :modes (org-agenda-mode))
;;           ("Bench" :commands (eshell) :modes (proced-mode))
;;           ("Folio" :file t :commands (consult-buffer)
;;                    :modes (magit-mode dired-mode grep-mode rg-mode))))
;;   (herdr-sanctum-tab-routing-mode 1)

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)

(defvar herdr-sanctum-buffer-name "*sanctum*")
(defvar herdr-sanctum-sidebar-buffer-name "*sanctum-spaces*")
(defvar herdr-sanctum-drawer-buffer-name "*sanctum-drawer*")

(defgroup herdr-sanctum-tab-routing nil
  "Declarative `tab-bar' buffer routing out of `herdr-sanctum'."
  :group 'herdr-sanctum
  :prefix "herdr-sanctum-tab-routing-")

(defcustom herdr-sanctum-tab-routing-rules nil
  "Declarative routing rules for buffers opened from Sanctum UI windows.
Each rule has the form `(TAB-NAME . PLIST)' where PLIST supports:
  `:modes'       -- list of major modes (checked with `derived-mode-p')
  `:minor-modes' -- list of minor mode variables to check
  `:file'        -- when non-nil, match any file-visiting buffer
  `:name-regexp' -- regexp matched against `buffer-name'
  `:commands'    -- list of `this-command' symbols (e.g. `consult-buffer')
  `:predicate'   -- function `(BUFFER)' returning non-nil on match.

Criteria within a single rule are combined with logical OR; use `:predicate'
when you need logical AND across multiple conditions.  Rules are evaluated
in order and the first matching rule wins.

Note that commands which display their buffer before setting its major mode
\(such as `eshell') must be matched via `:commands' or `:name-regexp' rather
than `:modes'.  Because `herdr-sanctum-tab-routing-mode' routes via
`display-buffer-in-tab', it relies on `tab-bar' and is intended for
tab-based setups rather than `herdr-sanctum-display-mode' set to `frame'."
  :type '(repeat (cons (string :tag "Target tab name")
                       (plist :tag "Match criteria")))
  :group 'herdr-sanctum-tab-routing)

(defun herdr-sanctum-tab-routing--rule-matches-p (buffer spec)
  "Return non-nil when BUFFER satisfies routing rule plist SPEC."
  (let ((modes (plist-get spec :modes))
        (minor-modes (plist-get spec :minor-modes))
        (match-file (plist-get spec :file))
        (name-re (plist-get spec :name-regexp))
        (commands (plist-get spec :commands))
        (pred (plist-get spec :predicate)))
    (with-current-buffer buffer
      (or (and match-file buffer-file-name)
          (and modes (apply #'derived-mode-p modes))
          (and minor-modes (seq-some (lambda (m) (and (boundp m) (symbol-value m)))
                                     minor-modes))
          (and name-re (string-match-p name-re (buffer-name)))
          (and commands (memq this-command commands))
          (and (functionp pred) (funcall pred buffer))))))

(defvar herdr-sanctum-tab-routing--last-match nil
  "Cached `(BUFFER . TAB-NAME)' from the latest condition check.
Reused by `herdr-sanctum-tab-routing--tab-name' across `display-buffer-in-tab'
and `display-buffer-in-new-tab' to avoid re-evaluating rules and `:predicate'.")

(defun herdr-sanctum-tab-routing--ui-buffer-name-p (name)
  "Return non-nil when NAME is one of Sanctum's UI buffer names."
  (or (equal name herdr-sanctum-buffer-name)
      (equal name herdr-sanctum-sidebar-buffer-name)
      (equal name herdr-sanctum-drawer-buffer-name)))

(defun herdr-sanctum-tab-routing--target-buffer (buffer-or-name)
  "Return buffer for BUFFER-OR-NAME when routing from Sanctum UI applies."
  (when (and (not (active-minibuffer-window))
             (herdr-sanctum-tab-routing--ui-buffer-name-p
              (buffer-name (window-buffer))))
    (when-let* ((buffer (get-buffer buffer-or-name)))
      (unless (herdr-sanctum-tab-routing--ui-buffer-name-p (buffer-name buffer))
        buffer))))

(defun herdr-sanctum-tab-routing--match-rules (buffer)
  "Return target tab name matching BUFFER in `herdr-sanctum-tab-routing-rules'."
  (cl-loop for (tab-name . spec) in herdr-sanctum-tab-routing-rules
           when (herdr-sanctum-tab-routing--rule-matches-p buffer spec)
           return tab-name))

(defun herdr-sanctum-tab-routing-match (buffer-or-name)
  "Return target tab name for BUFFER-OR-NAME when opened from Sanctum UI."
  (when-let* ((buffer (herdr-sanctum-tab-routing--target-buffer buffer-or-name)))
    (herdr-sanctum-tab-routing--match-rules buffer)))

(defun herdr-sanctum-tab-routing--condition (buffer-or-name _action)
  "Return non-nil when BUFFER-OR-NAME matches a Sanctum tab routing rule."
  (if-let* ((buffer (herdr-sanctum-tab-routing--target-buffer buffer-or-name))
            (tab (herdr-sanctum-tab-routing--match-rules buffer)))
      (progn
        (setq herdr-sanctum-tab-routing--last-match (cons buffer tab))
        t)
    (setq herdr-sanctum-tab-routing--last-match nil)))

(defun herdr-sanctum-tab-routing--tab-name (buffer _alist)
  "Return target tab name for BUFFER when invoked by `display-buffer-in-tab'."
  (if (eq (car-safe herdr-sanctum-tab-routing--last-match) buffer)
      (cdr herdr-sanctum-tab-routing--last-match)
    (herdr-sanctum-tab-routing--match-rules buffer)))

(defconst herdr-sanctum-tab-routing--entry
  (list #'herdr-sanctum-tab-routing--condition
        '(display-buffer-in-tab
          display-buffer-reuse-window
          display-buffer-use-some-window)
        (cons 'tab-name #'herdr-sanctum-tab-routing--tab-name)
        '(inhibit-same-window . nil))
  "Static `display-buffer-alist' entry for `herdr-sanctum-tab-routing-mode'.")

(defun herdr-sanctum-tab-routing--remove ()
  "Remove the Sanctum tab-routing entry from `display-buffer-alist'."
  (setq herdr-sanctum-tab-routing--last-match nil
        display-buffer-alist
        (seq-remove (lambda (entry)
                      (eq (car-safe entry) #'herdr-sanctum-tab-routing--condition))
                    display-buffer-alist)))

;;;###autoload
(define-minor-mode herdr-sanctum-tab-routing-mode
  "Route buffers opened from Sanctum UI windows into designated `tab-bar' tabs.
Intended for `tab-bar'-based setups (not `herdr-sanctum-display-mode' set to
`frame')."
  :global t
  :group 'herdr-sanctum-tab-routing
  (herdr-sanctum-tab-routing--remove)
  (when herdr-sanctum-tab-routing-mode
    (push herdr-sanctum-tab-routing--entry display-buffer-alist)))

(provide 'herdr-sanctum-tab-routing)
;;; herdr-sanctum-tab-routing.el ends here
