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

(declare-function tab-bar--current-tab "tab-bar" (&optional frame))
(declare-function tab-bar--tabs-recent "tab-bar" (&optional frame))

(defvar herdr-sanctum-buffer-name "*sanctum*")
(defvar herdr-sanctum-sidebar-buffer-name "*sanctum-spaces*")
(defvar herdr-sanctum-drawer-buffer-name "*sanctum-drawer*")
(defvar herdr-sanctum-tab-name "Sanctum")

(defgroup herdr-sanctum-tab-routing nil
  "Declarative `tab-bar' buffer routing out of `herdr-sanctum'."
  :group 'herdr-sanctum
  :prefix "herdr-sanctum-tab-routing-")

(defcustom herdr-sanctum-tab-routing-rules nil
  "Declarative routing rules for buffers opened from Sanctum UI windows.
Each rule has the form `(TAB-NAME . PLIST)' where TAB-NAME is either a
static tab name string or a 1-arg function `(BUFFER)' returning a target
tab name string or nil (for example, a project name or Herdr workspace
label).  When a TAB-NAME function returns nil or an empty string,
evaluation continues to the next rule.

PLIST supports:
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
  :type '(repeat (cons (choice (string :tag "Target tab name")
                               (function :tag "Function returning tab name"))
                       (plist :tag "Match criteria")))
  :group 'herdr-sanctum-tab-routing)

(defcustom herdr-sanctum-tab-routing-fallback-tab 'recent
  "Fallback target tab for unmatched buffers opened from the Sanctum tab.
Applies when `herdr-sanctum-tab-routing-mode' is active on the dedicated
Sanctum tab (`herdr-sanctum-tab-name'), no rule in
`herdr-sanctum-tab-routing-rules' matches, and the buffer is not excluded
by `herdr-sanctum-tab-routing-exclude-rules':
  `recent' -- route to the most recent non-Sanctum tab, or create \"Main\"
              when Sanctum is the sole tab
  string   -- route to that named tab
  nil      -- do not route unmatched buffers."
  :type '(choice (const :tag "Most recent non-Sanctum tab" recent)
                 (string :tag "Fixed tab name")
                 (const :tag "Do not route unmatched buffers" nil))
  :group 'herdr-sanctum-tab-routing)

(defcustom herdr-sanctum-tab-routing-exclude-rules
  '("\\` ?\\*\\(?:Completions\\|which-key\\|Embark\\|[Tt]ransient\\|Minibuf\\|Calc\\)")
  "Rules excluding buffers from `herdr-sanctum-tab-routing-fallback-tab'.
Each element may be:
  - a regexp string matched against `buffer-name'
  - a major-mode symbol checked with `derived-mode-p'
  - a 1-arg predicate function `(BUFFER)' returning non-nil to exclude.

Explicit rules in `herdr-sanctum-tab-routing-rules' are checked before
exclusion rules and always take precedence."
  :type '(repeat (choice (regexp :tag "Buffer name regexp")
                         (symbol :tag "Major mode symbol")
                         (function :tag "Predicate (BUFFER)")))
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

(defun herdr-sanctum-tab-routing--on-sanctum-tab-p ()
  "Return non-nil when `tab-bar-mode' is active on the dedicated Sanctum tab."
  (and (bound-and-true-p tab-bar-mode)
       (fboundp 'tab-bar--current-tab)
       (equal (alist-get 'name (tab-bar--current-tab))
              herdr-sanctum-tab-name)))

(defun herdr-sanctum-tab-routing--target-buffer (buffer-or-name)
  "Return buffer for BUFFER-OR-NAME when routing from Sanctum applies."
  (when (and (not (active-minibuffer-window))
             (or (herdr-sanctum-tab-routing--ui-buffer-name-p
                  (buffer-name (window-buffer)))
                 (herdr-sanctum-tab-routing--on-sanctum-tab-p)))
    (when-let* ((buffer (get-buffer buffer-or-name)))
      (unless (herdr-sanctum-tab-routing--ui-buffer-name-p (buffer-name buffer))
        buffer))))

(defun herdr-sanctum-tab-routing--resolve-tab-name (tab-spec buffer)
  "Resolve TAB-SPEC (string or 1-arg function) for BUFFER to a tab name string."
  (let ((resolved (if (functionp tab-spec)
                      (with-current-buffer buffer
                        (funcall tab-spec buffer))
                    tab-spec)))
    (when (and (stringp resolved) (not (string-empty-p resolved)))
      resolved)))

(defun herdr-sanctum-tab-routing--match-rules (buffer)
  "Return target tab name matching BUFFER in `herdr-sanctum-tab-routing-rules'."
  (cl-loop for (tab-spec . spec) in herdr-sanctum-tab-routing-rules
           thereis (and (herdr-sanctum-tab-routing--rule-matches-p buffer spec)
                        (herdr-sanctum-tab-routing--resolve-tab-name
                         tab-spec buffer))))

(defun herdr-sanctum-tab-routing--exclude-rule-matches-p (buffer rule)
  "Return non-nil when BUFFER matches exclude RULE."
  (with-current-buffer buffer
    (cond
     ((stringp rule)
      (string-match-p rule (buffer-name)))
     ((and (not (symbolp rule)) (functionp rule))
      (funcall rule buffer))
     ((symbolp rule)
      (or (derived-mode-p rule)
          (and (functionp rule)
               (not (get rule 'derived-mode-parent))
               (not (string-suffix-p "-mode" (symbol-name rule)))
               (let* ((arity (func-arity rule))
                      (min-args (car arity))
                      (max-args (cdr arity)))
                 (and (<= min-args 1)
                      (or (eq max-args 'many)
                          (and (integerp max-args) (>= max-args 1)))))
               (funcall rule buffer)))))))

(defun herdr-sanctum-tab-routing--excluded-p (buffer)
  "Return non-nil when BUFFER matches `herdr-sanctum-tab-routing-exclude-rules'."
  (cl-some (lambda (rule)
             (herdr-sanctum-tab-routing--exclude-rule-matches-p buffer rule))
           herdr-sanctum-tab-routing-exclude-rules))

(defun herdr-sanctum-tab-routing--recent-non-sanctum-tab ()
  "Return the most recent non-Sanctum tab name, or \"Main\" when none exists."
  (or (when (and (bound-and-true-p tab-bar-mode)
                 (fboundp 'tab-bar--tabs-recent))
        (cl-loop for tab in (tab-bar--tabs-recent)
                 for name = (alist-get 'name tab)
                 when (and (stringp name)
                           (not (string-empty-p name))
                           (not (equal name herdr-sanctum-tab-name)))
                 return name))
      "Main"))

(defun herdr-sanctum-tab-routing--fallback-tab (buffer)
  "Return fallback tab name for unmatched BUFFER on the dedicated Sanctum tab."
  (when (and herdr-sanctum-tab-routing-fallback-tab
             (herdr-sanctum-tab-routing--on-sanctum-tab-p)
             (not (herdr-sanctum-tab-routing--excluded-p buffer)))
    (cond
     ((eq herdr-sanctum-tab-routing-fallback-tab 'recent)
      (herdr-sanctum-tab-routing--recent-non-sanctum-tab))
     ((and (stringp herdr-sanctum-tab-routing-fallback-tab)
           (not (string-empty-p herdr-sanctum-tab-routing-fallback-tab)))
      herdr-sanctum-tab-routing-fallback-tab))))

(defun herdr-sanctum-tab-routing--target-tab (buffer)
  "Return target tab name for BUFFER via explicit rules or fallback."
  (or (herdr-sanctum-tab-routing--match-rules buffer)
      (herdr-sanctum-tab-routing--fallback-tab buffer)))

(defun herdr-sanctum-tab-routing-match (buffer-or-name)
  "Return target tab name for BUFFER-OR-NAME when opened from Sanctum."
  (when-let* ((buffer (herdr-sanctum-tab-routing--target-buffer buffer-or-name)))
    (herdr-sanctum-tab-routing--target-tab buffer)))

(defun herdr-sanctum-tab-routing--condition (buffer-or-name _action)
  "Return non-nil when BUFFER-OR-NAME matches a Sanctum tab routing rule."
  (if-let* ((buffer (herdr-sanctum-tab-routing--target-buffer buffer-or-name))
            (tab (herdr-sanctum-tab-routing--target-tab buffer)))
      (progn
        (setq herdr-sanctum-tab-routing--last-match (cons buffer tab))
        t)
    (setq herdr-sanctum-tab-routing--last-match nil)))

(defun herdr-sanctum-tab-routing--tab-name (buffer _alist)
  "Return target tab name for BUFFER when invoked by `display-buffer-in-tab'."
  (if (eq (car-safe herdr-sanctum-tab-routing--last-match) buffer)
      (cdr herdr-sanctum-tab-routing--last-match)
    (herdr-sanctum-tab-routing--target-tab buffer)))

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
