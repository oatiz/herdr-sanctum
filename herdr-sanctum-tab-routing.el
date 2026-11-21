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
;;           ("Bench" :modes (eshell-mode proced-mode))
;;           ("Folio" :file t :commands (consult-buffer)
;;                    :modes (magit-mode dired-mode grep-mode rg-mode))))
;;   (herdr-sanctum-tab-routing-mode 1)

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'herdr-sanctum-rpc)

(defcustom herdr-sanctum-tab-routing-rules nil
  "Declarative routing rules for buffers opened from `*sanctum*'.
Each rule has the form `(TAB-NAME . PLIST)' where PLIST supports:
  `:modes'       -- list of major modes (checked with `derived-mode-p')
  `:minor-modes' -- list of minor mode variables to check
  `:file'        -- when non-nil, match any file-visiting buffer
  `:name-regexp' -- regexp matched against `buffer-name'
  `:commands'    -- list of `this-command' symbols (e.g. `consult-buffer')
  `:predicate'   -- function `(BUFFER)' returning non-nil on match.
Rules are evaluated in order; the first matching rule wins."
  :type '(repeat (cons (string :tag "Target tab name")
                       (plist :tag "Match criteria")))
  :group 'herdr-sanctum
  :set (lambda (sym val)
         (set-default sym val)
         (when (bound-and-true-p herdr-sanctum-tab-routing-mode)
           (herdr-sanctum-tab-routing--reinstall))))

(defvar herdr-sanctum-tab-routing--installed-entries nil
  "Entries installed into `display-buffer-alist' by tab routing mode.")

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

(defun herdr-sanctum-tab-routing-match (buffer-or-name)
  "Return target tab name for BUFFER-OR-NAME when opened from `*sanctum*'."
  (when (and (not (active-minibuffer-window))
             (equal (buffer-name (window-buffer)) herdr-sanctum-buffer-name))
    (when-let* ((buffer (get-buffer buffer-or-name)))
      (unless (equal (buffer-name buffer) herdr-sanctum-buffer-name)
        (cl-loop for (tab-name . spec) in herdr-sanctum-tab-routing-rules
                 when (herdr-sanctum-tab-routing--rule-matches-p buffer spec)
                 return tab-name)))))

(defun herdr-sanctum-tab-routing--remove ()
  "Remove previously installed `display-buffer-alist' entries."
  (when herdr-sanctum-tab-routing--installed-entries
    (setq display-buffer-alist
          (seq-remove (lambda (entry)
                        (memq entry herdr-sanctum-tab-routing--installed-entries))
                      display-buffer-alist)
          herdr-sanctum-tab-routing--installed-entries nil)))

(defun herdr-sanctum-tab-routing--reinstall ()
  "Rebuild and prepend `display-buffer-alist' entries for current rules."
  (herdr-sanctum-tab-routing--remove)
  (let ((entries
         (mapcar
          (lambda (rule)
            (let ((target-tab (car rule)))
              (list (lambda (buffer-or-name _action)
                      (equal (herdr-sanctum-tab-routing-match buffer-or-name)
                             target-tab))
                    '(display-buffer-in-tab
                      display-buffer-reuse-window
                      display-buffer-use-some-window)
                    `(tab-name . ,target-tab)
                    '(inhibit-same-window . nil))))
          herdr-sanctum-tab-routing-rules)))
    (setq herdr-sanctum-tab-routing--installed-entries entries
          display-buffer-alist (append entries display-buffer-alist))))

;;;###autoload
(define-minor-mode herdr-sanctum-tab-routing-mode
  "Route buffers opened from `*sanctum*' into designated `tab-bar' tabs."
  :global t
  :group 'herdr-sanctum
  (if herdr-sanctum-tab-routing-mode
      (herdr-sanctum-tab-routing--reinstall)
    (herdr-sanctum-tab-routing--remove)))

(provide 'herdr-sanctum-tab-routing)
;;; herdr-sanctum-tab-routing.el ends here
