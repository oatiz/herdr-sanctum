;;; herdr-sanctum-git.el --- Async Git metadata cache for herdr-sanctum -*- lexical-binding: t; -*-

;; Copyright (C) 2026 oatiz

;; Author: oatiz <iamoatiz@gmail.com>
;; URL: https://github.com/oatiz/herdr-sanctum
;; Keywords: tools, vc

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

;; Non-blocking `git status --porcelain=v2 --branch' and `git diff --numstat'
;; polling with per-directory TTL caching and deduplication for Sanctum panels.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'herdr-sanctum-rpc)

(defcustom herdr-sanctum-git-cache-ttl 5.0
  "Minimum seconds between async `git status' polls for the same directory."
  :type 'number
  :group 'herdr-sanctum)

(defcustom herdr-sanctum-git-update-hook nil
  "Hook run with `(DIR INFO)' whenever async Git metadata updates."
  :type 'hook
  :group 'herdr-sanctum)

(defvar herdr-sanctum-git--cache (make-hash-table :test #'equal)
  "Async Git metadata cache keyed by normalized directory path.")

(defvar herdr-sanctum-git--inflight (make-hash-table :test #'equal)
  "In-flight async `git' processes keyed by normalized directory path.")

(defun herdr-sanctum-git-parse-status (output)
  "Parse `git status --porcelain=v2 --branch' OUTPUT into a status plist."
  (let ((branch nil)
        (detached nil)
        (ahead 0)
        (behind 0)
        (staged 0)
        (unstaged 0)
        (untracked 0)
        (files nil))
    (dolist (line (split-string output "\n" t))
      (cond
       ((string-prefix-p "# branch.head " line)
        (let ((head (substring line 14)))
          (if (equal head "(detached)")
              (setq branch "(detached)" detached t)
            (setq branch head))))
       ((string-prefix-p "# branch.ab " line)
        (when (string-match "\\`# branch\\.ab \\+\\([0-9]+\\) -\\([0-9]+\\)" line)
          (setq ahead (string-to-number (match-string 1 line))
                behind (string-to-number (match-string 2 line)))))
       ((string-prefix-p "1 " line)
        (let* ((xy (substring line 2 4))
               (x (aref xy 0))
               (y (aref xy 1))
               (parts (split-string line " ")))
          (when (/= x ?.) (cl-incf staged))
          (when (/= y ?.) (cl-incf unstaged))
          (when (>= (length parts) 9)
            (push (cons xy (string-join (nthcdr 8 parts) " ")) files))))
       ((string-prefix-p "2 " line)
        (let* ((xy (substring line 2 4))
               (x (aref xy 0))
               (y (aref xy 1))
               (tab-pos (string-search "\t" line))
               (head (if tab-pos (substring line 0 tab-pos) line))
               (parts (split-string head " ")))
          (when (/= x ?.) (cl-incf staged))
          (when (/= y ?.) (cl-incf unstaged))
          (when (>= (length parts) 10)
            (push (cons xy (string-join (nthcdr 9 parts) " ")) files))))
       ((string-prefix-p "u " line)
        (let ((parts (split-string line " ")))
          (cl-incf unstaged)
          (when (>= (length parts) 11)
            (push (cons "UU" (string-join (nthcdr 10 parts) " ")) files))))
       ((string-prefix-p "? " line)
        (cl-incf untracked)
        (push (cons "??" (substring line 2)) files))))
    (let ((ordered (nreverse files)))
      (list :branch (or branch "")
            :detached detached
            :ahead ahead
            :behind behind
            :staged staged
            :unstaged unstaged
            :untracked untracked
            :dirty (length ordered)
            :files (seq-take ordered 200)
            :updated (float-time)))))

(defun herdr-sanctum-git-parse-numstat (output)
  "Sum `(ADDED . REMOVED)' line counts from `git diff --numstat' OUTPUT."
  (let ((added 0)
        (removed 0))
    (dolist (line (split-string output "\n" t))
      (when (string-match "\\`\\([0-9]+\\)\t\\([0-9]+\\)\t" line)
        (cl-incf added (string-to-number (match-string 1 line)))
        (cl-incf removed (string-to-number (match-string 2 line)))))
    (cons added removed)))

(defun herdr-sanctum-git--fetch-diffstat (dir)
  "Asynchronously update `:added' and `:removed' counts for DIR."
  (let ((key (concat "diffstat:" dir)))
    (unless (process-live-p (gethash key herdr-sanctum-git--inflight))
      (let* ((chunks nil)
             (proc (make-process
                    :name "herdr-sanctum-git-diffstat"
                    :buffer nil
                    :command (list "git" "-C" dir "diff" "--numstat" "HEAD")
                    :noquery t
                    :filter (lambda (_p chunk) (push chunk chunks))
                    :sentinel
                    (lambda (p _event)
                      (unless (process-live-p p)
                        (remhash key herdr-sanctum-git--inflight)
                        (when (zerop (process-exit-status p))
                          (let* ((counts (herdr-sanctum-git-parse-numstat
                                          (apply #'concat (nreverse chunks))))
                                 (info (gethash dir herdr-sanctum-git--cache)))
                            (when (and info (not (plist-get info :not-repo)))
                              (let ((updated (plist-put
                                              (plist-put (copy-sequence info)
                                                         :added (car counts))
                                              :removed (cdr counts))))
                                (puthash dir updated herdr-sanctum-git--cache)
                                (run-hook-with-args 'herdr-sanctum-git-update-hook
                                                    dir updated)
                                (force-mode-line-update t))))))))))
        (puthash key proc herdr-sanctum-git--inflight)))))

(defun herdr-sanctum-git-fetch-dir (dir &optional force with-diffstat)
  "Asynchronously poll Git status for DIR into `herdr-sanctum-git--cache'.
When FORCE is non-nil, ignore `herdr-sanctum-git-cache-ttl'.
When WITH-DIFFSTAT is non-nil, also fetch line diff stats for dirty repos."
  (when (and (stringp dir) (file-directory-p dir) (executable-find "git"))
    (let* ((norm (file-name-as-directory (expand-file-name dir)))
           (cached (gethash norm herdr-sanctum-git--cache))
           (age (if cached (- (float-time) (or (plist-get cached :updated) 0.0)) 1e9)))
      (when (and (or force (>= age herdr-sanctum-git-cache-ttl))
                 (not (process-live-p (gethash norm herdr-sanctum-git--inflight))))
        (let* ((chunks nil)
               (proc (make-process
                      :name "herdr-sanctum-git-status"
                      :buffer nil
                      :command (list "git" "-C" norm "status" "--porcelain=v2" "--branch")
                      :noquery t
                      :filter (lambda (_p chunk) (push chunk chunks))
                      :sentinel
                      (lambda (p _event)
                        (unless (process-live-p p)
                          (remhash norm herdr-sanctum-git--inflight)
                          (let ((result
                                 (if (zerop (process-exit-status p))
                                     (let* ((prev (gethash norm herdr-sanctum-git--cache))
                                            (parsed (herdr-sanctum-git-parse-status
                                                     (apply #'concat (nreverse chunks)))))
                                       (if (> (plist-get parsed :dirty) 0)
                                           (progn
                                             (when (and prev (plist-get prev :added))
                                               (setq parsed (plist-put parsed :added (plist-get prev :added)))
                                               (setq parsed (plist-put parsed :removed (plist-get prev :removed))))
                                             (puthash norm parsed herdr-sanctum-git--cache)
                                             (when with-diffstat
                                               (herdr-sanctum-git--fetch-diffstat norm))
                                             parsed)
                                         (setq parsed (plist-put (plist-put parsed :added 0) :removed 0))
                                         (puthash norm parsed herdr-sanctum-git--cache)
                                         parsed))
                                   (let ((non-repo (list :not-repo t :updated (float-time))))
                                     (puthash norm non-repo herdr-sanctum-git--cache)
                                     non-repo))))
                            (run-hook-with-args 'herdr-sanctum-git-update-hook norm result)
                            (force-mode-line-update t)))))))
          (puthash norm proc herdr-sanctum-git--inflight))))))

(defun herdr-sanctum-git-refresh-snapshot (snapshot &optional force)
  "Schedule async Git polls for workspaces and focused pane in SNAPSHOT.
When FORCE is non-nil, bypass TTL throttling."
  (let ((focused-dir (herdr-sanctum-focused-dir snapshot))
        (seen (make-hash-table :test #'equal)))
    (when focused-dir
      (puthash focused-dir t seen)
      (herdr-sanctum-git-fetch-dir focused-dir force t))
    (dolist (ws (alist-get 'workspaces snapshot))
      (when-let* ((dir (herdr-sanctum-workspace-dir ws snapshot)))
        (unless (gethash dir seen)
          (puthash dir t seen)
          (herdr-sanctum-git-fetch-dir dir force nil))))))

(defun herdr-sanctum-git-info-for-dir (dir)
  "Return cached Git status plist for DIR, or nil when absent or non-repo."
  (when (stringp dir)
    (let ((info (gethash (file-name-as-directory (expand-file-name dir))
                         herdr-sanctum-git--cache)))
      (and info (not (plist-get info :not-repo)) info))))

(defun herdr-sanctum-git-clear ()
  "Abort in-flight Git processes and clear cached Git metadata."
  (maphash (lambda (_dir proc)
             (when (process-live-p proc)
               (delete-process proc)))
           herdr-sanctum-git--inflight)
  (clrhash herdr-sanctum-git--inflight))

(add-hook 'herdr-sanctum-snapshot-hook #'herdr-sanctum-git-refresh-snapshot)
(add-hook 'herdr-sanctum-disconnect-hook #'herdr-sanctum-git-clear)

(provide 'herdr-sanctum-git)
;;; herdr-sanctum-git.el ends here
