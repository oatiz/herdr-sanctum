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

;; Non-blocking `git status --porcelain=v2 -z --branch' and
;; `git diff --numstat' polling with per-directory TTL caching, a global
;; focus-prioritized concurrency cap, and deduplication for Sanctum panels.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'herdr-sanctum-rpc)

(defgroup herdr-sanctum-git nil
  "Async Git status and diffstat polling settings for `herdr-sanctum'."
  :group 'herdr-sanctum
  :prefix "herdr-sanctum-git-")

(defvar herdr-sanctum-git--executable nil
  "Cached path to the `git' executable, or nil before a successful lookup.")

(defvar herdr-sanctum-git--executable-program nil
  "Program name that populated `herdr-sanctum-git--executable'.")

(defcustom herdr-sanctum-git-enable t
  "When non-nil, asynchronously poll Git status and diffstat metadata."
  :type 'boolean
  :group 'herdr-sanctum-git)

(defcustom herdr-sanctum-git-program "git"
  "Name or path of the `git' executable used for async polling."
  :type 'string
  :set (lambda (sym val)
         (set-default sym val)
         (setq herdr-sanctum-git--executable nil
               herdr-sanctum-git--executable-program nil))
  :group 'herdr-sanctum-git)

(defcustom herdr-sanctum-git-skip-remote t
  "When non-nil, skip remote (`file-remote-p') directories cleanly."
  :type 'boolean
  :group 'herdr-sanctum-git)

(defcustom herdr-sanctum-git-exclude-dirs nil
  "Directories excluded from async Git polling.
Each element may be:
- a directory prefix string (matches that directory and any subdirectory),
- a cons cell `(regexp . PATTERN)' matched against the normalized path, or
- a predicate function called with the normalized directory path."
  :type '(repeat (choice (directory :tag "Directory prefix")
                         (cons :tag "Regexp" (const regexp) regexp)
                         (function :tag "Predicate")))
  :group 'herdr-sanctum-git)

(defcustom herdr-sanctum-git-diffstat-enable t
  "When non-nil, run secondary `git diff --numstat HEAD' jobs for dirty repos."
  :type 'boolean
  :group 'herdr-sanctum-git)

(defcustom herdr-sanctum-git-status-extra-args nil
  "Extra arguments appended to `git status --porcelain=v2 -z --branch'.
For example, `(\"--untracked-files=no\")' or `(\"--ignore-submodules=dirty\")'."
  :type '(repeat string)
  :group 'herdr-sanctum-git)

(defcustom herdr-sanctum-git-command-timeout 15.0
  "Maximum seconds an async `git' subprocess may run before being killed.
Set to nil to disable command timeouts."
  :type '(choice (number :tag "Seconds")
                 (const :tag "No timeout" nil))
  :group 'herdr-sanctum-git)

(defcustom herdr-sanctum-git-cache-ttl 5.0
  "Minimum seconds between async `git status' polls for the same directory."
  :type 'number
  :group 'herdr-sanctum-git)

(defcustom herdr-sanctum-git-non-repo-ttl 30.0
  "Minimum seconds between non-repo rescans and subrepo discovery walks."
  :type 'number
  :group 'herdr-sanctum-git)

(defcustom herdr-sanctum-git-update-hook nil
  "Hook run with `(DIR INFO)' whenever async Git metadata updates."
  :type 'hook
  :group 'herdr-sanctum-git)

(defcustom herdr-sanctum-git-subrepo-max-depth 1
  "Maximum subdirectory depth to scan for child Git repositories.
Used only when a directory itself is not inside a Git repository.
Set to 0 to disable child repository discovery."
  :type 'natnum
  :group 'herdr-sanctum-git)

(defcustom herdr-sanctum-git-subrepo-max-count 12
  "Maximum number of child Git repositories discovered per directory."
  :type 'natnum
  :group 'herdr-sanctum-git)

(defcustom herdr-sanctum-git-subrepo-exclude-dirs '("~/" "/")
  "Directories where child Git repository discovery is skipped."
  :type '(repeat directory)
  :group 'herdr-sanctum-git)

(defvaralias 'herdr-sanctum-git--subrepo-skip-names
  'herdr-sanctum-git-subrepo-skip-names)

(defcustom herdr-sanctum-git-subrepo-skip-names
  '("node_modules" "vendor" "target" "dist" "build" "__pycache__" ".venv")
  "Directory names skipped during child Git repository discovery."
  :type '(repeat string)
  :group 'herdr-sanctum-git)

(defconst herdr-sanctum-git-subrepo-max-inspected 200
  "Maximum directory entries inspected during a single subrepo discovery walk.")

(defconst herdr-sanctum-git--max-concurrency 4
  "Global maximum number of concurrent async `git' processes.")

(defconst herdr-sanctum-git--cache-max-entries 128
  "Maximum number of directories retained in `herdr-sanctum-git--cache'.")

(defvar herdr-sanctum-git--cache (make-hash-table :test #'equal)
  "Async Git cache keyed by canonical repo root or normalized directory path.
Each value is a cons cell `(INFO . META)', where INFO is a pure Git status
plist (or `(:not-repo t)') and META holds `:updated-at', `:subrepos', and
`:subrepos-at'.")

(defvar herdr-sanctum-git--dir-to-root (make-hash-table :test #'equal)
  "Map from normalized workspace directory to canonical Git repository root.")

(defvar herdr-sanctum-git--truename-to-root (make-hash-table :test #'equal)
  "Map from `file-truename' of a Git repo root to its canonical cache key.")

(defvar herdr-sanctum-git--inflight (make-hash-table :test #'equal)
  "In-flight async `git' processes keyed by job key.")

(defvar herdr-sanctum-git--multi-pending (make-hash-table :test #'equal)
  "In-flight multi-repo aggregation state keyed by normalized parent directory.")

(defvar herdr-sanctum-git--high-queue nil
  "Priority FIFO list of pending async `git' job plists.")

(defvar herdr-sanctum-git--low-queue nil
  "Normal FIFO list of pending async `git' job plists.")

(defvar herdr-sanctum-git--deferred-notify (make-hash-table :test #'equal)
  "Set of normalized parent directories with coalesced `subdiff' updates.")

(defun herdr-sanctum-git--executable ()
  "Return cached path to `herdr-sanctum-git-program', retrying until found."
  (if (and herdr-sanctum-git--executable
           (or (null herdr-sanctum-git--executable-program)
               (equal herdr-sanctum-git--executable-program
                      herdr-sanctum-git-program)))
      herdr-sanctum-git--executable
    (setq herdr-sanctum-git--executable nil
          herdr-sanctum-git--executable-program nil)
    (when-let* ((found (and (stringp herdr-sanctum-git-program)
                            (not (string-empty-p herdr-sanctum-git-program))
                            (executable-find herdr-sanctum-git-program))))
      (setq herdr-sanctum-git--executable found
            herdr-sanctum-git--executable-program herdr-sanctum-git-program)
      found)))

(defun herdr-sanctum-git--status-args (dir)
  "Build `git status' argument vector for DIR with configured extra flags."
  (append (list "-c" "status.relativePaths=false"
                "-C" dir
                "status" "--porcelain=v2" "-z" "--branch")
          herdr-sanctum-git-status-extra-args))

(defun herdr-sanctum-git--remote-dir-p (dir)
  "Return non-nil when DIR denotes a remote directory."
  (or (file-remote-p dir)
      (and (not (file-name-absolute-p dir))
           (stringp default-directory)
           (file-remote-p default-directory))))

(defun herdr-sanctum-git--poll-excluded-p (norm)
  "Return non-nil when NORM matches `herdr-sanctum-git-exclude-dirs'."
  (seq-some
   (lambda (rule)
     (cond
      ((stringp rule)
       (string-prefix-p (file-name-as-directory (expand-file-name rule)) norm))
      ((and (consp rule) (eq (car rule) 'regexp) (stringp (cdr rule)))
       (string-match-p (cdr rule) norm))
      ((functionp rule)
       (funcall rule norm))))
   herdr-sanctum-git-exclude-dirs))

(defun herdr-sanctum-git--find-repo-root (dir)
  "Return canonical Git repository root for local directory DIR, or nil.
Returns nil for remote directories, nonexistent directories, and non-Git
directories (such as multi-repo parent directories).  Supports Git worktrees
\(where `.git' is a regular file) and symlinks into a Git repository."
  (when (and (stringp dir) (not (herdr-sanctum-git--remote-dir-p dir)))
    (let ((norm (file-name-as-directory (expand-file-name dir))))
      (when (file-directory-p norm)
        (let* ((true-norm (or (ignore-errors
                                (file-name-as-directory (file-truename norm)))
                              norm))
               (dom (or (locate-dominating-file norm ".git")
                        (unless (or (equal true-norm norm)
                                    (file-remote-p true-norm))
                          (locate-dominating-file true-norm ".git")))))
          (when dom
            (let* ((cand (file-name-as-directory (expand-file-name dom)))
                   (true-cand (or (ignore-errors
                                    (file-name-as-directory (file-truename cand)))
                                  cand))
                   (existing (gethash true-cand herdr-sanctum-git--truename-to-root)))
              (if (and existing (file-directory-p existing))
                  existing
                (if (file-symlink-p (directory-file-name cand))
                    true-cand
                  cand)))))))))

(defun herdr-sanctum-git--record-dir-root (norm root)
  "Map normalized directory NORM to canonical Git repository ROOT.
Return non-nil when NORM was not previously mapped to ROOT."
  (let ((prev (gethash norm herdr-sanctum-git--dir-to-root)))
    (unless (equal prev root)
      (puthash norm root herdr-sanctum-git--dir-to-root)
      (unless (equal norm root)
        (remhash norm herdr-sanctum-git--cache))
      (when-let* ((true-root (ignore-errors
                               (file-name-as-directory (file-truename root)))))
        (unless (gethash true-root herdr-sanctum-git--truename-to-root)
          (puthash true-root root herdr-sanctum-git--truename-to-root)))
      t)))

(defun herdr-sanctum-git--mapped-dirs-for-root (root)
  "Return list of normalized directories mapped to ROOT in `--dir-to-root'."
  (let ((dirs nil))
    (maphash (lambda (dir mapped)
               (when (equal mapped root)
                 (push dir dirs)))
             herdr-sanctum-git--dir-to-root)
    dirs))

(defun herdr-sanctum-git--remove-root-mappings (root)
  "Remove all `--dir-to-root' and `--truename-to-root' entries targeting ROOT.
Return the list of removed directory keys from `--dir-to-root'."
  (let ((dirs (herdr-sanctum-git--mapped-dirs-for-root root))
        (true-keys nil))
    (dolist (dir dirs)
      (remhash dir herdr-sanctum-git--dir-to-root))
    (maphash (lambda (tk val)
               (when (equal val root)
                 (push tk true-keys)))
             herdr-sanctum-git--truename-to-root)
    (dolist (tk true-keys)
      (remhash tk herdr-sanctum-git--truename-to-root))
    dirs))

(defun herdr-sanctum-git--notify-update (key info)
  "Run `herdr-sanctum-git-update-hook' for KEY or its mapped dirs with INFO."
  (let ((targets (or (herdr-sanctum-git--mapped-dirs-for-root key)
                     (list key))))
    (dolist (dir targets)
      (run-hook-with-args 'herdr-sanctum-git-update-hook dir info))
    (force-mode-line-update t)))

;;; Cache & Metadata Helpers

(defun herdr-sanctum-git--evict-cache ()
  "Trim `herdr-sanctum-git--cache' to `herdr-sanctum-git--cache-max-entries'."
  (let ((overflow (- (hash-table-count herdr-sanctum-git--cache)
                     herdr-sanctum-git--cache-max-entries)))
    (when (> overflow 0)
      (let ((entries nil)
            (evicted (make-hash-table :test #'equal)))
        (maphash (lambda (k entry)
                   (push (cons (or (plist-get (cdr entry) :updated-at) 0.0) k)
                         entries))
                 herdr-sanctum-git--cache)
        (setq entries (sort entries (lambda (a b) (< (car a) (car b)))))
        (dolist (entry (seq-take entries overflow))
          (let ((key (cdr entry)))
            (remhash key herdr-sanctum-git--cache)
            (unless (gethash key herdr-sanctum-git--inflight)
              (puthash key t evicted))))
        (when (> (hash-table-count evicted) 0)
          (let ((stale-dirs nil)
                (stale-trues nil))
            (maphash (lambda (dir root)
                       (when (gethash root evicted)
                         (push dir stale-dirs)))
                     herdr-sanctum-git--dir-to-root)
            (dolist (dir stale-dirs)
              (remhash dir herdr-sanctum-git--dir-to-root))
            (maphash (lambda (tr root)
                       (when (gethash root evicted)
                         (push tr stale-trues)))
                     herdr-sanctum-git--truename-to-root)
            (dolist (tr stale-trues)
              (remhash tr herdr-sanctum-git--truename-to-root))))))))

(defun herdr-sanctum-git--commit-cache (norm info &optional meta defer-notify)
  "Store pure semantic Git state INFO for NORM and update metadata META.
When semantic UI state is unchanged, preserve the existing cached object so
`eq'-based header caches stay valid across TTL renewals.  When DEFER-NOTIFY
is non-nil, record NORM for coalesced notification instead of firing
`herdr-sanctum-git-update-hook' immediately."
  (let* ((prev-entry (gethash norm herdr-sanctum-git--cache))
         (prev (car prev-entry))
         (prev-meta (cdr prev-entry))
         (updated-at (or (plist-get meta :updated-at) (float-time)))
         (subrepos (or (plist-get meta :subrepos)
                       (and (plist-get info :multi-repo)
                            (plist-get prev-meta :subrepos))))
         (subrepos-at (or (plist-get meta :subrepos-at)
                          (and subrepos (plist-get prev-meta :subrepos-at))))
         (same (and prev (equal prev info)))
         (stored (if same prev info))
         (changed (and (not same)
                       (not (and (null prev) (plist-get info :not-repo)))))
         (new-meta (append (list :updated-at updated-at)
                           (when subrepos
                             (list :subrepos subrepos
                                   :subrepos-at (or subrepos-at updated-at))))))
    (puthash norm (cons stored new-meta) herdr-sanctum-git--cache)
    (herdr-sanctum-git--evict-cache)
    (cond
     ((and defer-notify changed)
      (puthash norm t herdr-sanctum-git--deferred-notify))
     ((and (not defer-notify)
           (or changed (gethash norm herdr-sanctum-git--deferred-notify)))
      (remhash norm herdr-sanctum-git--deferred-notify)
      (herdr-sanctum-git--notify-update norm stored)))))

;;; Global Focus-Prioritized Concurrency Scheduler

(defun herdr-sanctum-git--find-queued-job (key)
  "Return pending job plist matching KEY across priority queues, or nil."
  (or (cl-find key herdr-sanctum-git--high-queue
               :key (lambda (j) (plist-get j :key)) :test #'equal)
      (cl-find key herdr-sanctum-git--low-queue
               :key (lambda (j) (plist-get j :key)) :test #'equal)))

(defun herdr-sanctum-git--promote-queued-job (job)
  "Move JOB from the normal queue to the priority queue when present there."
  (when (memq job herdr-sanctum-git--low-queue)
    (setq herdr-sanctum-git--low-queue
          (delq job herdr-sanctum-git--low-queue)
          herdr-sanctum-git--high-queue
          (nconc herdr-sanctum-git--high-queue (list job)))))

(defun herdr-sanctum-git--upgrade-pending-job (key &optional with-diffstat priority)
  "If a job for KEY is already live or queued, upgrade it and return non-nil.
When WITH-DIFFSTAT is non-nil, mark the live process or queued job to request
diffstats.  When PRIORITY is non-nil, promote a queued job to the priority
queue."
  (let ((proc (gethash key herdr-sanctum-git--inflight)))
    (cond
     ((and (processp proc) (process-live-p proc))
      (when with-diffstat
        (process-put proc 'herdr-sanctum-with-diffstat t))
      t)
     ((when-let* ((existing (herdr-sanctum-git--find-queued-job key)))
        (when with-diffstat
          (plist-put existing :with-diffstat t))
        (when priority
          (herdr-sanctum-git--promote-queued-job existing))
        t)))))

(defun herdr-sanctum-git--delete-process (proc)
  "Delete async Git process PROC, its timeout timer, and its stderr pipe."
  (when (processp proc)
    (let ((timer (process-get proc 'herdr-sanctum-git-timer))
          (stderr (process-get proc 'herdr-sanctum-git-stderr)))
      (process-put proc 'herdr-sanctum-git-timer nil)
      (process-put proc 'herdr-sanctum-git-stderr nil)
      (when (timerp timer)
        (cancel-timer timer))
      (when (process-live-p proc)
        (delete-process proc))
      (when (and (processp stderr) (process-live-p stderr))
        (delete-process stderr)))))

(defun herdr-sanctum-git--cancel-inflight-key (key)
  "Cancel and remove any live in-flight process registered under KEY."
  (when-let* ((proc (gethash key herdr-sanctum-git--inflight)))
    (remhash key herdr-sanctum-git--inflight)
    (herdr-sanctum-git--delete-process proc)))

(defun herdr-sanctum-git--start-job (job)
  "Launch async `git' process described by JOB and register it in `--inflight'."
  (let ((guard (plist-get job :guard)))
    (when (or (null guard) (funcall guard))
      (let* ((key (plist-get job :key))
             (norm (plist-get job :norm))
             (default-directory
              (if (and (stringp norm)
                       (not (file-remote-p norm))
                       (file-directory-p norm))
                  norm
                temporary-file-directory))
             (git-bin (herdr-sanctum-git--executable))
             (on-exit (plist-get job :on-exit))
             (on-error (plist-get job :on-error))
             (chunks nil)
             (done nil)
             (stderr-proc nil)
             (proc
              (when git-bin
                (condition-case nil
                    (progn
                      (setq stderr-proc
                            (make-pipe-process :name "herdr-sanctum-git-stderr"
                                               :buffer nil
                                               :noquery t
                                               :filter #'ignore))
                      (make-process
                       :name (plist-get job :name)
                       :buffer nil
                       :command (cons git-bin
                                      (cons "--no-pager"
                                            (cons "--no-optional-locks"
                                                  (plist-get job :args))))
                       :connection-type 'pipe
                       :coding 'utf-8-unix
                       :noquery t
                       :stderr stderr-proc
                       :filter (lambda (_p chunk) (push chunk chunks))
                       :sentinel
                       (lambda (p _event)
                         (unless (process-live-p p)
                           (setq done t)
                           (let ((timer (process-get p 'herdr-sanctum-git-timer)))
                             (process-put p 'herdr-sanctum-git-timer nil)
                             (when (timerp timer)
                               (cancel-timer timer)))
                           (process-put p 'herdr-sanctum-git-stderr nil)
                           (when (and (processp stderr-proc)
                                      (process-live-p stderr-proc))
                             (delete-process stderr-proc))
                           (let ((active (eq (gethash key herdr-sanctum-git--inflight) p)))
                             (when active
                               (remhash key herdr-sanctum-git--inflight))
                             (unwind-protect
                                 (cond
                                  ((and active (eq (process-status p) 'exit))
                                   (funcall on-exit
                                            (process-exit-status p)
                                            (apply #'concat (nreverse chunks))
                                            p
                                            job))
                                  ((and active on-error)
                                   (funcall on-error job)))
                               (herdr-sanctum-git--pump-jobs)))))))
                  (error
                   (when (and (processp stderr-proc) (process-live-p stderr-proc))
                     (delete-process stderr-proc))
                   nil)))))
        (cond
         ((and (processp proc) (not done))
          (process-put proc 'herdr-sanctum-git-stderr stderr-proc)
          (puthash key proc herdr-sanctum-git--inflight)
          (when (and (numberp herdr-sanctum-git-command-timeout)
                     (> herdr-sanctum-git-command-timeout 0))
            (let ((timeout-proc proc))
              (process-put
               timeout-proc 'herdr-sanctum-git-timer
               (run-with-timer
                herdr-sanctum-git-command-timeout nil
                (lambda ()
                  (process-put timeout-proc 'herdr-sanctum-git-timer nil)
                  (when (eq (gethash key herdr-sanctum-git--inflight) timeout-proc)
                    (remhash key herdr-sanctum-git--inflight)
                    (herdr-sanctum-git--delete-process timeout-proc)
                    (unwind-protect
                        (when on-error (funcall on-error job))
                      (herdr-sanctum-git--pump-jobs)))))))))
         ((null proc)
          (when on-error (funcall on-error job))))))))

(defun herdr-sanctum-git--pump-jobs ()
  "Launch queued async `git' jobs up to `herdr-sanctum-git--max-concurrency'."
  (while (and (< (hash-table-count herdr-sanctum-git--inflight)
                 herdr-sanctum-git--max-concurrency)
              (or herdr-sanctum-git--high-queue
                  herdr-sanctum-git--low-queue))
    (when-let* ((job (or (pop herdr-sanctum-git--high-queue)
                         (pop herdr-sanctum-git--low-queue))))
      (unless (process-live-p (gethash (plist-get job :key) herdr-sanctum-git--inflight))
        (herdr-sanctum-git--start-job job)))))

(cl-defun herdr-sanctum-git--enqueue-git
    (&key key norm name args priority with-diffstat guard on-exit on-error)
  "Queue an async `git' job with KEY for NORM, deduplicating live/queued jobs.
NAME is the process name, ARGS are passed after
`git --no-pager --no-optional-locks', PRIORITY
places the job in the high-priority FIFO queue, WITH-DIFFSTAT marks the job
as requesting diffstats, GUARD (if non-nil) suppresses launch when it returns
nil, ON-EXIT is called as `(EXIT-CODE OUTPUT PROC JOB)' on normal exit, and
ON-ERROR is called as `(JOB)' if `make-process' fails."
  (when (herdr-sanctum-git--executable)
    (unless (herdr-sanctum-git--upgrade-pending-job key with-diffstat priority)
      (let ((job (list :key key
                       :norm norm
                       :name name
                       :args args
                       :with-diffstat (and with-diffstat t)
                       :guard guard
                       :on-exit on-exit
                       :on-error on-error)))
        (if priority
            (setq herdr-sanctum-git--high-queue
                  (nconc herdr-sanctum-git--high-queue (list job)))
          (setq herdr-sanctum-git--low-queue
                (nconc herdr-sanctum-git--low-queue (list job))))))
    (herdr-sanctum-git--pump-jobs)))

(defun herdr-sanctum-git--has-pending-subdiff-p (norm)
  "Return non-nil if any `subdiff' job for NORM is in-flight or queued."
  (let ((prefix (concat "subdiff:" norm ":"))
        (found nil))
    (maphash (lambda (key _proc)
               (when (and (not found)
                          (string-prefix-p prefix key))
                 (setq found t)))
             herdr-sanctum-git--inflight)
    (or found
        (cl-some (lambda (job)
                   (string-prefix-p prefix (plist-get job :key)))
                 herdr-sanctum-git--high-queue)
        (cl-some (lambda (job)
                   (string-prefix-p prefix (plist-get job :key)))
                 herdr-sanctum-git--low-queue))))

(defun herdr-sanctum-git--flush-deferred-notify (norm)
  "Fire deferred Git update hook for NORM once no `subdiff' jobs remain."
  (when (and (gethash norm herdr-sanctum-git--deferred-notify)
             (not (herdr-sanctum-git--has-pending-subdiff-p norm)))
    (remhash norm herdr-sanctum-git--deferred-notify)
    (when-let* ((stored (car (gethash norm herdr-sanctum-git--cache))))
      (herdr-sanctum-git--notify-update norm stored))))

(defun herdr-sanctum-git--cancel-dir-jobs (norm)
  "Abort queued and in-flight Git jobs belonging to directory NORM."
  (let ((diffstat-key (concat "diffstat:" norm))
        (substatus-prefix (concat "substatus:" norm ":"))
        (subdiff-prefix (concat "subdiff:" norm ":"))
        (keys nil)
        (procs nil))
    (remhash norm herdr-sanctum-git--multi-pending)
    (remhash norm herdr-sanctum-git--deferred-notify)
    (let ((pred (lambda (job) (equal (plist-get job :norm) norm))))
      (setq herdr-sanctum-git--high-queue
            (cl-remove-if pred herdr-sanctum-git--high-queue)
            herdr-sanctum-git--low-queue
            (cl-remove-if pred herdr-sanctum-git--low-queue)))
    (maphash (lambda (key proc)
               (when (or (equal key norm)
                         (equal key diffstat-key)
                         (string-prefix-p substatus-prefix key)
                         (string-prefix-p subdiff-prefix key))
                 (push key keys)
                 (when (processp proc)
                   (push proc procs))))
             herdr-sanctum-git--inflight)
    (dolist (key keys)
      (remhash key herdr-sanctum-git--inflight))
    (dolist (proc procs)
      (herdr-sanctum-git--delete-process proc))
    (herdr-sanctum-git--pump-jobs)))

;;; Path Resolution & Subrepo Discovery

(defun herdr-sanctum-git-file-path (git rel &optional fallback-dir)
  "Resolve changed-file path REL using GIT `:root' or FALLBACK-DIR.
Return nil when REL or the resolved base directory is missing."
  (let ((base (or (and (listp git) (plist-get git :root))
                  fallback-dir)))
    (when (and (stringp rel) (not (string-empty-p rel))
               (stringp base) (not (string-empty-p base)))
      (expand-file-name rel base))))

(defun herdr-sanctum-git--dir-excluded-p (norm)
  "Return non-nil when normalized directory NORM is excluded from subrepo scan."
  (seq-some (lambda (dir)
              (when (stringp dir)
                (let ((ex (file-name-as-directory (expand-file-name dir))))
                  (or (equal norm ex) (file-equal-p norm ex)))))
            herdr-sanctum-git-subrepo-exclude-dirs))

(defun herdr-sanctum-git--discover-subrepos (dir)
  "Discover child Git repositories beneath DIR via bounded breadth-first search.
Return an alist of `(REL-NAME . ABS-NORM-DIR)' sorted by REL-NAME."
  (when (and (stringp dir)
             (not (and herdr-sanctum-git-skip-remote
                       (herdr-sanctum-git--remote-dir-p dir)))
             (> herdr-sanctum-git-subrepo-max-depth 0)
             (> herdr-sanctum-git-subrepo-max-count 0)
             (> herdr-sanctum-git-subrepo-max-inspected 0))
    (let ((norm (file-name-as-directory (expand-file-name dir))))
      (when (and (file-directory-p norm)
                 (not (herdr-sanctum-git--dir-excluded-p norm)))
        (let ((found nil)
              (found-count 0)
              (inspected 0)
              (current-level (list (cons norm "")))
              (depth 0)
              (max-depth herdr-sanctum-git-subrepo-max-depth)
              (max-count herdr-sanctum-git-subrepo-max-count)
              (max-inspected herdr-sanctum-git-subrepo-max-inspected))
          (while (and current-level
                      (< depth max-depth)
                      (< found-count max-count)
                      (< inspected max-inspected))
            (setq depth (1+ depth))
            (let ((next-level nil))
              (while (and current-level
                          (< found-count max-count)
                          (< inspected max-inspected))
                (let* ((item (pop current-level))
                       (parent-dir (car item))
                       (rel-prefix (cdr item))
                       (entries (condition-case nil
                                    (directory-files parent-dir nil nil nil)
                                  (file-error nil))))
                  (while (and entries
                              (< found-count max-count)
                              (< inspected max-inspected))
                    (let ((name (pop entries)))
                      (unless (or (string-prefix-p "." name)
                                  (member name herdr-sanctum-git-subrepo-skip-names))
                        (cl-incf inspected)
                        (let ((child (expand-file-name name parent-dir)))
                          (when (eq (car (file-attributes child 'integer)) t)
                            (let ((child-norm (file-name-as-directory child))
                                  (rel (if (string-empty-p rel-prefix)
                                           name
                                         (concat rel-prefix "/" name))))
                              (if (file-exists-p (expand-file-name ".git" child-norm))
                                  (progn
                                    (push (cons rel child-norm) found)
                                    (cl-incf found-count))
                                (when (< depth max-depth)
                                  (push (cons child-norm rel) next-level)))))))))))
              (setq current-level (nreverse next-level))))
          (sort found (lambda (a b) (string< (car a) (car b)))))))))

;;; Parsers

(defun herdr-sanctum-git--field-tail (record spaces)
  "Return verbatim tail of RECORD after SPACES single-space delimiters."
  (let ((pos 0)
        (rem spaces)
        (len (length record)))
    (while (and (> rem 0) (< pos len))
      (let ((next (string-search " " record pos)))
        (if next
            (setq pos (1+ next)
                  rem (1- rem))
          (setq pos len))))
    (when (and (= rem 0) (< pos len))
      (substring record pos))))

(defun herdr-sanctum-git-parse-status (output)
  "Parse `git status --porcelain=v2 [-z] --branch' OUTPUT into a status plist."
  (let* ((nul-mode (not (null (string-search "\0" output))))
         (records (split-string output (if nul-mode "\0" "\n") t))
         (branch nil)
         (detached nil)
         (ahead 0)
         (behind 0)
         (staged 0)
         (unstaged 0)
         (untracked 0)
         (files nil))
    (while records
      (let ((line (pop records)))
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
                 (path (herdr-sanctum-git--field-tail line 8)))
            (when (/= x ?.) (cl-incf staged))
            (when (/= y ?.) (cl-incf unstaged))
            (when path
              (push (cons xy path) files))))
         ((string-prefix-p "2 " line)
          (let* ((xy (substring line 2 4))
                 (x (aref xy 0))
                 (y (aref xy 1))
                 (head (if nul-mode
                           line
                         (if-let* ((tab-pos (string-search "\t" line)))
                             (substring line 0 tab-pos)
                           line)))
                 (path (herdr-sanctum-git--field-tail head 9)))
            (when nul-mode
              ;; In `-z' mode, rename/copy entries are followed by origPath\0.
              (pop records))
            (when (/= x ?.) (cl-incf staged))
            (when (/= y ?.) (cl-incf unstaged))
            (when path
              (push (cons xy path) files))))
         ((string-prefix-p "u " line)
          (let ((xy (and (>= (length line) 4) (substring line 2 4)))
                (path (herdr-sanctum-git--field-tail line 10)))
            (cl-incf unstaged)
            (when (and xy path)
              (push (cons xy path) files))))
         ((string-prefix-p "? " line)
          (cl-incf untracked)
          (push (cons "??" (substring line 2)) files)))))
    (let ((ordered (nreverse files)))
      (list :branch (or branch "")
            :detached detached
            :ahead ahead
            :behind behind
            :staged staged
            :unstaged unstaged
            :untracked untracked
            :dirty (length ordered)
            :files (seq-take ordered 200)))))

(defun herdr-sanctum-git-parse-numstat (output)
  "Sum `(ADDED . REMOVED)' line counts from `git diff --numstat' OUTPUT."
  (let ((added 0)
        (removed 0))
    (dolist (line (split-string output "\n" t))
      (when (string-match "\\`\\([0-9]+\\)\t\\([0-9]+\\)\t" line)
        (cl-incf added (string-to-number (match-string 1 line)))
        (cl-incf removed (string-to-number (match-string 2 line)))))
    (cons added removed)))

(defun herdr-sanctum-git--aggregate-multi-repo (norm repos)
  "Aggregate per-subrepo plists REPOS into a multi-repo status plist for NORM."
  (let ((ahead 0)
        (behind 0)
        (staged 0)
        (unstaged 0)
        (untracked 0)
        (dirty 0)
        (added-sum 0)
        (removed-sum 0)
        (has-added nil)
        (has-removed nil)
        (files nil)
        (file-count 0))
    (dolist (repo repos)
      (cl-incf ahead (or (plist-get repo :ahead) 0))
      (cl-incf behind (or (plist-get repo :behind) 0))
      (cl-incf staged (or (plist-get repo :staged) 0))
      (cl-incf unstaged (or (plist-get repo :unstaged) 0))
      (cl-incf untracked (or (plist-get repo :untracked) 0))
      (cl-incf dirty (or (plist-get repo :dirty) 0))
      (when (integerp (plist-get repo :added))
        (setq has-added t)
        (cl-incf added-sum (plist-get repo :added)))
      (when (integerp (plist-get repo :removed))
        (setq has-removed t)
        (cl-incf removed-sum (plist-get repo :removed)))
      (let ((prefix (concat (plist-get repo :name) "/")))
        (dolist (entry (plist-get repo :files))
          (when (< file-count 200)
            (push (cons (car entry) (concat prefix (cdr entry))) files)
            (cl-incf file-count)))))
    (let ((n (length repos)))
      (list :multi-repo t
            :root norm
            :branch (format "%d repo%s" n (if (= n 1) "" "s"))
            :detached nil
            :ahead ahead
            :behind behind
            :staged staged
            :unstaged unstaged
            :untracked untracked
            :dirty dirty
            :added (cond (has-added added-sum)
                         ((= dirty 0) 0)
                         (t nil))
            :removed (cond (has-removed removed-sum)
                           ((= dirty 0) 0)
                           (t nil))
            :files (nreverse files)
            :repos repos))))

;;; Async Diffstat & Multi-Repo Workers

(defun herdr-sanctum-git--fetch-diffstat (dir)
  "Asynchronously update `:added' and `:removed' counts for DIR."
  (when herdr-sanctum-git-diffstat-enable
    (herdr-sanctum-git--enqueue-git
     :key (concat "diffstat:" dir)
     :norm dir
     :name "herdr-sanctum-git-diffstat"
     :args (list "-C" dir "diff" "--numstat" "HEAD")
     :priority t
     :on-exit
     (lambda (exit-code output _proc _job)
       (let* ((ok (zerop exit-code))
              (counts (when ok (herdr-sanctum-git-parse-numstat output)))
              (info (car (gethash dir herdr-sanctum-git--cache))))
         (when (and info
                    (not (plist-get info :not-repo))
                    (not (plist-get info :multi-repo))
                    (> (or (plist-get info :dirty) 0) 0))
           (let* ((added (if ok (car counts) (or (plist-get info :added) 0)))
                  (removed (if ok (cdr counts) (or (plist-get info :removed) 0)))
                  (updated (plist-put (plist-put (copy-sequence info) :added added)
                                      :removed removed)))
             (herdr-sanctum-git--commit-cache dir updated))))))))

(defun herdr-sanctum-git--fetch-subrepo-diffstat (norm sub-dir)
  "Queue an async `:added'/`:removed' update for SUB-DIR in multi-repo NORM."
  (when herdr-sanctum-git-diffstat-enable
    (herdr-sanctum-git--enqueue-git
     :key (concat "subdiff:" norm ":" sub-dir)
     :norm norm
     :name "herdr-sanctum-git-subdiff"
     :args (list "-C" sub-dir "diff" "--numstat" "HEAD")
     :priority t
     :on-error (lambda (_job) (herdr-sanctum-git--flush-deferred-notify norm))
     :on-exit
     (lambda (exit-code output _proc _job)
       (let* ((info (car (gethash norm herdr-sanctum-git--cache)))
              (ok (zerop exit-code))
              (counts (when ok (herdr-sanctum-git-parse-numstat output)))
              (changed nil))
         (when (and info (plist-get info :multi-repo))
           (let ((updated-repos
                  (mapcar
                   (lambda (repo)
                     (if (and (equal (plist-get repo :dir) sub-dir)
                              (> (or (plist-get repo :dirty) 0) 0))
                         (let* ((copy (copy-sequence repo))
                                (added (if ok
                                           (car counts)
                                         (or (plist-get copy :added) 0)))
                                (removed (if ok
                                             (cdr counts)
                                           (or (plist-get copy :removed) 0))))
                           (setq changed t)
                           (plist-put (plist-put copy :added added)
                                      :removed removed))
                       repo))
                   (plist-get info :repos))))
             (when changed
               (herdr-sanctum-git--commit-cache
                norm
                (herdr-sanctum-git--aggregate-multi-repo norm updated-repos)
                nil
                (herdr-sanctum-git--has-pending-subdiff-p norm)))))
         (unless changed
           (herdr-sanctum-git--flush-deferred-notify norm)))))))

(defun herdr-sanctum-git--finish-multi-status (norm batch)
  "Commit completed multi-repo status BATCH for NORM and kick off diffstats."
  (let ((results (plist-get batch :results))
        (repos nil))
    (dolist (sub (plist-get batch :subrepos))
      (when-let* ((entry (gethash (cdr sub) results)))
        (push entry repos)))
    (setq repos (nreverse repos))
    (if (null repos)
        (herdr-sanctum-git--commit-cache norm (list :not-repo t))
      (let ((agg (herdr-sanctum-git--aggregate-multi-repo norm repos))
            (meta (list :subrepos (plist-get batch :subrepos)
                        :subrepos-at (plist-get batch :subrepos-at))))
        (herdr-sanctum-git--commit-cache norm agg meta)
        (when (and (plist-get batch :with-diffstat)
                   herdr-sanctum-git-diffstat-enable)
          (dolist (repo repos)
            (when (> (plist-get repo :dirty) 0)
              (let ((sub-dir (plist-get repo :dir)))
                (herdr-sanctum-git--cancel-inflight-key
                 (concat "subdiff:" norm ":" sub-dir))
                (herdr-sanctum-git--fetch-subrepo-diffstat norm sub-dir)))))))))

(defun herdr-sanctum-git--complete-multi-item (norm batch)
  "Decrement remaining count for multi-repo BATCH on NORM and finalize if done."
  (when (eq (gethash norm herdr-sanctum-git--multi-pending) batch)
    (let ((rem (1- (plist-get batch :remaining))))
      (plist-put batch :remaining rem)
      (when (<= rem 0)
        (remhash norm herdr-sanctum-git--multi-pending)
        (herdr-sanctum-git--finish-multi-status norm batch)))))

(defun herdr-sanctum-git--fetch-multi-repo (norm subrepos with-diffstat &optional subrepos-at)
  "Poll SUBREPOS through the global concurrency scheduler for parent NORM.
When WITH-DIFFSTAT is non-nil, prioritize jobs and fetch diffstats for dirty
subrepos."
  (when (and (not (gethash norm herdr-sanctum-git--multi-pending))
             (herdr-sanctum-git--executable))
    (let* ((prev (car (gethash norm herdr-sanctum-git--cache)))
           (prev-by-dir (make-hash-table :test #'equal))
           (batch (list :remaining (length subrepos)
                        :prev-by-dir prev-by-dir
                        :results (make-hash-table :test #'equal)
                        :subrepos subrepos
                        :subrepos-at (or subrepos-at (float-time))
                        :with-diffstat with-diffstat)))
      (when (and prev (plist-get prev :multi-repo))
        (dolist (repo (plist-get prev :repos))
          (when-let* ((sub-dir (plist-get repo :dir)))
            (puthash sub-dir repo prev-by-dir))))
      (puthash norm batch herdr-sanctum-git--multi-pending)
      (if (null subrepos)
          (progn
            (remhash norm herdr-sanctum-git--multi-pending)
            (herdr-sanctum-git--finish-multi-status norm batch))
        (dolist (sub subrepos)
          (let ((rel-name (car sub))
                (sub-dir (cdr sub)))
            (herdr-sanctum-git--enqueue-git
             :key (concat "substatus:" norm ":" sub-dir)
             :norm norm
             :name "herdr-sanctum-git-substatus"
             :args (herdr-sanctum-git--status-args sub-dir)
             :priority with-diffstat
             :guard (lambda () (eq (gethash norm herdr-sanctum-git--multi-pending) batch))
             :on-error (lambda (_job) (herdr-sanctum-git--complete-multi-item norm batch))
             :on-exit
             (lambda (exit-code output _proc _job)
               (when (eq (gethash norm herdr-sanctum-git--multi-pending) batch)
                 (when (zerop exit-code)
                   (let* ((parsed (herdr-sanctum-git-parse-status output))
                          (dirty (plist-get parsed :dirty))
                          (prev-sub (gethash sub-dir prev-by-dir))
                          (carry-diff (and prev-sub
                                           (> (or (plist-get prev-sub :dirty) 0) 0)
                                           (integerp (plist-get prev-sub :added))
                                           (equal (plist-get prev-sub :files)
                                                  (plist-get parsed :files))))
                          (entry (append (list :name rel-name :dir sub-dir :root sub-dir)
                                         parsed
                                         (list :added (cond ((= dirty 0) 0)
                                                            (carry-diff (plist-get prev-sub :added)))
                                               :removed (cond ((= dirty 0) 0)
                                                              (carry-diff (plist-get prev-sub :removed)))))))
                     (puthash sub-dir entry (plist-get batch :results))))
                 (herdr-sanctum-git--complete-multi-item norm batch))))))))))

;;; Public Entry Points

(defun herdr-sanctum-git--evict-deleted-dir (norm)
  "Evict cache, mappings, and jobs for deleted normalized directory NORM."
  (let ((mapped-root (gethash norm herdr-sanctum-git--dir-to-root)))
    (cond
     ;; Deleted subdir of a still-existing repository: detach only this subdir.
     ((and mapped-root
           (not (equal mapped-root norm))
           (file-directory-p mapped-root))
      (remhash norm herdr-sanctum-git--dir-to-root)
      (when (gethash mapped-root herdr-sanctum-git--cache)
        (run-hook-with-args 'herdr-sanctum-git-update-hook norm nil)
        (force-mode-line-update t)))
     (t
      (let* ((target (or mapped-root norm))
             (had-entry (or (gethash target herdr-sanctum-git--cache)
                            (and (not (equal target norm))
                                 (gethash norm herdr-sanctum-git--cache))))
             (affected (herdr-sanctum-git--remove-root-mappings target)))
        (herdr-sanctum-git--cancel-dir-jobs target)
        (unless (equal target norm)
          (herdr-sanctum-git--cancel-dir-jobs norm)
          (remhash norm herdr-sanctum-git--cache))
        (remhash target herdr-sanctum-git--cache)
        (when had-entry
          (dolist (d (or affected (list norm)))
            (run-hook-with-args 'herdr-sanctum-git-update-hook d nil))
          (force-mode-line-update t)))))))

(defun herdr-sanctum-git--handle-status-exit (norm force exit-code output proc job)
  "Handle completion of an async `git status' process for NORM."
  (let* ((requested-diffstat
          (or (plist-get job :with-diffstat)
              (process-get proc 'herdr-sanctum-with-diffstat)))
         (want-diffstat (and herdr-sanctum-git-diffstat-enable
                             requested-diffstat))
         (prev (car (gethash norm herdr-sanctum-git--cache))))
    (if (zerop exit-code)
        (let* ((root (or (and (not force)
                              prev
                              (not (plist-get prev :multi-repo))
                              (plist-get prev :root))
                         (herdr-sanctum-git--find-repo-root norm)
                         norm))
               (parsed (plist-put (herdr-sanctum-git-parse-status output)
                                  :root root)))
          (if (> (plist-get parsed :dirty) 0)
              (progn
                (when (and prev
                           (not (plist-get prev :multi-repo))
                           (> (or (plist-get prev :dirty) 0) 0)
                           (integerp (plist-get prev :added))
                           (equal (plist-get prev :files)
                                  (plist-get parsed :files)))
                  (setq parsed (plist-put parsed :added (plist-get prev :added)))
                  (setq parsed (plist-put parsed :removed (plist-get prev :removed))))
                (herdr-sanctum-git--commit-cache norm parsed)
                (when want-diffstat
                  (herdr-sanctum-git--cancel-inflight-key
                   (concat "diffstat:" norm))
                  (herdr-sanctum-git--fetch-diffstat norm)))
            (herdr-sanctum-git--cancel-inflight-key (concat "diffstat:" norm))
            (setq parsed (plist-put (plist-put parsed :added 0) :removed 0))
            (herdr-sanctum-git--commit-cache norm parsed)))
      (let ((subrepos (herdr-sanctum-git--discover-subrepos norm)))
        (if subrepos
            (progn
              (herdr-sanctum-git--remove-root-mappings norm)
              (herdr-sanctum-git--fetch-multi-repo
               norm subrepos requested-diffstat (float-time)))
          (herdr-sanctum-git--commit-cache norm (list :not-repo t))
          (herdr-sanctum-git--remove-root-mappings norm))))))

(defun herdr-sanctum-git--cache-fresh-p (key now)
  "Return non-nil when KEY has a cached entry in `--cache' within its TTL at NOW."
  (when-let* ((entry (gethash key herdr-sanctum-git--cache)))
    (let* ((cached (car entry))
           (meta (cdr entry))
           (ttl (if (and cached (plist-get cached :not-repo))
                    herdr-sanctum-git-non-repo-ttl
                  herdr-sanctum-git-cache-ttl)))
      (< (- now (or (plist-get meta :updated-at) 0.0)) ttl))))

(defun herdr-sanctum-git-fetch-dir (dir &optional force with-diffstat)
  "Asynchronously poll Git status for DIR into `herdr-sanctum-git--cache'.
When FORCE is non-nil, ignore `herdr-sanctum-git-cache-ttl' and cancel any
in-flight poll for DIR before starting a fresh fetch.
When WITH-DIFFSTAT is non-nil, prioritize the job and fetch line diff stats
for dirty repos when `herdr-sanctum-git-diffstat-enable' is non-nil."
  (when (and herdr-sanctum-git-enable
             (stringp dir)
             (not (and herdr-sanctum-git-skip-remote
                       (herdr-sanctum-git--remote-dir-p dir))))
    (let ((norm (file-name-as-directory (expand-file-name dir))))
      (unless (herdr-sanctum-git--poll-excluded-p norm)
        (if (not (file-directory-p norm))
            (herdr-sanctum-git--evict-deleted-dir norm)
          (let* ((now (float-time))
                 (mapped-root (gethash norm herdr-sanctum-git--dir-to-root))
                 (root
                  (cond
                   (force
                    (let ((found (herdr-sanctum-git--find-repo-root norm)))
                      (when (and mapped-root (null found))
                        (remhash norm herdr-sanctum-git--dir-to-root))
                      found))
                   ((and mapped-root
                         (herdr-sanctum-git--cache-fresh-p mapped-root now))
                    mapped-root)
                   ((and (null mapped-root)
                         (or (gethash norm herdr-sanctum-git--multi-pending)
                             (herdr-sanctum-git--cache-fresh-p norm now)))
                    nil)
                   (t
                    (let ((found (herdr-sanctum-git--find-repo-root norm)))
                      (when (and mapped-root (null found))
                        (remhash norm herdr-sanctum-git--dir-to-root))
                      found)))))
            (unless (and root
                         (not (equal root norm))
                         (herdr-sanctum-git--poll-excluded-p root))
              (let* ((newly-mapped
                      (when root
                        (herdr-sanctum-git--record-dir-root norm root)))
                     (target (or root norm))
                     (entry (gethash target herdr-sanctum-git--cache))
                     (cached (car entry))
                     (cached-meta (cdr entry))
                     (ttl (if (and cached (plist-get cached :not-repo))
                              herdr-sanctum-git-non-repo-ttl
                            herdr-sanctum-git-cache-ttl))
                     (age (if cached
                              (- now (or (plist-get cached-meta :updated-at) 0.0))
                            1e9)))
                (when force
                  (herdr-sanctum-git--cancel-dir-jobs target))
                (cond
                 ((when-let* ((pending-multi (gethash target herdr-sanctum-git--multi-pending)))
                    (when (and with-diffstat herdr-sanctum-git-diffstat-enable)
                      (puthash target (plist-put pending-multi :with-diffstat t)
                               herdr-sanctum-git--multi-pending))
                    t))
                 ((herdr-sanctum-git--upgrade-pending-job target with-diffstat with-diffstat))
                 ((and (not force) (< age ttl))
                  (when (and newly-mapped cached (not (plist-get cached :not-repo)))
                    (run-hook-with-args 'herdr-sanctum-git-update-hook norm cached)
                    (force-mode-line-update t))
                  (when (and with-diffstat
                             herdr-sanctum-git-diffstat-enable
                             cached
                             (not (plist-get cached :not-repo))
                             (> (or (plist-get cached :dirty) 0) 0))
                    (if (plist-get cached :multi-repo)
                        (dolist (repo (plist-get cached :repos))
                          (when (and (> (or (plist-get repo :dirty) 0) 0)
                                     (null (plist-get repo :added)))
                            (herdr-sanctum-git--fetch-subrepo-diffstat
                             target (plist-get repo :dir))))
                      (when (null (plist-get cached :added))
                        (herdr-sanctum-git--fetch-diffstat target)))))
                 ((herdr-sanctum-git--executable)
                  (if-let* ((prev-subrepos (and (not force)
                                                (null root)
                                                cached
                                                (plist-get cached :multi-repo)
                                                (plist-get cached-meta :subrepos)))
                            (prev-sub-at (or (plist-get cached-meta :subrepos-at) 0.0))
                            ((< (- now prev-sub-at) herdr-sanctum-git-non-repo-ttl))
                            (live-subrepos (cl-remove-if-not
                                            (lambda (s) (file-directory-p (cdr s)))
                                            prev-subrepos)))
                      (herdr-sanctum-git--fetch-multi-repo
                       target live-subrepos with-diffstat prev-sub-at)
                    (herdr-sanctum-git--enqueue-git
                     :key target
                     :norm target
                     :name "herdr-sanctum-git-status"
                     :args (herdr-sanctum-git--status-args target)
                     :priority (or with-diffstat force)
                     :with-diffstat with-diffstat
                     :on-exit (lambda (exit-code output proc job)
                                (herdr-sanctum-git--handle-status-exit
                                 target force exit-code output proc job))))))))))))))

(defun herdr-sanctum-git-refresh-snapshot (snapshot &optional force)
  "Schedule async Git polls for workspaces and focused pane in SNAPSHOT.
When FORCE is non-nil, bypass TTL throttling and visibility gating."
  (when (and herdr-sanctum-git-enable
             (or force (herdr-sanctum-any-ui-window-visible-p)))
    (let ((focused-dir (herdr-sanctum-focused-dir snapshot))
          (seen (make-hash-table :test #'equal))
          (seen-roots (make-hash-table :test #'equal)))
      (cl-flet ((poll (dir with-diffstat)
                  (unless (or (gethash dir seen)
                              (and herdr-sanctum-git-skip-remote
                                   (herdr-sanctum-git--remote-dir-p dir)))
                    (puthash dir t seen)
                    (let* ((norm (file-name-as-directory (expand-file-name dir)))
                           (excluded (herdr-sanctum-git--poll-excluded-p norm))
                           (root (unless excluded
                                   (or (and (not force)
                                            (gethash norm herdr-sanctum-git--dir-to-root))
                                       (herdr-sanctum-git--find-repo-root norm)))))
                      (cond
                       (excluded nil)
                       ((and root
                             (not (equal root norm))
                             (herdr-sanctum-git--poll-excluded-p root))
                        nil)
                       ((and root (gethash root seen-roots) (file-directory-p norm))
                        (when (and (herdr-sanctum-git--record-dir-root norm root)
                                   (not force))
                          (when-let* ((cached (car (gethash root herdr-sanctum-git--cache)))
                                      ((not (plist-get cached :not-repo))))
                            (run-hook-with-args 'herdr-sanctum-git-update-hook norm cached)
                            (force-mode-line-update t))))
                       (t
                        (when root
                          (puthash root t seen-roots))
                        (herdr-sanctum-git-fetch-dir dir force with-diffstat)))))))
        (when focused-dir
          (poll focused-dir t))
        (dolist (ws (alist-get 'workspaces snapshot))
          (when-let* ((dir (herdr-sanctum-workspace-dir ws snapshot)))
            (poll dir nil)))))))

(defun herdr-sanctum-git--on-duration-tick ()
  "Periodically refresh Git metadata when Sanctum UI is visible."
  (when-let* ((snap (herdr-sanctum-snapshot)))
    (herdr-sanctum-git-refresh-snapshot snap)))

(defun herdr-sanctum-git-info-for-dir (dir)
  "Return cached Git status plist for DIR, or nil when absent or non-repo."
  (when (stringp dir)
    (let* ((entry (or (gethash dir herdr-sanctum-git--cache)
                      (when-let* ((root (gethash dir herdr-sanctum-git--dir-to-root)))
                        (gethash root herdr-sanctum-git--cache))))
           (entry (or entry
                      (unless (and herdr-sanctum-git-skip-remote
                                   (herdr-sanctum-git--remote-dir-p dir))
                        (let ((norm (file-name-as-directory (expand-file-name dir))))
                          (or (gethash norm herdr-sanctum-git--cache)
                              (when-let* ((root (gethash norm herdr-sanctum-git--dir-to-root)))
                                (gethash root herdr-sanctum-git--cache))
                              (when (and (> (hash-table-count herdr-sanctum-git--cache) 0)
                                         (not (herdr-sanctum-git--poll-excluded-p norm)))
                                (when-let* ((root (herdr-sanctum-git--find-repo-root norm))
                                            (root-entry (gethash root herdr-sanctum-git--cache)))
                                  (unless (plist-get (car root-entry) :multi-repo)
                                    (herdr-sanctum-git--record-dir-root norm root)
                                    root-entry))))))))
           (info (car entry)))
      (and info (not (plist-get info :not-repo)) info))))

(defun herdr-sanctum-git-clear ()
  "Abort in-flight Git processes and clear pending Git polling state."
  (let ((procs nil))
    (maphash (lambda (_key proc)
               (when (processp proc)
                 (push proc procs)))
             herdr-sanctum-git--inflight)
    (clrhash herdr-sanctum-git--multi-pending)
    (clrhash herdr-sanctum-git--inflight)
    (clrhash herdr-sanctum-git--cache)
    (clrhash herdr-sanctum-git--dir-to-root)
    (clrhash herdr-sanctum-git--truename-to-root)
    (clrhash herdr-sanctum-git--deferred-notify)
    (setq herdr-sanctum-git--high-queue nil
          herdr-sanctum-git--low-queue nil)
    (dolist (proc procs)
      (herdr-sanctum-git--delete-process proc))))

(add-hook 'herdr-sanctum-snapshot-hook #'herdr-sanctum-git-refresh-snapshot)
(add-hook 'herdr-sanctum-duration-tick-hook #'herdr-sanctum-git--on-duration-tick)
(add-hook 'herdr-sanctum-disconnect-hook #'herdr-sanctum-git-clear)

(provide 'herdr-sanctum-git)
;;; herdr-sanctum-git.el ends here
