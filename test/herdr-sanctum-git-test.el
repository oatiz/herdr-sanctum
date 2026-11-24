;;; herdr-sanctum-git-test.el --- Async Git cache tests for herdr-sanctum -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT tests covering `herdr-sanctum-git': porcelain/numstat parsing, real
;; temporary Git repos, multi-repo discovery & short-circuiting, unborn-HEAD
;; TTL handling, diffstat preservation/cancellation, deleted dir eviction,
;; concurrency/priority queue, and LRU cache eviction.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'herdr-sanctum-test-helper)

(ert-deftest herdr-sanctum-git-test/parse-status-and-numstat ()
  "Porcelain v2 (line and -z) and numstat parsers extract exact counts and paths."
  (let ((parsed (herdr-sanctum-git-parse-status
                 (concat "# branch.oid abc123\n"
                         "# branch.head feat/sanctum\n"
                         "# branch.ab +2 -1\n"
                         "1 M. N... 100644 100644 100644 aaa bbb src/a.el\n"
                         "1 .M N... 100644 100644 100644 aaa bbb src/my file.el\n"
                         "u UU N... 100644 100644 100644 100644 a b c conflict.el\n"
                         "? notes/todo.org\n"))))
    (should (equal (plist-get parsed :branch) "feat/sanctum"))
    (should-not (plist-get parsed :detached))
    (should (eql (plist-get parsed :ahead) 2))
    (should (eql (plist-get parsed :behind) 1))
    (should (eql (plist-get parsed :staged) 1))
    (should (eql (plist-get parsed :unstaged) 2))
    (should (eql (plist-get parsed :untracked) 1))
    (should (eql (plist-get parsed :dirty) 4))
    (should (member '(".M" . "src/my file.el") (plist-get parsed :files)))
    (should (member '("UU" . "conflict.el") (plist-get parsed :files))))
  ;; Detached HEAD and `-z' rename entry consuming origPath.
  (let ((z-parsed (herdr-sanctum-git-parse-status
                   (concat "# branch.head (detached)\0"
                           "2 R. N... 100644 100644 100644 aaa bbb R100 new name.el\0old.el\0"
                           "? extra.txt\0"))))
    (should (equal (plist-get z-parsed :branch) "(detached)"))
    (should (plist-get z-parsed :detached))
    (should (eql (plist-get z-parsed :staged) 1))
    (should (eql (plist-get z-parsed :untracked) 1))
    (should (equal (plist-get z-parsed :files)
                   '(("R." . "new name.el") ("??" . "extra.txt")))))
  ;; Numstat sums text diffs and skips binary `- -' rows.
  (should (equal (herdr-sanctum-git-parse-numstat
                  "12\t4\tsrc/a.el\n-\t-\timg.png\n3\t1\tsrc/b.el\n")
                 '(15 . 5))))

(ert-deftest herdr-sanctum-git-test/real-repo-diffstat-subdir-and-z-paths ()
  "Async Git fetch handles real repos, pipe diffstat, subdirs, and Unicode/tab/rename paths."
  (skip-unless (executable-find "git"))
  (herdr-sanctum-test-with-clean-state
    (herdr-sanctum-test-with-temp-dir repo
      (let* ((norm-repo (file-name-as-directory (expand-file-name repo)))
             (subdir (expand-file-name "packages/app" repo))
             (subfile (herdr-sanctum-test--init-git-repo
                       repo "packages/app/index.el" "line-1\nline-2\nline-3\n" "main"))
             (updated nil)
             (hook-fn (lambda (dir info)
                        (when (equal dir norm-repo) (setq updated info))))
             (process-connection-type t))
        (unwind-protect
            (progn
              (add-hook 'herdr-sanctum-git-update-hook hook-fn)
              ;; 1. Diffstat completes via pipe even when `process-connection-type' is t.
              (with-temp-file subfile
                (insert "line-1\nchanged-a\nchanged-b\nline-3\n"))
              (herdr-sanctum-git-fetch-dir repo t t)
              (should (herdr-sanctum-test--wait-until
                       (lambda ()
                         (let ((i (herdr-sanctum-git-info-for-dir repo)))
                           (and i (integerp (plist-get i :added))
                                (integerp (plist-get i :removed)))))
                       4.0))
              (let ((info (herdr-sanctum-git-info-for-dir repo)))
                (should (equal updated info))
                (should (equal (plist-get info :branch) "main"))
                (should (eql (plist-get info :added) 2))
                (should (eql (plist-get info :removed) 1)))
              ;; 2. Subdirectory resolves `:root' to repo root and files resolve on disk.
              (herdr-sanctum-git-fetch-dir subdir t nil)
              (should (herdr-sanctum-test--wait-until
                       (lambda () (herdr-sanctum-git-info-for-dir subdir)) 3.0))
              (let* ((info (herdr-sanctum-git-info-for-dir subdir))
                     (entry (car (plist-get info :files)))
                     (resolved (herdr-sanctum-git-file-path info (cdr entry) subdir)))
                (should (equal (plist-get info :root) norm-repo))
                (should (equal (cdr entry) "packages/app/index.el"))
                (should (file-equal-p resolved subfile)))
              ;; 3. Unicode, tab-containing, and renamed paths parse verbatim via `-z'.
              (let ((cn-file (expand-file-name "中文 文件.txt" repo))
                    (tab-file (expand-file-name "col1\tcol2.txt" repo))
                    (renamed (expand-file-name "moved.el" repo)))
                (with-temp-file cn-file (insert "你好\n"))
                (with-temp-file tab-file (insert "tab\n"))
                (should (zerop (call-process "git" nil nil nil "-C" repo
                                             "mv" "packages/app/index.el" "moved.el")))
                (herdr-sanctum-git-fetch-dir repo t nil)
                (should (herdr-sanctum-test--wait-until
                         (lambda ()
                           (let ((i (herdr-sanctum-git-info-for-dir repo)))
                             (and i (= (plist-get i :dirty) 3))))
                         3.0))
                (let* ((info (herdr-sanctum-git-info-for-dir repo))
                       (paths (mapcar #'cdr (plist-get info :files))))
                  (should (member "中文 文件.txt" paths))
                  (should (member "col1\tcol2.txt" paths))
                  (should (member "moved.el" paths))
                  (dolist (rel paths)
                    (let ((full (herdr-sanctum-git-file-path info rel repo)))
                      (should (and full (file-exists-p full)))))
                  (should (file-exists-p renamed)))))
          (remove-hook 'herdr-sanctum-git-update-hook hook-fn))))))

(ert-deftest herdr-sanctum-git-test/unborn-head-ttl ()
  "Unborn-HEAD repo commits 0/0 diffstat so TTL prevents infinite re-polls."
  (skip-unless (executable-find "git"))
  (herdr-sanctum-test-with-clean-state
    (herdr-sanctum-test-with-temp-dir repo
      (let ((herdr-sanctum-git-cache-ttl 60.0))
        (should (zerop (call-process "git" nil nil nil "-C" repo "init" "-b" "main")))
        (with-temp-file (expand-file-name "README.md" repo) (insert "hello\n"))
        (herdr-sanctum-git-fetch-dir repo t t)
        (should (herdr-sanctum-test--wait-until
                 (lambda ()
                   (let ((i (herdr-sanctum-git-info-for-dir repo)))
                     (and i
                          (= (hash-table-count herdr-sanctum-git--inflight) 0)
                          (integerp (plist-get i :added))
                          (integerp (plist-get i :removed)))))
                 4.0))
        (let ((info (herdr-sanctum-git-info-for-dir repo)))
          (should (equal (plist-get info :branch) "main"))
          (should (eql (plist-get info :untracked) 1))
          (should (eql (plist-get info :added) 0))
          (should (eql (plist-get info :removed) 0)))
        ;; Subsequent TTL fetch with `with-diffstat' must NOT re-spawn diffstat.
        (herdr-sanctum-git-fetch-dir repo nil t)
        (should (= (hash-table-count herdr-sanctum-git--inflight) 0))))))

(ert-deftest herdr-sanctum-git-test/multi-repo-discovery-bounds-and-short-circuit ()
  "Multi-repo discovery honors depth/exclude/max-inspected and short-circuits on 5s TTL."
  (skip-unless (executable-find "git"))
  (herdr-sanctum-test-with-clean-state
    (herdr-sanctum-test-with-temp-dir parent
      (let* ((norm-parent (file-name-as-directory (expand-file-name parent)))
             (alpha (expand-file-name "alpha" parent))
             (beta (expand-file-name "beta" parent))
             (gamma (expand-file-name "packages/gamma" parent))
             (skip-nm (expand-file-name "node_modules/dep" parent))
             (skip-hidden (expand-file-name ".hidden" parent))
             (alpha-a (herdr-sanctum-test--init-git-repo alpha "a.txt" "a1\na2\n" "main"))
             (beta-b (herdr-sanctum-test--init-git-repo beta "b.txt" "b1\nb2\nb3\n" "dev"))
             (gamma-g (herdr-sanctum-test--init-git-repo gamma "g.txt" "g1\n" "main")))
        (herdr-sanctum-test--init-git-repo skip-nm "dep.txt" "dep\n" "main")
        (herdr-sanctum-test--init-git-repo skip-hidden "hid.txt" "hid\n" "main")
        (with-temp-file alpha-a (insert "a1\na2-changed\na3\n"))
        (with-temp-file (expand-file-name "new.txt" alpha) (insert "untracked\n"))
        (with-temp-file beta-b (insert "b1\nb3\n"))
        (with-temp-file gamma-g (insert "g1\ng2\n"))
        (with-temp-file (expand-file-name "dep.txt" skip-nm) (insert "dep-mod\n"))
        (with-temp-file (expand-file-name "hid.txt" skip-hidden) (insert "hid-mod\n"))
        ;; 1. Depth-1 vs depth-2 discovery skips node_modules and hidden dirs.
        (let ((herdr-sanctum-git-subrepo-max-depth 1))
          (should (equal (mapcar #'car (herdr-sanctum-git--discover-subrepos parent))
                         '("alpha" "beta"))))
        (let ((herdr-sanctum-git-subrepo-max-depth 2))
          (should (equal (mapcar #'car (herdr-sanctum-git--discover-subrepos parent))
                         '("alpha" "beta" "packages/gamma")))
          ;; 2. Full multi-repo aggregation with diffstat.
          (herdr-sanctum-git-fetch-dir parent t t)
          (should (herdr-sanctum-test--wait-until
                   (lambda ()
                     (let ((info (herdr-sanctum-git-info-for-dir parent)))
                       (and info (plist-get info :multi-repo)
                            (= (length (plist-get info :repos)) 3)
                            (cl-every (lambda (r)
                                        (and (integerp (plist-get r :added))
                                             (integerp (plist-get r :removed))))
                                      (plist-get info :repos)))))
                   5.0))
          (let* ((info (herdr-sanctum-git-info-for-dir parent))
                 (files (plist-get info :files)))
            (should (equal (plist-get info :root) norm-parent))
            (should (equal (plist-get info :branch) "3 repos"))
            (should (eql (plist-get info :dirty) 4))
            (should (eql (plist-get info :added) 3))
            (should (eql (plist-get info :removed) 2))
            (should (member "alpha/a.txt" (mapcar #'cdr files)))
            (should (member "packages/gamma/g.txt" (mapcar #'cdr files)))
            (dolist (entry files)
              (should (file-exists-p (herdr-sanctum-git-file-path info (cdr entry) parent)))))
          ;; 3. Multi-repo 5s short-circuit: when cache TTL (5s) expires but subrepos-at
          ;;    is still within non-repo-ttl (30s), skip parent git status & discovery.
          (let* ((entry (gethash norm-parent herdr-sanctum-git--cache))
                 (meta (copy-sequence (cdr entry)))
                 (now (float-time))
                 (discover-calls 0))
            (plist-put meta :updated-at (- now 10.0))
            (plist-put meta :subrepos-at (- now 2.0))
            (puthash norm-parent (cons (car entry) meta) herdr-sanctum-git--cache)
            (cl-letf (((symbol-function 'herdr-sanctum-git--discover-subrepos)
                       (lambda (&rest _) (cl-incf discover-calls) nil)))
              (herdr-sanctum-git-fetch-dir parent nil nil)
              (should (gethash norm-parent herdr-sanctum-git--multi-pending))
              (should (= discover-calls 0))
              (should (herdr-sanctum-test--wait-until
                       (lambda () (not (gethash norm-parent herdr-sanctum-git--multi-pending)))
                       4.0))
              (should (= discover-calls 0)))))
        ;; 4. Exclude dirs and max-depth = 0 produce `:not-repo t'.
        (remhash norm-parent herdr-sanctum-git--cache)
        (let ((herdr-sanctum-git-subrepo-exclude-dirs (list parent)))
          (herdr-sanctum-git-fetch-dir parent t nil)
          (should (herdr-sanctum-test--wait-until
                   (lambda () (gethash norm-parent herdr-sanctum-git--cache)) 3.0))
          (should (plist-get (car (gethash norm-parent herdr-sanctum-git--cache)) :not-repo))
          (should-not (herdr-sanctum-git-info-for-dir parent)))
        (remhash norm-parent herdr-sanctum-git--cache)
        (let ((herdr-sanctum-git-subrepo-max-depth 0))
          (herdr-sanctum-git-fetch-dir parent t nil)
          (should (herdr-sanctum-test--wait-until
                   (lambda () (gethash norm-parent herdr-sanctum-git--cache)) 3.0))
          (should (plist-get (car (gethash norm-parent herdr-sanctum-git--cache)) :not-repo)))
        ;; 5. `max-inspected' caps directory entries scanned.
        (dotimes (i 12)
          (make-directory (expand-file-name (format "dir-%02d" i) parent) t))
        ;; Symlinked directory pointing to a repo is ignored via single lstat.
        (make-symbolic-link alpha (expand-file-name "alpha-link" parent))
        (let ((herdr-sanctum-git-subrepo-max-depth 1))
          (should (equal (mapcar #'car (herdr-sanctum-git--discover-subrepos parent))
                         '("alpha" "beta"))))
        (let ((herdr-sanctum-git-subrepo-max-inspected 4)
              (herdr-sanctum-git-subrepo-max-count 20)
              (checked 0)
              (orig-fattrs (symbol-function 'file-attributes)))
          (cl-letf (((symbol-function 'file-attributes)
                     (lambda (p &optional id-format)
                       (when (and (string-prefix-p norm-parent p)
                                  (not (equal p norm-parent)))
                         (cl-incf checked))
                       (funcall orig-fattrs p id-format))))
            (herdr-sanctum-git--discover-subrepos parent))
          (should (> checked 0))
          (should (<= checked 4)))))))

(ert-deftest herdr-sanctum-git-test/diffstat-preservation-and-ttl-upgrade ()
  "Clean->dirty drops stale 0, TTL upgrades diffstat, unfocused dirty preserves diffstat."
  (skip-unless (executable-find "git"))
  (herdr-sanctum-test-with-clean-state
    (herdr-sanctum-test-with-temp-dir repo
      (herdr-sanctum-test-with-temp-dir parent
        (let* ((herdr-sanctum-git-cache-ttl 60.0)
               (base (herdr-sanctum-test--init-git-repo repo "base.txt" "a\nb\n"))
               (norm-parent (file-name-as-directory (expand-file-name parent)))
               (alpha (expand-file-name "alpha" parent))
               (alpha-f (herdr-sanctum-test--init-git-repo alpha "a.txt" "a1\na2\n")))
          ;; 1. Clean repo (:added 0) -> dirty in background -> stale 0 NOT carried.
          (herdr-sanctum-git-fetch-dir repo t t)
          (should (herdr-sanctum-test--wait-until
                   (lambda ()
                     (let ((i (herdr-sanctum-git-info-for-dir repo)))
                       (and i (eql (plist-get i :dirty) 0))))
                   3.0))
          (should (eql (plist-get (herdr-sanctum-git-info-for-dir repo) :added) 0))
          (with-temp-file base (insert "a\nb-mod\nc\n"))
          (herdr-sanctum-git-fetch-dir repo t nil)
          (should (herdr-sanctum-test--wait-until
                   (lambda ()
                     (let ((i (herdr-sanctum-git-info-for-dir repo)))
                       (and i (eql (plist-get i :dirty) 1))))
                   3.0))
          (should-not (plist-get (herdr-sanctum-git-info-for-dir repo) :added))
          ;; 2. Focus within TTL upgrades diffstat to +2 -1.
          (herdr-sanctum-git-fetch-dir repo nil t)
          (should (herdr-sanctum-test--wait-until
                   (lambda ()
                     (integerp (plist-get (herdr-sanctum-git-info-for-dir repo) :added)))
                   3.0))
          (should (eql (plist-get (herdr-sanctum-git-info-for-dir repo) :added) 2))
          (should (eql (plist-get (herdr-sanctum-git-info-for-dir repo) :removed) 1))
          ;; 3. Unfocused background poll with unchanged dirty `:files' preserves +2 -1.
          (herdr-sanctum-git-fetch-dir repo t nil)
          (should (herdr-sanctum-test--wait-until
                   (lambda () (= (hash-table-count herdr-sanctum-git--inflight) 0)) 3.0))
          (should (eql (plist-get (herdr-sanctum-git-info-for-dir repo) :added) 2))
          (should (eql (plist-get (herdr-sanctum-git-info-for-dir repo) :removed) 1))
          ;; 4. Multi-repo TTL diffstat upgrade and pending batch upgrade.
          (with-temp-file alpha-f (insert "a1\na2-mod\na3\n"))
          (herdr-sanctum-git-fetch-dir parent t nil)
          (should (herdr-sanctum-test--wait-until
                   (lambda () (herdr-sanctum-git-info-for-dir parent)) 4.0))
          (should-not (plist-get (car (plist-get (herdr-sanctum-git-info-for-dir parent) :repos))
                                 :added))
          (herdr-sanctum-git-fetch-dir parent nil t)
          (should (herdr-sanctum-test--wait-until
                   (lambda ()
                     (let ((info (herdr-sanctum-git-info-for-dir parent)))
                       (and info (integerp (plist-get info :added)))))
                   4.0))
          (should (eql (plist-get (herdr-sanctum-git-info-for-dir parent) :added) 2))
          (should (eql (plist-get (herdr-sanctum-git-info-for-dir parent) :removed) 1))
          ;; Pending multi-repo batch upgrades `:with-diffstat' when called with force nil.
          (remhash norm-parent herdr-sanctum-git--cache)
          (let ((subrepos (herdr-sanctum-git--discover-subrepos parent)))
            (herdr-sanctum-git--fetch-multi-repo norm-parent subrepos nil)
            (should-not (plist-get (gethash norm-parent herdr-sanctum-git--multi-pending)
                                   :with-diffstat))
            (herdr-sanctum-git-fetch-dir parent nil t)
            (should (plist-get (gethash norm-parent herdr-sanctum-git--multi-pending)
                               :with-diffstat))
            (should (herdr-sanctum-test--wait-until
                     (lambda ()
                       (let ((info (herdr-sanctum-git-info-for-dir parent)))
                         (and info (integerp (plist-get info :added)))))
                     4.0))
            (should (eql (plist-get (herdr-sanctum-git-info-for-dir parent) :added) 2))))))))

(ert-deftest herdr-sanctum-git-test/commit-cache-dedup-and-not-repo-suppression ()
  "First `:not-repo' suppresses hook; unchanged commits preserve `eq' identity."
  (herdr-sanctum-test-with-clean-state
    (let* ((dir-a "/tmp/herdr-commit-a/")
           (dir-b "/tmp/herdr-commit-b/")
           (hook-calls nil)
           (hook-fn (lambda (d i) (push (cons d i) hook-calls))))
      (unwind-protect
          (progn
            (add-hook 'herdr-sanctum-git-update-hook hook-fn)
            ;; First `:not-repo' for an uncached dir does NOT fire update hook.
            (herdr-sanctum-git--commit-cache dir-b '(:not-repo t) '(:updated-at 10.0))
            (should (null hook-calls))
            (should (plist-get (car (gethash dir-b herdr-sanctum-git--cache)) :not-repo))
            (should-not (herdr-sanctum-git-info-for-dir dir-b))
            ;; First real repo fires hook; semantic duplicate with new meta preserves `eq'.
            (herdr-sanctum-git--commit-cache
             dir-a (list :branch "main" :dirty 0 :added 0 :removed 0) '(:updated-at 100.0))
            (should (= (length hook-calls) 1))
            (let ((first-obj (herdr-sanctum-git-info-for-dir dir-a)))
              (herdr-sanctum-git--commit-cache
               dir-a (list :branch "main" :dirty 0 :added 0 :removed 0) '(:updated-at 200.0))
              (should (= (length hook-calls) 1))
              (should (eq (herdr-sanctum-git-info-for-dir dir-a) first-obj)))
            ;; Semantic change fires hook.
            (herdr-sanctum-git--commit-cache
             dir-a (list :branch "main" :dirty 1 :added 3 :removed 1) '(:updated-at 205.0))
            (should (= (length hook-calls) 2))
            ;; Real repo -> `:not-repo' DOES fire hook.
            (herdr-sanctum-git--commit-cache dir-a '(:not-repo t) '(:updated-at 210.0))
            (should (= (length hook-calls) 3)))
        (remove-hook 'herdr-sanctum-git-update-hook hook-fn)))))

(ert-deftest herdr-sanctum-git-test/stale-diffstat-cancel-and-deleted-dir-eviction ()
  "Clean status cancels stale diffstat race; deleted dir evicts cache and notifies hook."
  (skip-unless (executable-find "git"))
  (herdr-sanctum-test-with-clean-state
    (herdr-sanctum-test-with-temp-dir repo
      (let* ((norm (file-name-as-directory (expand-file-name repo)))
             (stale-proc nil)
             (hook-events nil)
             (hook-fn (lambda (d i) (push (cons d i) hook-events))))
        (unwind-protect
            (progn
              (herdr-sanctum-test--init-git-repo repo "a.txt" "a1\n")
              (add-hook 'herdr-sanctum-git-update-hook hook-fn)
              ;; 1. Stale diffstat race: clean status cancels live `diffstat:<norm>' proc.
              (setq stale-proc (make-process :name "herdr-stale-diffstat"
                                             :buffer nil
                                             :command '("sleep" "5")
                                             :connection-type 'pipe
                                             :noquery t))
              (puthash (concat "diffstat:" norm) stale-proc herdr-sanctum-git--inflight)
              (herdr-sanctum-git-fetch-dir repo t nil)
              (should (herdr-sanctum-test--wait-until
                       (lambda ()
                         (let ((i (herdr-sanctum-git-info-for-dir repo)))
                           (and i (eql (plist-get i :dirty) 0))))
                       3.0))
              (should-not (process-live-p stale-proc))
              (should-not (gethash (concat "diffstat:" norm) herdr-sanctum-git--inflight))
              (should (eql (plist-get (herdr-sanctum-git-info-for-dir repo) :added) 0))
              ;; 2. Deleted directory is evicted from cache and fires hook with nil info.
              (setq hook-events nil)
              (delete-directory repo t)
              (herdr-sanctum-git-fetch-dir repo nil nil)
              (should-not (gethash norm herdr-sanctum-git--cache))
              (should (equal hook-events (list (cons norm nil)))))
          (remove-hook 'herdr-sanctum-git-update-hook hook-fn)
          (when (and (processp stale-proc) (process-live-p stale-proc))
            (delete-process stale-proc)))))))

(ert-deftest herdr-sanctum-git-test/parse-status-conflict-codes ()
  "Porcelain v2 unmerged `u' records preserve exact 2-char conflict codes."
  (let* ((line-parsed
          (herdr-sanctum-git-parse-status
           (concat "# branch.head main\n"
                   "u DD N... 100644 100644 100644 100644 a b c both-deleted.el\n"
                   "u AA N... 100644 100644 100644 100644 a b c both-added.el\n"
                   "u AU N... 100644 100644 100644 100644 a b c added-by-us.el\n"
                   "u UU N... 100644 100644 100644 100644 a b c both-modified.el\n")))
         (z-parsed
          (herdr-sanctum-git-parse-status
           (concat "# branch.head main\0"
                   "u DU N... 100644 100644 100644 100644 a b c del-us.el\0"
                   "u UA N... 100644 100644 100644 100644 a b c add-them.el\0"
                   "u UD N... 100644 100644 100644 100644 a b c del-them.el\0"))))
    (should (eql (plist-get line-parsed :unstaged) 4))
    (should (eql (plist-get line-parsed :dirty) 4))
    (should (equal (plist-get line-parsed :files)
                   '(("DD" . "both-deleted.el")
                     ("AA" . "both-added.el")
                     ("AU" . "added-by-us.el")
                     ("UU" . "both-modified.el"))))
    (should (eql (plist-get z-parsed :unstaged) 3))
    (should (equal (plist-get z-parsed :files)
                   '(("DU" . "del-us.el")
                     ("UA" . "add-them.el")
                     ("UD" . "del-them.el"))))))

(ert-deftest herdr-sanctum-git-test/executable-retries-after-nil ()
  "`herdr-sanctum-git--executable' never caches nil and caches the first found path."
  (let ((herdr-sanctum-git--executable nil)
        (calls 0)
        (results '(nil "/fake/bin/git" "/other/bin/git")))
    (cl-letf (((symbol-function 'executable-find)
               (lambda (_cmd &rest _)
                 (cl-incf calls)
                 (pop results))))
      (should-not (herdr-sanctum-git--executable))
      (should-not herdr-sanctum-git--executable)
      (should (equal (herdr-sanctum-git--executable) "/fake/bin/git"))
      (should (equal herdr-sanctum-git--executable "/fake/bin/git"))
      (should (equal (herdr-sanctum-git--executable) "/fake/bin/git"))
      (should (= calls 2)))))

(ert-deftest herdr-sanctum-git-test/start-job-with-nonexistent-default-directory ()
  "Git jobs succeed even when the current buffer's `default-directory' does not exist."
  (skip-unless (executable-find "git"))
  (herdr-sanctum-test-with-clean-state
    (herdr-sanctum-test-with-temp-dir repo
      (herdr-sanctum-test--init-git-repo repo "a.txt" "hello\n" "main")
      (let ((default-directory (expand-file-name "no-such-subdir/" repo)))
        ;; 1. Fetch for a valid repo binds `default-directory' to `norm'.
        (herdr-sanctum-git-fetch-dir repo t nil)
        (should (herdr-sanctum-test--wait-until
                 (lambda () (herdr-sanctum-git-info-for-dir repo)) 3.0))
        (should (equal (plist-get (herdr-sanctum-git-info-for-dir repo) :branch) "main"))
        ;; 2. Direct `--start-job' without `:norm' falls back to `temporary-file-directory'.
        (let ((exited nil)
              (out nil))
          (herdr-sanctum-git--start-job
           (list :key "safe-dir:version"
                 :name "herdr-sanctum-git-safe-dir"
                 :args '("--version")
                 :on-exit (lambda (code output _p _j)
                            (setq exited code out output))))
          (should (herdr-sanctum-test--wait-until (lambda () (integerp exited)) 3.0))
          (should (zerop exited))
          (should (string-search "git version" out)))))))

(ert-deftest herdr-sanctum-git-test/stderr-isolation-and-no-pipe-leaks ()
  "Stderr output does not corrupt `-z' stdout, and stderr pipes never leak."
  (skip-unless (executable-find "git"))
  (herdr-sanctum-test-with-clean-state
    (herdr-sanctum-test-with-temp-dir repo
      (let* ((norm-repo (file-name-as-directory (expand-file-name repo)))
             (count-stderr-pipes
              (lambda ()
                (cl-count-if (lambda (p)
                               (string-prefix-p "herdr-sanctum-git-stderr"
                                                (process-name p)))
                             (process-list)))))
        (herdr-sanctum-test--init-git-repo repo "a.txt" "hello\n" "main")
        (with-temp-file (expand-file-name "b.txt" repo) (insert "new\n"))
        (let ((process-environment (cons "GIT_TRACE=1" process-environment)))
          (should (zerop (funcall count-stderr-pipes)))
          ;; 1. Raw stdout isolation: GIT_TRACE=1 writes `trace:' to stderr,
          ;;    not stdout, and normal completion cleans up the stderr pipe.
          (let ((raw-code nil)
                (raw-out nil))
            (herdr-sanctum-git--start-job
             (list :key "stderr-raw:status"
                   :norm norm-repo
                   :name "herdr-sanctum-git-stderr-raw"
                   :args '("status" "--porcelain=v2" "-z" "--branch")
                   :on-exit (lambda (code output _p _j)
                              (setq raw-code code raw-out output))))
            (should (herdr-sanctum-test--wait-until
                     (lambda () (integerp raw-code)) 3.0))
            (should (zerop raw-code))
            (should (string-prefix-p "# branch.oid " raw-out))
            (should-not (string-search "trace:" raw-out))
            (should (zerop (funcall count-stderr-pipes))))
          ;; 2. Full fetch under GIT_TRACE=1 isolates stderr and cleans up pipe.
          (herdr-sanctum-git-fetch-dir repo t nil)
          (should (herdr-sanctum-test--wait-until
                   (lambda () (herdr-sanctum-git-info-for-dir repo)) 3.0))
          (let ((info (herdr-sanctum-git-info-for-dir repo)))
            (should (equal (plist-get info :branch) "main"))
            (should (equal (plist-get info :files) '(("??" . "b.txt")))))
          (should (= (funcall count-stderr-pipes) 0))
          ;; 3. `--cancel-dir-jobs' deletes live stderr pipe immediately.
          (herdr-sanctum-git--start-job
           (list :key norm-repo
                 :norm norm-repo
                 :name "herdr-sanctum-git-cancel-stderr"
                 :args '("hash-object" "--stdin")
                 :on-exit #'ignore))
          (should (= (funcall count-stderr-pipes) 1))
          (herdr-sanctum-git--cancel-dir-jobs norm-repo)
          (should (= (funcall count-stderr-pipes) 0))
          ;; 4. `herdr-sanctum-git-clear' deletes live stderr pipe immediately.
          (herdr-sanctum-git--start-job
           (list :key "clear-stderr:job"
                 :name "herdr-sanctum-git-clear-stderr"
                 :args '("hash-object" "--stdin")
                 :on-exit #'ignore))
          (should (= (funcall count-stderr-pipes) 1))
          (herdr-sanctum-git-clear)
          (should (= (funcall count-stderr-pipes) 0)))))))

(ert-deftest herdr-sanctum-git-test/info-for-dir-fast-path ()
  "`herdr-sanctum-git-info-for-dir' hits normalized cache keys without calling `expand-file-name'."
  (herdr-sanctum-test-with-clean-state
    (let* ((norm "/tmp/herdr-fast-path-repo/")
           (unnorm "/tmp/herdr-fast-path-repo")
           (payload (list :branch "main" :dirty 0))
           (expand-calls 0)
           (orig-expand (symbol-function 'expand-file-name)))
      (puthash norm (cons payload (list :updated-at (float-time)))
               herdr-sanctum-git--cache)
      (cl-letf (((symbol-function 'expand-file-name)
                 (lambda (name &optional default-dir)
                   (cl-incf expand-calls)
                   (funcall orig-expand name default-dir))))
        (should (eq (herdr-sanctum-git-info-for-dir norm) payload))
        (should (= expand-calls 0))
        (should (eq (herdr-sanctum-git-info-for-dir unnorm) payload))
        (should (= expand-calls 1))))))

(ert-deftest herdr-sanctum-git-test/multi-repo-subdiff-coalescing ()
  "Multi-repo `subdiff' completions coalesce into a single update hook notification."
  (skip-unless (executable-find "git"))
  (herdr-sanctum-test-with-clean-state
    (herdr-sanctum-test-with-temp-dir parent
      (let* ((norm-parent (file-name-as-directory (expand-file-name parent)))
             (alpha (expand-file-name "alpha" parent))
             (beta (expand-file-name "beta" parent))
             (gamma (expand-file-name "gamma" parent))
             (alpha-f (herdr-sanctum-test--init-git-repo alpha "a.txt" "a1\n"))
             (beta-f (herdr-sanctum-test--init-git-repo beta "b.txt" "b1\n"))
             (gamma-f (herdr-sanctum-test--init-git-repo gamma "g.txt" "g1\n"))
             (hook-calls nil)
             (hook-fn (lambda (dir info)
                        (when (equal dir norm-parent) (push info hook-calls)))))
        (unwind-protect
            (progn
              (with-temp-file alpha-f (insert "a1\na2\n"))
              (with-temp-file beta-f (insert "b1\nb2\nb3\n"))
              (with-temp-file gamma-f (insert "g1\ng2\n"))
              (add-hook 'herdr-sanctum-git-update-hook hook-fn)
              (herdr-sanctum-git-fetch-dir parent t t)
              (should (herdr-sanctum-test--wait-until
                       (lambda ()
                         (let ((info (herdr-sanctum-git-info-for-dir parent)))
                           (and info (plist-get info :multi-repo)
                                (= (hash-table-count herdr-sanctum-git--inflight) 0)
                                (= (length (plist-get info :repos)) 3)
                                (cl-every (lambda (r)
                                            (integerp (plist-get r :added)))
                                          (plist-get info :repos)))))
                       5.0))
              ;; 1 notification for multi-repo status + 1 coalesced notification for all 3 subdiffs.
              (should (= (length hook-calls) 2))
              (should (eql (plist-get (car hook-calls) :added) 4))
              (should (eql (plist-get (car hook-calls) :removed) 0)))
          (remove-hook 'herdr-sanctum-git-update-hook hook-fn))))))

(ert-deftest herdr-sanctum-git-test/clear-aborts-mid-batch ()
  "`herdr-sanctum-git-clear' aborts a multi-repo batch with zero cache pollution."
  (skip-unless (executable-find "git"))
  (herdr-sanctum-test-with-clean-state
    (herdr-sanctum-test-with-temp-dir parent
      (let* ((norm-parent (file-name-as-directory (expand-file-name parent)))
             (alpha (expand-file-name "alpha" parent))
             (beta (expand-file-name "beta" parent))
             (hook-calls nil)
             (hook-fn (lambda (dir info)
                        (when (equal dir norm-parent) (push info hook-calls)))))
        (unwind-protect
            (progn
              (herdr-sanctum-test--init-git-repo alpha "a.txt" "a1\n")
              (herdr-sanctum-test--init-git-repo beta "b.txt" "b1\n")
              (add-hook 'herdr-sanctum-git-update-hook hook-fn)
              (let ((subrepos (herdr-sanctum-git--discover-subrepos parent)))
                (herdr-sanctum-git--fetch-multi-repo norm-parent subrepos t)
                (should (gethash norm-parent herdr-sanctum-git--multi-pending))
                (should (> (hash-table-count herdr-sanctum-git--inflight) 0))
                (herdr-sanctum-git-clear)
                (accept-process-output nil 0.05)
                (should (null hook-calls))
                (should-not (gethash norm-parent herdr-sanctum-git--cache))
                (should (= (hash-table-count herdr-sanctum-git--inflight) 0))
                (should (= (hash-table-count herdr-sanctum-git--multi-pending) 0))))
          (remove-hook 'herdr-sanctum-git-update-hook hook-fn))))))

(ert-deftest herdr-sanctum-git-test/force-fetch-isolates-dir ()
  "Forced fetch cancels only its target directory and preserves unrelated jobs."
  (skip-unless (executable-find "git"))
  (herdr-sanctum-test-with-clean-state
    (herdr-sanctum-test-with-temp-dir parent
      (let* ((norm-parent (file-name-as-directory (expand-file-name parent)))
             (alpha (expand-file-name "alpha" parent))
             (beta (expand-file-name "beta" parent))
             (other-key "/tmp/herdr-sanctum-other-dir/")
             (other-proc nil))
        (unwind-protect
            (let ((alpha-f (herdr-sanctum-test--init-git-repo alpha "a.txt" "a1\n")))
              (herdr-sanctum-test--init-git-repo beta "b.txt" "b1\n")
              (with-temp-file alpha-f (insert "a1\na2\n"))
              (setq other-proc (make-process :name "herdr-sanctum-test-other"
                                             :buffer nil :command '("sleep" "5")
                                             :connection-type 'pipe :noquery t))
              (puthash other-key other-proc herdr-sanctum-git--inflight)
              (let ((subrepos (herdr-sanctum-git--discover-subrepos parent)))
                (herdr-sanctum-git--fetch-multi-repo norm-parent subrepos nil)
                (herdr-sanctum-git-fetch-dir parent t t)
                (should (process-live-p other-proc))
                (should (herdr-sanctum-test--wait-until
                         (lambda ()
                           (let ((info (herdr-sanctum-git-info-for-dir parent)))
                             (and info (plist-get info :multi-repo)
                                  (plist-get info :repos)
                                  (cl-every (lambda (r)
                                              (and (integerp (plist-get r :added))
                                                   (integerp (plist-get r :removed))))
                                            (plist-get info :repos)))))
                         4.0))
                (should (eql (plist-get (herdr-sanctum-git-info-for-dir parent) :added) 1))
                (should (process-live-p other-proc))))
          (remhash other-key herdr-sanctum-git--inflight)
          (when (and (processp other-proc) (process-live-p other-proc))
            (delete-process other-proc)))))))

(ert-deftest herdr-sanctum-git-test/priority-promotion-under-saturation ()
  "Promoting a queued directory jumps ahead of earlier normal jobs and upgrades diffstat."
  (skip-unless (executable-find "git"))
  (herdr-sanctum-test-with-clean-state
    (herdr-sanctum-test-with-temp-dir parent
      (let* ((alpha (expand-file-name "alpha" parent))
             (beta (expand-file-name "beta" parent))
             (norm-alpha (file-name-as-directory (expand-file-name alpha)))
             (norm-beta (file-name-as-directory (expand-file-name beta)))
             (blocker-procs nil)
             (started-keys nil)
             (orig-start (symbol-function 'herdr-sanctum-git--start-job)))
        (unwind-protect
            (let ((alpha-f (herdr-sanctum-test--init-git-repo alpha "a.txt" "a1\n"))
                  (beta-f (herdr-sanctum-test--init-git-repo beta "b.txt" "b1\n")))
              (with-temp-file alpha-f (insert "a1\na2\n"))
              (with-temp-file beta-f (insert "b1\nb2\n"))
              ;; Saturate all concurrency slots.
              (dotimes (i herdr-sanctum-git--max-concurrency)
                (let ((p (make-process :name (format "herdr-block-%d" i)
                                       :buffer nil :command '("sleep" "5")
                                       :connection-type 'pipe :noquery t)))
                  (push (cons (format "blocker:%d" i) p) blocker-procs)
                  (puthash (format "blocker:%d" i) p herdr-sanctum-git--inflight)))
              ;; Queue beta first, then alpha, both low priority; then promote alpha.
              (herdr-sanctum-git-fetch-dir beta nil nil)
              (herdr-sanctum-git-fetch-dir alpha nil nil)
              (herdr-sanctum-git-fetch-dir alpha nil t)
              (cl-letf (((symbol-function 'herdr-sanctum-git--start-job)
                         (lambda (job)
                           (push (plist-get job :key) started-keys)
                           (funcall orig-start job))))
                ;; Free one slot: promoted `alpha' must start before earlier-queued `beta'.
                (let ((first-blocker (pop blocker-procs)))
                  (remhash (car first-blocker) herdr-sanctum-git--inflight)
                  (delete-process (cdr first-blocker)))
                (herdr-sanctum-git--pump-jobs)
                (should (equal (car started-keys) norm-alpha))
                ;; Free remaining blockers and wait for both repos to finish.
                (dolist (entry blocker-procs)
                  (remhash (car entry) herdr-sanctum-git--inflight)
                  (when (process-live-p (cdr entry)) (delete-process (cdr entry))))
                (setq blocker-procs nil)
                (herdr-sanctum-git--pump-jobs)
                (should (herdr-sanctum-test--wait-until
                         (lambda ()
                           (let ((ia (herdr-sanctum-git-info-for-dir alpha))
                                 (ib (herdr-sanctum-git-info-for-dir beta)))
                             (and ia ib
                                  (integerp (plist-get ia :added))
                                  (= (hash-table-count herdr-sanctum-git--inflight) 0))))
                         4.0))
                (should (eql (plist-get (herdr-sanctum-git-info-for-dir alpha) :added) 1))
                (should-not (plist-get (herdr-sanctum-git-info-for-dir beta) :added))
                (should (member norm-beta started-keys))))
          (dolist (entry blocker-procs)
            (remhash (car entry) herdr-sanctum-git--inflight)
            (when (and (processp (cdr entry)) (process-live-p (cdr entry)))
              (delete-process (cdr entry)))))))))

(ert-deftest herdr-sanctum-git-test/lru-cache-eviction ()
  "LRU cache eviction retains only the newest `herdr-sanctum-git--cache-max-entries'."
  (herdr-sanctum-test-with-clean-state
    (dotimes (i (+ herdr-sanctum-git--cache-max-entries 5))
      (puthash (format "/tmp/herdr-lru-%03d/" i)
               (cons (list :branch "main" :dirty 0)
                     (list :updated-at (float i)))
               herdr-sanctum-git--cache))
    (herdr-sanctum-git--evict-cache)
    (should (= (hash-table-count herdr-sanctum-git--cache)
               herdr-sanctum-git--cache-max-entries))
    (should-not (gethash "/tmp/herdr-lru-000/" herdr-sanctum-git--cache))
    (should-not (gethash "/tmp/herdr-lru-004/" herdr-sanctum-git--cache))
    (should (gethash "/tmp/herdr-lru-005/" herdr-sanctum-git--cache))
    (should (gethash (format "/tmp/herdr-lru-%03d/"
                             (+ herdr-sanctum-git--cache-max-entries 4))
                     herdr-sanctum-git--cache))))

(ert-deftest herdr-sanctum-git-test/signal-termination-invokes-on-error ()
  "Signal termination of an active job invokes `:on-error' instead of `:on-exit'."
  (skip-unless (executable-find "git"))
  (herdr-sanctum-test-with-clean-state
    (let ((exit-called nil)
          (error-job nil)
          (sig-key "signal-test:job"))
      (herdr-sanctum-git--start-job
       (list :key sig-key
             :name "herdr-sanctum-git-signal-test"
             :args '("hash-object" "--stdin")
             :on-exit (lambda (&rest _) (setq exit-called t))
             :on-error (lambda (j) (setq error-job j))))
      (let ((proc (gethash sig-key herdr-sanctum-git--inflight)))
        (should (and (processp proc) (process-live-p proc)))
        (kill-process proc)
        (should (herdr-sanctum-test--wait-until (lambda () error-job) 2.0))
        (should-not exit-called)
        (should (equal (plist-get error-job :key) sig-key))
        (should-not (gethash sig-key herdr-sanctum-git--inflight))))))

(ert-deftest herdr-sanctum-git-test/no-optional-locks-and-status-extra-args ()
  "Background Git commands pass `--no-optional-locks' and status extra args."
  (skip-unless (executable-find "git"))
  (herdr-sanctum-test-with-clean-state
    (herdr-sanctum-test-with-temp-dir repo
      (let* ((file (herdr-sanctum-test--init-git-repo repo "a.txt" "line1\n" "main"))
             (commands nil)
             (orig-make-process (symbol-function 'make-process)))
        (with-temp-file file (insert "line1\nline2\n"))
        (with-temp-file (expand-file-name "untracked.txt" repo) (insert "new\n"))
        (cl-letf (((symbol-function 'make-process)
                   (lambda (&rest args)
                     (let ((cmd (plist-get args :command)))
                       (when (and (listp cmd) (cdr cmd))
                         (push cmd commands)))
                     (apply orig-make-process args))))
          ;; 1. Both status and diffstat pass `--no-optional-locks' right after `--no-pager'.
          (herdr-sanctum-git-fetch-dir repo t t)
          (should (herdr-sanctum-test--wait-until
                   (lambda ()
                     (let ((i (herdr-sanctum-git-info-for-dir repo)))
                       (and i (integerp (plist-get i :added))
                            (= (hash-table-count herdr-sanctum-git--inflight) 0))))
                   4.0))
          (should (>= (length commands) 2))
          (dolist (cmd commands)
            (should (equal (nth 1 cmd) "--no-pager"))
            (should (equal (nth 2 cmd) "--no-optional-locks")))
          (should (eql (plist-get (herdr-sanctum-git-info-for-dir repo) :untracked) 1))
          ;; 2. `herdr-sanctum-git-status-extra-args' appends extra status flags.
          (setq commands nil)
          (let ((herdr-sanctum-git-status-extra-args '("--untracked-files=no")))
            (herdr-sanctum-git-fetch-dir repo t nil)
            (should (herdr-sanctum-test--wait-until
                     (lambda ()
                       (let ((i (herdr-sanctum-git-info-for-dir repo)))
                         (and i (eql (plist-get i :untracked) 0)
                              (= (hash-table-count herdr-sanctum-git--inflight) 0))))
                     4.0))
            (let ((status-cmd (cl-find-if (lambda (c) (member "status" c)) commands)))
              (should status-cmd)
              (should (equal (car (last status-cmd)) "--untracked-files=no"))
              (should (eql (plist-get (herdr-sanctum-git-info-for-dir repo) :dirty) 1)))))))))

(ert-deftest herdr-sanctum-git-test/git-program-defcustom-resets-cache ()
  "Customizing `herdr-sanctum-git-program' resets cached executable immediately."
  (let ((orig-program herdr-sanctum-git-program)
        (herdr-sanctum-git--executable nil)
        (herdr-sanctum-git--executable-program nil)
        (seen-cmds nil))
    (unwind-protect
        (cl-letf (((symbol-function 'executable-find)
                   (lambda (cmd &rest _)
                     (push cmd seen-cmds)
                     (concat "/resolved/" cmd))))
          (should (equal (herdr-sanctum-git--executable) "/resolved/git"))
          (should (equal herdr-sanctum-git--executable "/resolved/git"))
          ;; Changing via `customize-set-variable' immediately clears cache.
          (customize-set-variable 'herdr-sanctum-git-program "custom-git")
          (should-not herdr-sanctum-git--executable)
          (should (equal (herdr-sanctum-git--executable) "/resolved/custom-git"))
          (should (equal seen-cmds '("custom-git" "git")))
          ;; Dynamic `let'-binding also resolves the bound program cleanly.
          (let ((herdr-sanctum-git-program "let-git"))
            (should (equal (herdr-sanctum-git--executable) "/resolved/let-git")))
          (should (equal (herdr-sanctum-git--executable) "/resolved/custom-git")))
      (customize-set-variable 'herdr-sanctum-git-program orig-program))))

(ert-deftest herdr-sanctum-git-test/command-timeout-reaps-hung-job-and-cancels-timers ()
  "Hung Git jobs are killed on timeout, free slots/stderr, and cancel timers."
  (skip-unless (executable-find "git"))
  (herdr-sanctum-test-with-clean-state
    (let* ((count-stderr-pipes
            (lambda ()
              (cl-count-if (lambda (p)
                             (string-prefix-p "herdr-sanctum-git-stderr"
                                              (process-name p)))
                           (process-list))))
           (blocker-procs nil))
      (unwind-protect
          (progn
            ;; 1. Hung job times out, invokes `:on-error', frees slot for queued job,
            ;;    and cleans up stderr pipe.
            (dotimes (i (1- herdr-sanctum-git--max-concurrency))
              (let ((p (make-process :name (format "herdr-timeout-block-%d" i)
                                     :buffer nil :command '("sleep" "5")
                                     :connection-type 'pipe :noquery t)))
                (push (cons (format "tblock:%d" i) p) blocker-procs)
                (puthash (format "tblock:%d" i) p herdr-sanctum-git--inflight)))
            (let ((herdr-sanctum-git-command-timeout 0.15)
                  (hung-exit nil)
                  (hung-err-count 0)
                  (hung-err-job nil)
                  (next-code nil)
                  (next-out nil))
              (herdr-sanctum-git--enqueue-git
               :key "timeout:hung"
               :name "herdr-sanctum-git-hung"
               :args '("hash-object" "--stdin")
               :on-exit (lambda (&rest _) (setq hung-exit t))
               :on-error (lambda (j)
                           (cl-incf hung-err-count)
                           (setq hung-err-job j)))
              (herdr-sanctum-git--enqueue-git
               :key "timeout:queued"
               :name "herdr-sanctum-git-queued"
               :args '("--version")
               :on-exit (lambda (code out _p _j)
                          (setq next-code code next-out out)))
              (let* ((hung-proc (gethash "timeout:hung" herdr-sanctum-git--inflight))
                     (timer (process-get hung-proc 'herdr-sanctum-git-timer)))
                (should (process-live-p hung-proc))
                (should (timerp timer))
                (should (memq timer timer-list))
                (should (= (length herdr-sanctum-git--low-queue) 1))
                (should (herdr-sanctum-test--wait-until
                         (lambda () (and (= hung-err-count 1) (integerp next-code)))
                         3.0))
                (accept-process-output nil 0.05)
                (should-not hung-exit)
                (should (= hung-err-count 1))
                (should (equal (plist-get hung-err-job :key) "timeout:hung"))
                (should-not (process-live-p hung-proc))
                (should-not (gethash "timeout:hung" herdr-sanctum-git--inflight))
                (should (zerop next-code))
                (should (string-search "git version" next-out))
                (should (zerop (funcall count-stderr-pipes)))
                (should-not (memq timer timer-list))))
            ;; Clear blockers before testing sentinel & clear timer cancellation.
            (dolist (entry blocker-procs)
              (remhash (car entry) herdr-sanctum-git--inflight)
              (when (process-live-p (cdr entry)) (delete-process (cdr entry))))
            (setq blocker-procs nil)
            ;; 2. Normal exit cancels its timeout timer.
            (let ((herdr-sanctum-git-command-timeout 30.0)
                  (done-code nil))
              (herdr-sanctum-git--start-job
               (list :key "timeout:fast"
                     :name "herdr-sanctum-git-fast"
                     :args '("--version")
                     :on-exit (lambda (code _out _p _j) (setq done-code code))))
              (let* ((proc (gethash "timeout:fast" herdr-sanctum-git--inflight))
                     (timer (and proc (process-get proc 'herdr-sanctum-git-timer))))
                (should (herdr-sanctum-test--wait-until
                         (lambda () (integerp done-code)) 3.0))
                (should (zerop done-code))
                (when timer
                  (should-not (memq timer timer-list)))))
            ;; 3. `herdr-sanctum-git-clear' cancels active timeout timers and stderr pipes.
            (let ((herdr-sanctum-git-command-timeout 30.0))
              (herdr-sanctum-git--start-job
               (list :key "timeout:clear"
                     :name "herdr-sanctum-git-clear-timer"
                     :args '("hash-object" "--stdin")
                     :on-exit #'ignore))
              (let* ((proc (gethash "timeout:clear" herdr-sanctum-git--inflight))
                     (timer (process-get proc 'herdr-sanctum-git-timer)))
                (should (timerp timer))
                (should (memq timer timer-list))
                (should (= (funcall count-stderr-pipes) 1))
                (herdr-sanctum-git-clear)
                (should-not (memq timer timer-list))
                (should (zerop (funcall count-stderr-pipes)))))
            ;; 4. Nil timeout disables timer arming.
            (let ((herdr-sanctum-git-command-timeout nil))
              (herdr-sanctum-git--start-job
               (list :key "timeout:nil"
                     :name "herdr-sanctum-git-no-timer"
                     :args '("hash-object" "--stdin")
                     :on-exit #'ignore))
              (let ((proc (gethash "timeout:nil" herdr-sanctum-git--inflight)))
                (should (process-live-p proc))
                (should-not (process-get proc 'herdr-sanctum-git-timer)))
              (herdr-sanctum-git-clear)))
        (dolist (entry blocker-procs)
          (remhash (car entry) herdr-sanctum-git--inflight)
          (when (and (processp (cdr entry)) (process-live-p (cdr entry)))
            (delete-process (cdr entry))))))))

(ert-deftest herdr-sanctum-git-test/monorepo-subdir-dedup-worktrees-and-deletion ()
  "Subdirs share one poll/cache entry; worktrees stay distinct; deleted subdirs evict."
  (skip-unless (executable-find "git"))
  (herdr-sanctum-test-with-clean-state
    (herdr-sanctum-test-with-temp-dir root-tmp
      (let* ((repo (expand-file-name "mono" root-tmp))
             (norm-repo (file-name-as-directory (expand-file-name repo)))
             (api (expand-file-name "packages/api" repo))
             (web (expand-file-name "packages/web" repo))
             (docs (expand-file-name "docs" repo))
             (never-polled (expand-file-name "packages/other" repo))
             (link-web (expand-file-name "link-web" root-tmp))
             (wt (expand-file-name "mono-wt" root-tmp))
             (wt-src (expand-file-name "src" wt))
             (multi (expand-file-name "multi-parent" root-tmp))
             (norm-api (file-name-as-directory (expand-file-name api)))
             (norm-web (file-name-as-directory (expand-file-name web)))
             (norm-docs (file-name-as-directory (expand-file-name docs)))
             (norm-wt (file-name-as-directory (expand-file-name wt)))
             (api-file (herdr-sanctum-test--init-git-repo
                        repo "packages/api/main.el" "a1\na2\n" "main"))
             (status-starts 0)
             (diffstat-starts 0)
             (orig-start (symbol-function 'herdr-sanctum-git--start-job))
             (hook-dirs nil)
             (hook-events nil)
             (hook-fn (lambda (dir info)
                        (push dir hook-dirs)
                        (push (cons dir info) hook-events))))
        (make-directory web t)
        (make-directory docs t)
        (make-directory never-polled t)
        (make-symbolic-link web link-web)
        (should (zerop (call-process "git" nil nil nil
                                     "-C" repo "worktree" "add" "-b" "wt-branch" wt)))
        (make-directory wt-src t)
        (herdr-sanctum-test--init-git-repo (expand-file-name "sub1" multi) "s.txt" "s\n")
        (with-temp-file api-file (insert "a1\na2-mod\na3\n"))
        (unwind-protect
            (progn
              (add-hook 'herdr-sanctum-git-update-hook hook-fn)
              ;; 1. Root resolution handles subdirs, worktrees, non-git multi-repo, remote.
              (should (equal (herdr-sanctum-git--find-repo-root repo) norm-repo))
              (should (equal (herdr-sanctum-git--find-repo-root api) norm-repo))
              (should (equal (herdr-sanctum-git--find-repo-root web) norm-repo))
              (should (equal (herdr-sanctum-git--find-repo-root wt) norm-wt))
              (should (equal (herdr-sanctum-git--find-repo-root wt-src) norm-wt))
              (should-not (herdr-sanctum-git--find-repo-root multi))
              (should-not (herdr-sanctum-git--find-repo-root "/ssh:host:/srv/app"))
              ;; 2. 3 workspaces in 1 monorepo via forced refresh-snapshot launch 1 status.
              (let ((snap (herdr-sanctum-test-make-snapshot
                           :focused-ws "w1" :focused-tab "w1:t1" :focused-pane "w1:p1"
                           :workspaces '(((workspace_id . "w1") (active_tab_id . "w1:t1"))
                                         ((workspace_id . "w2") (active_tab_id . "w2:t1"))
                                         ((workspace_id . "w3") (active_tab_id . "w3:t1")))
                           :tabs '(((tab_id . "w1:t1") (workspace_id . "w1"))
                                   ((tab_id . "w2:t1") (workspace_id . "w2"))
                                   ((tab_id . "w3:t1") (workspace_id . "w3")))
                           :panes `(((pane_id . "w1:p1") (workspace_id . "w1")
                                     (tab_id . "w1:t1") (focused . t) (foreground_cwd . ,api))
                                    ((pane_id . "w2:p1") (workspace_id . "w2")
                                     (tab_id . "w2:t1") (foreground_cwd . ,web))
                                    ((pane_id . "w3:p1") (workspace_id . "w3")
                                     (tab_id . "w3:t1") (foreground_cwd . ,docs))))))
                (cl-letf (((symbol-function 'herdr-sanctum-git--start-job)
                           (lambda (job)
                             (pcase (plist-get job :name)
                               ("herdr-sanctum-git-status" (cl-incf status-starts))
                               ("herdr-sanctum-git-diffstat" (cl-incf diffstat-starts)))
                             (funcall orig-start job))))
                  (herdr-sanctum-git-refresh-snapshot snap t)
                  (should (herdr-sanctum-test--wait-until
                           (lambda ()
                             (let ((i (herdr-sanctum-git-info-for-dir api)))
                               (and i (integerp (plist-get i :added))
                                    (= (hash-table-count herdr-sanctum-git--inflight) 0))))
                           4.0))
                  (should (= status-starts 1))
                  (should (= diffstat-starts 1))))
              ;; Shared `eq' cache object across all subdirs, symlink, and unpolled subdir.
              (let ((info-api (herdr-sanctum-git-info-for-dir api))
                    (info-web (herdr-sanctum-git-info-for-dir web))
                    (info-docs (herdr-sanctum-git-info-for-dir docs))
                    (info-root (herdr-sanctum-git-info-for-dir repo))
                    (info-link (herdr-sanctum-git-info-for-dir link-web))
                    (info-other (herdr-sanctum-git-info-for-dir never-polled)))
                (should info-api)
                (should (eq info-web info-api))
                (should (eq info-docs info-api))
                (should (eq info-root info-api))
                (should (eq info-link info-api))
                (should (eq info-other info-api))
                (should (equal (plist-get info-api :root) norm-repo))
                (should (eql (plist-get info-api :added) 2))
                (should (eql (plist-get info-api :removed) 1)))
              (should (member norm-api hook-dirs))
              (should (member norm-web hook-dirs))
              (should (member norm-docs hook-dirs))
              ;; 3. Concurrent direct `fetch-dir' across sibling subdirs shares 1 inflight job.
              (setq status-starts 0)
              (herdr-sanctum-git-clear)
              (cl-letf (((symbol-function 'herdr-sanctum-git--start-job)
                         (lambda (job)
                           (when (equal (plist-get job :name) "herdr-sanctum-git-status")
                             (cl-incf status-starts))
                           (funcall orig-start job))))
                (herdr-sanctum-git-fetch-dir web nil nil)
                (herdr-sanctum-git-fetch-dir docs nil nil)
                (herdr-sanctum-git-fetch-dir api nil t)
                (should (herdr-sanctum-test--wait-until
                         (lambda ()
                           (let ((i (herdr-sanctum-git-info-for-dir api)))
                             (and i (integerp (plist-get i :added))
                                  (= (hash-table-count herdr-sanctum-git--inflight) 0))))
                         4.0))
                (should (= status-starts 1)))
              ;; 4. Worktree subdir stays independent from main repo.
              (herdr-sanctum-git-fetch-dir wt-src t nil)
              (should (herdr-sanctum-test--wait-until
                       (lambda () (herdr-sanctum-git-info-for-dir wt-src)) 3.0))
              (should (equal (plist-get (herdr-sanctum-git-info-for-dir wt-src) :branch)
                             "wt-branch"))
              (should (equal (plist-get (herdr-sanctum-git-info-for-dir api) :branch)
                             "main"))
              ;; 5. Deleting one subdir evicts only that subdir, preserving sibling cache.
              (setq hook-events nil)
              (delete-directory docs t)
              (herdr-sanctum-git-fetch-dir docs nil nil)
              (should-not (herdr-sanctum-git-info-for-dir docs))
              (should-not (gethash norm-docs herdr-sanctum-git--dir-to-root))
              (should (equal hook-events (list (cons norm-docs nil))))
              (should (herdr-sanctum-git-info-for-dir api))
              ;; 6. `herdr-sanctum-git-clear' empties `--dir-to-root'.
              (herdr-sanctum-git-clear)
              (should (= (hash-table-count herdr-sanctum-git--dir-to-root) 0)))
          (remove-hook 'herdr-sanctum-git-update-hook hook-fn))))))

(ert-deftest herdr-sanctum-git-test/enable-remote-exclude-diffstat-and-skip-names ()
  "Guards honor git-enable, skip-remote, exclude-dirs, diffstat-enable, and skip-names."
  (skip-unless (executable-find "git"))
  (herdr-sanctum-test-with-clean-state
    (herdr-sanctum-test-with-temp-dir tmp
      (let* ((repo (expand-file-name "repo" tmp))
             (repo-sub (expand-file-name "sub" repo))
             (repo-sib (expand-file-name "repo-sib" tmp))
             (multi (expand-file-name "multi" tmp))
             (repo-f (herdr-sanctum-test--init-git-repo repo "a.txt" "a1\n" "main"))
             (multi-a (expand-file-name "alpha" multi))
             (multi-skip (expand-file-name "custom_skip" multi)))
        (make-directory repo-sub t)
        (herdr-sanctum-test--init-git-repo repo-sib "b.txt" "b1\n" "main")
        (let ((ma-f (herdr-sanctum-test--init-git-repo multi-a "m.txt" "m1\n" "main")))
          (with-temp-file ma-f (insert "m1\nm2\n")))
        (herdr-sanctum-test--init-git-repo multi-skip "c.txt" "c1\n" "main")
        (with-temp-file repo-f (insert "a1\na2\n"))
        ;; 1. `herdr-sanctum-git-enable' nil makes fetch-dir and refresh-snapshot no-ops.
        (let ((herdr-sanctum-git-enable nil)
              (snap (herdr-sanctum-test-make-snapshot :dir repo)))
          (herdr-sanctum-git-fetch-dir repo t t)
          (herdr-sanctum-git-refresh-snapshot snap t)
          (should (= (hash-table-count herdr-sanctum-git--inflight) 0))
          (should-not (herdr-sanctum-git-info-for-dir repo)))
        ;; 2. `herdr-sanctum-git-skip-remote' skips remote paths with zero TRAMP I/O.
        (let ((herdr-sanctum-git-skip-remote t)
              (stat-calls 0)
              (orig-dir-p (symbol-function 'file-directory-p)))
          (cl-letf (((symbol-function 'file-directory-p)
                     (lambda (p)
                       (when (string-prefix-p "/ssh:" p) (cl-incf stat-calls))
                       (funcall orig-dir-p p))))
            (herdr-sanctum-git-fetch-dir "/ssh:example.com:/srv/app" t t)
            (should-not (herdr-sanctum-git-info-for-dir "/ssh:example.com:/srv/app"))
            (should (= stat-calls 0))
            (should (= (hash-table-count herdr-sanctum-git--inflight) 0))))
        ;; 3. `herdr-sanctum-git-exclude-dirs': prefix, regexp, and predicate.
        (let ((herdr-sanctum-git-exclude-dirs (list repo)))
          (herdr-sanctum-git-fetch-dir repo t nil)
          (herdr-sanctum-git-fetch-dir repo-sub t nil)
          (should (= (hash-table-count herdr-sanctum-git--inflight) 0))
          ;; Sibling sharing a string prefix (`repo-sib' vs `repo/') is NOT excluded.
          (herdr-sanctum-git-fetch-dir repo-sib t nil)
          (should (herdr-sanctum-test--wait-until
                   (lambda () (herdr-sanctum-git-info-for-dir repo-sib)) 3.0))
          (should-not (herdr-sanctum-git-info-for-dir repo)))
        (herdr-sanctum-git-clear)
        (let ((herdr-sanctum-git-exclude-dirs '((regexp . "/repo/sub/"))))
          (herdr-sanctum-git-fetch-dir repo-sub t nil)
          (should (= (hash-table-count herdr-sanctum-git--inflight) 0)))
        (let ((herdr-sanctum-git-exclude-dirs
               (list (lambda (d) (string-search "/repo/" d)))))
          (herdr-sanctum-git-fetch-dir repo t nil)
          (should (= (hash-table-count herdr-sanctum-git--inflight) 0)))
        ;; 4. `herdr-sanctum-git-diffstat-enable' nil skips diffstat on single & multi repos.
        (herdr-sanctum-git-clear)
        (let ((herdr-sanctum-git-diffstat-enable nil)
              (herdr-sanctum-git-cache-ttl 60.0)
              (diffstat-jobs 0)
              (orig-start (symbol-function 'herdr-sanctum-git--start-job)))
          (cl-letf (((symbol-function 'herdr-sanctum-git--start-job)
                     (lambda (job)
                       (when (member (plist-get job :name)
                                     '("herdr-sanctum-git-diffstat"
                                       "herdr-sanctum-git-subdiff"))
                         (cl-incf diffstat-jobs))
                       (funcall orig-start job))))
            (herdr-sanctum-git-fetch-dir repo t t)
            (should (herdr-sanctum-test--wait-until
                     (lambda ()
                       (and (herdr-sanctum-git-info-for-dir repo)
                            (= (hash-table-count herdr-sanctum-git--inflight) 0)))
                     3.0))
            (let ((info (herdr-sanctum-git-info-for-dir repo)))
              (should (eql (plist-get info :dirty) 1))
              (should-not (plist-get info :added))
              (should-not (plist-get info :removed)))
            ;; TTL upgrade request with `with-diffstat' still skips diffstat.
            (herdr-sanctum-git-fetch-dir repo nil t)
            (should (= (hash-table-count herdr-sanctum-git--inflight) 0))
            ;; Multi-repo with `with-diffstat' also skips subdiff jobs.
            (herdr-sanctum-git-fetch-dir multi t t)
            (should (herdr-sanctum-test--wait-until
                     (lambda ()
                       (let ((i (herdr-sanctum-git-info-for-dir multi)))
                         (and i (plist-get i :multi-repo)
                              (= (hash-table-count herdr-sanctum-git--inflight) 0))))
                     4.0))
            (should (= diffstat-jobs 0))
            (let ((alpha-repo (cl-find "alpha"
                                       (plist-get (herdr-sanctum-git-info-for-dir multi) :repos)
                                       :key (lambda (r) (plist-get r :name))
                                       :test #'equal)))
              (should alpha-repo)
              (should (eql (plist-get alpha-repo :dirty) 1))
              (should-not (plist-get alpha-repo :added))
              (should-not (plist-get alpha-repo :removed)))))
        ;; 5. `herdr-sanctum-git-subrepo-skip-names' defcustom & alias work.
        (should (eq (indirect-variable 'herdr-sanctum-git--subrepo-skip-names)
                    'herdr-sanctum-git-subrepo-skip-names))
        (let ((herdr-sanctum-git-subrepo-skip-names
               (cons "custom_skip" herdr-sanctum-git-subrepo-skip-names)))
          (should (equal (mapcar #'car (herdr-sanctum-git--discover-subrepos multi))
                         '("alpha"))))))))

(provide 'herdr-sanctum-git-test)
;;; herdr-sanctum-git-test.el ends here
