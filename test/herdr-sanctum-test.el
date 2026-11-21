;;; herdr-sanctum-test.el --- ERT test suite for herdr-sanctum -*- lexical-binding: t; -*-

;;; Commentary:

;; Standalone ERT regression tests for `herdr-sanctum'.
;; Uses real temporary local Unix sockets and temporary Git repositories.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'json)
(require 'svg)
(require 'herdr-sanctum)
(require 'herdr-sanctum-ime)
(require 'herdr-sanctum-tab-routing)

(when (boundp 'native-comp-enable-subr-trampolines)
  (setq native-comp-enable-subr-trampolines nil))

(defvar-local herdr-sanctum-test--live nil
  "Buffer-local liveness flag for the mock terminal backend.")

(defvar herdr-sanctum-test--root
  (make-temp-file "herdr-sanctum-test-" t)
  "Temporary directory for sockets and fixtures.")

(add-hook 'kill-emacs-hook
          (lambda ()
            (when (file-directory-p herdr-sanctum-test--root)
              (ignore-errors (delete-directory herdr-sanctum-test--root t)))))

(defun herdr-sanctum-test--wait-until (pred &optional timeout)
  "Pump process output until PRED returns non-nil or TIMEOUT seconds elapse."
  (let ((deadline (+ (float-time) (or timeout 2.0))))
    (while (and (< (float-time) deadline) (not (funcall pred)))
      (accept-process-output nil 0.02))
    (funcall pred)))

;;; Git & Formatting Tests

(ert-deftest herdr-sanctum-test/git-parsers-and-elapsed ()
  "Git porcelain v2 parser, numstat parser, and elapsed duration formatter."
  (let ((parsed (herdr-sanctum-git-parse-status
                 (concat "# branch.oid abc123\n"
                         "# branch.head feat/sanctum\n"
                         "# branch.ab +2 -1\n"
                         "1 M. N... 100644 100644 100644 aaa bbb src/a.el\n"
                         "1 .M N... 100644 100644 100644 aaa bbb src/b.el\n"
                         "? notes/todo.org\n"))))
    (should (equal (plist-get parsed :branch) "feat/sanctum"))
    (should (eql (plist-get parsed :ahead) 2))
    (should (eql (plist-get parsed :behind) 1))
    (should (eql (plist-get parsed :staged) 1))
    (should (eql (plist-get parsed :unstaged) 1))
    (should (eql (plist-get parsed :untracked) 1))
    (should (eql (plist-get parsed :dirty) 3)))
  (should (equal (herdr-sanctum-git-parse-numstat
                  "12\t4\tsrc/a.el\n-\t-\timg.png\n3\t1\tsrc/b.el\n")
                 '(15 . 5)))
  (should (equal (herdr-sanctum-format-elapsed 15) "<1m"))
  (should (equal (herdr-sanctum-format-elapsed 420) "7m"))
  (should (equal (herdr-sanctum-format-elapsed 7200) "2h"))
  (should (equal (herdr-sanctum-format-elapsed 172800) "2d")))

(ert-deftest herdr-sanctum-test/git-async-real-repo ()
  "Async Git fetch parses a real temporary repository and fires update hook."
  (skip-unless (executable-find "git"))
  (let* ((repo (make-temp-file "herdr-sanctum-git-" t))
         (updated nil)
         (hook-fn (lambda (dir info)
                    (when (equal dir (file-name-as-directory (expand-file-name repo)))
                      (setq updated info)))))
    (unwind-protect
        (progn
          (call-process "git" nil nil nil "-C" repo "init" "-b" "main")
          (with-temp-file (expand-file-name "README.md" repo)
            (insert "hello\n"))
          (add-hook 'herdr-sanctum-git-update-hook hook-fn)
          (herdr-sanctum-git-fetch-dir repo t nil)
          (should (herdr-sanctum-test--wait-until (lambda () updated) 3.0))
          (should (equal (plist-get updated :branch) "main"))
          (should (eql (plist-get updated :untracked) 1))
          (should (eql (plist-get updated :dirty) 1))
          (should (equal (herdr-sanctum-git-info-for-dir repo) updated)))
      (remove-hook 'herdr-sanctum-git-update-hook hook-fn)
      (herdr-sanctum-git-clear)
      (when (file-directory-p repo)
        (delete-directory repo t)))))

;;; Theme & SVG Tests

(ert-deftest herdr-sanctum-test/theme-palette-and-badges ()
  "Theme palette derives valid colors for dark/light modes and renders SVG badges."
  (let ((herdr-sanctum-palette-function #'herdr-sanctum-default-palette-color))
    (cl-letf (((symbol-function 'herdr-sanctum--background-mode) (lambda () 'dark)))
      (let ((bg (herdr-sanctum-palette-color 'bg-chrome)))
        (should (stringp bg))
        (should (string-prefix-p "#" bg))))
    (cl-letf (((symbol-function 'herdr-sanctum--background-mode) (lambda () 'light)))
      (let ((bg (herdr-sanctum-palette-color 'bg-chrome)))
        (should (stringp bg))
        (should (string-prefix-p "#" bg)))))
  ;; Custom palette function override
  (let ((herdr-sanctum-palette-function
         (lambda (token _fb) (if (eq token 'accent) "#123456" "#abcdef"))))
    (should (equal (herdr-sanctum-palette-color 'accent) "#123456")))
  ;; SVG badges (builtin, svg-files, unicode)
  (cl-letf (((symbol-function 'herdr-sanctum-svg-available-p) (lambda () t)))
    (clrhash herdr-sanctum--svg-cache)
    (let ((sp-badge (herdr-sanctum-space-badge "skillshare" "w1")))
      (should (eq (car-safe (get-text-property 0 'display sp-badge)) 'image)))
    (let ((herdr-sanctum-space-badge-style 'none))
      (should (equal (herdr-sanctum-space-badge "skillshare" "w1") "")))
    (let ((herdr-sanctum-agent-icon-style 'builtin))
      (dolist (ag '("pi" "claude" "codex" "gemini" "opencode"))
        (dolist (st '("blocked" "done" "working" "idle"))
          (let ((ag-badge (herdr-sanctum-agent-badge ag st)))
            (should (eq (car-safe (get-text-property 0 'display ag-badge)) 'image))))))
    (let ((herdr-sanctum-agent-icon-style 'svg-files))
      (clrhash herdr-sanctum--svg-cache)
      (should (file-directory-p herdr-sanctum-agent-icon-directory))
      (dolist (ag '("pi" "claude" "codex" "gemini" "opencode" "unknown-agent"))
        (let ((ag-badge (herdr-sanctum-agent-badge ag "working")))
          (should (eq (car-safe (get-text-property 0 'display ag-badge)) 'image)))))
    (let ((herdr-sanctum-agent-icon-style 'unicode))
      (let ((ag-badge (herdr-sanctum-agent-badge "pi" "blocked")))
        (should (equal (substring-no-properties ag-badge) "◆"))
        (should-not (get-text-property 0 'display ag-badge))))
    (should (eq (car-safe (get-text-property 0 'display (herdr-sanctum-row-accent t "#24283b"))) 'image))
    (should (eq (car-safe (get-text-property 0 'display (herdr-sanctum-row-accent nil nil t))) 'image))
    (should (equal (get-text-property 0 'display (herdr-sanctum-row-accent nil)) '(space :width (8))))
    (clrhash herdr-sanctum--svg-cache)))

;;; Terminal Backend Abstraction Tests

(ert-deftest herdr-sanctum-test/custom-terminal-backend ()
  "Pluggable terminal backend protocol drives `herdr-sanctum' without Ghostel."
  (let* ((started nil)
         (resized 0)
         (setup-called nil)
         (herdr-sanctum-terminal-backends (copy-alist herdr-sanctum-terminal-backends))
         (herdr-sanctum-terminal-backend 'test-backend)
         (herdr-sanctum-display-mode 'current)
         (herdr-sanctum-program "sh")
         (herdr-sanctum-socket-path (expand-file-name "no-such.sock" herdr-sanctum-test--root))
         (herdr-sanctum-sidebar-default-open t)
         (herdr-sanctum-drawer-default-open nil)
         (herdr-sanctum-sidebar--enabled nil)
         (herdr-sanctum-drawer--enabled nil))
    (herdr-sanctum-register-terminal-backend
     'test-backend
     :start (lambda (buf prog _args)
              (setq started (cons (buffer-name buf) prog))
              (with-current-buffer buf
                (setq-local herdr-sanctum-test--live t)))
     :live-p (lambda (buf)
               (buffer-local-value 'herdr-sanctum-test--live buf))
     :setup (lambda (_buf) (setq setup-called t))
     :resize (lambda (_win) (cl-incf resized))
     :mode-banner (lambda (_buf) " TEST-BANNER "))
    (unwind-protect
        (progn
          (herdr-sanctum)
          (should started)
          (should (equal (car started) herdr-sanctum-buffer-name))
          (should setup-called)
          (should (> resized 0))
          (should (herdr-sanctum-in-sanctum-p))
          (should (equal (herdr-sanctum-header-line) " TEST-BANNER "))
          (should (herdr-sanctum-sidebar-visible-p))
          (should-not (herdr-sanctum-drawer-visible-p)))
      (herdr-sanctum-disconnect)
      (when-let* ((buf (get-buffer herdr-sanctum-buffer-name)))
        (kill-buffer buf)))))

;;; Socket, CWD Sync, Badge & Hooks Tests

(ert-deftest herdr-sanctum-test/socket-cwd-badge-and-hooks ()
  "Real Unix socket syncs CWD, global attention badge, and public hooks."
  (let* ((sock (expand-file-name "herdr-e2e.sock" herdr-sanctum-test--root))
         (project-dir (make-temp-file "herdr-proj-" t))
         (buf (get-buffer-create herdr-sanctum-buffer-name))
         (focus-events nil)
         (status-changes nil)
         (agent-status "working")
         (seq-num 1)
         (server (make-network-process
                  :name "herdr-sanctum-test-server"
                  :server t
                  :family 'local
                  :service sock
                  :coding 'utf-8-unix
                  :filter
                  (lambda (client data)
                    (cond
                     ((string-search "events.subscribe" data)
                      (process-send-string
                       client
                       "{\"id\":\"emacs:sub\",\"result\":{\"type\":\"subscription_started\"}}\n"))
                     ((string-search "session.snapshot" data)
                      (process-send-string
                       client
                       (concat (json-serialize
                                `((id . "emacs:snap")
                                  (result . ((type . "session_snapshot")
                                             (snapshot . ((focused_workspace_id . "w1")
                                                          (focused_tab_id . "w1:t1")
                                                          (focused_pane_id . "w1:p2")
                                                          (panes . [((pane_id . "w1:p2")
                                                                     (foreground_cwd . ,project-dir))])
                                                          (agents . [((pane_id . "w1:p1")
                                                                      (agent . "pi")
                                                                      (agent_status . ,agent-status)
                                                                      (state_change_seq . ,seq-num))
                                                                     ((pane_id . "w1:p2")
                                                                      (agent . "claude")
                                                                      (agent_status . "done")
                                                                      (state_change_seq . 1))])))))))
                               "\n")))))))
         (herdr-sanctum-socket-path sock)
         (focus-hook (lambda (dir pane ws tab)
                       (push (list dir pane ws tab) focus-events)))
         (status-hook (lambda (agent old new)
                        (push (list (alist-get 'pane_id agent) old new) status-changes))))
    (unwind-protect
        (progn
          (herdr-sanctum-disconnect)
          (add-hook 'herdr-sanctum-focus-change-hook focus-hook)
          (add-hook 'herdr-sanctum-agent-status-change-hook status-hook)
          (herdr-sanctum-ensure-connected)
          (should (herdr-sanctum-test--wait-until
                   (lambda ()
                     (equal (buffer-local-value 'default-directory buf)
                            (file-name-as-directory project-dir)))))
          (should (= (length focus-events) 1))
          (should (equal (nth 1 (car focus-events)) "w1:p2"))
          ;; Transition agent w1:p1 from working -> blocked
          (setq agent-status "blocked"
                seq-num 2)
          (herdr-sanctum-request-snapshot t)
          (should (herdr-sanctum-test--wait-until
                   (lambda () (consp status-changes))))
          (should (equal (car status-changes) '("w1:p1" "working" "blocked")))
          (should (stringp herdr-sanctum--mode-line-string))
          (should (string-search "◆1" herdr-sanctum--mode-line-string))
          (should (string-search "✓1" herdr-sanctum--mode-line-string))
          ;; Badge hidden when inside *sanctum* with sidebar visible, shown otherwise.
          (with-selected-window (selected-window)
            (switch-to-buffer buf)
            (should (equal (herdr-sanctum-mode-line) herdr-sanctum--mode-line-string))
            (herdr-sanctum-sidebar-show)
            (should-not (herdr-sanctum-mode-line))
            (when-let* ((sw (get-buffer-window herdr-sanctum-sidebar-buffer-name)))
              (delete-window sw))
            (switch-to-buffer "*scratch*")
            (should (equal (herdr-sanctum-mode-line) herdr-sanctum--mode-line-string))))
      (remove-hook 'herdr-sanctum-focus-change-hook focus-hook)
      (remove-hook 'herdr-sanctum-agent-status-change-hook status-hook)
      (herdr-sanctum-disconnect)
      (should-not (process-live-p herdr-sanctum--snap-process))
      (should-not (timerp herdr-sanctum--snap-timeout-timer))
      (when (process-live-p server) (delete-process server))
      (when (file-exists-p sock) (delete-file sock))
      (when (buffer-live-p buf) (kill-buffer buf))
      (when (file-directory-p project-dir) (delete-directory project-dir t)))))

;;; Sidebar, Drawer Registry & Fuzzy Jump Tests

(ert-deftest herdr-sanctum-test/sidebar-drawer-and-rpc ()
  "Sidebar, custom drawer views, and RPC dispatch work end-to-end."
  (let* ((sock (expand-file-name "herdr-ui.sock" herdr-sanctum-test--root))
         (recorded nil)
         (snapshot
          '((focused_workspace_id . "w1")
            (focused_tab_id . "w1:t2")
            (focused_pane_id . "w1:p2")
            (workspaces . [((workspace_id . "w1") (number . 1) (label . "def")
                            (focused . t) (tab_count . 3) (active_tab_id . "w1:t2")
                            (agent_status . "blocked"))
                           ((workspace_id . "w2") (number . 2) (label . "bpm")
                            (focused . :false) (tab_count . 2) (active_tab_id . "w2:t4")
                            (agent_status . "done"))])
            (tabs . [((tab_id . "w1:t1") (workspace_id . "w1") (number . 1)
                      (label . "home") (focused . :false) (pane_count . 1) (agent_status . "unknown"))
                     ((tab_id . "w1:t2") (workspace_id . "w1") (number . 2)
                      (label . "dotfiles") (focused . t) (pane_count . 2) (agent_status . "working"))
                     ((tab_id . "w1:t3") (workspace_id . "w1") (number . 3)
                      (label . "ops") (focused . :false) (pane_count . 1) (agent_status . "blocked"))
                     ((tab_id . "w2:t4") (workspace_id . "w2") (number . 1)
                      (label . "design") (focused . :false) (pane_count . 1) (agent_status . "done"))
                     ((tab_id . "w2:t5") (workspace_id . "w2") (number . 2)
                      (label . "impl") (focused . :false) (pane_count . 3) (agent_status . "idle"))])
            (panes . [])
            (agents . [((pane_id . "w1:p2") (workspace_id . "w1") (tab_id . "w1:t2")
                        (agent . "pi") (agent_status . "working") (state_change_seq . 10)
                        (tokens . ((topic . "dotfiles"))))
                       ((pane_id . "w1:p3") (workspace_id . "w1") (tab_id . "w1:t3")
                        (agent . "pi") (agent_status . "blocked") (state_change_seq . 8)
                        (tokens . ((topic . "build-fix"))))
                       ((pane_id . "w2:p4") (workspace_id . "w2") (tab_id . "w2:t4")
                        (agent . "pi") (agent_status . "done") (state_change_seq . 12)
                        (tokens . ((topic . "design"))))
                       ((pane_id . "w2:p5") (workspace_id . "w2") (tab_id . "w2:t5")
                        (agent . "pi") (agent_status . "idle") (state_change_seq . 20))])))
         (server (make-network-process
                  :name "herdr-sanctum-ui-server"
                  :server t
                  :family 'local
                  :service sock
                  :coding 'utf-8-unix
                  :filter
                  (lambda (client data)
                    (dolist (line (split-string data "\n" t))
                      (let* ((req (json-parse-string line :object-type 'alist :array-type 'list
                                                     :null-object nil :false-object nil))
                             (id (alist-get 'id req))
                             (method (alist-get 'method req)))
                        (push req recorded)
                        (cond
                         ((equal method "events.subscribe")
                          (process-send-string
                           client
                           (concat (json-serialize `((id . ,id) (result . ((type . "subscription_started"))))) "\n")))
                         ((equal method "session.snapshot")
                          (process-send-string
                           client
                           (concat (json-serialize `((id . ,id) (result . ((type . "session_snapshot")
                                                                           (snapshot . ,snapshot))))) "\n")))
                         (t
                          (process-send-string
                           client
                           (concat (json-serialize `((id . ,id) (result . ((ok . t))))) "\n")))))))))
         (herdr-sanctum-socket-path sock)
         (herdr-sanctum-drawer-views (copy-alist herdr-sanctum-drawer-views))
         (herdr-sanctum-drawer-enabled-views '(overview changes files notes))
         (custom-action-hit nil)
         (wait-rpc (lambda (method)
                     (herdr-sanctum-test--wait-until
                      (lambda ()
                        (seq-find (lambda (r) (equal (alist-get 'method r) method)) recorded)))
                     (seq-find (lambda (r) (equal (alist-get 'method r) method)) recorded))))
    (herdr-sanctum-register-drawer-view
     'notes
     :label "Notes"
     :render-fn (lambda (_ctx)
                  (let ((start (point)))
                    (insert "Custom Notes View\n")
                    (add-text-properties start (1- (point))
                                         '(herdr-sanctum-drawer-action custom-ping
                                           herdr-sanctum-drawer-value 42))))
     :action-fn (lambda (act val) (setq custom-action-hit (cons act val))))
    (unwind-protect
        (progn
          (herdr-sanctum-disconnect)
          (clrhash herdr-sanctum-sidebar--expanded-spaces)
          (setq herdr-sanctum-sidebar--enabled 'on)
          (herdr-sanctum-sidebar-show t)
          (herdr-sanctum-ensure-connected)
          (should (herdr-sanctum-test--wait-until #'herdr-sanctum-snapshot))
          (with-current-buffer (get-buffer herdr-sanctum-sidebar-buffer-name)
            (let ((text (buffer-string)))
              (should (string-prefix-p "Spaces" text))
              (should (string-search "def" text))
              (should (string-search "bpm" text))
              (should (string-search "Agents" text))
              (should (string-search "Blocked 1" text))
              (should (string-search "Working 1" text))
              (should (string-search "Done 1" text))
              (should (string-search "Idle 1" text))
              (should-not (herdr-sanctum-sidebar--find-item-pos 'tab "w1:t2")))
            ;; Expand/collapse via right/left keys
            (herdr-sanctum-sidebar-right)
            (should (herdr-sanctum-sidebar--find-item-pos 'tab "w1:t2"))
            (should (string-search "2p" (buffer-string)))
            (herdr-sanctum-sidebar-right)
            (should (equal (plist-get (herdr-sanctum-sidebar--item-at) :tab-id) "w1:t1"))
            (herdr-sanctum-sidebar-left)
            (should (eq (plist-get (herdr-sanctum-sidebar--item-at) :kind) 'workspace))
            ;; Next agent + peek dispatches pane.focus
            (herdr-sanctum-sidebar-next-agent)
            (should (equal (plist-get (herdr-sanctum-sidebar--item-at) :pane-id) "w1:p3"))
            (setq recorded nil)
            (herdr-sanctum-sidebar-peek)
            (let ((req (funcall wait-rpc "pane.focus")))
              (should req)
              (should (equal (alist-get 'pane_id (alist-get 'params req)) "w1:p3")))
            ;; Fold-all toggle
            (herdr-sanctum-sidebar-toggle-fold-all)
            (should-not (herdr-sanctum-sidebar--find-item-pos 'tab "w1:t2"))
            (herdr-sanctum-sidebar-toggle-fold-all)
            (should (herdr-sanctum-sidebar--find-item-pos 'tab "w1:t2"))
            (should (herdr-sanctum-sidebar--find-item-pos 'tab "w2:t5"))
            ;; Move down & rename RPC
            (goto-char (herdr-sanctum-sidebar--find-item-pos 'tab "w1:t2"))
            (setq recorded nil)
            (herdr-sanctum-sidebar-move-down)
            (let ((req (funcall wait-rpc "tab.move")))
              (should req)
              (should (eql (alist-get 'insert_index (alist-get 'params req)) 3)))
            (setq recorded nil)
            (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "notes")))
              (herdr-sanctum-sidebar-rename))
            (let ((req (funcall wait-rpc "tab.rename")))
              (should req)
              (should (equal (alist-get 'label (alist-get 'params req)) "notes"))))
          ;; Fuzzy jump priority: blocked -> done -> working
          (let ((cands (herdr-sanctum--jump-candidates (herdr-sanctum-snapshot))))
            (should (equal (plist-get (cdar cands) :pane-id) "w1:p3"))
            (should (equal (plist-get (cdr (nth 1 cands)) :pane-id) "w2:p4"))
            (should (equal (plist-get (cdr (nth 2 cands)) :pane-id) "w1:p2"))
            (setq recorded nil)
            (cl-letf (((symbol-function 'completing-read)
                       (lambda (_prompt _coll &rest _) (caar cands))))
              (herdr-sanctum-jump))
            (let ((req (funcall wait-rpc "pane.focus")))
              (should req)
              (should (equal (alist-get 'pane_id (alist-get 'params req)) "w1:p3"))))
          ;; Drawer views including dynamically registered `notes' view
          (herdr-sanctum-drawer-open-view 'overview)
          (should (herdr-sanctum-drawer-visible-p))
          (should (string-search "Notes" (herdr-sanctum-header-line-right)))
          (with-current-buffer (get-buffer herdr-sanctum-drawer-buffer-name)
            (should (string-search "AGENTS IN SPACE" (buffer-string)))
            (herdr-sanctum-drawer-cycle-tab)
            (should (eq (herdr-sanctum-drawer-current-tab) 'changes))
            (herdr-sanctum-drawer-cycle-tab)
            (should (eq (herdr-sanctum-drawer-current-tab) 'files))
            (herdr-sanctum-drawer-cycle-tab)
            (should (eq (herdr-sanctum-drawer-current-tab) 'notes))
            (should (string-search "Custom Notes View" (buffer-string)))
            (goto-char (point-min))
            (search-forward "Custom Notes View")
            (beginning-of-line)
            (herdr-sanctum-drawer-activate)
            (should (equal custom-action-hit '(custom-ping . 42))))
          (herdr-sanctum-drawer-toggle)
          (should-not (herdr-sanctum-drawer-visible-p)))
      (herdr-sanctum-disconnect)
      (clrhash herdr-sanctum-sidebar--expanded-spaces)
      (setq herdr-sanctum-sidebar--enabled nil
            herdr-sanctum-drawer--enabled nil
            herdr-sanctum-drawer--tab 'overview)
      (when (process-live-p server) (delete-process server))
      (when (file-exists-p sock) (delete-file sock)))))

;;; Optional Module Tests: IME & Tab Routing

(ert-deftest herdr-sanctum-test/per-pane-ime ()
  "Per-pane IME mode switches on focus, remembers manual toggles, and resets on agent change."
  (let* ((sock (expand-file-name "herdr-ime.sock" herdr-sanctum-test--root))
         (buf (get-buffer-create herdr-sanctum-buffer-name))
         (sub-client nil)
         (focused "w1:p1")
         (server (make-network-process
                  :name "herdr-sanctum-ime-server"
                  :server t
                  :family 'local
                  :service sock
                  :coding 'utf-8-unix
                  :filter
                  (lambda (client data)
                    (cond
                     ((string-search "events.subscribe" data)
                      (setq sub-client client)
                      (process-send-string
                       client
                       "{\"id\":\"emacs:sub\",\"result\":{\"type\":\"subscription_started\"}}\n"))
                     ((string-search "session.snapshot" data)
                      (process-send-string
                       client
                       (concat (json-serialize
                                `((id . "emacs:snap")
                                  (result . ((type . "session_snapshot")
                                             (snapshot . ((focused_pane_id . ,focused)
                                                          (panes . [((pane_id . "w1:p1") (agent . "pi"))
                                                                    ((pane_id . "w1:p2"))])
                                                          (agents . [((pane_id . "w1:p1") (agent . "pi") (agent_status . "idle"))])))))))
                               "\n")))))))
         (dummy-activate (lambda (_name)
                           (setq-local deactivate-current-input-method-function #'ignore)))
         (input-method-alist (cons (list "test-ime" "UTF-8" dummy-activate "漢")
                                   input-method-alist))
         (default-input-method "test-ime")
         (herdr-sanctum-ime-input-method nil)
         (herdr-sanctum-socket-path sock))
    (unwind-protect
        (progn
          (herdr-sanctum-disconnect)
          (with-current-buffer buf
            (deactivate-input-method))
          (herdr-sanctum-ime-mode 1)
          (herdr-sanctum-ensure-connected)
          (should (herdr-sanctum-test--wait-until
                   (lambda ()
                     (equal (buffer-local-value 'current-input-method buf) "test-ime"))))
          ;; Focusing shell pane immediately turns IME off.
          (setq focused "w1:p2")
          (process-send-string
           sub-client
           "{\"event\":\"pane.focused\",\"data\":{\"type\":\"pane_focused\",\"pane_id\":\"w1:p2\",\"workspace_id\":\"w1\"}}\n")
          (should (herdr-sanctum-test--wait-until
                   (lambda () (equal herdr-sanctum-ime--focused-pane-id "w1:p2"))))
          (should-not (buffer-local-value 'current-input-method buf))
          ;; Manual toggle on shell pane is remembered across switches.
          (with-current-buffer buf
            (activate-input-method "test-ime"))
          (should (eq (gethash "w1:p2" herdr-sanctum-ime--pane-states) 'on))
          (herdr-sanctum-ime-sync-pane "w1:p1")
          (with-current-buffer buf
            (deactivate-input-method))
          (should (eq (gethash "w1:p1" herdr-sanctum-ime--pane-states) 'off))
          (herdr-sanctum-ime-sync-pane "w1:p2")
          (should (equal (buffer-local-value 'current-input-method buf) "test-ime"))
          ;; Agent detection clears stale override.
          (with-current-buffer buf
            (deactivate-input-method))
          (herdr-sanctum--handle-event-line
           "{\"event\":\"pane.agent_detected\",\"data\":{\"type\":\"pane_agent_detected\",\"pane_id\":\"w1:p2\",\"agent\":\"pi\",\"released\":false}}")
          (should-not (gethash "w1:p2" herdr-sanctum-ime--pane-states))
          (should (equal (buffer-local-value 'current-input-method buf) "test-ime"))
          ;; Pane close prunes state.
          (herdr-sanctum--handle-event-line
           "{\"event\":\"pane.closed\",\"data\":{\"type\":\"pane_closed\",\"pane_id\":\"w1:p1\"}}")
          (should-not (gethash "w1:p1" herdr-sanctum-ime--pane-states))
          (should-not (gethash "w1:p1" herdr-sanctum-ime--pane-kinds)))
      (herdr-sanctum-ime-mode -1)
      (herdr-sanctum-disconnect)
      (when (process-live-p server) (delete-process server))
      (when (file-exists-p sock) (delete-file sock))
      (when (buffer-live-p buf) (kill-buffer buf)))))

(ert-deftest herdr-sanctum-test/tab-routing ()
  "Declarative tab routing matches rules cleanly and uninstalls without residue."
  (let* ((orig-alist (copy-sequence display-buffer-alist))
         (sanctum-buf (get-buffer-create herdr-sanctum-buffer-name))
         (code-buf (get-buffer-create " *sanctum-test-code*"))
         (tool-buf (get-buffer-create " *sanctum-test-tool*"))
         (popup-buf (get-buffer-create " *sanctum-test-popup*"))
         (herdr-sanctum-tab-routing-rules
          '(("Bench" :modes (eshell-mode))
            ("Folio" :file t :name-regexp "\\` \\*sanctum-test-code\\*\\'"))))
    (unwind-protect
        (progn
          (with-current-buffer tool-buf
            (setq major-mode 'eshell-mode))
          (herdr-sanctum-tab-routing-mode 1)
          (should (= (length display-buffer-alist) (+ (length orig-alist) 2)))
          (save-window-excursion
            (switch-to-buffer sanctum-buf)
            (should (equal (herdr-sanctum-tab-routing-match tool-buf) "Bench"))
            (should (equal (herdr-sanctum-tab-routing-match code-buf) "Folio"))
            (should-not (herdr-sanctum-tab-routing-match popup-buf))
            (should-not (herdr-sanctum-tab-routing-match sanctum-buf)))
          (herdr-sanctum-tab-routing-mode -1)
          (should (equal display-buffer-alist orig-alist)))
      (herdr-sanctum-tab-routing-mode -1)
      (dolist (b (list sanctum-buf code-buf tool-buf popup-buf))
        (when (buffer-live-p b) (kill-buffer b))))))

(ert-deftest herdr-sanctum-test/doctor ()
  "`herdr-sanctum-doctor' renders diagnostics and recommended config."
  (save-window-excursion
    (herdr-sanctum-doctor)
    (with-current-buffer "*herdr-sanctum-doctor*"
      (let ((text (buffer-string)))
        (should (string-search "herdr-sanctum doctor" text))
        (should (string-search "sidebar_start_collapsed = true" text))))
    (kill-buffer "*herdr-sanctum-doctor*")))

(provide 'herdr-sanctum-test)
;;; herdr-sanctum-test.el ends here
