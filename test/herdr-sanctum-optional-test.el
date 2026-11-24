;;; herdr-sanctum-optional-test.el --- Optional modules & terminal backend tests -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT tests covering optional companion modules (`herdr-sanctum-tab-routing'
;; and `herdr-sanctum-ime') and pluggable `herdr-sanctum-terminal' backends.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'face-remap)
(require 'tab-bar)
(require 'herdr-sanctum-test-helper)

(defun herdr-sanctum-optional-test--repo-dir ()
  "Return the repository root directory containing `herdr-sanctum' libraries."
  (file-name-directory
   (or (locate-library "herdr-sanctum-tab-routing")
       default-directory)))

(defun herdr-sanctum-optional-test--emacs-bin ()
  "Return the absolute path to the currently running Emacs binary."
  (expand-file-name invocation-name invocation-directory))

;;; Tab Routing Tests

(ert-deftest herdr-sanctum-optional-test/tab-routing-rules-and-ui-buffers ()
  "Tab routing installs one static entry, honors live `setq', and matches rules."
  (herdr-sanctum-test-with-clean-state
    (let* ((orig-alist (copy-sequence display-buffer-alist))
           (sanctum-buf (get-buffer-create herdr-sanctum-buffer-name))
           (sidebar-buf (get-buffer-create herdr-sanctum-sidebar-buffer-name))
           (drawer-buf (get-buffer-create herdr-sanctum-drawer-buffer-name))
           (code-buf (get-buffer-create " *sanctum-test-code*"))
           (tool-buf (get-buffer-create " *sanctum-test-tool*"))
           (eshell-buf (get-buffer-create "*eshell*"))
           (popup-buf (get-buffer-create " *sanctum-test-popup*"))
           (herdr-sanctum-tab-routing-rules
            '(("Bench" :commands (eshell) :modes (proced-mode)
               :name-regexp "\\`\\*eshell\\*")
              ("Folio" :file t :name-regexp "\\` \\*sanctum-test-code\\*\\'"))))
      (unwind-protect
          (progn
            (with-current-buffer tool-buf
              (setq major-mode 'proced-mode))
            ;; 1. Single static `display-buffer-alist' entry regardless of rule count.
            (herdr-sanctum-tab-routing-mode 1)
            (should (= (length display-buffer-alist) (1+ (length orig-alist))))
            (should (memq herdr-sanctum-tab-routing--entry display-buffer-alist))
            ;; 2. All 3 UI buffers work as source windows and are excluded as targets.
            (dolist (src (list sanctum-buf sidebar-buf drawer-buf))
              (save-window-excursion
                (switch-to-buffer src)
                (should (equal (herdr-sanctum-tab-routing-match tool-buf) "Bench"))
                (should (equal (herdr-sanctum-tab-routing-match code-buf) "Folio"))
                (should (equal (herdr-sanctum-tab-routing-match eshell-buf) "Bench"))
                (should-not (herdr-sanctum-tab-routing-match popup-buf))
                ;; `:commands' matches even while buffer is still in `fundamental-mode'.
                (let ((this-command 'eshell))
                  (should (equal (herdr-sanctum-tab-routing-match popup-buf) "Bench")))
                (dolist (ui-target (list sanctum-buf sidebar-buf drawer-buf))
                  (should-not (herdr-sanctum-tab-routing-match ui-target)))))
            ;; Non-UI source windows do not trigger routing.
            (save-window-excursion
              (switch-to-buffer popup-buf)
              (should-not (herdr-sanctum-tab-routing-match code-buf)))
            ;; 3. Live `setq' updates rules immediately without toggling the mode.
            (let ((calls 0))
              (setq herdr-sanctum-tab-routing-rules
                    (list (list "Tab1" :predicate (lambda (_b) (cl-incf calls) nil))
                          (list "Tab2" :predicate (lambda (_b) (cl-incf calls) nil))
                          (list "Live" :name-regexp "\\` \\*sanctum-test-popup\\*\\'")))
              (save-window-excursion
                (switch-to-buffer sanctum-buf)
                (should (equal (herdr-sanctum-tab-routing-match popup-buf) "Live"))
                (should (= calls 2))
                (should-not (herdr-sanctum-tab-routing-match tool-buf))
                (should (= calls 4))))
            (herdr-sanctum-tab-routing-mode -1)
            (should (equal display-buffer-alist orig-alist))
            (should-not herdr-sanctum-tab-routing--last-match))
        (herdr-sanctum-tab-routing-mode -1)
        (setq display-buffer-alist orig-alist)
        (dolist (b (list code-buf tool-buf eshell-buf popup-buf))
          (when (buffer-live-p b) (kill-buffer b)))))))

(ert-deftest herdr-sanctum-optional-test/tab-routing-display-buffer-single-eval ()
  "Real `display-buffer' evaluates `:predicate' once and routes `:commands'."
  (herdr-sanctum-test-with-clean-state
    (let* ((orig-alist (copy-sequence display-buffer-alist))
           (orig-tab-bar-mode (bound-and-true-p tab-bar-mode))
           (sanctum-buf (get-buffer-create herdr-sanctum-buffer-name))
           (code-buf (get-buffer-create " *sanctum-test-code*"))
           (eshell-buf (get-buffer-create " *sanctum-test-eshell*"))
           (pred-calls 0)
           (herdr-sanctum-tab-routing-rules
            (list (list "Folio"
                        :predicate (lambda (buf)
                                     (cl-incf pred-calls)
                                     (eq buf code-buf)))
                  (list "Bench" :commands '(eshell)))))
      (unwind-protect
          (progn
            (herdr-sanctum-tab-routing-mode 1)
            (tab-bar-mode 1)
            ;; 1. Creating a new tab evaluates `:predicate' only once (not 3x).
            (switch-to-buffer sanctum-buf)
            (display-buffer code-buf)
            (should (= pred-calls 1))
            (should (equal (alist-get 'name (tab-bar--current-tab)) "Folio"))
            (should (get-buffer-window code-buf))
            ;; 2. Re-routing into the existing tab also evaluates `:predicate' once.
            (tab-bar-switch-to-recent-tab)
            (switch-to-buffer sanctum-buf)
            (display-buffer code-buf)
            (should (= pred-calls 2))
            (should (equal (alist-get 'name (tab-bar--current-tab)) "Folio"))
            ;; 3. `:commands (eshell)' routes a `fundamental-mode' buffer into Bench.
            (tab-bar-switch-to-recent-tab)
            (switch-to-buffer sanctum-buf)
            (let ((this-command 'eshell))
              (display-buffer eshell-buf))
            (should (equal (alist-get 'name (tab-bar--current-tab)) "Bench"))
            (should (get-buffer-window eshell-buf)))
        (herdr-sanctum-tab-routing-mode -1)
        (setq display-buffer-alist orig-alist)
        (while (> (length (tab-bar-tabs)) 1)
          (tab-bar-close-tab))
        (tab-bar-mode (if orig-tab-bar-mode 1 -1))
        (dolist (b (list code-buf eshell-buf))
          (when (buffer-live-p b) (kill-buffer b)))))))

(ert-deftest herdr-sanctum-optional-test/tab-routing-standalone-load ()
  "Loading `herdr-sanctum-tab-routing' alone does not pull in `herdr-sanctum-rpc'."
  (let* ((repo (herdr-sanctum-optional-test--repo-dir))
         (emacs (herdr-sanctum-optional-test--emacs-bin))
         (exit-code
          (call-process
           emacs nil nil nil
           "-Q" "--batch" "-L" repo
           "--eval"
           (prin1-to-string
            '(progn
               (require 'herdr-sanctum-tab-routing)
               (when (featurep 'herdr-sanctum-rpc)
                 (error "herdr-sanctum-rpc was unexpectedly loaded"))
               (unless (equal herdr-sanctum-buffer-name "*sanctum*")
                 (error "Unexpected buffer name: %S" herdr-sanctum-buffer-name))
               (herdr-sanctum-tab-routing-mode 1)
               (herdr-sanctum-tab-routing-mode -1))))))
    (should (eql exit-code 0))))

;;; IME Sync Tests

(ert-deftest herdr-sanctum-optional-test/ime-per-pane-and-isolation ()
  "Per-pane IME syncs over socket, remembers overrides, and isolates buffers."
  (herdr-sanctum-test-with-clean-state
    (let* ((buf (get-buffer-create herdr-sanctum-buffer-name))
           (other-buf (get-buffer-create " *sanctum-ime-other*"))
           (sub-client nil)
           (focused "w1:p1")
           (dummy-activate (lambda (_name)
                             (setq-local deactivate-current-input-method-function #'ignore)))
           (input-method-alist (cons (list "test-ime" "UTF-8" dummy-activate "漢")
                                     input-method-alist))
           (default-input-method "test-ime")
           (herdr-sanctum-ime-input-method nil))
      (unwind-protect
          (herdr-sanctum-test-with-mock-server (_sock)
              (lambda (client data)
                (dolist (req (herdr-sanctum-test--parse-json-lines data))
                  (let ((id (alist-get 'id req))
                        (method (alist-get 'method req)))
                    (cond
                     ((equal method "events.subscribe")
                      (unless (equal id "emacs:agent-sub")
                        (setq sub-client client))
                      (herdr-sanctum-test--send-json
                       client `((id . ,id) (result . ((type . "subscription_started"))))))
                     ((equal method "session.snapshot")
                      (herdr-sanctum-test--send-json
                       client
                       `((id . ,id)
                         (result . ((type . "session_snapshot")
                                    (snapshot . ((focused_pane_id . ,focused)
                                                 (panes . [((pane_id . "w1:p1") (agent . "pi"))
                                                           ((pane_id . "w1:p2"))])
                                                 (agents . [((pane_id . "w1:p1") (agent . "pi")
                                                             (agent_status . "idle"))]))))))))))))
            (with-current-buffer buf
              (deactivate-input-method))
            (herdr-sanctum-ime-mode 1)
            (herdr-sanctum-ensure-connected)
            (should (herdr-sanctum-test--wait-until
                     (lambda ()
                       (equal (buffer-local-value 'current-input-method buf) "test-ime"))))
            ;; Focusing shell pane immediately turns IME off.
            (setq focused "w1:p2")
            (herdr-sanctum-test--send-json
             sub-client '((event . "pane.focused")
                          (data . ((type . "pane_focused") (pane_id . "w1:p2")
                                   (workspace_id . "w1")))))
            (should (herdr-sanctum-test--wait-until
                     (lambda () (equal herdr-sanctum-ime--focused-pane-id "w1:p2"))))
            (should-not (buffer-local-value 'current-input-method buf))
            ;; Manual toggle on shell pane is remembered across switches.
            (with-current-buffer buf (activate-input-method "test-ime"))
            (should (eq (cdr (gethash "w1:p2" herdr-sanctum-ime--panes)) 'on))
            (herdr-sanctum-ime-sync-pane "w1:p1")
            (with-current-buffer buf (deactivate-input-method))
            (should (eq (cdr (gethash "w1:p1" herdr-sanctum-ime--panes)) 'off))
            (herdr-sanctum-ime-sync-pane "w1:p2")
            (should (equal (buffer-local-value 'current-input-method buf) "test-ime"))
            ;; Agent detection clears stale override.
            (with-current-buffer buf (deactivate-input-method))
            (herdr-sanctum--handle-event-line
             "{\"event\":\"pane.agent_detected\",\"data\":{\"type\":\"pane_agent_detected\",\"pane_id\":\"w1:p2\",\"agent\":\"pi\",\"released\":false}}")
            (should-not (cdr (gethash "w1:p2" herdr-sanctum-ime--panes)))
            (should (equal (buffer-local-value 'current-input-method buf) "test-ime"))
            ;; `default-input-method' is read inside `*sanctum*', isolated from unrelated buffers.
            (with-current-buffer buf
              (let ((herdr-sanctum-ime--syncing t))
                (deactivate-input-method))
              (setq-local default-input-method "test-ime"))
            (with-current-buffer other-buf
              (setq-local default-input-method nil)
              (herdr-sanctum-ime-sync-pane "w1:p2"))
            (should (equal (buffer-local-value 'current-input-method buf) "test-ime"))
            ;; Pane close and nil snapshot focus prune state and clear `--focused-pane-id'.
            (herdr-sanctum-ime--on-event "pane_closed" '((pane_id . "w1:p2")))
            (should-not herdr-sanctum-ime--focused-pane-id)
            (should-not (gethash "w1:p2" herdr-sanctum-ime--panes))
            (herdr-sanctum-ime-sync-pane "w1:p1")
            (herdr-sanctum-ime--on-snapshot '((focused_pane_id . nil) (panes . nil)))
            (should-not herdr-sanctum-ime--focused-pane-id))
        (when (buffer-live-p other-buf) (kill-buffer other-buf))))))

(ert-deftest herdr-sanctum-optional-test/ime-terminal-start-hook-sync ()
  "Starting or restarting `*sanctum*' syncs IME via `herdr-sanctum-terminal-start-hook'."
  (herdr-sanctum-test-with-clean-state
    (let* ((dummy-activate (lambda (_name)
                             (setq-local deactivate-current-input-method-function #'ignore)))
           (input-method-alist (cons (list "test-ime" "UTF-8" dummy-activate "漢")
                                     input-method-alist))
           (default-input-method "test-ime")
           (herdr-sanctum-ime-input-method nil)
           (herdr-sanctum-program "sh")
           (herdr-sanctum-terminal-backends (copy-alist herdr-sanctum-terminal-backends))
           (herdr-sanctum-terminal-backend 'ime-hook-test-backend)
           (snap (herdr-sanctum-test-make-snapshot
                  :focused-pane "w1:p1"
                  :panes '(((pane_id . "w1:p1") (agent . "claude"))
                           ((pane_id . "w1:p2"))))))
      (herdr-sanctum-register-terminal-backend
       'ime-hook-test-backend
       :start (lambda (buf _prog _args)
                (with-current-buffer buf
                  (deactivate-input-method)
                  (setq-local herdr-sanctum-test--live t)))
       :live-p (lambda (buf) (buffer-local-value 'herdr-sanctum-test--live buf)))
      ;; 1. RPC already has snapshot BEFORE `*sanctum*' exists.
      (herdr-sanctum-ime-mode 1)
      (should (memq #'herdr-sanctum-ime--on-terminal-start
                    herdr-sanctum-terminal-start-hook))
      (herdr-sanctum-apply-snapshot snap)
      (should-not (get-buffer herdr-sanctum-buffer-name))
      (should (equal herdr-sanctum-ime--focused-pane-id "w1:p1"))
      (let ((buf (get-buffer-create herdr-sanctum-buffer-name)))
        (herdr-sanctum-terminal-start buf)
        (should (equal (buffer-local-value 'current-input-method buf) "test-ime"))
        ;; 2. Restart with cleared focused-pane-id falls back to cached snapshot.
        (with-current-buffer buf
          (let ((herdr-sanctum-ime--syncing t))
            (deactivate-input-method))
          (setq-local herdr-sanctum-test--live nil))
        (setq herdr-sanctum-ime--focused-pane-id nil)
        (clrhash herdr-sanctum-ime--panes)
        (herdr-sanctum-terminal-start buf)
        (should (equal herdr-sanctum-ime--focused-pane-id "w1:p1"))
        (should (equal (buffer-local-value 'current-input-method buf) "test-ime")))
      ;; Disabling removes the startup hook.
      (herdr-sanctum-ime-mode -1)
      (should-not (memq #'herdr-sanctum-ime--on-terminal-start
                        herdr-sanctum-terminal-start-hook)))))

(ert-deftest herdr-sanctum-optional-test/ime-explicit-input-method-sets-local-default ()
  "Explicit `herdr-sanctum-ime-input-method' sets buffer-local `default-input-method'."
  (herdr-sanctum-test-with-clean-state
    (let* ((buf (get-buffer-create herdr-sanctum-buffer-name))
           (other-buf (get-buffer-create " *sanctum-ime-global*"))
           (dummy-activate (lambda (_name)
                             (setq-local deactivate-current-input-method-function #'ignore)))
           (input-method-alist (cons (list "custom-ime" "UTF-8" dummy-activate "漢")
                                     input-method-alist))
           (default-input-method nil)
           (herdr-sanctum-ime-input-method "custom-ime"))
      (unwind-protect
          (progn
            (herdr-sanctum-ime-mode 1)
            (herdr-sanctum-ime--on-snapshot
             '((focused_pane_id . "w1:shell")
               (panes . (((pane_id . "w1:shell"))))))
            ;; Even on a shell pane (IME off), `*sanctum*' gets buffer-local `default-input-method'.
            (should-not (buffer-local-value 'current-input-method buf))
            (should (equal (buffer-local-value 'default-input-method buf) "custom-ime"))
            (should-not (buffer-local-value 'default-input-method other-buf))
            ;; `toggle-input-method' (C-\) toggles `custom-ime' without prompting and records pref.
            (with-current-buffer buf
              (toggle-input-method)
              (should (equal current-input-method "custom-ime"))
              (should (eq (cdr (gethash "w1:shell" herdr-sanctum-ime--panes)) 'on))
              (toggle-input-method)
              (should-not current-input-method)
              (should (eq (cdr (gethash "w1:shell" herdr-sanctum-ime--panes)) 'off))))
        (when (buffer-live-p other-buf) (kill-buffer other-buf))))))

(ert-deftest herdr-sanctum-optional-test/ime-empty-agent-falls-back-to-display-agent ()
  "Empty-string `agent' does not shadow non-empty `display_agent'."
  (herdr-sanctum-test-with-clean-state
    (should (eq (herdr-sanctum-ime--pane-kind
                 '((pane_id . "w1:p1") (agent . "") (display_agent . "claude")))
                'agent))
    (should (eq (herdr-sanctum-ime--pane-kind
                 '((pane_id . "w1:p2") (agent . "") (display_agent . "")))
                'shell))
    (should (eq (herdr-sanctum-ime--pane-kind
                 '((pane_id . "w1:p3") (agent . "pi") (display_agent . "")))
                'agent))
    (herdr-sanctum-ime-mode 1)
    (herdr-sanctum-ime--on-snapshot
     '((focused_pane_id . "w1:p1")
       (panes . (((pane_id . "w1:p1") (agent . "") (display_agent . "claude"))
                 ((pane_id . "w1:p2") (agent . "") (display_agent . nil))))))
    (should (eq (car (gethash "w1:p1" herdr-sanctum-ime--panes)) 'agent))
    (should (eq (car (gethash "w1:p2" herdr-sanctum-ime--panes)) 'shell))
    (herdr-sanctum-ime--on-event
     "pane_created"
     '((pane . ((pane_id . "w1:p3") (agent . "") (display_agent . "codex")))))
    (should (eq (car (gethash "w1:p3" herdr-sanctum-ime--panes)) 'agent))))

;;; Terminal Backend Tests

(ert-deftest herdr-sanctum-optional-test/terminal-backend-lifecycle ()
  "Terminal backend starts, sets line-spacing, runs start hook, and renders banner."
  (herdr-sanctum-test-with-clean-state
    (let* ((started nil)
           (resized 0)
           (setup-called 0)
           (hook-buffers nil)
           (herdr-sanctum-terminal-start-hook
            (list (lambda () (push (current-buffer) hook-buffers))))
           (herdr-sanctum-terminal-backends (copy-alist herdr-sanctum-terminal-backends))
           (herdr-sanctum-terminal-backend 'test-backend)
           (herdr-sanctum-display-mode 'current)
           (herdr-sanctum-program "sh")
           (herdr-sanctum-socket-path (expand-file-name "no-such.sock" herdr-sanctum-test--root))
           (herdr-sanctum-sidebar-default-open t)
           (herdr-sanctum-drawer-default-open nil)
           (herdr-sanctum-terminal-line-spacing 0.25)
           (herdr-sanctum-terminal-font-height 115))
      (herdr-sanctum-register-terminal-backend
       'test-backend
       :start (lambda (buf prog _args)
                (setq started (cons (buffer-name buf) prog))
                (with-current-buffer buf
                  (setq-local herdr-sanctum-test--live t)))
       :live-p (lambda (buf) (buffer-local-value 'herdr-sanctum-test--live buf))
       :setup (lambda (_buf) (cl-incf setup-called))
       :resize (lambda (_win) (cl-incf resized))
       :mode-banner (lambda (_buf) " TEST-BANNER "))
      ;; 1. `validate-start' returns `(BACKEND . PROGRAM)' before start and `(BACKEND . nil)' when live.
      (let ((resolved (herdr-sanctum-terminal-validate-start nil)))
        (should (listp (car resolved)))
        (should (stringp (cdr resolved))))
      ;; 2. Full `herdr-sanctum' start applies backend, line-spacing, font-height, and start hook.
      (herdr-sanctum)
      (let ((buf (get-buffer herdr-sanctum-buffer-name)))
        (should (equal (car started) herdr-sanctum-buffer-name))
        (should (= setup-called 1))
        (should (> resized 0))
        (should (equal hook-buffers (list buf)))
        (should (eql (buffer-local-value 'line-spacing buf) 0.25))
        (should (herdr-sanctum-in-sanctum-p))
        (should (equal (herdr-sanctum-header-line) " TEST-BANNER "))
        (should (herdr-sanctum-sidebar-visible-p))
        (should-not (herdr-sanctum-drawer-visible-p))
        (let ((resolved-live (herdr-sanctum-terminal-validate-start buf)))
          (should (listp (car resolved-live)))
          (should-not (cdr resolved-live)))))))

(ert-deftest herdr-sanctum-optional-test/terminal-font-height-preserves-remappings ()
  "Font-height remapping preserves pre-existing `default' remaps and resizes live PTY."
  (herdr-sanctum-test-with-clean-state
    (let* ((orig-height (default-value 'herdr-sanctum-terminal-font-height))
           (resized 0)
           (herdr-sanctum-terminal-backends (copy-alist herdr-sanctum-terminal-backends))
           (herdr-sanctum-terminal-backend 'font-test-backend)
           (buf (get-buffer-create herdr-sanctum-buffer-name)))
      (herdr-sanctum-register-terminal-backend
       'font-test-backend
       :start #'ignore
       :live-p (lambda (b) (buffer-local-value 'herdr-sanctum-test--live b))
       :resize (lambda (_win) (cl-incf resized)))
      (unwind-protect
          (progn
            (switch-to-buffer buf)
            (with-current-buffer buf
              (setq-local herdr-sanctum-test--live t)
              ;; Simulate a backend like `ghostel-mode' remapping `default' -> `ghostel-default'.
              (face-remap-set-base 'default 'ghostel-default))
            ;; 1. Nil font-height leaves `ghostel-default' untouched.
            (let ((herdr-sanctum-terminal-font-height nil))
              (herdr-sanctum-terminal--apply-font-height buf)
              (with-current-buffer buf
                (should-not herdr-sanctum-terminal--font-cookie)
                (should (equal (assq 'default face-remapping-alist)
                               '(default ghostel-default)))))
            ;; 2. Setting numeric height via `setopt' adds relative cookie and resizes live PTY.
            (setopt herdr-sanctum-terminal-font-height 115)
            (should (= resized 1))
            (with-current-buffer buf
              (should herdr-sanctum-terminal--font-cookie)
              (let ((specs (cdr (assq 'default face-remapping-alist))))
                (should (member '(:height 115) specs))
                (should (memq 'ghostel-default specs))))
            ;; 3. Resetting to nil via `setopt' removes ONLY Sanctum's cookie and keeps `ghostel-default'.
            (setopt herdr-sanctum-terminal-font-height nil)
            (should (= resized 2))
            (with-current-buffer buf
              (should-not herdr-sanctum-terminal--font-cookie)
              (should (equal (assq 'default face-remapping-alist)
                             '(default ghostel-default)))))
        (set-default 'herdr-sanctum-terminal-font-height orig-height)))))

(ert-deftest herdr-sanctum-optional-test/terminal-backend-validation-and-banners ()
  "Backend resolver validates `:start'/`:live-p' and Ghostel banners are pre-allocated."
  (herdr-sanctum-test-with-clean-state
    ;; Invalid symbol and non-symbol backends.
    (let ((herdr-sanctum-terminal-backend 'nonexistent-backend))
      (should-not (herdr-sanctum-terminal-resolve-backend t))
      (should-not (herdr-sanctum-terminal-mode-banner))
      (should-error (herdr-sanctum-terminal-validate-start) :type 'user-error))
    (let ((herdr-sanctum-terminal-backend 42))
      (should-not (herdr-sanctum-terminal-resolve-backend t))
      (should-not (herdr-sanctum-terminal-mode-banner))
      (should-error (herdr-sanctum-terminal-validate-start) :type 'user-error))
    ;; Invalid inline plist missing `:start' or `:live-p' or with non-function values.
    (dolist (bad-plist (list (list :live-p #'ignore)
                             (list :start #'ignore)
                             (list :start "not-a-fn" :live-p #'ignore)
                             (list :start #'ignore :live-p 123)))
      (let ((herdr-sanctum-terminal-backend bad-plist))
        (should-not (herdr-sanctum-terminal-resolve-backend t))
        (should-not (herdr-sanctum-terminal-mode-banner))
        (should-error (herdr-sanctum-terminal-resolve-backend) :type 'user-error)
        (should-error (herdr-sanctum-terminal-validate-start) :type 'user-error)))
    ;; Valid inline plist with non-function optional `:resize' / `:mode-banner' does not crash.
    (let ((herdr-sanctum-terminal-backend
           (list :start #'ignore :live-p #'ignore :resize 'not-fn :mode-banner 42)))
      (should (eq (herdr-sanctum-terminal-resolve-backend)
                  herdr-sanctum-terminal-backend))
      (should-not (herdr-sanctum-terminal-mode-banner (current-buffer)))
      (should-not (herdr-sanctum-terminal-resize (selected-window))))
    ;; Ghostel mode banners are pre-allocated (`eq' across calls).
    (with-temp-buffer
      (setq-local ghostel--input-mode 'copy)
      (should (eq (herdr-sanctum-ghostel--mode-banner (current-buffer))
                  (herdr-sanctum-ghostel--mode-banner (current-buffer))))
      (setq-local ghostel--input-mode 'emacs)
      (should (eq (herdr-sanctum-ghostel--mode-banner (current-buffer))
                  (herdr-sanctum-ghostel--mode-banner (current-buffer))))
      (setq-local ghostel--input-mode 'char)
      (should (eq (herdr-sanctum-ghostel--mode-banner (current-buffer))
                  (herdr-sanctum-ghostel--mode-banner (current-buffer)))))))

(ert-deftest herdr-sanctum-optional-test/terminal-standalone-load ()
  "Loading `herdr-sanctum-terminal' alone avoids loading RPC or theme modules."
  (let* ((repo (herdr-sanctum-optional-test--repo-dir))
         (emacs (herdr-sanctum-optional-test--emacs-bin))
         (exit-code
          (call-process
           emacs nil nil nil
           "-Q" "--batch" "-L" repo
           "--eval"
           (prin1-to-string
            '(progn
               (require 'herdr-sanctum-terminal)
               (when (featurep 'herdr-sanctum-rpc)
                 (error "herdr-sanctum-rpc was unexpectedly loaded"))
               (when (featurep 'herdr-sanctum-theme)
                 (error "herdr-sanctum-theme was unexpectedly loaded"))
               (unless (herdr-sanctum-terminal-resolve-backend)
                 (error "Default ghostel backend failed to resolve")))))))
    (should (eql exit-code 0))))

(provide 'herdr-sanctum-optional-test)
;;; herdr-sanctum-optional-test.el ends here
