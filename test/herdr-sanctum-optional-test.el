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

(defvar vterm-shell)
(defvar vterm-kill-buffer-on-exit)

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

(defun herdr-sanctum-optional-test--named-exclude-p (buf)
  "Return non-nil when BUF is named \"*custom-excluded*\"."
  (equal (buffer-name buf) "*custom-excluded*"))

(ert-deftest herdr-sanctum-optional-test/tab-routing-dynamic-tab-name-function ()
  "Dynamic `TAB-NAME' functions resolve per buffer, fall through, and eval once."
  (herdr-sanctum-test-with-clean-state
    (let* ((orig-alist (copy-sequence display-buffer-alist))
           (orig-tab-bar-mode (bound-and-true-p tab-bar-mode))
           (sanctum-buf (get-buffer-create herdr-sanctum-buffer-name))
           (proj-buf (get-buffer-create " *sanctum-proj-a*"))
           (fallback-buf (get-buffer-create " *sanctum-proj-none*"))
           (empty-buf (get-buffer-create " *sanctum-proj-empty*"))
           (pred-calls 0)
           (namer-calls 0)
           (herdr-sanctum-tab-routing-rules
            (list (list (lambda (buf)
                          (cl-incf namer-calls)
                          (cond
                           ((eq buf proj-buf)
                            ;; Verify `with-current-buffer' matches BUF.
                            (should (eq (current-buffer) buf))
                            "Project-A")
                           ((eq buf empty-buf) "")
                           (t nil)))
                        :predicate (lambda (buf)
                                     (cl-incf pred-calls)
                                     (memq buf (list proj-buf fallback-buf empty-buf))))
                  (list "Folio" :name-regexp "\\` \\*sanctum-proj-"))))
      (unwind-protect
          (progn
            (herdr-sanctum-tab-routing-mode 1)
            (tab-bar-mode 1)
            (switch-to-buffer sanctum-buf)
            ;; 1. Dynamic function returning nil or "" falls through to later rules.
            (should (equal (herdr-sanctum-tab-routing-match fallback-buf) "Folio"))
            (should (equal (herdr-sanctum-tab-routing-match empty-buf) "Folio"))
            ;; 2. Real `display-buffer' evaluates `:predicate' and `TAB-NAME' fn once.
            (setq pred-calls 0
                  namer-calls 0)
            (display-buffer proj-buf)
            (should (= pred-calls 1))
            (should (= namer-calls 1))
            (should (equal (alist-get 'name (tab-bar--current-tab)) "Project-A"))
            (should (get-buffer-window proj-buf)))
        (herdr-sanctum-tab-routing-mode -1)
        (setq display-buffer-alist orig-alist)
        (while (> (length (tab-bar-tabs)) 1)
          (tab-bar-close-tab))
        (tab-bar-mode (if orig-tab-bar-mode 1 -1))
        (dolist (b (list proj-buf fallback-buf empty-buf))
          (when (buffer-live-p b) (kill-buffer b)))))))

(ert-deftest herdr-sanctum-optional-test/tab-routing-fallback-tab ()
  "Fallback routing protects the Sanctum tab for `recent', string, and nil."
  (herdr-sanctum-test-with-clean-state
    (let* ((orig-alist (copy-sequence display-buffer-alist))
           (orig-tab-bar-mode (bound-and-true-p tab-bar-mode))
           (sanctum-buf (get-buffer-create herdr-sanctum-buffer-name))
           (sidebar-buf (get-buffer-create herdr-sanctum-sidebar-buffer-name))
           (drawer-buf (get-buffer-create herdr-sanctum-drawer-buffer-name))
           (notes-buf (get-buffer-create "*sanctum-test-notes*"))
           (other-buf (get-buffer-create "*sanctum-test-other*"))
           (herdr-sanctum-tab-routing-rules nil)
           (herdr-sanctum-tab-routing-fallback-tab 'recent))
      (unwind-protect
          (progn
            (herdr-sanctum-tab-routing-mode 1)
            (tab-bar-mode 1)
            ;; 1. Sole Sanctum tab + `recent' creates "Main" and keeps Sanctum unsplit.
            (switch-to-buffer sanctum-buf)
            (tab-bar-rename-tab herdr-sanctum-tab-name)
            (should (equal (herdr-sanctum-tab-routing-match notes-buf) "Main"))
            (dolist (ui (list sanctum-buf sidebar-buf drawer-buf))
              (should-not (herdr-sanctum-tab-routing-match ui)))
            (display-buffer notes-buf)
            (should (equal (alist-get 'name (tab-bar--current-tab)) "Main"))
            (should (get-buffer-window notes-buf))
            (tab-bar-select-tab-by-name herdr-sanctum-tab-name)
            (should (equal (mapcar (lambda (w) (buffer-name (window-buffer w)))
                                   (window-list))
                           (list herdr-sanctum-buffer-name)))
            ;; 2. `recent' picks the most recent non-Sanctum tab among multiple tabs.
            (tab-bar-new-tab)
            (tab-bar-rename-tab "Alpha")
            (tab-bar-new-tab)
            (tab-bar-rename-tab "Beta")
            (tab-bar-select-tab-by-name "Alpha")
            (tab-bar-select-tab-by-name herdr-sanctum-tab-name)
            (should (equal (herdr-sanctum-tab-routing-match other-buf) "Alpha"))
            (display-buffer other-buf)
            (should (equal (alist-get 'name (tab-bar--current-tab)) "Alpha"))
            (should (get-buffer-window other-buf))
            ;; Non-Sanctum tab does not trigger fallback routing.
            (should-not (herdr-sanctum-tab-routing-match notes-buf))
            ;; 3. Fixed string fallback routes unmatched buffers to that named tab.
            (tab-bar-select-tab-by-name herdr-sanctum-tab-name)
            (let ((herdr-sanctum-tab-routing-fallback-tab "Overflow"))
              (should (equal (herdr-sanctum-tab-routing-match notes-buf) "Overflow"))
              (display-buffer notes-buf)
              (should (equal (alist-get 'name (tab-bar--current-tab)) "Overflow")))
            ;; 4. Nil fallback disables unmatched routing even on the Sanctum tab.
            (tab-bar-select-tab-by-name herdr-sanctum-tab-name)
            (let ((herdr-sanctum-tab-routing-fallback-tab nil))
              (should-not (herdr-sanctum-tab-routing-match notes-buf))))
        (herdr-sanctum-tab-routing-mode -1)
        (setq display-buffer-alist orig-alist)
        (while (> (length (tab-bar-tabs)) 1)
          (tab-bar-close-tab))
        (tab-bar-mode (if orig-tab-bar-mode 1 -1))
        (dolist (b (list notes-buf other-buf))
          (when (buffer-live-p b) (kill-buffer b)))))))

(ert-deftest herdr-sanctum-optional-test/tab-routing-exclude-rules ()
  "Exclude rules prevent fallback from hijacking popups, modes, and predicates."
  (herdr-sanctum-test-with-clean-state
    (let* ((orig-alist (copy-sequence display-buffer-alist))
           (orig-tab-bar-mode (bound-and-true-p tab-bar-mode))
           (sanctum-buf (get-buffer-create herdr-sanctum-buffer-name))
           (normal-buf (get-buffer-create "*sanctum-normal*"))
           (help-buf (get-buffer-create "*sanctum-help-test*"))
           (pred-buf (get-buffer-create "*sanctum-lambda-excluded*"))
           (named-buf (get-buffer-create "*custom-excluded*"))
           (popups (mapcar #'get-buffer-create
                           '("*Completions*" "*which-key*" " *which-key*"
                             "*Embark Actions*" "*Transient*" " *transient*"
                             "*Minibuf-1*" "*Calculator*")))
           (herdr-sanctum-tab-routing-rules nil)
           (herdr-sanctum-tab-routing-fallback-tab "Work"))
      (unwind-protect
          (progn
            (with-current-buffer help-buf
              (setq major-mode 'help-mode))
            (herdr-sanctum-tab-routing-mode 1)
            (tab-bar-mode 1)
            (switch-to-buffer sanctum-buf)
            (tab-bar-rename-tab herdr-sanctum-tab-name)
            ;; 1. Default exclude rules block transient/completion popups.
            (dolist (popup popups)
              (should-not (herdr-sanctum-tab-routing-match popup)))
            (should (equal (herdr-sanctum-tab-routing-match normal-buf) "Work"))
            ;; 2. Major-mode symbols (loaded & autoloaded), lambdas, and named fns.
            (let ((herdr-sanctum-tab-routing-exclude-rules
                   (list 'help-mode
                         'dired-mode
                         (lambda (buf) (eq buf pred-buf))
                         #'herdr-sanctum-optional-test--named-exclude-p)))
              (should-not (herdr-sanctum-tab-routing-match help-buf))
              (should-not (herdr-sanctum-tab-routing-match pred-buf))
              (should-not (herdr-sanctum-tab-routing-match named-buf))
              ;; Non-matching buffer does not fail when checking loaded/autoloaded modes.
              (should (equal (herdr-sanctum-tab-routing-match normal-buf) "Work")))
            ;; 3. Explicit routing rules take precedence over exclude rules.
            (let ((herdr-sanctum-tab-routing-rules
                   '(("CalcTab" :name-regexp "\\`\\*Calculator\\*"))))
              (should (equal (herdr-sanctum-tab-routing-match
                              (get-buffer "*Calculator*"))
                             "CalcTab"))))
        (herdr-sanctum-tab-routing-mode -1)
        (setq display-buffer-alist orig-alist)
        (while (> (length (tab-bar-tabs)) 1)
          (tab-bar-close-tab))
        (tab-bar-mode (if orig-tab-bar-mode 1 -1))
        (dolist (b (append (list normal-buf help-buf pred-buf named-buf) popups))
          (when (buffer-live-p b) (kill-buffer b)))))))

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

(ert-deftest herdr-sanctum-optional-test/ime-default-function-and-custom-method-override ()
  "Custom `herdr-sanctum-ime-default-function' values and manual overrides work."
  (herdr-sanctum-test-with-clean-state
    (let* ((buf (get-buffer-create herdr-sanctum-buffer-name))
           (dummy-activate (lambda (_name)
                             (setq-local deactivate-current-input-method-function #'ignore)))
           (input-method-alist
            (append (list (list "ime-base" "UTF-8" dummy-activate "漢")
                          (list "ime-ja" "UTF-8" dummy-activate "あ")
                          (list "ime-ko" "UTF-8" dummy-activate "한"))
                    input-method-alist))
           (default-input-method "ime-base")
           (herdr-sanctum-ime-input-method nil)
           (herdr-sanctum-ime-agent-default t)
           (herdr-sanctum-ime-shell-default nil)
           (seen-args nil)
           (snap '((focused_pane_id . "w1:on")
                   (panes . (((pane_id . "w1:on") (tag . "force-on"))
                             ((pane_id . "w1:t") (tag . "force-t"))
                             ((pane_id . "w1:off") (agent . "claude") (tag . "force-off"))
                             ((pane_id . "w1:nil") (agent . "claude") (tag . "force-nil"))
                             ((pane_id . "w1:ja") (tag . "ja"))
                             ((pane_id . "w1:ko") (tag . "ko"))
                             ((pane_id . "w1:def-agent") (agent . "pi") (tag . "def"))
                             ((pane_id . "w1:def-shell") (tag . "def"))))))
           (herdr-sanctum-ime-default-function
            (lambda (pane snapshot)
              (setq seen-args (cons pane snapshot))
              (pcase (alist-get 'tag pane)
                ("force-on" 'on)
                ("force-t" t)
                ("force-off" 'off)
                ("force-nil" nil)
                ("ja" "ime-ja")
                ("ko" "ime-ko")
                ("def" 'default)
                (_ (if (alist-get 'agent pane) "ime-ja" 'default))))))
      (with-current-buffer buf
        (deactivate-input-method))
      (herdr-sanctum-ime-mode 1)
      (herdr-sanctum-ime--on-snapshot snap)
      ;; `'on' activates configured method and passes (PANE SNAPSHOT).
      (should (equal (buffer-local-value 'current-input-method buf) "ime-base"))
      (should (equal (alist-get 'pane_id (car seen-args)) "w1:on"))
      (should (eq (cdr seen-args) snap))
      ;; `'off' and `nil' disable IME even on agent panes.
      (herdr-sanctum-ime-sync-pane "w1:off")
      (should-not (buffer-local-value 'current-input-method buf))
      ;; `t' enables configured method even on shell panes.
      (herdr-sanctum-ime-sync-pane "w1:t")
      (should (equal (buffer-local-value 'current-input-method buf) "ime-base"))
      (herdr-sanctum-ime-sync-pane "w1:nil")
      (should-not (buffer-local-value 'current-input-method buf))
      ;; Non-empty string activates that specific input method and switches cleanly.
      (herdr-sanctum-ime-sync-pane "w1:ja")
      (should (equal (buffer-local-value 'current-input-method buf) "ime-ja"))
      (herdr-sanctum-ime-sync-pane "w1:ko")
      (should (equal (buffer-local-value 'current-input-method buf) "ime-ko"))
      ;; `'default' falls back to agent/shell defaults.
      (herdr-sanctum-ime-sync-pane "w1:def-shell")
      (should-not (buffer-local-value 'current-input-method buf))
      (herdr-sanctum-ime-sync-pane "w1:def-agent")
      (should (equal (buffer-local-value 'current-input-method buf) "ime-base"))
      ;; Manual override recording works for per-pane custom method strings.
      (herdr-sanctum-ime-sync-pane "w1:ja")
      (should (equal (buffer-local-value 'current-input-method buf) "ime-ja"))
      (with-current-buffer buf (deactivate-input-method))
      (should (eq (cdr (gethash "w1:ja" herdr-sanctum-ime--panes)) 'off))
      (herdr-sanctum-ime-sync-pane "w1:ko")
      (should (equal (buffer-local-value 'current-input-method buf) "ime-ko"))
      (herdr-sanctum-ime-sync-pane "w1:ja")
      (should-not (buffer-local-value 'current-input-method buf))
      ;; Re-enabling custom method manually records `'on' and restores "ime-ja".
      (with-current-buffer buf (activate-input-method "ime-ja"))
      (should (eq (cdr (gethash "w1:ja" herdr-sanctum-ime--panes)) 'on))
      (herdr-sanctum-ime-sync-pane "w1:def-shell")
      (should-not (buffer-local-value 'current-input-method buf))
      (herdr-sanctum-ime-sync-pane "w1:ja")
      (should (equal (buffer-local-value 'current-input-method buf) "ime-ja"))
      ;; `pane_agent_detected' updates cached pane data seen by default-function.
      (herdr-sanctum-ime--on-event
       "pane_created" '((pane . ((pane_id . "w1:dyn")))))
      (herdr-sanctum-ime-sync-pane "w1:dyn")
      (should-not (buffer-local-value 'current-input-method buf))
      (herdr-sanctum-ime--on-event
       "pane_agent_detected"
       '((pane_id . "w1:dyn") (agent . "claude") (released . nil)))
      (should (equal (buffer-local-value 'current-input-method buf) "ime-ja"))
      ;; Custom method strings still activate and deactivate even with nil global default.
      (let ((default-input-method nil))
        (herdr-sanctum-ime-sync-pane "w1:off")
        (should-not (buffer-local-value 'current-input-method buf))
        (herdr-sanctum-ime-sync-pane "w1:ko")
        (should (equal (buffer-local-value 'current-input-method buf) "ime-ko"))
        (herdr-sanctum-ime-sync-pane "w1:off")
        (should-not (buffer-local-value 'current-input-method buf))))))

(ert-deftest herdr-sanctum-optional-test/ime-external-os-callbacks ()
  "External OS IME callbacks work without `input-method-alist' or Emacs IME."
  (herdr-sanctum-test-with-clean-state
    (let* ((buf (get-buffer-create herdr-sanctum-buffer-name))
           (input-method-alist nil)
           (default-input-method nil)
           (herdr-sanctum-ime-input-method nil)
           (calls nil)
           (herdr-sanctum-ime-activate-function
            (lambda (method) (push (list 'activate method) calls)))
           (herdr-sanctum-ime-deactivate-function
            (lambda () (push (list 'deactivate) calls)))
           (herdr-sanctum-ime-active-p-function nil)
           (snap '((focused_pane_id . "w1:a1")
                   (panes . (((pane_id . "w1:a1") (agent . "pi"))
                             ((pane_id . "w1:a2") (agent . "claude"))
                             ((pane_id . "w1:s1"))
                             ((pane_id . "w1:s2")))))))
      (ignore buf)
      (herdr-sanctum-ime-mode 1)
      ;; Available without `input-method-alist' or `herdr-sanctum-ime-input-method'.
      (should (herdr-sanctum-ime--available-p))
      ;; 1. Without `active-p-function', tracks synced state in `--external-active'.
      (herdr-sanctum-ime--on-snapshot snap)
      (should (eq herdr-sanctum-ime--external-active t))
      (should (equal calls '((activate nil))))
      ;; Switching to another agent pane does not redundantly re-activate.
      (herdr-sanctum-ime-sync-pane "w1:a2")
      (should (= (length calls) 1))
      ;; Switching to shell pane deactivates and resets `--external-active'.
      (herdr-sanctum-ime-sync-pane "w1:s1")
      (should-not herdr-sanctum-ime--external-active)
      (should (equal (car calls) '(deactivate)))
      ;; Another shell pane does not redundantly deactivate.
      (herdr-sanctum-ime-sync-pane "w1:s2")
      (should (= (length calls) 2))
      ;; Per-pane custom method strings pass through to `activate-function'.
      (let ((herdr-sanctum-ime-default-function
             (lambda (pane _snap)
               (pcase (alist-get 'pane_id pane)
                 ("w1:a1" "com.apple.inputmethod.SCIM.ITABC")
                 ("w1:a2" "com.apple.inputmethod.Kotoeri.Japanese")
                 (_ 'off)))))
        (herdr-sanctum-ime-sync-pane "w1:a1")
        (should (equal (car calls) '(activate "com.apple.inputmethod.SCIM.ITABC")))
        (herdr-sanctum-ime-sync-pane "w1:a2")
        (should (equal (car calls) '(activate "com.apple.inputmethod.Kotoeri.Japanese"))))
      ;; 2. With `active-p-function', live predicate controls switching.
      (setq calls nil)
      (let* ((os-active t)
             (herdr-sanctum-ime-active-p-function (lambda () os-active)))
        ;; OS already active: agent sync skips redundant activate.
        (herdr-sanctum-ime-sync-pane "w1:a1")
        (should-not calls)
        ;; OS externally switched off: agent sync re-activates.
        (setq os-active nil)
        (herdr-sanctum-ime-sync-pane "w1:a1")
        (should (equal calls '((activate nil))))
        ;; OS active and switching to shell: deactivates.
        (setq os-active t)
        (herdr-sanctum-ime-sync-pane "w1:s1")
        (should (equal (car calls) '(deactivate)))))))

(ert-deftest herdr-sanctum-optional-test/ime-change-hook-and-indicator-string ()
  "`herdr-sanctum-ime-change-hook' and `herdr-sanctum-ime-indicator-string' work."
  (herdr-sanctum-test-with-clean-state
    (let* ((buf (get-buffer-create herdr-sanctum-buffer-name))
           (other-buf (get-buffer-create " *sanctum-ime-ind-other*"))
           (dummy-activate (lambda (_name)
                             (setq-local deactivate-current-input-method-function #'ignore)))
           (input-method-alist
            (cons (list "test-ime" "UTF-8" dummy-activate "漢")
                  input-method-alist))
           (default-input-method "test-ime")
           (herdr-sanctum-ime-input-method nil)
           (events nil)
           (herdr-sanctum-ime-change-hook
            (list (lambda ()
                    (push (herdr-sanctum-ime-indicator-string) events)))))
      (unwind-protect
          (progn
            (with-current-buffer buf (deactivate-input-method))
            ;; Disabled mode returns nil indicator.
            (should-not (herdr-sanctum-ime-indicator-string))
            ;; Enabling mode fires change hook; indicator is "EN".
            (herdr-sanctum-ime-mode 1)
            (should (equal events '("EN")))
            (should (equal (herdr-sanctum-ime-indicator-string) "EN"))
            ;; Snapshot switching to agent fires change hook and shows method title.
            (herdr-sanctum-ime--on-snapshot
             '((focused_pane_id . "w1:p1")
               (panes . (((pane_id . "w1:p1") (agent . "pi"))
                         ((pane_id . "w1:p2"))))))
            (should (equal events '("漢" "EN")))
            (should (equal (herdr-sanctum-ime-indicator-string) "漢"))
            ;; Works from another buffer (defaults to `*sanctum*') and with explicit buffer.
            (with-current-buffer other-buf
              (deactivate-input-method)
              (should (equal (herdr-sanctum-ime-indicator-string) "漢"))
              (should (equal (herdr-sanctum-ime-indicator-string other-buf) "EN")))
            ;; No-op sync on already-synced pane does not fire change hook.
            (herdr-sanctum-ime-sync-pane "w1:p1")
            (should (= (length events) 2))
            ;; Manual deactivate in `*sanctum*' fires hook with indicator already "EN".
            (with-current-buffer buf (deactivate-input-method))
            (should (equal (car events) "EN"))
            (should (= (length events) 3))
            ;; Manual activate in `*sanctum*' fires hook with title "漢".
            (with-current-buffer buf (activate-input-method "test-ime"))
            (should (equal (car events) "漢"))
            (should (= (length events) 4))
            ;; Manual toggle in unrelated buffer does NOT fire change hook.
            (with-current-buffer other-buf
              (activate-input-method "test-ime")
              (deactivate-input-method))
            (should (= (length events) 4))
            ;; External OS IME returns "IME" when active and "EN" when inactive.
            (let* ((ext-active nil)
                   (herdr-sanctum-ime-activate-function (lambda (_m) (setq ext-active t)))
                   (herdr-sanctum-ime-deactivate-function (lambda () (setq ext-active nil)))
                   (herdr-sanctum-ime-active-p-function (lambda () ext-active)))
              (should (equal (herdr-sanctum-ime-indicator-string) "EN"))
              (herdr-sanctum-ime-sync-pane "w1:p1")
              (should (equal (herdr-sanctum-ime-indicator-string) "IME"))
              (should (equal (car events) "IME"))
              (herdr-sanctum-ime-sync-pane "w1:p2")
              (should (equal (herdr-sanctum-ime-indicator-string) "EN"))
              (should (equal (car events) "EN")))
            ;; Disabling mode fires change hook and indicator becomes nil.
            (herdr-sanctum-ime-mode -1)
            (should-not (car events))
            (should-not (herdr-sanctum-ime-indicator-string)))
        (when (buffer-live-p other-buf) (kill-buffer other-buf))))))

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

(ert-deftest herdr-sanctum-optional-test/terminal-vterm-and-eat-backends ()
  "Built-in `vterm' and `eat' backends register and run full lifecycle."
  (herdr-sanctum-test-with-clean-state
    (let ((orig-require (symbol-function 'require))
          (orig-featurep (symbol-function 'featurep))
          (orig-locate (symbol-function 'locate-library)))
      ;; 1. Both built-in backends are registered and resolve.
      (dolist (name '(vterm eat))
        (let ((herdr-sanctum-terminal-backend name))
          (let ((spec (herdr-sanctum-terminal-resolve-backend)))
            (should (functionp (plist-get spec :start)))
            (should (functionp (plist-get spec :live-p)))
            (should (functionp (plist-get spec :setup)))
            (should (functionp (plist-get spec :resize)))
            (should (functionp (plist-get spec :mode-banner)))
            (should (functionp (plist-get spec :available-p)))
            ;; `:available-p' reflects library availability.
            (cl-letf (((symbol-function 'featurep)
                       (lambda (f &optional sub)
                         (and (not (eq f name)) (funcall orig-featurep f sub))))
                      ((symbol-function 'locate-library)
                       (lambda (lib &rest args)
                         (unless (equal lib (symbol-name name))
                           (apply orig-locate lib args)))))
              (should-not (funcall (plist-get spec :available-p))))
            (cl-letf (((symbol-function 'locate-library)
                       (lambda (lib &rest args)
                         (if (equal lib (symbol-name name))
                             (format "/fake/%s.el" lib)
                           (apply orig-locate lib args)))))
              (should (funcall (plist-get spec :available-p)))))))
      ;; 2. Missing package signals `user-error' in `:start'.
      (with-temp-buffer
        (cl-letf (((symbol-function 'require)
                   (lambda (feat &optional file noerror)
                     (unless (memq feat '(vterm eat))
                       (funcall orig-require feat file noerror)))))
          (should-error (herdr-sanctum-vterm--start (current-buffer) "/bin/herdr" nil)
                        :type 'user-error)
          (should-error (herdr-sanctum-eat--start (current-buffer) "/bin/herdr" nil)
                        :type 'user-error)))
      ;; 3. Built-in `vterm' backend lifecycle.
      (let* ((buf (get-buffer-create herdr-sanctum-buffer-name))
             (herdr-sanctum-terminal-backend 'vterm)
             (herdr-sanctum-program "sh")
             (herdr-sanctum-program-args '("--socket" "/tmp/my sock"))
             (seen-shell nil)
             (seen-kill-on-exit 'unset)
             (adjust-args nil)
             (win-size-args nil)
             (proc nil))
        (unwind-protect
            (cl-letf (((symbol-function 'require)
                       (lambda (feat &optional file noerror)
                         (if (eq feat 'vterm) t
                           (funcall orig-require feat file noerror))))
                      ((symbol-function 'vterm-mode)
                       (lambda ()
                         (setq major-mode 'vterm-mode
                               seen-shell vterm-shell
                               seen-kill-on-exit vterm-kill-buffer-on-exit)
                         (setq proc (start-process "test-vterm" (current-buffer) "sleep" "30"))))
                      ((symbol-function 'vterm--window-adjust-process-window-size)
                       (lambda (p wins)
                         (setq adjust-args (list p wins))
                         '(100 . 40)))
                      ((symbol-function 'set-process-window-size)
                       (lambda (p h w) (setq win-size-args (list p h w)))))
              (switch-to-buffer buf)
              (should-not (herdr-sanctum-terminal-live-p buf))
              (herdr-sanctum-terminal-start
               buf (cons (herdr-sanctum-terminal-resolve-backend) "/usr/bin/herdr"))
              (should (equal seen-shell "/usr/bin/herdr --socket /tmp/my\\ sock"))
              (should (null seen-kill-on-exit))
              (should (local-variable-p 'vterm-kill-buffer-on-exit buf))
              (should-not (buffer-local-value 'vterm-kill-buffer-on-exit buf))
              (should (herdr-sanctum-terminal-live-p buf))
              (herdr-sanctum-terminal-resize (selected-window))
              (should (equal adjust-args (list proc (list (selected-window)))))
              (should (equal win-size-args (list proc 40 100)))
              (with-current-buffer buf
                (setq-local vterm-copy-mode nil)
                (should-not (herdr-sanctum-terminal-mode-banner buf))
                (setq-local vterm-copy-mode t)
                (let ((b1 (herdr-sanctum-terminal-mode-banner buf))
                      (b2 (herdr-sanctum-terminal-mode-banner buf)))
                  (should (stringp b1))
                  (should (eq b1 b2))
                  (should (string-search "COPY" b1))))
              (delete-process proc)
              (should-not (herdr-sanctum-terminal-live-p buf)))
          (when (and proc (process-live-p proc)) (delete-process proc))
          (when (buffer-live-p buf) (kill-buffer buf))))
      ;; 4. Built-in `eat' backend lifecycle.
      (let* ((buf (get-buffer-create herdr-sanctum-buffer-name))
             (herdr-sanctum-terminal-backend 'eat)
             (herdr-sanctum-program "sh")
             (herdr-sanctum-program-args '("--flag" "val"))
             (eat-mode-calls 0)
             (exec-args nil)
             (adjust-args nil)
             (win-size-args nil)
             (proc nil))
        (unwind-protect
            (cl-letf (((symbol-function 'require)
                       (lambda (feat &optional file noerror)
                         (if (eq feat 'eat) t
                           (funcall orig-require feat file noerror))))
                      ((symbol-function 'eat-mode)
                       (lambda ()
                         (cl-incf eat-mode-calls)
                         (setq major-mode 'eat-mode)))
                      ((symbol-function 'eat-exec)
                       (lambda (b name cmd startfile switches)
                         (setq exec-args (list b name cmd startfile switches))
                         (when (and proc (process-live-p proc)) (delete-process proc))
                         (setq proc (start-process "test-eat" b "sleep" "30"))))
                      ((symbol-function 'eat--adjust-process-window-size)
                       (lambda (p wins)
                         (setq adjust-args (list p wins))
                         '(110 . 36)))
                      ((symbol-function 'set-process-window-size)
                       (lambda (p h w) (setq win-size-args (list p h w)))))
              (switch-to-buffer buf)
              (should-not (herdr-sanctum-terminal-live-p buf))
              (herdr-sanctum-terminal-start
               buf (cons (herdr-sanctum-terminal-resolve-backend) "/usr/bin/herdr"))
              (should (= eat-mode-calls 1))
              (should (equal exec-args (list buf "sanctum" "/usr/bin/herdr" nil '("--flag" "val"))))
              (should (local-variable-p 'eat-kill-buffer-on-exit buf))
              (should-not (buffer-local-value 'eat-kill-buffer-on-exit buf))
              (should (herdr-sanctum-terminal-live-p buf))
              ;; Restarting inside an existing `eat-mode' buffer skips `eat-mode'.
              (herdr-sanctum-eat--start buf "/usr/bin/herdr" '("--again"))
              (should (= eat-mode-calls 1))
              (should (equal (nth 4 exec-args) '("--again")))
              (herdr-sanctum-terminal-resize (selected-window))
              (should (equal adjust-args (list proc (list (selected-window)))))
              (should (equal win-size-args (list proc 36 110)))
              ;; Mode banners: char, semi-char (nil), emacs, and non-eat buffer (nil).
              (with-current-buffer buf
                (setq-local eat--char-mode t
                            eat--semi-char-mode nil)
                (let ((b1 (herdr-sanctum-terminal-mode-banner buf))
                      (b2 (herdr-sanctum-terminal-mode-banner buf)))
                  (should (eq b1 b2))
                  (should (string-search "CHAR" b1)))
                (setq-local eat--char-mode nil
                            eat--semi-char-mode t)
                (should-not (herdr-sanctum-terminal-mode-banner buf))
                (setq-local eat--semi-char-mode nil)
                (let ((b1 (herdr-sanctum-terminal-mode-banner buf))
                      (b2 (herdr-sanctum-terminal-mode-banner buf)))
                  (should (eq b1 b2))
                  (should (string-search "EMACS" b1))))
              (with-temp-buffer
                (should-not (herdr-sanctum-terminal-mode-banner (current-buffer))))
              (delete-process proc)
              (should-not (herdr-sanctum-terminal-live-p buf)))
          (when (and proc (process-live-p proc)) (delete-process proc))
          (when (buffer-live-p buf) (kill-buffer buf)))))))

(ert-deftest herdr-sanctum-optional-test/terminal-mode-line-format ()
  "`herdr-sanctum-terminal-mode-line-format' supports nil, `default', custom spec, and live `:set'."
  (herdr-sanctum-test-with-clean-state
    (let* ((orig-ml (default-value 'herdr-sanctum-terminal-mode-line-format))
           (resized 0)
           (herdr-sanctum-terminal-backends (copy-alist herdr-sanctum-terminal-backends))
           (herdr-sanctum-terminal-backend 'ml-test-backend)
           (herdr-sanctum-program "sh")
           (buf (get-buffer-create herdr-sanctum-buffer-name)))
      (herdr-sanctum-register-terminal-backend
       'ml-test-backend
       :start (lambda (b _prog _args)
                (with-current-buffer b (setq-local herdr-sanctum-test--live t)))
       :live-p (lambda (b) (buffer-local-value 'herdr-sanctum-test--live b))
       :resize (lambda (_win) (cl-incf resized)))
      (unwind-protect
          (progn
            (switch-to-buffer buf)
            ;; 1. Default nil hides buffer-local mode-line on start.
            (setopt herdr-sanctum-terminal-mode-line-format nil)
            (herdr-sanctum-terminal-start buf)
            (with-current-buffer buf
              (should (local-variable-p 'mode-line-format))
              (should-not mode-line-format))
            (setq resized 0)
            ;; 2. `default' kills buffer-local `mode-line-format' and resizes live PTY.
            (setopt herdr-sanctum-terminal-mode-line-format 'default)
            (should (= resized 1))
            (with-current-buffer buf
              (should-not (local-variable-p 'mode-line-format))
              (should (equal mode-line-format (default-value 'mode-line-format))))
            ;; 3. Custom spec sets buffer-local `mode-line-format' and resizes live PTY.
            (let ((custom-spec '(" SANCTUM " mode-line-buffer-identification)))
              (setopt herdr-sanctum-terminal-mode-line-format custom-spec)
              (should (= resized 2))
              (with-current-buffer buf
                (should (local-variable-p 'mode-line-format))
                (should (equal mode-line-format custom-spec))))
            ;; 4. Resetting to nil re-hides mode-line and resizes live PTY.
            (setopt herdr-sanctum-terminal-mode-line-format nil)
            (should (= resized 3))
            (with-current-buffer buf
              (should (local-variable-p 'mode-line-format))
              (should-not mode-line-format))
            ;; 5. When terminal is not live, `:set' still updates buffer without calling `:resize'.
            (with-current-buffer buf (setq-local herdr-sanctum-test--live nil))
            (setopt herdr-sanctum-terminal-mode-line-format 'default)
            (should (= resized 3))
            (with-current-buffer buf
              (should-not (local-variable-p 'mode-line-format))))
        (set-default 'herdr-sanctum-terminal-mode-line-format orig-ml)))))

(ert-deftest herdr-sanctum-optional-test/terminal-ghostel-banner-customization ()
  "Ghostel banner text defcustoms rebuild pre-allocated banners live with preserved faces."
  (herdr-sanctum-test-with-clean-state
    (let ((orig-copy (default-value 'herdr-sanctum-ghostel-banner-copy-text))
          (orig-emacs (default-value 'herdr-sanctum-ghostel-banner-emacs-text))
          (orig-char (default-value 'herdr-sanctum-ghostel-banner-char-text)))
      (unwind-protect
          (with-temp-buffer
            (setopt herdr-sanctum-ghostel-banner-copy-text " MY-COPY ")
            (setopt herdr-sanctum-ghostel-banner-emacs-text " MY-EMACS ")
            (setopt herdr-sanctum-ghostel-banner-char-text " MY-CHAR ")
            (dolist (spec '((copy " MY-COPY " herdr-sanctum-banner-info)
                            (emacs " MY-EMACS " herdr-sanctum-banner-info)
                            (char " MY-CHAR " herdr-sanctum-banner-warn)))
              (setq-local ghostel--input-mode (nth 0 spec))
              (let ((b1 (herdr-sanctum-ghostel--mode-banner (current-buffer)))
                    (b2 (herdr-sanctum-ghostel--mode-banner (current-buffer))))
                (should (eq b1 b2))
                (should (string-prefix-p (nth 1 spec) b1))
                (should (eq (get-text-property 0 'face b1) (nth 2 spec))))))
        (set-default 'herdr-sanctum-ghostel-banner-copy-text orig-copy)
        (set-default 'herdr-sanctum-ghostel-banner-emacs-text orig-emacs)
        (set-default 'herdr-sanctum-ghostel-banner-char-text orig-char)
        (herdr-sanctum-ghostel--rebuild-banners)))))

(provide 'herdr-sanctum-optional-test)
;;; herdr-sanctum-optional-test.el ends here
