;;; herdr-sanctum-ui-test.el --- UI, sidebar, drawer & core tests -*- lexical-binding: t; -*-

;;; Commentary:

;; Focused ERT tests covering:
;;  - `herdr-sanctum-ui': pixel-width / Emacs 30.1 `truncate-px' fallback,
;;    ellipsis cache invalidation, `format-header-line', remappings, panel
;;    padding, same-side stacking, `restore-windows', and buffer-local hooks
;;  - `herdr-sanctum-sidebar': rendering, chrome, section configs, folding,
;;    newline click/RET semantics, git-meta, digit jumps, and sole-window safety
;;  - `herdr-sanctum-drawer': Overview/Changes/Files/custom views, cursor
;;    stability, `:full-row' vs inline button semantics, narrow wrapping,
;;    `:dirty > 200', `file-name-all-completions' fast listing, resize skip,
;;    coalesced Git updates, `same-view-p' scroll reset, and offline refresh
;;  - `herdr-sanctum': entry fast-path, `toggle-back' guard, `close-dwim',
;;    tab/frame `maximize-window', `--fetch-jump-snapshot' timeout cleanup,
;;    per-frame `mode-line' badge, pre-formatted `*sanctum*' header-line,
;;    and `doctor'

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'seq)
(require 'tab-bar)
(require 'button)
(require 'face-remap)
(require 'herdr-sanctum-test-helper)

(defun herdr-sanctum-ui-test--rich-snapshot (&optional dir)
  "Build a multi-space, multi-agent snapshot fixture, optionally using DIR."
  (herdr-sanctum-test-make-snapshot
   :focused-ws "w1" :focused-tab "w1:t2" :focused-pane "w1:p2"
   :workspaces '(((workspace_id . "w1") (number . 1) (label . "def")
                  (focused . t) (tab_count . 3) (active_tab_id . "w1:t2")
                  (agent_status . "blocked"))
                 ((workspace_id . "w2") (number . 2) (label . "bpm")
                  (focused . nil) (tab_count . 2) (active_tab_id . "w2:t4")
                  (agent_status . "done")))
   :tabs '(((tab_id . "w1:t1") (workspace_id . "w1") (number . 1)
            (label . "home") (pane_count . 1) (agent_status . "unknown"))
           ((tab_id . "w1:t2") (workspace_id . "w1") (number . 2)
            (label . "dotfiles") (focused . t) (pane_count . 2) (agent_status . "working"))
           ((tab_id . "w1:t3") (workspace_id . "w1") (number . 3)
            (label . "ops") (pane_count . 1) (agent_status . "blocked"))
           ((tab_id . "w2:t4") (workspace_id . "w2") (number . 1)
            (label . "design") (pane_count . 1) (agent_status . "done"))
           ((tab_id . "w2:t5") (workspace_id . "w2") (number . 2)
            (label . "impl") (pane_count . 3) (agent_status . "idle")))
   :panes (when dir
            `(((pane_id . "w1:p2") (workspace_id . "w1") (tab_id . "w1:t2")
               (focused . t) (foreground_cwd . ,dir))))
   :agents '(((pane_id . "w1:p2") (workspace_id . "w1") (tab_id . "w1:t2")
              (agent . "pi") (agent_status . "working") (state_change_seq . 10)
              (tokens . ((topic . "dotfiles"))))
             ((pane_id . "w1:p3") (workspace_id . "w1") (tab_id . "w1:t3")
              (agent . "pi") (agent_status . "blocked") (state_change_seq . 8)
              (tokens . ((topic . "build-fix"))))
             ((pane_id . "w2:p4") (workspace_id . "w2") (tab_id . "w2:t4")
              (agent . "pi") (agent_status . "done") (state_change_seq . 12)
              (tokens . ((topic . "design"))))
             ((pane_id . "w2:p5") (workspace_id . "w2") (tab_id . "w2:t5")
              (agent . "pi") (agent_status . "idle") (state_change_seq . 20)))))

(defun herdr-sanctum-ui-test--drawer-snapshot (dir)
  "Build a single-workspace, two-pane linked-worktree snapshot for DIR."
  (herdr-sanctum-test-make-snapshot
   :workspaces '(((workspace_id . "w1") (number . 1) (label . "alpha")
                  (tab_count . 1) (active_tab_id . "w1:t1")
                  (worktree . ((is_linked_worktree . t)))))
   :tabs '(((tab_id . "w1:t1") (workspace_id . "w1") (number . 1)
            (label . "work") (pane_count . 2)))
   :panes `(((pane_id . "w1:p1") (workspace_id . "w1") (tab_id . "w1:t1")
             (focused . t) (foreground_cwd . ,dir))
            ((pane_id . "w1:p2") (workspace_id . "w1") (tab_id . "w1:t1")
             (foreground_cwd . ,dir)))))

;;; UI Infrastructure, Pixel Layout & Chrome Tests

(ert-deftest herdr-sanctum-ui-test/ui-remapping-padding-and-chrome-buttons ()
  "UI remapping neutralizes float scaling; padding and button keymaps work."
  (herdr-sanctum-test-with-clean-state
    ;; 1. Float height neutralizes compound scaling on chrome/status bars; int height does not.
    (let ((herdr-sanctum-ui-font-height 0.9))
      (let ((remaps (herdr-sanctum-ui-remapping "#161824" "#161a2b")))
        (should (equal (cadr (assq 'fringe remaps)) '(:background "#161824")))
        (should (assq 'header-line remaps))
        (should (assq 'header-line-active remaps))
        (should (assq 'header-line-inactive remaps))
        (should (equal (cadr (assq 'herdr-sanctum-chrome-bar remaps)) '(:height 1.0)))
        (should (equal (cadr (assq 'herdr-sanctum-status-bar remaps)) '(:height 1.0)))))
    (let ((herdr-sanctum-ui-font-height 110))
      (let ((remaps (herdr-sanctum-ui-remapping "#161824" "#161a2b")))
        (should-not (assq 'herdr-sanctum-chrome-bar remaps))
        (should-not (assq 'herdr-sanctum-status-bar remaps))))
    ;; 2. Terminal header remapping preserves existing non-header-line remappings.
    (with-temp-buffer
      (setq-local face-remapping-alist '((default ghostel-default)))
      (herdr-sanctum-apply-terminal-header-remapping (current-buffer))
      (should (equal (assq 'default face-remapping-alist) '(default ghostel-default)))
      (should (assq 'header-line face-remapping-alist))
      (should (assq 'header-line-active face-remapping-alist))
      (should (assq 'header-line-inactive face-remapping-alist)))
    ;; 3. Panel padding sets zero margins, TTY 0 fringes, and GUI configured fringes.
    (with-temp-buffer
      (setq-local left-margin-width 2 right-margin-width 2)
      (herdr-sanctum-apply-panel-padding)
      (should (eql left-margin-width 0))
      (should (eql right-margin-width 0))
      (should (eql left-fringe-width 0))
      (should (eql right-fringe-width 0))
      (should-not indicate-empty-lines)
      (should (equal (assq 'truncation fringe-indicator-alist) '(truncation nil nil)))
      (cl-letf (((symbol-function 'display-graphic-p) (lambda (&optional _) t)))
        (herdr-sanctum-apply-panel-padding)
        (should (eql left-fringe-width herdr-sanctum-panel-fringe-width))
        (should (eql right-fringe-width herdr-sanctum-panel-fringe-width))))
    ;; 4. Chrome buttons cache keymaps by `(AREA . KEY)' and bind target area mouse-1.
    (let ((b1 (herdr-sanctum-make-chrome-button
               " + " 'header-line #'herdr-sanctum-drawer-toggle 'default "tip"))
          (b2 (herdr-sanctum-make-chrome-button
               " + " 'header-line #'herdr-sanctum-drawer-toggle 'default "tip"))
          (b3 (herdr-sanctum-make-chrome-button
               " + " 'tab-line #'herdr-sanctum-drawer-toggle 'default "tip")))
      (should (eq (get-text-property 0 'local-map b1)
                  (get-text-property 0 'local-map b2)))
      (should-not (eq (get-text-property 0 'local-map b1)
                      (get-text-property 0 'local-map b3)))
      (should (eq (lookup-key (get-text-property 0 'local-map b1) [header-line mouse-1])
                  #'herdr-sanctum-drawer-toggle))
      (should (eq (lookup-key (get-text-property 0 'local-map b3) [tab-line mouse-1])
                  #'herdr-sanctum-drawer-toggle)))))

(ert-deftest herdr-sanctum-ui-test/pixel-width-compat-and-truncate-px-fallback ()
  "Emacs 30.1 pixel-width compat, `truncate-px' fallback, and ellipsis cache invalidation."
  (herdr-sanctum-test-with-clean-state
    ;; 1. `herdr-sanctum-string-pixel-width' handles 1-arg (Emacs 30.1) and 2-arg paths.
    (let ((seen-bufs nil)
          (other-buf (get-buffer-create " *herdr-spw-other*")))
      (unwind-protect
          (progn
            (let ((herdr-sanctum--spw-2arg-p nil))
              (cl-letf (((symbol-function 'string-pixel-width)
                         (lambda (s)
                           (push (current-buffer) seen-bufs)
                           (* 8 (length s)))))
                (should (= (herdr-sanctum-string-pixel-width "abcd" other-buf) 32))
                (should (eq (car seen-bufs) other-buf))))
            (let ((herdr-sanctum--spw-2arg-p t))
              (cl-letf (((symbol-function 'string-pixel-width)
                         (lambda (s &optional b)
                           (push b seen-bufs)
                           (* 10 (length s)))))
                (should (= (herdr-sanctum-string-pixel-width "abc" other-buf) 30))
                (should (eq (car seen-bufs) other-buf)))))
        (when (buffer-live-p other-buf) (kill-buffer other-buf))))
    ;; 2. Emacs 30.1 `truncate-px' fallback (when `truncate-string-pixelwise' is absent).
    (with-temp-buffer
      (cl-letf (((symbol-function 'truncate-string-pixelwise) nil)
                ((symbol-function 'herdr-sanctum-string-pixel-width)
                 (lambda (s &optional _b)
                   (if (equal s "…") 12 (* 8 (length s))))))
        ;; Empty and non-positive bounds return "".
        (should (equal (herdr-sanctum-truncate-px "" 40) ""))
        (should (equal (herdr-sanctum-truncate-px "abc" 0) ""))
        (should (equal (herdr-sanctum-truncate-px "abc" -5) ""))
        ;; Short string that fits returns unchanged.
        (should (equal (herdr-sanctum-truncate-px "short" 40) "short"))
        ;; `0 < max-px < ell-px' returns "" rather than overflowing with ellipsis.
        (should (equal (herdr-sanctum-truncate-px "abcdefghij" 8) ""))
        (should (equal (herdr-sanctum-truncate-px "abcdefghij" 11) ""))
        ;; `max-px == ell-px' fits ellipsis alone.
        (should (equal (herdr-sanctum-truncate-px "abcdefghij" 12) "…"))
        ;; Truncated string fits within `max-px' and applies optional `bg-face' to ellipsis.
        (let ((tr (herdr-sanctum-truncate-px "abcdefghij" 40 'herdr-sanctum-chrome-bar)))
          (should (equal (substring-no-properties tr) "abc…"))
          (should (eq (get-text-property 3 'face tr) 'herdr-sanctum-chrome-bar)))))
    ;; 3. Ellipsis pixel-width cache invalidates on `face-remapping-alist', `frame-char-width',
    ;;    and `herdr-sanctum-theme-revision'.
    (with-temp-buffer
      (let ((measure-calls 0)
            (fake-char-w 8))
        (cl-letf (((symbol-function 'herdr-sanctum-string-pixel-width)
                   (lambda (_s &optional _b)
                     (cl-incf measure-calls)
                     fake-char-w))
                  ((symbol-function 'frame-char-width)
                   (lambda (&optional _f) fake-char-w)))
          (should (= (herdr-sanctum--ellipsis-px "…" 'default (current-buffer)) 8))
          (should (= measure-calls 1))
          ;; Repeat hit reuses cached value.
          (should (= (herdr-sanctum--ellipsis-px "…" 'default (current-buffer)) 8))
          (should (= measure-calls 1))
          ;; Changing `face-remapping-alist' invalidates cache.
          (setq-local face-remapping-alist '((default (:height 1.2) default)))
          (should (= (herdr-sanctum--ellipsis-px "…" 'default (current-buffer)) 8))
          (should (= measure-calls 2))
          ;; Changing `frame-char-width' invalidates cache.
          (setq fake-char-w 11)
          (should (= (herdr-sanctum--ellipsis-px "…" 'default (current-buffer)) 11))
          (should (= measure-calls 3))
          ;; Changing `herdr-sanctum-theme-revision' invalidates cache.
          (let ((herdr-sanctum-theme-revision (1+ herdr-sanctum-theme-revision)))
            (should (= (herdr-sanctum--ellipsis-px "…" 'default (current-buffer)) 11))
            (should (= measure-calls 4))))))))

(ert-deftest herdr-sanctum-ui-test/gui-layout-header-line-escaping-and-card-row ()
  "GUI layout helpers align pixel-wise, avoid wrong-window fallback, and escape `%'."
  (herdr-sanctum-test-with-clean-state
    ;; 1. GUI path for `format-row', `format-header-line', 1-arg `card-padding-line', divider.
    (let ((win (selected-window)))
      (with-temp-buffer
        (cl-letf (((symbol-function 'display-graphic-p) (lambda (&optional _) t))
                  ((symbol-function 'window-body-width)
                   (lambda (&optional _w pixelwise) (if pixelwise 240 30)))
                  ((symbol-function 'window-fringes) (lambda (&optional _) '(10 10 nil))))
          ;; `format-row' emits pixel align-to spacer on GUI.
          (let ((row (herdr-sanctum-format-row "alpha" "main" 30 win)))
            (should (string-search "alpha" row))
            (should (string-search "main" row))
            (should (cl-some (lambda (i)
                               (let ((d (get-text-property i 'display row)))
                                 (and (eq (car-safe d) 'space)
                                      (plist-get (cdr d) :align-to))))
                             (number-sequence 0 (1- (length row))))))
          ;; `format-header-line' with explicit GUI win adds left inset alignment.
          (let ((hl (herdr-sanctum-format-header-line "Spaces" " + " win)))
            (should (equal (get-text-property 0 'display hl) '(space :align-to (0))))
            (should (string-search "Spaces" hl))
            (should (string-search "+" hl)))
          ;; `format-header-line' with `win = nil' on an undisplayed temp buffer must NOT
          ;; borrow `(selected-window)'; it falls back cleanly to 80-col padding.
          (let ((hl-undisplayed (herdr-sanctum-format-header-line "Spaces" " + ")))
            (should-not (get-text-property 0 'display hl-undisplayed))
            (should (= (string-width hl-undisplayed) 80))
            (should (string-search "Spaces" hl-undisplayed))
            (should (string-search "+" hl-undisplayed)))
          ;; 1-arg `herdr-sanctum-card-padding-line' returns "\n" on GUI win, "" on nil.
          (should (equal (substring-no-properties (herdr-sanctum-card-padding-line win)) "\n"))
          (should (equal (herdr-sanctum-card-padding-line nil) ""))
          ;; Sidebar divider inserts pixel align-to hairline on GUI.
          (erase-buffer)
          (herdr-sanctum-sidebar--insert-divider 30 win)
          (should (eq (car-safe (get-text-property (point-min) 'display)) 'space))))
      ;; On TTY, 1-arg `card-padding-line' returns "" and sidebar divider inserts `─'.
      (with-temp-buffer
        (should (equal (herdr-sanctum-card-padding-line win) ""))
        (herdr-sanctum-sidebar--insert-divider 12 win)
        (should (string-prefix-p "────────────" (buffer-string)))))
    ;; 2. Header-line fallback face and `%' escaping preserving all text properties.
    (let* ((map (make-sparse-keymap))
           (left (propertize "50% done" 'face 'bold 'help-echo "left-tip"))
           (right (propertize "100%" 'face 'herdr-sanctum-chrome-pill
                              'mouse-face 'mode-line-highlight
                              'local-map map
                              'help-echo "right-tip"))
           (hl (herdr-sanctum-format-header-line left right))
           (lpos (string-search "50%% done" hl))
           (rpos (string-search "100%%" hl)))
      (should lpos)
      (should rpos)
      (should-not (string-match-p "[^%]%[^%]" hl))
      (dolist (idx (list (+ lpos 2) (+ lpos 3)))
        (should (equal (get-text-property idx 'face hl)
                       '(bold herdr-sanctum-chrome-bar)))
        (should (equal (get-text-property idx 'help-echo hl) "left-tip")))
      (dolist (idx (list (+ rpos 3) (+ rpos 4)))
        (should (equal (get-text-property idx 'face hl)
                       '(herdr-sanctum-chrome-pill herdr-sanctum-chrome-bar)))
        (should (eq (get-text-property idx 'mouse-face hl) 'mode-line-highlight))
        (should (eq (get-text-property idx 'local-map hl) map))))
    ;; 3. Card row preserves foregrounds and extends card background onto trailing newline.
    (let* ((left (propertize "● 2 files" 'face 'warning))
           (right (propertize " Diff " 'face 'herdr-sanctum-drawer-button))
           (row (herdr-sanctum-card-row left right 30)))
      (should (string-suffix-p "\n" row))
      (should (eq (get-text-property (1- (length row)) 'face row)
                  'herdr-sanctum-drawer-card))
      (should (equal (get-text-property 0 'face row)
                     '(warning herdr-sanctum-drawer-card)))
      (let ((diff-pos (string-search "Diff" row)))
        (should diff-pos)
        (should (equal (get-text-property diff-pos 'face row)
                       '(herdr-sanctum-drawer-button herdr-sanctum-drawer-card)))))))

(ert-deftest herdr-sanctum-ui-test/panel-same-side-stacking ()
  "Same-side sidebar (`slot 0') and drawer (`slot 1') stack without eviction."
  (herdr-sanctum-test-with-clean-state
    (dolist (side '(left right))
      (herdr-sanctum-test--reset-state)
      (let ((herdr-sanctum-sidebar-side side)
            (herdr-sanctum-drawer-side side))
        (let ((sb-win (herdr-sanctum-sidebar-show nil))
              (dr-win (herdr-sanctum-drawer-show nil)))
          (should (window-live-p sb-win))
          (should (window-live-p dr-win))
          (should-not (eq sb-win dr-win))
          (should (herdr-sanctum-sidebar-visible-p))
          (should (herdr-sanctum-drawer-visible-p))
          (should (eq (window-parameter sb-win 'window-side) side))
          (should (eq (window-parameter dr-win 'window-side) side))
          (should (eql (window-parameter sb-win 'window-slot) 0))
          (should (eql (window-parameter dr-win 'window-slot) 1)))))))

(ert-deftest herdr-sanctum-ui-test/panel-restore-windows-and-shrinking-buffer ()
  "`restore-windows' preserves non-selected scroll and never clamps start to `point-max'."
  (herdr-sanctum-test-with-clean-state
    ;; 1. Direct test on a shrinking buffer: stale `window-start >= point-max' resets to `point-min'.
    (let* ((buf (get-buffer-create " *herdr-shrink-test*"))
           (win (split-window-right)))
      (unwind-protect
          (progn
            (set-window-buffer win buf)
            (with-current-buffer buf
              (dotimes (i 30)
                (insert (format "line-%02d\n" i)))
              (goto-char (point-min))
              (forward-line 20)
              (let ((start-pos (point)))
                (forward-line 5)
                (set-window-start win start-pos t)
                (set-window-point win (point)))
              (let ((state (herdr-sanctum-panel-save-windows)))
                ;; Shrink buffer dramatically so saved `start-pos' is now past `(point-max)'.
                (erase-buffer)
                (insert "only-line-1\nonly-line-2\n")
                (herdr-sanctum-panel-restore-windows state)
                (should (= (window-start win) (point-min)))
                (should (< (window-start win) (point-max)))
                (should (<= (window-point win) (1- (point-max)))))
              ;; When restored target moves above previous `window-start', start resets to `point-min'.
              (erase-buffer)
              (dotimes (i 30)
                (insert (format "row-%02d\n" i)))
              (goto-char (point-min))
              (forward-line 15)
              (set-window-start win (point) t)
              (set-window-point win (point))
              (let ((state (herdr-sanctum-panel-save-windows)))
                (herdr-sanctum-panel-restore-windows state nil (point-min))
                (should (= (window-start win) (point-min)))
                (should (= (window-point win) (point-min))))))
        (when (window-live-p win) (delete-window win))
        (when (buffer-live-p buf) (kill-buffer buf))))
    ;; 2. Live non-selected sidebar & drawer windows preserve `window-start' and `window-point'.
    (herdr-sanctum-test-with-temp-dir repo-dir
      (let* ((workspaces (cl-loop for i from 1 to 25
                                  collect `((workspace_id . ,(format "w%d" i))
                                            (number . ,i)
                                            (label . ,(format "space-%02d" i))
                                            (tab_count . 1))))
             (many-files (cl-loop for i from 1 to 25
                                  collect (cons ".M" (format "src/file-%02d.el" i))))
             (snap (herdr-sanctum-test-make-snapshot
                    :focused-ws "w1" :focused-tab "w1:t1" :focused-pane "w1:p1"
                    :workspaces workspaces
                    :tabs '(((tab_id . "w1:t1") (workspace_id . "w1") (label . "work")))
                    :panes `(((pane_id . "w1:p1") (workspace_id . "w1") (tab_id . "w1:t1")
                              (focused . t) (foreground_cwd . ,repo-dir)))
                    :agents nil)))
        (let ((other-win (selected-window))
              (sb-win (herdr-sanctum-sidebar-show nil)))
          (herdr-sanctum-apply-snapshot snap)
          (let* ((sb-buf (get-buffer herdr-sanctum-sidebar-buffer-name))
                 (target-pos (with-current-buffer sb-buf
                               (herdr-sanctum-sidebar--find-item-pos 'workspace "w18")))
                 (start-pos (with-current-buffer sb-buf
                              (herdr-sanctum-sidebar--find-item-pos 'workspace "w10"))))
            (set-window-start sb-win start-pos t)
            (set-window-point sb-win target-pos)
            (select-window other-win)
            (should-not (eq (selected-window) sb-win))
            (herdr-sanctum-sidebar-refresh)
            (should (= (window-start sb-win) start-pos))
            (should (= (window-point sb-win) target-pos))))
        (herdr-sanctum-test--put-git-cache
         repo-dir (list :branch "main" :dirty 25 :added 0 :removed 0 :files many-files))
        (setq herdr-sanctum-drawer--tab 'changes)
        (let ((other-win (selected-window))
              (dr-win (herdr-sanctum-drawer-show nil)))
          (let* ((dr-buf (get-buffer herdr-sanctum-drawer-buffer-name))
                 (start-pos (with-current-buffer dr-buf
                              (goto-char (point-min))
                              (search-forward "src/file-10.el")
                              (line-beginning-position)))
                 (target-pos (with-current-buffer dr-buf
                               (goto-char (point-min))
                               (search-forward "src/file-18.el")
                               (line-beginning-position))))
            (set-window-start dr-win start-pos t)
            (set-window-point dr-win target-pos)
            (select-window other-win)
            (should-not (eq (selected-window) dr-win))
            (herdr-sanctum-drawer-refresh)
            (should (= (window-start dr-win) start-pos))
            (should (= (window-point dr-win) target-pos))))))))

(ert-deftest herdr-sanctum-ui-test/panel-buffer-local-hooks-dirty-flush-and-per-window-width ()
  "Panel hooks stay buffer-local, flush hidden dirty panels, skip height-only resizes, and track per-window width."
  (herdr-sanctum-test-with-clean-state
    (herdr-sanctum-test-with-temp-dir repo-dir
      (let ((snap (herdr-sanctum-test-make-snapshot :dir repo-dir)))
        (herdr-sanctum-sidebar-show)
        (herdr-sanctum-drawer-show)
        (herdr-sanctum-apply-snapshot snap)
        ;; 1. Zero global hook pollution; hooks are installed buffer-locally on panels.
        (should-not (memq #'herdr-sanctum--panel-on-window-size-change
                          (default-value 'window-size-change-functions)))
        (should-not (memq #'herdr-sanctum--panel-on-window-buffer-change
                          (default-value 'window-buffer-change-functions)))
        (dolist (name (list herdr-sanctum-sidebar-buffer-name
                            herdr-sanctum-drawer-buffer-name))
          (with-current-buffer (get-buffer name)
            (should (local-variable-p 'window-size-change-functions))
            (should (local-variable-p 'window-buffer-change-functions))
            (should (memq #'herdr-sanctum--panel-on-window-size-change
                          window-size-change-functions))
            (should (memq #'herdr-sanctum--panel-on-window-buffer-change
                          window-buffer-change-functions))))
        ;; 2. Hidden dirty panel defers render and flushes on window-buffer-change display.
        (when-let* ((w (get-buffer-window herdr-sanctum-sidebar-buffer-name t)))
          (delete-window w))
        (when-let* ((w (get-buffer-window herdr-sanctum-drawer-buffer-name t)))
          (delete-window w))
        (let ((sb-renders 0)
              (dr-renders 0)
              (git-refreshes 0)
              (orig-sb (symbol-function 'herdr-sanctum-sidebar--render))
              (orig-dr (symbol-function 'herdr-sanctum-drawer--render)))
          (cl-letf (((symbol-function 'herdr-sanctum-sidebar--render)
                     (lambda (&optional win) (cl-incf sb-renders) (funcall orig-sb win)))
                    ((symbol-function 'herdr-sanctum-drawer--render)
                     (lambda (&optional win) (cl-incf dr-renders) (funcall orig-dr win)))
                    ((symbol-function 'herdr-sanctum-git-refresh-snapshot)
                     (lambda (&rest _) (cl-incf git-refreshes))))
            (herdr-sanctum-sidebar-refresh)
            (herdr-sanctum-drawer-refresh)
            (should (= sb-renders 0))
            (should (= dr-renders 0))
            (should (herdr-sanctum-panel-dirty-p herdr-sanctum-sidebar-buffer-name))
            (should (herdr-sanctum-panel-dirty-p herdr-sanctum-drawer-buffer-name))
            ;; Displaying the hidden buffer in a window flushes dirty state and runs on-flush-fn.
            (let ((sb-buf (get-buffer herdr-sanctum-sidebar-buffer-name))
                  (w (split-window-right)))
              (unwind-protect
                  (progn
                    (set-window-buffer w sb-buf)
                    (with-current-buffer sb-buf
                      (herdr-sanctum--panel-on-window-buffer-change w))
                    (should (= sb-renders 1))
                    (should (= git-refreshes 1))
                    (should-not (herdr-sanctum-panel-dirty-p sb-buf))
                    ;; Subsequent call while clean is a no-op.
                    (with-current-buffer sb-buf
                      (herdr-sanctum--panel-on-window-buffer-change w))
                    (should (= sb-renders 1)))
                (when (window-live-p w) (delete-window w))))))
        ;; 3. Height-only resizes are skipped; width tracked independently per window.
        (let* ((sb-win1 (herdr-sanctum-sidebar-show))
               (sb-buf (get-buffer herdr-sanctum-sidebar-buffer-name))
               (sb-win2 (split-window-right))
               (rendered-wins nil)
               (w1-width 30)
               (w2-width 30)
               (fake-h 40))
          (unwind-protect
              (progn
                (set-window-buffer sb-win2 sb-buf)
                (set-window-parameter sb-win1 'herdr-sanctum-panel-width 30)
                (set-window-parameter sb-win2 'herdr-sanctum-panel-width 30)
                (cl-letf (((symbol-function 'window-body-width)
                           (lambda (&optional win _px)
                             (cond ((eq win sb-win1) w1-width)
                                   ((eq win sb-win2) w2-width)
                                   (t 80))))
                          ((symbol-function 'window-body-height)
                           (lambda (&rest _) fake-h))
                          ((symbol-function 'herdr-sanctum-sidebar--render)
                           (lambda (&optional win) (push win rendered-wins))))
                  (with-current-buffer sb-buf
                    ;; Height-only change triggers 0 renders.
                    (setq fake-h 22)
                    (herdr-sanctum--panel-on-window-size-change sb-win1)
                    (herdr-sanctum--panel-on-window-size-change (selected-frame))
                    (should (null rendered-wins))
                    ;; Width change on `sb-win2' only renders `sb-win2' and updates its param.
                    (setq w2-width 42)
                    (herdr-sanctum--panel-on-window-size-change (selected-frame))
                    (should (equal rendered-wins (list sb-win2)))
                    (should (eql (window-parameter sb-win1 'herdr-sanctum-panel-width) 30))
                    (should (eql (window-parameter sb-win2 'herdr-sanctum-panel-width) 42))
                    ;; Repeat with unchanged widths triggers 0 extra renders.
                    (herdr-sanctum--panel-on-window-size-change (selected-frame))
                    (should (= (length rendered-wins) 1))
                    ;; Now changing `sb-win1' width renders `sb-win1' independently.
                    (setq w1-width 26)
                    (herdr-sanctum--panel-on-window-size-change sb-win1)
                    (should (equal rendered-wins (list sb-win1 sb-win2)))
                    (should (eql (window-parameter sb-win1 'herdr-sanctum-panel-width) 26)))))
            (when (window-live-p sb-win2) (delete-window sb-win2))))))))

;;; Sidebar Tests

(ert-deftest herdr-sanctum-ui-test/sidebar-render-and-chrome ()
  "Sidebar renders spaces/agents, pre-formatted chrome, and cursor policy."
  (herdr-sanctum-test-with-clean-state
    (let ((snap (herdr-sanctum-ui-test--rich-snapshot)))
      (herdr-sanctum-sidebar-show t)
      (herdr-sanctum-apply-snapshot snap)
      (with-current-buffer (get-buffer herdr-sanctum-sidebar-buffer-name)
        ;; Non-selected cursor disabled in sidebar mode.
        (should (local-variable-p 'cursor-in-non-selected-windows))
        (should-not cursor-in-non-selected-windows)
        ;; Pre-formatted header-line string with clickable `+` button.
        (should (stringp header-line-format))
        (let* ((hl header-line-format)
               (plus-pos (string-search "+" hl))
               (plus-map (and plus-pos (get-text-property plus-pos 'local-map hl))))
          (should (string-search "Spaces" hl))
          (should plus-pos)
          (should (keymapp plus-map))
          (should (eq (lookup-key plus-map [header-line mouse-1])
                      #'herdr-sanctum-sidebar-new-workspace)))
        ;; Pre-formatted mode-line list with space count, clickable Config, and styled tail space.
        (should (listp mode-line-format))
        (let* ((sl (nth 0 mode-line-format))
               (sr (nth 2 mode-line-format))
               (cfg-pos (string-search "Config" sr))
               (cfg-map (and cfg-pos (get-text-property cfg-pos 'local-map sr))))
          (should (eq (nth 1 mode-line-format) 'mode-line-format-right-align))
          (should (string-search "2 spaces" sl))
          (should cfg-pos)
          (should (keymapp cfg-map))
          (should (eq (lookup-key cfg-map [mode-line mouse-1])
                      #'herdr-sanctum-sidebar-open-config))
          (should (string-suffix-p " " sr))
          (should (eq (get-text-property (1- (length sr)) 'face sr)
                      'herdr-sanctum-status-bar)))
        ;; Body renders focused w1 expanded, w2 collapsed, grouped agents, topic dedup.
        (let ((text (buffer-string)))
          (should-not (string-prefix-p "Spaces" text))
          (should (string-search "def" text))
          (should (string-search "bpm" text))
          (should (string-search "2p" text))
          (should (string-search "Agents" text))
          (should (string-search "Blocked 1" text))
          (should (string-search "Working 1" text))
          (should (string-search "Done 1" text))
          (should (string-search "Idle 1" text))
          (should (string-search "build-fix" text))
          (should-not (string-search "def · build-fix" text))
          (should (string-search "bpm · design" text))
          (should (herdr-sanctum-sidebar--find-item-pos 'tab "w1:t2"))
          (should-not (herdr-sanctum-sidebar--find-item-pos 'tab "w2:t4")))
        ;; Active parent uses `space-expanded' when child tab is focused.
        (goto-char (herdr-sanctum-sidebar--find-item-pos 'workspace "w1"))
        (search-forward "def" (line-end-position))
        (should (eq (get-text-property (1- (point)) 'face)
                    'herdr-sanctum-sidebar-space-expanded))))))

(ert-deftest herdr-sanctum-ui-test/sidebar-sections-chrome-and-spacing ()
  "Sidebar `'(agents)' and `'(agents spaces)' update chrome, headers, and section spacing."
  (herdr-sanctum-test-with-clean-state
    (let ((snap (herdr-sanctum-ui-test--rich-snapshot)))
      (herdr-sanctum-sidebar-show t)
      (herdr-sanctum-apply-snapshot snap)
      ;; 1. `'(agents)' only: header is "Agents" without "+", mode-line shows agents count,
      ;;    no duplicate inline "Agents" header, no leading blank line, no spaces.
      (let ((herdr-sanctum-sidebar-sections '(agents)))
        (herdr-sanctum-sidebar-refresh)
        (with-current-buffer (get-buffer herdr-sanctum-sidebar-buffer-name)
          (should (string-search "Agents" header-line-format))
          (should-not (string-search "Spaces" header-line-format))
          (should-not (string-search "+" header-line-format))
          (should (string-search "4 agents" (nth 0 mode-line-format)))
          (should-not (string-search "space" (nth 0 mode-line-format)))
          (let ((body (buffer-string)))
            (should (string-prefix-p "Blocked 1" body))
            (should-not (string-search "Agents" body))
            (should-not (herdr-sanctum-sidebar--find-item-pos 'workspace "w1")))))
      ;; 2. `'(agents spaces)': header is "Agents & Spaces" with "+", mode-line shows both,
      ;;    inline "Agents" appears before divider + inline "Spaces".
      (let ((herdr-sanctum-sidebar-sections '(agents spaces)))
        (herdr-sanctum-sidebar-refresh)
        (with-current-buffer (get-buffer herdr-sanctum-sidebar-buffer-name)
          (should (string-search "Agents & Spaces" header-line-format))
          (should (string-search "+" header-line-format))
          (should (string-search "4 agents · 2 spaces" (nth 0 mode-line-format)))
          (let* ((body (buffer-string))
                 (p-agents (string-search "Agents" body))
                 (p-div (string-search "\n──" body))
                 (p-spaces (string-search "Spaces" body))
                 (p-def (string-search "def" body)))
            (should (and p-agents p-div p-spaces p-def))
            (should (< p-agents p-div p-spaces p-def))))))))

(ert-deftest herdr-sanctum-ui-test/sidebar-git-meta-and-tab-count-fallback ()
  "2-arg `herdr-sanctum-sidebar--space-git-meta' formats sync, branch, multi-repo, and tab fallback."
  (dolist (case `(((:branch "main" :ahead 1 :behind 0 :dirty 2) . "↑1 •")
                  ((:branch "main" :ahead 0 :behind 2 :dirty 1) . "↓2 •")
                  ((:branch "main" :ahead 2 :behind 1 :dirty 3) . "↓1↑2 •")
                  ((:branch "main" :ahead 0 :behind 0 :dirty 2) . "main •")
                  ((:branch "main" :ahead 0 :behind 0 :dirty 0) . "main")
                  ((:multi-repo t :branch "2 repos" :ahead 2 :behind 0 :dirty 2) . "↑2 •")
                  ((:multi-repo t :branch "2 repos" :ahead 0 :behind 0 :dirty 1) . "2 repos •")))
    (should (equal (substring-no-properties
                    (herdr-sanctum-sidebar--space-git-meta (car case) 1))
                   (cdr case))))
  (should (equal (substring-no-properties
                  (herdr-sanctum-sidebar--space-git-meta nil 3))
                 "3"))
  (should (equal (herdr-sanctum-sidebar--space-git-meta nil 1) "")))

(ert-deftest herdr-sanctum-ui-test/sidebar-navigation-folding-digit-and-rpc ()
  "Sidebar navigation, folding, agent-row fold guard, digit-jump safety, and RPCs work."
  (herdr-sanctum-test-with-clean-state
    (let* ((recorded nil)
           (snap (herdr-sanctum-ui-test--rich-snapshot))
           (wait-rpc (lambda (method)
                       (herdr-sanctum-test--wait-until
                        (lambda ()
                          (seq-find (lambda (r) (equal (alist-get 'method r) method)) recorded)))
                       (seq-find (lambda (r) (equal (alist-get 'method r) method)) recorded))))
      (herdr-sanctum-test-with-mock-server (_sock)
          (lambda (client data)
            (dolist (req (herdr-sanctum-test--parse-json-lines data))
              (push req recorded)
              (herdr-sanctum-test--send-json
               client `((id . ,(alist-get 'id req)) (result . ((ok . t)))))))
        (let ((herdr-sanctum-sidebar-default-expand 'focused))
          (herdr-sanctum-sidebar-show t)
          (herdr-sanctum-apply-snapshot snap)
          (with-current-buffer (get-buffer herdr-sanctum-sidebar-buffer-name)
            ;; Tab -> parent -> collapse (switches to space-active) -> expand -> first tab.
            (goto-char (herdr-sanctum-sidebar--find-item-pos 'tab "w1:t2"))
            (herdr-sanctum-sidebar-left)
            (should (equal (plist-get (herdr-sanctum-sidebar--item-at) :workspace-id) "w1"))
            (herdr-sanctum-sidebar-left)
            (should-not (herdr-sanctum-sidebar--find-item-pos 'tab "w1:t2"))
            (goto-char (herdr-sanctum-sidebar--find-item-pos 'workspace "w1"))
            (search-forward "def" (line-end-position))
            (let ((face (get-text-property (1- (point)) 'face)))
              (should (memq 'herdr-sanctum-sidebar-space-active
                            (if (listp face) face (list face)))))
            (beginning-of-line)
            (herdr-sanctum-sidebar-right)
            (should (herdr-sanctum-sidebar--find-item-pos 'tab "w1:t2"))
            (herdr-sanctum-sidebar-right)
            (should (equal (plist-get (herdr-sanctum-sidebar--item-at) :tab-id) "w1:t1"))
            ;; Accordion recovery: toggle unfocused w2 open then closed clears override.
            (goto-char (herdr-sanctum-sidebar--find-item-pos 'workspace "w2"))
            (herdr-sanctum-sidebar-toggle-fold)
            (should (herdr-sanctum-sidebar--find-item-pos 'tab "w2:t4"))
            (should (eq (gethash "w2" herdr-sanctum-sidebar--expanded-spaces 'unset) t))
            (herdr-sanctum-sidebar-toggle-fold)
            (should-not (herdr-sanctum-sidebar--find-item-pos 'tab "w2:t4"))
            (should (eq (gethash "w2" herdr-sanctum-sidebar--expanded-spaces 'unset) 'unset))
            ;; Focus switch to w2 auto-expands w2 and auto-collapses w1.
            (let ((snap2 (copy-tree snap)))
              (setf (alist-get 'focused_workspace_id snap2) "w2"
                    (alist-get 'focused_tab_id snap2) "w2:t4")
              (herdr-sanctum-apply-snapshot snap2)
              (should (herdr-sanctum-sidebar--find-item-pos 'tab "w2:t4"))
              (should-not (herdr-sanctum-sidebar--find-item-pos 'tab "w1:t2")))
            (herdr-sanctum-apply-snapshot snap)
            ;; `toggle-fold' on an `agent' row is a safe no-op (does not toggle its workspace).
            (herdr-sanctum-sidebar-next-agent)
            (should (eq (plist-get (herdr-sanctum-sidebar--item-at) :kind) 'agent))
            (should (equal (plist-get (herdr-sanctum-sidebar--item-at) :pane-id) "w1:p3"))
            (let ((before-text (buffer-string))
                  (before-pt (point)))
              (herdr-sanctum-sidebar-toggle-fold)
              (should (equal (buffer-string) before-text))
              (should (= (point) before-pt))
              (should (herdr-sanctum-sidebar--find-item-pos 'tab "w1:t2")))
            ;; `peek' on agent row dispatches `pane.focus'.
            (setq recorded nil)
            (herdr-sanctum-sidebar-peek)
            (let ((req (funcall wait-rpc "pane.focus")))
              (should req)
              (should (equal (alist-get 'pane_id (alist-get 'params req)) "w1:p3")))
            ;; Digit jump ignores non-integer events like `'return' without error,
            ;; and dispatches `workspace.focus' for digit events like `?2'.
            (setq recorded nil)
            (let ((last-command-event 'return))
              (herdr-sanctum-sidebar-select-space-by-digit))
            (should (null recorded))
            (let ((last-command-event ?2))
              (herdr-sanctum-sidebar-select-space-by-digit))
            (let ((req (funcall wait-rpc "workspace.focus")))
              (should req)
              (should (equal (alist-get 'workspace_id (alist-get 'params req)) "w2")))
            ;; Fold-all collapses and re-expands all spaces.
            (herdr-sanctum-sidebar-toggle-fold-all)
            (should-not (herdr-sanctum-sidebar--find-item-pos 'tab "w1:t2"))
            (herdr-sanctum-sidebar-toggle-fold-all)
            (should (herdr-sanctum-sidebar--find-item-pos 'tab "w2:t5"))
            ;; Move down & rename RPC.
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
              (should (equal (alist-get 'label (alist-get 'params req)) "notes")))))))))

(ert-deftest herdr-sanctum-ui-test/sidebar-newline-click-and-ret ()
  "Clicking or pressing RET on trailing `\\n' of short sidebar rows resolves to the row item."
  (herdr-sanctum-test-with-clean-state
    (let ((focused-items nil)
          (snap (herdr-sanctum-ui-test--rich-snapshot)))
      (herdr-sanctum-sidebar-show t)
      (herdr-sanctum-apply-snapshot snap)
      (with-current-buffer (get-buffer herdr-sanctum-sidebar-buffer-name)
        (cl-letf (((symbol-function 'herdr-sanctum-sidebar-focus-item)
                   (lambda (item focus-term)
                     (push (list (plist-get item :kind)
                                 (or (plist-get item :pane-id)
                                     (plist-get item :tab-id)
                                     (plist-get item :workspace-id))
                                 focus-term)
                           focused-items))))
          (let ((win (selected-window)))
            ;; Workspace, tab, and agent rows resolve on RET and mouse-1 on trailing `\n'.
            (dolist (spec '((workspace . "w2") (tab . "w1:t2") (agent . "w1:p3")))
              (goto-char (herdr-sanctum-sidebar--find-item-pos (car spec) (cdr spec)))
              (end-of-line)
              (should (eq (char-after (point)) ?\n))
              (should-not (get-text-property (point) 'herdr-sanctum-item))
              (let ((resolved (herdr-sanctum-sidebar--item-at (point))))
                (should (eq (plist-get resolved :kind) (car spec))))
              (herdr-sanctum-sidebar-activate)
              (herdr-sanctum-sidebar-mouse-click
               (list 'mouse-1 (list win (point) '(0 . 0) 0))))
            (should (equal (nreverse focused-items)
                           '((workspace "w2" t)
                             (workspace "w2" t)
                             (tab "w1:t2" t)
                             (tab "w1:t2" t)
                             (agent "w1:p3" t)
                             (agent "w1:p3" t))))
            ;; Trailing `\n' on a non-item group header line returns nil and ignores clicks.
            (setq focused-items nil)
            (goto-char (point-min))
            (search-forward "Blocked 1")
            (end-of-line)
            (should (eq (char-after (point)) ?\n))
            (should-not (herdr-sanctum-sidebar--item-at (point)))
            (herdr-sanctum-sidebar-mouse-click
             (list 'mouse-1 (list win (point) '(0 . 0) 0)))
            (should (null focused-items))))))))

(ert-deftest herdr-sanctum-ui-test/sidebar-deferred-close-tick-git-coalesce-and-sole-window ()
  "Sidebar defers group-close prompt, guards 30s tick, coalesces Git updates, and handles sole-window toggle."
  (herdr-sanctum-test-with-clean-state
    ;; 1. Guarded 30s duration tick skips render when agents section or agents are absent.
    (let ((renders 0)
          (orig-render (symbol-function 'herdr-sanctum-sidebar--render)))
      (herdr-sanctum-sidebar-show)
      (herdr-sanctum-apply-snapshot (herdr-sanctum-test-make-snapshot :agents nil))
      (cl-letf (((symbol-function 'herdr-sanctum-sidebar--render)
                 (lambda (&optional win) (cl-incf renders) (funcall orig-render win))))
        (herdr-sanctum-sidebar--on-duration-tick)
        (should (= renders 0))
        (herdr-sanctum-apply-snapshot
         (herdr-sanctum-test-make-snapshot
          :agents '(((pane_id . "w1:p1") (workspace_id . "w1") (tab_id . "w1:t1")
                     (agent . "pi") (agent_status . "working")))))
        (setq renders 0)
        (let ((herdr-sanctum-sidebar-sections '(spaces)))
          (herdr-sanctum-sidebar--on-duration-tick)
          (should (= renders 0)))
        (let ((herdr-sanctum-sidebar-sections '(spaces agents)))
          (herdr-sanctum-sidebar--on-duration-tick)
          (should (= renders 1)))
        ;; 2. Batched Git updates coalesce into 1 visible sidebar render via timer.
        (setq renders 0)
        (dotimes (_ 5)
          (herdr-sanctum-sidebar--on-git-update "/tmp/x/" '(:branch "main" :dirty 1)))
        (should (= renders 0))
        (should (timerp herdr-sanctum-sidebar--git-refresh-timer))
        (should (herdr-sanctum-test--wait-until (lambda () (= renders 1)) 1.0))
        (should-not herdr-sanctum-sidebar--git-refresh-timer)))
    ;; 3. `workspace_group_close_required' defers follow-up `y-or-n-p' via 0-delay timer.
    (let ((close-calls nil)
          (prompts nil)
          (scheduled-timers 0)
          (orig-run-at-time (symbol-function 'run-at-time)))
      (herdr-sanctum-test-with-mock-server (_sock)
          (lambda (client data)
            (dolist (req (herdr-sanctum-test--parse-json-lines data))
              (let ((id (alist-get 'id req))
                    (method (alist-get 'method req))
                    (params (alist-get 'params req)))
                (when (equal method "workspace.close")
                  (push params close-calls)
                  (if (alist-get 'close_group params)
                      (herdr-sanctum-test--send-json client `((id . ,id) (result . ((ok . t)))))
                    (herdr-sanctum-test--send-json
                     client `((id . ,id)
                              (error . ((code . "workspace_group_close_required")
                                        (message . "has linked worktrees"))))))))))
        (herdr-sanctum-sidebar-show t)
        (herdr-sanctum-apply-snapshot (herdr-sanctum-test-make-snapshot))
        (with-current-buffer (get-buffer herdr-sanctum-sidebar-buffer-name)
          (goto-char (herdr-sanctum-sidebar--find-item-pos 'workspace "w1"))
          (cl-letf (((symbol-function 'y-or-n-p)
                     (lambda (prompt) (push prompt prompts) t))
                    ((symbol-function 'run-at-time)
                     (lambda (time repeat fn &rest args)
                       (when (and (eql time 0) (null repeat))
                         (cl-incf scheduled-timers))
                       (apply orig-run-at-time time repeat fn args))))
            (herdr-sanctum-sidebar-close)
            (should (herdr-sanctum-test--wait-until
                     (lambda () (= (length close-calls) 2)) 2.0))))
        (should (= scheduled-timers 1))
        (should (= (length prompts) 2))
        (should (alist-get 'close_group (car close-calls)))))
    ;; 4. Sole-window `herdr-sanctum-sidebar-toggle' does not error when deleting sole window.
    (delete-other-windows)
    (switch-to-buffer (get-buffer-create herdr-sanctum-sidebar-buffer-name))
    (should (= (count-windows) 1))
    (should-not (window-parent (selected-window)))
    (herdr-sanctum-sidebar-toggle)
    (should (eq herdr-sanctum-sidebar--enabled 'off))
    (should (= (count-windows) 1))))

;;; Drawer Tests

(ert-deftest herdr-sanctum-ui-test/drawer-overview-dirty-and-chrome ()
  "Drawer Overview renders dirty Git state, pre-formatted chrome, and center header controls."
  (herdr-sanctum-test-with-clean-state
    (herdr-sanctum-test-with-temp-dir repo-dir
      (let ((snap (herdr-sanctum-ui-test--drawer-snapshot repo-dir)))
        (herdr-sanctum-test--put-git-cache
         repo-dir
         (list :branch "feat/layout" :ahead 2 :behind 1
               :staged 2 :unstaged 3 :untracked 1 :dirty 6
               :added 42 :removed 7
               :files '(("M." . "src/a.el") (".M" . "src/b.el") ("A." . "src/c.el")
                        (".M" . "src/d.el") (".M" . "src/e.el") ("??" . "src/f.el"))))
        (herdr-sanctum-apply-snapshot snap)
        ;; Center header-line right BEFORE drawer opens shows ordered view pills + badge.
        (let* ((right (herdr-sanctum-header-line-right))
               (p-ov (string-search "Overview" right))
               (p-ch (string-search "Changes 6" right))
               (p-fi (string-search "Files" right)))
          (should (string-search "+" right))
          (should (string-search "│" right))
          (should-not (string-search "Hide" right))
          (should (and p-ov p-ch p-fi (< p-ov p-ch p-fi))))
        ;; Open wide drawer so summary fits on one line.
        (let ((herdr-sanctum-drawer-width 48))
          (herdr-sanctum-drawer-open-view 'overview))
        (should (herdr-sanctum-drawer-visible-p))
        ;; Center header-line right AFTER drawer opens converges to single Hide toggle.
        (let ((right (herdr-sanctum-header-line-right)))
          (should (string-search "Hide" right))
          (should-not (string-search "Overview" right)))
        ;; Empty enabled-views omits stray `│'.
        (let ((herdr-sanctum-drawer-enabled-views nil))
          (cl-letf (((symbol-function 'herdr-sanctum-drawer-visible-p) (lambda (&optional _) nil)))
            (should-not (string-search "│" (herdr-sanctum-header-line-right)))))
        (with-current-buffer (get-buffer herdr-sanctum-drawer-buffer-name)
          (should (local-variable-p 'cursor-in-non-selected-windows))
          (should-not cursor-in-non-selected-windows)
          ;; Pre-formatted unified single-row header-line and status mode-line (no tab-line).
          (should (stringp header-line-format))
          (should (string-search "Overview" header-line-format))
          (should (string-search "Changes 6" header-line-format))
          (should (string-search "Files" header-line-format))
          (should (string-search "✕" header-line-format))
          (should-not tab-line-format)
          (let* ((ch-pos (string-search "Changes" header-line-format))
                 (ch-map (get-text-property ch-pos 'local-map header-line-format)))
            (should (keymapp ch-map))
            (should (lookup-key ch-map [header-line mouse-1])))
          (let* ((dsr (nth 2 mode-line-format))
                 (ref-pos (string-search "Refresh" dsr))
                 (ref-map (and ref-pos (get-text-property ref-pos 'local-map dsr))))
            (should (eq (nth 1 mode-line-format) 'mode-line-format-right-align))
            (should ref-pos)
            (should (keymapp ref-map))
            (should (eq (lookup-key ref-map [mode-line mouse-1])
                        #'herdr-sanctum-drawer-refresh-action))
            (should (string-suffix-p " " dsr))
            (should (eq (get-text-property (1- (length dsr)) 'face dsr)
                        'herdr-sanctum-status-bar)))
          ;; Summary, Git card, Workspace, and top-5 Changed Files preview.
          (let ((body (buffer-string)))
            (should (string-search "1 tab · 2 panes · 0 agents · 6 changes" body))
            (should (string-search "6 files uncommitted" body))
            (should (string-search "+42 -7 · 2 staged" body))
            (should (string-search "WORKSPACE" body))
            (should (string-search "linked worktree" body))
            (should (string-search "CHANGED FILES · 6" body))
            (should (string-search "src/a.el" body))
            (should (string-search "src/e.el" body))
            (should-not (string-search "src/f.el" body))
            (should (string-search "View all 6 changes →" body)))
          ;; Activating Git card (dirty) triggers `open-diff'.
          (let* ((diff-called nil)
                 (herdr-sanctum-diff-function (lambda () (setq diff-called t))))
            (cl-letf (((symbol-function 'herdr-sanctum-open-outside)
                       (lambda (fn &optional _d) (funcall fn))))
              (goto-char (point-min))
              (search-forward "6 files uncommitted")
              (beginning-of-line)
              (should (eq (plist-get (herdr-sanctum-drawer--item-at) :action) 'open-diff))
              (herdr-sanctum-drawer-activate)
              (should diff-called)))
          ;; Activating `6 changes' on the summary card switches to `changes'.
          (goto-char (point-min))
          (search-forward "6 changes")
          (backward-char 1)
          (should (eq (plist-get (herdr-sanctum-drawer--item-at) :action) 'switch-tab))
          (should (eq (plist-get (herdr-sanctum-drawer--item-at) :value) 'changes))
          (herdr-sanctum-drawer-activate)
          (should (eq (herdr-sanctum-drawer-current-tab) 'changes)))))))

(ert-deftest herdr-sanctum-ui-test/drawer-overview-clean-non-repo-and-status-bar ()
  "Drawer Overview handles clean/sync, dirty-without-diffstat, non-repo, and status-bar truncation."
  (herdr-sanctum-test-with-clean-state
    (herdr-sanctum-test-with-temp-dir repo-dir
      (herdr-sanctum-apply-snapshot (herdr-sanctum-ui-test--drawer-snapshot repo-dir))
      (herdr-sanctum-drawer-open-view 'overview)
      ;; 1. Clean + ahead/behind: RET on Git card opens git-status.
      (herdr-sanctum-test--put-git-cache
       repo-dir (list :branch "main" :ahead 3 :behind 2 :dirty 0 :files nil))
      (herdr-sanctum-drawer-refresh)
      (with-current-buffer (get-buffer herdr-sanctum-drawer-buffer-name)
        (let ((body (buffer-string))
              (git-dir nil))
          (should (string-search "Working tree clean" body))
          (should (string-search "↑3 ahead" body))
          (should (string-search "↓2 behind" body))
          (should (string-search "Find file" body))
          (should (string-search "Git Status" body))
          (should-not (string-search "Refresh" body))
          (let ((herdr-sanctum-git-status-function (lambda (d) (setq git-dir d))))
            (cl-letf (((symbol-function 'herdr-sanctum-open-outside)
                       (lambda (fn &optional _d) (funcall fn))))
              (goto-char (point-min))
              (search-forward "Working tree clean")
              (beginning-of-line)
              (herdr-sanctum-drawer-activate)
              (should (equal git-dir (file-name-as-directory repo-dir)))))))
      ;; 2. Dirty without diffstat omits leading `· '.
      (herdr-sanctum-test--put-git-cache
       repo-dir (list :branch "main" :dirty 2 :staged 0 :unstaged 1 :untracked 1
                      :added nil :removed nil :files '((".M" . "a.el") ("??" . "b.el"))))
      (herdr-sanctum-drawer-refresh)
      (with-current-buffer (get-buffer herdr-sanctum-drawer-buffer-name)
        (let ((body (buffer-string)))
          (should (string-search "1 modified · 1 untracked" body))
          (should-not (string-search "· 1 modified" body))))
      ;; 3. Non-repo shows `No Git repository' and hides `Git Status'.
      (herdr-sanctum-git-clear)
      (herdr-sanctum-drawer-refresh)
      (with-current-buffer (get-buffer herdr-sanctum-drawer-buffer-name)
        (let ((body (buffer-string)))
          (should (string-search "No Git repository" body))
          (should (string-search "Find file" body))
          (should-not (string-search "Git Status" body))))
      ;; 4. Drawer status-bar truncates <= 32 cols and escapes `%'.
      (let ((herdr-sanctum-drawer-width 32))
        (dolist (path (list (expand-file-name "Code/a/b/c/herdr-sanctum" (getenv "HOME"))
                            "/tmp/a/b/super-long-repository-directory-name"
                            "/tmp/work/100%done"))
          (let* ((bar (herdr-sanctum-drawer--format-status-bar
                       (list :win nil :dir path)))
                 (left (nth 0 bar))
                 (right (nth 2 bar)))
            (should (<= (+ (string-width left) (string-width right)) 32))
            (when (string-search "100%done" path)
              (should (string-search "100%%done" left))
              (should-not (string-match-p "[^%]%[^%]" left)))))))))

(ert-deftest herdr-sanctum-ui-test/drawer-narrow-summary-wrapping-and-dirty-over-200 ()
  "Narrow 32-col Overview wraps summary onto two lines; `:dirty > 200' shows true count."
  (herdr-sanctum-test-with-clean-state
    (herdr-sanctum-test-with-temp-dir repo-dir
      (let* ((many-files (cl-loop for i from 1 to 200
                                  collect (cons ".M" (format "src/f-%03d.el" i))))
             (herdr-sanctum-drawer-width 32))
        (herdr-sanctum-test--put-git-cache
         repo-dir (list :branch "main" :dirty 250 :staged 10 :unstaged 200 :untracked 40
                        :added 500 :removed 120 :files many-files))
        (herdr-sanctum-apply-snapshot (herdr-sanctum-ui-test--drawer-snapshot repo-dir))
        (herdr-sanctum-drawer-open-view 'overview)
        (with-current-buffer (get-buffer herdr-sanctum-drawer-buffer-name)
          ;; Summary wraps cleanly onto two lines at 32 cols so `250 changes' is never clipped.
          (goto-char (point-min))
          (let* ((line1 (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
                 (line2 (progn
                          (forward-line 1)
                          (buffer-substring-no-properties (line-beginning-position) (line-end-position)))))
            (should (string-search "1 tab · 2 panes" line1))
            (should-not (string-search "250 changes" line1))
            (should (string-search "0 agents · 250 changes" line2))
            ;; The wrapped `250 changes' span remains clickable.
            (search-forward "250 changes" (line-end-position))
            (backward-char 1)
            (should (eq (plist-get (herdr-sanctum-drawer--item-at) :id) 'summary-changes)))
          ;; `:dirty > 200' renders true 250 count across Overview and header-line badge.
          (let ((body (buffer-string)))
            (should (string-search "250 files uncommitted" body))
            (should (string-search "CHANGED FILES · 250" body))
            (should (string-search "View all 250 changes →" body)))
          (should (string-search "Changes 250" header-line-format)))
        ;; Changes view header also shows true 250 count (not capped 200).
        (herdr-sanctum-drawer--dispatch 'switch-tab 'changes)
        (with-current-buffer (get-buffer herdr-sanctum-drawer-buffer-name)
          (should (string-search "CHANGED FILES · 250" (buffer-string))))))))

(ert-deftest herdr-sanctum-ui-test/drawer-custom-view-and-digit-selection ()
  "Custom drawer views use single `herdr-sanctum-drawer-item' property; digit keys ignore `'return'."
  (herdr-sanctum-test-with-clean-state
    (herdr-sanctum-test-with-temp-dir repo-dir
      (let ((herdr-sanctum-drawer-views (copy-alist herdr-sanctum-drawer-views))
            (herdr-sanctum-drawer-enabled-views '(overview changes files notes))
            (custom-action-hit nil)
            (badge-ws nil))
        (herdr-sanctum-register-drawer-view
         'notes
         :label "Notes"
         :badge-fn (lambda (ctx)
                     (setq badge-ws (alist-get 'label (plist-get ctx :workspace)))
                     badge-ws)
         :render-fn (lambda (_ctx)
                      (let ((start (point)))
                        (insert "Custom Notes View\n")
                        (add-text-properties
                         start (1- (point))
                         (list 'herdr-sanctum-drawer-item
                               (herdr-sanctum-drawer--make-item
                                'custom-ping 42 'notes-row t)))))
         :action-fn (lambda (act val) (setq custom-action-hit (cons act val))))
        (herdr-sanctum-apply-snapshot (herdr-sanctum-ui-test--drawer-snapshot repo-dir))
        (should (string-search "Notes alpha" (herdr-sanctum-header-line-right)))
        (should (equal badge-ws "alpha"))
        (let ((herdr-sanctum-drawer-width 48))
          (herdr-sanctum-drawer-open-view 'overview))
        (with-current-buffer (get-buffer herdr-sanctum-drawer-buffer-name)
          (should (string-search "Notes alpha" header-line-format))
          ;; Non-integer `last-command-event' like `'return' is a safe no-op.
          (let ((last-command-event 'return))
            (herdr-sanctum-drawer-select-view-by-digit))
          (should (eq (herdr-sanctum-drawer-current-tab) 'overview))
          ;; Digit `?4' selects the 4th view (`notes').
          (let ((last-command-event ?4))
            (herdr-sanctum-drawer-select-view-by-digit))
          (should (eq (herdr-sanctum-drawer-current-tab) 'notes))
          (should (string-search "Custom Notes View" (buffer-string)))
          (goto-char (point-min))
          (search-forward "Custom Notes View")
          (beginning-of-line)
          (herdr-sanctum-drawer-activate)
          (should (equal custom-action-hit '(custom-ping . 42)))
          ;; Cycling wraps back from `notes' to `overview'.
          (herdr-sanctum-drawer-cycle-tab)
          (should (eq (herdr-sanctum-drawer-current-tab) 'overview)))))))

(ert-deftest herdr-sanctum-ui-test/drawer-cursor-stability-and-newline-semantics ()
  "Drawer keeps cursor on duplicate-action controls on refresh and restricts `\\n' to `:full-row'."
  (herdr-sanctum-test-with-clean-state
    (herdr-sanctum-test-with-temp-dir parent-dir
      (let* ((norm-parent (file-name-as-directory (expand-file-name parent-dir)))
             (alpha-dir (file-name-as-directory (expand-file-name "alpha" parent-dir)))
             (snap (herdr-sanctum-ui-test--drawer-snapshot parent-dir)))
        (make-directory alpha-dir t)
        (herdr-sanctum-apply-snapshot snap)
        ;; 1. Dirty single-repo Overview: `summary-changes' vs `all-changes' stability
        ;;    and `:full-row' vs inline-button `\n' behavior.
        (herdr-sanctum-test--put-git-cache
         parent-dir
         (list :branch "main" :dirty 6 :staged 1 :unstaged 5 :untracked 0
               :added 10 :removed 2
               :files '(("M." . "a.el") (".M" . "b.el") (".M" . "c.el")
                        (".M" . "d.el") (".M" . "e.el") (".M" . "f.el"))))
        (herdr-sanctum-drawer-open-view 'overview)
        (with-current-buffer (get-buffer herdr-sanctum-drawer-buffer-name)
          ;; Cursor on `all-changes' stays on `all-changes' across refresh (not `summary-changes').
          (goto-char (point-min))
          (search-forward "View all 6 changes")
          (beginning-of-line)
          (should (eq (plist-get (herdr-sanctum-drawer--item-at) :id) 'all-changes))
          (herdr-sanctum-drawer-refresh)
          (should (eq (plist-get (herdr-sanctum-drawer--item-at) :id) 'all-changes))
          ;; Trailing `\n' on `:full-row t' lines resolves and dispatches on RET and mouse-1.
          (let ((win (herdr-sanctum-panel-window))
                (dispatched nil))
            (cl-letf (((symbol-function 'herdr-sanctum-drawer--dispatch)
                       (lambda (act val) (push (cons act val) dispatched))))
              (end-of-line)
              (should (eq (char-after (point)) ?\n))
              (should (eq (plist-get (herdr-sanctum-drawer--item-at) :id) 'all-changes))
              (herdr-sanctum-drawer-activate)
              (herdr-sanctum-drawer-mouse-click
               (list 'mouse-1 (list win (point) '(0 . 0) 0)))
              (should (equal dispatched '((switch-tab . changes) (switch-tab . changes))))
              ;; Trailing `\n' on inline-button line (`Find file` / `Git Status`) does NOT fire.
              (setq dispatched nil)
              (goto-char (point-min))
              (search-forward "Find file")
              (end-of-line)
              (should (eq (char-after (point)) ?\n))
              (should-not (herdr-sanctum-drawer--item-at))
              (herdr-sanctum-drawer-activate)
              (herdr-sanctum-drawer-mouse-click
               (list 'mouse-1 (list win (point) '(0 . 0) 0)))
              (should (null dispatched))
              ;; Trailing `\n' on summary line (inline `6 changes' without `:full-row') returns nil.
              (goto-char (point-min))
              (search-forward "6 changes")
              (end-of-line)
              (should (eq (char-after (point)) ?\n))
              (should-not (herdr-sanctum-drawer--item-at))
              (herdr-sanctum-drawer-mouse-click
               (list 'mouse-1 (list win (point) '(0 . 0) 0)))
              (should (null dispatched)))))
        ;; 2. Clean Overview: `git-card`, `ws-branch`, and `btn-git-status` all share
        ;;    `:action open-git-status :value nil' but preserve distinct cursor targets.
        (herdr-sanctum-test--put-git-cache
         parent-dir (list :branch "main" :dirty 0 :ahead 0 :behind 0 :files nil))
        (herdr-sanctum-drawer-refresh)
        (with-current-buffer (get-buffer herdr-sanctum-drawer-buffer-name)
          (dolist (case '(("Working tree clean" . git-card)
                          ("main" . ws-branch)
                          ("Git Status" . btn-git-status)))
            (goto-char (point-min))
            (search-forward (car case))
            (backward-char 1)
            (should (eq (plist-get (herdr-sanctum-drawer--item-at) :id) (cdr case)))
            (herdr-sanctum-drawer-refresh)
            (should (eq (plist-get (herdr-sanctum-drawer--item-at) :id) (cdr case)))))
        ;; 3. Multi-repo Changes: `repo-header` vs `repo-diff-btn` both share
        ;;    `:action open-diff :value alpha-dir' and preserve distinct cursor targets.
        (herdr-sanctum-test--put-git-cache
         parent-dir
         (list :multi-repo t :root norm-parent :branch "1 repo" :dirty 1
               :files '((".M" . "alpha/a.el"))
               :repos (list (list :name "alpha" :dir alpha-dir :root alpha-dir
                                  :branch "main" :dirty 1 :files '((".M" . "a.el"))))))
        (herdr-sanctum-drawer--dispatch 'switch-tab 'changes)
        (with-current-buffer (get-buffer herdr-sanctum-drawer-buffer-name)
          (goto-char (point-min))
          (search-forward "alpha · main (1)")
          (let ((hdr-pos (match-beginning 0))
                (diff-pos (progn (search-forward "Diff" (line-end-position))
                                 (match-beginning 0))))
            ;; Cursor on `repo-header' stays on `repo-header'.
            (goto-char hdr-pos)
            (should (equal (plist-get (herdr-sanctum-drawer--item-at) :id)
                           (cons 'repo-header alpha-dir)))
            (herdr-sanctum-drawer-refresh)
            (should (equal (plist-get (herdr-sanctum-drawer--item-at) :id)
                           (cons 'repo-header alpha-dir)))
            ;; Cursor on `repo-diff-btn' stays on `repo-diff-btn'.
            (goto-char diff-pos)
            (should (equal (plist-get (herdr-sanctum-drawer--item-at) :id)
                           (cons 'repo-diff-btn alpha-dir)))
            (herdr-sanctum-drawer-refresh)
            (should (equal (plist-get (herdr-sanctum-drawer--item-at) :id)
                           (cons 'repo-diff-btn alpha-dir)))
            ;; Trailing `\n' on the multi-repo header row does not trigger inline controls.
            (end-of-line)
            (should (eq (char-after (point)) ?\n))
            (should-not (herdr-sanctum-drawer--item-at))))))))

(ert-deftest herdr-sanctum-ui-test/drawer-files-fast-listing-resize-git-coalesce-and-offline ()
  "`Files' uses `file-name-all-completions' (0 `file-directory-p'), skips resize body render, coalesces Git, and refreshes offline."
  (herdr-sanctum-test-with-clean-state
    (herdr-sanctum-test-with-temp-dir dir
      (with-temp-file (expand-file-name "hello.el" dir) (insert ";; hi\n"))
      (with-temp-file (expand-file-name "Alpha.el" dir) (insert ";; cap\n"))
      (make-directory (expand-file-name "sub" dir) t)
      (make-directory (expand-file-name "Zed" dir) t)
      (let* ((norm (file-name-as-directory (expand-file-name dir)))
             (snap (herdr-sanctum-test-make-snapshot :dir dir))
             (render-calls 0)
             (chrome-calls 0)
             (format-row-calls 0)
             (completion-reads 0)
             (dir-p-on-children 0)
             (orig-render (symbol-function 'herdr-sanctum-drawer--render))
             (orig-chrome (symbol-function 'herdr-sanctum-drawer--refresh-chrome))
             (orig-format-row (symbol-function 'herdr-sanctum-format-row))
             (orig-completions (symbol-function 'file-name-all-completions))
             (orig-dir-p (symbol-function 'file-directory-p)))
        ;; Direct check: `--read-dir-entries' uses `file-name-all-completions' with 0 `file-directory-p'.
        (with-temp-buffer
          (let ((dir-p-calls 0))
            (cl-letf (((symbol-function 'file-directory-p)
                       (lambda (&rest _) (cl-incf dir-p-calls) t)))
              (let ((res (herdr-sanctum-drawer--read-dir-entries norm)))
                (should (= dir-p-calls 0))
                (should (equal (mapcar #'file-name-nondirectory (car res)) '("sub" "Zed")))
                (should (equal (mapcar #'file-name-nondirectory (cdr res)) '("Alpha.el" "hello.el")))))))
        (herdr-sanctum-apply-snapshot snap)
        (herdr-sanctum-drawer-show)
        (cl-letf (((symbol-function 'herdr-sanctum-drawer--render)
                   (lambda (&optional win) (cl-incf render-calls) (funcall orig-render win)))
                  ((symbol-function 'herdr-sanctum-drawer--refresh-chrome)
                   (lambda () (cl-incf chrome-calls) (funcall orig-chrome)))
                  ((symbol-function 'herdr-sanctum-format-row)
                   (lambda (&rest args) (cl-incf format-row-calls) (apply orig-format-row args)))
                  ((symbol-function 'file-name-all-completions)
                   (lambda (&rest args) (cl-incf completion-reads) (apply orig-completions args)))
                  ((symbol-function 'file-directory-p)
                   (lambda (p)
                     (when (and (stringp p)
                                (string-prefix-p norm p)
                                (not (equal (file-name-as-directory p) norm)))
                       (cl-incf dir-p-on-children))
                     (funcall orig-dir-p p))))
          ;; Switching to `files' renders once, reads completions once, 0 `format-row', 0 child stats.
          (herdr-sanctum-drawer--dispatch 'switch-tab 'files)
          (should (= render-calls 1))
          (should (= completion-reads 1))
          (should (= format-row-calls 0))
          (should (= dir-p-on-children 0))
          (let* ((buf (get-buffer herdr-sanctum-drawer-buffer-name))
                 (win (herdr-sanctum-panel-window buf))
                 (tick-before (buffer-chars-modified-tick buf)))
            ;; Burst of Git updates coalesces via timer into 1 chrome-only update (body untouched).
            (dotimes (i 4)
              (herdr-sanctum-git--commit-cache norm (list :branch "main" :dirty (1+ i))))
            (should (timerp herdr-sanctum-drawer--git-refresh-timer))
            (should (= chrome-calls 0))
            (should (herdr-sanctum-test--wait-until
                     (lambda () (null herdr-sanctum-drawer--git-refresh-timer)) 1.0))
            (should (= chrome-calls 1))
            (should (= render-calls 1))
            (should (= completion-reads 1))
            (should (= (buffer-chars-modified-tick buf) tick-before))
            (with-current-buffer buf
              (should (string-search "Changes 4" header-line-format)))
            ;; Unrelated directory Git update schedules no timer.
            (herdr-sanctum-drawer--on-git-update "/tmp/other-unrelated/" '(:branch "dev" :dirty 9))
            (should-not (timerp herdr-sanctum-drawer--git-refresh-timer))
            ;; Resize skips `Files' body re-render when directory is unchanged.
            (with-current-buffer buf
              (herdr-sanctum-drawer--on-resize win))
            (should (= render-calls 1))
            (should (= completion-reads 1))
            (should (= (buffer-chars-modified-tick buf) tick-before))
            ;; Unchanged-dir snapshot skips full `Files' render.
            (herdr-sanctum-drawer--on-snapshot snap)
            (should (= render-calls 1))
            (should (= completion-reads 1))
            (should (= (buffer-chars-modified-tick buf) tick-before))
            ;; Duration tick skips built-in `files' view.
            (herdr-sanctum-drawer--on-duration-tick)
            (should (= render-calls 1))
            ;; `g' (`herdr-sanctum-drawer-refresh-action') refreshes `Files' even when disconnected.
            (with-temp-file (expand-file-name "brand-new.el" dir) (insert ";; new\n"))
            (let ((herdr-sanctum-socket-path (expand-file-name "missing.sock" herdr-sanctum-test--root)))
              (should-not (herdr-sanctum-connected-p))
              (herdr-sanctum-drawer-refresh-action))
            (should (= completion-reads 2))
            (with-current-buffer buf
              (should (string-search "brand-new.el" (buffer-string)))
              ;; Trailing `\n' resolves on `:full-row' file/dir rows, not on `.. Up / Find file'.
              (goto-char (point-min))
              (search-forward "Find file")
              (end-of-line)
              (should-not (herdr-sanctum-drawer--item-at))
              (search-forward "brand-new.el")
              (end-of-line)
              (should (eq (plist-get (herdr-sanctum-drawer--item-at) :action) 'open-file)))))
        ;; `up-dir' with a deleted `--files-dir' navigates up from effective focused dir.
        (let ((sub (expand-file-name "sub" dir)))
          (herdr-sanctum-apply-snapshot (herdr-sanctum-test-make-snapshot :dir sub))
          (setq herdr-sanctum-drawer--files-dir
                (file-name-as-directory (expand-file-name "deleted/nested" dir)))
          (herdr-sanctum-drawer-up-dir)
          (should (equal herdr-sanctum-drawer--files-dir norm)))))))

(ert-deftest herdr-sanctum-ui-test/drawer-multi-repo-stepping-and-dispatch ()
  "Multi-repo sidebar tooltip, drawer item stepping, Changes layout, and repo resolution work."
  (herdr-sanctum-test-with-clean-state
    (herdr-sanctum-test-with-temp-dir parent-dir
      (let* ((norm-parent (file-name-as-directory (expand-file-name parent-dir)))
             (alpha-dir (file-name-as-directory (expand-file-name "alpha" parent-dir)))
             (beta-dir (file-name-as-directory (expand-file-name "beta" parent-dir)))
             (gone-dir (file-name-as-directory (expand-file-name "deleted-repo" parent-dir)))
             (snap (herdr-sanctum-test-make-snapshot :dir parent-dir))
             (one-dirty
              (list :multi-repo t :root norm-parent :branch "2 repos"
                    :ahead 2 :behind 0 :staged 1 :unstaged 1 :untracked 0 :dirty 2
                    :added 10 :removed 2
                    :files '(("M." . "alpha/a.el") (".M" . "alpha/c.el"))
                    :repos (list (list :name "alpha" :dir alpha-dir :root alpha-dir
                                       :branch "main" :ahead 2 :behind 0 :dirty 2
                                       :files '(("M." . "a.el") (".M" . "c.el")))
                                 (list :name "beta" :dir beta-dir :root beta-dir
                                       :branch "dev" :ahead 0 :behind 0 :dirty 0 :files nil))))
             (two-dirty
              (list :multi-repo t :root norm-parent :branch "2 repos"
                    :ahead 0 :behind 0 :staged 1 :unstaged 2 :untracked 0 :dirty 3
                    :added 12 :removed 4
                    :files '(("M." . "alpha/a.el") (".M" . "alpha/c.el") (".M" . "beta/b.el"))
                    :repos (list (list :name "alpha" :dir alpha-dir :root alpha-dir
                                       :branch "main" :dirty 2
                                       :files '(("M." . "a.el") (".M" . "c.el")))
                                 (list :name "beta" :dir beta-dir :root beta-dir
                                       :branch "dev" :dirty 1
                                       :files '((".M" . "b.el")))))))
        (make-directory alpha-dir t)
        (make-directory beta-dir t)
        (herdr-sanctum-test--put-git-cache parent-dir two-dirty)
        (herdr-sanctum-apply-snapshot snap)
        ;; 1. Sidebar tooltip includes multi-repo breakdown.
        (herdr-sanctum-sidebar-show)
        (with-current-buffer (get-buffer herdr-sanctum-sidebar-buffer-name)
          (let ((tip (get-text-property (herdr-sanctum-sidebar--find-item-pos 'workspace "w1")
                                        'help-echo)))
            (should (string-search "2 repos: alpha [main *2], beta [dev *1]" tip))))
        ;; 2. Overview item stepping visits every span including Find file & Git Status.
        (setq herdr-sanctum-drawer--tab 'overview)
        (herdr-sanctum-drawer-show)
        (with-current-buffer (get-buffer herdr-sanctum-drawer-buffer-name)
          (should (string-search "3 files uncommitted" (buffer-string)))
          (should (string-search "REPOSITORIES · 2" (buffer-string)))
          (goto-char (point-min))
          (let ((visited nil)
                (positions nil)
                (prev -1))
            (when-let* ((it (herdr-sanctum-drawer--item-at)))
              (push (cons (plist-get it :action) (plist-get it :value)) visited)
              (push (point) positions))
            (while (/= (point) prev)
              (setq prev (point))
              (herdr-sanctum-drawer-next-item)
              (when (/= (point) prev)
                (let ((it (herdr-sanctum-drawer--item-at)))
                  (push (cons (plist-get it :action) (plist-get it :value)) visited)
                  (push (point) positions))))
            (setq visited (nreverse visited)
                  positions (nreverse positions))
            (should (member '(switch-tab . changes) visited))
            (should (member (cons 'open-git-status alpha-dir) visited))
            (should (member (cons 'open-file (expand-file-name "b.el" beta-dir)) visited))
            (should (member '(find-file . nil) visited))
            (should (member '(open-git-status . nil) visited))
            (let ((rev (list (point))) (p -1))
              (while (/= (point) p)
                (setq p (point))
                (herdr-sanctum-drawer-prev-item)
                (when (/= (point) p) (push (point) rev)))
              (should (equal rev positions)))))
        ;; 3. Grouped Changes view renders per-repo headers and Omnidiff fits <= 32.
        (herdr-sanctum-drawer--dispatch 'switch-tab 'changes)
        (with-current-buffer (get-buffer herdr-sanctum-drawer-buffer-name)
          (should (string-search "alpha · main (2)" (buffer-string)))
          (should (string-search "beta · dev (1)" (buffer-string))))
        (let ((herdr-sanctum-diff-label "Omnidiff"))
          (herdr-sanctum-drawer-refresh)
          (with-current-buffer (get-buffer herdr-sanctum-drawer-buffer-name)
            (goto-char (point-min))
            (let ((bar (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
              (should (string-search "Omnidiff" bar))
              (should (<= (string-width bar) 32)))))
        ;; 4. Smart `--resolve-repo-dir' dispatch and deleted-dir safety.
        (let ((opened nil)
              (prompted 0)
              (herdr-sanctum-diff-function #'ignore)
              (herdr-sanctum-git-status-function #'ignore))
          (cl-letf (((symbol-function 'herdr-sanctum-open-outside)
                     (lambda (_fn &optional d) (push d opened)))
                    ((symbol-function 'completing-read)
                     (lambda (_p coll &rest _) (cl-incf prompted) (caar (last coll)))))
            (should-not (herdr-sanctum-drawer--resolve-repo-dir gone-dir t))
            (herdr-sanctum-drawer--dispatch 'open-diff gone-dir)
            (should (null opened))
            (should (= prompted 0))
            (herdr-sanctum-test--put-git-cache parent-dir one-dirty)
            (herdr-sanctum-drawer--dispatch 'open-diff nil)
            (should (= prompted 0))
            (should (equal (car opened) alpha-dir))
            (herdr-sanctum-drawer--dispatch 'open-git-status nil)
            (should (= prompted 1))
            (should (equal (car opened) beta-dir))
            (delete-directory beta-dir t)
            (should (equal (herdr-sanctum-drawer--resolve-repo-dir nil nil) alpha-dir))
            (delete-directory alpha-dir t)
            (should-not (herdr-sanctum-drawer--resolve-repo-dir nil nil))))))))

(ert-deftest herdr-sanctum-ui-test/drawer-open-view-scroll-reset-coalesce-and-sole-window ()
  "Drawer `open-view' skips resize when open, resets scroll across repos, coalesces Git, and handles sole-window toggle."
  (herdr-sanctum-test-with-clean-state
    (herdr-sanctum-test-with-temp-dir repo-a
      (herdr-sanctum-test-with-temp-dir repo-b
        (let* ((files-a (cl-loop for i from 1 to 25
                                 collect (cons ".M" (format "src/a-%02d.el" i))))
               (files-b (cl-loop for i from 1 to 25
                                 collect (cons ".M" (format "src/b-%02d.el" i))))
               (fetch-calls nil)
               (resize-calls 0))
          (herdr-sanctum-test--put-git-cache
           repo-a (list :branch "main" :dirty 25 :files files-a))
          (herdr-sanctum-test--put-git-cache
           repo-b (list :branch "dev" :dirty 25 :files files-b))
          (herdr-sanctum-apply-snapshot (herdr-sanctum-test-make-snapshot :dir repo-a))
          ;; 1. `open-view' calls `terminal-resize' on open/close, skips it when switching open view.
          (cl-letf (((symbol-function 'herdr-sanctum-git-fetch-dir)
                     (lambda (d &optional force with-diffstat)
                       (push (list d force with-diffstat) fetch-calls)))
                    ((symbol-function 'herdr-sanctum-terminal-resize)
                     (lambda (&rest _) (cl-incf resize-calls)))
                    ((symbol-function 'herdr-sanctum-ensure-connected) #'ignore))
            (herdr-sanctum-drawer-open-view 'overview)
            (should (= resize-calls 1))
            (should (equal (car fetch-calls)
                           (list (file-name-as-directory (expand-file-name repo-a)) nil t)))
            ;; Switching open drawer from `overview' -> `changes' must NOT resize terminal.
            (setq fetch-calls nil)
            (herdr-sanctum-drawer-open-view 'changes)
            (should (= resize-calls 1))
            (should (eq (herdr-sanctum-drawer-current-tab) 'changes))
            (should (equal (car fetch-calls)
                           (list (file-name-as-directory (expand-file-name repo-a)) nil t)))
            ;; 2. `same-view-p' resets scroll to `point-min' when switching repos in `changes'/`overview'.
            (let* ((dr-buf (get-buffer herdr-sanctum-drawer-buffer-name))
                   (dr-win (herdr-sanctum-panel-window dr-buf))
                   (start-pos (with-current-buffer dr-buf
                                (goto-char (point-min))
                                (search-forward "src/a-10.el")
                                (line-beginning-position)))
                   (pt-pos (with-current-buffer dr-buf
                             (goto-char (point-min))
                             (search-forward "src/a-18.el")
                             (line-beginning-position))))
              (set-window-start dr-win start-pos t)
              (set-window-point dr-win pt-pos)
              ;; Same repo refresh preserves scroll.
              (herdr-sanctum-drawer-refresh)
              (should (= (window-start dr-win) start-pos))
              ;; Switching focus to `repo-b' resets scroll and point to `(point-min)'.
              (herdr-sanctum-apply-snapshot (herdr-sanctum-test-make-snapshot :dir repo-b))
              (should (= (window-start dr-win) (point-min)))
              (should (= (window-point dr-win) (point-min)))
              ;; Same check in `overview' view switching back to `repo-a'.
              (herdr-sanctum-drawer-open-view 'overview)
              (with-current-buffer dr-buf
                (goto-char (point-min))
                (search-forward "CHANGED FILES")
                (set-window-start dr-win (line-beginning-position) t)
                (set-window-point dr-win (line-beginning-position)))
              (herdr-sanctum-apply-snapshot (herdr-sanctum-test-make-snapshot :dir repo-a))
              (should (= (window-start dr-win) (point-min)))
              (should (= (window-point dr-win) (point-min))))
            ;; 3. Coalesced Git updates in `overview' batch multiple events into 1 render.
            (let ((dr-renders 0)
                  (orig-render (symbol-function 'herdr-sanctum-drawer--render)))
              (cl-letf (((symbol-function 'herdr-sanctum-drawer--render)
                         (lambda (&optional win) (cl-incf dr-renders) (funcall orig-render win))))
                (dotimes (_ 5)
                  (herdr-sanctum-drawer--on-git-update
                   (file-name-as-directory (expand-file-name repo-a))
                   '(:branch "main" :dirty 25)))
                (should (= dr-renders 0))
                (should (timerp herdr-sanctum-drawer--git-refresh-timer))
                (should (herdr-sanctum-test--wait-until (lambda () (= dr-renders 1)) 1.0))))
            ;; Closing open view via `open-view' toggles drawer closed and resizes terminal.
            (herdr-sanctum-drawer-open-view 'overview)
            (should-not (herdr-sanctum-drawer-visible-p))
            (should (= resize-calls 2)))
          ;; 4. Sole-window `herdr-sanctum-drawer-toggle' safety.
          (delete-other-windows)
          (switch-to-buffer (get-buffer-create herdr-sanctum-drawer-buffer-name))
          (should (= (count-windows) 1))
          (herdr-sanctum-drawer-toggle)
          (should (eq herdr-sanctum-drawer--enabled 'off))
          (should (= (count-windows) 1))
          ;; 5. Disconnect cleanup resets browse state and kills drawer buffer.
          (setq herdr-sanctum-drawer--files-dir repo-a
                herdr-sanctum-drawer--last-focused-dir repo-a)
          (herdr-sanctum-disconnect)
          (should-not herdr-sanctum-drawer--files-dir)
          (should-not herdr-sanctum-drawer--last-focused-dir)
          (should-not (get-buffer herdr-sanctum-drawer-buffer-name)))))))

;;; Core `herdr-sanctum' Commands & Lifecycle Tests

(ert-deftest herdr-sanctum-ui-test/core-jump-timeout-cleanup-and-agent-step ()
  "`jump' fails fast, cleans up RPC timer/proc on timeout or abort, and orders targets."
  (herdr-sanctum-test-with-clean-state
    ;; 1. Cold-start jump fails fast when socket is absent.
    (let ((herdr-sanctum-socket-path (expand-file-name "absent.sock" herdr-sanctum-test--root))
          (t0 (float-time)))
      (should-error (herdr-sanctum-jump) :type 'user-error)
      (should (< (- (float-time) t0) 0.2)))
    ;; 2. `--fetch-jump-snapshot' fails fast on immediate server close (no 600ms stall).
    (herdr-sanctum-test-with-mock-server (_sock)
        (lambda (client _data) (delete-process client))
      (let ((t0 (float-time)))
        (should-not (herdr-sanctum--fetch-jump-snapshot))
        (should (< (- (float-time) t0) 0.3))))
    ;; 3. `--fetch-jump-snapshot' cleans up in-flight proc and timer on timeout AND on quit.
    (herdr-sanctum-test-with-mock-server (_sock)
        (lambda (_client _data) nil) ; stall without replying
      (let ((spawned-proc nil)
            (spawned-timer nil)
            (orig-rpc (symbol-function 'herdr-sanctum-rpc-call)))
        ;; 3a. Quit mid-wait (C-g) triggers unwind-protect cleanup immediately.
        (cl-letf (((symbol-function 'herdr-sanctum-rpc-call)
                   (lambda (&rest args)
                     (let ((p (apply orig-rpc args)))
                       (setq spawned-proc p
                             spawned-timer (and (processp p)
                                                (process-get p 'herdr-sanctum-rpc-timer)))
                       p)))
                  ((symbol-function 'accept-process-output)
                   (lambda (&rest _) (signal 'quit nil))))
          (should (eq (condition-case nil (herdr-sanctum--fetch-jump-snapshot) (quit 'quitted))
                      'quitted))
          (should (processp spawned-proc))
          (should-not (process-live-p spawned-proc))
          (should (timerp spawned-timer))
          (should-not (memq spawned-timer timer-list)))
        ;; 3b. Full stall timeout cleans up proc and timer cleanly.
        (setq spawned-proc nil spawned-timer nil)
        (cl-letf (((symbol-function 'herdr-sanctum-rpc-call)
                   (lambda (&rest args)
                     (let ((p (apply orig-rpc args)))
                       (setq spawned-proc p
                             spawned-timer (and (processp p)
                                                (process-get p 'herdr-sanctum-rpc-timer)))
                       p))))
          (should-not (herdr-sanctum--fetch-jump-snapshot))
          (should (processp spawned-proc))
          (should-not (process-live-p spawned-proc))
          (should (timerp spawned-timer))
          (should-not (memq spawned-timer timer-list)))))
    ;; 4. Cold-start jump abort (C-g during completing-read) leaks no timers or snapshot.
    (herdr-sanctum-test-with-mock-server (_sock)
        (lambda (client data)
          (when (string-search "session.snapshot" data)
            (herdr-sanctum-test--send-json
             client '((id . "r")
                      (result . ((snapshot . ((workspaces . [((workspace_id . "w1") (label . "a"))])
                                              (tabs . []) (agents . [])))))))))
      (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) (signal 'quit nil))))
        (should (eq (condition-case nil (herdr-sanctum-jump) (quit 'quitted)) 'quitted)))
      (should-not (herdr-sanctum-snapshot))
      (should-not (timerp herdr-sanctum--duration-timer)))
    ;; 5. Priority ordering + visibility check for `jump' and `agent-step'.
    (let ((recorded nil)
          (sanctum-calls 0)
          (snap (herdr-sanctum-ui-test--rich-snapshot)))
      (herdr-sanctum-test-with-mock-server (_sock)
          (lambda (client data)
            (dolist (req (herdr-sanctum-test--parse-json-lines data))
              (push req recorded)
              (herdr-sanctum-test--send-json
               client `((id . ,(alist-get 'id req)) (result . ((ok . t)))))))
        (herdr-sanctum-apply-snapshot snap)
        (let ((cands (herdr-sanctum--jump-candidates snap))
              (buf (get-buffer-create herdr-sanctum-buffer-name)))
          (should (equal (plist-get (cdar cands) :pane-id) "w1:p3"))
          (should (equal (plist-get (cdr (nth 1 cands)) :pane-id) "w2:p4"))
          (should (equal (plist-get (cdr (nth 2 cands)) :pane-id) "w1:p2"))
          (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) (caar cands)))
                    ((symbol-function 'herdr-sanctum) (lambda (&rest _) (cl-incf sanctum-calls))))
            (herdr-sanctum-jump)
            (should (= sanctum-calls 1))
            (should (herdr-sanctum-test--wait-until
                     (lambda () (seq-find (lambda (r) (equal (alist-get 'method r) "pane.focus")) recorded)))))
          (switch-to-buffer buf)
          (setq sanctum-calls 0 recorded nil)
          (cl-letf (((symbol-function 'herdr-sanctum) (lambda (&rest _) (cl-incf sanctum-calls))))
            (herdr-sanctum-next-agent)
            (should (= sanctum-calls 0))
            (should (herdr-sanctum-test--wait-until
                     (lambda () (seq-find (lambda (r) (equal (alist-get 'method r) "pane.focus")) recorded))))))))))

(ert-deftest herdr-sanctum-ui-test/core-close-dwim ()
  "`close-dwim' defers pane.current prompt in Sanctum and kills buffer when only 1 main window + side window."
  (herdr-sanctum-test-with-clean-state
    ;; 1. Inside `*sanctum*' without snapshot: queries `pane.current' and defers `y-or-n-p'.
    (let* ((closed-id nil)
           (herdr-sanctum-terminal-backends (copy-alist herdr-sanctum-terminal-backends))
           (herdr-sanctum-terminal-backend 'close-test-backend)
           (buf (get-buffer-create herdr-sanctum-buffer-name)))
      (herdr-sanctum-register-terminal-backend
       'close-test-backend
       :start #'ignore
       :live-p (lambda (b) (buffer-local-value 'herdr-sanctum-test--live b)))
      (with-current-buffer buf (setq-local herdr-sanctum-test--live t))
      (herdr-sanctum-test-with-mock-server (_sock)
          (lambda (client data)
            (dolist (req (herdr-sanctum-test--parse-json-lines data))
              (let ((id (alist-get 'id req))
                    (method (alist-get 'method req)))
                (cond
                 ((equal method "pane.current")
                  (herdr-sanctum-test--send-json
                   client `((id . ,id) (result . ((pane_id . "w1:p99"))))))
                 ((equal method "pane.close")
                  (setq closed-id (alist-get 'pane_id (alist-get 'params req)))
                  (herdr-sanctum-test--send-json
                   client `((id . ,id) (result . ((ok . t))))))))))
        (switch-to-buffer buf)
        (cl-letf (((symbol-function 'y-or-n-p) (lambda (_p) t)))
          (herdr-sanctum-close-dwim)
          (should (herdr-sanctum-test--wait-until (lambda () (equal closed-id "w1:p99")) 1.5)))))
    ;; 2. Outside `*sanctum*' with side window + 1 main window (`window-deletable-p' => 'frame):
    ;;    kills current buffer safely instead of calling `delete-window'.
    (let ((b1 (get-buffer-create " *close-dwim-sole-main*"))
          (b2 (get-buffer-create " *close-dwim-split-main*")))
      (unwind-protect
          (progn
            (delete-other-windows)
            (switch-to-buffer b1)
            (herdr-sanctum-sidebar-show nil)
            (should (herdr-sanctum-sidebar-visible-p))
            (should (eq (current-buffer) b1))
            (should-not (eq (window-deletable-p) t))
            (herdr-sanctum-close-dwim)
            (should-not (buffer-live-p b1))
            (should (herdr-sanctum-sidebar-visible-p))
            ;; With 2 main windows (`window-deletable-p' => t): deletes window, keeps buffer alive.
            (switch-to-buffer b2)
            (let ((w2 (split-window-right)))
              (select-window w2)
              (should (eq (window-deletable-p) t))
              (herdr-sanctum-close-dwim)
              (should-not (window-live-p w2))
              (should (buffer-live-p b2))))
        (dolist (b (list b1 b2))
          (when (buffer-live-p b) (kill-buffer b)))))))

(ert-deftest herdr-sanctum-ui-test/core-maximize-window-tab-bar-and-frame ()
  "`default-maximize-window' persists across buffer switches in both `tab-bar-mode' and non-`tab-bar-mode'."
  (herdr-sanctum-test-with-clean-state
    (let ((orig-tab-bar-mode (bound-and-true-p tab-bar-mode))
          (b1 (get-buffer-create " *max-test-1*"))
          (b2 (get-buffer-create " *max-test-2*"))
          (b3 (get-buffer-create " *max-test-3*")))
      (unwind-protect
          (progn
            ;; 1. `tab-bar-mode' ON: side-window safe no-op, buffer-switch safe, per-tab isolation.
            (tab-bar-mode 1)
            (delete-other-windows)
            (switch-to-buffer b1)
            (split-window-right)
            (herdr-sanctum-sidebar-show nil)
            (should (herdr-sanctum-sidebar-visible-p))
            ;; Invoking maximize while a side window is selected is a safe no-op.
            (let ((main-win (selected-window))
                  (sb-win (get-buffer-window herdr-sanctum-sidebar-buffer-name)))
              (select-window sb-win)
              (herdr-sanctum-default-maximize-window)
              (should (eq (selected-window) sb-win))
              (should (= (seq-count (lambda (w) (not (window-parameter w 'window-side)))
                                    (window-list nil 'no-minibuf))
                         2))
              (should-not (alist-get 'herdr-sanctum-saved-window-config
                                     (cdr (tab-bar--current-tab-find))))
              (select-window main-win))
            ;; Maximize tab 1: 2 non-side + 1 side -> 1 non-side + 1 side.
            (herdr-sanctum-default-maximize-window)
            (should (= (seq-count (lambda (w) (not (window-parameter w 'window-side)))
                                  (window-list nil 'no-minibuf))
                       1))
            (should (window-configuration-p
                     (alist-get 'herdr-sanctum-saved-window-config
                                (cdr (tab-bar--current-tab-find)))))
            ;; Switch to a different buffer `b3' inside the maximized window!
            (switch-to-buffer b3)
            ;; Create tab 2 and maximize/restore independently without clobbering tab 1.
            (tab-bar-new-tab)
            (tab-bar-rename-tab "TabTwo")
            (switch-to-buffer b2)
            (split-window-below)
            (herdr-sanctum-default-maximize-window)
            (should (= (count-windows) 1))
            (switch-to-buffer b3)
            (herdr-sanctum-default-maximize-window)
            (should (= (count-windows) 2))
            (should-not (alist-get 'herdr-sanctum-saved-window-config
                                   (cdr (tab-bar--current-tab-find))))
            ;; Close tab 2 and restore tab 1 (still showing `b3' in maximized window!).
            (tab-bar-close-tab)
            (should (eq (current-buffer) b3))
            (herdr-sanctum-default-maximize-window)
            (should (= (seq-count (lambda (w) (not (window-parameter w 'window-side)))
                                  (window-list nil 'no-minibuf))
                       2))
            (should-not (alist-get 'herdr-sanctum-saved-window-config
                                   (cdr (tab-bar--current-tab-find))))
            ;; 2. `tab-bar-mode' OFF: stores on frame parameter and restores across buffer switch.
            (while (> (length (tab-bar-tabs)) 1) (tab-bar-close-tab))
            (tab-bar-mode -1)
            (when-let* ((w (get-buffer-window herdr-sanctum-sidebar-buffer-name)))
              (delete-window w))
            (delete-other-windows)
            (switch-to-buffer b1)
            (split-window-right)
            (should (= (count-windows) 2))
            (herdr-sanctum-default-maximize-window)
            (should (= (count-windows) 1))
            (should (window-configuration-p
                     (frame-parameter nil 'herdr-sanctum-saved-window-config)))
            ;; Switch buffers while maximized, then restore.
            (switch-to-buffer b3)
            (herdr-sanctum-default-maximize-window)
            (should (= (count-windows) 2))
            (should-not (frame-parameter nil 'herdr-sanctum-saved-window-config)))
        (when (bound-and-true-p tab-bar-mode)
          (while (> (length (tab-bar-tabs)) 1) (tab-bar-close-tab)))
        (tab-bar-mode (if orig-tab-bar-mode 1 -1))
        (dolist (b (list b1 b2 b3)) (when (buffer-live-p b) (kill-buffer b)))))))

(ert-deftest herdr-sanctum-ui-test/core-entry-fast-path-toggle-back-and-rollback ()
  "Entry fast-path, `toggle-back' tab-name guard, cross-boundary nav, and startup rollback work."
  (herdr-sanctum-test-with-clean-state
    (let* ((orig-tab-bar-mode (bound-and-true-p tab-bar-mode))
           (start-calls 0)
           (resize-calls 0)
           (herdr-sanctum-terminal-backends (copy-alist herdr-sanctum-terminal-backends))
           (herdr-sanctum-terminal-backend 'entry-test-backend)
           (herdr-sanctum-display-mode 'current)
           (herdr-sanctum-program "sh")
           (herdr-sanctum-sidebar-default-open nil)
           (herdr-sanctum-drawer-default-open nil)
           (other-buf (get-buffer-create " *sanctum-nav-other*")))
      (unwind-protect
          (herdr-sanctum-test-with-mock-server (_sock)
              (lambda (client data)
                (dolist (req (herdr-sanctum-test--parse-json-lines data))
                  (let ((id (alist-get 'id req))
                        (method (alist-get 'method req)))
                    (cond
                     ((equal method "pane.focus_direction")
                      (herdr-sanctum-test--send-json
                       client `((id . ,id) (result . ((changed . :false))))))
                     ((equal method "events.subscribe")
                      (herdr-sanctum-test--send-json
                       client `((id . ,id) (result . ((type . "subscription_started"))))))
                     ((equal method "session.snapshot")
                      (herdr-sanctum-test--send-json
                       client `((id . ,id)
                                (result . ((type . "session_snapshot")
                                           (snapshot . ((focused_workspace_id . "w1")
                                                        (workspaces . [((workspace_id . "w1")
                                                                        (label . "alpha"))])
                                                        (tabs . [])
                                                        (panes . [])
                                                        (agents . []))))))))))))
            (herdr-sanctum-register-terminal-backend
             'entry-test-backend
             :start (lambda (buf _prog _args)
                      (cl-incf start-calls)
                      (with-current-buffer buf (setq-local herdr-sanctum-test--live t)))
             :live-p (lambda (buf) (buffer-local-value 'herdr-sanctum-test--live buf))
             :resize (lambda (_win) (cl-incf resize-calls)))
            ;; 1. Initial start + fast-path re-entry avoids re-running :start/:resize.
            (herdr-sanctum '(4))
            (should (= start-calls 1))
            (should (= resize-calls 1))
            (let ((sanctum-win (selected-window)))
              (let ((other-win (split-window-right)))
                (set-window-buffer other-win other-buf)
                (select-window other-win)
                (cl-letf (((symbol-function 'executable-find) (lambda (&rest _) nil)))
                  (herdr-sanctum '(4)))
                (should (eq (selected-window) sanctum-win))
                (should (= start-calls 1))
                (should (= resize-calls 1))
                (should (= (count-windows) 2))
                ;; 2. Cross-boundary navigate moves from `*sanctum*' to adjacent Emacs window.
                (herdr-sanctum-navigate 'right)
                (should (herdr-sanctum-test--wait-until
                         (lambda () (eq (selected-window) other-win)) 2.0))))
            ;; 3. `default-toggle-back' refuses to switch away from a non-Sanctum tab,
            ;;    and switches back when on the Sanctum tab.
            (tab-bar-mode 1)
            (tab-bar-rename-tab "Coding")
            (tab-bar-new-tab)
            (tab-bar-rename-tab herdr-sanctum-tab-name)
            (let ((herdr-sanctum-display-mode 'tab))
              (tab-bar-select-tab-by-name "Coding")
              (should-not (herdr-sanctum-default-toggle-back))
              (should (equal (alist-get 'name (tab-bar--current-tab)) "Coding"))
              (tab-bar-select-tab-by-name herdr-sanctum-tab-name)
              (should (herdr-sanctum-default-toggle-back))
              (should (equal (alist-get 'name (tab-bar--current-tab)) "Coding")))
            (while (> (length (tab-bar-tabs)) 1) (tab-bar-close-tab))
            (tab-bar-mode -1)
            ;; 4. Startup failure rolls back windows and kills newly created buffer.
            (herdr-sanctum-disconnect)
            (when-let* ((b (get-buffer herdr-sanctum-buffer-name)))
              (kill-buffer b))
            (delete-other-windows)
            (let ((w1 (selected-window))
                  (w2 (split-window-right)))
              (set-window-buffer w1 other-buf)
              (set-window-buffer w2 (get-buffer-create "*scratch*"))
              (select-window w1)
              (let ((herdr-sanctum-program "definitely-missing-herdr-bin-xyz"))
                (should-error (herdr-sanctum '(4)) :type 'user-error)
                (should (= (count-windows) 2))
                (should-not (get-buffer herdr-sanctum-buffer-name)))
              (herdr-sanctum-register-terminal-backend
               'entry-test-backend
               :start (lambda (&rest _) (error "boom-on-start"))
               :live-p (lambda (_b) nil))
              (cl-letf (((symbol-function 'executable-find) (lambda (&rest _) "/bin/true")))
                (should-error (herdr-sanctum '(4)))
                (should (= (count-windows) 2))
                (should-not (get-buffer herdr-sanctum-buffer-name)))))
        (when (bound-and-true-p tab-bar-mode)
          (while (> (length (tab-bar-tabs)) 1) (tab-bar-close-tab)))
        (tab-bar-mode (if orig-tab-bar-mode 1 -1))
        (when (buffer-live-p other-buf) (kill-buffer other-buf))))))

(ert-deftest herdr-sanctum-ui-test/core-header-line-refresh-triggers ()
  "Pre-formatted `*sanctum*' header-line refreshes on drawer, snapshot, git, theme, and resize."
  (herdr-sanctum-test-with-clean-state
    (herdr-sanctum-test-with-temp-dir repo-dir
      (let* ((norm-dir (file-name-as-directory (expand-file-name repo-dir)))
             (herdr-sanctum-terminal-backends (copy-alist herdr-sanctum-terminal-backends))
             (herdr-sanctum-terminal-backend 'hl-test-backend)
             (herdr-sanctum-display-mode 'current)
             (herdr-sanctum-program "sh")
             (herdr-sanctum-sidebar-default-open nil)
             (herdr-sanctum-drawer-default-open nil)
             (herdr-sanctum-socket-path (expand-file-name "no.sock" herdr-sanctum-test--root)))
        (herdr-sanctum-register-terminal-backend
         'hl-test-backend
         :start (lambda (buf _prog _args)
                  (with-current-buffer buf (setq-local herdr-sanctum-test--live t)))
         :live-p (lambda (buf) (buffer-local-value 'herdr-sanctum-test--live buf)))
        (herdr-sanctum '(4))
        (let* ((buf (get-buffer herdr-sanctum-buffer-name))
               (win (selected-window))
               (format-calls 0)
               (orig-fmt (symbol-function 'herdr-sanctum-format-header-line)))
          (with-current-buffer buf
            (should (equal header-line-format
                           '(:eval (herdr-sanctum-header-line))))
            ;; Advising `herdr-sanctum-header-line' takes effect through `header-line-format'.
            (let ((adv (lambda (orig) (concat "[ADV]" (funcall orig)))))
              (advice-add 'herdr-sanctum-header-line :around adv)
              (unwind-protect
                  (should (string-prefix-p "[ADV]" (eval (cadr header-line-format))))
                (advice-remove 'herdr-sanctum-header-line adv)))
            (should (stringp herdr-sanctum--header-line-string))
            (should-not herdr-sanctum--header-line-dirty)
            ;; 1. Cached `herdr-sanctum-header-line' returns `eq' string with 0 format calls.
            (cl-letf (((symbol-function 'herdr-sanctum-format-header-line)
                       (lambda (&rest args) (cl-incf format-calls) (apply orig-fmt args))))
              (let ((h1 (herdr-sanctum-header-line))
                    (h2 (herdr-sanctum-header-line)))
                (should (eq h1 h2))
                (should (= format-calls 0)))
              ;; 2. Snapshot hook refreshes breadcrumb.
              (herdr-sanctum-apply-snapshot
               (herdr-sanctum-test-make-snapshot
                :workspaces '(((workspace_id . "w1") (label . "beta") (active_tab_id . "w1:t1")))
                :tabs '(((tab_id . "w1:t1") (workspace_id . "w1") (label . "deploy")))
                :dir repo-dir))
              (should (= format-calls 1))
              (should (string-search "beta" herdr-sanctum--header-line-string))
              (should (string-search "deploy" herdr-sanctum--header-line-string))
              (should (string-search "Overview" herdr-sanctum--header-line-string))
              ;; 3. Git update for focused dir refreshes badge; unrelated dir is ignored.
              (herdr-sanctum-git--commit-cache
               norm-dir (list :branch "main" :dirty 3 :files '((".M" . "a.el"))))
              (should (= format-calls 2))
              (should (string-search "Changes 3" herdr-sanctum--header-line-string))
              (herdr-sanctum--on-git-header-line "/tmp/other-dir/" '(:branch "x" :dirty 9))
              (should (= format-calls 2))
              ;; 4. Drawer change hook toggles header-line right between view pills and `Hide'.
              (cl-letf (((symbol-function 'herdr-sanctum-git-fetch-dir) #'ignore)
                        ((symbol-function 'herdr-sanctum-ensure-connected) #'ignore))
                (herdr-sanctum-drawer-toggle)
                (should (string-search "Hide" herdr-sanctum--header-line-string))
                (should-not (string-search "Overview" herdr-sanctum--header-line-string))
                (herdr-sanctum-drawer-toggle)
                (should (string-search "Overview" herdr-sanctum--header-line-string))
                (should-not (string-search "Hide" herdr-sanctum--header-line-string)))
              ;; 5. Theme change hook refreshes header-line.
              (let ((before-theme format-calls))
                (herdr-sanctum-apply-theme t)
                (should (= format-calls (1+ before-theme))))
              ;; 6. Terminal resize ignores unchanged width and refreshes when width changes.
              (let ((before-resize format-calls)
                    (fake-w (window-parameter win 'herdr-sanctum-header-width)))
                (cl-letf (((symbol-function 'window-body-width) (lambda (&rest _) fake-w)))
                  (herdr-sanctum--on-terminal-size-change win)
                  (should (= format-calls before-resize))
                  (setq fake-w (+ fake-w 12))
                  (herdr-sanctum--on-terminal-size-change win)
                  (should (= format-calls (1+ before-resize)))
                  (should (eql (window-parameter win 'herdr-sanctum-header-width) fake-w))))
              ;; 7. Hidden `*sanctum*' marks dirty instead of formatting, then refreshes on read.
              (let ((before-hidden format-calls))
                (set-window-dedicated-p win nil)
                (set-window-buffer win (get-buffer-create "*scratch*"))
                (should-not (herdr-sanctum-panel-window buf))
                (herdr-sanctum--sync-header-line)
                (should (= format-calls before-hidden))
                (should (buffer-local-value 'herdr-sanctum--header-line-dirty buf))
                (with-current-buffer buf
                  (herdr-sanctum-header-line)
                  (should (= format-calls (1+ before-hidden)))
                  (should-not herdr-sanctum--header-line-dirty))))))))))

(ert-deftest herdr-sanctum-ui-test/core-mode-line-badge-per-frame-doctor-and-autoloads ()
  "Global badge respects per-frame sidebar visibility; `open-outside', `doctor', keymap, and autoloads work."
  (herdr-sanctum-test-with-clean-state
    (herdr-sanctum-test-with-temp-dir dir
      ;; 1. Global attention badge updates on snapshot, clears on disconnect, and checks `(window-frame)'.
      (herdr-sanctum-apply-snapshot
       (herdr-sanctum-test-make-snapshot
        :agents '(((pane_id . "p1") (agent_status . "blocked"))
                  ((pane_id . "p2") (agent_status . "done")))))
      (should (string-search "◆1" herdr-sanctum--mode-line-string))
      (should (string-search "✓1" herdr-sanctum--mode-line-string))
      (should (equal (herdr-sanctum-mode-line) herdr-sanctum--mode-line-string))
      ;; Visible sidebar on the current frame hides the badge.
      (herdr-sanctum-sidebar-show)
      (should-not (herdr-sanctum-mode-line))
      ;; Per-frame check: hides only on frames where sidebar is visible.
      (let (queried-frames)
        (cl-letf (((symbol-function 'herdr-sanctum-sidebar-visible-p)
                   (lambda (&optional frame)
                     (push frame queried-frames)
                     (eq frame 'frame-with-sidebar))))
          (cl-letf (((symbol-function 'window-frame) (lambda (&optional _) 'frame-with-sidebar)))
            (should-not (herdr-sanctum-mode-line)))
          (cl-letf (((symbol-function 'window-frame) (lambda (&optional _) 'frame-without-sidebar)))
            (should (equal (herdr-sanctum-mode-line) herdr-sanctum--mode-line-string)))
          (should (equal (nreverse queried-frames)
                         '(frame-with-sidebar frame-without-sidebar)))))
      ;; Disconnect clears badge.
      (herdr-sanctum-disconnect)
      (should-not herdr-sanctum--mode-line-string)
      ;; 2. `open-outside' binds `default-directory' or delegates to custom function.
      (let ((seen-dir nil))
        (herdr-sanctum-open-outside (lambda () (setq seen-dir default-directory)) dir)
        (should (equal seen-dir dir)))
      (let* ((dispatched nil)
             (herdr-sanctum-open-outside-function
              (lambda (fn d) (setq dispatched (cons fn d)))))
        (herdr-sanctum-open-outside #'ignore dir)
        (should (equal dispatched (cons #'ignore dir))))
      ;; 3. `herdr-sanctum-doctor' reports environment and config recommendations.
      (save-window-excursion
        (herdr-sanctum-doctor)
        (with-current-buffer "*herdr-sanctum-doctor*"
          (let ((text (buffer-string)))
            (should (string-search "herdr-sanctum doctor" text))
            (should (string-search "terminal backend" text))
            (should (string-search "sidebar_start_collapsed = true" text)))))
      ;; 4. Prefix keymap bindings and Emacs 30.1 package/autoload headers.
      (should-not (keymap-lookup herdr-sanctum-command-map "I"))
      (should (eq (keymap-lookup herdr-sanctum-command-map "s") #'herdr-sanctum))
      (should (eq (keymap-lookup herdr-sanctum-command-map "?") #'herdr-sanctum-doctor))
      (with-temp-buffer
        (insert-file-contents (locate-library "herdr-sanctum.el"))
        (goto-char (point-min))
        (should (search-forward "Package-Requires: ((emacs \"30.1\"))" nil t))
        (goto-char (point-min))
        (should (re-search-forward ";;;###autoload\n(defun herdr-sanctum (" nil t))
        (goto-char (point-min))
        (should (re-search-forward ";;;###autoload\n(defvar-keymap herdr-sanctum-command-map" nil t))
        (goto-char (point-min))
        (should-not (re-search-forward ";;;###autoload\n(defun herdr-sanctum--fetch-jump-snapshot" nil t))
        (goto-char (point-min))
        (should-not (re-search-forward ";;;###autoload\n(defun herdr-sanctum-default-toggle-back" nil t))))))

(ert-deftest herdr-sanctum-ui-test/emacs-30-window-hooks-from-other-buffer ()
  "Window hooks work when `current-buffer' stays on another buffer (Emacs 30.1/30.2)."
  (herdr-sanctum-test-with-clean-state
    (herdr-sanctum-test-with-temp-dir repo-dir
      (let* ((snap (herdr-sanctum-test-make-snapshot :dir repo-dir))
             (term-buf (get-buffer-create herdr-sanctum-buffer-name))
             (term-win (selected-window)))
        (set-window-buffer term-win term-buf)
        (herdr-sanctum-apply-snapshot snap)
        (let* ((sb-win (herdr-sanctum-sidebar-show nil))
               (sb-buf (get-buffer herdr-sanctum-sidebar-buffer-name))
               (dr-win (herdr-sanctum-drawer-show nil))
               (dr-buf (get-buffer herdr-sanctum-drawer-buffer-name))
               (sb-renders 0)
               (sb-render-buf nil)
               (sb-flushes 0)
               (dr-resizes 0)
               (dr-resize-buf nil)
               (hl-refreshes 0)
               (hl-refresh-buf nil)
               (orig-sb (symbol-function 'herdr-sanctum-sidebar--render))
               (orig-dr-resize (symbol-function 'herdr-sanctum-drawer--on-resize))
               (orig-hl-refresh (symbol-function 'herdr-sanctum--refresh-header-line)))
          (herdr-sanctum--refresh-header-line term-win)
          (let ((orig-term-w (window-parameter term-win 'herdr-sanctum-header-width)))
            (set-window-parameter dr-win 'herdr-sanctum-panel-width 30)
            (herdr-sanctum-panel-mark-dirty sb-buf)
            (cl-letf (((symbol-function 'herdr-sanctum-sidebar--render)
                       (lambda (&optional win)
                         (cl-incf sb-renders)
                         (setq sb-render-buf (current-buffer))
                         (funcall orig-sb win)))
                      ((symbol-function 'herdr-sanctum-git-refresh-snapshot)
                       (lambda (&rest _) (cl-incf sb-flushes)))
                      ((symbol-function 'herdr-sanctum-drawer--on-resize)
                       (lambda (&optional win)
                         (cl-incf dr-resizes)
                         (setq dr-resize-buf (current-buffer))
                         (funcall orig-dr-resize win)))
                      ((symbol-function 'herdr-sanctum--refresh-header-line)
                       (lambda (&optional win)
                         (cl-incf hl-refreshes)
                         (setq hl-refresh-buf (current-buffer))
                         (funcall orig-hl-refresh win)))
                      ((symbol-function 'window-body-width)
                       (lambda (&optional win _px)
                         (cond ((eq win dr-win) 48)
                               ((eq win term-win) (+ orig-term-w 14))
                               (t 30)))))
              ;; Invoke all 3 hook functions with an unrelated buffer current.
              (with-temp-buffer
                (should-not (eq (current-buffer) sb-buf))
                (should-not (eq (current-buffer) dr-buf))
                (should-not (eq (current-buffer) term-buf))
                ;; 1. Dirty sidebar flushes on `window-buffer-change'.
                (herdr-sanctum--panel-on-window-buffer-change sb-win)
                (should (= sb-renders 1))
                (should (eq sb-render-buf sb-buf))
                (should (= sb-flushes 1))
                (should-not (herdr-sanctum-panel-dirty-p sb-buf))
                ;; 2. Drawer resizes on `window-size-change'.
                (herdr-sanctum--panel-on-window-size-change dr-win)
                (should (= dr-resizes 1))
                (should (eq dr-resize-buf dr-buf))
                (should (eql (window-parameter dr-win 'herdr-sanctum-panel-width) 48))
                ;; 3. `*sanctum*' header-line refreshes on terminal size change.
                (herdr-sanctum--on-terminal-size-change term-win)
                (should (= hl-refreshes 1))
                (should (eq hl-refresh-buf term-buf))
                (should (eql (window-parameter term-win 'herdr-sanctum-header-width)
                             (+ orig-term-w 14)))
                ;; Dirty-only header-line refresh also reads `*sanctum*'s buffer-local flag.
                (with-current-buffer term-buf
                  (setq herdr-sanctum--header-line-dirty t))
                (herdr-sanctum--on-terminal-size-change term-win)
                (should (= hl-refreshes 2))
                (should-not (buffer-local-value 'herdr-sanctum--header-line-dirty term-buf))
                ;; Unrelated window is a safe no-op for all 3 hooks.
                (let ((other-win (split-window term-win nil 'below)))
                  (unwind-protect
                      (progn
                        (set-window-buffer other-win (get-buffer-create "*scratch*"))
                        (herdr-sanctum--panel-on-window-buffer-change other-win)
                        (herdr-sanctum--panel-on-window-size-change other-win)
                        (herdr-sanctum--on-terminal-size-change other-win)
                        (should (= sb-renders 1))
                        (should (= dr-resizes 1))
                        (should (= hl-refreshes 2)))
                    (when (window-live-p other-win)
                      (delete-window other-win))))))))))))

(ert-deftest herdr-sanctum-ui-test/panel-sole-main-window-alongside-side-window ()
  "`toggle-panel' and `cleanup-panel' do not crash when panel is the sole main window alongside a side window."
  (herdr-sanctum-test-with-clean-state
    (delete-other-windows)
    (let* ((main-win (selected-window))
           (sb-buf (get-buffer-create herdr-sanctum-sidebar-buffer-name)))
      (with-current-buffer sb-buf
        (herdr-sanctum-sidebar-mode))
      (set-window-buffer main-win sb-buf)
      (let ((dr-win (cl-letf (((symbol-function 'herdr-sanctum-git-fetch-dir) #'ignore))
                      (herdr-sanctum-drawer-show nil))))
        (should (= (count-windows) 2))
        (should (eq (window-buffer main-win) sb-buf))
        ;; Main window has a parent (internal root with side window) yet is NOT deletable.
        (should (window-parent main-win))
        (should-not (eq (window-deletable-p main-win) t))
        (should (window-live-p dr-win))
        ;; `herdr-sanctum-sidebar-toggle' must succeed without error.
        (herdr-sanctum-sidebar-toggle)
        (should (eq herdr-sanctum-sidebar--enabled 'off))
        (should (window-live-p main-win))
        (should (window-live-p dr-win))
        (should (buffer-live-p sb-buf))
        ;; `herdr-sanctum-cleanup-panel' must succeed and still kill the buffer.
        (herdr-sanctum-cleanup-panel herdr-sanctum-sidebar-buffer-name)
        (should-not (get-buffer herdr-sanctum-sidebar-buffer-name))
        (should (window-live-p main-win))
        (should (window-live-p dr-win))))))

(ert-deftest herdr-sanctum-ui-test/pixel-width-1arg-face-remapping-fallback ()
  "Emacs 30.1 1-arg `string-pixel-width' fallback honors `face-remapping-alist' via `buffer-text-pixel-size'."
  (herdr-sanctum-test-with-clean-state
    (let ((herdr-sanctum--spw-2arg-p nil)
          (target (get-buffer-create " *herdr-spw-remap-test*"))
          (remap '((default (:height 0.85) default))))
      (unwind-protect
          (progn
            ;; 1. Empty string returns 0 immediately without calling either measurer.
            (cl-letf (((symbol-function 'string-pixel-width)
                       (lambda (&rest _) (ert-fail "string-pixel-width called on empty string")))
                      ((symbol-function 'buffer-text-pixel-size)
                       (lambda (&rest _) (ert-fail "buffer-text-pixel-size called on empty string"))))
              (should (= (herdr-sanctum-string-pixel-width "" target) 0)))
            ;; 2. With `face-remapping-alist' nil on target, falls back to 1-arg `string-pixel-width'.
            (with-current-buffer target
              (setq-local face-remapping-alist nil))
            (let ((spw-buf nil))
              (cl-letf (((symbol-function 'string-pixel-width)
                         (lambda (s)
                           (setq spw-buf (current-buffer))
                           (* 7 (length s))))
                        ((symbol-function 'buffer-text-pixel-size)
                         (lambda (&rest _)
                           (ert-fail "buffer-text-pixel-size should not run when remap is nil"))))
                (should (= (herdr-sanctum-string-pixel-width "abcd" target) 28))
                (should (eq spw-buf target))))
            ;; 3. With `face-remapping-alist' non-nil on target, measures in a temp buffer
            ;;    with target's remappings and calls `(buffer-text-pixel-size nil nil t)'.
            (with-current-buffer target
              (setq-local face-remapping-alist remap))
            (let ((btps-args nil)
                  (btps-buf nil)
                  (btps-remap nil)
                  (btps-truncate nil)
                  (btps-text nil)
                  (btps-props nil))
              (cl-letf (((symbol-function 'string-pixel-width)
                         (lambda (&rest _)
                           (ert-fail "1-arg string-pixel-width should not run when remap is non-nil")))
                        ((symbol-function 'buffer-text-pixel-size)
                         (lambda (&rest args)
                           (setq btps-args args
                                 btps-buf (current-buffer)
                                 btps-remap face-remapping-alist
                                 btps-truncate truncate-lines
                                 btps-text (buffer-string)
                                 btps-props (list (get-text-property (point-min) 'display-line-numbers-disable)
                                                  (get-text-property (point-min) 'line-prefix)
                                                  (get-text-property (point-min) 'wrap-prefix)
                                                  (get-text-property (point-min) 'face)))
                           '(42 . 16))))
                (should (= (herdr-sanctum-string-pixel-width
                            (propertize "abcd" 'face 'bold) target)
                           42))
                (should (equal btps-args '(nil nil t)))
                (should-not (eq btps-buf target))
                (should (equal btps-remap remap))
                (should (eq btps-truncate t))
                (should (equal btps-text "abcd"))
                (should (equal btps-props '(t "" "" bold)))
                ;; Also works when BUF is nil and `current-buffer' carries the remapping.
                (setq btps-remap nil btps-buf nil)
                (with-current-buffer target
                  (should (= (herdr-sanctum-string-pixel-width "xy") 42)))
                (should-not (eq btps-buf target))
                (should (equal btps-remap remap)))))
        (when (buffer-live-p target)
          (kill-buffer target))))))

(ert-deftest herdr-sanctum-ui-test/drawer-render-frame-context-and-no-dead-revision ()
  "`herdr-sanctum-drawer--render' renders inside WIN's frame context and dead revision var is gone."
  (herdr-sanctum-test-with-clean-state
    (should-not (boundp 'herdr-sanctum-drawer--views-revision))
    (let* ((dr-win (cl-letf (((symbol-function 'herdr-sanctum-git-fetch-dir) #'ignore))
                     (herdr-sanctum-drawer-show nil)))
           (dr-buf (get-buffer herdr-sanctum-drawer-buffer-name))
           (orig-frame (selected-frame))
           (cur-frame orig-frame)
           (seen-frame nil)
           (seen-buf nil)
           (orig-window-frame (symbol-function 'window-frame))
           (orig-frame-live-p (symbol-function 'frame-live-p))
           (orig-render-overview (symbol-function 'herdr-sanctum-drawer--render-overview)))
      (cl-letf (((symbol-function 'selected-frame) (lambda () cur-frame))
                ((symbol-function 'frame-live-p)
                 (lambda (f) (or (eq f 'other-frame) (funcall orig-frame-live-p f))))
                ((symbol-function 'select-frame)
                 (lambda (f &optional _norecord)
                   (setq cur-frame f)
                   ;; Real `select-frame' switches `current-buffer' to that frame's buffer.
                   (set-buffer (get-buffer-create "*scratch*"))
                   f))
                ((symbol-function 'window-frame)
                 (lambda (&optional w)
                   (if (eq w dr-win) 'other-frame (funcall orig-window-frame w))))
                ((symbol-function 'display-graphic-p) (lambda (&optional _) nil))
                ((symbol-function 'herdr-sanctum-drawer--render-overview)
                 (lambda (ctx)
                   (setq seen-frame (selected-frame)
                         seen-buf (current-buffer))
                   (funcall orig-render-overview ctx))))
        (with-current-buffer dr-buf
          (herdr-sanctum-drawer--render dr-win)
          (should (eq seen-frame 'other-frame))
          (should (eq seen-buf dr-buf))
          (should (eq (selected-frame) orig-frame))
          (should (eq (current-buffer) dr-buf)))))))

(ert-deftest herdr-sanctum-ui-test/ergonomics-preserve-size-narrow-collapse-and-truncate-px ()
  "Side panels preserve size, auto-collapse on narrow frames, and truncate safely from narrow windows."
  (herdr-sanctum-test-with-clean-state
    ;; 1. Side panels set `window-preserved-size' horizontally on show.
    (let ((sb-win (herdr-sanctum-sidebar-show nil)))
      (should (window-preserved-size sb-win t)))
    ;; 2. Toggling opposite drawer on narrow frame auto-collapses sidebar; wide frame keeps both.
    (let ((herdr-sanctum-min-terminal-width 40)
          (herdr-sanctum-sidebar-width 30)
          (herdr-sanctum-drawer-width 34)
          (herdr-sanctum-sidebar-side 'left)
          (herdr-sanctum-drawer-side 'right)
          (sb-hook-runs 0))
      (add-hook 'herdr-sanctum-sidebar-change-hook (lambda () (cl-incf sb-hook-runs)))
      (unwind-protect
          (cl-letf (((symbol-function 'herdr-sanctum-git-fetch-dir) #'ignore)
                    ((symbol-function 'herdr-sanctum-ensure-connected) #'ignore)
                    ((symbol-function 'frame-width) (lambda (&optional _) 80)))
            (should (herdr-sanctum-sidebar-visible-p))
            (herdr-sanctum-drawer-toggle)
            (should (herdr-sanctum-drawer-visible-p))
            (should-not (herdr-sanctum-sidebar-visible-p))
            (should (> sb-hook-runs 0)))
        (setq herdr-sanctum-sidebar-change-hook nil)))
    ;; 3. `herdr-sanctum-truncate-px' skips C `truncate-string-pixelwise' when selected-window is narrower.
    (with-temp-buffer
      (let ((tsp-called nil))
        (cl-letf (((symbol-function 'display-graphic-p) (lambda (&optional _) t))
                  ((symbol-function 'window-body-width) (lambda (&optional _w _px) 120))
                  ((symbol-function 'truncate-string-pixelwise)
                   (lambda (&rest _) (setq tsp-called t) "WRONG-UNTRUNCATED"))
                  ((symbol-function 'herdr-sanctum-string-pixel-width)
                   (lambda (s &optional _b) (if (equal s "…") 7 (* 7 (length s))))))
          (let ((res (herdr-sanctum-truncate-px "docs/architecture-overview-and-security-notes.md" 155)))
            (should-not tsp-called)
            (should (string-suffix-p "…" res))
            (should (<= (herdr-sanctum-string-pixel-width res (current-buffer)) 155))))))
    ;; 4. `herdr-sanctum-truncate-path' preserves trailing workspace directory segments.
    (with-temp-buffer
      (let ((tr (herdr-sanctum-truncate-path "/tmp/sanctum-audit/projects/acme-api" 24)))
        (should (equal tr "…/projects/acme-api")))
      (let ((tr (herdr-sanctum-truncate-path "/tmp/sanctum-audit/projects/acme-api" 14)))
        (should (equal tr "…/acme-api"))))))

(ert-deftest herdr-sanctum-ui-test/ergonomics-jump-matching-header-badge-and-open-outside ()
  "Vanilla jump matches without leading glyphs, header shows hidden-sidebar badge, and open-outside protects Sanctum tab."
  (herdr-sanctum-test-with-clean-state
    (let* ((orig-tab-bar-mode (bound-and-true-p tab-bar-mode))
           (snap (herdr-sanctum-ui-test--rich-snapshot)))
      (unwind-protect
          (progn
            ;; 1. Jump candidates start with workspace name (no leading glyph) and carry `:glyph'.
            (let* ((cands (herdr-sanctum--jump-candidates snap))
                   (completion-styles '(basic partial-completion)))
              (should (string-prefix-p "def / " (substring-no-properties (caar cands))))
              (should (plist-get (cdar cands) :glyph))
              (should (completion-all-completions "def" cands nil 3)))
            ;; 2. Header-line right surfaces attention badge when sidebar is hidden, hides when visible.
            (herdr-sanctum-apply-snapshot snap)
            (should-not (herdr-sanctum-sidebar-visible-p))
            (let ((right-hidden (herdr-sanctum-header-line-right)))
              (should (string-search "◆1" right-hidden))
              (should (string-search "✓1" right-hidden)))
            (herdr-sanctum-sidebar-show)
            (let ((right-visible (herdr-sanctum-header-line-right)))
              (should-not (string-search "◆1" right-visible))
              (should-not (string-search "✓1" right-visible)))
            ;; 3. `herdr-sanctum-default-open-outside' switches away from Sanctum tab when tab-routing is off.
            (tab-bar-mode 1)
            (tab-bar-rename-tab "Coding")
            (tab-bar-new-tab)
            (tab-bar-rename-tab herdr-sanctum-tab-name)
            (let ((herdr-sanctum-display-mode 'tab)
                  (sanctum-buf (get-buffer-create herdr-sanctum-buffer-name))
                  (ran-tab nil))
              (switch-to-buffer sanctum-buf)
              (herdr-sanctum-open-outside
               (lambda ()
                 (setq ran-tab (alist-get 'name (tab-bar--current-tab)))))
              (should (equal ran-tab "Coding"))
              ;; When `herdr-sanctum-tab-routing-mode' is enabled with active rules,
              ;; `default-open-outside' does not pre-switch even if fallback is nil.
              (tab-bar-select-tab-by-name herdr-sanctum-tab-name)
              (switch-to-buffer sanctum-buf)
              (let ((herdr-sanctum-tab-routing-mode t)
                    (herdr-sanctum-tab-routing-rules '(("Folio" :file t)))
                    (herdr-sanctum-tab-routing-fallback-tab nil))
                (herdr-sanctum-open-outside
                 (lambda ()
                   (setq ran-tab (alist-get 'name (tab-bar--current-tab))))))
              (should (equal ran-tab herdr-sanctum-tab-name))
              ;; With empty rules and an active fallback tab, it also defers to tab-routing.
              (tab-bar-select-tab-by-name herdr-sanctum-tab-name)
              (switch-to-buffer sanctum-buf)
              (let ((herdr-sanctum-tab-routing-mode t)
                    (herdr-sanctum-tab-routing-rules nil)
                    (herdr-sanctum-tab-routing-fallback-tab 'recent))
                (herdr-sanctum-open-outside
                 (lambda ()
                   (setq ran-tab (alist-get 'name (tab-bar--current-tab))))))
              (should (equal ran-tab herdr-sanctum-tab-name))
              ;; With empty rules AND nil fallback, `default-open-outside' still protects Sanctum tab.
              (tab-bar-select-tab-by-name herdr-sanctum-tab-name)
              (switch-to-buffer sanctum-buf)
              (let ((herdr-sanctum-tab-routing-mode t)
                    (herdr-sanctum-tab-routing-rules nil)
                    (herdr-sanctum-tab-routing-fallback-tab nil))
                (herdr-sanctum-open-outside
                 (lambda ()
                   (setq ran-tab (alist-get 'name (tab-bar--current-tab))))))
              (should (equal ran-tab "Coding"))))
        (when (bound-and-true-p tab-bar-mode)
          (while (> (length (tab-bar-tabs)) 1) (tab-bar-close-tab)))
        (tab-bar-mode (if orig-tab-bar-mode 1 -1))))))

(ert-deftest herdr-sanctum-ui-test/ergonomics-focus-overlays-disconnected-and-files-polish ()
  "Inline button & active row focus overlays, disconnected onboarding, and Files ignore/dirty markers work."
  (herdr-sanctum-test-with-clean-state
    ;; 1. Disconnected drawer Overview shows onboarding card with Retry and Doctor buttons.
    (herdr-sanctum-drawer-show t)
    (with-current-buffer (get-buffer herdr-sanctum-drawer-buffer-name)
      (let ((body (buffer-string)))
        (should (string-search "Not connected to Herdr" body))
        (should (string-search "Retry" body))
        (should (string-search "Doctor" body)))
      ;; Stepping onto inline `Doctor' button with `l' places high-priority focus overlay on `Doctor'.
      (goto-char (point-min))
      (herdr-sanctum-drawer-next-item)
      (herdr-sanctum-drawer--update-focus-overlay)
      (should (eq (plist-get (herdr-sanctum-drawer--item-at) :id) 'btn-retry))
      (should (overlayp herdr-sanctum-drawer--focus-overlay))
      (should (equal (buffer-substring-no-properties
                      (overlay-start herdr-sanctum-drawer--focus-overlay)
                      (overlay-end herdr-sanctum-drawer--focus-overlay))
                     " Retry "))
      (should (eq (keymap-lookup herdr-sanctum-drawer-mode-map "l")
                  #'herdr-sanctum-drawer-next-item))
      (should (eq (keymap-lookup herdr-sanctum-drawer-mode-map "h")
                  #'herdr-sanctum-drawer-prev-item))
      (should (eq (keymap-lookup herdr-sanctum-drawer-mode-map "?")
                  #'herdr-sanctum-drawer-help)))
    ;; 2. Sidebar active row focus overlay activates when focused and clears on blur; `Q'/`?' bound.
    (herdr-sanctum-apply-snapshot (herdr-sanctum-ui-test--rich-snapshot))
    (let ((sb-win (herdr-sanctum-sidebar-show t)))
      (with-current-buffer (get-buffer herdr-sanctum-sidebar-buffer-name)
        (should (eq (keymap-lookup herdr-sanctum-sidebar-mode-map "Q")
                    #'herdr-sanctum-sidebar-toggle))
        (should (eq (keymap-lookup herdr-sanctum-sidebar-mode-map "?")
                    #'herdr-sanctum-sidebar-help))
        (should (string-search "▾" (buffer-string)))
        (should (string-search "▸" (buffer-string)))
        (goto-char (herdr-sanctum-sidebar--find-item-pos 'tab "w1:t2"))
        (set-window-point sb-win (point))
        (herdr-sanctum-sidebar--update-focus-overlay)
        (should (overlayp herdr-sanctum-sidebar--focus-overlay))
        (should (overlay-buffer herdr-sanctum-sidebar--focus-overlay))))
    ;; 3. Files view hides `.git'/`.DS_Store' and marks Git-dirty entries with `•'.
    (herdr-sanctum-test-with-temp-dir dir
      (make-directory (expand-file-name ".git" dir) t)
      (with-temp-file (expand-file-name ".DS_Store" dir) (insert "x"))
      (with-temp-file (expand-file-name "dirty.el" dir) (insert ";; d\n"))
      (with-temp-file (expand-file-name "clean.el" dir) (insert ";; c\n"))
      (herdr-sanctum-test--put-git-cache
       dir (list :root dir :branch "main" :dirty 1 :files '((".M" . "dirty.el"))))
      (herdr-sanctum-apply-snapshot (herdr-sanctum-test-make-snapshot :dir dir))
      (herdr-sanctum-drawer--dispatch 'switch-tab 'files)
      (with-current-buffer (get-buffer herdr-sanctum-drawer-buffer-name)
        (let ((body (buffer-string)))
          (should-not (string-search ".git" body))
          (should-not (string-search ".DS_Store" body))
          (should (string-search "dirty.el •" body))
          (should (string-search "clean.el" body))
          (should-not (string-search "clean.el •" body)))))))

;;; Wave 3 & 4 Integration Regression Tests

(ert-deftest herdr-sanctum-ui-test/ui-wave3-line-spacing-remap-evil-invoke-and-frame-outside ()
  "Panel line-spacing, non-destructive remaps, Evil config, `invoke-command-or-fn', and frame open-outside work."
  (herdr-sanctum-test-with-clean-state
    (let ((orig-ls (default-value 'herdr-sanctum-panel-line-spacing))
          (orig-evil (default-value 'herdr-sanctum-evil-initial-state))
          (orig-overrides (default-value 'herdr-sanctum-palette-overrides))
          (herdr-sanctum--evil-registered-modes
           (copy-sequence herdr-sanctum--evil-registered-modes)))
      (unwind-protect
          (progn
            ;; 1. `herdr-sanctum-panel-line-spacing' initializes panels and updates live via `:set'.
            (herdr-sanctum-sidebar-show nil)
            (cl-letf (((symbol-function 'herdr-sanctum-git-fetch-dir) #'ignore))
              (herdr-sanctum-drawer-show nil))
            (let ((sb-buf (get-buffer herdr-sanctum-sidebar-buffer-name))
                  (dr-buf (get-buffer herdr-sanctum-drawer-buffer-name)))
              (should (eql (buffer-local-value 'line-spacing sb-buf) orig-ls))
              (should (eql (buffer-local-value 'line-spacing dr-buf) orig-ls))
              (setopt herdr-sanctum-panel-line-spacing 0.35)
              (should (eql (buffer-local-value 'line-spacing sb-buf) 0.35))
              (should (eql (buffer-local-value 'line-spacing dr-buf) 0.35))
              ;; 2. Non-destructive `face-remapping-alist' preserves user `face-remap-add-relative'
              ;;    and applies `'bg-panel' across repeated theme updates without leaking duplicates.
              (with-current-buffer sb-buf
                (let ((scale-cookie (face-remap-add-relative 'default :height 1.25))
                      (custom-cookie (face-remap-add-relative 'bold :underline t)))
                  (ignore scale-cookie custom-cookie)
                  (let ((herdr-sanctum-palette-overrides
                         '((bg-chrome . "#111111") (bg-panel . "#223344"))))
                    (clrhash herdr-sanctum--palette-cache)
                    (herdr-sanctum-apply-theme t)
                    (herdr-sanctum-apply-panel-remapping sb-buf)
                    (let ((def-specs (cdr (assq 'default face-remapping-alist)))
                          (fr-specs (cdr (assq 'fringe face-remapping-alist)))
                          (bold-specs (cdr (assq 'bold face-remapping-alist))))
                      (should (member '(:height 1.25) def-specs))
                      (should (member '(:underline t) bold-specs))
                      (should (member '(:background "#223344") fr-specs))
                      (should (seq-some (lambda (s)
                                          (and (listp s)
                                               (equal (plist-get s :background) "#223344")))
                                        def-specs))
                      ;; Old Sanctum cookies were replaced, not duplicated.
                      (should (= (cl-count-if (lambda (s)
                                                (and (listp s)
                                                     (equal (plist-get s :background) "#223344")))
                                              fr-specs)
                                 1)))))))
            ;; 3. `herdr-sanctum-configure-evil-mode' and `herdr-sanctum-evil-initial-state' `:set'.
            (should (memq 'herdr-sanctum-sidebar-mode herdr-sanctum--evil-registered-modes))
            (should (memq 'herdr-sanctum-drawer-mode herdr-sanctum--evil-registered-modes))
            (let ((evil-calls nil))
              (cl-letf (((symbol-function 'evil-set-initial-state)
                         (lambda (mode state) (push (cons mode state) evil-calls))))
                (herdr-sanctum-configure-evil-mode 'sanctum-test-panel-mode)
                (should (equal (car evil-calls) '(sanctum-test-panel-mode . emacs)))
                (setq evil-calls nil)
                (setopt herdr-sanctum-evil-initial-state 'motion)
                (should (member '(herdr-sanctum-sidebar-mode . motion) evil-calls))
                (should (member '(herdr-sanctum-drawer-mode . motion) evil-calls))
                (should (member '(sanctum-test-panel-mode . motion) evil-calls))
                (setq evil-calls nil)
                (setopt herdr-sanctum-evil-initial-state nil)
                (should-not evil-calls)))
            ;; 4. `herdr-sanctum-invoke-command-or-fn' uses `call-interactively' iff no args & positive arity.
            (let ((cmd-req (lambda (x) (interactive (list 'prompted)) (list 'req x)))
                  (cmd-zero (lambda () (interactive) 'zero))
                  (plain-fn (lambda (a b) (+ a b))))
              (should (equal (herdr-sanctum-invoke-command-or-fn cmd-req) '(req prompted)))
              (should (equal (herdr-sanctum-invoke-command-or-fn cmd-req 'direct) '(req direct)))
              (should (eq (herdr-sanctum-invoke-command-or-fn cmd-zero) 'zero))
              (should (= (herdr-sanctum-invoke-command-or-fn plain-fn 2 3) 5)))
            ;; 5. `herdr-sanctum-default-open-outside' in `frame' mode focuses MRU non-Sanctum frame or creates one.
            (herdr-sanctum-test-with-temp-dir target-dir
              (let* ((herdr-sanctum-display-mode 'frame)
                     (herdr-sanctum-frame-name "Sanctum")
                     (focused-frame nil)
                     (created-frame nil)
                     (ran-dir nil)
                     (orig-win-live (symbol-function 'window-live-p))
                     (frames '(f-sanctum f-hidden f-child f-mini f-old f-recent)))
                (cl-letf (((symbol-function 'selected-frame) (lambda () 'f-sanctum))
                          ((symbol-function 'frame-list) (lambda () frames))
                          ((symbol-function 'frame-visible-p) (lambda (f) (not (eq f 'f-hidden))))
                          ((symbol-function 'frame-parameter)
                           (lambda (f param)
                             (pcase (cons f param)
                               ('(f-sanctum . name) "Sanctum")
                               ('(f-child . parent-frame) 'f-old)
                               ('(f-mini . minibuffer) 'only)
                               (_ nil))))
                          ((symbol-function 'frame-selected-window)
                           (lambda (f) (pcase f ('f-old 'w-old) ('f-recent 'w-recent) (_ nil))))
                          ((symbol-function 'window-live-p)
                           (lambda (w) (or (memq w '(w-old w-recent)) (funcall orig-win-live w))))
                          ((symbol-function 'window-use-time)
                           (lambda (w) (pcase w ('w-old 10) ('w-recent 42) (_ 0))))
                          ((symbol-function 'select-frame-set-input-focus)
                           (lambda (f &optional _) (setq focused-frame f)))
                          ((symbol-function 'make-frame)
                           (lambda (&optional _) (setq created-frame 'f-new) 'f-new)))
                  (herdr-sanctum-default-open-outside
                   (lambda () (setq ran-dir default-directory))
                   target-dir)
                  (should (eq focused-frame 'f-recent))
                  (should-not created-frame)
                  (should (equal ran-dir target-dir))
                  ;; When no visible non-Sanctum frame exists, creates and focuses a new frame.
                  (setq frames '(f-sanctum f-hidden f-child f-mini)
                        focused-frame nil
                        created-frame nil)
                  (herdr-sanctum-default-open-outside #'ignore target-dir)
                  (should (eq created-frame 'f-new))
                  (should (eq focused-frame 'f-new))))))
        (set-default 'herdr-sanctum-panel-line-spacing orig-ls)
        (set-default 'herdr-sanctum-evil-initial-state orig-evil)
        (set-default 'herdr-sanctum-palette-overrides orig-overrides)
        (clrhash herdr-sanctum--palette-cache)))))

(ert-deftest herdr-sanctum-ui-test/sidebar-wave3a-collapse-agents-meta-footer-chevron-and-quit ()
  "Sidebar Wave 3A: collapse, agent filters, custom/wide meta, footer/config, chevron click, agent close, quit, focus-item."
  (herdr-sanctum-test-with-clean-state
    (let ((orig-idle (default-value 'herdr-sanctum-sidebar-show-idle-agents))
          (orig-statuses (default-value 'herdr-sanctum-sidebar-agent-statuses))
          (orig-meta-fn (default-value 'herdr-sanctum-sidebar-space-meta-function))
          (orig-footer (default-value 'herdr-sanctum-sidebar-footer-action))
          (orig-cfg (default-value 'herdr-sanctum-config-file))
          (orig-sb-hook herdr-sanctum-sidebar-change-hook)
          (orig-dr-hook herdr-sanctum-drawer-change-hook)
          (snap (herdr-sanctum-ui-test--rich-snapshot)))
      (unwind-protect
          (progn
            ;; 1. `sidebar-collapse' & narrow-frame mutual collapse with drawer.
            (let ((sb-hooks 0)
                  (dr-hooks 0)
                  (herdr-sanctum-min-terminal-width 40)
                  (herdr-sanctum-sidebar-width 30)
                  (herdr-sanctum-drawer-width 34)
                  (herdr-sanctum-sidebar-side 'left)
                  (herdr-sanctum-drawer-side 'right))
              (add-hook 'herdr-sanctum-sidebar-change-hook (lambda () (cl-incf sb-hooks)))
              (add-hook 'herdr-sanctum-drawer-change-hook (lambda () (cl-incf dr-hooks)))
              (cl-letf (((symbol-function 'herdr-sanctum-git-fetch-dir) #'ignore)
                        ((symbol-function 'herdr-sanctum-ensure-connected) #'ignore)
                        ((symbol-function 'frame-width) (lambda (&optional _) 80)))
                (herdr-sanctum-sidebar-show nil)
                (should (herdr-sanctum-sidebar-visible-p))
                (setq sb-hooks 0)
                (herdr-sanctum-sidebar-collapse)
                (should-not (herdr-sanctum-sidebar-visible-p))
                (should (eq herdr-sanctum-sidebar--enabled 'off))
                (should (= sb-hooks 1))
                ;; Open drawer, then toggle sidebar on narrow frame -> collapses drawer.
                (herdr-sanctum-drawer-toggle)
                (should (herdr-sanctum-drawer-visible-p))
                (setq dr-hooks 0)
                (herdr-sanctum-sidebar-toggle)
                (should (herdr-sanctum-sidebar-visible-p))
                (should-not (herdr-sanctum-drawer-visible-p))
                (should (eq herdr-sanctum-drawer--enabled 'off))
                (should (> dr-hooks 0))
                ;; Toggle drawer back open -> collapses sidebar via `herdr-sanctum-sidebar-collapse'.
                (setq sb-hooks 0)
                (herdr-sanctum-drawer-toggle)
                (should (herdr-sanctum-drawer-visible-p))
                (should-not (herdr-sanctum-sidebar-visible-p))
                (should (eq herdr-sanctum-sidebar--enabled 'off))
                (should (> sb-hooks 0))))
            ;; 2. `sidebar-show-idle-agents' and `sidebar-agent-statuses'.
            (herdr-sanctum-sidebar-show t)
            (herdr-sanctum-apply-snapshot snap)
            (with-current-buffer (get-buffer herdr-sanctum-sidebar-buffer-name)
              (should (string-search "Idle 1" (buffer-string)))
              (setopt herdr-sanctum-sidebar-show-idle-agents nil)
              (should-not (string-search "Idle" (buffer-string)))
              (should (string-search "Blocked 1" (buffer-string)))
              (setopt herdr-sanctum-sidebar-agent-statuses '("done" "blocked"))
              (let* ((body (buffer-string))
                     (p-done (string-search "Done 1" body))
                     (p-blk (string-search "Blocked 1" body)))
                (should (and p-done p-blk (< p-done p-blk)))
                (should-not (string-search "Working" body))
                (should-not (string-search "Idle" body))))
            ;; 3. Wide-sidebar git meta & custom `sidebar-space-meta-function'.
            (let ((git (list :branch "feat/auth" :ahead 2 :behind 0 :dirty 1)))
              (should (equal (substring-no-properties
                              (herdr-sanctum-sidebar--space-git-meta git 1 30))
                             "↑2 •"))
              (let ((wide (substring-no-properties
                           (herdr-sanctum-sidebar--space-git-meta git 1 46))))
                (should (string-search "feat/auth" wide))
                (should (string-search "↑2 •" wide))))
            (let ((meta-calls nil))
              (setopt herdr-sanctum-sidebar-space-meta-function
                      (lambda (ws _git max-px)
                        (push (list (alist-get 'workspace_id ws) max-px) meta-calls)
                        (when (equal (alist-get 'workspace_id ws) "w1")
                          "CUSTOM-META")))
              (with-current-buffer (get-buffer herdr-sanctum-sidebar-buffer-name)
                (should (string-search "CUSTOM-META" (buffer-string)))
                (should (cl-every (lambda (c) (integerp (cadr c))) meta-calls))))
            (setopt herdr-sanctum-sidebar-space-meta-function nil)
            ;; 4. `sidebar-footer-action', `config-file nil' safety, and `,' keybinding.
            (should (eq (keymap-lookup herdr-sanctum-sidebar-mode-map ",")
                        #'herdr-sanctum-sidebar-open-config))
            (with-current-buffer (get-buffer herdr-sanctum-sidebar-buffer-name)
              (setopt herdr-sanctum-sidebar-footer-action 'none)
              (should-not (string-search "Config" (nth 2 mode-line-format)))
              (let ((custom-hit nil))
                (setopt herdr-sanctum-sidebar-footer-action
                        (cons "Docs" (lambda () (setq custom-hit t))))
                (let* ((sr (nth 2 mode-line-format))
                       (pos (string-search "Docs" sr))
                       (map (and pos (get-text-property pos 'local-map sr))))
                  (should pos)
                  (call-interactively (lookup-key map [mode-line mouse-1]))
                  (should custom-hit)))
              (setopt herdr-sanctum-sidebar-footer-action nil)
              (setopt herdr-sanctum-config-file nil)
              (should-not (string-search "Config" (nth 2 mode-line-format)))
              (should-error (herdr-sanctum-sidebar-open-config) :type 'user-error)
              (herdr-sanctum-test-with-temp-dir cfg-dir
                (let* ((cfg-file (expand-file-name "config.toml" cfg-dir))
                       (opened nil)
                       (herdr-sanctum-open-file-function (lambda (p) (setq opened p)))
                       (herdr-sanctum-open-outside-function nil))
                  (with-temp-file cfg-file (insert "[ui]\n"))
                  (setopt herdr-sanctum-config-file cfg-file)
                  (should (string-search "Config" (nth 2 mode-line-format)))
                  (herdr-sanctum-sidebar-open-config)
                  (should (equal opened cfg-file)))))
            ;; 5. Chevron click folds/unfolds workspace without focusing `*sanctum*'.
            (with-current-buffer (get-buffer herdr-sanctum-sidebar-buffer-name)
              (let* ((sb-win (herdr-sanctum-panel-window))
                     (focus-calls 0)
                     (ws-pos (herdr-sanctum-sidebar--find-item-pos 'workspace "w1"))
                     (chev-pos (text-property-not-all ws-pos (save-excursion
                                                               (goto-char ws-pos)
                                                               (line-end-position))
                                                      'herdr-sanctum-chevron nil)))
                (should chev-pos)
                (should (equal (get-text-property chev-pos 'herdr-sanctum-chevron) "w1"))
                (should (eq (lookup-key (get-text-property chev-pos 'keymap) [mouse-1])
                            #'herdr-sanctum-sidebar-mouse-toggle-fold))
                (cl-letf (((symbol-function 'herdr-sanctum-sidebar-focus-item)
                           (lambda (&rest _) (cl-incf focus-calls))))
                  (should (herdr-sanctum-sidebar--find-item-pos 'tab "w1:t2"))
                  (herdr-sanctum-sidebar-mouse-toggle-fold
                   (list 'mouse-1 (list sb-win chev-pos '(0 . 0) 0)))
                  (should-not (herdr-sanctum-sidebar--find-item-pos 'tab "w1:t2"))
                  (should (= focus-calls 0))
                  ;; Also works through `herdr-sanctum-sidebar-mouse-click'.
                  (let* ((ws-pos2 (herdr-sanctum-sidebar--find-item-pos 'workspace "w1"))
                         (chev-pos2 (text-property-not-all ws-pos2 (save-excursion
                                                                     (goto-char ws-pos2)
                                                                     (line-end-position))
                                                           'herdr-sanctum-chevron nil)))
                    (should chev-pos2)
                    (herdr-sanctum-sidebar-mouse-click
                     (list 'mouse-1 (list sb-win chev-pos2 '(0 . 0) 0)))
                    (should (herdr-sanctum-sidebar--find-item-pos 'tab "w1:t2"))
                    (should (= focus-calls 0))))))
            ;; 6. `'agent' row `d'/`x' sends `pane.close' RPC.
            (should (eq (keymap-lookup herdr-sanctum-sidebar-mode-map "d")
                        #'herdr-sanctum-sidebar-close))
            (should (eq (keymap-lookup herdr-sanctum-sidebar-mode-map "x")
                        #'herdr-sanctum-sidebar-close))
            (let ((recorded nil))
              (herdr-sanctum-test-with-mock-server (_sock)
                  (lambda (client data)
                    (dolist (req (herdr-sanctum-test--parse-json-lines data))
                      (push req recorded)
                      (herdr-sanctum-test--send-json
                       client `((id . ,(alist-get 'id req)) (result . ((ok . t)))))))
                (with-current-buffer (get-buffer herdr-sanctum-sidebar-buffer-name)
                  (goto-char (herdr-sanctum-sidebar--find-item-pos 'agent "w1:p3"))
                  (cl-letf (((symbol-function 'y-or-n-p) (lambda (_p) t)))
                    (herdr-sanctum-sidebar-close))
                  (should (herdr-sanctum-test--wait-until
                           (lambda ()
                             (seq-some (lambda (r)
                                         (and (equal (alist-get 'method r) "pane.close")
                                              (equal (alist-get 'pane_id (alist-get 'params r))
                                                     "w1:p3")))
                                       recorded)))))))
            ;; 7. `sidebar-focus-item' switches to Sanctum when `*sanctum*' is absent.
            (let ((rpc-methods nil)
                  (sanctum-args nil))
              (cl-letf (((symbol-function 'herdr-sanctum-rpc-call)
                         (lambda (m &rest _) (push m rpc-methods)))
                        ((symbol-function 'herdr-sanctum)
                         (lambda (&optional arg) (setq sanctum-args arg))))
                (should-not (get-buffer-window herdr-sanctum-buffer-name (selected-frame)))
                (herdr-sanctum-sidebar-focus-item '(:kind workspace :workspace-id "w1") t)
                (should (equal rpc-methods '("workspace.focus")))
                (should (equal sanctum-args '(4)))))
            ;; 8. `sidebar-quit' falls back to MRU non-side window when `*sanctum*' is absent,
            ;;    and toggles off when sidebar is the sole window.
            (when-let* ((dr-w (get-buffer-window herdr-sanctum-drawer-buffer-name)))
              (delete-window dr-w))
            (when-let* ((sb-w (get-buffer-window herdr-sanctum-sidebar-buffer-name)))
              (delete-window sb-w))
            (delete-other-windows)
            (let* ((w1 (selected-window))
                   (w2 (split-window-right)))
              (select-window w1)
              (select-window w2)
              (herdr-sanctum-sidebar-show t)
              (should (equal (buffer-name (window-buffer (selected-window)))
                             herdr-sanctum-sidebar-buffer-name))
              (herdr-sanctum-sidebar-quit)
              (should (eq (selected-window) w2)))
            (when-let* ((sb-w (get-buffer-window herdr-sanctum-sidebar-buffer-name)))
              (delete-window sb-w))
            (delete-other-windows)
            (switch-to-buffer (get-buffer-create herdr-sanctum-sidebar-buffer-name))
            (setq herdr-sanctum-sidebar--enabled 'on)
            (herdr-sanctum-sidebar-quit)
            (should (eq herdr-sanctum-sidebar--enabled 'off)))
        (setq herdr-sanctum-sidebar-change-hook orig-sb-hook
              herdr-sanctum-drawer-change-hook orig-dr-hook)
        (set-default 'herdr-sanctum-sidebar-show-idle-agents orig-idle)
        (set-default 'herdr-sanctum-sidebar-agent-statuses orig-statuses)
        (set-default 'herdr-sanctum-sidebar-space-meta-function orig-meta-fn)
        (set-default 'herdr-sanctum-sidebar-footer-action orig-footer)
        (set-default 'herdr-sanctum-config-file orig-cfg)))))

(ert-deftest herdr-sanctum-ui-test/drawer-wave3b-git-diff-files-and-limits ()
  "Drawer Wave 3B: real non-Magit diff, project fallback, per-file/deleted diff, dotfiles/ignore/D/dir-icon, and caps."
  (herdr-sanctum-test-with-clean-state
    (let ((orig-dotfiles (default-value 'herdr-sanctum-drawer-files-show-dotfiles))
          (orig-ignore (default-value 'herdr-sanctum-drawer-files-ignore))
          (orig-dir-icon (default-value 'herdr-sanctum-dir-icon-function))
          (orig-require (symbol-function 'require)))
      (unwind-protect
          (herdr-sanctum-test-with-temp-dir repo1
            (herdr-sanctum-test-with-temp-dir repo2
              (let* ((app-file (herdr-sanctum-test--init-git-repo repo1 "app.txt" "v1\n"))
                     (del-file (expand-file-name "gone.txt" repo1))
                     (other-file (herdr-sanctum-test--init-git-repo repo2 "other.txt" "orig\n"))
                     (other-buf nil))
                ;; Commit a second file to delete, and modify both repos.
                (with-temp-file del-file (insert "bye\n"))
                (should (zerop (call-process "git" nil nil nil "-C" repo1 "add" "gone.txt")))
                (should (zerop (call-process "git" nil nil nil
                                             "-c" "user.name=Test" "-c" "user.email=t@e"
                                             "-C" repo1 "commit" "-m" "add gone")))
                (with-temp-file app-file (insert "v2\n"))
                (delete-file del-file)
                (with-temp-file other-file (insert "unrelated-change\n"))
                (unwind-protect
                    (progn
                      ;; 1. Non-Magit `herdr-sanctum-default-diff' diffs explicit `repo1' fileset,
                      ;;    NOT the unrelated modified file visited in the current buffer.
                      (setq other-buf (find-file-noselect other-file))
                      (with-current-buffer other-buf
                        (cl-letf (((symbol-function 'require)
                                   (lambda (feat &optional file noerror)
                                     (unless (eq feat 'magit-diff)
                                       (funcall orig-require feat file noerror)))))
                          (let ((default-directory (file-name-as-directory repo1)))
                            (herdr-sanctum-default-diff))))
                      (let ((dbuf (get-buffer "*vc-diff*")))
                        (should dbuf)
                        (should (herdr-sanctum-test--wait-until
                                 (lambda ()
                                   (let ((p (get-buffer-process dbuf)))
                                     (not (and p (process-live-p p)))))
                                 3.0))
                        (with-current-buffer dbuf
                          (let ((txt (buffer-string)))
                            (should (string-search "app.txt" txt))
                            (should (string-search "+v2" txt))
                            (should-not (string-search "other.txt" txt))
                            (should-not (string-search "unrelated-change" txt))))
                        (kill-buffer dbuf))
                      ;; Real `herdr-sanctum-default-file-diff' handles deleted file cleanly.
                      (herdr-sanctum-default-file-diff del-file)
                      (let ((dbuf (get-buffer "*vc-diff*")))
                        (should dbuf)
                        (should (herdr-sanctum-test--wait-until
                                 (lambda ()
                                   (let ((p (get-buffer-process dbuf)))
                                     (not (and p (process-live-p p)))))
                                 3.0))
                        (with-current-buffer dbuf
                          (let ((txt (buffer-string)))
                            (should (string-search "gone.txt" txt))
                            (should (string-search "-bye" txt))))
                        (kill-buffer dbuf)))
                  (when (buffer-live-p other-buf) (kill-buffer other-buf))
                  (when-let* ((dbuf (get-buffer "*vc-diff*")))
                    (when-let* ((p (get-buffer-process dbuf)))
                      (when (process-live-p p) (delete-process p)))
                    (kill-buffer dbuf)))
                ;; 2. `herdr-sanctum-default-project-find-file' uses project inside git repo,
                ;;    falls back to interactive `find-file' outside any project.
                (require 'project)
                (herdr-sanctum-test-with-temp-dir non-proj
                  (let ((proj-hits 0)
                        (ff-hits 0))
                    (cl-letf (((symbol-function 'project-find-file)
                               (lambda (&optional _) (cl-incf proj-hits)))
                              ((symbol-function 'find-file)
                               (lambda (&rest _) (interactive) (cl-incf ff-hits))))
                      (let ((default-directory (file-name-as-directory repo1)))
                        (herdr-sanctum-default-project-find-file)
                        (should (= proj-hits 1))
                        (should (= ff-hits 0)))
                      (let ((default-directory (file-name-as-directory non-proj)))
                        (herdr-sanctum-default-project-find-file)
                        (should (= proj-hits 1))
                        (should (= ff-hits 1))))))
                ;; 3. `d' per-file vs whole-repo diff, deleted-file tooltip & `RET' diff routing.
                (herdr-sanctum-test--put-git-cache
                 repo1 (list :root (file-name-as-directory repo1) :branch "main" :dirty 2
                             :staged 0 :unstaged 2 :untracked 0
                             :files '((" M" . "app.txt") (" D" . "gone.txt"))))
                (herdr-sanctum-apply-snapshot (herdr-sanctum-test-make-snapshot :dir repo1))
                (herdr-sanctum-drawer-open-view 'changes)
                (with-current-buffer (get-buffer herdr-sanctum-drawer-buffer-name)
                  (let* ((file-diffs nil)
                        (repo-diffs nil)
                        (opened-files nil)
                        (herdr-sanctum-open-outside-function nil)
                        (herdr-sanctum-file-diff-function (lambda (f) (push f file-diffs)))
                        (herdr-sanctum-diff-function (lambda () (push default-directory repo-diffs)))
                        (herdr-sanctum-open-file-function (lambda (f) (push f opened-files))))
                    ;; Tooltip for existing file vs deleted file.
                    (goto-char (point-min))
                    (search-forward "app.txt")
                    (beginning-of-line)
                    (should (equal (get-text-property (point) 'help-echo) "Open app.txt"))
                    ;; `d' on changed file opens per-file diff.
                    (herdr-sanctum-drawer-open-diff)
                    (should (equal file-diffs (list app-file)))
                    (should-not repo-diffs)
                    ;; `RET' on existing file opens file.
                    (herdr-sanctum-drawer-activate)
                    (should (equal opened-files (list app-file)))
                    ;; Deleted file has "Diff deleted ..." tooltip and `RET' routes to per-file diff.
                    (goto-char (point-min))
                    (search-forward "gone.txt")
                    (beginning-of-line)
                    (should (equal (get-text-property (point) 'help-echo) "Diff deleted gone.txt"))
                    (setq file-diffs nil opened-files nil)
                    (herdr-sanctum-drawer-activate)
                    (should (equal file-diffs (list del-file)))
                    (should-not opened-files)
                    ;; `d' on non-file button opens whole-repo diff.
                    (goto-char (point-min))
                    (search-forward "Diff")
                    (backward-char 1)
                    (setq file-diffs nil repo-diffs nil)
                    (herdr-sanctum-drawer-open-diff)
                    (should-not file-diffs)
                    (should (equal repo-diffs (list (file-name-as-directory repo1))))))))
            ;; 4. `Files': subdir `Find file', `.' dotfile toggle, regexp/predicate ignore + `:set',
            ;;    `D' `open-directory-function', `dir-icon-function', and >150 anti-starvation cap.
            (herdr-sanctum-test-with-temp-dir fdir
              (let* ((norm-fdir (file-name-as-directory (expand-file-name fdir)))
                     (sub (file-name-as-directory (expand-file-name "sub" fdir))))
                (make-directory sub t)
                (with-temp-file (expand-file-name ".env" fdir) (insert "K=1\n"))
                (with-temp-file (expand-file-name "keep.el" fdir) (insert ";; ok\n"))
                (with-temp-file (expand-file-name "junk.bak" fdir) (insert "x\n"))
                (with-temp-file (expand-file-name "skip-me.txt" fdir) (insert "x\n"))
                (with-temp-file (expand-file-name "nested.el" sub) (insert ";; sub\n"))
                (herdr-sanctum-apply-snapshot (herdr-sanctum-test-make-snapshot :dir fdir))
                (herdr-sanctum-drawer-open-view 'files t)
                (with-current-buffer (get-buffer herdr-sanctum-drawer-buffer-name)
                  ;; Dotfile visible by default, toggled off and back on via `.'.
                  (should (eq (keymap-lookup herdr-sanctum-drawer-mode-map ".")
                              #'herdr-sanctum-drawer-toggle-dotfiles))
                  (should (string-search ".env" (buffer-string)))
                  (herdr-sanctum-drawer-toggle-dotfiles)
                  (should-not (string-search ".env" (buffer-string)))
                  (herdr-sanctum-drawer-toggle-dotfiles)
                  (should (string-search ".env" (buffer-string)))
                  ;; Regexp + predicate `files-ignore' via `:set' invalidates cache and refreshes live.
                  (setopt herdr-sanctum-drawer-files-ignore
                          (list ".git"
                                '(regexp . "\\.bak\\'")
                                (lambda (base) (string-prefix-p "skip-" base))))
                  (let ((body (buffer-string)))
                    (should (string-search "keep.el" body))
                    (should-not (string-search "junk.bak" body))
                    (should-not (string-search "skip-me.txt" body)))
                  ;; `dir-icon-function' (1-arg and 2-arg) styles directory rows.
                  (setq herdr-sanctum-dir-icon-function (lambda (name) (format "[D:%s] " name)))
                  (herdr-sanctum-drawer-refresh)
                  (should (string-search "[D:sub] sub" (buffer-string)))
                  (setq herdr-sanctum-dir-icon-function (lambda (name _fr) (format "<%s> " name)))
                  (herdr-sanctum-drawer-refresh)
                  (should (string-search "<sub" (buffer-string)))
                  ;; `D' on directory row opens that subdir; elsewhere opens current browsed dir.
                  (should (eq (keymap-lookup herdr-sanctum-drawer-mode-map "D")
                              #'herdr-sanctum-drawer-open-directory))
                  (let* ((opened-dirs nil)
                        (herdr-sanctum-open-outside-function nil)
                        (herdr-sanctum-open-directory-function (lambda (d) (push d opened-dirs))))
                    (goto-char (point-min))
                    (search-forward "sub")
                    (beginning-of-line)
                    (herdr-sanctum-drawer-open-directory)
                    (should (equal (car opened-dirs) sub))
                    (goto-char (point-min))
                    (herdr-sanctum-drawer-open-directory)
                    (should (equal (car opened-dirs) norm-fdir)))
                  ;; Browsing into `sub/' passes `sub/' to `Find file'.
                  (herdr-sanctum-drawer--browse-dir sub)
                  (let* ((ff-dir nil)
                        (herdr-sanctum-open-outside-function
                         (lambda (_fn d) (setq ff-dir d))))
                    (herdr-sanctum-drawer-find-file)
                    (should (equal ff-dir sub))))))
            ;; 5. >150 entries anti-starvation + `"… N more items"' footer, and
            ;;    >200 `"… N more changed files"' footer in single- and multi-repo Changes.
            (herdr-sanctum-test-with-temp-dir big-dir
              (dotimes (i 100)
                (make-directory (expand-file-name (format "d-%03d" i) big-dir) t)
                (with-temp-file (expand-file-name (format "f-%03d.txt" i) big-dir)
                  (insert "")))
              (herdr-sanctum-apply-snapshot (herdr-sanctum-test-make-snapshot :dir big-dir))
              (herdr-sanctum-drawer-open-view 'files t)
              (with-current-buffer (get-buffer herdr-sanctum-drawer-buffer-name)
                (let ((entries (herdr-sanctum-drawer--read-dir-entries
                                (file-name-as-directory big-dir))))
                  ;; Anti-starvation: 100 dirs + 100 files -> 75 dirs + 75 files (not 150 dirs + 0 files).
                  (should (= (length (car entries)) 75))
                  (should (= (length (cdr entries)) 75)))
                (should (string-search "… 50 more items" (buffer-string))))
              ;; Changes >200 footer (single-repo and multi-repo).
              (let ((files200 (cl-loop for i from 1 to 200
                                       collect (cons ".M" (format "f-%03d.el" i)))))
                (herdr-sanctum-test--put-git-cache
                 big-dir (list :root (file-name-as-directory big-dir)
                               :branch "main" :dirty 245 :files files200))
                (herdr-sanctum-drawer-open-view 'changes t)
                (with-current-buffer (get-buffer herdr-sanctum-drawer-buffer-name)
                  (should (string-search "… 45 more changed files" (buffer-string))))
                (let ((sub-a (file-name-as-directory (expand-file-name "d-000" big-dir))))
                  (herdr-sanctum-test--put-git-cache
                   big-dir
                   (list :multi-repo t :root (file-name-as-directory big-dir)
                         :branch "1 repo" :dirty 201 :files files200
                         :repos (list (list :name "d-000" :dir sub-a :root sub-a
                                            :branch "main" :dirty 201 :files files200))))
                  (herdr-sanctum-drawer-refresh)
                  (with-current-buffer (get-buffer herdr-sanctum-drawer-buffer-name)
                    (should (string-search "… 1 more changed file" (buffer-string))))))))
        (set-default 'herdr-sanctum-drawer-files-show-dotfiles orig-dotfiles)
        (set-default 'herdr-sanctum-drawer-files-ignore orig-ignore)
        (set-default 'herdr-sanctum-dir-icon-function orig-dir-icon)))))

(ert-deftest herdr-sanctum-ui-test/drawer-wave3b-views-registry-collapse-and-quit ()
  "Drawer Wave 3B: default-view, overview-max-files/actions, unregister, extended view spec, collapse, and quit."
  (herdr-sanctum-test-with-clean-state
    (let ((orig-def-view (default-value 'herdr-sanctum-drawer-default-view))
          (orig-max-files (default-value 'herdr-sanctum-drawer-overview-max-files))
          (orig-actions (default-value 'herdr-sanctum-drawer-overview-actions))
          (orig-enabled (default-value 'herdr-sanctum-drawer-enabled-views))
          (orig-views (copy-alist herdr-sanctum-drawer-views))
          (orig-dr-hook herdr-sanctum-drawer-change-hook))
      (unwind-protect
          (herdr-sanctum-test-with-temp-dir repo-dir
            (herdr-sanctum-test--put-git-cache
             repo-dir
             (list :branch "main" :dirty 6 :staged 1 :unstaged 5 :untracked 0
                   :files '(("M." . "a.el") (".M" . "b.el") (".M" . "c.el")
                            (".M" . "d.el") (".M" . "e.el") (".M" . "f.el"))))
            (herdr-sanctum-apply-snapshot (herdr-sanctum-ui-test--drawer-snapshot repo-dir))
            ;; 1. `drawer-default-view' controls initial active view when `--tab' is nil.
            (should-not herdr-sanctum-drawer--tab)
            (should (eq (herdr-sanctum-drawer-current-tab) 'overview))
            (setopt herdr-sanctum-drawer-default-view 'files)
            (should (eq (herdr-sanctum-drawer-current-tab) 'files))
            (setopt herdr-sanctum-drawer-default-view 'nonexistent-view)
            (should (eq (herdr-sanctum-drawer-current-tab) 'overview))
            (setopt herdr-sanctum-drawer-default-view nil)
            ;; 2. `overview-max-files' and `overview-actions'.
            (herdr-sanctum-drawer-show t)
            (with-current-buffer (get-buffer herdr-sanctum-drawer-buffer-name)
              (setopt herdr-sanctum-drawer-overview-max-files 2)
              (let ((body (buffer-string)))
                (should (string-search "a.el" body))
                (should (string-search "b.el" body))
                (should-not (string-search "c.el" body))
                (should (string-search "View all 6 changes →" body)))
              (setopt herdr-sanctum-drawer-overview-max-files 0)
              (let ((body (buffer-string)))
                (should-not (string-search "CHANGED FILES" body))
                (should (string-search "ACTIONS" body)))
              (let ((ran nil)
                    (herdr-sanctum-open-outside-function nil))
                (setopt herdr-sanctum-drawer-overview-actions
                        (list (cons "Run Tests" (lambda () (setq ran 'tests)))
                              (cons "Logs" (lambda () (setq ran 'logs)))))
                (let ((body (buffer-string)))
                  (should (string-search "Run Tests" body))
                  (should (string-search "Logs" body))
                  (should-not (string-search "Find file" body)))
                (goto-char (point-min))
                (search-forward "Run Tests")
                (backward-char 1)
                (herdr-sanctum-drawer-activate)
                (should (eq ran 'tests))))
            ;; 3. Extended view spec: `:keymap', `:refresh-fn', `:render-key-fn',
            ;;    `:tick-refresh', 3-arg `:action-fn', and `unregister-drawer-view'.
            (let* ((custom-key-hit nil)
                   (refresh-ctx nil)
                   (action-args nil)
                   (render-key-val 'key-1)
                   (render-count 0)
                   (dr-hooks 0)
                   (vmap (let ((m (make-sparse-keymap)))
                           (define-key m "z" (lambda () (interactive) (setq custom-key-hit t)))
                           m)))
              (add-hook 'herdr-sanctum-drawer-change-hook (lambda () (cl-incf dr-hooks)))
              (herdr-sanctum-register-drawer-view
               'tasks
               :label "Tasks"
               :keymap vmap
               :tick-refresh t
               :render-key-fn (lambda (_ctx) render-key-val)
               :refresh-fn (lambda (ctx) (setq refresh-ctx ctx))
               :render-fn (lambda (_ctx)
                            (cl-incf render-count)
                            (dotimes (i 12)
                              (let ((st (point)))
                                (insert (format "Task %02d\n" i))
                                (add-text-properties
                                 st (1- (point))
                                 (list 'herdr-sanctum-drawer-item
                                       (herdr-sanctum-drawer-make-item 'run-task i i t))))))
               :action-fn (lambda (act val ctx) (setq action-args (list act val ctx))))
              (setopt herdr-sanctum-drawer-enabled-views '(overview changes files tasks))
              (herdr-sanctum-drawer-open-view 'tasks t)
              (with-current-buffer (get-buffer herdr-sanctum-drawer-buffer-name)
                ;; `:keymap' composes over `herdr-sanctum-drawer-mode-map'.
                (should (key-binding "z"))
                (call-interactively (key-binding "z"))
                (should custom-key-hit)
                (should (eq (key-binding "g") #'herdr-sanctum-drawer-refresh-action))
                ;; 3-arg `:action-fn' receives `(ACTION VALUE CTX)'.
                (goto-char (point-min))
                (search-forward "Task 05")
                (beginning-of-line)
                (herdr-sanctum-drawer-activate)
                (should (eq (nth 0 action-args) 'run-task))
                (should (eql (nth 1 action-args) 5))
                (should (equal (plist-get (nth 2 action-args) :dir)
                               (file-name-as-directory (expand-file-name repo-dir))))
                ;; `:render-key-fn' preserves cursor on same key and resets to point-min on key change.
                (let ((dr-win (herdr-sanctum-panel-window)))
                  (set-window-point dr-win (point))
                  (herdr-sanctum-drawer-refresh)
                  (should (eql (plist-get (herdr-sanctum-drawer-item-at (window-point dr-win)) :value) 5))
                  (setq render-key-val 'key-2)
                  (herdr-sanctum-drawer-refresh)
                  (should (= (window-point dr-win) (point-min))))
                ;; `:tick-refresh t' refreshes on duration tick.
                (let ((before render-count))
                  (herdr-sanctum-drawer--on-duration-tick)
                  (should (= render-count (1+ before))))
                ;; `:refresh-fn' runs on `g' (`herdr-sanctum-drawer-refresh-action').
                (let ((herdr-sanctum-socket-path
                       (expand-file-name "none.sock" herdr-sanctum-test--root)))
                  (herdr-sanctum-drawer-refresh-action))
                (should (plist-get refresh-ctx :snapshot))
                ;; Switching away restores base mode-map (no custom `"z"` binding).
                (herdr-sanctum-drawer--switch-tab 'overview)
                (should (eq (current-local-map) herdr-sanctum-drawer-mode-map))
                (should (eq (key-binding "z") 'undefined))
                ;; `unregister-drawer-view' returns nil for unknown, t for active view,
                ;; resets `--tab', fires change hook, and falls back cleanly.
                (herdr-sanctum-drawer--switch-tab 'tasks)
                (should-not (herdr-sanctum-unregister-drawer-view 'no-such-view))
                (setq dr-hooks 0)
                (should (herdr-sanctum-unregister-drawer-view 'tasks))
                (should-not (assq 'tasks herdr-sanctum-drawer-views))
                (should-not herdr-sanctum-drawer--tab)
                (should (eq (herdr-sanctum-drawer-current-tab) 'overview))
                (should (> dr-hooks 0))))
            ;; 4. `drawer-collapse' marks disabled, hides window, and runs change hook.
            (let ((dr-hooks 0))
              (setq herdr-sanctum-drawer-change-hook (list (lambda () (cl-incf dr-hooks))))
              (should (herdr-sanctum-drawer-visible-p))
              (herdr-sanctum-drawer-collapse)
              (should-not (herdr-sanctum-drawer-visible-p))
              (should (eq herdr-sanctum-drawer--enabled 'off))
              (should (= dr-hooks 1)))
            ;; 5. `drawer-quit' selects `*sanctum*', else non-side window, else toggles off.
            (delete-other-windows)
            (let ((main-win (selected-window)))
              (herdr-sanctum-drawer-show t)
              (should (equal (buffer-name (window-buffer (selected-window)))
                             herdr-sanctum-drawer-buffer-name))
              ;; `*sanctum*' absent -> focuses non-side window.
              (herdr-sanctum-drawer-quit)
              (should (eq (selected-window) main-win))
              ;; Sole window -> toggles drawer off.
              (when-let* ((dw (get-buffer-window herdr-sanctum-drawer-buffer-name)))
                (delete-window dw))
              (delete-other-windows)
              (switch-to-buffer (get-buffer-create herdr-sanctum-drawer-buffer-name))
              (setq herdr-sanctum-drawer--enabled 'on)
              (herdr-sanctum-drawer-quit)
              (should (eq herdr-sanctum-drawer--enabled 'off))))
        (setq herdr-sanctum-drawer-views orig-views
              herdr-sanctum-drawer-change-hook orig-dr-hook)
        (set-default 'herdr-sanctum-drawer-default-view orig-def-view)
        (set-default 'herdr-sanctum-drawer-overview-max-files orig-max-files)
        (set-default 'herdr-sanctum-drawer-overview-actions orig-actions)
        (set-default 'herdr-sanctum-drawer-enabled-views orig-enabled)))))

(ert-deftest herdr-sanctum-ui-test/core-wave4a-hooks-header-badges-and-ime-pill ()
  "Core Wave 4A: start-hook survival, header-line `:set'/segments, custom badges/global-mode-string, and IME pill."
  (herdr-sanctum-test-with-clean-state
    (let* ((orig-hl-en (default-value 'herdr-sanctum-header-line-enable))
           (orig-hl-l (default-value 'herdr-sanctum-header-line-left-function))
           (orig-hl-r (default-value 'herdr-sanctum-header-line-right-function))
           (orig-badge-fn (default-value 'herdr-sanctum-status-badge-format-function))
           (orig-badge-gms (default-value 'herdr-sanctum-status-badge-global-mode-line))
           (orig-badge-vis (default-value 'herdr-sanctum-status-badge-show-when-sidebar-visible))
           (orig-gms (copy-sequence global-mode-string))
           (orig-start-hook (copy-sequence herdr-sanctum-terminal-start-hook))
           (resized 0)
           (hook-order nil)
           (herdr-sanctum-terminal-backends (copy-alist herdr-sanctum-terminal-backends))
           (herdr-sanctum-terminal-backend 'w4a-hook-backend)
           (herdr-sanctum-display-mode 'current)
           (herdr-sanctum-program "sh")
           (herdr-sanctum-sidebar-default-open nil)
           (herdr-sanctum-drawer-default-open nil)
           (herdr-sanctum-socket-path (expand-file-name "no.sock" herdr-sanctum-test--root))
           (dummy-activate (lambda (_name)
                             (setq-local deactivate-current-input-method-function #'ignore)))
           (input-method-alist (cons (list "w4a-ime" "UTF-8" dummy-activate "漢")
                                     input-method-alist))
           (default-input-method "w4a-ime")
           (herdr-sanctum-ime-input-method nil))
      (herdr-sanctum-register-terminal-backend
       'w4a-hook-backend
       :start (lambda (buf _prog _args)
                (with-current-buffer buf (setq-local herdr-sanctum-test--live t)))
       :live-p (lambda (buf) (buffer-local-value 'herdr-sanctum-test--live buf))
       :resize (lambda (_win) (cl-incf resized)))
      (unwind-protect
          (progn
            ;; 1. User `herdr-sanctum-terminal-start-hook' at depth 0 runs AFTER built-in chrome init
            ;;    and its custom `mode-line-format' / fringes survive startup (not clobbered).
            (let ((user-hook (lambda ()
                               (push (list 'user header-line-format) hook-order)
                               (setq-local mode-line-format '(" USER-IME-ML ")
                                           left-fringe-width 6))))
              (add-hook 'herdr-sanctum-terminal-start-hook user-hook)
              (unwind-protect
                  (progn
                    (herdr-sanctum-ime-mode 1)
                    (herdr-sanctum-apply-snapshot
                     '((focused_pane_id . "w1:p1")
                       (panes . (((pane_id . "w1:p1") (agent . "pi"))))))
                    (herdr-sanctum '(4))
                    (let ((buf (get-buffer herdr-sanctum-buffer-name)))
                      ;; Built-in chrome ran first so user hook saw `'(:eval (herdr-sanctum-header-line))'.
                      (should (equal (cadr (car hook-order))
                                     '(:eval (herdr-sanctum-header-line))))
                      (should (equal (buffer-local-value 'mode-line-format buf)
                                     '(" USER-IME-ML ")))
                      (should (eql (buffer-local-value 'left-fringe-width buf) 6))
                      ;; IME synced via start-hook too without clobbering user mode-line.
                      (should (equal (buffer-local-value 'current-input-method buf) "w4a-ime"))
                      ;; Visible mode-line hides duplicate IME pill in header-line.
                      (should-not (string-search "漢" (herdr-sanctum-header-line-right)))))
                (remove-hook 'herdr-sanctum-terminal-start-hook user-hook)))
            ;; 2. `herdr-sanctum-header-line-enable' `:set' toggles header-line live and resizes PTY.
            (let ((buf (get-buffer herdr-sanctum-buffer-name)))
              (setq resized 0)
              (setopt herdr-sanctum-header-line-enable nil)
              (should-not (buffer-local-value 'header-line-format buf))
              (should (= resized 1))
              (setopt herdr-sanctum-header-line-enable t)
              (should (equal (buffer-local-value 'header-line-format buf)
                             '(:eval (herdr-sanctum-header-line))))
              (should (= resized 2))
              ;; 3. Custom left/right header-line functions (including lower arity) refresh live.
              (setopt herdr-sanctum-header-line-left-function (lambda (_snap) " CUSTOM-L"))
              (setopt herdr-sanctum-header-line-right-function (lambda () "CUSTOM-R "))
              (let ((hl (buffer-local-value 'herdr-sanctum--header-line-string buf)))
                (should (string-search "CUSTOM-L" hl))
                (should (string-search "CUSTOM-R" hl)))
              (setopt herdr-sanctum-header-line-left-function orig-hl-l)
              (setopt herdr-sanctum-header-line-right-function orig-hl-r)
              ;; 4. IME header pill appears when mode-line is hidden and refreshes via `ime-change-hook'.
              (should (memq #'herdr-sanctum--on-ime-change herdr-sanctum-ime-change-hook))
              (with-current-buffer buf
                (setq-local mode-line-format nil)
                (herdr-sanctum--refresh-header-line (selected-window))
                (let* ((hl herdr-sanctum--header-line-string)
                       (pos (string-search "漢" hl)))
                  (should pos)
                  (should (eq (lookup-key (get-text-property pos 'local-map hl)
                                          [header-line mouse-1])
                              #'herdr-sanctum--toggle-terminal-ime)))
                ;; Toggling via pill command switches IME off and updates header-line to "EN" via hook.
                (herdr-sanctum--toggle-terminal-ime)
                (should-not current-input-method)
                (should (string-search " EN " herdr-sanctum--header-line-string))
                (should-not (string-search " 漢 " herdr-sanctum--header-line-string))
                ;; External OS IME pill click signals `user-error'.
                (let ((herdr-sanctum-ime-activate-function #'ignore)
                      (herdr-sanctum-ime-deactivate-function #'ignore))
                  (should-error (herdr-sanctum--toggle-terminal-ime) :type 'user-error))))
            ;; 5. Custom badge formatter, show-when-sidebar-visible, and `global-mode-string' toggle.
            (herdr-sanctum-apply-snapshot (herdr-sanctum-ui-test--rich-snapshot))
            (let ((seen-ctxs nil))
              (setopt herdr-sanctum-status-badge-format-function
                      (lambda (blocked done working ctx)
                        (push (list blocked done working ctx) seen-ctxs)
                        (format "B%d/D%d/W%d" blocked done working)))
              (should (member '(1 1 1 mode-line) seen-ctxs))
              (should (member '(1 1 1 header-line) seen-ctxs))
              (should (string-search "B1/D1/W1" (herdr-sanctum-mode-line)))
              ;; Auto-attaches click map when custom string has none.
              (should (keymapp (get-text-property 0 'local-map (herdr-sanctum-mode-line))))
              (should (string-search "B1/D1/W1" (herdr-sanctum-header-line-right)))
              ;; `show-when-sidebar-visible' keeps mode-line badge visible with sidebar open.
              (herdr-sanctum-sidebar-show nil)
              (should-not (herdr-sanctum-mode-line))
              (setopt herdr-sanctum-status-badge-show-when-sidebar-visible t)
              (should (string-search "B1/D1/W1" (herdr-sanctum-mode-line))))
            ;; `global-mode-string' toggle removes and re-adds entry cleanly.
            (should (member herdr-sanctum--global-mode-string-entry global-mode-string))
            (setopt herdr-sanctum-status-badge-global-mode-line nil)
            (should-not (member herdr-sanctum--global-mode-string-entry global-mode-string))
            (setopt herdr-sanctum-status-badge-global-mode-line t)
            (should (= (cl-count herdr-sanctum--global-mode-string-entry
                                 global-mode-string :test #'equal)
                       1)))
        (setq herdr-sanctum-terminal-start-hook orig-start-hook
              global-mode-string orig-gms)
        (set-default 'herdr-sanctum-header-line-enable orig-hl-en)
        (set-default 'herdr-sanctum-header-line-left-function orig-hl-l)
        (set-default 'herdr-sanctum-header-line-right-function orig-hl-r)
        (set-default 'herdr-sanctum-status-badge-format-function orig-badge-fn)
        (set-default 'herdr-sanctum-status-badge-global-mode-line orig-badge-gms)
        (set-default 'herdr-sanctum-status-badge-show-when-sidebar-visible orig-badge-vis)))))

(ert-deftest herdr-sanctum-ui-test/core-wave4a-layout-frame-dwim-jump-step-maps-and-doctor ()
  "Core Wave 4A: narrow fast-path/reset-layout, frame params/toggle-back, panel DWIM, jump/step, maps, and doctor."
  (herdr-sanctum-test-with-clean-state
    (let* ((start-calls 0)
           (resize-calls 0)
           (herdr-sanctum-terminal-backends (copy-alist herdr-sanctum-terminal-backends))
           (herdr-sanctum-terminal-backend 'w4a-core-backend)
           (herdr-sanctum-display-mode 'current)
           (herdr-sanctum-program "sh")
           (herdr-sanctum-min-terminal-width 40)
           (herdr-sanctum-sidebar-width 30)
           (herdr-sanctum-drawer-width 34)
           (herdr-sanctum-sidebar-side 'left)
           (herdr-sanctum-drawer-side 'right)
           (herdr-sanctum-sidebar-default-open t)
           (herdr-sanctum-drawer-default-open t)
           (herdr-sanctum-socket-path (expand-file-name "no.sock" herdr-sanctum-test--root))
           (stray-buf (get-buffer-create " *w4a-stray*")))
      (herdr-sanctum-register-terminal-backend
       'w4a-core-backend
       :start (lambda (buf _prog _args)
                (cl-incf start-calls)
                (with-current-buffer buf (setq-local herdr-sanctum-test--live t)))
       :live-p (lambda (buf) (buffer-local-value 'herdr-sanctum-test--live buf))
       :resize (lambda (_win) (cl-incf resize-calls)))
      (unwind-protect
          (progn
            ;; 1. Narrow-frame fast-path & `herdr-sanctum-reset-layout'.
            (cl-letf (((symbol-function 'herdr-sanctum-ensure-connected) #'ignore)
                      ((symbol-function 'herdr-sanctum-git-fetch-dir) #'ignore)
                      ((symbol-function 'frame-width) (lambda (&optional _) 80)))
              (herdr-sanctum '(4))
              ;; On 80-col frame, sidebar fits alone (drawer omitted so center >= 40 cols).
              (should (herdr-sanctum-sidebar-visible-p))
              (should-not (herdr-sanctum-drawer-visible-p))
              (should (herdr-sanctum--panels-ready-p))
              ;; Re-entering takes fast-path without resizing or restarting.
              (let ((prev-resize resize-calls))
                (herdr-sanctum '(4))
                (should (= start-calls 1))
                (should (= resize-calls prev-resize)))
              ;; Add a stray split window, then `reset-layout' restores clean dedicated layout.
              (let* ((term-win (get-buffer-window herdr-sanctum-buffer-name))
                     (stray-win (split-window term-win nil 'below)))
                (set-window-buffer stray-win stray-buf)
                (select-window stray-win)
                (herdr-sanctum-reset-layout)
                (should-not (window-live-p stray-win))
                (should (equal (buffer-name (window-buffer (selected-window)))
                               herdr-sanctum-buffer-name))
                (should (window-dedicated-p (selected-window)))
                (should (herdr-sanctum-sidebar-visible-p))
                (should-not (herdr-sanctum-drawer-visible-p))))
            ;; On wide frame (140 cols), `reset-layout' restores both enabled panels.
            (cl-letf (((symbol-function 'herdr-sanctum-ensure-connected) #'ignore)
                      ((symbol-function 'herdr-sanctum-git-fetch-dir) #'ignore)
                      ((symbol-function 'frame-width) (lambda (&optional _) 140)))
              (herdr-sanctum-reset-layout)
              (should (herdr-sanctum-sidebar-visible-p))
              (should (herdr-sanctum-drawer-visible-p)))
            ;; 2. `'frame' mode `frame-parameters' & `default-toggle-back'.
            (let* ((herdr-sanctum-display-mode 'frame)
                   (herdr-sanctum-frame-name "Sanctum")
                   (herdr-sanctum-frame-parameters '((alpha-background . 90) (name . "Wrong")))
                   (cur 'f-orig)
                   (made-params nil)
                   (focused nil)
                   (frames '(f-orig f-other))
                   (orig-flp (symbol-function 'frame-live-p))
                   (orig-wlp (symbol-function 'window-live-p)))
              (cl-letf (((symbol-function 'selected-frame) (lambda () cur))
                        ((symbol-function 'frame-list) (lambda () frames))
                        ((symbol-function 'frame-live-p)
                         (lambda (f) (or (memq f frames) (funcall orig-flp f))))
                        ((symbol-function 'frame-visible-p) (lambda (_f) t))
                        ((symbol-function 'frame-parameter)
                         (lambda (f p) (when (and (eq f 'f-sanctum) (eq p 'name)) "Sanctum")))
                        ((symbol-function 'frame-selected-window)
                         (lambda (f) (pcase f ('f-other 'w-other) ('f-orig 'w-orig) (_ nil))))
                        ((symbol-function 'window-live-p)
                         (lambda (w) (or (memq w '(w-orig w-other)) (funcall orig-wlp w))))
                        ((symbol-function 'window-use-time)
                         (lambda (w) (pcase w ('w-other 50) ('w-orig 10) (_ 0))))
                        ((symbol-function 'make-frame)
                         (lambda (params)
                           (setq made-params params)
                           (push 'f-sanctum frames)
                           'f-sanctum))
                        ((symbol-function 'select-frame-set-input-focus)
                         (lambda (f &optional _) (setq focused f cur f))))
                (herdr-sanctum--prepare-host)
                (should (eq herdr-sanctum--previous-frame 'f-orig))
                (should (equal (alist-get 'name made-params) "Sanctum"))
                (should (eql (alist-get 'alpha-background made-params) 90))
                (should (eq focused 'f-sanctum))
                ;; Toggle back prefers `--previous-frame' while live/visible.
                (should (herdr-sanctum-default-toggle-back))
                (should (eq focused 'f-orig))
                ;; When `--previous-frame' is dead, falls back to MRU non-Sanctum frame.
                (setq cur 'f-sanctum
                      frames '(f-sanctum f-other))
                (should (herdr-sanctum-default-toggle-back))
                (should (eq focused 'f-other))))
            ;; 3. Side-panel DWIM (`close-dwim', `split-right'/`down', `new-tab-dwim').
            (let ((rpc-calls nil)
                  (fallback-hit nil))
              (herdr-sanctum-apply-snapshot (herdr-sanctum-ui-test--rich-snapshot))
              (cl-letf (((symbol-function 'herdr-sanctum-rpc-call)
                         (lambda (m &optional params &rest _) (push (cons m params) rpc-calls)))
                        ((symbol-function 'herdr-sanctum-ensure-connected) #'ignore))
                ;; Inside sidebar: split-right/down & new-tab-dwim send RPCs; close-dwim hides sidebar.
                (select-window (get-buffer-window herdr-sanctum-sidebar-buffer-name))
                (herdr-sanctum-split-right)
                (should (equal (car rpc-calls) '("pane.split" (direction . "right") (focus . t))))
                (herdr-sanctum-split-down)
                (should (equal (car rpc-calls) '("pane.split" (direction . "down") (focus . t))))
                (herdr-sanctum-new-tab-dwim)
                (should (equal (caar rpc-calls) "tab.create"))
                (should (equal (alist-get 'workspace_id (cdar rpc-calls)) "w1"))
                (herdr-sanctum-close-dwim)
                (should-not (herdr-sanctum-sidebar-visible-p))
                ;; Inside drawer: close-dwim hides drawer.
                (select-window (get-buffer-window herdr-sanctum-drawer-buffer-name))
                (herdr-sanctum-close-dwim)
                (should-not (herdr-sanctum-drawer-visible-p))
                ;; Non-Sanctum side window cannot be split; `new-tab-dwim' calls fallback outside Sanctum.
                (let ((side-w (display-buffer-in-side-window stray-buf '((side . bottom)))))
                  (unwind-protect
                      (progn
                        (select-window side-w)
                        (should-error (herdr-sanctum-split-right) :type 'user-error)
                        (should-error (herdr-sanctum-split-down) :type 'user-error)
                        (let ((herdr-sanctum-new-tab-fallback-function
                               (lambda () (interactive) (setq fallback-hit t))))
                          (herdr-sanctum-new-tab-dwim)
                          (should fallback-hit)))
                    (when (window-live-p side-w) (delete-window side-w))))))
            ;; 4. `jump' `include-idle-agents' & `'herdr-sanctum-target' text property.
            (let ((snap (herdr-sanctum-ui-test--rich-snapshot)))
              (let* ((herdr-sanctum-jump-include-idle-agents nil)
                     (cands (herdr-sanctum--jump-candidates snap)))
                (should-not (seq-some (lambda (c) (equal (plist-get (cdr c) :pane-id) "w2:p5"))
                                      cands))
                (dolist (c cands)
                  (should (equal (get-text-property 0 'herdr-sanctum-target (car c)) (cdr c)))))
              (let* ((herdr-sanctum-jump-include-idle-agents t)
                     (cands (herdr-sanctum--jump-candidates snap))
                     (agent-ids (mapcar (lambda (c) (plist-get (cdr c) :pane-id))
                                        (seq-filter (lambda (c) (eq (plist-get (cdr c) :kind) 'agent))
                                                    cands))))
                (should (equal agent-ids '("w1:p3" "w2:p4" "w1:p2" "w2:p5")))))
            ;; 5. `agent-step' cold-start, rapid optimistic stepping, & attention-only stepping.
            (let* ((focused-ids nil)
                   (snap (herdr-sanctum-ui-test--rich-snapshot))
                   (wire-snap
                    `((focused_workspace_id . ,(alist-get 'focused_workspace_id snap))
                      (focused_tab_id . ,(alist-get 'focused_tab_id snap))
                      (focused_pane_id . ,(alist-get 'focused_pane_id snap))
                      (workspaces . ,(vconcat (alist-get 'workspaces snap)))
                      (tabs . ,(vconcat (alist-get 'tabs snap)))
                      (panes . ,(vconcat (alist-get 'panes snap)))
                      (agents . ,(vconcat (alist-get 'agents snap))))))
              (herdr-sanctum-test-with-mock-server (_sock)
                  (lambda (client data)
                    (dolist (req (herdr-sanctum-test--parse-json-lines data))
                      (let ((id (alist-get 'id req))
                            (method (alist-get 'method req)))
                        (cond
                         ((equal method "session.snapshot")
                          (herdr-sanctum-test--send-json
                           client `((id . ,id)
                                    (result . ((snapshot . ,wire-snap))))))
                         ((equal method "pane.focus")
                          (push (alist-get 'pane_id (alist-get 'params req)) focused-ids)
                          (herdr-sanctum-test--send-json
                           client `((id . ,id) (result . ((ok . t))))))))))
                ;; Cold-start: no cached snapshot -> fetches over socket and focuses next agent.
                (setq herdr-sanctum--snapshot nil
                      herdr-sanctum--last-step-pane-id nil
                      herdr-sanctum--last-step-time 0.0)
                ;; Ensure `*sanctum*' is visible so `agent-step' doesn't invoke full `herdr-sanctum'.
                (switch-to-buffer (get-buffer-create herdr-sanctum-buffer-name))
                ;; Focused pane in snap is "w1:p2" (index 2 of ["w1:p3" "w2:p4" "w1:p2" "w2:p5"]).
                (herdr-sanctum-next-agent)
                (should (herdr-sanctum-test--wait-until (lambda () (= (length focused-ids) 1))))
                (should (equal (car focused-ids) "w2:p5"))
                ;; Rapid second call within 0.5s advances past "w2:p5" -> wraps to "w1:p3".
                (herdr-sanctum-next-agent)
                (should (herdr-sanctum-test--wait-until (lambda () (= (length focused-ids) 2))))
                (should (equal (car focused-ids) "w1:p3"))
                ;; Attention-only stepping visits ONLY blocked ("w1:p3") and done ("w2:p4").
                (herdr-sanctum-next-attention-agent)
                (should (herdr-sanctum-test--wait-until (lambda () (= (length focused-ids) 3))))
                (should (equal (car focused-ids) "w2:p4"))
                (herdr-sanctum-prev-attention-agent)
                (should (herdr-sanctum-test--wait-until (lambda () (= (length focused-ids) 4))))
                (should (equal (car focused-ids) "w1:p3"))
                ;; Empty attention set does not dispatch RPC.
                (herdr-sanctum-apply-snapshot
                 (herdr-sanctum-test-make-snapshot
                  :agents '(((pane_id . "w1:p1") (agent_status . "working")))))
                (herdr-sanctum-next-attention-agent)
                (should (= (length focused-ids) 4))))
            ;; 6. `command-map' and `repeat-map' bindings.
            (dolist (pair '(("s" . herdr-sanctum)
                            ("b" . herdr-sanctum-sidebar-toggle)
                            ("d" . herdr-sanctum-drawer-toggle)
                            ("o" . herdr-sanctum-drawer-open-view)
                            ("f" . herdr-sanctum-focus-terminal)
                            ("r" . herdr-sanctum-reset-layout)
                            ("j" . herdr-sanctum-jump)
                            ("n" . herdr-sanctum-next-agent)
                            ("p" . herdr-sanctum-prev-agent)
                            ("a" . herdr-sanctum-next-attention-agent)
                            ("A" . herdr-sanctum-prev-attention-agent)
                            ("[" . herdr-sanctum-prev-tab)
                            ("]" . herdr-sanctum-next-tab)
                            ("t" . herdr-sanctum-new-tab-dwim)
                            ("v" . herdr-sanctum-split-right)
                            ("h" . herdr-sanctum-split-down)
                            ("z" . herdr-sanctum-zoom-or-maximize)
                            ("x" . herdr-sanctum-close-dwim)
                            ("g" . herdr-sanctum-refresh-state)
                            ("q" . herdr-sanctum-disconnect)
                            ("?" . herdr-sanctum-doctor)))
              (should (eq (keymap-lookup herdr-sanctum-command-map (car pair)) (cdr pair))))
            (dolist (pair '(("n" . herdr-sanctum-next-agent)
                            ("p" . herdr-sanctum-prev-agent)
                            ("a" . herdr-sanctum-next-attention-agent)
                            ("A" . herdr-sanctum-prev-attention-agent)
                            ("]" . herdr-sanctum-next-tab)
                            ("[" . herdr-sanctum-prev-tab)))
              (should (eq (keymap-lookup herdr-sanctum-repeat-map (car pair)) (cdr pair)))
              (should (eq (get (cdr pair) 'repeat-map) 'herdr-sanctum-repeat-map)))
            ;; 7. `doctor' git, config-button, and live vs stale vs missing socket checks.
            (herdr-sanctum-test-with-temp-dir doc-dir
              (let* ((cfg-file (expand-file-name "config.toml" doc-dir))
                     (stale-sock (expand-file-name
                                  (format "stale-%d.sock" (cl-incf herdr-sanctum-test--seq))
                                  herdr-sanctum-test--root))
                     (stale-srv (make-network-process :name "stale-probe-srv"
                                                      :server t
                                                      :family 'local
                                                      :service stale-sock
                                                      :noquery t))
                     (opened-cfg nil)
                     (herdr-sanctum-open-outside-function nil)
                     (herdr-sanctum-open-file-function (lambda (p) (setq opened-cfg p))))
                (delete-process stale-srv)
                (with-temp-file cfg-file (insert "[ui]\n"))
                (unwind-protect
                    (progn
                      ;; 7a. Live socket + existing config button + git executable.
                      (herdr-sanctum-test-with-mock-server (_live-sock)
                          (lambda (_c _d) nil)
                        (let ((herdr-sanctum-config-file cfg-file))
                          (save-window-excursion
                            (herdr-sanctum-doctor)
                            (with-current-buffer "*herdr-sanctum-doctor*"
                              (let ((txt (buffer-string)))
                                (should (string-search "git executable" txt))
                                (should-not (string-search "git executable   : MISSING" txt))
                                (should (string-search "(live)" txt))
                                (should (string-search "(found)" txt)))
                              (goto-char (point-min))
                              (search-forward cfg-file)
                              (let ((btn (button-at (match-beginning 0))))
                                (should btn)
                                (button-activate btn)
                                (should (equal opened-cfg cfg-file)))))))
                      ;; 7b. Stale socket + nil config + missing git program.
                      (let ((herdr-sanctum-socket-path stale-sock)
                            (herdr-sanctum-config-file nil)
                            (herdr-sanctum-git-program "definitely-missing-git-xyz"))
                        (save-window-excursion
                          (herdr-sanctum-doctor)
                          (with-current-buffer "*herdr-sanctum-doctor*"
                            (let ((txt (buffer-string)))
                              (should (string-search "MISSING (definitely-missing-git-xyz" txt))
                              (should (string-search "(stale (not accepting connections))" txt))
                              (should (string-search "herdr config     : nil (disabled)" txt)))))))
                  (when (file-exists-p stale-sock)
                    (ignore-errors (delete-file stale-sock)))))))
        (when (buffer-live-p stray-buf) (kill-buffer stray-buf))))))

(provide 'herdr-sanctum-ui-test)
;;; herdr-sanctum-ui-test.el ends here
