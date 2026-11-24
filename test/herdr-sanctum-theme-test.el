;;; herdr-sanctum-theme-test.el --- Theme & SVG badge tests -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT tests exclusively covering `herdr-sanctum-theme': standalone theme load
;; (no RPC/UI), real `modus-vivendi' load/disable `'unspecified' fallback,
;; palette caching, `apply-theme' coalescing & failure rollback, `default' face
;; re-apply (daemon->GUI), agent badge opacity/cache-key/`setopt' invalidation,
;; SVG prolog stripping, and frame-aware `svg-available-p'.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'svg)
(require 'herdr-sanctum-test-helper)

(declare-function modus-themes-get-color-value "modus-themes" (color &optional overrides theme))

(ert-deftest herdr-sanctum-theme-test/standalone-no-rpc ()
  "Loading `herdr-sanctum-theme' standalone does not load `rpc' or `ui'."
  (let* ((emacs-bin (expand-file-name invocation-name invocation-directory))
         (repo-dir (file-name-directory
                    (file-truename (locate-library "herdr-sanctum-theme"))))
         (output
          (with-temp-buffer
            (let ((rc (call-process
                       emacs-bin nil t nil
                       "-Q" "--batch" "-L" repo-dir
                       "--eval"
                       "(progn (require 'herdr-sanctum-theme) (princ (list (featurep 'herdr-sanctum-theme) (featurep 'herdr-sanctum-rpc) (featurep 'herdr-sanctum-ui))))")))
              (should (zerop rc))
              (buffer-string)))))
    (should (equal output "(t nil nil)"))))

(ert-deftest herdr-sanctum-theme-test/palette-derivation-and-caching ()
  "Palette handles `modus-vivendi' load/disable, overrides, and caching."
  (herdr-sanctum-test-with-clean-state
    ;; 1. Built-in `modus-vivendi' load -> disable exercises real `'unspecified' fallback.
    (unwind-protect
        (let ((herdr-sanctum-palette-function #'herdr-sanctum-default-palette-color))
          (load-theme 'modus-vivendi t)
          (let ((modus-bg (modus-themes-get-color-value 'bg-dim t))
                (resolved (herdr-sanctum-palette-color 'bg-chrome))
                (card (herdr-sanctum-palette-color 'bg-card))
                (sel (herdr-sanctum-palette-color 'bg-selected))
                (hl (herdr-sanctum-palette-color 'bg-hl)))
            (should (herdr-sanctum--valid-color-p modus-bg))
            (should (equal resolved modus-bg))
            ;; Surface ladder stays distinct so cards and selections never vanish.
            (should-not (equal resolved card))
            (should-not (equal card sel))
            (should-not (equal resolved hl)))
          ;; Disabling `modus-vivendi' causes `modus-themes-get-color-value' to return
          ;; `'unspecified'; `disable-theme' hook and palette resolution must fall back cleanly.
          (disable-theme 'modus-vivendi)
          (should (eq (modus-themes-get-color-value 'bg-tab-bar t) 'unspecified))
          ;; `modus-operandi' also keeps distinct chrome/card/selected/hl steps.
          (unwind-protect
              (progn
                (load-theme 'modus-operandi t)
                (let ((chrome (herdr-sanctum-palette-color 'bg-chrome))
                      (card (herdr-sanctum-palette-color 'bg-card))
                      (sel (herdr-sanctum-palette-color 'bg-selected))
                      (hl (herdr-sanctum-palette-color 'bg-hl)))
                  (should (equal chrome (modus-themes-get-color-value 'bg-dim t)))
                  (should-not (equal chrome card))
                  (should-not (equal card sel))
                  (should-not (equal chrome hl))))
            (when (memq 'modus-operandi custom-enabled-themes)
              (disable-theme 'modus-operandi)))
          (let* ((bg (herdr-sanctum-palette-color 'bg-chrome))
                 (spec (get 'herdr-sanctum-chrome-bar 'face-defface-spec))
                 (gfx-attrs (cdr (assoc '((type graphic)) spec)))
                 (tty-attrs (cdr (assq t spec))))
            (should (herdr-sanctum--valid-color-p bg))
            (should (herdr-sanctum--valid-color-p (plist-get tty-attrs :background)))
            (should-not (plist-get tty-attrs :underline))
            (should (herdr-sanctum--valid-color-p
                     (plist-get (plist-get gfx-attrs :underline) :color))))
          ;; Generic dark/light derivation works and caches by `(background-mode . token)'.
          (cl-letf (((symbol-function 'herdr-sanctum--background-mode) (lambda () 'light)))
            (let ((bg (herdr-sanctum-palette-color 'bg-chrome)))
              (should (stringp bg))
              (should (string-prefix-p "#" bg))))
          (clrhash herdr-sanctum--palette-cache)
          (let ((derive-calls 0)
                (orig-derive (symbol-function 'herdr-sanctum--derive-generic-color)))
            (cl-letf (((symbol-function 'herdr-sanctum--background-mode) (lambda () 'dark))
                      ((symbol-function 'herdr-sanctum--derive-generic-color)
                       (lambda (tok) (cl-incf derive-calls) (funcall orig-derive tok))))
              (let ((c1 (herdr-sanctum-default-palette-color 'bg-chrome))
                    (c2 (herdr-sanctum-default-palette-color 'bg-chrome)))
                (should (equal c1 c2))
                (should (herdr-sanctum--valid-color-p c1))
                (should (= derive-calls 1))))))
      (when (memq 'modus-vivendi custom-enabled-themes)
        (disable-theme 'modus-vivendi))
      (herdr-sanctum-apply-theme t))
    ;; 2. Custom palette function honors valid colors and ignores `'unspecified'/invalid returns.
    (let ((herdr-sanctum-palette-function
           (lambda (token _fb)
             (pcase token
               ('accent "#123456")
               ('bg-chrome 'unspecified)
               ('fg-main "")
               (_ nil)))))
      (should (equal (herdr-sanctum-palette-color 'accent) "#123456"))
      (should (herdr-sanctum--valid-color-p (herdr-sanctum-palette-color 'bg-chrome)))
      (should (herdr-sanctum--valid-color-p (herdr-sanctum-palette-color 'fg-main))))))

(ert-deftest herdr-sanctum-theme-test/apply-theme-coalescing-and-face-specs ()
  "`apply-theme' coalesces, rolls back on error, and emits single `(t ...)' specs."
  (herdr-sanctum-test-with-clean-state
    (should (memq #'herdr-sanctum--on-theme-change enable-theme-functions))
    (should (memq #'herdr-sanctum--on-theme-change disable-theme-functions))
    (let* ((hook-calls 0)
           (hook-fn (lambda () (cl-incf hook-calls))))
      (unwind-protect
          (progn
            (add-hook 'herdr-sanctum-theme-change-hook hook-fn)
            (herdr-sanctum-apply-theme t)
            (let ((rev1 herdr-sanctum-theme-revision))
              (should (= hook-calls 1))
              ;; Duplicate non-forced call with identical state coalesces cleanly.
              (herdr-sanctum-apply-theme)
              (should (= herdr-sanctum-theme-revision rev1))
              (should (= hook-calls 1))
              ;; Forced call re-applies and increments revision.
              (herdr-sanctum-apply-theme t)
              (should (= herdr-sanctum-theme-revision (1+ rev1)))
              (should (= hook-calls 2))
              ;; Error during `face-spec-set' must NOT commit `--last-theme-state'.
              (let ((prev-state herdr-sanctum--last-theme-state)
                    (rev2 herdr-sanctum-theme-revision)
                    (herdr-sanctum-ui-font-height 1.15))
                (cl-letf (((symbol-function 'face-spec-set)
                           (lambda (&rest _) (error "simulated face-spec-set failure"))))
                  (should-error (herdr-sanctum-apply-theme)))
                (should (equal herdr-sanctum--last-theme-state prev-state))
                (should (= herdr-sanctum-theme-revision rev2))
                ;; Subsequent non-forced call retries and succeeds cleanly.
                (herdr-sanctum-apply-theme)
                (should (= herdr-sanctum-theme-revision (1+ rev2))))))
        (remove-hook 'herdr-sanctum-theme-change-hook hook-fn)
        (herdr-sanctum-apply-theme t)))
    ;; Chrome bar splits graphic vs TTY underline clauses; status bar stays single `(t ...)'.
    (let* ((chrome-spec (get 'herdr-sanctum-chrome-bar 'face-defface-spec))
           (tty-attrs (cdr (assq t chrome-spec)))
           (status-spec (get 'herdr-sanctum-status-bar 'face-defface-spec)))
      (should (= (length chrome-spec) 2))
      (should (assoc '((type graphic)) chrome-spec))
      (should (equal (plist-get tty-attrs :height) herdr-sanctum-ui-font-height))
      (should (herdr-sanctum--valid-color-p (plist-get tty-attrs :background)))
      (should (= (length status-spec) 1)))))

(ert-deftest herdr-sanctum-theme-test/default-face-change-reapplies ()
  "`default' face bg/fg change triggers re-apply with unchanged themes and mode."
  (herdr-sanctum-test-with-clean-state
    (let ((orig-bg (face-attribute 'default :background nil))
          (orig-fg (face-attribute 'default :foreground nil)))
      (unwind-protect
          (progn
            (herdr-sanctum-apply-theme t)
            (let ((rev1 herdr-sanctum-theme-revision)
                  (themes1 (copy-sequence custom-enabled-themes))
                  (mode1 (herdr-sanctum--background-mode))
                  (bg1 (herdr-sanctum-palette-color 'bg-chrome)))
              ;; Simulate daemon/TTY -> GUI transition where `default' gains concrete colors
              ;; while `custom-enabled-themes' and `background-mode' stay unchanged.
              (set-face-attribute 'default nil :background "#101820" :foreground "#e0e8f0")
              (should (equal custom-enabled-themes themes1))
              (should (eq (herdr-sanctum--background-mode) mode1))
              (herdr-sanctum--frame-theme (selected-frame))
              (should (= herdr-sanctum-theme-revision (1+ rev1)))
              (let ((bg2 (herdr-sanctum-palette-color 'bg-chrome)))
                (should-not (equal bg2 bg1))
                (should (equal bg2 (herdr-sanctum--blend "#101820" "#e0e8f0" 0.06))))
              ;; Same state coalesces on repeat.
              (herdr-sanctum-apply-theme)
              (should (= herdr-sanctum-theme-revision (1+ rev1)))
              ;; Changing only `:foreground' also invalidates coalesced state.
              (set-face-attribute 'default nil :foreground "#aabbcc")
              (herdr-sanctum-apply-theme)
              (should (= herdr-sanctum-theme-revision (+ rev1 2)))))
        (set-face-attribute 'default nil :background orig-bg :foreground orig-fg)
        (herdr-sanctum-apply-theme t)))))

(ert-deftest herdr-sanctum-theme-test/svg-badges-fallback-and-path-traversal ()
  "SVG badges render, cache sizes, fall back cleanly, and block traversal."
  (herdr-sanctum-test-with-clean-state
    (cl-letf (((symbol-function 'herdr-sanctum-svg-available-p) (lambda (&optional _) t)))
      ;; 1. Space badges: SVG 15px vs 13px distinct cache entries, text, and none.
      (clrhash herdr-sanctum--svg-cache)
      (let* ((b15 (herdr-sanctum-space-badge "skillshare" "w1"))
             (b13 (herdr-sanctum-space-badge "skillshare" "w1" nil 13))
             (img15 (get-text-property 0 'display b15))
             (img13 (get-text-property 0 'display b13))
             (data15 (plist-get (cdr img15) :data))
             (data13 (plist-get (cdr img13) :data)))
        (should (eq (car-safe img15) 'image))
        (should (eq (car-safe img13) 'image))
        (should-not (equal img15 img13))
        (should (= (hash-table-count herdr-sanctum--svg-cache) 2))
        (should (string-search "width=\"15\"" data15))
        (should (string-search "width=\"13\"" data13)))
      (let ((herdr-sanctum-space-badge-style 'text))
        (let ((txt (herdr-sanctum-space-badge "skillshare" "w1")))
          (should (equal (substring-no-properties txt) " S "))
          (should-not (get-text-property 0 'display txt))))
      (let ((herdr-sanctum-space-badge-style 'none))
        (should (equal (herdr-sanctum-space-badge "skillshare" "w1") "")))
      ;; 2. Agent badges: static SVG files, alias, monogram fallback, and unicode.
      (let ((herdr-sanctum-agent-icon-style 'svg-files))
        (clrhash herdr-sanctum--svg-cache)
        (should (file-directory-p herdr-sanctum-agent-icon-directory))
        (dolist (ag '("pi" "claude" "claude-code" "codex" "gemini" "opencode" "unknown-agent"))
          (dolist (st '("blocked" "done" "working" "idle"))
            (let ((ag-badge (herdr-sanctum-agent-badge ag st)))
              (should (eq (car-safe (get-text-property 0 'display ag-badge)) 'image))))))
      (let ((herdr-sanctum-agent-icon-style 'unicode))
        (let ((ag-badge (herdr-sanctum-agent-badge "pi" "blocked")))
          (should (equal (substring-no-properties ag-badge) "◆"))
          (should (eq (get-text-property 0 'face ag-badge) 'herdr-sanctum-status-blocked))
          (should-not (get-text-property 0 'display ag-badge))))
      ;; 3. SVG path-traversal guard rejects traversal stems and malicious brand aliases.
      (clrhash herdr-sanctum--svg-file-cache)
      (dolist (bad '("../etc/passwd" "../../secret" "." ".." "sub/icon" "a\\b" "UPPER" "bad name"))
        (should-not (herdr-sanctum--agent-svg-file-raw bad)))
      (let ((herdr-sanctum-agent-brands '(("evil" . (:file "../claude")))))
        (should-not (herdr-sanctum--agent-svg-file-raw "evil")))
      ;; 4. Row accent: active SVG, guide SVG, 8px spacer.
      (should (eq (car-safe (get-text-property 0 'display (herdr-sanctum-row-accent t "#24283b"))) 'image))
      (should (eq (car-safe (get-text-property 0 'display (herdr-sanctum-row-accent nil nil t))) 'image))
      (should (equal (get-text-property 0 'display (herdr-sanctum-row-accent nil)) '(space :width (8)))))
    ;; TTY fallback for row accent.
    (cl-letf (((symbol-function 'herdr-sanctum-svg-available-p) (lambda (&optional _) nil)))
      (should (equal (substring-no-properties (herdr-sanctum-row-accent t)) "▎"))
      (should (equal (substring-no-properties (herdr-sanctum-row-accent nil nil t)) "│"))
      (should (equal (herdr-sanctum-row-accent nil) " ")))))

(ert-deftest herdr-sanctum-theme-test/agent-badge-opacity-cache-prolog-and-frame-svg ()
  "Agent badges handle nil/idle opacity, cache keys, prologs, and frame SVG."
  (herdr-sanctum-test-with-clean-state
    ;; 1. `herdr-sanctum-svg-available-p' accepts `&optional frame' and forwards it.
    (let (seen-frames)
      (cl-letf (((symbol-function 'display-graphic-p)
                 (lambda (&optional f) (push f seen-frames) (eq f 'fake-gui-frame)))
                ((symbol-function 'image-type-available-p)
                 (lambda (type) (eq type 'svg))))
        (should-not (herdr-sanctum-svg-available-p))
        (should (herdr-sanctum-svg-available-p 'fake-gui-frame))
        (should-not (herdr-sanctum-svg-available-p 'fake-tty-frame))
        (should (equal (nreverse seen-frames) '(nil fake-gui-frame fake-tty-frame)))))
    (cl-letf (((symbol-function 'herdr-sanctum-svg-available-p) (lambda (&optional _) t)))
      ;; 2. Calling `(agent-badge "pi" nil)' before `(agent-badge "pi" "idle")' uses 0.58 opacity.
      (clrhash herdr-sanctum--svg-cache)
      (let* ((b-nil (herdr-sanctum-agent-badge "pi" nil))
             (b-idle (herdr-sanctum-agent-badge "pi" "idle"))
             (b-work (herdr-sanctum-agent-badge "pi" "working"))
             (img-nil (get-text-property 0 'display b-nil))
             (img-idle (get-text-property 0 'display b-idle))
             (img-work (get-text-property 0 'display b-work))
             (data-nil (plist-get (cdr img-nil) :data))
             (data-idle (plist-get (cdr img-idle) :data))
             (data-work (plist-get (cdr img-work) :data)))
        (should (string-search "opacity=\"0.58\"" data-nil))
        (should (string-search "opacity=\"0.58\"" data-idle))
        (should (eq img-nil img-idle))
        (should (string-search "opacity=\"1.00\"" data-work)))
      ;; 3. Cache key distinguishes `dot-color', `cutout-bg' (`bg-panel'), and `agent-icon-directory'.
      (clrhash herdr-sanctum--svg-cache)
      (let* ((dot-red "#ff1111")
             (panel-bg "#111111")
             (herdr-sanctum-palette-function
              (lambda (tok _fb)
                (pcase tok
                  ('accent "#82aaff")
                  ('red dot-red)
                  ('bg-panel panel-bg)
                  (_ "#a6b2d4")))))
        (let ((d1 (plist-get (cdr (get-text-property 0 'display
                                                     (herdr-sanctum-agent-badge "pi" "blocked")))
                             :data)))
          (should (string-search "fill=\"#ff1111\"" d1))
          (should (string-search "stroke=\"#111111\"" d1)))
        (setq dot-red "#ee2222")
        (let ((d2 (plist-get (cdr (get-text-property 0 'display
                                                     (herdr-sanctum-agent-badge "pi" "blocked")))
                             :data)))
          (should (string-search "fill=\"#ee2222\"" d2))
          (should (string-search "stroke=\"#111111\"" d2)))
        (setq panel-bg "#222222")
        (let ((d3 (plist-get (cdr (get-text-property 0 'display
                                                     (herdr-sanctum-agent-badge "pi" "blocked")))
                             :data)))
          (should (string-search "fill=\"#ee2222\"" d3))
          (should (string-search "stroke=\"#222222\"" d3))))
      ;; 4. `setopt' on `herdr-sanctum-agent-brands' invalidates SVG cache & notifies hook.
      (let ((orig-brands herdr-sanctum-agent-brands)
            (hook-calls 0)
            (hook-fn nil))
        (setq hook-fn (lambda () (cl-incf hook-calls)))
        (unwind-protect
            (progn
              (add-hook 'herdr-sanctum-theme-change-hook hook-fn)
              (herdr-sanctum-agent-badge "pi" "working")
              (should (> (hash-table-count herdr-sanctum--svg-cache) 0))
              (let ((rev1 herdr-sanctum-theme-revision))
                (setopt herdr-sanctum-agent-brands '(("pi" . (:color "#123456"))))
                (should (= (hash-table-count herdr-sanctum--svg-cache) 0))
                (should (= herdr-sanctum-theme-revision (1+ rev1)))
                (should (= hook-calls 1))
                (let ((data (plist-get (cdr (get-text-property
                                             0 'display
                                             (herdr-sanctum-agent-badge "pi" "working")))
                                       :data)))
                  (should (string-search "#123456" data)))))
          (remove-hook 'herdr-sanctum-theme-change-hook hook-fn)
          (setopt herdr-sanctum-agent-brands orig-brands)))
      ;; 4b. `setopt' on `herdr-sanctum-agent-icon-directory' invalidates SVG caches.
      (let ((orig-dir herdr-sanctum-agent-icon-directory))
        (unwind-protect
            (progn
              (herdr-sanctum-agent-badge "pi" "working")
              (should (> (hash-table-count herdr-sanctum--svg-cache) 0))
              (let ((rev1 herdr-sanctum-theme-revision))
                (setopt herdr-sanctum-agent-icon-directory nil)
                (should (= (hash-table-count herdr-sanctum--svg-cache) 0))
                (should (= (hash-table-count herdr-sanctum--svg-file-cache) 0))
                (should (= herdr-sanctum-theme-revision (1+ rev1)))))
          (setopt herdr-sanctum-agent-icon-directory orig-dir)))
      ;; 5. Custom SVG prologs (`<?xml ...?>`, `<!DOCTYPE ...>`, comments) are stripped cleanly.
      (herdr-sanctum-test-with-temp-dir tmpdir
        (with-temp-file (expand-file-name "custom.svg" tmpdir)
          (insert "\ufeff<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
                  "<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\"\n"
                  "  \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">\n"
                  "<!-- exported icon -->\n"
                  "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"1em\" height=\"1em\""
                  " viewBox=\"0 0 24 24\" fill=\"currentColor\">"
                  "<circle cx=\"12\" cy=\"12\" r=\"9\"/></svg>\n"))
        (with-temp-file (expand-file-name "subset.svg" tmpdir)
          (insert "<?xml version=\"1.0\"?>\n"
                  "<!DOCTYPE svg [\n<!ENTITY r \"8\">\n]>\n"
                  "<svg xmlns=\"http://www.w3.org/2000/svg\" viewBox=\"0 0 24 24\">"
                  "<rect width=\"24\" height=\"24\"/></svg>"))
        (with-temp-file (expand-file-name "empty-prolog.svg" tmpdir)
          (insert "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"))
        (with-temp-file (expand-file-name "pi.svg" tmpdir)
          (insert "<svg xmlns=\"http://www.w3.org/2000/svg\" viewBox=\"0 0 24 24\">"
                  "<polygon points=\"12,2 22,22 2,22\"/></svg>"))
        ;; Changing `herdr-sanctum-agent-icon-directory' produces a distinct cache entry.
        (let ((default-pi (plist-get (cdr (get-text-property
                                           0 'display
                                           (herdr-sanctum-agent-badge "pi" "working")))
                                     :data)))
          (let ((herdr-sanctum-agent-icon-directory tmpdir))
            (let ((custom-pi (plist-get (cdr (get-text-property
                                              0 'display
                                              (herdr-sanctum-agent-badge "pi" "working")))
                                        :data)))
              (should-not (equal default-pi custom-pi))
              (should (string-search "<polygon " custom-pi)))))
        (let ((herdr-sanctum-agent-icon-directory tmpdir))
          (let ((raw (herdr-sanctum--agent-svg-file-raw "custom")))
            (should (string-prefix-p "<svg " raw))
            (should-not (string-search "<?xml" raw))
            (should-not (string-search "<!DOCTYPE" raw))
            (should-not (string-search "<!--" raw)))
          (let ((data (plist-get (cdr (get-text-property
                                       0 'display
                                       (herdr-sanctum-agent-badge "custom" "working")))
                                 :data)))
            (should-not (string-search "<?xml" data))
            (should-not (string-search "<!DOCTYPE" data))
            (should (string-search "<circle cx=\"12\" cy=\"12\" r=\"9\"/>" data))
            (should (string-search "width=\"24\"" data)))
          (let ((raw-sub (herdr-sanctum--agent-svg-file-raw "subset")))
            (should (string-prefix-p "<svg " raw-sub))
            (should-not (string-search "<!DOCTYPE" raw-sub)))
          ;; Prolog-only SVG returns nil and falls back cleanly to monogram SVG.
          (should-not (herdr-sanctum--agent-svg-file-raw "empty-prolog"))
          (let ((mono (get-text-property
                       0 'display
                       (herdr-sanctum-agent-badge "empty-prolog" "working"))))
            (should (eq (car-safe mono) 'image))))))))

(ert-deftest herdr-sanctum-theme-test/palette-overrides-and-panel-surface-blend ()
  "`palette-overrides', `panel-surface-blend', `bg-panel', and custom options."
  (herdr-sanctum-test-with-clean-state
    (let ((orig-overrides herdr-sanctum-palette-overrides)
          (orig-blend herdr-sanctum-panel-surface-blend)
          (orig-palette herdr-sanctum-space-badge-palette)
          (orig-bg (face-attribute 'default :background nil)))
      (unwind-protect
          (progn
            ;; 1. `palette-overrides' takes precedence over palette function and ignores invalid colors.
            (let ((herdr-sanctum-palette-function (lambda (_tok _fb) "#999999"))
                  (herdr-sanctum-palette-overrides '((accent . "#abcdef")
                                                     (border . "not-a-color")
                                                     (guide . unspecified))))
              (should (equal (herdr-sanctum-palette-color 'accent) "#abcdef"))
              (should (equal (herdr-sanctum-palette-color 'border) "#999999"))
              (should (equal (herdr-sanctum-palette-color 'guide) "#999999")))
            ;; 2. `bg-panel' equals `bg-chrome' when blend is nil, and blends `default' -> `bg-chrome' when numeric.
            (set-face-attribute 'default nil :background "#0d0e1c")
            (let ((herdr-sanctum-palette-overrides '((bg-chrome . "#151828")))
                  (herdr-sanctum-panel-surface-blend nil))
              (should (equal (herdr-sanctum-palette-color 'bg-panel) "#151828")))
            (let ((herdr-sanctum-palette-overrides '((bg-chrome . "#151828")))
                  (herdr-sanctum-panel-surface-blend 0.58))
              (should (equal (herdr-sanctum-palette-color 'bg-panel)
                             (herdr-sanctum--blend "#0d0e1c" "#151828" 0.58)))
              (should (equal (herdr-sanctum-palette-color 'bg-panel) "#111322")))
            ;; Clamping 0.0 / <0 -> default bg, 1.0 / >1 -> bg-chrome; explicit `bg-panel' override wins.
            (let ((herdr-sanctum-palette-overrides '((bg-chrome . "#151828"))))
              (let ((herdr-sanctum-panel-surface-blend -0.5))
                (should (equal (herdr-sanctum-palette-color 'bg-panel) "#0d0e1c")))
              (let ((herdr-sanctum-panel-surface-blend 1.8))
                (should (equal (herdr-sanctum-palette-color 'bg-panel) "#151828"))))
            (let ((herdr-sanctum-palette-overrides '((bg-chrome . "#151828")
                                                     (bg-panel . "#223344")))
                  (herdr-sanctum-panel-surface-blend 0.58))
              (should (equal (herdr-sanctum-palette-color 'bg-panel) "#223344")))
            ;; 3. Agent badge cutout stroke tracks blended `bg-panel' (not `bg-chrome') and `active-bg'.
            (cl-letf (((symbol-function 'herdr-sanctum-svg-available-p) (lambda (&optional _) t)))
              (clrhash herdr-sanctum--svg-cache)
              (let ((herdr-sanctum-palette-overrides '((bg-chrome . "#151828")))
                    (herdr-sanctum-panel-surface-blend 0.58))
                (let ((data (plist-get (cdr (get-text-property
                                             0 'display
                                             (herdr-sanctum-agent-badge "pi" "blocked")))
                                       :data))
                      (active-data (plist-get (cdr (get-text-property
                                                    0 'display
                                                    (herdr-sanctum-agent-badge "pi" "blocked" "#24283b")))
                                              :data)))
                  (should (string-search "stroke=\"#111322\"" data))
                  (should-not (string-search "stroke=\"#151828\"" data))
                  (should (string-search "stroke=\"#24283b\"" active-data)))))
            ;; 4. Theme state coalescing detects `palette-overrides' and `panel-surface-blend' changes.
            (herdr-sanctum-apply-theme t)
            (let ((rev1 herdr-sanctum-theme-revision))
              (let ((herdr-sanctum-palette-overrides '((accent . "#112233"))))
                (herdr-sanctum-apply-theme)
                (should (= herdr-sanctum-theme-revision (1+ rev1))))
              (let ((herdr-sanctum-panel-surface-blend 0.58))
                (herdr-sanctum-apply-theme)
                (should (= herdr-sanctum-theme-revision (+ rev1 2)))))
            ;; `setopt' triggers re-apply / cache clear; promoted variables are `defcustom's.
            (should (custom-variable-p 'herdr-sanctum-palette-overrides))
            (should (custom-variable-p 'herdr-sanctum-panel-surface-blend))
            (should (custom-variable-p 'herdr-sanctum-space-badge-palette))
            (should (custom-variable-p 'herdr-sanctum-theme-change-hook))
            (let ((rev2 herdr-sanctum-theme-revision))
              (setopt herdr-sanctum-panel-surface-blend 0.5)
              (should (= herdr-sanctum-theme-revision (1+ rev2)))
              (setopt herdr-sanctum-palette-overrides '((accent . "#445566")))
              (should (= herdr-sanctum-theme-revision (+ rev2 2))))
            (cl-letf (((symbol-function 'herdr-sanctum-svg-available-p) (lambda (&optional _) t)))
              (herdr-sanctum-space-badge "alpha" "w1")
              (should (> (hash-table-count herdr-sanctum--svg-cache) 0))
              (let ((rev3 herdr-sanctum-theme-revision))
                (setopt herdr-sanctum-space-badge-palette '(("#112233" . "#ffffff")))
                (should (= (hash-table-count herdr-sanctum--svg-cache) 0))
                (should (= herdr-sanctum-theme-revision (1+ rev3))))))
        (set-face-attribute 'default nil :background orig-bg)
        (setopt herdr-sanctum-palette-overrides orig-overrides)
        (setopt herdr-sanctum-panel-surface-blend orig-blend)
        (setopt herdr-sanctum-space-badge-palette orig-palette)
        (herdr-sanctum-apply-theme t)))))

(ert-deftest herdr-sanctum-theme-test/status-glyphs-and-faces ()
  "Dedicated status faces and `status-glyphs' unify status indicators."
  (herdr-sanctum-test-with-clean-state
    (let ((orig-glyphs herdr-sanctum-status-glyphs))
      (unwind-protect
          (progn
            ;; 1. Dedicated faces exist with expected inheritance.
            (dolist (spec '((herdr-sanctum-status-blocked error bold)
                            (herdr-sanctum-status-working warning unspecified)
                            (herdr-sanctum-status-done success unspecified)
                            (herdr-sanctum-status-idle shadow unspecified)))
              (let ((face (nth 0 spec)))
                (should (facep face))
                (should (eq (face-attribute face :inherit nil) (nth 1 spec)))
                (should (eq (face-attribute face :weight nil) (nth 2 spec)))))
            ;; 2. `herdr-sanctum-status-face' maps every status cleanly.
            (should (eq (herdr-sanctum-status-face "blocked") 'herdr-sanctum-status-blocked))
            (should (eq (herdr-sanctum-status-face "working") 'herdr-sanctum-status-working))
            (should (eq (herdr-sanctum-status-face "done") 'herdr-sanctum-status-done))
            (should (eq (herdr-sanctum-status-face "idle") 'herdr-sanctum-status-idle))
            (should (eq (herdr-sanctum-status-face nil) 'herdr-sanctum-status-idle))
            (should (eq (herdr-sanctum-status-face "unknown") 'herdr-sanctum-status-idle))
            ;; 3. `herdr-sanctum-status-glyph' reads `status-glyphs' and applies dedicated faces.
            (dolist (pair '(("blocked" "◆" herdr-sanctum-status-blocked)
                            ("working" "●" herdr-sanctum-status-working)
                            ("done"    "✓" herdr-sanctum-status-done)
                            ("idle"    "○" herdr-sanctum-status-idle)))
              (let ((g (herdr-sanctum-status-glyph (nth 0 pair)))
                    (g-bg (herdr-sanctum-status-glyph (nth 0 pair) "#24283b")))
                (should (equal (substring-no-properties g) (nth 1 pair)))
                (should (eq (get-text-property 0 'face g) (nth 2 pair)))
                (should (equal (get-text-property 0 'face g-bg)
                               (list (nth 2 pair) '(:background "#24283b"))))))
            (should-not (herdr-sanctum-status-glyph nil))
            (should-not (herdr-sanctum-status-glyph "unknown"))
            ;; 4. `setopt' on `herdr-sanctum-status-glyphs' updates glyphs, unicode badges, and hook.
            (let ((hook-calls 0)
                  (hook-fn nil))
              (setq hook-fn (lambda () (cl-incf hook-calls)))
              (unwind-protect
                  (progn
                    (add-hook 'herdr-sanctum-theme-change-hook hook-fn)
                    (setopt herdr-sanctum-status-glyphs
                            '(("blocked" . "!") ("working" . "*") ("done" . "+") ("idle" . "-")))
                    (should (= hook-calls 1))
                    (should (equal (substring-no-properties (herdr-sanctum-status-glyph "blocked")) "!"))
                    (let ((herdr-sanctum-agent-icon-style 'unicode))
                      (let ((badge (herdr-sanctum-agent-badge "pi" "blocked")))
                        (should (equal (substring-no-properties badge) "!"))
                        (should (eq (get-text-property 0 'face badge)
                                    'herdr-sanctum-status-blocked)))
                      (let ((work (herdr-sanctum-agent-badge "pi" "working")))
                        (should (equal (substring-no-properties work) "*"))
                        (should (eq (get-text-property 0 'face work)
                                    'herdr-sanctum-status-working)))))
                (remove-hook 'herdr-sanctum-theme-change-hook hook-fn))))
        (setopt herdr-sanctum-status-glyphs orig-glyphs)))))

(ert-deftest herdr-sanctum-theme-test/agent-brands-absolute-file-and-glyph ()
  "`agent-brands' supports absolute/`~/' `:file' and `:glyph' while blocking traversal."
  (herdr-sanctum-test-with-clean-state
    (herdr-sanctum-test-with-temp-dir tmpdir
      (let ((abs-svg (expand-file-name "brand.svg" tmpdir))
            (tilde-svg (expand-file-name "tilde.svg" tmpdir))
            (non-svg (expand-file-name "notes.txt" tmpdir)))
        (with-temp-file abs-svg
          (insert "<?xml version=\"1.0\"?>\n"
                  "<svg xmlns=\"http://www.w3.org/2000/svg\" viewBox=\"0 0 24 24\" fill=\"currentColor\">"
                  "<polygon points=\"12,1 23,23 1,23\"/></svg>"))
        (with-temp-file tilde-svg
          (insert "<svg xmlns=\"http://www.w3.org/2000/svg\" viewBox=\"0 0 24 24\" fill=\"currentColor\">"
                  "<ellipse cx=\"12\" cy=\"12\" rx=\"9\" ry=\"5\"/></svg>"))
        (with-temp-file non-svg
          (insert "<svg></svg>"))
        (cl-letf (((symbol-function 'herdr-sanctum-svg-available-p) (lambda (&optional _) t)))
          ;; 1. Absolute `:file' loads even when `herdr-sanctum-agent-icon-directory' is nil.
          (clrhash herdr-sanctum--svg-cache)
          (clrhash herdr-sanctum--svg-file-cache)
          (let ((herdr-sanctum-agent-icon-directory nil)
                (herdr-sanctum-agent-brands `(("custom" . (:file ,abs-svg :color "#336699")))))
            (let* ((badge (herdr-sanctum-agent-badge "custom" "working"))
                   (data (plist-get (cdr (get-text-property 0 'display badge)) :data)))
              (should (string-search "<polygon " data))
              (should (string-search "#336699" data))
              (should-not (string-search "<?xml" data))))
          ;; Dynamically rebinding `:file' produces a distinct SVG cache entry.
          (let ((herdr-sanctum-agent-icon-directory nil)
                (herdr-sanctum-agent-brands `(("custom" . (:file ,tilde-svg :color "#336699")))))
            (let* ((badge (herdr-sanctum-agent-badge "custom" "working"))
                   (data (plist-get (cdr (get-text-property 0 'display badge)) :data)))
              (should (string-search "<ellipse " data))))
          ;; 2. `~/'-prefixed `:file' expands against HOME and loads cleanly.
          (let ((process-environment (cons (concat "HOME=" tmpdir) process-environment))
                (herdr-sanctum-agent-icon-directory nil)
                (herdr-sanctum-agent-brands '(("tilde-agent" . (:file "~/tilde.svg")))))
            (let* ((badge (herdr-sanctum-agent-badge "tilde-agent" "working"))
                   (data (plist-get (cdr (get-text-property 0 'display badge)) :data)))
              (should (string-search "<ellipse " data))))
          ;; 3. Security: reject `..' segments in absolute/`~/' `:file', non-`.svg' files, and unconfigured abs canon.
          (let ((herdr-sanctum-agent-brands
                 `(("trav-abs"   . (:file ,(concat tmpdir "/sub/../brand.svg")))
                   ("trav-tilde" . (:file "~/../tilde.svg"))
                   ("trav-rel"   . (:file "../brand"))
                   ("bad-ext"    . (:file ,non-svg)))))
            (should-not (herdr-sanctum--agent-svg-file-raw "trav-abs"))
            (should-not (herdr-sanctum--agent-svg-file-raw "trav-tilde"))
            (should-not (herdr-sanctum--agent-svg-file-raw "trav-rel"))
            (should-not (herdr-sanctum--agent-svg-file-raw "bad-ext"))
            (should-not (herdr-sanctum--agent-svg-file-raw abs-svg)))
          ;; 4. Optional `:glyph' is used in `unicode' mode, falling back to status glyph when absent.
          (let ((herdr-sanctum-agent-icon-style 'unicode)
                (herdr-sanctum-agent-brands '(("pi" . (:glyph "π"))
                                              ("codex" . (:color accent)))))
            (let ((pi-b (herdr-sanctum-agent-badge "pi" "blocked"))
                  (pi-bg (herdr-sanctum-agent-badge "pi" "done" "#24283b"))
                  (cx-b (herdr-sanctum-agent-badge "codex" "blocked")))
              (should (equal (substring-no-properties pi-b) "π"))
              (should (eq (get-text-property 0 'face pi-b) 'herdr-sanctum-status-blocked))
              (should-not (get-text-property 0 'display pi-b))
              (should (equal (substring-no-properties pi-bg) "π"))
              (should (equal (get-text-property 0 'face pi-bg)
                             '(herdr-sanctum-status-done (:background "#24283b"))))
              (should (equal (substring-no-properties cx-b) "◆"))
              (should (eq (get-text-property 0 'face cx-b) 'herdr-sanctum-status-blocked)))))))))

(ert-deftest herdr-sanctum-theme-test/tty-vs-graphic-chrome-bar-underline ()
  "`chrome-bar' omits underline on TTY while `drawer-tab-active' keeps underline."
  (herdr-sanctum-test-with-clean-state
    (herdr-sanctum-apply-theme t)
    (let* ((chrome-spec (get 'herdr-sanctum-chrome-bar 'face-defface-spec))
           (chrome-gfx (cdr (assoc '((type graphic)) chrome-spec)))
           (chrome-tty (cdr (assq t chrome-spec)))
           (tab-spec (get 'herdr-sanctum-drawer-tab-active 'face-defface-spec))
           (tab-gfx (cdr (assoc '((type graphic)) tab-spec)))
           (tab-tty (cdr (assq t tab-spec))))
      ;; Graphic clauses carry positioned border/accent underlines.
      (should (eq (plist-get (plist-get chrome-gfx :underline) :position) t))
      (should (herdr-sanctum--valid-color-p
               (plist-get (plist-get chrome-gfx :underline) :color)))
      (should (eq (plist-get (plist-get tab-gfx :underline) :position) t))
      (should (herdr-sanctum--valid-color-p
               (plist-get (plist-get tab-gfx :underline) :color)))
      ;; TTY fallback clauses suppress chrome-bar underline and keep active tab underline.
      (should-not (plist-get chrome-tty :underline))
      (should (eq (plist-get tab-tty :underline) t))
      ;; In `--batch' (non-graphic TTY), resolved face attributes match TTY clarity rules.
      (should-not (face-attribute 'herdr-sanctum-chrome-bar :underline nil))
      (should-not (face-attribute 'herdr-sanctum-drawer-tab :underline nil t))
      (should (eq (face-attribute 'herdr-sanctum-drawer-tab-active :underline nil) t))
      ;; Display-matcher & `face-spec-choose' select the TTY clause on non-graphic frames.
      (should-not (face-spec-set-match-display '((type graphic)) (selected-frame)))
      (should-not (plist-get (face-spec-choose chrome-spec (selected-frame)) :underline))
      (should (eq (plist-get (face-spec-choose tab-spec (selected-frame)) :underline) t)))))

(provide 'herdr-sanctum-theme-test)
;;; herdr-sanctum-theme-test.el ends here
