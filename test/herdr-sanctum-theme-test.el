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
          (let ((modus-bg (modus-themes-get-color-value 'bg-tab-bar t))
                (resolved (herdr-sanctum-palette-color 'bg-chrome)))
            (should (herdr-sanctum--valid-color-p modus-bg))
            (should (equal resolved modus-bg)))
          ;; Disabling `modus-vivendi' causes `modus-themes-get-color-value' to return
          ;; `'unspecified'; `disable-theme' hook and palette resolution must fall back cleanly.
          (disable-theme 'modus-vivendi)
          (should (eq (modus-themes-get-color-value 'bg-tab-bar t) 'unspecified))
          (let* ((bg (herdr-sanctum-palette-color 'bg-chrome))
                 (spec (get 'herdr-sanctum-chrome-bar 'face-defface-spec))
                 (attrs (cdr (assq t spec))))
            (should (herdr-sanctum--valid-color-p bg))
            (should (herdr-sanctum--valid-color-p (plist-get attrs :background)))
            (should (herdr-sanctum--valid-color-p
                     (plist-get (plist-get attrs :underline) :color))))
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
    ;; Chrome face specs emit a single clean `(t ...)' clause.
    (let* ((spec (get 'herdr-sanctum-chrome-bar 'face-defface-spec))
           (attrs (cdr (assq t spec))))
      (should (= (length spec) 1))
      (should (equal (plist-get attrs :height) herdr-sanctum-ui-font-height))
      (should (herdr-sanctum--valid-color-p (plist-get attrs :background))))))

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
      ;; 3. Cache key distinguishes `dot-color', `cutout-bg', and `agent-icon-directory'.
      (clrhash herdr-sanctum--svg-cache)
      (let* ((dot-red "#ff1111")
             (chrome-bg "#111111")
             (herdr-sanctum-palette-function
              (lambda (tok _fb)
                (pcase tok
                  ('accent "#82aaff")
                  ('red dot-red)
                  ('bg-chrome chrome-bg)
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
        (setq chrome-bg "#222222")
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

(provide 'herdr-sanctum-theme-test)
;;; herdr-sanctum-theme-test.el ends here
