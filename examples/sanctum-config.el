;;; sanctum-config.el --- Polished Emacs UI configuration example for herdr-sanctum -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Reference configuration demonstrating a cohesive, high-contrast three-pane
;; workbench aesthetic for `herdr-sanctum' across both dark and light themes:
;;
;;   - Built-in tonal surface separation (`herdr-sanctum-panel-surface-blend')
;;     and comfortable panel row spacing (`herdr-sanctum-panel-line-spacing')
;;     for `*sanctum-spaces*' and `*sanctum-drawer*' framing `*sanctum*'.
;;   - Refined `modus-themes' overrides (`modus-vivendi-tinted' /
;;     `modus-operandi-tinted'), flat `tab-bar' / `mode-line', and subtle
;;     1px `window-divider'.
;;   - Soft integration with optional packages (`spacious-padding',
;;     `nerd-icons', `vertico', `orderless', `ghostel').
;;   - Keybindings and optional companion modules (`herdr-sanctum-ime',
;;     `herdr-sanctum-tab-routing').
;;
;; Copy the sections you like into your `init.el', or load this file directly
;; when experimenting in `emacs -Q'.

;;; Code:

(require 'herdr-sanctum)
(require 'herdr-sanctum-ime)
(require 'herdr-sanctum-tab-routing)

;;;; 1. Typography (adjust to your preferred fonts)

(defun sanctum-example-setup-fonts ()
  "Configure crisp monospace, UI sans-serif, and CJK fallback fonts."
  (when (display-graphic-p)
    (let ((mono (seq-find (lambda (f) (find-font (font-spec :name f)))
                          '("MonoLisa" "Aporetic Sans Mono" "JetBrains Mono"
                            "SF Mono" "Iosevka" "Menlo")))
          (sans (seq-find (lambda (f) (find-font (font-spec :name f)))
                          '("SF Pro Text" "Inter" "Helvetica Neue")))
          (cjk  (seq-find (lambda (f) (find-font (font-spec :name f)))
                          '("LXGW WenKai Mono" "PingFang SC" "Noto Sans CJK SC"))))
      (when mono
        (set-face-attribute 'default nil :family mono :height 130)
        (set-face-attribute 'fixed-pitch nil :family mono :height 130))
      (when sans
        (set-face-attribute 'variable-pitch nil :family sans :height 125))
      (when cjk
        (set-fontset-font t 'han (font-spec :family cjk))))))

;;;; 2. Theme & Cohesive Chrome Palette (`modus-themes')

;; Soften borders and align chrome surfaces across dark and light Modus themes.
(setq modus-themes-italic-constructs t
      modus-themes-bold-constructs t
      modus-themes-mixed-fonts t
      modus-themes-common-palette-overrides
      '((border-mode-line-active unspecified)
        (border-mode-line-inactive unspecified)
        (fringe unspecified))
      modus-operandi-tinted-palette-overrides
      '((bg-dim "#eae3d6")
        (bg-inactive "#e3dbcc")
        (bg-active "#d2c8b6")
        (bg-tab-bar "#eae3d6")
        (bg-tab-current "#fbf7f0")
        (bg-tab-other "#e2dacb")
        (bg-mode-line-active "#eae3d6")
        (bg-mode-line-inactive "#f3eee4")
        (border "#d5ccbc"))
      modus-vivendi-tinted-palette-overrides
      '((bg-dim "#151828")
        (bg-inactive "#1c2136")
        (bg-active "#2b314d")
        (bg-tab-bar "#151828")
        (bg-tab-current "#0d0e1c")
        (bg-tab-other "#111422")
        (bg-mode-line-active "#151828")
        (bg-mode-line-inactive "#111322")
        (border "#252a40")))

;;;; 3. Chrome Polish (Dividers, Tab-Bar & Mode-Line)

(defun sanctum-example-apply-chrome-polish ()
  "Harmonize window dividers, tab-bar, and mode-line with Sanctum."
  (let ((border (herdr-sanctum-palette-color 'border))
        (bg-chrome (herdr-sanctum-palette-color 'bg-chrome))
        (bg-main (let ((bg (face-background 'default nil t)))
                   (if (and (stringp bg)
                            (not (string-prefix-p "unspecified" bg)))
                       bg
                     (herdr-sanctum-palette-color 'bg-panel "#10121e"))))
        (fg-dim (herdr-sanctum-palette-color 'fg-dim)))
    (set-face-attribute 'window-divider nil :foreground border)
    (set-face-attribute 'window-divider-first-pixel nil :foreground border)
    (set-face-attribute 'window-divider-last-pixel nil :foreground border)
    (set-face-attribute 'mode-line-active nil
                        :background bg-chrome
                        :overline border
                        :box `(:line-width (-1 . 3) :color ,bg-chrome))
    (set-face-attribute 'mode-line-inactive nil
                        :background bg-main
                        :foreground fg-dim
                        :overline border
                        :box `(:line-width (-1 . 3) :color ,bg-main))
    (set-face-attribute 'tab-bar nil
                        :background bg-chrome
                        :box `(:line-width 4 :color ,bg-chrome))
    (set-face-attribute 'tab-bar-tab nil
                        :background bg-main
                        :box `(:line-width 4 :color ,bg-main)
                        :weight 'semi-bold)
    (set-face-attribute 'tab-bar-tab-inactive nil
                        :background bg-chrome
                        :foreground fg-dim
                        :box `(:line-width 4 :color ,bg-chrome))))

(add-hook 'herdr-sanctum-theme-change-hook #'sanctum-example-apply-chrome-polish)

;;;; 4. Clean Mode-Line with Pill IME Badge & Global Attention Counter

(defun sanctum-example--ime-badge ()
  "Render a subtle pill indicator for the active input method or ASCII."
  (let* ((active current-input-method)
         (bg-pill (if active
                      (herdr-sanctum-palette-color 'bg-pill-active)
                    (herdr-sanctum-palette-color 'bg-card)))
         (fg-pill (if active
                      (herdr-sanctum-palette-color 'yellow)
                    (herdr-sanctum-palette-color 'fg-dim)))
         (label (if active (format " %s " current-input-method-title) " EN ")))
    (propertize label
                'face `(:background ,bg-pill
                        :foreground ,fg-pill
                        :weight ,(if active 'bold 'medium)))))

(defun sanctum-example-setup-mode-line ()
  "Install a quiet, structured mode-line tailored for Sanctum."
  (setq-default mode-line-format
                '("%e"
                  " "
                  (:eval (sanctum-example--ime-badge))
                  "  "
                  mode-line-buffer-identification
                  "   "
                  (:eval (propertize (format-mode-line mode-name) 'face 'shadow))
                  (:eval (or (herdr-sanctum-mode-line) ""))
                  mode-line-end-spaces)))

(defun sanctum-example--sync-terminal-mode-line ()
  "Show the minimal mode-line in `*sanctum*' while `herdr-sanctum-ime-mode' is active."
  (when-let* ((buf (get-buffer herdr-sanctum-buffer-name)))
    (with-current-buffer buf
      (setq-local mode-line-format
                  (when (bound-and-true-p herdr-sanctum-ime-mode)
                    (default-value 'mode-line-format))))
    (force-mode-line-update t)))

(add-hook 'herdr-sanctum-ime-mode-hook #'sanctum-example--sync-terminal-mode-line)
(add-hook 'herdr-sanctum-terminal-start-hook #'sanctum-example--sync-terminal-mode-line)

;;;; 5. Core `herdr-sanctum' Settings & Soft Integrations

;; Use `setopt' for `herdr-sanctum' options so custom `:set' handlers apply
;; tonal surface blending, panel line-spacing, and fringes live.
(setopt herdr-sanctum-display-mode 'tab
        herdr-sanctum-tab-name "Sanctum"
        herdr-sanctum-sidebar-width 31
        herdr-sanctum-drawer-width 35
        herdr-sanctum-sidebar-default-open t
        herdr-sanctum-drawer-default-open t
        herdr-sanctum-ui-font-height 0.94
        herdr-sanctum-panel-surface-blend 0.58
        herdr-sanctum-panel-line-spacing 0.22
        herdr-sanctum-panel-fringe-width 14
        herdr-sanctum-terminal-line-spacing 0.08)

(setq tab-bar-show 1
      tab-bar-close-button-show nil
      tab-bar-new-button-show nil
      window-divider-default-places 'right-only
      window-divider-default-right-width 1
      window-divider-default-bottom-width 0)

(window-divider-mode 1)

;; Prevent Ghostel's internal cursor-blink timer from colliding with deleted
;; side windows when Emacs manages cursor style directly.
(setq ghostel-ignore-cursor-change t)

(when (require 'spacious-padding nil t)
  (setq spacious-padding-widths
        '(:internal-border-width 8
          :header-line-width 5
          :mode-line-width 5
          :tab-width 4
          :right-divider-width 1
          :scroll-bar-width 0
          :fringe-width 10)
        spacious-padding-subtle-mode-line nil)
  (spacious-padding-mode 1))

(when (require 'vertico nil t)
  (setq vertico-count 8
        vertico-resize nil
        vertico-cycle t)
  (vertico-mode 1))

(when (require 'orderless nil t)
  (setq completion-styles '(orderless basic)
        completion-category-overrides '((file (styles basic partial-completion)))))

(require 'nerd-icons nil t)

;;;; 6. Keybindings & Companion Modules

(keymap-global-set "C-c h" #'herdr-sanctum)
(keymap-global-set "C-c H" #'herdr-sanctum-sidebar-toggle)
(keymap-global-set "C-c D" #'herdr-sanctum-drawer-toggle)
(keymap-global-set "C-c J" #'herdr-sanctum-jump)
(keymap-global-set "C-c s" herdr-sanctum-command-map)

;; Optional: automatic per-pane IME switching (e.g. "rime", "pyim", or built-in "chinese-py").
(setopt herdr-sanctum-ime-input-method "chinese-py")

;; Optional: keep `*sanctum*' unsplit by routing opened files/tools into tabs.
;; Unmatched buffers (such as `*herdr-sanctum-doctor*') automatically route via
;; `herdr-sanctum-tab-routing-fallback-tab' (default `recent').
(setopt herdr-sanctum-tab-routing-rules
        '(("Atlas" :modes (org-agenda-mode))
          ("Bench" :commands (eshell) :modes (proced-mode))
          ("Folio" :file t :commands (consult-buffer)
                   :modes (prog-mode text-mode diff-mode vc-dir-mode
                           magit-mode dired-mode))))

;; Enable companion modules in your `init.el' when ready:
;; (herdr-sanctum-ime-mode 1)
;; (herdr-sanctum-tab-routing-mode 1)

(provide 'sanctum-config)
;;; sanctum-config.el ends here
