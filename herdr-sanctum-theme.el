;;; herdr-sanctum-theme.el --- Faces, palette, and SVG icons for herdr-sanctum -*- lexical-binding: t; -*-

;; Copyright (C) 2026 oatiz

;; Author: oatiz <iamoatiz@gmail.com>
;; URL: https://github.com/oatiz/herdr-sanctum
;; Keywords: faces, tools

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

;; Pure theme layer for `herdr-sanctum': semantic palette token resolution
;; (Modus, Ef, and generic dark/light color derivation), palette-driven chrome
;; faces, and inline SVG/Unicode badges for spaces and coding agents.

;;; Code:

(require 'cl-lib)
(require 'color)
(eval-when-compile (require 'let-alist))
(require 'seq)
(require 'subr-x)

(declare-function ef-themes-get-color-value "ef-themes" (color &optional overrides theme))
(declare-function modus-themes-get-color-value "modus-themes" (color &optional overrides theme))
(declare-function svg-create "svg" (width height &rest args))
(declare-function svg-circle "svg" (svg x y radius &rest args))
(declare-function svg-image "svg" (svg &rest props))
(declare-function svg-rectangle "svg" (svg x y width height &rest args))
(declare-function svg-text "svg" (svg text &rest args))

(defgroup herdr-sanctum-theme nil
  "Faces, palette derivation, and SVG icons for `herdr-sanctum'."
  :group 'herdr-sanctum
  :prefix "herdr-sanctum-")

(defvar herdr-sanctum-theme-revision 0
  "Monotonic revision incremented whenever Sanctum theme faces or icons change.")

(defcustom herdr-sanctum-theme-change-hook nil
  "Hook run after theme application or icon options update Sanctum visuals."
  :type 'hook
  :group 'herdr-sanctum-theme)

(defvar herdr-sanctum--svg-cache (make-hash-table :test #'equal)
  "Cache of rendered inline SVG images for space monograms and agent glyphs.")

(defvar herdr-sanctum--svg-file-cache (make-hash-table :test #'equal)
  "Cache of raw SVG strings loaded from `herdr-sanctum-agent-icon-directory'.")

(defvar herdr-sanctum--palette-cache (make-hash-table :test #'equal)
  "Cache of resolved default palette colors keyed by `(BG-MODE . TOKEN)'.")

(defvar herdr-sanctum--last-theme-state nil
  "Last theme state snapshot applied by `herdr-sanctum-apply-theme'.")

(defun herdr-sanctum--set-theme-option (sym val)
  "Set visual option SYM to VAL and re-apply Sanctum faces when loaded."
  (set-default sym val)
  (when (and (featurep 'herdr-sanctum-theme)
             (fboundp 'herdr-sanctum-apply-theme))
    (herdr-sanctum-apply-theme t)))

(defun herdr-sanctum--set-icon-option (sym val)
  "Set icon option SYM to VAL, clear SVG caches, and notify panels."
  (set-default sym val)
  (when (boundp 'herdr-sanctum--svg-cache)
    (clrhash herdr-sanctum--svg-cache))
  (when (boundp 'herdr-sanctum--svg-file-cache)
    (clrhash herdr-sanctum--svg-file-cache))
  (when (featurep 'herdr-sanctum-theme)
    (cl-incf herdr-sanctum-theme-revision)
    (run-hooks 'herdr-sanctum-theme-change-hook)))

(defcustom herdr-sanctum-palette-function #'herdr-sanctum-default-palette-color
  "Function `(TOKEN FALLBACK)' resolving semantic palette colors.
Supported tokens include `bg-chrome', `bg-panel', `bg-card', `bg-hl',
`bg-selected', `bg-pill-active', `border', `guide', `accent', `fg-main',
`fg-dim', `fg-alt', `yellow', `red', and `green'."
  :type 'function
  :group 'herdr-sanctum-theme
  :set #'herdr-sanctum--set-theme-option)

(defcustom herdr-sanctum-palette-overrides nil
  "Alist of `(TOKEN . COLOR)' overrides taking precedence over palette lookup.
Each TOKEN is a semantic palette symbol such as `bg-chrome', `bg-panel',
`accent', or `border', and COLOR is a hex color string or color name."
  :type '(alist :key-type symbol :value-type string)
  :group 'herdr-sanctum-theme
  :set #'herdr-sanctum--set-theme-option)

(defcustom herdr-sanctum-panel-surface-blend nil
  "Blend factor for `bg-panel' between `default' background and `bg-chrome'.
When nil, `bg-panel' matches `bg-chrome'.  When a number in 0.0..1.0,
`bg-panel' blends `default' background toward `bg-chrome' by this factor
\(for example 0.58 blends 58% `bg-chrome' with 42% `default' background)."
  :type '(choice (const :tag "Same as chrome bar" nil)
                 (float :tag "Blend factor from default toward bg-chrome (0.0-1.0)"))
  :group 'herdr-sanctum-theme
  :set #'herdr-sanctum--set-theme-option)

(defcustom herdr-sanctum-ui-font nil
  "Font family used for Sanctum graphical chrome (sidebar, drawer, top bar).
When nil, inherit `variable-pitch' if it resolves to a concrete font family.
Set to symbol `default' to keep the frame's default monospace font, or a
font family name string."
  :type '(choice (const :tag "Auto (variable-pitch)" nil)
                 (const :tag "Default monospace" default)
                 (string :tag "Font family"))
  :group 'herdr-sanctum-theme
  :set #'herdr-sanctum--set-theme-option)

(defcustom herdr-sanctum-ui-font-height 0.9
  "Height for Sanctum chrome UI text.
May be a floating-point scale relative to `default' (for example 0.9) or an
integer in 1/10pt units (for example 110)."
  :type '(choice (float :tag "Relative scale")
                 (integer :tag "Absolute 1/10pt"))
  :group 'herdr-sanctum-theme
  :set #'herdr-sanctum--set-theme-option)

(defcustom herdr-sanctum-space-badge-style 'svg
  "Rendering style for Space monogram badges in the sidebar and drawer."
  :type '(choice (const :tag "Inline SVG monogram" svg)
                 (const :tag "Text monogram" text)
                 (const :tag "None" none))
  :group 'herdr-sanctum-theme
  :set #'herdr-sanctum--set-icon-option)

(defcustom herdr-sanctum-space-badge-palette
  '(("#2a6b68" . "#e4f7f5")
    ("#8a5f2f" . "#fdf1e0")
    ("#5458a6" . "#eceeff")
    ("#3a7852" . "#e4f7ea")
    ("#944568" . "#fde8f1")
    ("#3a639c" . "#e8f1ff"))
  "Muted `(BG . FG)' color pairs used for Space monogram badges."
  :type '(repeat (cons (string :tag "Background") (string :tag "Foreground")))
  :group 'herdr-sanctum-theme
  :set #'herdr-sanctum--set-icon-option)

(defcustom herdr-sanctum-agent-icon-style 'svg-files
  "Icon rendering backend for Herdr coding-agent rows.
`svg-files' loads static SVGs from `herdr-sanctum-agent-icon-directory' with
automatic fallback to a 1-letter SVG monogram; `unicode' always uses plain
Unicode glyphs."
  :type '(choice (const :tag "Static SVG files" svg-files)
                 (const :tag "Plain Unicode glyphs" unicode))
  :group 'herdr-sanctum-theme
  :set #'herdr-sanctum--set-icon-option)

(defcustom herdr-sanctum-agent-icon-directory
  (let* ((src (or load-file-name (locate-library "herdr-sanctum-theme")))
         (base (and src (file-name-directory (file-truename src)))))
    (and base (expand-file-name "assets/agents" base)))
  "Directory containing `<agent>.svg' static icons for `svg-files' style."
  :type '(choice (const :tag "None" nil) directory)
  :group 'herdr-sanctum-theme
  :set #'herdr-sanctum--set-icon-option)

(defcustom herdr-sanctum-agent-brands
  '(("pi"             . (:color accent))
    ("claude-code"    . (:file "claude"))
    ("qwen-code"      . (:file "qwen"))
    ("github-copilot" . (:file "copilot")))
  "Alist mapping canonical agent names to brand tint, SVG, and glyph metadata.
Each entry is `(NAME . PLIST)' where PLIST supports:
  `:color' -- semantic palette token symbol or hex color string (used to tint
              `currentColor' SVGs and fallback monograms; defaults to `fg-alt')
  `:file'  -- alternate `<file>.svg' stem in the agent icon directory, or an
              explicit absolute or `~/' path to an `.svg' file
  `:glyph' -- optional Unicode glyph string used in `unicode' icon mode
              (defaults to the glyph from `herdr-sanctum-status-glyphs')."
  :type '(alist :key-type string :value-type plist)
  :group 'herdr-sanctum-theme
  :set #'herdr-sanctum--set-icon-option)

(defcustom herdr-sanctum-status-glyphs
  '(("blocked" . "◆")
    ("working" . "●")
    ("done"    . "✓")
    ("idle"    . "○"))
  "Alist mapping agent status strings to single-column Unicode glyphs.
Recognized status keys are \"blocked\", \"working\", \"done\", and \"idle\"."
  :type '(alist :key-type string :value-type string)
  :group 'herdr-sanctum-theme
  :set #'herdr-sanctum--set-icon-option)

;;; Faces

;; Semantic foreground faces stay purely declarative via `:inherit' so they
;; track active themes across dark, light, and TTY frames automatically.

(defface herdr-sanctum-sidebar-header
  '((t :inherit shadow :weight semi-bold))
  "Face for section headers in Sanctum sidebars and drawers."
  :group 'herdr-sanctum-theme)

(defface herdr-sanctum-sidebar-group-working
  '((t :inherit warning :weight semi-bold))
  "Face for the `Working' agent group header."
  :group 'herdr-sanctum-theme)

(defface herdr-sanctum-sidebar-group-blocked
  '((t :inherit error :weight semi-bold))
  "Face for the `Blocked' agent group header."
  :group 'herdr-sanctum-theme)

(defface herdr-sanctum-sidebar-group-done
  '((t :inherit success :weight semi-bold))
  "Face for the `Done' agent group header."
  :group 'herdr-sanctum-theme)

(defface herdr-sanctum-status-blocked
  '((t :inherit error :weight bold))
  "Face for blocked agent status indicators."
  :group 'herdr-sanctum-theme)

(defface herdr-sanctum-status-working
  '((t :inherit warning))
  "Face for working agent status indicators."
  :group 'herdr-sanctum-theme)

(defface herdr-sanctum-status-done
  '((t :inherit success))
  "Face for completed agent status indicators."
  :group 'herdr-sanctum-theme)

(defface herdr-sanctum-status-idle
  '((t :inherit shadow))
  "Face for idle agent status indicators."
  :group 'herdr-sanctum-theme)

(defface herdr-sanctum-sidebar-space
  '((t :inherit default :weight medium))
  "Face for space labels in `*sanctum-spaces*'."
  :group 'herdr-sanctum-theme)

(defface herdr-sanctum-sidebar-space-expanded
  '((t :inherit default :weight semi-bold))
  "Face for an expanded focused space row when a child tab holds focus."
  :group 'herdr-sanctum-theme)

(defface herdr-sanctum-sidebar-tab
  '((t :inherit default))
  "Face for tab labels in the focused space."
  :group 'herdr-sanctum-theme)

(defface herdr-sanctum-sidebar-meta
  '((t :inherit shadow))
  "Face for right-aligned Git branches, counts, and durations."
  :group 'herdr-sanctum-theme)

(defface herdr-sanctum-sidebar-tab-dim
  '((t :inherit herdr-sanctum-sidebar-meta))
  "Face for inactive tab labels in non-focused spaces."
  :group 'herdr-sanctum-theme)

(defface herdr-sanctum-sidebar-tab-ws-active
  '((t :inherit font-lock-doc-face))
  "Face for a non-focused space's active tab."
  :group 'herdr-sanctum-theme)

(defface herdr-sanctum-sidebar-tab-active
  '((t :inherit herdr-sanctum-sidebar-space-active))
  "Face for the globally focused tab or agent row."
  :group 'herdr-sanctum-theme)

(defface herdr-sanctum-sidebar-git-behind
  '((t :inherit error :weight medium))
  "Face for behind-remote (`↓N') Git indicators."
  :group 'herdr-sanctum-theme)

(defface herdr-sanctum-sidebar-git-ahead
  '((t :inherit font-lock-function-name-face :weight medium))
  "Face for ahead-of-remote (`↑N') Git indicators."
  :group 'herdr-sanctum-theme)

(defface herdr-sanctum-drawer-tab nil
  "Inactive view tab face in `*sanctum-drawer*'."
  :group 'herdr-sanctum-theme)

;; Palette-driven chrome and surface faces populated by `herdr-sanctum-apply-theme'.

(defface herdr-sanctum-sidebar-space-active nil
  "Face for the focused space row in `*sanctum-spaces*'."
  :group 'herdr-sanctum-theme)

(defface herdr-sanctum-sidebar-guide nil
  "Face for vertical tree guides and section dividers."
  :group 'herdr-sanctum-theme)

(defface herdr-sanctum-chrome-bar nil
  "Shared header-line bar face across Sanctum panes."
  :group 'herdr-sanctum-theme)

(defface herdr-sanctum-chrome-pill nil
  "Clickable header-line button face for Sanctum chrome bars."
  :group 'herdr-sanctum-theme)

(defface herdr-sanctum-chrome-pill-active nil
  "Active header-line button pill face for Sanctum chrome bars."
  :group 'herdr-sanctum-theme)

(defface herdr-sanctum-chrome-sep nil
  "Subtle separator face for dividing control groups in Sanctum header-lines."
  :group 'herdr-sanctum-theme)

(defface herdr-sanctum-status-bar nil
  "Bottom status strip face for Sanctum sidebars and drawers."
  :group 'herdr-sanctum-theme)

(defface herdr-sanctum-drawer-tab-active nil
  "Active view tab face in `*sanctum-drawer*'."
  :group 'herdr-sanctum-theme)

(defface herdr-sanctum-drawer-card nil
  "Elevated card background face in `*sanctum-drawer*'."
  :group 'herdr-sanctum-theme)

(defface herdr-sanctum-drawer-button nil
  "Compact action button pill face in `*sanctum-drawer*'."
  :group 'herdr-sanctum-theme)

(defface herdr-sanctum-drawer-button-focus nil
  "Focused inline action button pill face in `*sanctum-drawer*'."
  :group 'herdr-sanctum-theme)

(defface herdr-sanctum-panel-focus-active nil
  "Face highlighting an active row while its Sanctum side panel has focus."
  :group 'herdr-sanctum-theme)

(defface herdr-sanctum-banner-info nil
  "Header-line banner face for terminal read-only or info modes."
  :group 'herdr-sanctum-theme)

(defface herdr-sanctum-banner-warn nil
  "Header-line banner face for raw character passthrough mode."
  :group 'herdr-sanctum-theme)

;;; Palette Derivation (Modus / Ef / Generic Light & Dark)

(defconst herdr-sanctum--modus-token-map
  '((bg-chrome      . bg-dim)
    (bg-card        . bg-inactive)
    (bg-hl          . bg-hl-line)
    (bg-selected    . bg-inactive)
    (bg-pill-active . bg-completion)
    (border         . bg-active)
    (guide          . bg-active)
    (accent         . blue)
    (fg-main        . fg-main)
    (fg-dim         . fg-dim)
    (fg-alt         . fg-alt)
    (yellow         . yellow-warmer)
    (red            . red-intense)
    (green          . green))
  "Mapping from Sanctum semantic tokens to Modus/Ef palette symbols.")

(defconst herdr-sanctum--dark-fallback-palette
  '((bg-chrome      . "#161824")
    (bg-panel       . "#161824")
    (bg-card        . "#1e2234")
    (bg-hl          . "#161a2b")
    (bg-selected    . "#24283b")
    (bg-pill-active . "#263047")
    (border         . "#1e2235")
    (guide          . "#2c3048")
    (accent         . "#82aaff")
    (fg-main        . "#c8ccd8")
    (fg-dim         . "#868ca3")
    (fg-alt         . "#a6b2d4")
    (yellow         . "#dbb47b")
    (red            . "#e67e90")
    (green          . "#7ec495"))
  "Default dark palette used when theme colors cannot be queried.")

(defconst herdr-sanctum--light-fallback-palette
  '((bg-chrome      . "#eceff4")
    (bg-panel       . "#eceff4")
    (bg-card        . "#e2e7f0")
    (bg-hl          . "#e2e6ef")
    (bg-selected    . "#d8dee9")
    (bg-pill-active . "#cfd8ea")
    (border         . "#cbd2df")
    (guide          . "#bfc7d5")
    (accent         . "#3b6ea8")
    (fg-main        . "#2e3440")
    (fg-dim         . "#677084")
    (fg-alt         . "#4c566a")
    (yellow         . "#9a6700")
    (red            . "#c93b4b")
    (green          . "#2f7d4d"))
  "Default light palette used when theme colors cannot be queried.")

(defun herdr-sanctum--valid-color-p (color)
  "Return non-nil when COLOR is a concrete color string usable by `color.el'."
  (and (stringp color)
       (not (string-empty-p color))
       (not (string-prefix-p "unspecified" color))
       (or (string-match-p "\\`#[0-9a-fA-F]\\{6\\}\\'" color)
           (color-name-to-rgb color))))

(defun herdr-sanctum--color-to-rgb (color)
  "Convert hex or named COLOR to an `(R G B)' list of floats in [0.0, 1.0]."
  (if (and (stringp color)
           (string-match "\\`#\\([0-9a-fA-F]\\{2\\}\\)\\([0-9a-fA-F]\\{2\\}\\)\\([0-9a-fA-F]\\{2\\}\\)\\'"
                         color))
      (list (/ (string-to-number (match-string 1 color) 16) 255.0)
            (/ (string-to-number (match-string 2 color) 16) 255.0)
            (/ (string-to-number (match-string 3 color) 16) 255.0))
    (color-name-to-rgb color)))

(defun herdr-sanctum--blend (color1 color2 alpha)
  "Blend COLOR1 toward COLOR2 by ALPHA (0.0 = COLOR1, 1.0 = COLOR2)."
  (when-let* ((rgb1 (herdr-sanctum--color-to-rgb color1))
              (rgb2 (herdr-sanctum--color-to-rgb color2)))
    (apply #'color-rgb-to-hex
           (append (cl-mapcar (lambda (a b) (+ (* (- 1.0 alpha) a) (* alpha b)))
                              rgb1 rgb2)
                   '(2)))))

(defun herdr-sanctum--face-color (face attr)
  "Return concrete color for FACE's ATTR (`:foreground' or `:background'), or nil."
  (when (facep face)
    (let ((val (face-attribute face attr nil 'default)))
      (when (herdr-sanctum--valid-color-p val)
        val))))

(defun herdr-sanctum--background-mode ()
  "Return `light' or `dark' for the active frame."
  (let ((mode (frame-parameter nil 'background-mode)))
    (if (memq mode '(light dark))
        mode
      (let ((bg (herdr-sanctum--face-color 'default :background)))
        (if-let* ((rgb (and bg (herdr-sanctum--color-to-rgb bg))))
            (let ((lum (+ (* 0.2126 (nth 0 rgb))
                          (* 0.7152 (nth 1 rgb))
                          (* 0.0722 (nth 2 rgb)))))
              (if (> lum 0.5) 'light 'dark))
          'dark)))))

(defun herdr-sanctum--derive-generic-color (token)
  "Derive a harmonious color for TOKEN from standard Emacs faces."
  (let* ((light-p (eq (herdr-sanctum--background-mode) 'light))
         (fallback-tbl (if light-p
                           herdr-sanctum--light-fallback-palette
                         herdr-sanctum--dark-fallback-palette))
         (bg (or (herdr-sanctum--face-color 'default :background)
                 (if light-p "#ffffff" "#0d0e1c")))
         (fg (or (herdr-sanctum--face-color 'default :foreground)
                 (alist-get 'fg-main fallback-tbl)))
         (accent (or (herdr-sanctum--face-color 'font-lock-function-name-face :foreground)
                     (herdr-sanctum--face-color 'link :foreground)
                     (alist-get 'accent fallback-tbl))))
    (let ((chrome (herdr-sanctum--blend bg fg (if light-p 0.05 0.06))))
      (or (pcase token
            ('bg-chrome      chrome)
            ('bg-panel       chrome)
            ('bg-card        (herdr-sanctum--blend bg fg (if light-p 0.10 0.11)))
            ('bg-hl          (herdr-sanctum--blend chrome accent (if light-p 0.14 0.20)))
            ('bg-selected    (herdr-sanctum--blend bg fg (if light-p 0.15 0.16)))
            ('bg-pill-active (herdr-sanctum--blend bg accent (if light-p 0.18 0.24)))
            ('border         (herdr-sanctum--blend bg fg (if light-p 0.16 0.14)))
            ('guide          (herdr-sanctum--blend bg fg (if light-p 0.22 0.20)))
            ('accent         accent)
            ('fg-main        fg)
            ('fg-dim         (or (herdr-sanctum--face-color 'shadow :foreground)
                                 (herdr-sanctum--blend fg bg 0.38)))
            ('fg-alt         (herdr-sanctum--blend fg bg 0.20))
            ('yellow         (herdr-sanctum--face-color 'warning :foreground))
            ('red            (herdr-sanctum--face-color 'error :foreground))
            ('green          (herdr-sanctum--face-color 'success :foreground)))
          (alist-get token fallback-tbl)))))

(defun herdr-sanctum--query-theme-palette (query-fn token)
  "Resolve semantic TOKEN via Modus/Ef QUERY-FN, keeping surface steps distinct."
  (if (eq token 'bg-selected)
      (let ((base (ignore-errors (funcall query-fn 'bg-inactive t)))
            (edge (ignore-errors (funcall query-fn 'bg-active t))))
        (cond
         ((and (herdr-sanctum--valid-color-p base)
               (herdr-sanctum--valid-color-p edge))
          (herdr-sanctum--blend base edge 0.35))
         ((herdr-sanctum--valid-color-p base) base)))
    (let* ((mapped (or (alist-get token herdr-sanctum--modus-token-map) token))
           (c (ignore-errors (funcall query-fn mapped t))))
      (when (herdr-sanctum--valid-color-p c) c))))

(defun herdr-sanctum-default-palette-color (token &optional fallback)
  "Resolve semantic palette TOKEN using Modus, Ef, or generic face derivation.
FALLBACK is returned if no color could be determined."
  (let ((safe-fallback (when (herdr-sanctum--valid-color-p fallback) fallback)))
    (if (eq token 'bg-panel)
        (let ((bg-chrome (herdr-sanctum-palette-color 'bg-chrome safe-fallback)))
          (if (numberp herdr-sanctum-panel-surface-blend)
              (let* ((alpha (min 1.0 (max 0.0 (float herdr-sanctum-panel-surface-blend))))
                     (light-p (eq (herdr-sanctum--background-mode) 'light))
                     (bg-main (or (herdr-sanctum--face-color 'default :background)
                                  (and (fboundp 'modus-themes-get-color-value)
                                       (herdr-sanctum--query-theme-palette
                                        #'modus-themes-get-color-value 'bg-main))
                                  (and (fboundp 'ef-themes-get-color-value)
                                       (herdr-sanctum--query-theme-palette
                                        #'ef-themes-get-color-value 'bg-main))
                                  (if light-p "#ffffff" "#0d0e1c"))))
                (or (and bg-chrome bg-main
                         (herdr-sanctum--blend bg-main bg-chrome alpha))
                    bg-chrome
                    safe-fallback))
            (or bg-chrome safe-fallback)))
      (let* ((key (cons (herdr-sanctum--background-mode) token))
             (cached (gethash key herdr-sanctum--palette-cache 'miss)))
        (if (not (eq cached 'miss))
            (or cached safe-fallback)
          (let ((resolved
                 (or (and (fboundp 'modus-themes-get-color-value)
                          (herdr-sanctum--query-theme-palette
                           #'modus-themes-get-color-value token))
                     (and (fboundp 'ef-themes-get-color-value)
                          (herdr-sanctum--query-theme-palette
                           #'ef-themes-get-color-value token))
                     (let ((c (herdr-sanctum--derive-generic-color token)))
                       (when (herdr-sanctum--valid-color-p c) c)))))
            (puthash key resolved herdr-sanctum--palette-cache)
            (or resolved safe-fallback)))))))

(defun herdr-sanctum-palette-color (token &optional fallback)
  "Resolve semantic color TOKEN via overrides or `herdr-sanctum-palette-function'."
  (let ((override (alist-get token herdr-sanctum-palette-overrides)))
    (if (herdr-sanctum--valid-color-p override)
        override
      (let ((color (and (functionp herdr-sanctum-palette-function)
                        (funcall herdr-sanctum-palette-function token fallback))))
        (cond
         ((herdr-sanctum--valid-color-p color) color)
         ((eq herdr-sanctum-palette-function #'herdr-sanctum-default-palette-color)
          (when (herdr-sanctum--valid-color-p fallback) fallback))
         (t (herdr-sanctum-default-palette-color token fallback)))))))

;;; Frame & Font Helpers

(defun herdr-sanctum--graphic-frame (&optional frame)
  "Return graphical FRAME, selected frame if graphical, or first GUI frame."
  (or (and frame (display-graphic-p frame) frame)
      (and (display-graphic-p) (selected-frame))
      (seq-find (lambda (cand)
                  (and (display-graphic-p cand)
                       (not (frame-parameter cand 'parent-frame))))
                (frame-list))))

(defun herdr-sanctum-ui-font-family (&optional frame)
  "Return the UI font family for graphical Sanctum chrome, or nil."
  (cond
   ((eq herdr-sanctum-ui-font 'default) nil)
   ((stringp herdr-sanctum-ui-font) herdr-sanctum-ui-font)
   (t
    (when-let* ((f (herdr-sanctum--graphic-frame frame))
                (vp (face-attribute 'variable-pitch :family f t)))
      (when (and (stringp vp)
                 (not (member vp '("unspecified" "Sans Serif")))
                 (not (string-empty-p vp)))
        vp)))))

;;; Theme Application

(defun herdr-sanctum--theme-state ()
  "Return a comparable snapshot of the active theme and UI chrome settings."
  (list (copy-sequence custom-enabled-themes)
        (herdr-sanctum--background-mode)
        (herdr-sanctum--face-color 'default :background)
        (herdr-sanctum--face-color 'default :foreground)
        herdr-sanctum-ui-font
        herdr-sanctum-ui-font-height
        herdr-sanctum-palette-function
        (copy-tree herdr-sanctum-palette-overrides)
        herdr-sanctum-panel-surface-blend
        (herdr-sanctum-ui-font-family)))

(defun herdr-sanctum--chrome-face-specs (pal family-attrs ui-height)
  "Build surface and chrome `(FACE . SPEC)' alist from palette PAL."
  (let-alist pal
    (let ((chrome-base `(,@family-attrs :height ,ui-height
                         :background ,.bg-chrome :foreground ,.fg-dim
                         :weight normal :slant normal :inverse-video nil
                         :box (:line-width (-1 . 3) :color ,.bg-chrome)
                         :overline nil))
          (tab-active-base `(:inherit herdr-sanctum-chrome-bar
                             :foreground ,.fg-main :weight semi-bold)))
      `((herdr-sanctum-sidebar-space-active
         (t :foreground ,.fg-main :background ,.bg-selected :weight semi-bold :extend t))
        (herdr-sanctum-sidebar-guide
         (t :foreground ,.guide))
        (herdr-sanctum-chrome-bar
         (((type graphic)) ,@chrome-base :underline (:color ,.border :position t))
         (t ,@chrome-base :underline nil))
        (herdr-sanctum-chrome-pill
         (t :inherit herdr-sanctum-chrome-bar :foreground ,.fg-alt :background ,.bg-chrome))
        (herdr-sanctum-chrome-pill-active
         (t :inherit herdr-sanctum-chrome-bar :foreground ,.fg-main :background ,.bg-selected
            :weight medium :box (:line-width (-1 . 3) :color ,.bg-selected)))
        (herdr-sanctum-chrome-sep
         (t :inherit herdr-sanctum-chrome-bar :foreground ,.guide))
        (herdr-sanctum-status-bar
         (t ,@family-attrs :height ,ui-height
            :background ,.bg-chrome :foreground ,.fg-dim
            :weight normal :slant normal :inverse-video nil
            :underline nil :overline ,.border :box (:line-width (-1 . 3) :color ,.bg-chrome)))
        (herdr-sanctum-drawer-tab
         (t :inherit herdr-sanctum-chrome-bar :foreground ,.fg-dim :weight normal))
        (herdr-sanctum-drawer-tab-active
         (((type graphic)) ,@tab-active-base :underline (:color ,.accent :position t))
         (t ,@tab-active-base :underline t))
        (herdr-sanctum-drawer-card
         (t :background ,.bg-card :foreground ,.fg-main :extend t))
        (herdr-sanctum-drawer-button
         (t :background ,.bg-selected :foreground ,.fg-main :weight semi-bold
            :box (:line-width (-1 . -1) :color ,.guide)))
        (herdr-sanctum-drawer-button-focus
         (t :background ,.bg-pill-active :foreground ,.fg-main :weight bold
            :box (:line-width (-1 . -1) :color ,.accent)))
        (herdr-sanctum-panel-focus-active
         (t :background ,.bg-pill-active :foreground ,.fg-main :weight semi-bold :extend t))
        (herdr-sanctum-banner-info
         (t :inherit herdr-sanctum-chrome-bar :background ,.bg-selected
            :foreground ,.accent :weight bold
            :box (:line-width (-1 . 3) :color ,.bg-selected) :extend t))
        (herdr-sanctum-banner-warn
         (t :inherit herdr-sanctum-chrome-bar :background ,.bg-card
            :foreground ,.yellow :weight bold
            :box (:line-width (-1 . 3) :color ,.bg-card) :extend t))))))

;;;###autoload
(defun herdr-sanctum-apply-theme (&optional force)
  "Align Sanctum chrome and surface faces with the active theme.
Coalesces duplicate calls when theme and UI font state have not changed
unless FORCE is non-nil (always non-nil when called interactively)."
  (interactive (list t))
  (let ((state (herdr-sanctum--theme-state)))
    (when (or force (not (equal state herdr-sanctum--last-theme-state)))
      (clrhash herdr-sanctum--svg-cache)
      (clrhash herdr-sanctum--palette-cache)
      (let* ((fallbacks (if (eq (herdr-sanctum--background-mode) 'light)
                            herdr-sanctum--light-fallback-palette
                          herdr-sanctum--dark-fallback-palette))
             (pal (mapcar (lambda (entry)
                            (cons (car entry)
                                  (herdr-sanctum-palette-color (car entry)
                                                               (cdr entry))))
                          fallbacks))
             (var-family (herdr-sanctum-ui-font-family))
             (family-attrs (when (and (stringp var-family)
                                      (not (equal var-family "unspecified")))
                             (list :family var-family)))
             (specs (herdr-sanctum--chrome-face-specs
                     pal family-attrs herdr-sanctum-ui-font-height)))
        (dolist (entry specs)
          (face-spec-set (car entry) (cdr entry) 'face-defface-spec)))
      (setq herdr-sanctum--last-theme-state state)
      (cl-incf herdr-sanctum-theme-revision)
      (force-mode-line-update t)
      (run-hooks 'herdr-sanctum-theme-change-hook))))

(defun herdr-sanctum--on-theme-change (&rest _)
  "Coalescing hook trampoline for theme enable, disable, and post-load hooks."
  (herdr-sanctum-apply-theme))

(defun herdr-sanctum--frame-theme (frame)
  "Refresh Sanctum chrome faces after a top-level FRAME finishes setup."
  (when (and (frame-live-p frame)
             (not (frame-parameter frame 'parent-frame))
             (not (eq (frame-parameter frame 'minibuffer) 'only))
             (or (display-graphic-p frame)
                 (not (herdr-sanctum--graphic-frame))))
    (with-selected-frame frame
      (herdr-sanctum-apply-theme))))

(add-hook 'enable-theme-functions #'herdr-sanctum--on-theme-change)
(add-hook 'disable-theme-functions #'herdr-sanctum--on-theme-change)
(add-hook 'modus-themes-after-load-theme-hook #'herdr-sanctum--on-theme-change)
(add-hook 'ef-themes-post-load-hook #'herdr-sanctum--on-theme-change)
(add-hook 'after-make-frame-functions #'herdr-sanctum--frame-theme)
(herdr-sanctum-apply-theme)

;;; SVG & Unicode Badges

(defun herdr-sanctum-svg-available-p (&optional frame)
  "Return non-nil when inline SVG images can be displayed on FRAME.
FRAME defaults to the selected frame."
  (and (display-graphic-p frame)
       (fboundp 'image-type-available-p)
       (image-type-available-p 'svg)
       (require 'svg nil t)))

(defun herdr-sanctum-space-badge (label &optional key active-bg size)
  "Return a propertized monogram badge for space LABEL (hashed by KEY).
When ACTIVE-BG is non-nil, apply it to the badge face background.
Optional SIZE specifies the SVG width and height in pixels (default 15)."
  (if (eq herdr-sanctum-space-badge-style 'none)
      ""
    (let* ((trimmed (string-trim (or label "")))
           (letter (upcase (if (string-empty-p trimmed) "?" (substring trimmed 0 1))))
           (palette (or herdr-sanctum-space-badge-palette '(("#5458a6" . "#eceeff"))))
           (idx (mod (abs (sxhash-equal (or key label ""))) (length palette)))
           (pair (nth idx palette))
           (bg (car pair))
           (fg (cdr pair)))
      (if (and (eq herdr-sanctum-space-badge-style 'svg)
               (herdr-sanctum-svg-available-p))
          (let* ((sz (or size 15))
                 (rx (if (<= sz 13) 3 3.5))
                 (cache-key (list 'space letter bg fg sz))
                 (img (or (gethash cache-key herdr-sanctum--svg-cache)
                          (let ((svg (svg-create sz sz)))
                            (svg-rectangle svg 0 0 sz sz :rx rx :ry rx :fill bg)
                            (svg-text svg letter
                                      :x (/ sz 2.0) :y (* sz 0.72)
                                      :text-anchor "middle"
                                      :font-family "sans-serif"
                                      :font-size (max 8 (round (* sz 0.66)))
                                      :font-weight "700"
                                      :fill fg)
                            (let ((created (svg-image svg :ascent 'center)))
                              (puthash cache-key created herdr-sanctum--svg-cache)
                              created)))))
            (if active-bg
                (propertize " " 'display img 'face `(:background ,active-bg))
              (propertize " " 'display img)))
        (propertize (format " %s " letter)
                    'face `(:background ,bg :foreground ,fg :weight bold))))))

(defun herdr-sanctum--agent-brand-color (agent)
  "Return muted brand color hex for canonical AGENT name."
  (let* ((canon (downcase (or agent "")))
         (spec (cdr (assoc canon herdr-sanctum-agent-brands)))
         (color-spec (plist-get spec :color)))
    (cond
     ((herdr-sanctum--valid-color-p color-spec) color-spec)
     ((symbolp color-spec)
      (herdr-sanctum-palette-color (or color-spec 'fg-alt) "#a6b2d4"))
     (t (herdr-sanctum-palette-color 'fg-alt "#a6b2d4")))))

(defun herdr-sanctum--agent-svg-file-raw (canon)
  "Read and cache raw SVG text for agent CANON from icon dir or `:file'.
Strips leading XML declarations, DOCTYPE declarations, and comments so the
root `<svg>' element can be safely embedded inside an outer `<g>'."
  (when (and (stringp canon) (not (string-empty-p canon)))
    (let* ((spec (cdr (assoc canon herdr-sanctum-agent-brands)))
           (file-spec (plist-get spec :file))
           (path
            (cond
             ((and (stringp file-spec)
                   (or (file-name-absolute-p file-spec)
                       (string-prefix-p "~/" file-spec)))
              (when (and (not (member ".." (split-string file-spec "[/\\\\]")))
                         (let ((ext (file-name-extension file-spec)))
                           (and ext (string-equal-ignore-case ext "svg"))))
                (expand-file-name file-spec)))
             (herdr-sanctum-agent-icon-directory
              (let ((name (or file-spec canon)))
                (when (and (stringp name)
                           (not (member name '("." "..")))
                           (string-match-p "\\`[a-z0-9._-]+\\'" name))
                  (expand-file-name (concat name ".svg")
                                    herdr-sanctum-agent-icon-directory)))))))
      (when path
        (let ((cached (gethash path herdr-sanctum--svg-file-cache 'miss)))
          (if (not (eq cached 'miss))
              cached
            (let ((raw (when (and (file-regular-p path)
                                  (file-readable-p path))
                         (with-temp-buffer
                           (insert-file-contents path)
                           (goto-char (point-min))
                           (when (eq (char-after) ?\ufeff)
                             (forward-char 1))
                           (let ((case-fold-search nil))
                             (while (or (progn (skip-chars-forward " \t\r\n") nil)
                                        (looking-at "<\\?\\(?:.\\|\n\\)*?\\?>")
                                        (looking-at "<!--\\(?:.\\|\n\\)*?-->")
                                        (looking-at "<!DOCTYPE\\(?:[^[>]\\|\\[\\(?:.\\|\n\\)*?\\]\\)*>"))
                               (goto-char (match-end 0))))
                           (let ((body (string-trim
                                        (buffer-substring-no-properties
                                         (point) (point-max)))))
                             (unless (string-empty-p body)
                               body))))))
              (puthash path raw herdr-sanctum--svg-file-cache)
              raw)))))))

(defun herdr-sanctum--agent-svg-file-image (canon color opacity dot-color cutout-bg active-bg)
  "Compose a 14x14 SVG image from `<agent>.svg' for CANON with status overlay."
  (when-let* ((raw (herdr-sanctum--agent-svg-file-raw canon)))
    (let* ((tinted (replace-regexp-in-string "currentColor" color raw t t))
           (sized (replace-regexp-in-string
                   "\\b\\(width\\|height\\)=\"1em\"" "\\1=\"24\"" tinted))
           (bg-rect (if active-bg
                        (format "<rect width=\"24\" height=\"24\" fill=\"%s\"/>" active-bg)
                      ""))
           (dot (if dot-color
                    (format "<circle cx=\"19.5\" cy=\"19.5\" r=\"4.1\" fill=\"%s\" stroke=\"%s\" stroke-width=\"2\"/>"
                            dot-color cutout-bg)
                  ""))
           (composed (format
                      "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"14\" height=\"14\" viewBox=\"0 0 24 24\">%s<g opacity=\"%.2f\">%s</g>%s</svg>"
                      bg-rect opacity sized dot)))
      (create-image composed 'svg t :ascent 'center))))

(defun herdr-sanctum--agent-monogram-image (canon color opacity dot-color cutout-bg active-bg)
  "Build a 14x14 fallback 1-letter SVG monogram image for agent CANON."
  (let ((svg (svg-create 14 14))
        (letter (upcase (if (string-empty-p canon) "A" (substring canon 0 1)))))
    (when active-bg
      (svg-rectangle svg 0 0 14 14 :fill active-bg))
    (svg-rectangle svg 1.0 1.0 12.0 12.0 :rx 3.5 :ry 3.5
                   :fill color :fill-opacity (* 0.20 opacity)
                   :stroke color :stroke-width 0.9
                   :stroke-opacity (* 0.65 opacity))
    (svg-text svg letter
              :x 7.0 :y 10.1
              :text-anchor "middle"
              :font-family "sans-serif"
              :font-size 9
              :font-weight "700"
              :fill color :fill-opacity opacity)
    (when dot-color
      (svg-circle svg 11.4 11.4 2.4
                  :fill dot-color :stroke cutout-bg :stroke-width 1.2))
    (svg-image svg :ascent 'center)))

(defun herdr-sanctum-agent-badge (agent status &optional active-bg)
  "Return a propertized 14x14 brand icon for AGENT with STATUS overlay."
  (let* ((canon (downcase (string-trim (or agent ""))))
         (st (or status "idle"))
         (spec (cdr (assoc canon herdr-sanctum-agent-brands)))
         (brand-glyph (let ((g (plist-get spec :glyph)))
                        (when (and (stringp g) (not (string-empty-p g))) g)))
         (status-glyph (let ((g (cdr (assoc st herdr-sanctum-status-glyphs))))
                         (if (and (stringp g) (not (string-empty-p g)))
                             g
                           (pcase st
                             ("blocked" "◆")
                             ("working" "●")
                             ("done"    "✓")
                             (_         "○")))))
         (base-face (herdr-sanctum-status-face st))
         (face (if active-bg (list base-face `(:background ,active-bg)) base-face)))
    (if (and (eq herdr-sanctum-agent-icon-style 'svg-files)
             (herdr-sanctum-svg-available-p))
        (let* ((color (herdr-sanctum--agent-brand-color canon))
               (opacity (if (equal st "idle") 0.58 1.0))
               (dot-color (pcase st
                            ("blocked" (herdr-sanctum-palette-color 'red))
                            ("done"    (herdr-sanctum-palette-color 'green))))
               (cutout-bg (or active-bg (herdr-sanctum-palette-color 'bg-panel)))
               (cache-key (list 'agent-brand canon (plist-get spec :file) st color
                                dot-color cutout-bg active-bg
                                herdr-sanctum-agent-icon-directory))
               (img (or (gethash cache-key herdr-sanctum--svg-cache)
                        (let ((created
                               (or (herdr-sanctum--agent-svg-file-image
                                    canon color opacity dot-color cutout-bg active-bg)
                                   (herdr-sanctum--agent-monogram-image
                                    canon color opacity dot-color cutout-bg active-bg))))
                          (puthash cache-key created herdr-sanctum--svg-cache)
                          created))))
          (propertize status-glyph 'display img 'face face))
      (propertize (or brand-glyph status-glyph) 'face face))))

(defun herdr-sanctum-row-accent (active &optional active-bg guide)
  "Return a pixel-matched 8px left accent bar (or spacer) for Sanctum rows.
When ACTIVE is non-nil, render the focused accent bar; when GUIDE is non-nil
and ACTIVE is nil, render a subtle tree guide; otherwise return an 8px spacer."
  (if (herdr-sanctum-svg-available-p)
      (if (or active guide)
          (let* ((color (if active
                            (herdr-sanctum-palette-color 'accent)
                          (herdr-sanctum-palette-color 'guide)))
                 (cache-key (list 'accent (and active t) (and guide t) color active-bg))
                 (img (or (gethash cache-key herdr-sanctum--svg-cache)
                          (let ((svg (svg-create 8 16)))
                            (when active-bg
                              (svg-rectangle svg 0 0 8 16 :fill active-bg))
                            (if active
                                (svg-rectangle svg 0 1.5 2.5 13 :rx 1.2 :ry 1.2 :fill color)
                              (svg-rectangle svg 1 0 1 16 :fill color))
                            (let ((created (svg-image svg :ascent 'center)))
                              (puthash cache-key created herdr-sanctum--svg-cache)
                              created)))))
            (propertize (if active "▎" "│")
                        'display img
                        'face (cond
                               (active-bg `(:background ,active-bg))
                               (active `(:foreground ,color :weight bold))
                               (t 'herdr-sanctum-sidebar-guide))))
        (if active-bg
            (propertize " " 'display '(space :width (8)) 'face `(:background ,active-bg))
          (propertize " " 'display '(space :width (8)))))
    (cond
     (active (propertize "▎" 'face
                         `(:foreground ,(herdr-sanctum-palette-color 'accent)
                           :background ,(or active-bg
                                            (herdr-sanctum-palette-color 'bg-selected))
                           :weight bold)))
     (guide  (propertize "│" 'face 'herdr-sanctum-sidebar-guide))
     (t      " "))))

(defun herdr-sanctum-status-face (status)
  "Return the dedicated status face symbol for agent STATUS."
  (pcase status
    ("blocked" 'herdr-sanctum-status-blocked)
    ("working" 'herdr-sanctum-status-working)
    ("done"    'herdr-sanctum-status-done)
    (_         'herdr-sanctum-status-idle)))

(defun herdr-sanctum-status-glyph (status &optional active-bg)
  "Return a propertized single-column glyph for agent STATUS, or nil."
  (when-let* ((glyph (and (stringp status)
                          (cdr (assoc status herdr-sanctum-status-glyphs))))
              ((and (stringp glyph) (not (string-empty-p glyph)))))
    (let ((base-face (herdr-sanctum-status-face status)))
      (propertize glyph
                  'face (if active-bg
                            (list base-face `(:background ,active-bg))
                          base-face)))))

(provide 'herdr-sanctum-theme)
;;; herdr-sanctum-theme.el ends here
