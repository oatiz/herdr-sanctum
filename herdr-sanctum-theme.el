;;; herdr-sanctum-theme.el --- Faces, palette, SVG icons & layout helpers -*- lexical-binding: t; -*-

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

;; Theme-adaptive faces, palette token resolution (Modus, Ef, and generic
;; dark/light color derivation), inline SVG space monograms and coding-agent
;; badges (with pure-text TTY fallback), and pixel-exact row formatting.

;;; Code:

(require 'cl-lib)
(require 'color)
(require 'seq)
(require 'subr-x)
(require 'herdr-sanctum-rpc)

(declare-function ef-themes-get-color-value "ef-themes" (color &optional overrides theme))
(declare-function modus-themes-get-color-value "modus-themes" (color &optional overrides theme))
(declare-function svg-create "svg" (width height &rest args))
(declare-function svg-circle "svg" (svg x y radius &rest args))
(declare-function svg-image "svg" (svg &rest props))
(declare-function svg-line "svg" (svg x1 y1 x2 y2 &rest args))
(declare-function svg-polygon "svg" (svg points &rest args))
(declare-function svg-polyline "svg" (svg points &rest args))
(declare-function svg-rectangle "svg" (svg x y width height &rest args))
(declare-function svg-text "svg" (svg text &rest args))

(defcustom herdr-sanctum-palette-function #'herdr-sanctum-default-palette-color
  "Function `(TOKEN FALLBACK)' resolving semantic palette colors.
Supported tokens include `bg-chrome', `bg-card', `bg-hl', `bg-selected',
`bg-pill-active', `border', `guide', `accent', `fg-main', `fg-dim',
`fg-alt', `yellow', `red', `green', `cyan', `magenta', and `red-warmer'."
  :type 'function
  :group 'herdr-sanctum)

(defcustom herdr-sanctum-ui-font nil
  "Font family used for Sanctum graphical chrome (sidebar, drawer, top bar).
When nil, inherit `variable-pitch' if it resolves to a concrete font family.
Set to symbol `default' to keep the frame's default monospace font, or a
font family name string."
  :type '(choice (const :tag "Auto (variable-pitch)" nil)
                 (const :tag "Default monospace" default)
                 (string :tag "Font family"))
  :group 'herdr-sanctum)

(defcustom herdr-sanctum-ui-font-candidates nil
  "Fallback font family names tried when `variable-pitch' is unspecified."
  :type '(repeat string)
  :group 'herdr-sanctum)

(defcustom herdr-sanctum-ui-font-height 0.9
  "Height for Sanctum chrome UI text.
May be a floating-point scale relative to `default' (for example 0.9) or an
integer in 1/10pt units (for example 110)."
  :type '(choice (float :tag "Relative scale")
                 (integer :tag "Absolute 1/10pt"))
  :group 'herdr-sanctum)

(defcustom herdr-sanctum-space-badge-style 'svg
  "Rendering style for Space monogram badges in the sidebar and drawer."
  :type '(choice (const :tag "Inline SVG monogram" svg)
                 (const :tag "Text monogram" text)
                 (const :tag "None" none))
  :group 'herdr-sanctum)

(defcustom herdr-sanctum-space-badge-palette
  '(("#2a6b68" . "#e4f7f5")
    ("#8a5f2f" . "#fdf1e0")
    ("#5458a6" . "#eceeff")
    ("#3a7852" . "#e4f7ea")
    ("#944568" . "#fde8f1")
    ("#3a639c" . "#e8f1ff"))
  "Muted `(BG . FG)' color pairs used for Space monogram badges."
  :type '(repeat (cons (color :tag "Background") (color :tag "Foreground")))
  :group 'herdr-sanctum)

(defvar herdr-sanctum--svg-cache (make-hash-table :test #'equal)
  "Cache of rendered inline SVG images for space monograms and agent glyphs.")

(defvar herdr-sanctum--svg-file-cache (make-hash-table :test #'equal)
  "Cache of raw SVG strings loaded from `herdr-sanctum-agent-icon-directory'.")

(defcustom herdr-sanctum-theme-change-hook nil
  "Hook run after `herdr-sanctum-apply-theme' updates Sanctum faces."
  :type 'hook
  :group 'herdr-sanctum)

(defcustom herdr-sanctum-agent-icon-style 'svg-files
  "Icon rendering backend for Herdr coding-agent rows.
`builtin' renders crisp 14x14 geometric SVGs via `svg.el'; `svg-files'
loads static SVGs from `herdr-sanctum-agent-icon-directory' with automatic
fallback to `builtin'; `unicode' always uses plain Unicode glyphs."
  :type '(choice (const :tag "Static SVG files" svg-files)
                 (const :tag "Built-in geometric SVG" builtin)
                 (const :tag "Plain Unicode glyphs" unicode))
  :group 'herdr-sanctum
  :set (lambda (sym val)
         (set-default sym val)
         (when (boundp 'herdr-sanctum--svg-cache)
           (clrhash herdr-sanctum--svg-cache))
         (run-hooks 'herdr-sanctum-theme-change-hook)))

(defcustom herdr-sanctum-agent-icon-directory
  (let* ((src (or load-file-name (locate-library "herdr-sanctum-theme")))
         (base (and src (file-name-directory (file-truename src)))))
    (and base (expand-file-name "assets/agents" base)))
  "Directory containing `<agent>.svg' static icons for `svg-files' style."
  :type '(choice (const :tag "None" nil) directory)
  :group 'herdr-sanctum)

(defcustom herdr-sanctum-agent-brands
  '(("claude"         . (:color red-warmer :fallback "#de8e80"))
    ("claude-code"    . (:color red-warmer :fallback "#de8e80" :file "claude"))
    ("pi"             . (:color accent     :fallback "#82aaff"))
    ("codex"          . (:color cyan       :fallback "#7dc2b8"))
    ("gemini"         . (:color magenta    :fallback "#b4a4e6"))
    ("qwen-code"      . (:color fg-alt     :fallback "#a6b2d4" :file "qwen"))
    ("github-copilot" . (:color fg-alt     :fallback "#a6b2d4" :file "copilot")))
  "Alist mapping canonical agent names to brand color and SVG file metadata.
Each entry is `(NAME . PLIST)' where PLIST supports:
  `:color'    -- semantic palette token symbol or hex color string
  `:fallback' -- fallback hex color string
  `:file'     -- alternate `<file>.svg' stem in icon directory."
  :type '(alist :key-type string :value-type plist)
  :group 'herdr-sanctum)

;;; Faces

(defface herdr-sanctum-sidebar-brand
  '((t :inherit variable-pitch :weight bold))
  "Face for the top brand or title label in Sanctum sidebars."
  :group 'herdr-sanctum)

(defface herdr-sanctum-sidebar-header
  '((t :inherit shadow :weight semi-bold))
  "Face for section headers in Sanctum sidebars."
  :group 'herdr-sanctum)

(defface herdr-sanctum-sidebar-group-working
  '((t :inherit warning :weight semi-bold))
  "Face for the `Working' agent group header."
  :group 'herdr-sanctum)

(defface herdr-sanctum-sidebar-group-blocked
  '((t :inherit error :weight semi-bold))
  "Face for the `Blocked' agent group header."
  :group 'herdr-sanctum)

(defface herdr-sanctum-sidebar-group-done
  '((t :inherit success :weight semi-bold))
  "Face for the `Done' agent group header."
  :group 'herdr-sanctum)

(defface herdr-sanctum-sidebar-space
  '((t :inherit default :weight medium))
  "Face for space labels in `*sanctum-spaces*'."
  :group 'herdr-sanctum)

(defface herdr-sanctum-sidebar-space-active
  '((t :inherit (bold hl-line) :extend t))
  "Face for the focused space row in `*sanctum-spaces*'."
  :group 'herdr-sanctum)

(defface herdr-sanctum-sidebar-tab
  '((t :inherit default))
  "Face for tab labels in the focused space."
  :group 'herdr-sanctum)

(defface herdr-sanctum-sidebar-tab-dim
  '((t :inherit shadow))
  "Face for inactive tab labels in non-focused spaces."
  :group 'herdr-sanctum)

(defface herdr-sanctum-sidebar-tab-ws-active
  '((t :inherit font-lock-doc-face))
  "Face for a non-focused space's active tab."
  :group 'herdr-sanctum)

(defface herdr-sanctum-sidebar-tab-active
  '((t :inherit (bold hl-line) :extend t))
  "Face for the globally focused tab or agent row."
  :group 'herdr-sanctum)

(defface herdr-sanctum-sidebar-guide
  '((t :inherit shadow))
  "Face for vertical tree guides and section dividers."
  :group 'herdr-sanctum)

(defface herdr-sanctum-sidebar-guide-active
  '((t :inherit (font-lock-function-name-face bold)))
  "Face for the active row's left accent bar."
  :group 'herdr-sanctum)

(defface herdr-sanctum-sidebar-meta
  '((t :inherit shadow))
  "Face for right-aligned Git branches, counts, and durations."
  :group 'herdr-sanctum)

(defface herdr-sanctum-sidebar-git-behind
  '((t :inherit error :weight medium))
  "Face for behind-remote (`↓N') Git indicators."
  :group 'herdr-sanctum)

(defface herdr-sanctum-sidebar-git-ahead
  '((t :inherit font-lock-function-name-face :weight medium))
  "Face for ahead-of-remote (`↑N') Git indicators."
  :group 'herdr-sanctum)

(defface herdr-sanctum-chrome-bar
  '((t :inherit header-line))
  "Header-line bar face for `*sanctum*'."
  :group 'herdr-sanctum)

(defface herdr-sanctum-chrome-pill
  '((t :inherit herdr-sanctum-chrome-bar))
  "Clickable header-line button face for `*sanctum*'."
  :group 'herdr-sanctum)

(defface herdr-sanctum-chrome-pill-active
  '((t :inherit herdr-sanctum-chrome-bar :weight semi-bold))
  "Active header-line button pill face for `*sanctum*'."
  :group 'herdr-sanctum)

(defface herdr-sanctum-status-bar
  '((t :inherit mode-line-inactive))
  "Bottom status strip face for Sanctum sidebars and drawers."
  :group 'herdr-sanctum)

(defface herdr-sanctum-drawer-tab
  '((t :inherit shadow :weight medium))
  "Inactive view tab face in `*sanctum-drawer*'."
  :group 'herdr-sanctum)

(defface herdr-sanctum-drawer-tab-active
  '((t :inherit default :weight semi-bold))
  "Active view tab face in `*sanctum-drawer*'."
  :group 'herdr-sanctum)

(defface herdr-sanctum-drawer-card
  '((t :inherit default :extend t))
  "Elevated card background face in `*sanctum-drawer*'."
  :group 'herdr-sanctum)

(defface herdr-sanctum-drawer-button
  '((t :inherit default :weight semi-bold))
  "Compact action button pill face in `*sanctum-drawer*'."
  :group 'herdr-sanctum)

(defface herdr-sanctum-banner-info
  '((t :inherit header-line :weight bold :extend t))
  "Header-line banner face for terminal read-only or info modes."
  :group 'herdr-sanctum)

(defface herdr-sanctum-banner-warn
  '((t :inherit warning :weight bold :extend t))
  "Header-line banner face for raw character passthrough mode."
  :group 'herdr-sanctum)

;;; Palette Derivation (Modus / Ef / Generic Light & Dark)

(defconst herdr-sanctum--modus-token-map
  '((bg-chrome      . bg-tab-bar)
    (bg-card        . bg-inactive)
    (bg-hl          . bg-hl-line)
    (bg-selected    . bg-tab-current)
    (bg-pill-active . bg-completion)
    (border         . border)
    (guide          . bg-active)
    (accent         . blue)
    (fg-main        . fg-main)
    (fg-dim         . fg-dim)
    (fg-alt         . fg-alt)
    (yellow         . yellow-warmer)
    (red            . red-intense)
    (red-warmer     . red-warmer)
    (green          . green)
    (cyan           . cyan)
    (magenta        . magenta-cooler))
  "Mapping from Sanctum semantic tokens to Modus/Ef palette symbols.")

(defconst herdr-sanctum--dark-fallback-palette
  '((bg-chrome      . "#161824")
    (bg-card        . "#1b1e2e")
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
    (red-warmer     . "#de8e80")
    (green          . "#7ec495")
    (cyan           . "#7dc2b8")
    (magenta        . "#b4a4e6"))
  "Default dark palette used when theme colors cannot be queried.")

(defconst herdr-sanctum--light-fallback-palette
  '((bg-chrome      . "#eceff4")
    (bg-card        . "#e5e9f0")
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
    (red-warmer     . "#c05640")
    (green          . "#2f7d4d")
    (cyan           . "#267a78")
    (magenta        . "#7b52ab"))
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
    (or (pcase token
          ('bg-chrome      (herdr-sanctum--blend bg fg (if light-p 0.05 0.06)))
          ('bg-card        (herdr-sanctum--blend bg fg (if light-p 0.08 0.09)))
          ('bg-hl          (or (herdr-sanctum--face-color 'hl-line :background)
                               (herdr-sanctum--blend bg fg (if light-p 0.07 0.08))))
          ('bg-selected    (herdr-sanctum--blend bg fg (if light-p 0.13 0.15)))
          ('bg-pill-active (herdr-sanctum--blend bg accent (if light-p 0.18 0.22)))
          ('border         (herdr-sanctum--blend bg fg (if light-p 0.16 0.14)))
          ('guide          (herdr-sanctum--blend bg fg (if light-p 0.22 0.20)))
          ('accent         accent)
          ('fg-main        fg)
          ('fg-dim         (or (herdr-sanctum--face-color 'shadow :foreground)
                               (herdr-sanctum--blend fg bg 0.38)))
          ('fg-alt         (herdr-sanctum--blend fg bg 0.20))
          ('yellow         (herdr-sanctum--face-color 'warning :foreground))
          ('red            (herdr-sanctum--face-color 'error :foreground))
          ('red-warmer     (herdr-sanctum--face-color 'error :foreground))
          ('green          (herdr-sanctum--face-color 'success :foreground))
          ('cyan           (herdr-sanctum--face-color 'font-lock-type-face :foreground))
          ('magenta        (herdr-sanctum--face-color 'font-lock-keyword-face :foreground)))
        (alist-get token fallback-tbl))))

(defun herdr-sanctum-default-palette-color (token &optional fallback)
  "Resolve semantic palette TOKEN using Modus, Ef, or generic face derivation.
FALLBACK is returned if no color could be determined."
  (let ((mapped (or (alist-get token herdr-sanctum--modus-token-map) token)))
    (or (and (fboundp 'modus-themes-get-color-value)
             (ignore-errors (modus-themes-get-color-value mapped t)))
        (and (fboundp 'ef-themes-get-color-value)
             (ignore-errors (ef-themes-get-color-value mapped t)))
        (herdr-sanctum--derive-generic-color token)
        fallback)))

(defun herdr-sanctum-palette-color (token &optional fallback)
  "Resolve semantic color TOKEN via `herdr-sanctum-palette-function'."
  (or (and (functionp herdr-sanctum-palette-function)
           (funcall herdr-sanctum-palette-function token fallback))
      (herdr-sanctum-default-palette-color token fallback)))

;;; Font & Remapping

(defun herdr-sanctum-ui-font-family (&optional frame)
  "Return the UI font family for graphical Sanctum chrome, or nil."
  (cond
   ((eq herdr-sanctum-ui-font 'default) nil)
   ((stringp herdr-sanctum-ui-font) herdr-sanctum-ui-font)
   (t
    (when-let* ((f (or (and frame (display-graphic-p frame) frame)
                       (and (display-graphic-p) (selected-frame))
                       (seq-find (lambda (cand)
                                   (and (display-graphic-p cand)
                                        (not (frame-parameter cand 'parent-frame))))
                                 (frame-list))))
                (vp (face-attribute 'variable-pitch :family f t)))
      (cond
       ((and (stringp vp)
             (not (member vp '("unspecified" "Sans Serif")))
             (not (string-empty-p vp)))
        vp)
       (herdr-sanctum-ui-font-candidates
        (cl-find-if (lambda (cand) (member cand (font-family-list f)))
                    herdr-sanctum-ui-font-candidates)))))))

(defun herdr-sanctum-ui-remapping (bg hl &optional var-family)
  "Build buffer `face-remapping-alist' for Sanctum sidebars and drawers.
BG is the chrome background color, HL is the `hl-line' background, and
VAR-FAMILY overrides the UI font family when provided."
  (let ((family (or var-family (herdr-sanctum-ui-font-family))))
    `((default ,(append (when (and (stringp family) (not (equal family "unspecified")))
                          (list :family family))
                        (list :background bg
                              :height herdr-sanctum-ui-font-height))
               default)
      (hl-line (:background ,hl :extend t) hl-line)
      (mode-line (:inherit herdr-sanctum-status-bar) mode-line)
      (mode-line-active (:inherit herdr-sanctum-status-bar) mode-line-active)
      (mode-line-inactive (:inherit herdr-sanctum-status-bar) mode-line-inactive))))

(defun herdr-sanctum-apply-theme (&rest _)
  "Align Sanctum sidebar, header, status bar, and drawer faces with active theme."
  (clrhash herdr-sanctum--svg-cache)
  (let ((bg (herdr-sanctum-palette-color 'bg-chrome))
        (card (herdr-sanctum-palette-color 'bg-card))
        (hl (herdr-sanctum-palette-color 'bg-hl))
        (sel (herdr-sanctum-palette-color 'bg-selected))
        (pill-active (herdr-sanctum-palette-color 'bg-pill-active))
        (border (herdr-sanctum-palette-color 'border))
        (accent (herdr-sanctum-palette-color 'accent))
        (fg-main (herdr-sanctum-palette-color 'fg-main))
        (fg-dim (herdr-sanctum-palette-color 'fg-dim))
        (fg-alt (herdr-sanctum-palette-color 'fg-alt))
        (yellow (herdr-sanctum-palette-color 'yellow))
        (red (herdr-sanctum-palette-color 'red))
        (green (herdr-sanctum-palette-color 'green))
        (guide (herdr-sanctum-palette-color 'guide))
        (var-family (herdr-sanctum-ui-font-family)))
    (face-spec-set 'herdr-sanctum-sidebar-brand `((t :foreground ,fg-main :weight bold)) 'face-defface-spec)
    (face-spec-set 'herdr-sanctum-sidebar-header `((t :foreground ,fg-dim :weight semi-bold)) 'face-defface-spec)
    (face-spec-set 'herdr-sanctum-sidebar-group-working `((t :foreground ,yellow :weight semi-bold)) 'face-defface-spec)
    (face-spec-set 'herdr-sanctum-sidebar-group-blocked `((t :foreground ,red :weight semi-bold)) 'face-defface-spec)
    (face-spec-set 'herdr-sanctum-sidebar-group-done `((t :foreground ,green :weight semi-bold)) 'face-defface-spec)
    (face-spec-set 'herdr-sanctum-sidebar-space `((t :foreground ,fg-alt :weight medium)) 'face-defface-spec)
    (face-spec-set 'herdr-sanctum-sidebar-space-active `((t :foreground ,fg-main :background ,sel :weight semi-bold :extend t)) 'face-defface-spec)
    (face-spec-set 'herdr-sanctum-sidebar-tab `((t :foreground ,fg-main)) 'face-defface-spec)
    (face-spec-set 'herdr-sanctum-sidebar-tab-dim `((t :foreground ,fg-dim)) 'face-defface-spec)
    (face-spec-set 'herdr-sanctum-sidebar-tab-ws-active `((t :foreground ,fg-alt)) 'face-defface-spec)
    (face-spec-set 'herdr-sanctum-sidebar-tab-active `((t :foreground ,fg-main :background ,sel :weight semi-bold :extend t)) 'face-defface-spec)
    (face-spec-set 'herdr-sanctum-sidebar-guide `((t :foreground ,guide)) 'face-defface-spec)
    (face-spec-set 'herdr-sanctum-sidebar-guide-active `((t :foreground ,accent :background ,sel :weight bold)) 'face-defface-spec)
    (face-spec-set 'herdr-sanctum-sidebar-meta `((t :foreground ,fg-dim)) 'face-defface-spec)
    (face-spec-set 'herdr-sanctum-sidebar-git-behind `((t :foreground ,red :weight medium)) 'face-defface-spec)
    (face-spec-set 'herdr-sanctum-sidebar-git-ahead `((t :foreground ,accent :weight medium)) 'face-defface-spec)
    (face-spec-set 'herdr-sanctum-chrome-bar
                   `((t ,@(when (and (stringp var-family) (not (equal var-family "unspecified")))
                            (list :family var-family))
                        :height ,herdr-sanctum-ui-font-height
                        :background ,bg :foreground ,fg-dim
                        :underline (:color ,border :position t)))
                   'face-defface-spec)
    (face-spec-set 'herdr-sanctum-chrome-pill
                   `((t :inherit herdr-sanctum-chrome-bar :foreground ,fg-alt :background ,bg))
                   'face-defface-spec)
    (face-spec-set 'herdr-sanctum-chrome-pill-active
                   `((t :inherit herdr-sanctum-chrome-bar :foreground ,fg-main :background ,pill-active
                        :weight semi-bold :box (:line-width (-1 . -1) :color ,accent)))
                   'face-defface-spec)
    (face-spec-set 'herdr-sanctum-status-bar
                   `((t ,@(when (and (stringp var-family) (not (equal var-family "unspecified")))
                            (list :family var-family))
                        :height ,herdr-sanctum-ui-font-height
                        :background ,bg :foreground ,fg-dim
                        :overline ,border :box (:line-width (4 . 3) :color ,bg)))
                   'face-defface-spec)
    (face-spec-set 'herdr-sanctum-drawer-tab
                   `((t :foreground ,fg-dim :weight medium)) 'face-defface-spec)
    (face-spec-set 'herdr-sanctum-drawer-tab-active
                   `((t :foreground ,fg-main :weight semi-bold
                        :underline (:color ,accent :position t)))
                   'face-defface-spec)
    (face-spec-set 'herdr-sanctum-drawer-card
                   `((t :background ,card :foreground ,fg-main :extend t))
                   'face-defface-spec)
    (face-spec-set 'herdr-sanctum-drawer-button
                   `((t :background ,sel :foreground ,fg-main :weight semi-bold
                        :box (:line-width (-1 . -1) :color ,guide)))
                   'face-defface-spec)
    (face-spec-set 'herdr-sanctum-banner-info
                   `((t :inherit herdr-sanctum-chrome-bar
                        :background ,sel :foreground ,accent :weight bold :extend t))
                   'face-defface-spec)
    (face-spec-set 'herdr-sanctum-banner-warn
                   `((t :inherit herdr-sanctum-chrome-bar
                        :background ,card :foreground ,yellow :weight bold :extend t))
                   'face-defface-spec)
    (dolist (name (list herdr-sanctum-sidebar-buffer-name
                        herdr-sanctum-drawer-buffer-name))
      (when-let* ((buf (get-buffer name)))
        (with-current-buffer buf
          (setq-local face-remapping-alist
                      (herdr-sanctum-ui-remapping bg hl var-family))))))
  (run-hooks 'herdr-sanctum-theme-change-hook))

(defun herdr-sanctum--frame-theme (frame)
  "Refresh Sanctum chrome faces after a graphical FRAME finishes setup."
  (when (and (display-graphic-p frame)
             (not (frame-parameter frame 'parent-frame)))
    (with-selected-frame frame
      (herdr-sanctum-apply-theme))))

(add-hook 'enable-theme-functions #'herdr-sanctum-apply-theme)
(add-hook 'modus-themes-after-load-theme-hook #'herdr-sanctum-apply-theme)
(add-hook 'ef-themes-post-load-hook #'herdr-sanctum-apply-theme)
(add-hook 'after-make-frame-functions #'herdr-sanctum--frame-theme)
(herdr-sanctum-apply-theme)

;;; SVG Badges & Icons

(defun herdr-sanctum-svg-available-p ()
  "Return non-nil when inline SVG images can be displayed on the selected frame."
  (and (display-graphic-p)
       (fboundp 'image-type-available-p)
       (image-type-available-p 'svg)
       (require 'svg nil t)))

(defun herdr-sanctum-space-badge (label &optional key active-bg)
  "Return a propertized monogram badge for space LABEL (hashed by KEY).
When ACTIVE-BG is non-nil, apply it to the badge face background."
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
          (let* ((cache-key (list 'space letter bg fg))
                 (img (or (gethash cache-key herdr-sanctum--svg-cache)
                          (let ((svg (svg-create 17 17)))
                            (svg-rectangle svg 0 0 17 17 :rx 4 :ry 4 :fill bg)
                            (svg-text svg letter
                                      :x 8.5 :y 12.2
                                      :text-anchor "middle"
                                      :font-family "sans-serif"
                                      :font-size 11
                                      :font-weight "700"
                                      :fill fg)
                            (let ((created (svg-image svg :ascent 'center)))
                              (puthash cache-key created herdr-sanctum--svg-cache)
                              created)))))
            (propertize " "
                        'display img
                        'face (if active-bg `(:background ,active-bg) 'default)))
        (propertize (format " %s " letter)
                    'face `(:background ,bg :foreground ,fg :weight bold))))))

(defun herdr-sanctum--agent-brand-color (agent)
  "Return muted brand color hex for canonical AGENT name."
  (let* ((canon (downcase (or agent "")))
         (spec (cdr (assoc canon herdr-sanctum-agent-brands)))
         (color-spec (plist-get spec :color))
         (fallback (or (plist-get spec :fallback)
                       (herdr-sanctum-palette-color 'fg-alt "#a6b2d4"))))
    (cond
     ((stringp color-spec) color-spec)
     ((symbolp color-spec) (herdr-sanctum-palette-color color-spec fallback))
     (t (herdr-sanctum-palette-color 'fg-alt "#a6b2d4")))))

(defun herdr-sanctum--agent-svg-file-raw (canon)
  "Read and cache raw SVG text for agent CANON from the icon directory."
  (when (and herdr-sanctum-agent-icon-directory
             (not (string-empty-p canon)))
    (let* ((spec (cdr (assoc canon herdr-sanctum-agent-brands)))
           (name (or (plist-get spec :file) canon))
           (path (expand-file-name (concat name ".svg")
                                   herdr-sanctum-agent-icon-directory))
           (cached (gethash path herdr-sanctum--svg-file-cache 'miss)))
      (if (not (eq cached 'miss))
          cached
        (let ((raw (when (file-readable-p path)
                     (with-temp-buffer
                       (insert-file-contents path)
                       (buffer-string)))))
          (puthash path raw herdr-sanctum--svg-file-cache)
          raw)))))

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

;;;###autoload
(defun herdr-sanctum-toggle-agent-icon-style ()
  "Toggle coding-agent icons between `svg-files' and `builtin' styles."
  (interactive)
  (let ((next (if (eq herdr-sanctum-agent-icon-style 'svg-files) 'builtin 'svg-files)))
    (setq herdr-sanctum-agent-icon-style next)
    (clrhash herdr-sanctum--svg-cache)
    (run-hooks 'herdr-sanctum-theme-change-hook)
    (message "Sanctum agent icon style: %s" next)))

(defun herdr-sanctum-agent-badge (agent status &optional active-bg)
  "Return a propertized 14x14 brand icon for AGENT with STATUS overlay."
  (let* ((canon (downcase (string-trim (or agent ""))))
         (fallback (pcase status
                     ("blocked" (cons "◆" 'warning))
                     ("done"    (cons "✓" 'success))
                     ("working" (cons "●" 'font-lock-function-name-face))
                     (_         (cons "○" 'herdr-sanctum-sidebar-meta))))
         (glyph (car fallback))
         (base-face (cdr fallback))
         (face (if active-bg (list base-face `(:background ,active-bg)) base-face)))
    (if (and (not (eq herdr-sanctum-agent-icon-style 'unicode))
             (herdr-sanctum-svg-available-p))
        (let* ((color (herdr-sanctum--agent-brand-color canon))
               (opacity (if (equal status "idle") 0.58 1.0))
               (dot-color (pcase status
                            ("blocked" (herdr-sanctum-palette-color 'red))
                            ("done"    (herdr-sanctum-palette-color 'green))))
               (cutout-bg (or active-bg (herdr-sanctum-palette-color 'bg-chrome)))
               (style herdr-sanctum-agent-icon-style)
               (cache-key (list 'agent-brand style canon (or status "idle") color active-bg))
               (img (or (gethash cache-key herdr-sanctum--svg-cache)
                        (when-let* ((file-img
                                     (and (eq style 'svg-files)
                                          (herdr-sanctum--agent-svg-file-image
                                           canon color opacity dot-color cutout-bg active-bg))))
                          (puthash cache-key file-img herdr-sanctum--svg-cache)
                          file-img)
                        (let ((svg (svg-create 14 14)))
                          (when active-bg
                            (svg-rectangle svg 0 0 14 14 :fill active-bg))
                          (pcase canon
                            ("pi"
                             (svg-line svg 2.0 3.5 12.0 3.5
                                       :stroke color :stroke-width 1.8
                                       :stroke-linecap "round" :stroke-opacity opacity)
                             (svg-line svg 4.7 3.8 4.7 11.2
                                       :stroke color :stroke-width 1.7
                                       :stroke-linecap "round" :stroke-opacity opacity)
                             (svg-polyline svg '((9.2 . 3.8) (9.2 . 10.6) (10.6 . 11.3))
                                           :fill "none" :stroke color :stroke-width 1.7
                                           :stroke-linecap "round" :stroke-linejoin "round"
                                           :stroke-opacity opacity))
                            ((or "claude" "claude-code")
                             (svg-line svg 7.0 2.0 7.0 12.0
                                       :stroke color :stroke-width 1.55
                                       :stroke-linecap "round" :stroke-opacity opacity)
                             (svg-line svg 2.0 7.0 12.0 7.0
                                       :stroke color :stroke-width 1.55
                                       :stroke-linecap "round" :stroke-opacity opacity)
                             (svg-line svg 3.6 3.6 10.4 10.4
                                       :stroke color :stroke-width 1.45
                                       :stroke-linecap "round" :stroke-opacity opacity)
                             (svg-line svg 10.4 3.6 3.6 10.4
                                       :stroke color :stroke-width 1.45
                                       :stroke-linecap "round" :stroke-opacity opacity)
                             (svg-circle svg 7.0 7.0 1.3 :fill color :fill-opacity opacity))
                            ("codex"
                             (svg-rectangle svg 1.0 1.5 12.0 11.0 :rx 3 :ry 3
                                            :fill color :fill-opacity (* 0.14 opacity)
                                            :stroke color :stroke-width 1.1
                                            :stroke-opacity (* 0.78 opacity))
                             (svg-polyline svg '((3.6 . 4.7) (6.2 . 7.0) (3.6 . 9.3))
                                           :fill "none" :stroke color :stroke-width 1.5
                                           :stroke-linecap "round" :stroke-linejoin "round"
                                           :stroke-opacity opacity)
                             (svg-line svg 7.4 9.3 10.4 9.3
                                       :stroke color :stroke-width 1.5
                                       :stroke-linecap "round" :stroke-opacity opacity))
                            ("gemini"
                             (svg-polygon svg '((7.0 . 1.2) (8.6 . 5.4) (12.8 . 7.0)
                                                (8.6 . 8.6) (7.0 . 12.8) (5.4 . 8.6)
                                                (1.2 . 7.0) (5.4 . 5.4))
                                          :fill color :fill-opacity opacity))
                            (_
                             (let ((letter (upcase (if (string-empty-p canon)
                                                       "A"
                                                     (substring canon 0 1)))))
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
                                         :fill color :fill-opacity opacity))))
                          (when dot-color
                            (svg-circle svg 11.4 11.4 2.4
                                        :fill dot-color
                                        :stroke cutout-bg
                                        :stroke-width 1.2))
                          (let ((created (svg-image svg :ascent 'center)))
                            (puthash cache-key created herdr-sanctum--svg-cache)
                            created)))))
          (propertize glyph 'display img 'face face))
      (propertize glyph 'face face))))

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
                               (active 'herdr-sanctum-sidebar-guide-active)
                               (t 'herdr-sanctum-sidebar-guide))))
        (propertize " "
                    'display '(space :width (8))
                    'face (if active-bg `(:background ,active-bg) 'default)))
    (cond
     (active (propertize "▎" 'face 'herdr-sanctum-sidebar-guide-active))
     (guide  (propertize "│" 'face 'herdr-sanctum-sidebar-guide))
     (t      " "))))

;;; Shared Layout & Formatting Helpers

(defun herdr-sanctum-status-glyph (status &optional active-bg)
  "Return a propertized single-column glyph for agent STATUS, or nil."
  (let* ((spec (pcase status
                 ("blocked" (cons "◆" 'warning))
                 ("working" (cons "●" 'font-lock-function-name-face))
                 ("done"    (cons "✓" 'success))))
         (glyph (car spec))
         (base-face (cdr spec)))
    (when glyph
      (propertize glyph
                  'face (if active-bg
                            (list base-face `(:background ,active-bg))
                          base-face)))))

(defun herdr-sanctum-truncate (text max-width &optional bg-face)
  "Truncate TEXT to MAX-WIDTH display columns using an ellipsis."
  (if (<= max-width 0)
      ""
    (let ((ellipsis (if bg-face (propertize "…" 'face bg-face) "…")))
      (truncate-string-to-width text max-width nil nil ellipsis))))

(defun herdr-sanctum-truncate-px (text max-px &optional bg-face)
  "Truncate propertized TEXT to fit within MAX-PX pixels on graphical frames."
  (let ((buf (current-buffer)))
    (if (or (<= max-px 0) (<= (string-pixel-width text buf) max-px))
        text
      (let* ((ellipsis (if bg-face (propertize "…" 'face bg-face) "…"))
             (ell-px (string-pixel-width ellipsis buf))
             (limit (max 8 (- max-px ell-px)))
             (lo 0)
             (hi (length text)))
        (while (< lo hi)
          (let ((mid (/ (+ lo hi 1) 2)))
            (if (<= (string-pixel-width (substring text 0 mid) buf) limit)
                (setq lo mid)
              (setq hi (1- mid)))))
        (concat (substring text 0 lo) ellipsis)))))

(defun herdr-sanctum--pad-row (left right width &optional bg-face)
  "Align LEFT and RIGHT within WIDTH columns, optionally filling with BG-FACE."
  (let* ((rw (string-width right))
         (max-left (max 1 (- width (if (> rw 0) (1+ rw) 0))))
         (trimmed (if (> (string-width left) max-left)
                      (herdr-sanctum-truncate left max-left bg-face)
                    left))
         (gap (max (if (> rw 0) 1 0) (- width (string-width trimmed) rw)))
         (pad (if bg-face
                  (propertize (make-string gap ?\s) 'face bg-face)
                (make-string gap ?\s))))
    (concat trimmed pad right)))

(defun herdr-sanctum-format-row (left right width win &optional bg-face)
  "Align LEFT and RIGHT for WIN (pixel-exact on GUI, column-aligned on TTY)."
  (if (and (display-graphic-p)
           (window-live-p win)
           (fboundp 'string-pixel-width))
      (let* ((buf (current-buffer))
             ;; With zero fringes and `truncate-lines', Emacs reserves one
             ;; rightmost cell for the truncation slot even when it is blank.
             (usable-px (max 80 (- (window-body-width win t)
                                   (frame-char-width (window-frame win))
                                   2)))
             (right-px (if (string-empty-p right) 0 (string-pixel-width right buf)))
             (gap-px (if (> right-px 0) 10 0))
             (align-px (max 0 (- usable-px right-px)))
             (trimmed (herdr-sanctum-truncate-px
                       left (- align-px gap-px) bg-face)))
        (if (string-empty-p right)
            trimmed
          (concat trimmed
                  (propertize " "
                              'display `(space :align-to (,align-px))
                              'face (or bg-face 'default))
                  right)))
    (herdr-sanctum--pad-row left right width bg-face)))

(defun herdr-sanctum-divider-line (width &optional win)
  "Return a subtle full-width horizontal hairline divider string."
  (if (and (display-graphic-p) (window-live-p win))
      (let ((px (max 40 (- (window-body-width win t)
                           (frame-char-width (window-frame win))
                           2)))
            (color (herdr-sanctum-palette-color 'border)))
        (propertize " "
                    'display `(space :align-to (,px))
                    'face `(:underline (:color ,color :position t))))
    (propertize (make-string (max 8 width) ?─) 'face 'herdr-sanctum-sidebar-guide)))

(defun herdr-sanctum-make-chrome-button (label fn face help &optional area)
  "Create a clickable chrome button LABEL running FN in AREA.
AREA is `header-line' (default) or `mode-line'; FACE and HELP style it."
  (let ((map (make-sparse-keymap))
        (key (vector (or area 'header-line) 'mouse-1)))
    (define-key map key
                (lambda ()
                  (interactive)
                  (funcall fn)))
    (propertize label
                'face face
                'mouse-face 'mode-line-highlight
                'local-map map
                'help-echo help)))

(provide 'herdr-sanctum-theme)
;;; herdr-sanctum-theme.el ends here
