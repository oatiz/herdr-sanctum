;;; herdr-sanctum-ui.el --- Shared side-panel UI, layout & chrome widgets -*- lexical-binding: t; -*-

;; Copyright (C) 2026 oatiz

;; Author: oatiz <iamoatiz@gmail.com>
;; URL: https://github.com/oatiz/herdr-sanctum
;; Keywords: tools, frames

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

;; Shared UI and side-panel infrastructure for `herdr-sanctum':
;;  - Unified panel/terminal face remapping, fringe padding, and lifecycle
;;  - Buffer-local side-panel controller (per-window width resize tracking,
;;    deferred dirty refresh on redisplay, and window scroll save/restore)
;;  - Emacs 30.1+ pixel-exact row, header-line, truncation, and chrome buttons
;;  - Shared Git ahead/behind/dirty formatter and outside-opening dispatcher

;;; Code:

(require 'cl-lib)
(require 'face-remap)
(require 'subr-x)
(require 'herdr-sanctum-theme)
(require 'herdr-sanctum-rpc)

(declare-function evil-set-initial-state "evil" (mode state))
(declare-function truncate-string-pixelwise "subr-x"
                  (string max-pixels &optional buffer ellipsis ellipsis-pixels))
(declare-function tab-bar--current-tab "tab-bar" (&optional frame))
(declare-function tab-bar-new-tab "tab-bar" (&optional arg to-index))
(declare-function tab-bar-switch-to-recent-tab "tab-bar" (&optional arg))
(declare-function tab-bar-tabs "tab-bar" (&optional frame))
(defvar hl-line-sticky-flag)

(defgroup herdr-sanctum-ui nil
  "Shared side-panel layout and chrome widgets for `herdr-sanctum'."
  :group 'herdr-sanctum
  :prefix "herdr-sanctum-")

(defvar herdr-sanctum--chrome-keymap-cache (make-hash-table :test #'equal)
  "Cache of reusable chrome button keymaps keyed by `(AREA . KEY)'.")

(defvar-local herdr-sanctum--panel-dirty nil
  "Non-nil when the current side-panel buffer deferred a render while hidden.")

(defvar-local herdr-sanctum--panel-render-fn nil
  "Buffer-local render function `(WIN)' for the current side-panel buffer.")

(defvar-local herdr-sanctum--panel-size-change-fn nil
  "Buffer-local resize callback `(WIN)' for the current side-panel buffer.
Defaults to `herdr-sanctum--panel-render-fn' when nil.")

(defvar-local herdr-sanctum--panel-after-flush-fn nil
  "Optional zero-argument callback run after flushing a deferred panel render.")

(defvar-local herdr-sanctum--ellipsis-px-cache nil
  "Cached `(KEY . ALIST)' mapping BG-FACE to ellipsis pixel width.")

(defvar-local herdr-sanctum--panel-remap-cookies nil
  "List of `face-remap-add-relative' cookies managed by Sanctum in a panel.")

(defvar herdr-sanctum--evil-registered-modes nil
  "List of major mode symbols registered via `herdr-sanctum-configure-evil-mode'.")

(defun herdr-sanctum--resolve-buffer (&optional buf-or-name)
  "Return live buffer for BUF-OR-NAME (default current buffer), or nil."
  (let ((buf (if buf-or-name
                 (get-buffer buf-or-name)
               (current-buffer))))
    (when (buffer-live-p buf)
      buf)))

(defun herdr-sanctum--set-panel-fringe-width (sym val)
  "Set `herdr-sanctum-panel-fringe-width' SYM to VAL and refresh live panels."
  (set-default sym val)
  (when (and (featurep 'herdr-sanctum-ui)
             (fboundp 'herdr-sanctum-apply-panel-padding)
             (fboundp 'herdr-sanctum-panel-refresh-or-mark-dirty))
    (dolist (name (list herdr-sanctum-sidebar-buffer-name
                        herdr-sanctum-drawer-buffer-name))
      (when-let* ((buf (get-buffer name)))
        (let ((wins (get-buffer-window-list buf nil t)))
          (if wins
              (dolist (win wins)
                (herdr-sanctum-apply-panel-padding win))
            (with-current-buffer buf
              (herdr-sanctum-apply-panel-padding))))
        (herdr-sanctum-panel-refresh-or-mark-dirty name)))
    (force-mode-line-update t)))

(defcustom herdr-sanctum-panel-fringe-width 10
  "Horizontal fringe width in pixels on graphical Sanctum side panels.
Applied symmetrically to left and right fringes of `*sanctum-spaces*' and
`*sanctum-drawer*' (with zero window margins) so panel padding inherits
`bg-panel' seamlessly."
  :type 'natnum
  :group 'herdr-sanctum-ui
  :set #'herdr-sanctum--set-panel-fringe-width)

(defun herdr-sanctum--set-panel-line-spacing (sym val)
  "Set `herdr-sanctum-panel-line-spacing' SYM to VAL and update live panels."
  (set-default sym val)
  (when (featurep 'herdr-sanctum-ui)
    (dolist (name (list herdr-sanctum-sidebar-buffer-name
                        herdr-sanctum-drawer-buffer-name))
      (when-let* ((buf (get-buffer name)))
        (with-current-buffer buf
          (setq-local line-spacing val))))))

(defcustom herdr-sanctum-panel-line-spacing 0.22
  "Buffer-local `line-spacing' used in Sanctum side-panel buffers.
Applied to `*sanctum-spaces*' and `*sanctum-drawer*'."
  :type '(choice (const :tag "Default" nil)
                 (number :tag "Line spacing"))
  :group 'herdr-sanctum
  :set #'herdr-sanctum--set-panel-line-spacing)

(defun herdr-sanctum--set-evil-initial-state (sym val)
  "Set `herdr-sanctum-evil-initial-state' SYM to VAL and re-apply to modes."
  (set-default sym val)
  (when (and (featurep 'herdr-sanctum-ui)
             val
             (fboundp 'evil-set-initial-state))
    (dolist (mode herdr-sanctum--evil-registered-modes)
      (evil-set-initial-state mode val))))

(defcustom herdr-sanctum-evil-initial-state 'emacs
  "Initial Evil state used in Sanctum side-panel buffers.
Applies to major modes registered via `herdr-sanctum-configure-evil-mode'
when `evil' is loaded.  Set to nil to leave Evil's default state untouched."
  :type '(choice (const :tag "Emacs state (use Sanctum's Vi/Emacs keys)" emacs)
                 (const :tag "Evil motion state" motion)
                 (const :tag "Evil normal state" normal)
                 (const :tag "Do not configure Evil" nil))
  :group 'herdr-sanctum
  :set #'herdr-sanctum--set-evil-initial-state)

(defun herdr-sanctum--apply-evil-state-all ()
  "Apply `herdr-sanctum-evil-initial-state' to all registered panel modes."
  (when (and herdr-sanctum-evil-initial-state
             (fboundp 'evil-set-initial-state))
    (dolist (mode herdr-sanctum--evil-registered-modes)
      (evil-set-initial-state mode herdr-sanctum-evil-initial-state))))

(defun herdr-sanctum-configure-evil-mode (mode)
  "Register major MODE to use `herdr-sanctum-evil-initial-state' under Evil.
Applies immediately when `evil-set-initial-state' is already defined, or
defers until `evil' is loaded."
  (cl-pushnew mode herdr-sanctum--evil-registered-modes :test #'eq)
  (if (fboundp 'evil-set-initial-state)
      (when herdr-sanctum-evil-initial-state
        (evil-set-initial-state mode herdr-sanctum-evil-initial-state))
    (eval-after-load 'evil #'herdr-sanctum--apply-evil-state-all)))

(defun herdr-sanctum--set-hide-scroll-bars (sym val)
  "Set `herdr-sanctum-hide-scroll-bars' SYM to VAL and refresh Sanctum windows."
  (set-default sym val)
  (when (and (featurep 'herdr-sanctum-ui)
             (fboundp 'herdr-sanctum-apply-window-scroll-bars))
    (dolist (name (herdr-sanctum-ui-buffers))
      (when-let* ((buf (get-buffer name)))
        (dolist (win (get-buffer-window-list buf nil t))
          (herdr-sanctum-apply-window-scroll-bars win))))
    (when (fboundp 'herdr-sanctum-panel-refresh-or-mark-dirty)
      (dolist (name (list herdr-sanctum-sidebar-buffer-name
                          herdr-sanctum-drawer-buffer-name))
        (herdr-sanctum-panel-refresh-or-mark-dirty name)))))

(defcustom herdr-sanctum-hide-scroll-bars t
  "When non-nil, hide scroll bars locally on Sanctum windows on GUI frames.
Applies per-window via `set-window-scroll-bars' without altering global
`scroll-bar-mode'."
  :type 'boolean
  :group 'herdr-sanctum-ui
  :set #'herdr-sanctum--set-hide-scroll-bars)

(defun herdr-sanctum-apply-window-scroll-bars (window)
  "Apply `herdr-sanctum-hide-scroll-bars' locally to live WINDOW."
  (when (and (window-live-p window)
             (display-graphic-p (window-frame window)))
    (if herdr-sanctum-hide-scroll-bars
        (set-window-scroll-bars window 0 nil 0 nil)
      (set-window-scroll-bars window nil nil nil nil))))

(defcustom herdr-sanctum-min-terminal-width 40
  "Minimum center terminal columns to keep when toggling opposite side panels.
When non-nil and toggling a side panel open on a narrow frame would squeeze
the center area below this width, automatically collapse the opposite side
panel.  Set to nil to always allow both side panels simultaneously."
  :type '(choice (const :tag "Disable auto-collapse" nil)
                 (integer :tag "Minimum columns"))
  :group 'herdr-sanctum-ui)

(defun herdr-sanctum-invoke-command-or-fn (fn &rest args)
  "Invoke FN with ARGS, using `call-interactively' for commands requiring args.
When ARGS is nil and FN is an interactive command whose minimum arity is
positive (such as `find-file' or `vc-root-diff'), invoke it via
`call-interactively' so interactive arguments are prompted instead of
signaling `wrong-number-of-arguments'.  Otherwise call `(apply fn args)'."
  (if (and (null args)
           (commandp fn)
           (let ((arity (ignore-errors (func-arity fn))))
             (and (consp arity)
                  (numberp (car arity))
                  (> (car arity) 0))))
      (call-interactively fn)
    (apply fn args)))

(defun herdr-sanctum-default-open-outside (fn dir)
  "Default outside-opening dispatcher running FN with `default-directory' DIR.
When `herdr-sanctum-display-mode' is `tab', `tab-bar-mode' is active,
`herdr-sanctum-tab-routing-mode' has no active rules or fallback tab, and
the active window belongs to the dedicated Sanctum tab, switch to the
previous tab (or create a new tab if Sanctum is the sole tab) before
running FN so `*sanctum*' remains unsplit.
When `herdr-sanctum-display-mode' is `frame' and the selected frame is the
dedicated Sanctum frame, focus the most recently used visible non-Sanctum
frame (or create a new frame) before running FN."
  (let ((target-dir (or dir default-directory)))
    (cond
     ((and (eq (bound-and-true-p herdr-sanctum-display-mode) 'tab)
           (bound-and-true-p tab-bar-mode)
           (not (and (bound-and-true-p herdr-sanctum-tab-routing-mode)
                     (or (bound-and-true-p herdr-sanctum-tab-routing-rules)
                         (bound-and-true-p herdr-sanctum-tab-routing-fallback-tab))))
           (member (buffer-name (window-buffer)) (herdr-sanctum-ui-buffers))
           (let ((tab-name (bound-and-true-p herdr-sanctum-tab-name)))
             (and tab-name
                  (equal (alist-get 'name (tab-bar--current-tab)) tab-name))))
      (if (> (length (tab-bar-tabs)) 1)
          (tab-bar-switch-to-recent-tab)
        (tab-bar-new-tab)))
     ((and (eq (bound-and-true-p herdr-sanctum-display-mode) 'frame)
           (let ((frame-name (bound-and-true-p herdr-sanctum-frame-name)))
             (and frame-name
                  (equal (frame-parameter (selected-frame) 'name) frame-name))))
      (let ((cur-frame (selected-frame))
            (frame-name (bound-and-true-p herdr-sanctum-frame-name))
            (best nil)
            (best-time nil))
        (dolist (f (frame-list))
          (when (and (not (eq f cur-frame))
                     (eq (frame-visible-p f) t)
                     (not (frame-parameter f 'parent-frame))
                     (not (eq (frame-parameter f 'minibuffer) 'only))
                     (not (equal (frame-parameter f 'name) frame-name)))
            (let* ((win (ignore-errors (frame-selected-window f)))
                   (use-time (if (and win (window-live-p win))
                                 (or (ignore-errors (window-use-time win)) 0)
                               0)))
              (when (or (null best) (> use-time best-time))
                (setq best f
                      best-time use-time)))))
        (select-frame-set-input-focus (or best (make-frame))))))
    (let ((default-directory target-dir))
      (herdr-sanctum-invoke-command-or-fn fn))))

(defcustom herdr-sanctum-open-outside-function #'herdr-sanctum-default-open-outside
  "Function `(FN DIR)' used when opening files or Git views from Sanctum.
Defaults to `herdr-sanctum-default-open-outside', which switches away from
the dedicated Sanctum tab (in `tab' mode without active routing rules or
fallback) or Sanctum frame (in `frame' mode).  When nil, simply invoke FN
via `herdr-sanctum-invoke-command-or-fn' with `default-directory' bound
to DIR."
  :type '(choice (const :tag "Current context (no routing)" nil)
                 (function :tag "Custom dispatcher"))
  :group 'herdr-sanctum-ui)

(defun herdr-sanctum-open-outside (fn &optional dir)
  "Run FN in DIR (default focused Herdr directory), respecting outside routing."
  (let ((target-dir (or dir (herdr-sanctum-focused-dir) default-directory)))
    (if (functionp herdr-sanctum-open-outside-function)
        (funcall herdr-sanctum-open-outside-function fn target-dir)
      (let ((default-directory target-dir))
        (herdr-sanctum-invoke-command-or-fn fn)))))

;;; Face Remapping & Panel Lifecycle

(defconst herdr-sanctum-header-line-remapping
  '((header-line (:inherit herdr-sanctum-chrome-bar) header-line)
    (header-line-active (:inherit herdr-sanctum-chrome-bar) header-line-active)
    (header-line-inactive (:inherit herdr-sanctum-chrome-bar) header-line-inactive))
  "Buffer `face-remapping-alist' entries aligning header-lines across panes.")

(defun herdr-sanctum-ui-remapping (bg hl &optional var-family)
  "Build buffer `face-remapping-alist' for Sanctum sidebars and drawers.
BG is the panel background color, HL is the `hl-line' background, and
VAR-FAMILY overrides the UI font family when provided.  When
`herdr-sanctum-ui-font-height' is a relative float, neutralizes compound
scaling on `herdr-sanctum-chrome-bar' and `herdr-sanctum-status-bar' so
header/status bars match body scaling and track `global-text-scale-adjust'."
  (let ((family (or var-family (herdr-sanctum-ui-font-family)))
        (float-height-p (floatp herdr-sanctum-ui-font-height)))
    `((default ,(append (when (and (stringp family) (not (equal family "unspecified")))
                          (list :family family))
                        (list :background bg
                              :height herdr-sanctum-ui-font-height))
               default)
      (fringe (:background ,bg) fringe)
      (hl-line (:background ,hl :extend t) hl-line)
      ,@(when float-height-p
          '((herdr-sanctum-chrome-bar (:height 1.0) herdr-sanctum-chrome-bar)
            (herdr-sanctum-status-bar (:height 1.0) herdr-sanctum-status-bar)))
      ,@herdr-sanctum-header-line-remapping
      (tab-line (:inherit herdr-sanctum-chrome-bar) tab-line)
      (mode-line (:inherit herdr-sanctum-status-bar) mode-line)
      (mode-line-active (:inherit herdr-sanctum-status-bar) mode-line-active)
      (mode-line-inactive (:inherit herdr-sanctum-status-bar) mode-line-inactive))))

(defun herdr-sanctum-apply-terminal-header-remapping (&optional buf)
  "Apply Sanctum header-line face remappings to terminal buffer BUF.
BUF may be a buffer, buffer name, or nil (defaulting to the current buffer).
Preserves all non-header-line entries in `face-remapping-alist'."
  (when-let* ((target (herdr-sanctum--resolve-buffer buf)))
    (with-current-buffer target
      (let ((others (cl-remove-if
                     (lambda (entry)
                       (assq (car-safe entry)
                             herdr-sanctum-header-line-remapping))
                     face-remapping-alist)))
        (setq-local face-remapping-alist
                    (append (copy-alist herdr-sanctum-header-line-remapping)
                            others))))))

(defun herdr-sanctum-apply-panel-remapping (&optional buf bg hl var-family)
  "Apply Sanctum side-panel face remappings non-destructively to BUF.
BUF may be a buffer, buffer name, or nil (defaulting to the current buffer).
Optional BG, HL, and VAR-FAMILY override the panel background color
defaulting to `bg-panel' (falling back to `bg-chrome'), `hl-line'
background (`bg-hl'), and UI font family.  Removes only previously managed
`herdr-sanctum--panel-remap-cookies' so user `text-scale-mode',
`buffer-face-mode', and custom `face-remap-add-relative' entries survive
theme changes."
  (when-let* ((target (herdr-sanctum--resolve-buffer buf)))
    (let ((panel-bg (or bg
                        (herdr-sanctum-palette-color 'bg-panel)
                        (herdr-sanctum-palette-color 'bg-chrome)))
          (hl-bg (or hl (herdr-sanctum-palette-color 'bg-hl))))
      (with-current-buffer target
        (dolist (cookie herdr-sanctum--panel-remap-cookies)
          (face-remap-remove-relative cookie))
        (setq herdr-sanctum--panel-remap-cookies nil)
        (dolist (entry (herdr-sanctum-ui-remapping panel-bg hl-bg var-family))
          (push (face-remap-add-relative (car entry) (cadr entry))
                herdr-sanctum--panel-remap-cookies))))))

(defun herdr-sanctum--sync-buffer-remappings ()
  "Update `face-remapping-alist' on live Sanctum UI and terminal buffers."
  (let ((bg (or (herdr-sanctum-palette-color 'bg-panel)
                (herdr-sanctum-palette-color 'bg-chrome)))
        (hl (herdr-sanctum-palette-color 'bg-hl))
        (var-family (herdr-sanctum-ui-font-family)))
    (dolist (name (list herdr-sanctum-sidebar-buffer-name
                        herdr-sanctum-drawer-buffer-name))
      (when-let* ((buf (get-buffer name)))
        (herdr-sanctum-apply-panel-remapping buf bg hl var-family)))
    (when-let* ((term-buf (get-buffer herdr-sanctum-buffer-name)))
      (herdr-sanctum-apply-terminal-header-remapping term-buf))))

;; Run ahead of default-depth panel refresh hooks so updated `:family' and
;; `:height' remappings are active before panels measure pixel widths.
(add-hook 'herdr-sanctum-theme-change-hook #'herdr-sanctum--sync-buffer-remappings -50)

(defun herdr-sanctum-apply-panel-padding (&optional window)
  "Apply symmetric fringe padding and zero margins for a Sanctum side panel.
Sets buffer-local margins to 0 and fringes to `herdr-sanctum-panel-fringe-width'
on graphical frames (0 on TTY) in WINDOW's buffer (or the current buffer when
WINDOW is nil), suppressing fringe truncation bitmaps.  When WINDOW is live (or
the buffer is already displayed on the selected frame), updates window margins
and fringes immediately."
  (let* ((win (and (window-live-p window) window))
         (buf (if win (window-buffer win) (current-buffer)))
         (frame (if win (window-frame win) (selected-frame)))
         (fringe (if (display-graphic-p frame)
                     (max 0 (or herdr-sanctum-panel-fringe-width 0))
                   0)))
    (with-current-buffer buf
      (setq-local left-margin-width 0
                  right-margin-width 0
                  left-fringe-width fringe
                  right-fringe-width fringe
                  indicate-empty-lines nil
                  fringe-indicator-alist
                  (cons '(truncation nil nil)
                        (assq-delete-all 'truncation
                                         (copy-alist fringe-indicator-alist)))))
    (when-let* ((target (or win (get-buffer-window buf frame))))
      (set-window-margins target 0 0)
      (set-window-fringes target fringe fringe)
      (herdr-sanctum-apply-window-scroll-bars target))))

(defun herdr-sanctum-panel-window (&optional buf)
  "Return the preferred live window displaying BUF (default current buffer).
Searches the selected frame first, then visible frames, then all frames.
BUF may be a buffer, buffer name, or nil."
  (when-let* ((b (herdr-sanctum--resolve-buffer buf)))
    (or (get-buffer-window b)
        (get-buffer-window b 'visible)
        (get-buffer-window b t))))

(defun herdr-sanctum--panel-window-width (win)
  "Return tracked body width for panel window WIN (pixels on GUI, cols on TTY)."
  (window-body-width win (display-graphic-p (window-frame win))))

(defun herdr-sanctum--panel-hook-target (window-or-frame)
  "Resolve `(BUFFER . WINDOWS)' for a panel window hook argument WINDOW-OR-FRAME.
When WINDOW-OR-FRAME is a live window with a live buffer, target that window's
buffer directly because Emacs 30.1/30.2 keeps `current-buffer' on the selected
window when invoking buffer-local window hooks.  Otherwise treat
WINDOW-OR-FRAME as a frame specifier for `get-buffer-window-list' on the
current buffer."
  (if (windowp window-or-frame)
      (when (window-live-p window-or-frame)
        (let ((buf (window-buffer window-or-frame)))
          (when (buffer-live-p buf)
            (cons buf (list window-or-frame)))))
    (let ((buf (current-buffer)))
      (when (buffer-live-p buf)
        (when-let* ((wins (get-buffer-window-list buf nil window-or-frame)))
          (cons buf wins))))))

(defun herdr-sanctum-panel-record-size (&optional win)
  "Record the current body width on panel window WIN.
WIN defaults to `herdr-sanctum-panel-window' for the current buffer.
Stores width in the window parameter `herdr-sanctum-panel-width' and
returns it, or nil when no live window is found."
  (when-let* ((w (or (and (window-live-p win) win)
                     (herdr-sanctum-panel-window))))
    (let ((width (herdr-sanctum--panel-window-width w)))
      (set-window-parameter w 'herdr-sanctum-panel-width width)
      width)))

(defun herdr-sanctum--panel-on-window-size-change (window-or-frame)
  "Re-render the target panel buffer when an affected window's width changed."
  (when-let* ((target (herdr-sanctum--panel-hook-target window-or-frame)))
    (with-current-buffer (car target)
      (when herdr-sanctum--panel-render-fn
        (when-let* ((fn (or herdr-sanctum--panel-size-change-fn
                            herdr-sanctum--panel-render-fn)))
          (dolist (win (cdr target))
            (when (window-live-p win)
              (let ((width (herdr-sanctum--panel-window-width win)))
                (unless (equal width (window-parameter win 'herdr-sanctum-panel-width))
                  (set-window-parameter win 'herdr-sanctum-panel-width width)
                  (setq herdr-sanctum--panel-dirty nil)
                  (funcall fn win))))))))))

(defun herdr-sanctum--panel-on-window-buffer-change (window-or-frame)
  "Flush deferred render for the target panel when it becomes displayed."
  (when-let* ((target (herdr-sanctum--panel-hook-target window-or-frame)))
    (with-current-buffer (car target)
      (when (and herdr-sanctum--panel-dirty
                 (functionp herdr-sanctum--panel-render-fn))
        (let ((win (cadr target)))
          (when (window-live-p win)
            (setq herdr-sanctum--panel-dirty nil)
            (herdr-sanctum-panel-record-size win)
            (funcall herdr-sanctum--panel-render-fn win)
            (when (functionp herdr-sanctum--panel-after-flush-fn)
              (funcall herdr-sanctum--panel-after-flush-fn))))))))

(defun herdr-sanctum-init-panel-buffer (render-fn &optional on-size-change-fn on-flush-fn)
  "Initialize shared side-panel defaults and buffer-local controller hooks.
RENDER-FN is called with `(WIN)' to render the panel buffer.
Optional ON-SIZE-CHANGE-FN (defaulting to RENDER-FN) is called with `(WIN)'
when a window displaying the panel changes width.
Optional ON-FLUSH-FN is a zero-argument function invoked after a deferred
dirty render is flushed when the panel becomes displayed."
  (let ((table (make-display-table)))
    (set-display-table-slot table 'truncation ?\s)
    (setq-local buffer-display-table table))
  (setq-local truncate-lines t
              cursor-type nil
              cursor-in-non-selected-windows nil
              hl-line-sticky-flag nil
              mode-line-format nil
              header-line-format nil
              line-spacing herdr-sanctum-panel-line-spacing
              herdr-sanctum--panel-render-fn render-fn
              herdr-sanctum--panel-size-change-fn (or on-size-change-fn render-fn)
              herdr-sanctum--panel-after-flush-fn on-flush-fn)
  (herdr-sanctum-apply-panel-padding)
  (hl-line-mode 1)
  (herdr-sanctum-apply-panel-remapping)
  (add-hook 'kill-buffer-hook #'herdr-sanctum-maybe-disconnect-on-kill nil t)
  (add-hook 'window-size-change-functions
            #'herdr-sanctum--panel-on-window-size-change nil t)
  (add-hook 'window-buffer-change-functions
            #'herdr-sanctum--panel-on-window-buffer-change nil t))

(defun herdr-sanctum-show-panel (buffer-name mode-fn side width render-fn &optional select slot)
  "Show side panel BUFFER-NAME on SIDE with WIDTH using MODE-FN and RENDER-FN.
RENDER-FN is called with `(WIN)' inside the panel buffer after display
(defaulting to the buffer's `herdr-sanctum--panel-render-fn' when nil).
When SELECT is non-nil, focus the displayed side window; otherwise preserve
the previously selected window.
Optional SLOT sets the side-window slot (default 0) so multiple panels on
the same SIDE stack instead of evicting each other."
  (let* ((buf (get-buffer-create buffer-name))
         (origin (selected-window))
         (action `((display-buffer-reuse-window display-buffer-in-side-window)
                   (side . ,side)
                   (slot . ,(or slot 0))
                   (window-width . ,width)
                   (preserve-size . (t . nil))
                   (dedicated . t)
                   (window-parameters . ((no-delete-other-windows . t))))))
    (with-current-buffer buf
      (unless (derived-mode-p mode-fn)
        (funcall mode-fn)))
    (let ((win (display-buffer buf action)))
      (when (window-live-p win)
        (herdr-sanctum-apply-panel-padding win)
        (window-preserve-size win t t)
        (set-window-dedicated-p win t))
      (with-current-buffer buf
        (setq herdr-sanctum--panel-dirty nil)
        (when (window-live-p win)
          (herdr-sanctum-panel-record-size win))
        (when-let* ((fn (or render-fn herdr-sanctum--panel-render-fn)))
          (funcall fn win)))
      (when (window-live-p win)
        (if select
            (select-window win)
          (when (window-live-p origin)
            (select-window origin))))
      win)))

(defun herdr-sanctum-toggle-panel
    (buffer-name show-fn set-enabled-fn &optional on-open-fn on-change-fn)
  "Toggle side-panel BUFFER-NAME on the selected frame.
When BUFFER-NAME is visible on the selected frame, call SET-ENABLED-FN with
nil and delete its window unless it is the frame's sole window.  Otherwise
call SET-ENABLED-FN with t, open the panel with SHOW-FN, call optional
ON-OPEN-FN (zero arguments), and ensure Herdr is connected.  In both cases
call optional ON-CHANGE-FN (zero arguments, for example terminal resize)
and force a mode-line update.  Return the shown window on open, or nil."
  (let ((existing (and (get-buffer buffer-name)
                       (get-buffer-window buffer-name (selected-frame))))
        (win nil))
    (if existing
        (progn
          (when (functionp set-enabled-fn)
            (funcall set-enabled-fn nil))
          (when (eq (window-deletable-p existing) t)
            (delete-window existing)))
      (when (functionp set-enabled-fn)
        (funcall set-enabled-fn t))
      (setq win (funcall show-fn))
      (when (functionp on-open-fn)
        (funcall on-open-fn))
      (herdr-sanctum-ensure-connected))
    (when (functionp on-change-fn)
      (funcall on-change-fn))
    (force-mode-line-update t)
    win))

(defun herdr-sanctum-cleanup-panel (buffer-name)
  "Delete side-panel windows displaying BUFFER-NAME and kill the buffer."
  (when-let* ((buf (get-buffer buffer-name)))
    (dolist (win (get-buffer-window-list buf nil t))
      (when (and (window-live-p win) (eq (window-deletable-p win) t))
        (delete-window win)))
    (kill-buffer buf)))

(defun herdr-sanctum-maybe-collapse-opposite-panel
    (opening-side opening-width other-buffer-name other-side &optional on-collapse-fn)
  "Collapse OTHER-BUFFER-NAME on narrow frames before opening OPENING-SIDE.
OPENING-WIDTH is the target column width of the panel being opened, and
OTHER-SIDE is the configured side of OTHER-BUFFER-NAME.  When opposite-side
panels would leave fewer than `herdr-sanctum-min-terminal-width' columns on
the selected frame, delete the opposite panel's window and invoke optional
ON-COLLAPSE-FN."
  (when (numberp herdr-sanctum-min-terminal-width)
    (when-let* ((other-buf (get-buffer other-buffer-name))
                (other-win (get-buffer-window other-buf (selected-frame)))
                ((window-live-p other-win))
                ((not (eq opening-side
                          (or (window-parameter other-win 'window-side)
                              other-side))))
                ((eq (window-deletable-p other-win) t))
                (avail (- (frame-width (selected-frame))
                          opening-width
                          (window-total-width other-win)))
                ((< avail herdr-sanctum-min-terminal-width)))
      (delete-window other-win)
      (when (functionp on-collapse-fn)
        (funcall on-collapse-fn))
      t)))

;;; Shared Side-Panel Controller Helpers

(defun herdr-sanctum-panel-dirty-p (&optional buf-or-name)
  "Return non-nil when side-panel buffer BUF-OR-NAME has a deferred render.
BUF-OR-NAME may be a buffer, buffer name, or nil (current buffer)."
  (when-let* ((buf (herdr-sanctum--resolve-buffer buf-or-name)))
    (buffer-local-value 'herdr-sanctum--panel-dirty buf)))

(defun herdr-sanctum-panel-mark-dirty (&optional buf-or-name)
  "Mark side-panel buffer BUF-OR-NAME dirty if it exists.
BUF-OR-NAME may be a buffer, buffer name, or nil (current buffer)."
  (when-let* ((buf (herdr-sanctum--resolve-buffer buf-or-name)))
    (with-current-buffer buf
      (setq herdr-sanctum--panel-dirty t))))

(defun herdr-sanctum-panel-clear-dirty (&optional buf-or-name)
  "Clear the dirty flag on side-panel buffer BUF-OR-NAME if it exists.
BUF-OR-NAME may be a buffer, buffer name, or nil (current buffer)."
  (when-let* ((buf (herdr-sanctum--resolve-buffer buf-or-name)))
    (with-current-buffer buf
      (setq herdr-sanctum--panel-dirty nil))))

(defun herdr-sanctum-panel-refresh-or-mark-dirty (buf-name &optional render-fn)
  "Render BUF-NAME via RENDER-FN if shown on any frame, else mark it dirty.
RENDER-FN defaults to the buffer's `herdr-sanctum--panel-render-fn' and is
called with the panel's preferred live window."
  (when-let* ((buf (herdr-sanctum--resolve-buffer buf-name)))
    (with-current-buffer buf
      (if-let* ((win (herdr-sanctum-panel-window buf)))
          (let ((fn (or render-fn herdr-sanctum--panel-render-fn)))
            (setq herdr-sanctum--panel-dirty nil)
            (when (functionp fn)
              (funcall fn win)))
        (setq herdr-sanctum--panel-dirty t)))))

(defun herdr-sanctum-panel-save-windows (&optional item-at-fn)
  "Capture point, scroll start, and item identity for every live panel window.
ITEM-AT-FN, when non-nil, is called with a buffer position to capture a
logical item identity.  Returns a state plist for
`herdr-sanctum-panel-restore-windows'."
  (let* ((pt (point))
         (buf-item (and item-at-fn (funcall item-at-fn pt)))
         (wins (mapcar
                (lambda (win)
                  (let ((wpt (window-point win)))
                    (list win
                          (window-start win)
                          wpt
                          (and item-at-fn (funcall item-at-fn wpt)))))
                (get-buffer-window-list (current-buffer) nil t))))
    (list :point pt :item buf-item :windows wins)))

(defun herdr-sanctum-panel-restore-windows (state &optional find-item-fn fallback-pos)
  "Restore buffer point and every live window's point and scroll from STATE.
STATE is a plist returned by `herdr-sanctum-panel-save-windows'.
FIND-ITEM-FN, when non-nil, maps a saved item identity to a new buffer
position (or nil).  FALLBACK-POS is used when item matching returns nil;
otherwise the prior position is clamped to the buffer bounds.  Uses
`set-window-start' with NOFORCE non-nil to preserve scroll in non-selected
windows while resetting stale scroll positions when a buffer shrinks or the
target moves above the previous window start."
  (let* ((max-pos (max (point-min) (1- (point-max))))
         (resolve
          (lambda (saved-pt saved-item)
            (let ((pos (or (and find-item-fn saved-item
                                (funcall find-item-fn saved-item))
                           fallback-pos
                           saved-pt
                           (point-min))))
              (max (point-min) (min pos max-pos)))))
         (buf-target (funcall resolve
                              (plist-get state :point)
                              (plist-get state :item))))
    (goto-char buf-target)
    (dolist (entry (plist-get state :windows))
      (pcase-let ((`(,win ,start ,wpt ,witem) entry))
        (when (and (window-live-p win)
                   (eq (window-buffer win) (current-buffer)))
          (let* ((target (funcall resolve wpt witem))
                 (clamped-start
                  (if (>= start (point-max))
                      (point-min)
                    (save-excursion
                      (goto-char (max (point-min) (min start max-pos)))
                      (line-beginning-position))))
                 (new-start (if (< target clamped-start)
                                (point-min)
                              clamped-start)))
            (set-window-start win new-start t)
            (set-window-point win target)))))))

;;; Shared Git Sync Formatter

(defun herdr-sanctum-format-git-sync (git &optional include-dirty active-bg verbose)
  "Format Git ahead/behind sync counters from GIT plist, or nil when empty.
When INCLUDE-DIRTY is non-nil and `:dirty' > 0, append a ` •' dirty dot
(or `•' alone when ahead/behind are both 0).
When ACTIVE-BG is a color string, overlay `(:background ACTIVE-BG)' on each
face; when it is another non-nil face spec, append it as a fallback face.
When VERBOSE is non-nil, format as `↑N ahead · ↓N behind'."
  (when git
    (let* ((ahead (or (plist-get git :ahead) 0))
           (behind (or (plist-get git :behind) 0))
           (dirty (or (plist-get git :dirty) 0))
           (face-of (lambda (f)
                      (cond
                       ((stringp active-bg) (list f `(:background ,active-bg)))
                       (active-bg (list f active-bg))
                       (t f)))))
      (if verbose
          (let ((parts
                 (delq nil
                       (list (when (> ahead 0)
                               (propertize (format "↑%d ahead" ahead)
                                           'face (funcall face-of 'herdr-sanctum-sidebar-git-ahead)))
                             (when (> behind 0)
                               (propertize (format "↓%d behind" behind)
                                           'face (funcall face-of 'herdr-sanctum-sidebar-git-behind)))))))
            (when parts
              (string-join parts
                           (propertize " · " 'face (funcall face-of 'herdr-sanctum-sidebar-meta)))))
        (let ((sync (concat
                     (when (> behind 0)
                       (propertize (format "↓%d" behind)
                                   'face (funcall face-of 'herdr-sanctum-sidebar-git-behind)))
                     (when (> ahead 0)
                       (propertize (format "↑%d" ahead)
                                   'face (funcall face-of 'herdr-sanctum-sidebar-git-ahead))))))
          (cond
           ((not (string-empty-p sync))
            (if (and include-dirty (> dirty 0))
                (concat sync
                        (propertize " •" 'face (funcall face-of 'herdr-sanctum-sidebar-meta)))
              sync))
           ((and include-dirty (> dirty 0))
            (propertize "•" 'face (funcall face-of 'herdr-sanctum-sidebar-meta)))
           (t nil)))))))

;;; Pixel-Exact Layout & Chrome Widgets

(defconst herdr-sanctum--spw-2arg-p
  (and (fboundp 'string-pixel-width)
       (let ((max (cdr (func-arity #'string-pixel-width))))
         (or (eq max 'many) (and (integerp max) (>= max 2)))))
  "Non-nil when `string-pixel-width' accepts the Emacs 31+ BUFFER argument.")

(defun herdr-sanctum-string-pixel-width (str &optional buf)
  "Return pixel width of STR in BUF (Emacs 30.1+ compatible)."
  (cond
   ((zerop (length str)) 0)
   (herdr-sanctum--spw-2arg-p
    (with-suppressed-warnings ((callargs string-pixel-width))
      (string-pixel-width str buf)))
   (t
    (let* ((target (or (herdr-sanctum--resolve-buffer buf) (current-buffer)))
           (remap (buffer-local-value 'face-remapping-alist target)))
      (if remap
          (with-temp-buffer
            (setq-local face-remapping-alist remap
                        line-prefix nil
                        wrap-prefix nil
                        truncate-lines t)
            (let (deactivate-mark)
              (insert str))
            (add-text-properties
             (point-min) (point-max)
             '(display-line-numbers-disable t line-prefix "" wrap-prefix ""))
            (car (buffer-text-pixel-size nil nil t)))
        (if (eq target (current-buffer))
            (string-pixel-width str)
          (with-current-buffer target
            (string-pixel-width str))))))))

(defun herdr-sanctum--ellipsis-px (ellipsis bg-face buf)
  "Return cached pixel width of ELLIPSIS styled with BG-FACE in BUF."
  (with-current-buffer buf
    (let ((cache-key (list herdr-sanctum-theme-revision
                           face-remapping-alist
                           (frame-char-width))))
      (unless (equal (car herdr-sanctum--ellipsis-px-cache) cache-key)
        (setq herdr-sanctum--ellipsis-px-cache (cons cache-key nil)))
      (or (alist-get bg-face (cdr herdr-sanctum--ellipsis-px-cache) nil nil #'equal)
          (let ((px (herdr-sanctum-string-pixel-width ellipsis buf)))
            (setf (alist-get bg-face (cdr herdr-sanctum--ellipsis-px-cache) nil nil #'equal)
                  px)
            px)))))

(defun herdr-sanctum-truncate (text max-width &optional bg-face)
  "Truncate TEXT to MAX-WIDTH display columns using an ellipsis."
  (if (or (<= max-width 0) (string-empty-p text))
      ""
    (let ((ellipsis (if bg-face (propertize "…" 'face bg-face) "…")))
      (truncate-string-to-width text max-width nil nil ellipsis))))

(defun herdr-sanctum-truncate-path (path width &optional win face)
  "Truncate file PATH to fit WIDTH/WIN while preserving trailing segments.
Optional FACE styles the returned string and governs pixel measurement."
  (let* ((styled (if face (propertize path 'face face) path))
         (gui-p (and (window-live-p win)
                     (display-graphic-p (window-frame win))
                     (fboundp 'string-pixel-width)))
         (limit (if gui-p (herdr-sanctum-usable-body-px win 80) (max 1 width)))
         (buf (current-buffer))
         (fits-p (lambda (s)
                   (if gui-p
                       (<= (herdr-sanctum-string-pixel-width s buf) limit)
                     (<= (string-width s) limit)))))
    (if (funcall fits-p styled)
        styled
      (let* ((trimmed (directory-file-name path))
             (parts (split-string trimmed "/" t))
             (best nil))
        (when (> (length parts) 1)
          (let ((acc nil))
            (dolist (seg (nreverse parts))
              (push seg acc)
              (let* ((cand (concat "…/" (string-join acc "/")))
                     (cand-s (if face (propertize cand 'face face) cand)))
                (when (funcall fits-p cand-s)
                  (setq best cand-s))))))
        (or best
            (let* ((tail (file-name-nondirectory trimmed))
                   (short (if (and (not (string-empty-p tail))
                                   (not (equal tail trimmed)))
                              (concat "…/" tail)
                            path))
                   (short-s (if face (propertize short 'face face) short)))
              (if gui-p
                  (herdr-sanctum-truncate-px short-s limit face)
                (herdr-sanctum-truncate short-s limit face))))))))

(defun herdr-sanctum-truncate-px (text max-px &optional bg-face)
  "Truncate propertized TEXT to fit within MAX-PX pixels on graphical frames."
  (cond
   ((or (<= max-px 0) (string-empty-p text)) "")
   (t
    (let* ((buf (current-buffer))
           (ellipsis (if bg-face (propertize "…" 'face bg-face) "…"))
           (ell-px (herdr-sanctum--ellipsis-px ellipsis bg-face buf)))
      ;; Emacs's C `truncate-string-pixelwise' wraps internally against the
      ;; selected window's width; skip it when the selected window is narrower
      ;; than `max-px + ell-px' so wider side panels never lose truncation.
      (if (and (fboundp 'truncate-string-pixelwise)
               (let ((sel (selected-window)))
                 (and (window-live-p sel)
                      (display-graphic-p (window-frame sel))
                      (< (+ max-px ell-px) (window-body-width sel t)))))
          (truncate-string-pixelwise text max-px buf ellipsis ell-px)
        (cond
         ((<= (herdr-sanctum-string-pixel-width text buf) max-px)
          text)
         ((< max-px ell-px)
          "")
         (t
          (let ((limit (- max-px ell-px))
                (lo 0)
                (hi (length text)))
            (while (< lo hi)
              (let ((mid (/ (+ lo hi 1) 2)))
                (if (<= (herdr-sanctum-string-pixel-width (substring text 0 mid) buf)
                        limit)
                    (setq lo mid)
                  (setq hi (1- mid)))))
            (concat (substring text 0 lo) ellipsis)))))))))

(defun herdr-sanctum-usable-body-px (win &optional min-px)
  "Return usable horizontal pixel width inside WIN's text body.
When WIN has zero right fringe, reserve one character cell for the
`truncate-lines' truncation slot; with a GUI right fringe, use the full
body width minus a 2px safety margin."
  (let* ((body-px (window-body-width win t))
         (right-fringe (or (nth 1 (window-fringes win)) 0))
         (reserve (if (> right-fringe 0)
                      2
                    (+ (frame-char-width (window-frame win)) 2))))
    (max (or min-px 40) (- body-px reserve))))

(defun herdr-sanctum-apply-fallback-face (str face)
  "Return a copy of STR with FACE appended as lowest-priority fallback face."
  (if (or (null str) (string-empty-p str))
      ""
    (let ((copy (copy-sequence str)))
      (add-face-text-property 0 (length copy) face t copy)
      copy)))

(defun herdr-sanctum-escape-mode-line (str)
  "Escape literal `%' characters in STR as `%%' for mode/header/tab lines.
Preserves all text properties on each escaped `%' character."
  (if (and (stringp str) (string-search "%" str))
      (replace-regexp-in-string
       "%"
       (lambda (m) (apply #'propertize "%%" (text-properties-at 0 m)))
       str t t)
    (or str "")))

(defun herdr-sanctum--pad-row (left right width &optional bg-face no-truncate)
  "Align LEFT and RIGHT within WIDTH columns, optionally filling with BG-FACE."
  (let* ((rw (string-width right))
         (max-left (max 1 (- width (if (> rw 0) (1+ rw) 0))))
         (trimmed (if (and (not no-truncate) (> (string-width left) max-left))
                      (herdr-sanctum-truncate left max-left bg-face)
                    left))
         (gap (max (if (> rw 0) 1 0) (- width (string-width trimmed) rw)))
         (pad (if bg-face
                  (propertize (make-string gap ?\s) 'face bg-face)
                (make-string gap ?\s))))
    (concat trimmed pad right)))

(defun herdr-sanctum--align-row-px
    (left right usable-px gap-px bg-face &optional no-truncate left-pad)
  "Align LEFT and RIGHT pixel-exactly within USABLE-PX in the current buffer.
GAP-PX is the minimum pixel gap between LEFT and non-empty RIGHT.  BG-FACE
styles the gap spacer and truncation ellipsis (defaulting to `default' on
the spacer when nil).  When NO-TRUNCATE is non-nil, keep LEFT intact.
Optional LEFT-PAD is prepended before LEFT when LEFT is non-empty."
  (let* ((buf (current-buffer))
         (has-right (not (string-empty-p right)))
         (right-px (if has-right
                       (herdr-sanctum-string-pixel-width right buf)
                     0))
         (effective-gap (if (> right-px 0) gap-px 0))
         (align-px (max 0 (- usable-px right-px)))
         (max-left-px (if (> right-px 0)
                          (- align-px effective-gap)
                        usable-px))
         (trimmed (cond
                   ((string-empty-p left) "")
                   (no-truncate left)
                   (t (herdr-sanctum-truncate-px left max-left-px bg-face))))
         (prefix (if (and left-pad (not (string-empty-p trimmed)))
                     left-pad
                   "")))
    (if (not has-right)
        (concat prefix trimmed)
      (concat prefix
              trimmed
              (propertize " "
                          'display `(space :align-to (,align-px))
                          'face (or bg-face 'default))
              right))))

(defun herdr-sanctum-format-row (left right width win &optional bg-face no-truncate)
  "Align LEFT and RIGHT for WIN (pixel-exact on GUI, column-aligned on TTY).
When BG-FACE is non-nil, apply it to the gap spacer and truncation ellipsis.
When NO-TRUNCATE is non-nil and RIGHT is empty with nil BG-FACE, return LEFT
directly without width measurement."
  (cond
   ((and no-truncate (string-empty-p right) (null bg-face))
    left)
   ((and (window-live-p win)
         (display-graphic-p (window-frame win))
         (fboundp 'string-pixel-width))
    (herdr-sanctum--align-row-px
     left right (herdr-sanctum-usable-body-px win 80) 10 bg-face no-truncate))
   (t
    (herdr-sanctum--pad-row left right width bg-face no-truncate))))

(defun herdr-sanctum-format-header-line (left right &optional win)
  "Format a full-width Sanctum header-line aligning LEFT and RIGHT in WIN.
Appends `herdr-sanctum-chrome-bar' as fallback face to all characters in
LEFT and RIGHT so pixel measurements match rendered header-line glyphs
across `*sanctum*', `*sanctum-spaces*', and `*sanctum-drawer*'.  When WIN
is nil, looks up a window showing the current buffer via
`herdr-sanctum-panel-window' and falls back to column alignment when the
buffer is not displayed (never borrowing another buffer's window).  On GUI
windows with a left fringe or margin, indents LEFT to align with the text
body and pins RIGHT cleanly to the right edge without clipping.  Escapes
literal `%' characters via `herdr-sanctum-escape-mode-line'."
  (let* ((win (or (and (window-live-p win) win)
                  (herdr-sanctum-panel-window)))
         (left-str (herdr-sanctum-apply-fallback-face left 'herdr-sanctum-chrome-bar))
         (right-str (herdr-sanctum-apply-fallback-face right 'herdr-sanctum-chrome-bar))
         (result
          (if (and (window-live-p win)
                   (display-graphic-p (window-frame win))
                   (fboundp 'string-pixel-width))
              (let* ((frame (window-frame win))
                     (buf (window-buffer win))
                     (render
                      (lambda ()
                        (with-current-buffer buf
                          (let* ((body-px (window-body-width win t))
                                 (usable-px (max 40 (- body-px 2)))
                                 (fringe-left (or (nth 0 (window-fringes win)) 0))
                                 (margin-left (or (car (window-margins win)) 0))
                                 (left-pad
                                  (when (or (> fringe-left 0) (> margin-left 0))
                                    (propertize " "
                                                'display '(space :align-to (0))
                                                'face 'herdr-sanctum-chrome-bar))))
                            (herdr-sanctum--align-row-px
                             left-str right-str usable-px 8
                             'herdr-sanctum-chrome-bar nil left-pad))))))
                (if (eq frame (selected-frame))
                    (funcall render)
                  (with-selected-frame frame
                    (funcall render))))
            (let ((width (if (window-live-p win)
                             (max 16 (window-body-width win))
                           80)))
              (herdr-sanctum--pad-row
               left-str right-str width 'herdr-sanctum-chrome-bar)))))
    (herdr-sanctum-escape-mode-line result)))

(defun herdr-sanctum-format-status-bar (left right &optional win)
  "Return a `mode-line-format' list aligning LEFT and RIGHT with WIN's fringes.
LEFT and RIGHT should already be propertized with `herdr-sanctum-status-bar'
(or a compatible button face) without leading or trailing padding spaces."
  (let* ((w (and (window-live-p win) win))
         (gui-p (and w (display-graphic-p (window-frame w))))
         (fringe-l (if gui-p (or (nth 0 (window-fringes w)) 0) 0))
         (fringe-r (if gui-p (or (nth 1 (window-fringes w)) 0) 0))
         (lead (if (> fringe-l 0)
                   (propertize " "
                               'display '(space :align-to (0))
                               'face 'herdr-sanctum-status-bar)
                 (propertize " " 'face 'herdr-sanctum-status-bar)))
         (tail (if (> fringe-r 0)
                   (propertize " "
                               'display `(space :width (,fringe-r))
                               'face 'herdr-sanctum-status-bar)
                 (propertize " " 'face 'herdr-sanctum-status-bar))))
    (list (concat lead left)
          'mode-line-format-right-align
          (concat right tail))))

(defun herdr-sanctum-make-chrome-button
    (label area fn &optional face help-echo cache-key)
  "Create a clickable chrome button LABEL in AREA running FN on mouse-1.
AREA is `header-line' (default when nil), `mode-line', or `tab-line'.
FN is an interactive command (bound directly without a wrapper) or a
zero-argument function.  Optional FACE and HELP-ECHO style the button.
When CACHE-KEY is non-nil or FN is a symbol, reuse a cached keymap from
`herdr-sanctum--chrome-keymap-cache' keyed by `(AREA . KEY)'."
  (let* ((target-area (or area 'header-line))
         (key (or cache-key (and (symbolp fn) fn)))
         (entry-key (when key (cons target-area key)))
         (map (or (and entry-key
                       (gethash entry-key herdr-sanctum--chrome-keymap-cache))
                  (let ((m (make-sparse-keymap))
                        (binding (if (commandp fn)
                                     fn
                                   (lambda ()
                                     (interactive)
                                     (funcall fn)))))
                    (define-key m (vector target-area 'mouse-1) binding)
                    (when entry-key
                      (puthash entry-key m herdr-sanctum--chrome-keymap-cache))
                    m)))
         (btn (propertize label
                          'mouse-face 'mode-line-highlight
                          'local-map map)))
    (when face
      (put-text-property 0 (length btn) 'face face btn))
    (when help-echo
      (put-text-property 0 (length btn) 'help-echo help-echo btn))
    btn))

(provide 'herdr-sanctum-ui)
;;; herdr-sanctum-ui.el ends here
