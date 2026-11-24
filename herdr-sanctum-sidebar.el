;;; herdr-sanctum-sidebar.el --- Persistent Spaces & Agents sidebar -*- lexical-binding: t; -*-

;; Copyright (C) 2026 oatiz

;; Author: oatiz <iamoatiz@gmail.com>
;; URL: https://github.com/oatiz/herdr-sanctum
;; Keywords: tools, terminals

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

;; Persistent side-window (`*sanctum-spaces*') displaying Herdr workspaces
;; (with inline SVG monograms, async Git branch/status, and on-demand tab
;; expansion) and an Agents section grouped by Blocked / Working / Done /
;; Idle with elapsed durations.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'herdr-sanctum-rpc)
(require 'herdr-sanctum-git)
(require 'herdr-sanctum-theme)
(require 'herdr-sanctum-ui)
(require 'herdr-sanctum-terminal)

(declare-function herdr-sanctum "herdr-sanctum" (&optional arg))
(declare-function herdr-sanctum-drawer-collapse "herdr-sanctum-drawer" ())
(declare-function herdr-sanctum-jump "herdr-sanctum" ())
(declare-function hl-line-highlight "hl-line" ())
(declare-function hl-line-unhighlight "hl-line" ())
(defvar herdr-sanctum-drawer-side)

(defgroup herdr-sanctum-sidebar nil
  "Persistent Spaces and Agents sidebar settings for `herdr-sanctum'."
  :group 'herdr-sanctum
  :prefix "herdr-sanctum-")

(defun herdr-sanctum-sidebar--set-option (sym val)
  "Set sidebar option SYM to VAL and refresh `*sanctum-spaces*' when live."
  (set-default sym val)
  (when (and (featurep 'herdr-sanctum-sidebar)
             (fboundp 'herdr-sanctum-sidebar-refresh)
             (get-buffer herdr-sanctum-sidebar-buffer-name))
    (herdr-sanctum-sidebar-refresh)))

(defcustom herdr-sanctum-sidebar-side 'left
  "Side of the frame where `*sanctum-spaces*' is displayed."
  :type '(choice (const :tag "Left" left)
                 (const :tag "Right" right))
  :group 'herdr-sanctum-sidebar)

(defcustom herdr-sanctum-sidebar-width 30
  "Column width of the `*sanctum-spaces*' side-window."
  :type 'integer
  :group 'herdr-sanctum-sidebar)

(defcustom herdr-sanctum-sidebar-default-open t
  "When non-nil, `herdr-sanctum' opens `*sanctum-spaces*' alongside `*sanctum*'."
  :type 'boolean
  :group 'herdr-sanctum-sidebar)

(defcustom herdr-sanctum-sidebar-default-expand 'focused
  "Default tab tree expansion mode for spaces in `*sanctum-spaces*'.
- `focused': automatically expand only the currently focused space.
- t: expand all spaces by default.
- nil: collapse all spaces by default."
  :type '(choice (const :tag "Focused space only" focused)
                 (const :tag "All expanded" t)
                 (const :tag "All collapsed" nil))
  :group 'herdr-sanctum-sidebar
  :set #'herdr-sanctum-sidebar--set-option)

(defcustom herdr-sanctum-sidebar-sections '(spaces agents)
  "Ordered list of sections rendered in `*sanctum-spaces*'.
Supported symbols are `spaces' and `agents'."
  :type '(repeat (choice (const :tag "Spaces" spaces)
                         (const :tag "Agents" agents)))
  :group 'herdr-sanctum-sidebar
  :set #'herdr-sanctum-sidebar--set-option)

(defcustom herdr-sanctum-sidebar-show-idle-agents t
  "When non-nil, include idle agents in the `*sanctum-spaces*' Agents section."
  :type 'boolean
  :group 'herdr-sanctum-sidebar
  :set #'herdr-sanctum-sidebar--set-option)

(defcustom herdr-sanctum-sidebar-agent-statuses '("blocked" "working" "done" "idle")
  "Ordered list of agent status groups rendered in `*sanctum-spaces*'.
Supported values are \"blocked\", \"working\", \"done\", and \"idle\".
Setting `herdr-sanctum-sidebar-show-idle-agents' to nil hides \"idle\"
even when it appears in this list."
  :type '(repeat (choice (const :tag "Blocked" "blocked")
                         (const :tag "Working" "working")
                         (const :tag "Done" "done")
                         (const :tag "Idle" "idle")
                         (string :tag "Status")))
  :group 'herdr-sanctum-sidebar
  :set #'herdr-sanctum-sidebar--set-option)

(defcustom herdr-sanctum-sidebar-space-meta-function nil
  "Optional function `(WS GIT-INFO MAX-PX)' formatting right-side space meta.
When non-nil, called for each space row with the workspace alist WS,
its Git info plist GIT-INFO (or nil), and an integer pixel budget MAX-PX.
Should return a propertized string, or nil to omit right-side metadata.
When nil, use the default compact/wide Git metadata formatter."
  :type '(choice (const :tag "Default compact Git meta" nil)
                 function)
  :group 'herdr-sanctum-sidebar
  :set #'herdr-sanctum-sidebar--set-option)

(defcustom herdr-sanctum-sidebar-footer-action nil
  "Right-aligned action button in the `*sanctum-spaces*' status bar.
- nil: show the default `Config' button (omitted when
  `herdr-sanctum-config-file' is nil).
- `none': hide the right-side footer button.
- `(LABEL . ACTION)': render a clickable button with LABEL invoking
  interactive command or zero-argument function ACTION."
  :type '(choice (const :tag "Default Config button" nil)
                 (const :tag "Hidden" none)
                 (cons (string :tag "Label") (function :tag "Action")))
  :group 'herdr-sanctum-sidebar
  :set #'herdr-sanctum-sidebar--set-option)

(defcustom herdr-sanctum-config-file "~/.config/herdr/config.toml"
  "Path to Herdr's configuration file opened from the sidebar status bar.
Set to nil to hide the default `Config' button."
  :type '(choice (const :tag "None (hide Config button)" nil)
                 (file :tag "Config file"))
  :group 'herdr-sanctum-sidebar
  :set #'herdr-sanctum-sidebar--set-option)

(defcustom herdr-sanctum-sidebar-change-hook nil
  "Hook run when `*sanctum-spaces*' visibility changes."
  :type 'hook
  :group 'herdr-sanctum-sidebar)

(defvar herdr-sanctum-sidebar--enabled nil
  "Current session toggle state for the sidebar (`unset' inherits default).")

(defvar herdr-sanctum-sidebar--expanded-spaces (make-hash-table :test #'equal)
  "Hash table of explicitly toggled Herdr `workspace_id' keys.")

(defvar herdr-sanctum-sidebar--git-refresh-timer nil
  "Zero-delay timer coalescing batched Git updates into one sidebar render.")

(defvar-local herdr-sanctum-sidebar--focus-overlay nil
  "Overlay highlighting an active row while `*sanctum-spaces*' has focus.")

(defvar herdr-sanctum-sidebar--chevron-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] #'herdr-sanctum-sidebar-mouse-toggle-fold)
    map)
  "Overlay keymap for clickable workspace fold chevrons in `*sanctum-spaces*'.")

(defun herdr-sanctum-sidebar-enabled-p ()
  "Return non-nil when the sidebar should be shown when opening Sanctum."
  (if (eq herdr-sanctum-sidebar--enabled nil)
      herdr-sanctum-sidebar-default-open
    (eq herdr-sanctum-sidebar--enabled 'on)))

(defun herdr-sanctum-sidebar-visible-p (&optional frame)
  "Return non-nil when `*sanctum-spaces*' is visible on FRAME."
  (when-let* ((buf (get-buffer herdr-sanctum-sidebar-buffer-name))
              (win (get-buffer-window buf (or frame (selected-frame)))))
    (window-live-p win)))

(defun herdr-sanctum-sidebar--space-expanded-p (ws-id &optional focused-ws)
  "Return non-nil when workspace WS-ID's tabs are expanded.
FOCUSED-WS, when provided, avoids re-reading the snapshot in `focused' mode."
  (let ((state (gethash ws-id herdr-sanctum-sidebar--expanded-spaces 'unset)))
    (cond
     ((not (eq state 'unset)) state)
     ((eq herdr-sanctum-sidebar-default-expand 'focused)
      (and ws-id
           (equal ws-id
                  (or focused-ws
                      (alist-get 'focused_workspace_id (herdr-sanctum-snapshot))))))
     (t herdr-sanctum-sidebar-default-expand))))

(defun herdr-sanctum-sidebar--set-space-expanded (ws-id expanded)
  "Set explicit expansion state EXPANDED for workspace WS-ID.
In `focused' mode, toggling back to the workspace's natural accordion state
clears the manual override so future focus switches auto-expand/collapse."
  (let ((val (and expanded t)))
    (if (and (eq herdr-sanctum-sidebar-default-expand 'focused)
             (eq val (and ws-id
                          (equal ws-id
                                 (alist-get 'focused_workspace_id
                                            (herdr-sanctum-snapshot)))
                          t)))
        (remhash ws-id herdr-sanctum-sidebar--expanded-spaces)
      (puthash ws-id val herdr-sanctum-sidebar--expanded-spaces))))

(defun herdr-sanctum-sidebar--active-sections ()
  "Return deduplicated valid sections from `herdr-sanctum-sidebar-sections'."
  (seq-uniq (seq-filter (lambda (s) (memq s '(spaces agents)))
                        herdr-sanctum-sidebar-sections)))

(defun herdr-sanctum-sidebar--space-git-meta (git-info tab-count &optional width)
  "Format right-aligned Git metadata (or TAB-COUNT fallback) from GIT-INFO.
Optional WIDTH is the sidebar column width (defaulting to
`herdr-sanctum-sidebar-width').  Widths above 34 columns expand the branch
budget and show abbreviated branch alongside sync counters when both fit."
  (let ((tabs (or tab-count 1))
        (col-width (or width herdr-sanctum-sidebar-width 30)))
    (cond
     (git-info
      (let* ((wide-p (> col-width 34))
             (budget (if wide-p
                         (min 24 (+ 11 (max 0 (- col-width 34))))
                       11))
             (branch (or (plist-get git-info :branch) ""))
             (dirty (or (plist-get git-info :dirty) 0))
             (has-sync (or (> (or (plist-get git-info :behind) 0) 0)
                           (> (or (plist-get git-info :ahead) 0) 0))))
        (if has-sync
            (let ((sync (or (herdr-sanctum-format-git-sync git-info t) "")))
              (if (and wide-p (not (string-empty-p branch)))
                  (let ((branch-budget (- budget (string-width sync) 1)))
                    (if (>= branch-budget 4)
                        (concat (propertize (herdr-sanctum-truncate branch branch-budget)
                                            'face 'herdr-sanctum-sidebar-meta)
                                (propertize " " 'face 'herdr-sanctum-sidebar-meta)
                                sync)
                      sync))
                sync))
          (let* ((max-branch-width (if (> dirty 0)
                                       (max 4 (if wide-p (- budget 2) 10))
                                     budget))
                 (short-branch (herdr-sanctum-truncate branch max-branch-width)))
            (if (not (string-empty-p short-branch))
                (propertize (if (> dirty 0) (concat short-branch " •") short-branch)
                            'face 'herdr-sanctum-sidebar-meta)
              (or (herdr-sanctum-format-git-sync git-info t) ""))))))
     ((> tabs 1)
      (propertize (number-to-string tabs) 'face 'herdr-sanctum-sidebar-meta))
     (t ""))))

(defun herdr-sanctum-sidebar--item-at (&optional pos)
  "Return the Sanctum sidebar item plist at POS (default point), or nil.
When POS is on a line's trailing newline (for example after clicking empty
space to the right of a short row), fall back to the item at the beginning
of that line."
  (let ((p (or pos (point))))
    (or (get-text-property p 'herdr-sanctum-item)
        (when (eq (char-after p) ?\n)
          (let ((bol (if pos
                         (save-excursion
                           (goto-char p)
                           (line-beginning-position))
                       (line-beginning-position))))
            (when (< bol p)
              (get-text-property bol 'herdr-sanctum-item)))))))

(defun herdr-sanctum-sidebar--find-item-pos (kind id)
  "Find buffer position of KIND (`agent', `workspace', or `tab') with ID."
  (let ((key (pcase kind
               ('agent :pane-id)
               ('tab :tab-id)
               (_ :workspace-id)))
        (pos (point-min))
        found)
    (while (and (not found) (< pos (point-max)))
      (let ((item (get-text-property pos 'herdr-sanctum-item)))
        (if (and item
                 (eq (plist-get item :kind) kind)
                 (equal (plist-get item key) id))
            (setq found pos)
          (setq pos (next-single-property-change
                     pos 'herdr-sanctum-item nil (point-max))))))
    found))

(defun herdr-sanctum-sidebar--insert-divider (width win)
  "Insert a subtle full-width section divider line for WIDTH and WIN."
  (if (and (window-live-p win) (display-graphic-p (window-frame win)))
      (let ((px (herdr-sanctum-usable-body-px win 40))
            (color (herdr-sanctum-palette-color 'border)))
        (insert (propertize " "
                            'display `(space :align-to (,px))
                            'face `(:underline (:color ,color :position t)))
                (propertize "\n" 'face '(:height 0.45))))
    (insert (propertize (make-string (max 8 width) ?─)
                        'face 'herdr-sanctum-sidebar-guide)
            "\n")))

(defun herdr-sanctum-sidebar--prepare-render-data (snapshot)
  "Build pure lookup tables and metadata plist for rendering SNAPSHOT."
  (let ((workspaces (alist-get 'workspaces snapshot))
        (tabs (alist-get 'tabs snapshot))
        (tabs-by-ws (make-hash-table :test #'equal))
        (ws-labels (make-hash-table :test #'equal))
        (tab-labels (make-hash-table :test #'equal)))
    (dolist (tab tabs)
      (when-let* ((ws-id (alist-get 'workspace_id tab)))
        (puthash ws-id (cons tab (gethash ws-id tabs-by-ws)) tabs-by-ws))
      (when-let* ((tab-id (alist-get 'tab_id tab)))
        (puthash tab-id (or (alist-get 'label tab) tab-id) tab-labels)))
    (maphash (lambda (k v) (puthash k (nreverse v) tabs-by-ws)) tabs-by-ws)
    (dolist (ws workspaces)
      (when-let* ((ws-id (alist-get 'workspace_id ws)))
        (puthash ws-id (or (alist-get 'label ws) ws-id) ws-labels)))
    (list :snapshot snapshot
          :workspaces workspaces
          :agent-groups (and snapshot (herdr-sanctum-grouped-agents snapshot))
          :focused-ws (alist-get 'focused_workspace_id snapshot)
          :focused-tab (alist-get 'focused_tab_id snapshot)
          :focused-pane (alist-get 'focused_pane_id snapshot)
          :tabs-by-ws tabs-by-ws
          :ws-labels ws-labels
          :tab-labels tab-labels)))

(defun herdr-sanctum-sidebar--repos-tooltip (ws-git)
  "Format multi-repo summary suffix for workspace tooltip from WS-GIT, or nil."
  (when-let* ((repos (and ws-git
                          (plist-get ws-git :multi-repo)
                          (plist-get ws-git :repos))))
    (let ((n (length repos)))
      (format " · %d repo%s: %s"
              n (if (= n 1) "" "s")
              (mapconcat
               (lambda (repo)
                 (let* ((name (or (plist-get repo :name) ""))
                        (branch (plist-get repo :branch))
                        (has-branch (and (stringp branch) (not (string-empty-p branch))))
                        (dirty (or (plist-get repo :dirty) 0)))
                   (cond
                    ((and has-branch (> dirty 0)) (format "%s [%s *%d]" name branch dirty))
                    (has-branch                   (format "%s [%s]" name branch))
                    ((> dirty 0)                  (format "%s [*%d]" name dirty))
                    (t name))))
               repos ", ")))))

(defun herdr-sanctum-sidebar--insert-workspace-tabs
    (ws ws-tabs ws-active focused-tab width win sel-bg)
  "Insert expanded tab rows for workspace WS with WS-TABS."
  (let ((ws-id (alist-get 'workspace_id ws))
        (ws-label (or (alist-get 'label ws) (alist-get 'workspace_id ws)))
        (ws-active-tab (alist-get 'active_tab_id ws))
        (n-tabs (length ws-tabs))
        (tab-idx 0))
    (dolist (tab ws-tabs)
      (let* ((tab-id (alist-get 'tab_id tab))
             (tab-label (or (alist-get 'label tab) tab-id))
             (is-focused (equal tab-id focused-tab))
             (is-ws-active (and (not is-focused) (equal tab-id ws-active-tab)))
             (pane-count (or (alist-get 'pane_count tab) 1))
             (t-bg-face (when is-focused 'herdr-sanctum-sidebar-tab-active))
             (t-active-bg (when is-focused sel-bg))
             (guide (herdr-sanctum-row-accent is-focused t-active-bg t))
             (glyph (or (herdr-sanctum-status-glyph (alist-get 'agent_status tab)) " "))
             (lbl-face (cond
                        (is-focused 'herdr-sanctum-sidebar-tab-active)
                        (ws-active 'herdr-sanctum-sidebar-tab)
                        (is-ws-active 'herdr-sanctum-sidebar-tab-ws-active)
                        (t 'herdr-sanctum-sidebar-tab-dim)))
             (lbl (propertize tab-label 'face lbl-face))
             (t-right (if (> pane-count 1)
                          (propertize (format "%dp" pane-count)
                                      'face 'herdr-sanctum-sidebar-meta)
                        ""))
             (t-left (concat "  " guide " " glyph " " lbl))
             (t-row (herdr-sanctum-format-row t-left t-right width win t-bg-face))
             (t-start (point)))
        (insert t-row (if is-focused (propertize "\n" 'face t-bg-face) "\n"))
        (when (and is-focused sel-bg)
          (add-face-text-property t-start (1- (point)) (list :background sel-bg) t))
        (add-text-properties
         t-start (1- (point))
         (append
          (when is-focused '(herdr-sanctum-active-row t))
          (list 'herdr-sanctum-item
               (list :kind 'tab
                     :workspace-id ws-id
                     :tab-id tab-id
                     :label tab-label
                     :index tab-idx
                     :count n-tabs)
               'mouse-face 'highlight
               'help-echo (format "%s / %s (%d pane%s)"
                                  ws-label tab-label pane-count
                                  (if (= pane-count 1) "" "s"))))))
      (cl-incf tab-idx))))

(defun herdr-sanctum-sidebar--insert-workspace-row (ws ws-idx ws-count data width win sel-bg)
  "Insert workspace WS (at WS-IDX of WS-COUNT) and its expanded tabs from DATA."
  (let* ((snapshot (plist-get data :snapshot))
         (focused-ws (plist-get data :focused-ws))
         (focused-tab (plist-get data :focused-tab))
         (ws-id (alist-get 'workspace_id ws))
         (ws-label (or (alist-get 'label ws) ws-id))
         (ws-active (equal ws-id focused-ws))
         (ws-tabs (gethash ws-id (plist-get data :tabs-by-ws)))
         (tab-count (or (alist-get 'tab_count ws) (length ws-tabs)))
         (expanded (herdr-sanctum-sidebar--space-expanded-p ws-id focused-ws))
         (has-focused-tab
          (and ws-active expanded focused-tab
               (seq-some (lambda (tb) (equal (alist-get 'tab_id tb) focused-tab))
                         ws-tabs)))
         (ws-strong (and ws-active (not has-focused-tab)))
         (worktree (alist-get 'worktree ws))
         (linked-wt (and worktree (alist-get 'is_linked_worktree worktree)))
         (bg-face (when ws-strong 'herdr-sanctum-sidebar-space-active))
         (active-bg (when ws-strong sel-bg))
         (badge (herdr-sanctum-space-badge ws-label ws-id))
         (chevron-face (if active-bg
                           (list 'herdr-sanctum-sidebar-meta
                                 `(:background ,active-bg))
                         'herdr-sanctum-sidebar-meta))
         (chevron (if (> tab-count 0)
                      (propertize (if expanded "▾" "▸")
                                  'face chevron-face
                                  'herdr-sanctum-chevron ws-id
                                  'keymap herdr-sanctum-sidebar--chevron-keymap
                                  'pointer 'hand)
                    (propertize " " 'face (when active-bg `(:background ,active-bg)))))
         (chev-gap (if (and (window-live-p win) (display-graphic-p (window-frame win)))
                       (propertize " "
                                   'display '(space :width (3))
                                   'face (when active-bg `(:background ,active-bg)))
                     " "))
         (wt-prefix (if linked-wt
                        (propertize "↳ " 'face chevron-face)
                      ""))
         (label-str (propertize ws-label
                                'face (cond
                                       (ws-strong 'herdr-sanctum-sidebar-space-active)
                                       (ws-active 'herdr-sanctum-sidebar-space-expanded)
                                       (t 'herdr-sanctum-sidebar-space))))
         (ws-dir (herdr-sanctum-workspace-dir ws snapshot))
         (ws-git (and ws-dir (herdr-sanctum-git-info-for-dir ws-dir)))
         (meta-width (if (and (window-live-p win)
                              (window-parameter win 'window-side))
                         (max (window-total-width win)
                              (or herdr-sanctum-sidebar-width 30))
                       (or herdr-sanctum-sidebar-width 30)))
         (meta-max-px (if (and (window-live-p win)
                               (display-graphic-p (window-frame win)))
                          (max 40 (/ (herdr-sanctum-usable-body-px win 80) 2))
                        (max 8 (/ (* width
                                     (frame-char-width
                                      (and (window-live-p win) (window-frame win))))
                                  2))))
         (right (if (functionp herdr-sanctum-sidebar-space-meta-function)
                    (let ((custom (funcall herdr-sanctum-sidebar-space-meta-function
                                           ws ws-git meta-max-px)))
                      (if (stringp custom) custom ""))
                  (herdr-sanctum-sidebar--space-git-meta ws-git tab-count meta-width)))
         (left (concat (herdr-sanctum-row-accent ws-strong active-bg)
                       chevron chev-gap wt-prefix badge
                       (if (string-empty-p badge) "" " ") label-str))
         (row (herdr-sanctum-format-row left right width win bg-face))
         (line-start (point)))
    (insert row (if ws-strong (propertize "\n" 'face bg-face) "\n"))
    (when (and ws-strong sel-bg)
      (add-face-text-property line-start (1- (point)) (list :background sel-bg) t))
    (add-text-properties
     line-start (1- (point))
     (append
      (when ws-strong '(herdr-sanctum-active-row t))
      (list 'herdr-sanctum-item
           (list :kind 'workspace
                 :workspace-id ws-id
                 :label ws-label
                 :index ws-idx
                 :count ws-count)
           'mouse-face 'highlight
           'help-echo (format "Space %s (%d tab%s%s; TAB to %s tabs)"
                              ws-label tab-count (if (= tab-count 1) "" "s")
                              (or (herdr-sanctum-sidebar--repos-tooltip ws-git) "")
                              (if expanded "collapse" "expand")))))
    (when expanded
      (herdr-sanctum-sidebar--insert-workspace-tabs
       ws ws-tabs ws-active focused-tab width win sel-bg))))

(defun herdr-sanctum-sidebar--render-spaces (data width win sel-bg &optional show-header)
  "Render the Spaces section from DATA into the current buffer.
When SHOW-HEADER is non-nil, prepend an inline Spaces section header."
  (let ((snapshot (plist-get data :snapshot))
        (workspaces (plist-get data :workspaces)))
    (when show-header
      (insert (herdr-sanctum-format-row
               (propertize "Spaces" 'face 'herdr-sanctum-sidebar-header)
               (if workspaces
                   (propertize (number-to-string (length workspaces))
                               'face 'herdr-sanctum-sidebar-meta)
                 "")
               width win)
              "\n"))
    (cond
     ((null snapshot)
      (insert (propertize "Connecting to herdr…" 'face 'herdr-sanctum-sidebar-meta) "\n"))
     ((null workspaces)
      (insert (propertize "No workspaces" 'face 'herdr-sanctum-sidebar-meta) "\n"))
     (t
      (let ((ws-count (length workspaces))
            (ws-idx 0))
        (dolist (ws workspaces)
          (herdr-sanctum-sidebar--insert-workspace-row
           ws ws-idx ws-count data width win sel-bg)
          (cl-incf ws-idx)))))))

(defun herdr-sanctum-sidebar--insert-agent-row
    (agent status data width win sel-bg &optional primary-focus)
  "Insert a single AGENT row with STATUS using render DATA.
When PRIMARY-FOCUS is non-nil (Agents is the sole primary focus section),
style the focused agent row with a full selection background."
  (let* ((pane-id (alist-get 'pane_id agent))
         (ws-id (alist-get 'workspace_id agent))
         (tab-id (alist-get 'tab_id agent))
         (ws-label (or (gethash ws-id (plist-get data :ws-labels)) ws-id ""))
         (tab-label (or (gethash tab-id (plist-get data :tab-labels)) tab-id ""))
         (raw-topic (alist-get 'topic (alist-get 'tokens agent)))
         (topic (and (stringp raw-topic)
                     (let ((tr (string-trim raw-topic)))
                       (unless (string-empty-p tr) tr))))
         (detail (if (and topic (not (equal topic ws-label)))
                     topic
                   (unless (or (string-empty-p tab-label)
                               (equal tab-label ws-label))
                     tab-label)))
         (is-focused (and pane-id (equal pane-id (plist-get data :focused-pane))))
         (strong-focus (and is-focused primary-focus))
         (in-focused-ws (and (plist-get data :focused-ws)
                             (equal ws-id (plist-get data :focused-ws))))
         (bg-face (when strong-focus 'herdr-sanctum-sidebar-tab-active))
         (active-bg (when strong-focus sel-bg))
         (badge (herdr-sanctum-agent-badge (alist-get 'agent agent) status active-bg))
         (primary-face (cond
                        (strong-focus 'herdr-sanctum-sidebar-tab-active)
                        (is-focused 'herdr-sanctum-sidebar-space-expanded)
                        (t 'herdr-sanctum-sidebar-space)))
         (title-str
          (if (and in-focused-ws detail)
              (propertize detail 'face primary-face)
            (concat (propertize ws-label 'face primary-face)
                    (if detail
                        (propertize (concat " · " detail) 'face 'herdr-sanctum-sidebar-meta)
                      ""))))
         (elapsed (herdr-sanctum-agent-elapsed-string pane-id))
         (right (if (string-empty-p elapsed)
                    ""
                  (propertize elapsed 'face 'herdr-sanctum-sidebar-meta)))
         (left (concat (herdr-sanctum-row-accent is-focused active-bg) badge " " title-str))
         (row (herdr-sanctum-format-row left right width win bg-face))
         (a-start (point)))
    (insert row (if strong-focus (propertize "\n" 'face bg-face) "\n"))
    (when (and strong-focus sel-bg)
      (add-face-text-property a-start (1- (point)) (list :background sel-bg) t))
    (add-text-properties
     a-start (1- (point))
     (append
      (when strong-focus '(herdr-sanctum-active-row t))
      (list 'herdr-sanctum-item
           (list :kind 'agent
                 :pane-id pane-id
                 :workspace-id ws-id
                 :tab-id tab-id
                 :label (or detail ws-label))
           'mouse-face 'highlight
           'help-echo (format "%s / %s%s (%s %s)"
                              ws-label tab-label
                              (if (and topic (not (equal topic tab-label)))
                                  (format " · %s" topic)
                                "")
                              (or (alist-get 'agent agent) "agent")
                              status))))))

(defun herdr-sanctum-sidebar--render-agents
    (data width win sel-bg &optional show-header primary-focus)
  "Render the Agents section from DATA into the current buffer.
When SHOW-HEADER is non-nil, prepend an inline Agents section header."
  (let* ((all-groups (plist-get data :agent-groups))
         (allowed (seq-uniq
                   (if herdr-sanctum-sidebar-show-idle-agents
                       herdr-sanctum-sidebar-agent-statuses
                     (seq-remove (lambda (s) (equal s "idle"))
                                 herdr-sanctum-sidebar-agent-statuses))))
         (agent-groups (delq nil
                             (mapcar (lambda (status)
                                       (assoc status all-groups))
                                     allowed)))
         (total-agents (cl-loop for g in agent-groups sum (length (cdr g)))))
    (when show-header
      (insert (herdr-sanctum-format-row
               (propertize "Agents" 'face 'herdr-sanctum-sidebar-header)
               (if (> total-agents 0)
                   (propertize (number-to-string total-agents) 'face 'herdr-sanctum-sidebar-meta)
                 "")
               width win)
              "\n"))
    (if (null agent-groups)
        (insert (propertize " No active agents" 'face 'herdr-sanctum-sidebar-meta) "\n")
      (dolist (group agent-groups)
        (let* ((status (car group))
               (items (cdr group))
               (g-label (format "%s %d" (capitalize status) (length items)))
               (g-face (pcase status
                         ("blocked" 'herdr-sanctum-sidebar-group-blocked)
                         ("working" 'herdr-sanctum-sidebar-group-working)
                         ("done"    'herdr-sanctum-sidebar-group-done)
                         (_         'herdr-sanctum-sidebar-meta))))
          (insert (propertize g-label 'face g-face) "\n")
          (dolist (agent items)
            (herdr-sanctum-sidebar--insert-agent-row
             agent status data width win sel-bg primary-focus)))))))

(defun herdr-sanctum-sidebar--update-chrome (&optional win sections space-count agent-count)
  "Recompute buffer-local `header-line-format' and `mode-line-format'.
WIN defaults to the sidebar's live window; SECTIONS defaults to
`herdr-sanctum-sidebar--active-sections'; SPACE-COUNT and AGENT-COUNT
default to the counts in the current snapshot."
  (let* ((w (or (and (window-live-p win) win)
                (herdr-sanctum-panel-window)))
         (secs (or sections (herdr-sanctum-sidebar--active-sections)))
         (has-spaces (memq 'spaces secs))
         (has-agents (memq 'agents secs))
         (snap (when (or (null space-count) (null agent-count))
                 (herdr-sanctum-snapshot)))
         (n-spaces (or space-count (length (alist-get 'workspaces snap))))
         (n-agents (or agent-count (length (alist-get 'agents snap))))
         (title (pcase (car secs)
                  ('spaces "Spaces")
                  ('agents (if has-spaces "Agents & Spaces" "Agents"))
                  (_ "Sanctum")))
         (hl-left (propertize title
                              'face '(:inherit herdr-sanctum-chrome-bar :weight semi-bold)))
         (hl-right (if has-spaces
                       (herdr-sanctum-make-chrome-button
                        " + "
                        'header-line
                        #'herdr-sanctum-sidebar-new-workspace
                        'herdr-sanctum-chrome-pill
                        "Create new Herdr space (C)")
                     ""))
         (status-text (cond
                       ((and has-spaces has-agents (eq (car secs) 'agents))
                        (format "%d agent%s · %d space%s"
                                n-agents (if (= n-agents 1) "" "s")
                                n-spaces (if (= n-spaces 1) "" "s")))
                       ((and has-spaces has-agents (> n-agents 0))
                        (format "%d space%s · %d agent%s"
                                n-spaces (if (= n-spaces 1) "" "s")
                                n-agents (if (= n-agents 1) "" "s")))
                       (has-spaces
                        (format "%d space%s" n-spaces (if (= n-spaces 1) "" "s")))
                       (has-agents
                        (format "%d agent%s" n-agents (if (= n-agents 1) "" "s")))
                       (t "")))
         (st-left (propertize status-text 'face 'herdr-sanctum-status-bar))
         (st-right
          (pcase herdr-sanctum-sidebar-footer-action
            ('none "")
            (`(,(and (pred stringp) label) . ,(and (pred functionp) fn))
             (herdr-sanctum-make-chrome-button
              label
              'mode-line
              fn
              'herdr-sanctum-status-bar
              (if (symbolp fn) (format "Run `%s'" fn) label)))
            (_
             (if (and (stringp herdr-sanctum-config-file)
                      (not (string-empty-p herdr-sanctum-config-file)))
                 (herdr-sanctum-make-chrome-button
                  "Config"
                  'mode-line
                  #'herdr-sanctum-sidebar-open-config
                  'herdr-sanctum-status-bar
                  (format "Open %s (,)" herdr-sanctum-config-file))
               "")))))
    (setq-local header-line-format
                (herdr-sanctum-format-header-line hl-left hl-right w)
                mode-line-format
                (herdr-sanctum-format-status-bar st-left st-right w))
    (force-mode-line-update)))

(defun herdr-sanctum-sidebar--render (&optional win)
  "Render current Herdr snapshot into `*sanctum-spaces*' for WIN."
  (let* ((buf (current-buffer))
         (win-state (herdr-sanctum-panel-save-windows #'herdr-sanctum-sidebar--item-at))
         (win (or (and (window-live-p win) win)
                  (herdr-sanctum-panel-window buf)))
         (frame (and win (window-frame win)))
         (data (herdr-sanctum-sidebar--prepare-render-data (herdr-sanctum-snapshot)))
         (focused-ws (plist-get data :focused-ws))
         (focused-tab (plist-get data :focused-tab))
         (focused-pane (plist-get data :focused-pane))
         (render-body
          (lambda ()
            (with-current-buffer buf
              (let ((width (max 18 (if (window-live-p win)
                                       (1- (window-body-width win))
                                     (- herdr-sanctum-sidebar-width 3))))
                    (sel-bg (herdr-sanctum-palette-color 'bg-selected))
                    (sections (herdr-sanctum-sidebar--active-sections))
                    (inhibit-read-only t)
                    (prev-section nil))
                (herdr-sanctum-panel-record-size win)
                (herdr-sanctum-sidebar--update-chrome
                 win sections
                 (length (plist-get data :workspaces))
                 (length (alist-get 'agents (plist-get data :snapshot))))
                (erase-buffer)
                (dolist (section sections)
                  (when prev-section
                    (insert "\n")
                    (herdr-sanctum-sidebar--insert-divider width win))
                  (pcase section
                    ('spaces (herdr-sanctum-sidebar--render-spaces
                              data width win sel-bg (not (null prev-section))))
                    ('agents (herdr-sanctum-sidebar--render-agents
                              data width win sel-bg
                              (> (length sections) 1)
                              (not (memq 'spaces sections)))))
                  (setq prev-section section)))))))
    (when (plist-get data :snapshot)
      (let ((ws-labels (plist-get data :ws-labels))
            (stale nil))
        (maphash (lambda (k _v)
                   (unless (gethash k ws-labels)
                     (push k stale)))
                 herdr-sanctum-sidebar--expanded-spaces)
        (dolist (k stale)
          (remhash k herdr-sanctum-sidebar--expanded-spaces))))
    (if (and frame (not (eq frame (selected-frame))))
        (with-selected-frame frame
          (funcall render-body))
      (funcall render-body))
    (let ((fallback (or (and focused-tab
                             (herdr-sanctum-sidebar--find-item-pos 'tab focused-tab))
                        (and focused-ws
                             (herdr-sanctum-sidebar--find-item-pos 'workspace focused-ws))
                        (and focused-pane
                             (herdr-sanctum-sidebar--find-item-pos 'agent focused-pane))
                        (point-min))))
      (herdr-sanctum-panel-restore-windows
       win-state
       (lambda (item)
         (let ((kind (plist-get item :kind)))
           (herdr-sanctum-sidebar--find-item-pos
            kind
            (pcase kind
              ('agent (plist-get item :pane-id))
              ('tab (plist-get item :tab-id))
              (_ (plist-get item :workspace-id))))))
       fallback))
    (herdr-sanctum-sidebar--update-focus-overlay)))

(defun herdr-sanctum-sidebar-refresh (&rest _)
  "Re-render `*sanctum-spaces*' when visible, or defer until shown."
  (when (timerp herdr-sanctum-sidebar--git-refresh-timer)
    (cancel-timer herdr-sanctum-sidebar--git-refresh-timer)
    (setq herdr-sanctum-sidebar--git-refresh-timer nil))
  (herdr-sanctum-panel-refresh-or-mark-dirty
   herdr-sanctum-sidebar-buffer-name
   #'herdr-sanctum-sidebar--render))

(defun herdr-sanctum-sidebar--on-duration-tick ()
  "Refresh `*sanctum-spaces*' on duration ticks only when showing agents."
  (when (and (memq 'agents herdr-sanctum-sidebar-sections)
             (alist-get 'agents (herdr-sanctum-snapshot)))
    (herdr-sanctum-sidebar-refresh)))

(defun herdr-sanctum-sidebar--on-git-update (&rest _)
  "Coalesce bursts of Git updates into a single visible sidebar refresh."
  (when-let* ((buf (get-buffer herdr-sanctum-sidebar-buffer-name)))
    (if (not (herdr-sanctum-panel-window buf))
        (herdr-sanctum-panel-mark-dirty buf)
      (unless (timerp herdr-sanctum-sidebar--git-refresh-timer)
        (setq herdr-sanctum-sidebar--git-refresh-timer
              (run-with-timer
               0 nil
               (lambda ()
                 (setq herdr-sanctum-sidebar--git-refresh-timer nil)
                 (herdr-sanctum-sidebar-refresh))))))))

;;; Sidebar Navigation & Commands

(defun herdr-sanctum-sidebar--step-row (delta pred)
  "Move by DELTA lines until PRED returns non-nil, returning non-nil if moved.
Leaves point at the beginning of the matched line, or restores point on
failure."
  (let ((origin (point))
        (moved nil))
    (while (and (not moved)
                (zerop (forward-line delta)))
      (when (funcall pred)
        (setq moved t)))
    (if moved
        (progn
          (beginning-of-line)
          t)
      (goto-char origin)
      nil)))

(defun herdr-sanctum-sidebar--step-item (delta)
  "Move point by DELTA selectable rows in `*sanctum-spaces*'."
  (herdr-sanctum-sidebar--step-row delta #'herdr-sanctum-sidebar--item-at))

(defun herdr-sanctum-sidebar-next-item ()
  "Move to the next selectable row."
  (interactive)
  (herdr-sanctum-sidebar--step-item 1))

(defun herdr-sanctum-sidebar-prev-item ()
  "Move to the previous selectable row."
  (interactive)
  (herdr-sanctum-sidebar--step-item -1))

(defun herdr-sanctum-sidebar--step-space (delta)
  "Move point by DELTA space rows in `*sanctum-spaces*'."
  (herdr-sanctum-sidebar--step-row
   delta
   (lambda ()
     (eq (plist-get (herdr-sanctum-sidebar--item-at) :kind) 'workspace))))

(defun herdr-sanctum-sidebar-next-space ()
  "Move to the next space row."
  (interactive)
  (herdr-sanctum-sidebar--step-space 1))

(defun herdr-sanctum-sidebar-prev-space ()
  "Move to the previous space row."
  (interactive)
  (herdr-sanctum-sidebar--step-space -1))

(defun herdr-sanctum-sidebar--toggle-workspace-fold (ws-id &optional from-tab win)
  "Toggle expanded tab tree for workspace WS-ID and re-render `*sanctum-spaces*'.
When FROM-TAB is non-nil and collapsing, move point to WS-ID's workspace row.
Optional WIN specifies the sidebar window to render."
  (when ws-id
    (let ((now-expanded (not (herdr-sanctum-sidebar--space-expanded-p ws-id))))
      (herdr-sanctum-sidebar--set-space-expanded ws-id now-expanded)
      (herdr-sanctum-sidebar--render win)
      (when (and (not now-expanded) from-tab)
        (when-let* ((pos (herdr-sanctum-sidebar--find-item-pos 'workspace ws-id)))
          (goto-char pos)
          (beginning-of-line))))))

(defun herdr-sanctum-sidebar-toggle-fold ()
  "Toggle expanded tab tree for the space at point (or parent space of a tab)."
  (interactive)
  (when-let* ((item (herdr-sanctum-sidebar--item-at))
              (kind (plist-get item :kind))
              ((memq kind '(workspace tab)))
              (ws-id (plist-get item :workspace-id)))
    (herdr-sanctum-sidebar--toggle-workspace-fold ws-id (eq kind 'tab))))

(defun herdr-sanctum-sidebar-mouse-toggle-fold (event)
  "Toggle workspace fold for the chevron clicked by mouse EVENT."
  (interactive "e")
  (let* ((end (event-end event))
         (win (posn-window end))
         (pos (posn-point end)))
    (when (and (window-live-p win) (integerp pos))
      (with-current-buffer (window-buffer win)
        (when-let* ((ws-id (or (get-text-property pos 'herdr-sanctum-chevron)
                               (let ((item (herdr-sanctum-sidebar--item-at pos)))
                                 (when (eq (plist-get item :kind) 'workspace)
                                   (plist-get item :workspace-id))))))
          (herdr-sanctum-sidebar--toggle-workspace-fold ws-id nil win))))))

(defun herdr-sanctum-sidebar-toggle-fold-all ()
  "Collapse all spaces when any is expanded, otherwise expand all."
  (interactive)
  (when-let* ((snap (herdr-sanctum-snapshot))
              (workspaces (alist-get 'workspaces snap)))
    (let* ((focused-ws (alist-get 'focused_workspace_id snap))
           (any-expanded (seq-some
                          (lambda (ws)
                            (herdr-sanctum-sidebar--space-expanded-p
                             (alist-get 'workspace_id ws) focused-ws))
                          workspaces)))
      (dolist (ws workspaces)
        (when-let* ((ws-id (alist-get 'workspace_id ws)))
          (herdr-sanctum-sidebar--set-space-expanded ws-id (not any-expanded))))
      (herdr-sanctum-sidebar--render))))

(defun herdr-sanctum-sidebar-left ()
  "Collapse expanded space at point, or jump from a tab to its parent space."
  (interactive)
  (when-let* ((item (herdr-sanctum-sidebar--item-at))
              (ws-id (plist-get item :workspace-id)))
    (pcase (plist-get item :kind)
      ('tab
       (when-let* ((pos (herdr-sanctum-sidebar--find-item-pos 'workspace ws-id)))
         (goto-char pos)
         (beginning-of-line)))
      ('workspace
       (when (herdr-sanctum-sidebar--space-expanded-p ws-id)
         (herdr-sanctum-sidebar--set-space-expanded ws-id nil)
         (herdr-sanctum-sidebar--render))))))

(defun herdr-sanctum-sidebar-right ()
  "Expand collapsed space, enter expanded space's first tab, or activate tab/agent."
  (interactive)
  (when-let* ((item (herdr-sanctum-sidebar--item-at)))
    (pcase (plist-get item :kind)
      ('workspace
       (let ((ws-id (plist-get item :workspace-id)))
         (if (not (herdr-sanctum-sidebar--space-expanded-p ws-id))
             (progn
               (herdr-sanctum-sidebar--set-space-expanded ws-id t)
               (herdr-sanctum-sidebar--render))
           (let ((origin (point)))
             (herdr-sanctum-sidebar--step-item 1)
             (let ((next (herdr-sanctum-sidebar--item-at)))
               (unless (and (eq (plist-get next :kind) 'tab)
                            (equal (plist-get next :workspace-id) ws-id))
                 (goto-char origin)))))))
      ((or 'tab 'agent)
       (herdr-sanctum-sidebar-activate)))))

(defun herdr-sanctum-sidebar-next-agent ()
  "Jump to the next agent row in `*sanctum-spaces*'."
  (interactive)
  (let ((origin (point))
        (agent-p (lambda ()
                   (eq (plist-get (herdr-sanctum-sidebar--item-at) :kind) 'agent))))
    (unless (herdr-sanctum-sidebar--step-row 1 agent-p)
      (goto-char (point-min))
      (unless (or (funcall agent-p)
                  (herdr-sanctum-sidebar--step-row 1 agent-p))
        (goto-char origin)))))

(defun herdr-sanctum-sidebar-select-space-by-digit ()
  "Focus the Nth Herdr space corresponding to the pressed digit key."
  (interactive)
  (let ((ev (event-basic-type last-command-event)))
    (when (and (integerp ev) (<= ?1 ev ?9))
      (let* ((idx (- ev ?1))
             (workspaces (alist-get 'workspaces (herdr-sanctum-snapshot)))
             (ws (nth idx workspaces))
             (ws-id (alist-get 'workspace_id ws)))
        (when ws-id
          (herdr-sanctum-rpc-call "workspace.focus" `((workspace_id . ,ws-id)))
          (herdr-sanctum-focus-terminal))))))

(defun herdr-sanctum-sidebar-focus-item (item focus-terminal)
  "Focus Herdr ITEM (`agent', `tab', or `workspace'); maybe focus `*sanctum*'."
  (let ((dispatched t))
    (pcase (plist-get item :kind)
      ('agent
       (herdr-sanctum-rpc-call "pane.focus" `((pane_id . ,(plist-get item :pane-id)))))
      ('tab
       (herdr-sanctum-rpc-call "tab.focus" `((tab_id . ,(plist-get item :tab-id)))))
      ('workspace
       (herdr-sanctum-rpc-call "workspace.focus" `((workspace_id . ,(plist-get item :workspace-id)))))
      (_
       (setq dispatched nil)))
    (when (and dispatched focus-terminal)
      (if (get-buffer-window herdr-sanctum-buffer-name (selected-frame))
          (herdr-sanctum-focus-terminal)
        (when (fboundp 'herdr-sanctum)
          (herdr-sanctum '(4)))))))

(defun herdr-sanctum-sidebar-activate ()
  "Focus the space, tab, or agent at point and return focus to `*sanctum*'."
  (interactive)
  (when-let* ((item (herdr-sanctum-sidebar--item-at)))
    (herdr-sanctum-sidebar-focus-item item t)))

(defun herdr-sanctum-sidebar-peek ()
  "Focus the space, tab, or agent at point while staying in `*sanctum-spaces*'."
  (interactive)
  (when-let* ((item (herdr-sanctum-sidebar--item-at)))
    (herdr-sanctum-sidebar-focus-item item nil)))

(defun herdr-sanctum-sidebar-mouse-click (event)
  "Dispatch mouse-1 EVENT on `*sanctum-spaces*' without shadowing mode keymap."
  (interactive "e")
  (let* ((end (event-end event))
         (win (posn-window end))
         (pos (posn-point end)))
    (when (and (window-live-p win) (integerp pos))
      (with-current-buffer (window-buffer win)
        (if-let* ((ws-id (get-text-property pos 'herdr-sanctum-chevron)))
            (herdr-sanctum-sidebar--toggle-workspace-fold ws-id nil win)
          (when-let* ((item (herdr-sanctum-sidebar--item-at pos)))
            (herdr-sanctum-sidebar-focus-item item t)))))))

(defun herdr-sanctum-sidebar-new-tab ()
  "Create a new tab in the space at point (or active space) and focus it."
  (interactive)
  (let* ((item (herdr-sanctum-sidebar--item-at))
         (ws-id (or (plist-get item :workspace-id)
                    (alist-get 'focused_workspace_id (herdr-sanctum-snapshot))))
         (label (string-trim (read-string "New tab name (empty for default): ")))
         (params `((focus . t))))
    (when ws-id
      (push (cons 'workspace_id ws-id) params))
    (unless (string-empty-p label)
      (push (cons 'label label) params))
    (herdr-sanctum-rpc-call "tab.create" params)
    (herdr-sanctum-focus-terminal)))

(defun herdr-sanctum-sidebar-new-workspace ()
  "Create a new Herdr space and focus it."
  (interactive)
  (let* ((label (string-trim (read-string "New space name (empty for default): ")))
         (params `((focus . t))))
    (unless (string-empty-p label)
      (push (cons 'label label) params))
    (herdr-sanctum-rpc-call "workspace.create" params)
    (herdr-sanctum-focus-terminal)))

(defun herdr-sanctum-sidebar-rename ()
  "Rename the tab or space at point."
  (interactive)
  (when-let* ((item (herdr-sanctum-sidebar--item-at))
              (kind (plist-get item :kind))
              ((memq kind '(tab workspace))))
    (let* ((old (or (plist-get item :label) ""))
           (prompt (if (eq kind 'tab) "Rename tab: " "Rename space: "))
           (new (string-trim (read-string prompt old))))
      (when (and (not (string-empty-p new)) (not (equal new old)))
        (if (eq kind 'tab)
            (herdr-sanctum-rpc-call "tab.rename"
                                    `((tab_id . ,(plist-get item :tab-id))
                                      (label . ,new)))
          (herdr-sanctum-rpc-call "workspace.rename"
                                  `((workspace_id . ,(plist-get item :workspace-id))
                                    (label . ,new))))))))

(defun herdr-sanctum-sidebar-close ()
  "Close the tab, space, or agent pane at point after confirmation."
  (interactive)
  (when-let* ((item (herdr-sanctum-sidebar--item-at)))
    (let ((kind (plist-get item :kind))
          (label (or (plist-get item :label) "")))
      (pcase kind
        ('agent
         (when-let* ((pane-id (plist-get item :pane-id)))
           (when (y-or-n-p (format "Close agent pane `%s'? "
                                   (if (string-empty-p label) pane-id label)))
             (herdr-sanctum-rpc-call "pane.close" `((pane_id . ,pane-id))))))
        ('tab
         (when (y-or-n-p (format "Close tab `%s'? " label))
           (herdr-sanctum-rpc-call "tab.close" `((tab_id . ,(plist-get item :tab-id))))))
        ('workspace
         (let ((ws-id (plist-get item :workspace-id)))
           (when (y-or-n-p (format "Close space `%s'? " label))
             (herdr-sanctum-rpc-call
              "workspace.close" `((workspace_id . ,ws-id))
              (lambda (_res err)
                (when err
                  (if (equal (alist-get 'code err) "workspace_group_close_required")
                      (run-at-time
                       0 nil
                       (lambda ()
                         (when (y-or-n-p
                                (format "Space `%s' has linked worktrees. Close whole group? "
                                        label))
                           (herdr-sanctum-rpc-call
                            "workspace.close"
                            `((workspace_id . ,ws-id) (close_group . t))))))
                    (message "Herdr workspace.close failed: %s"
                             (or (alist-get 'message err) (alist-get 'code err))))))))))))))

(defun herdr-sanctum-sidebar--move (delta)
  "Move the tab or space at point by DELTA (-1 up, +1 down)."
  (when-let* ((item (herdr-sanctum-sidebar--item-at))
              (idx (plist-get item :index))
              (count (plist-get item :count)))
    ;; Herdr move_tab/move_workspace take a pre-removal insert_index:
    ;; moving up uses idx - 1; moving down one slot uses idx + 2.
    (let ((insert-idx (cond
                       ((and (< delta 0) (> idx 0)) (1- idx))
                       ((and (> delta 0) (< (1+ idx) count)) (+ idx 2)))))
      (when insert-idx
        (if (eq (plist-get item :kind) 'tab)
            (herdr-sanctum-rpc-call "tab.move"
                                    `((tab_id . ,(plist-get item :tab-id))
                                      (insert_index . ,insert-idx)))
          (herdr-sanctum-rpc-call "workspace.move"
                                  `((workspace_id . ,(plist-get item :workspace-id))
                                    (insert_index . ,insert-idx))))))))

(defun herdr-sanctum-sidebar-move-up ()
  "Move the tab or space at point up one position."
  (interactive)
  (herdr-sanctum-sidebar--move -1))

(defun herdr-sanctum-sidebar-move-down ()
  "Move the tab or space at point down one position."
  (interactive)
  (herdr-sanctum-sidebar--move 1))

(defun herdr-sanctum-sidebar-open-config ()
  "Open `herdr-sanctum-config-file' outside Sanctum."
  (interactive)
  (unless (and (stringp herdr-sanctum-config-file)
               (not (string-empty-p herdr-sanctum-config-file)))
    (user-error "No Herdr config file configured (`herdr-sanctum-config-file' is nil)"))
  (let ((path (expand-file-name herdr-sanctum-config-file))
        (opener (or (let ((fn (bound-and-true-p herdr-sanctum-open-file-function)))
                      (and (functionp fn) fn))
                    #'find-file)))
    (herdr-sanctum-open-outside
     (lambda ()
       (herdr-sanctum-invoke-command-or-fn opener path)))))

(defun herdr-sanctum-sidebar-quit ()
  "Leave `*sanctum-spaces*' for `*sanctum*' or another non-side window.
If `*sanctum*' is visible on the current frame, focus it.  Otherwise
select the most recently used non-side window on the current frame, or
toggle the sidebar off when it is the sole window."
  (interactive)
  (let ((cur (selected-window)))
    (cond
     ((when-let* ((term-win (get-buffer-window herdr-sanctum-buffer-name
                                               (selected-frame))))
        (select-window term-win)
        t))
     ((when-let* ((candidates (seq-filter
                               (lambda (w)
                                 (and (not (eq w cur))
                                      (not (window-parameter w 'window-side))))
                               (window-list (selected-frame) 'no-minibuf)))
                  (other (car (sort candidates
                                    (lambda (a b)
                                      (> (window-use-time a)
                                         (window-use-time b)))))))
        (select-window other)
        t))
     (t
      (herdr-sanctum-sidebar-toggle)))))

(defun herdr-sanctum-sidebar-help ()
  "Echo a concise summary of `*sanctum-spaces*' keybindings."
  (interactive)
  (message "Spaces: RET open · o peek · TAB/h/l fold · j/k move · a agent · / jump · c/C new · r rename · d close · , config · q back · Q hide"))

(defun herdr-sanctum-sidebar--update-focus-overlay (&optional _frame)
  "Highlight an active row with `herdr-sanctum-panel-focus-active' when focused."
  (when-let* ((buf (get-buffer herdr-sanctum-sidebar-buffer-name)))
    (with-current-buffer buf
      (let* ((sel-win (selected-window))
             (focused (and (window-live-p sel-win)
                           (eq (window-buffer sel-win) buf)))
             (pt (and focused (window-point sel-win)))
             (active-row (and pt
                              (save-excursion
                                (goto-char pt)
                                (get-text-property (line-beginning-position)
                                                   'herdr-sanctum-active-row)))))
        (if active-row
            (save-excursion
              (goto-char pt)
              (let ((beg (line-beginning-position))
                    (end (min (point-max) (1+ (line-end-position)))))
                (if (overlayp herdr-sanctum-sidebar--focus-overlay)
                    (move-overlay herdr-sanctum-sidebar--focus-overlay beg end buf)
                  (setq herdr-sanctum-sidebar--focus-overlay
                        (make-overlay beg end buf)))
                (overlay-put herdr-sanctum-sidebar--focus-overlay 'priority 100)
                (overlay-put herdr-sanctum-sidebar--focus-overlay 'face
                             'herdr-sanctum-panel-focus-active)))
          (when (overlayp herdr-sanctum-sidebar--focus-overlay)
            (delete-overlay herdr-sanctum-sidebar--focus-overlay)))
        (when (bound-and-true-p hl-line-mode)
          (if focused
              (hl-line-highlight)
            (hl-line-unhighlight)))))))

;;; Major Mode & Lifecycle

(defvar-keymap herdr-sanctum-sidebar-mode-map
  :doc "Keymap for `*sanctum-spaces*'."
  :parent special-mode-map
  "<mouse-1>" #'herdr-sanctum-sidebar-mouse-click
  "RET" #'herdr-sanctum-sidebar-activate
  "o" #'herdr-sanctum-sidebar-peek
  "TAB" #'herdr-sanctum-sidebar-toggle-fold
  "<tab>" #'herdr-sanctum-sidebar-toggle-fold
  "SPC" #'herdr-sanctum-sidebar-toggle-fold
  "<backtab>" #'herdr-sanctum-sidebar-toggle-fold-all
  "j" #'herdr-sanctum-sidebar-next-item
  "n" #'herdr-sanctum-sidebar-next-item
  "C-n" #'herdr-sanctum-sidebar-next-item
  "<down>" #'herdr-sanctum-sidebar-next-item
  "k" #'herdr-sanctum-sidebar-prev-item
  "p" #'herdr-sanctum-sidebar-prev-item
  "C-p" #'herdr-sanctum-sidebar-prev-item
  "<up>" #'herdr-sanctum-sidebar-prev-item
  "h" #'herdr-sanctum-sidebar-left
  "<left>" #'herdr-sanctum-sidebar-left
  "l" #'herdr-sanctum-sidebar-right
  "<right>" #'herdr-sanctum-sidebar-right
  "a" #'herdr-sanctum-sidebar-next-agent
  "/" #'herdr-sanctum-jump
  "s" #'herdr-sanctum-jump
  "M-n" #'herdr-sanctum-sidebar-next-space
  "]" #'herdr-sanctum-sidebar-next-space
  "M-p" #'herdr-sanctum-sidebar-prev-space
  "[" #'herdr-sanctum-sidebar-prev-space
  "c" #'herdr-sanctum-sidebar-new-tab
  "+" #'herdr-sanctum-sidebar-new-tab
  "C" #'herdr-sanctum-sidebar-new-workspace
  "r" #'herdr-sanctum-sidebar-rename
  "d" #'herdr-sanctum-sidebar-close
  "x" #'herdr-sanctum-sidebar-close
  "K" #'herdr-sanctum-sidebar-move-up
  "M-<up>" #'herdr-sanctum-sidebar-move-up
  "J" #'herdr-sanctum-sidebar-move-down
  "M-<down>" #'herdr-sanctum-sidebar-move-down
  "1" #'herdr-sanctum-sidebar-select-space-by-digit
  "2" #'herdr-sanctum-sidebar-select-space-by-digit
  "3" #'herdr-sanctum-sidebar-select-space-by-digit
  "4" #'herdr-sanctum-sidebar-select-space-by-digit
  "5" #'herdr-sanctum-sidebar-select-space-by-digit
  "6" #'herdr-sanctum-sidebar-select-space-by-digit
  "7" #'herdr-sanctum-sidebar-select-space-by-digit
  "8" #'herdr-sanctum-sidebar-select-space-by-digit
  "9" #'herdr-sanctum-sidebar-select-space-by-digit
  "," #'herdr-sanctum-sidebar-open-config
  "g" #'herdr-sanctum-refresh-state
  "?" #'herdr-sanctum-sidebar-help
  "q" #'herdr-sanctum-sidebar-quit
  "<escape>" #'herdr-sanctum-sidebar-quit
  "Q" #'herdr-sanctum-sidebar-toggle)

(define-derived-mode herdr-sanctum-sidebar-mode special-mode "Sanctum-Spaces"
  "Major mode for the persistent Herdr Sanctum sidebar."
  :group 'herdr-sanctum-sidebar
  (herdr-sanctum-init-panel-buffer
   #'herdr-sanctum-sidebar--render
   nil
   (lambda ()
     (when-let* ((snap (herdr-sanctum-snapshot)))
       (herdr-sanctum-git-refresh-snapshot snap))))
  (add-hook 'post-command-hook #'herdr-sanctum-sidebar--update-focus-overlay nil t)
  (add-hook 'window-selection-change-functions
            #'herdr-sanctum-sidebar--update-focus-overlay nil t))

(herdr-sanctum-configure-evil-mode 'herdr-sanctum-sidebar-mode)

;;;###autoload
(defun herdr-sanctum-sidebar-show (&optional select)
  "Ensure `*sanctum-spaces*' is shown in its side-window.
With SELECT non-nil, move focus to the sidebar window."
  (interactive "P")
  (prog1
      (herdr-sanctum-show-panel
       herdr-sanctum-sidebar-buffer-name
       #'herdr-sanctum-sidebar-mode
       herdr-sanctum-sidebar-side
       herdr-sanctum-sidebar-width
       #'herdr-sanctum-sidebar--render
       select
       0)
    (run-hooks 'herdr-sanctum-sidebar-change-hook)))

(defun herdr-sanctum-sidebar-collapse ()
  "Collapse `*sanctum-spaces*' on the current frame and mark it disabled."
  (setq herdr-sanctum-sidebar--enabled 'off)
  (when-let* ((buf (get-buffer herdr-sanctum-sidebar-buffer-name))
              (win (get-buffer-window buf (selected-frame)))
              ((window-live-p win))
              ((eq (window-deletable-p win) t)))
    (delete-window win))
  (run-hooks 'herdr-sanctum-sidebar-change-hook)
  (force-mode-line-update t))

;;;###autoload
(defun herdr-sanctum-sidebar-toggle ()
  "Toggle the `*sanctum-spaces*' side-window."
  (interactive)
  (let ((shown nil))
    (herdr-sanctum-toggle-panel
     herdr-sanctum-sidebar-buffer-name
     (lambda ()
       (herdr-sanctum-maybe-collapse-opposite-panel
        herdr-sanctum-sidebar-side
        herdr-sanctum-sidebar-width
        herdr-sanctum-drawer-buffer-name
        (bound-and-true-p herdr-sanctum-drawer-side)
        (lambda ()
          (when (fboundp 'herdr-sanctum-drawer-collapse)
            (herdr-sanctum-drawer-collapse))))
       (prog1 (herdr-sanctum-sidebar-show)
         (setq shown t)))
     (lambda (enabled)
       (setq herdr-sanctum-sidebar--enabled (if enabled 'on 'off)))
     nil
     (lambda ()
       (herdr-sanctum-terminal-resize)
       (unless shown
         (run-hooks 'herdr-sanctum-sidebar-change-hook))))))

(defun herdr-sanctum-sidebar--cleanup ()
  "Delete sidebar windows and kill `*sanctum-spaces*' on disconnect."
  (when (timerp herdr-sanctum-sidebar--git-refresh-timer)
    (cancel-timer herdr-sanctum-sidebar--git-refresh-timer)
    (setq herdr-sanctum-sidebar--git-refresh-timer nil))
  (herdr-sanctum-cleanup-panel herdr-sanctum-sidebar-buffer-name))

(add-hook 'herdr-sanctum-snapshot-hook #'herdr-sanctum-sidebar-refresh)
(add-hook 'herdr-sanctum-git-update-hook #'herdr-sanctum-sidebar--on-git-update)
(add-hook 'herdr-sanctum-duration-tick-hook #'herdr-sanctum-sidebar--on-duration-tick)
(add-hook 'herdr-sanctum-theme-change-hook #'herdr-sanctum-sidebar-refresh)
(add-hook 'herdr-sanctum-disconnect-hook #'herdr-sanctum-sidebar--cleanup)

(provide 'herdr-sanctum-sidebar)
;;; herdr-sanctum-sidebar.el ends here
