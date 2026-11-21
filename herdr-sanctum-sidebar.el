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
;; expansion) and a bottom-anchored Agents section grouped by
;; Blocked / Working / Done / Idle with elapsed durations.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'herdr-sanctum-rpc)
(require 'herdr-sanctum-git)
(require 'herdr-sanctum-theme)
(require 'herdr-sanctum-terminal)

(declare-function herdr-sanctum-jump "herdr-sanctum" ())

(defcustom herdr-sanctum-sidebar-side 'left
  "Side of the frame where `*sanctum-spaces*' is displayed."
  :type '(choice (const :tag "Left" left)
                 (const :tag "Right" right))
  :group 'herdr-sanctum)

(defcustom herdr-sanctum-sidebar-width 30
  "Column width of the `*sanctum-spaces*' side-window."
  :type 'integer
  :group 'herdr-sanctum)

(defcustom herdr-sanctum-sidebar-default-open t
  "When non-nil, `herdr-sanctum' opens `*sanctum-spaces*' alongside `*sanctum*'."
  :type 'boolean
  :group 'herdr-sanctum)

(defcustom herdr-sanctum-sidebar-pin-agents-bottom t
  "When non-nil, push the Agents section toward the lower half of the sidebar."
  :type 'boolean
  :group 'herdr-sanctum)

(defcustom herdr-sanctum-sidebar-default-expand nil
  "When non-nil, expand workspace tab trees by default in `*sanctum-spaces*'."
  :type 'boolean
  :group 'herdr-sanctum)

(defcustom herdr-sanctum-sidebar-sections '(spaces agents footer)
  "Ordered list of sections rendered in `*sanctum-spaces*'.
Supported symbols are `spaces', `agents', and `footer'."
  :type '(repeat (choice (const :tag "Spaces" spaces)
                         (const :tag "Agents" agents)
                         (const :tag "Footer links" footer)))
  :group 'herdr-sanctum)

(defcustom herdr-sanctum-config-file "~/.config/herdr/config.toml"
  "Path to Herdr's configuration file opened from the sidebar footer."
  :type 'file
  :group 'herdr-sanctum)

(defvar herdr-sanctum-sidebar--enabled nil
  "Current session toggle state for the sidebar (`unset' inherits default).")

(defvar herdr-sanctum-sidebar--expanded-spaces (make-hash-table :test #'equal)
  "Hash table of explicitly toggled Herdr `workspace_id' keys.")

(defvar-local herdr-sanctum-sidebar--last-size nil
  "Window `(WIDTH . HEIGHT)' last used to render `*sanctum-spaces*'.")

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

(defun herdr-sanctum-sidebar--space-expanded-p (ws-id)
  "Return non-nil when workspace WS-ID's tabs are expanded."
  (let ((state (gethash ws-id herdr-sanctum-sidebar--expanded-spaces 'unset)))
    (if (eq state 'unset)
        herdr-sanctum-sidebar-default-expand
      state)))

(defun herdr-sanctum-sidebar--set-space-expanded (ws-id expanded)
  "Set explicit expansion state EXPANDED for workspace WS-ID."
  (puthash ws-id (and expanded t) herdr-sanctum-sidebar--expanded-spaces))

(defun herdr-sanctum-sidebar--space-git-meta (ws snapshot &optional active-bg)
  "Format right-aligned Git metadata (or tab count fallback) for workspace WS."
  (let* ((dir (herdr-sanctum-workspace-dir ws snapshot))
         (info (and dir (herdr-sanctum-git-info-for-dir dir)))
         (tab-count (or (alist-get 'tab_count ws) 1))
         (face-fn (lambda (f) (if active-bg (list f `(:background ,active-bg)) f))))
    (cond
     (info
      (let* ((branch (or (plist-get info :branch) ""))
             (behind (or (plist-get info :behind) 0))
             (ahead (or (plist-get info :ahead) 0))
             (dirty (or (plist-get info :dirty) 0))
             (short-branch (truncate-string-to-width branch 14 nil nil "…")))
        (cond
         ((> behind 0)
          (propertize (format "↓%d" behind)
                      'face (funcall face-fn 'herdr-sanctum-sidebar-git-behind)))
         ((> ahead 0)
          (propertize (format "↑%d" ahead)
                      'face (funcall face-fn 'herdr-sanctum-sidebar-git-ahead)))
         ((not (string-empty-p short-branch))
          (propertize (if (> dirty 0) (concat short-branch " •") short-branch)
                      'face (funcall face-fn 'herdr-sanctum-sidebar-meta)))
         (t ""))))
     ((> tab-count 1)
      (propertize (number-to-string tab-count)
                  'face (funcall face-fn 'herdr-sanctum-sidebar-meta)))
     (t ""))))

(defun herdr-sanctum-sidebar--item-at (&optional pos)
  "Return a plist describing the Sanctum sidebar item at POS (default point)."
  (let ((p (or pos (point))))
    (when-let* ((kind (get-text-property p 'herdr-sanctum-item-kind)))
      (list :kind kind
            :pane-id (get-text-property p 'herdr-sanctum-pane-id)
            :workspace-id (get-text-property p 'herdr-sanctum-workspace-id)
            :tab-id (get-text-property p 'herdr-sanctum-tab-id)
            :label (get-text-property p 'herdr-sanctum-label)
            :index (get-text-property p 'herdr-sanctum-index)
            :count (get-text-property p 'herdr-sanctum-count)))))

(defun herdr-sanctum-sidebar--find-item-pos (kind id)
  "Find buffer position of KIND (`agent', `workspace', or `tab') with ID."
  (let ((prop (pcase kind
                ('agent 'herdr-sanctum-pane-id)
                ('tab 'herdr-sanctum-tab-id)
                (_ 'herdr-sanctum-workspace-id)))
        (pos (point-min))
        found)
    (while (and (not found) (< pos (point-max)))
      (if (and (eq (get-text-property pos 'herdr-sanctum-item-kind) kind)
               (equal (get-text-property pos prop) id))
          (setq found pos)
        (setq pos (next-single-property-change
                   pos 'herdr-sanctum-item-kind nil (point-max)))))
    found))

(defun herdr-sanctum-sidebar--render ()
  "Render current Herdr snapshot into `*sanctum-spaces*'."
  (let* ((prev (herdr-sanctum-sidebar--item-at))
         (win (get-buffer-window (current-buffer)))
         (width (max 18 (if (window-live-p win)
                            (1- (window-body-width win))
                          (- herdr-sanctum-sidebar-width 3))))
         (sel-bg (herdr-sanctum-palette-color 'bg-selected))
         (snapshot (herdr-sanctum-snapshot))
         (workspaces (alist-get 'workspaces snapshot))
         (tabs (alist-get 'tabs snapshot))
         (agent-groups (and snapshot (herdr-sanctum-grouped-agents snapshot)))
         (focused-ws (alist-get 'focused_workspace_id snapshot))
         (focused-tab (alist-get 'focused_tab_id snapshot))
         (focused-pane (alist-get 'focused_pane_id snapshot))
         (tabs-by-ws (make-hash-table :test #'equal))
         (ws-labels (make-hash-table :test #'equal))
         (tab-labels (make-hash-table :test #'equal))
         (live-ws (make-hash-table :test #'equal))
         (inhibit-read-only t)
         (top-lines 0)
         (bottom-lines 0))
    (setq herdr-sanctum-sidebar--last-size
          (if (window-live-p win)
              (cons (window-body-width win) (window-body-height win))
            (cons width 0)))
    (dolist (tab tabs)
      (when-let* ((ws-id (alist-get 'workspace_id tab)))
        (puthash ws-id (cons tab (gethash ws-id tabs-by-ws)) tabs-by-ws))
      (when-let* ((tab-id (alist-get 'tab_id tab)))
        (puthash tab-id (or (alist-get 'label tab) tab-id) tab-labels)))
    (maphash (lambda (k v) (puthash k (nreverse v) tabs-by-ws)) tabs-by-ws)
    (dolist (ws workspaces)
      (when-let* ((ws-id (alist-get 'workspace_id ws)))
        (puthash ws-id t live-ws)
        (puthash ws-id (or (alist-get 'label ws) ws-id) ws-labels)))
    (when snapshot
      (let (stale)
        (maphash (lambda (k _v)
                   (unless (gethash k live-ws) (push k stale)))
                 herdr-sanctum-sidebar--expanded-spaces)
        (dolist (k stale)
          (remhash k herdr-sanctum-sidebar--expanded-spaces))))
    (erase-buffer)
    ;; Spaces section
    (when (memq 'spaces herdr-sanctum-sidebar-sections)
      (let* ((title (propertize "Spaces" 'face 'herdr-sanctum-sidebar-header))
             (add-btn (propertize "+"
                                  'face 'herdr-sanctum-sidebar-meta
                                  'mouse-face 'highlight
                                  'herdr-sanctum-click-action 'new-space
                                  'help-echo "Create new Herdr space (C)")))
        (insert (herdr-sanctum-format-row title add-btn width win) "\n")
        (cl-incf top-lines))
      (cond
       ((null snapshot)
        (insert (propertize "Connecting to herdr…" 'face 'herdr-sanctum-sidebar-meta) "\n")
        (cl-incf top-lines))
       ((null workspaces)
        (insert (propertize "No workspaces" 'face 'herdr-sanctum-sidebar-meta) "\n")
        (cl-incf top-lines))
       (t
        (let ((ws-count (length workspaces))
              (ws-idx 0))
          (dolist (ws workspaces)
            (let* ((ws-id (alist-get 'workspace_id ws))
                   (ws-label (or (alist-get 'label ws) ws-id))
                   (ws-active (equal ws-id focused-ws))
                   (ws-active-tab (alist-get 'active_tab_id ws))
                   (ws-tabs (gethash ws-id tabs-by-ws))
                   (tab-count (or (alist-get 'tab_count ws) (length ws-tabs)))
                   (expanded (herdr-sanctum-sidebar--space-expanded-p ws-id))
                   (worktree (alist-get 'worktree ws))
                   (linked-wt (and worktree (alist-get 'is_linked_worktree worktree)))
                   (bg-face (when ws-active 'herdr-sanctum-sidebar-space-active))
                   (active-bg (when ws-active sel-bg))
                   (sp (propertize " " 'face (or bg-face 'default)))
                   (badge (herdr-sanctum-space-badge ws-label ws-id active-bg))
                   (wt-prefix (if linked-wt
                                  (propertize "↳ " 'face (or bg-face 'herdr-sanctum-sidebar-meta))
                                ""))
                   (label-str (propertize ws-label
                                          'face (if ws-active
                                                    'herdr-sanctum-sidebar-space-active
                                                  'herdr-sanctum-sidebar-space)))
                   (right (herdr-sanctum-sidebar--space-git-meta ws snapshot active-bg))
                   (left (concat (herdr-sanctum-row-accent ws-active active-bg)
                                 wt-prefix badge (if (string-empty-p badge) "" sp) label-str))
                   (row (herdr-sanctum-format-row left right width win bg-face))
                   (line-start (point)))
              (insert row (if ws-active (propertize "\n" 'face bg-face) "\n"))
              (cl-incf top-lines)
              (add-text-properties
               line-start (1- (point))
               `(herdr-sanctum-item-kind workspace
                 herdr-sanctum-workspace-id ,ws-id
                 herdr-sanctum-label ,ws-label
                 herdr-sanctum-index ,ws-idx
                 herdr-sanctum-count ,ws-count
                 mouse-face highlight
                 herdr-sanctum-click-action activate
                 help-echo ,(format "Space %s (%d tab%s; TAB to %s tabs)"
                                    ws-label tab-count (if (= tab-count 1) "" "s")
                                    (if expanded "collapse" "expand"))))
              (when expanded
                (let ((tab-idx 0)
                      (n-tabs (length ws-tabs)))
                  (dolist (tab ws-tabs)
                    (let* ((tab-id (alist-get 'tab_id tab))
                           (tab-label (or (alist-get 'label tab) tab-id))
                           (is-focused (equal tab-id focused-tab))
                           (is-ws-active (and (not is-focused) (equal tab-id ws-active-tab)))
                           (pane-count (or (alist-get 'pane_count tab) 1))
                           (t-bg-face (when is-focused 'herdr-sanctum-sidebar-tab-active))
                           (t-active-bg (when is-focused sel-bg))
                           (guide (herdr-sanctum-row-accent is-focused t-active-bg t))
                           (t-sp (propertize " " 'face (or t-bg-face 'default)))
                           (glyph (or (herdr-sanctum-status-glyph
                                       (alist-get 'agent_status tab) t-active-bg)
                                      t-sp))
                           (lbl-face (cond
                                      (is-focused 'herdr-sanctum-sidebar-tab-active)
                                      (ws-active 'herdr-sanctum-sidebar-tab)
                                      (is-ws-active 'herdr-sanctum-sidebar-tab-ws-active)
                                      (t 'herdr-sanctum-sidebar-tab-dim)))
                           (lbl (propertize tab-label 'face lbl-face))
                           (t-right (if (> pane-count 1)
                                        (propertize (format "%dp" pane-count)
                                                    'face (if t-active-bg
                                                              (list 'herdr-sanctum-sidebar-meta
                                                                    `(:background ,t-active-bg))
                                                            'herdr-sanctum-sidebar-meta))
                                      ""))
                           (t-left (concat "  " guide t-sp glyph t-sp lbl))
                           (t-row (herdr-sanctum-format-row t-left t-right width win t-bg-face))
                           (t-start (point)))
                      (insert t-row (if is-focused (propertize "\n" 'face t-bg-face) "\n"))
                      (cl-incf top-lines)
                      (add-text-properties
                       t-start (1- (point))
                       `(herdr-sanctum-item-kind tab
                         herdr-sanctum-workspace-id ,ws-id
                         herdr-sanctum-tab-id ,tab-id
                         herdr-sanctum-label ,tab-label
                         herdr-sanctum-index ,tab-idx
                         herdr-sanctum-count ,n-tabs
                         mouse-face highlight
                         herdr-sanctum-click-action activate
                         help-echo ,(format "%s / %s (%d pane%s)"
                                            ws-label tab-label pane-count
                                            (if (= pane-count 1) "" "s")))))
                    (cl-incf tab-idx)))))
            (cl-incf ws-idx))))))
    ;; Bottom spacer before Agents / Footer
    (when (or (memq 'agents herdr-sanctum-sidebar-sections)
              (memq 'footer herdr-sanctum-sidebar-sections))
      (setq bottom-lines
            (+ (if (memq 'agents herdr-sanctum-sidebar-sections)
                   (+ 2 (if agent-groups
                            (cl-loop for grp in agent-groups
                                     sum (+ 1 (length (cdr grp))))
                          1))
                 0)
               (if (memq 'footer herdr-sanctum-sidebar-sections) 3 0)))
      (let ((spacer (if (and herdr-sanctum-sidebar-pin-agents-bottom
                             (memq 'spaces herdr-sanctum-sidebar-sections))
                        (let ((avail (if (window-live-p win)
                                         (if (display-graphic-p)
                                             (floor (window-body-height win t)
                                                    (max 1 (round (* (frame-char-height (window-frame win)) 1.24))))
                                           (window-body-height win))
                                       0)))
                          (max 1 (- avail top-lines bottom-lines 1)))
                      (if (memq 'spaces herdr-sanctum-sidebar-sections) 1 0))))
        (when (> spacer 0)
          (insert (make-string spacer ?\n)))))
    ;; Agents section
    (when (memq 'agents herdr-sanctum-sidebar-sections)
      (when (memq 'spaces herdr-sanctum-sidebar-sections)
        (insert (herdr-sanctum-divider-line width win) "\n"))
      (let ((total-agents (length (alist-get 'agents snapshot))))
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
              (let* ((pane-id (alist-get 'pane_id agent))
                     (ws-id (alist-get 'workspace_id agent))
                     (tab-id (alist-get 'tab_id agent))
                     (ws-label (or (gethash ws-id ws-labels) ws-id ""))
                     (tab-label (or (gethash tab-id tab-labels) tab-id ""))
                     (raw-topic (alist-get 'topic (alist-get 'tokens agent)))
                     (topic (and (stringp raw-topic)
                                 (let ((tr (string-trim raw-topic)))
                                   (unless (string-empty-p tr) tr))))
                     (detail (if (and topic (not (equal topic ws-label)))
                                 topic
                               (unless (equal tab-label ws-label) tab-label)))
                     (is-focused (and pane-id (equal pane-id focused-pane)))
                     (bg-face (when is-focused 'herdr-sanctum-sidebar-tab-active))
                     (active-bg (when is-focused sel-bg))
                     (sp (propertize " " 'face (or bg-face 'default)))
                     (badge (herdr-sanctum-agent-badge (alist-get 'agent agent) status active-bg))
                     (ws-str (propertize ws-label
                                         'face (if is-focused
                                                   'herdr-sanctum-sidebar-tab-active
                                                 'herdr-sanctum-sidebar-space)))
                     (detail-str (if detail
                                     (propertize (concat " · " detail)
                                                 'face (if active-bg
                                                           (list 'herdr-sanctum-sidebar-meta
                                                                 `(:background ,active-bg))
                                                         'herdr-sanctum-sidebar-meta))
                                   ""))
                     (elapsed (herdr-sanctum-agent-elapsed-string pane-id))
                     (right (if (string-empty-p elapsed)
                                ""
                              (propertize elapsed
                                          'face (if active-bg
                                                    (list 'herdr-sanctum-sidebar-meta
                                                          `(:background ,active-bg))
                                                  'herdr-sanctum-sidebar-meta))))
                     (left (concat (herdr-sanctum-row-accent is-focused active-bg)
                                   badge sp ws-str detail-str))
                     (row (herdr-sanctum-format-row left right width win bg-face))
                     (a-start (point)))
                (insert row (if is-focused (propertize "\n" 'face bg-face) "\n"))
                (add-text-properties
                 a-start (1- (point))
                 `(herdr-sanctum-item-kind agent
                   herdr-sanctum-pane-id ,pane-id
                   herdr-sanctum-workspace-id ,ws-id
                   herdr-sanctum-tab-id ,tab-id
                   herdr-sanctum-label ,(or detail ws-label)
                   mouse-face highlight
                   herdr-sanctum-click-action activate
                   help-echo ,(format "%s / %s%s (%s %s)"
                                      ws-label tab-label
                                      (if (and topic (not (equal topic tab-label)))
                                          (format " · %s" topic)
                                        "")
                                      (or (alist-get 'agent agent) "agent")
                                      status)))))))))
    ;; Footer utility links
    (when (memq 'footer herdr-sanctum-sidebar-sections)
      (insert (herdr-sanctum-divider-line width win) "\n")
      (insert (propertize "+ New space"
                          'face 'herdr-sanctum-sidebar-meta
                          'mouse-face 'highlight
                          'herdr-sanctum-click-action 'new-space
                          'help-echo "Create a new Herdr space (C)")
              "\n")
      (insert (propertize "Herdr config"
                          'face 'herdr-sanctum-sidebar-meta
                          'mouse-face 'highlight
                          'herdr-sanctum-click-action 'open-config
                          'help-echo (format "Open %s" herdr-sanctum-config-file))
              "\n"))
    ;; Restore point
    (let ((target (or (and prev
                           (herdr-sanctum-sidebar--find-item-pos
                            (plist-get prev :kind)
                            (pcase (plist-get prev :kind)
                              ('agent (plist-get prev :pane-id))
                              ('tab (plist-get prev :tab-id))
                              (_ (plist-get prev :workspace-id)))))
                      (and focused-tab (herdr-sanctum-sidebar--find-item-pos 'tab focused-tab))
                      (and focused-ws (herdr-sanctum-sidebar--find-item-pos 'workspace focused-ws))
                      (point-min))))
      (goto-char target)
      (beginning-of-line))))

(defun herdr-sanctum-sidebar-refresh (&rest _)
  "Re-render `*sanctum-spaces*' when its buffer is live."
  (when-let* ((buf (get-buffer herdr-sanctum-sidebar-buffer-name)))
    (with-current-buffer buf
      (herdr-sanctum-sidebar--render))))

(defun herdr-sanctum-sidebar--on-window-size-change (frame)
  "Re-render `*sanctum-spaces*' when its window geometry changes on FRAME."
  (when-let* ((buf (get-buffer herdr-sanctum-sidebar-buffer-name))
              (win (get-buffer-window buf frame)))
    (let ((sz (cons (window-body-width win) (window-body-height win))))
      (with-current-buffer buf
        (unless (equal sz herdr-sanctum-sidebar--last-size)
          (herdr-sanctum-sidebar--render))))))

;;; Sidebar Navigation & Commands

(defun herdr-sanctum-sidebar--step-item (delta)
  "Move point by DELTA selectable rows in `*sanctum-spaces*'."
  (let ((origin (point))
        (moved nil))
    (while (and (not moved)
                (zerop (forward-line delta)))
      (when (herdr-sanctum-sidebar--item-at)
        (setq moved t)))
    (unless moved
      (goto-char origin))
    (beginning-of-line)))

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
  (let ((origin (point))
        (moved nil))
    (while (and (not moved)
                (zerop (forward-line delta)))
      (when (eq (plist-get (herdr-sanctum-sidebar--item-at) :kind) 'workspace)
        (setq moved t)))
    (unless moved
      (goto-char origin))
    (beginning-of-line)))

(defun herdr-sanctum-sidebar-next-space ()
  "Move to the next space row."
  (interactive)
  (herdr-sanctum-sidebar--step-space 1))

(defun herdr-sanctum-sidebar-prev-space ()
  "Move to the previous space row."
  (interactive)
  (herdr-sanctum-sidebar--step-space -1))

(defun herdr-sanctum-sidebar-toggle-fold ()
  "Toggle expanded tab tree for the space at point (or parent space of a tab)."
  (interactive)
  (when-let* ((item (herdr-sanctum-sidebar--item-at))
              (ws-id (plist-get item :workspace-id)))
    (let ((now-expanded (not (herdr-sanctum-sidebar--space-expanded-p ws-id))))
      (herdr-sanctum-sidebar--set-space-expanded ws-id now-expanded)
      (herdr-sanctum-sidebar--render)
      (when (and (not now-expanded)
                 (eq (plist-get item :kind) 'tab))
        (when-let* ((pos (herdr-sanctum-sidebar--find-item-pos 'workspace ws-id)))
          (goto-char pos)
          (beginning-of-line))))))

(defun herdr-sanctum-sidebar-toggle-fold-all ()
  "Collapse all spaces when any is expanded, otherwise expand all."
  (interactive)
  (when-let* ((workspaces (alist-get 'workspaces (herdr-sanctum-snapshot))))
    (let ((any-expanded (seq-some
                         (lambda (ws)
                           (herdr-sanctum-sidebar--space-expanded-p
                            (alist-get 'workspace_id ws)))
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
             (unless (and (eq (plist-get (herdr-sanctum-sidebar--item-at) :kind) 'tab)
                          (equal (plist-get (herdr-sanctum-sidebar--item-at) :workspace-id) ws-id))
               (goto-char origin))))))
      ((or 'tab 'agent)
       (herdr-sanctum-sidebar-activate)))))

(defun herdr-sanctum-sidebar-next-agent ()
  "Jump to the next agent row in `*sanctum-spaces*'."
  (interactive)
  (let ((origin (point))
        found)
    (while (and (not found) (zerop (forward-line 1)))
      (when (eq (plist-get (herdr-sanctum-sidebar--item-at) :kind) 'agent)
        (setq found (point))))
    (unless found
      (goto-char (point-min))
      (while (and (not found) (< (point) origin))
        (if (eq (plist-get (herdr-sanctum-sidebar--item-at) :kind) 'agent)
            (setq found (point))
          (unless (zerop (forward-line 1))
            (goto-char origin)))))
    (goto-char (or found origin))
    (beginning-of-line)))

(defun herdr-sanctum-sidebar-select-space-by-digit ()
  "Focus the Nth Herdr space corresponding to the pressed digit key."
  (interactive)
  (let* ((idx (1- (- (event-basic-type last-command-event) ?0)))
         (workspaces (alist-get 'workspaces (herdr-sanctum-snapshot)))
         (ws (and (>= idx 0) (nth idx workspaces)))
         (ws-id (alist-get 'workspace_id ws)))
    (when ws-id
      (herdr-sanctum-rpc-call "workspace.focus" `((workspace_id . ,ws-id)))
      (herdr-sanctum-focus-terminal))))

(defun herdr-sanctum-sidebar-focus-item (item focus-terminal)
  "Focus Herdr ITEM (`agent', `tab', or `workspace'); maybe focus `*sanctum*'."
  (pcase (plist-get item :kind)
    ('agent
     (herdr-sanctum-rpc-call "pane.focus" `((pane_id . ,(plist-get item :pane-id))))
     (when focus-terminal (herdr-sanctum-focus-terminal)))
    ('tab
     (herdr-sanctum-rpc-call "tab.focus" `((tab_id . ,(plist-get item :tab-id))))
     (when focus-terminal (herdr-sanctum-focus-terminal)))
    ('workspace
     (herdr-sanctum-rpc-call "workspace.focus" `((workspace_id . ,(plist-get item :workspace-id))))
     (when focus-terminal (herdr-sanctum-focus-terminal)))))

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
      (let ((action (with-current-buffer (window-buffer win)
                      (get-text-property pos 'herdr-sanctum-click-action)))
            (item (with-current-buffer (window-buffer win)
                    (herdr-sanctum-sidebar--item-at pos))))
        (pcase action
          ('new-space
           (with-selected-window win
             (herdr-sanctum-sidebar-new-workspace)))
          ('open-config
           (herdr-sanctum-open-outside
            (lambda ()
              (find-file (expand-file-name herdr-sanctum-config-file)))))
          ('toggle-fold
           (with-selected-window win
             (goto-char pos)
             (herdr-sanctum-sidebar-toggle-fold)))
          ('activate
           (when item
             (herdr-sanctum-sidebar-focus-item item t))))))))

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
  "Close the tab or space at point after confirmation."
  (interactive)
  (when-let* ((item (herdr-sanctum-sidebar--item-at)))
    (let ((kind (plist-get item :kind))
          (label (or (plist-get item :label) "")))
      (pcase kind
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
                      (when (y-or-n-p (format "Space `%s' has linked worktrees. Close whole group? " label))
                        (herdr-sanctum-rpc-call "workspace.close"
                                                `((workspace_id . ,ws-id) (close_group . t))))
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

;;; Bottom Status Strip & Major Mode

(defun herdr-sanctum-sidebar-status-left ()
  "Format the left segment of `*sanctum-spaces*' bottom status strip."
  (let* ((raw (car (split-string (system-name) "\\.")))
         (label (if (> (length raw) 14) "local" raw)))
    (propertize (format " %s" label) 'face 'herdr-sanctum-status-bar)))

(defun herdr-sanctum-sidebar-status-right ()
  "Format the right segment of `*sanctum-spaces*' bottom status strip."
  (let ((n (length (alist-get 'workspaces (herdr-sanctum-snapshot)))))
    (propertize (format "%d space%s " n (if (= n 1) "" "s"))
                'face 'herdr-sanctum-status-bar)))

(defconst herdr-sanctum-sidebar-status-bar-format
  '((:eval (herdr-sanctum-sidebar-status-left))
    mode-line-format-right-align
    (:eval (herdr-sanctum-sidebar-status-right)))
  "Top-level `mode-line-format' for `*sanctum-spaces*'.")

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
  "g" #'herdr-sanctum-refresh-state
  "I" #'herdr-sanctum-toggle-agent-icon-style
  "q" #'herdr-sanctum-focus-terminal
  "<escape>" #'herdr-sanctum-focus-terminal)

(define-derived-mode herdr-sanctum-sidebar-mode special-mode "Sanctum-Spaces"
  "Major mode for the persistent Herdr Sanctum sidebar."
  :group 'herdr-sanctum
  (let ((table (make-display-table)))
    (set-display-table-slot table 'truncation ?\s)
    (setq-local buffer-display-table table))
  (setq-local truncate-lines t
              cursor-type nil
              mode-line-format herdr-sanctum-sidebar-status-bar-format
              header-line-format nil
              line-spacing 0.22
              left-fringe-width 0
              right-fringe-width 0
              left-margin-width 1
              right-margin-width 1)
  (hl-line-mode 1)
  (let ((bg (herdr-sanctum-palette-color 'bg-chrome))
        (hl (herdr-sanctum-palette-color 'bg-hl)))
    (setq-local face-remapping-alist (herdr-sanctum-ui-remapping bg hl))))

;;;###autoload
(defun herdr-sanctum-sidebar-show (&optional select)
  "Ensure `*sanctum-spaces*' is shown in its side-window.
With SELECT non-nil, move focus to the sidebar window."
  (interactive "P")
  (let* ((buf (get-buffer-create herdr-sanctum-sidebar-buffer-name))
         (origin (selected-window))
         (action `((display-buffer-reuse-window display-buffer-in-side-window)
                   (side . ,herdr-sanctum-sidebar-side)
                   (slot . 0)
                   (window-width . ,herdr-sanctum-sidebar-width)
                   (dedicated . t)
                   (window-parameters . ((no-delete-other-windows . t))))))
    (with-current-buffer buf
      (unless (derived-mode-p 'herdr-sanctum-sidebar-mode)
        (herdr-sanctum-sidebar-mode))
      (herdr-sanctum-sidebar--render))
    (let ((win (display-buffer buf action)))
      (when (window-live-p win)
        (set-window-fringes win 0 0)
        (set-window-margins win 1 1)
        (set-window-dedicated-p win t)
        (with-current-buffer buf
          (herdr-sanctum-sidebar--render))
        (if select
            (select-window win)
          (when (window-live-p origin)
            (select-window origin))))
      win)))

;;;###autoload
(defun herdr-sanctum-sidebar-toggle ()
  "Toggle the `*sanctum-spaces*' side-window."
  (interactive)
  (let ((existing (and (get-buffer herdr-sanctum-sidebar-buffer-name)
                       (get-buffer-window herdr-sanctum-sidebar-buffer-name (selected-frame)))))
    (if existing
        (progn
          (setq herdr-sanctum-sidebar--enabled 'off)
          (delete-window existing))
      (setq herdr-sanctum-sidebar--enabled 'on)
      (herdr-sanctum-sidebar-show)
      (herdr-sanctum-ensure-connected))
    (herdr-sanctum-terminal-resize)
    (force-mode-line-update t)))

(defun herdr-sanctum-sidebar--cleanup ()
  "Delete sidebar windows and kill `*sanctum-spaces*' on disconnect."
  (when-let* ((buf (get-buffer herdr-sanctum-sidebar-buffer-name)))
    (dolist (win (get-buffer-window-list buf nil t))
      (when (and (window-live-p win) (window-parent win))
        (delete-window win)))
    (kill-buffer buf)))

(add-hook 'window-size-change-functions #'herdr-sanctum-sidebar--on-window-size-change)
(add-hook 'herdr-sanctum-snapshot-hook #'herdr-sanctum-sidebar-refresh)
(add-hook 'herdr-sanctum-git-update-hook #'herdr-sanctum-sidebar-refresh)
(add-hook 'herdr-sanctum-duration-tick-hook #'herdr-sanctum-sidebar-refresh)
(add-hook 'herdr-sanctum-theme-change-hook #'herdr-sanctum-sidebar-refresh)
(add-hook 'herdr-sanctum-disconnect-hook #'herdr-sanctum-sidebar--cleanup)

(provide 'herdr-sanctum-sidebar)
;;; herdr-sanctum-sidebar.el ends here
