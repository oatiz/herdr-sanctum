;;; herdr-sanctum-drawer.el --- Extensible context drawer for herdr-sanctum -*- lexical-binding: t; -*-

;; Copyright (C) 2026 oatiz

;; Author: oatiz <iamoatiz@gmail.com>
;; URL: https://github.com/oatiz/herdr-sanctum
;; Keywords: tools, vc, files

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

;; Toggleable context side-window (`*sanctum-drawer*') backed by a
;; declarative view registry (`herdr-sanctum-register-drawer-view').
;; Ships with three built-in views: `overview', `changes', and `files',
;; switched via buffer-local `tab-line-format'.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'herdr-sanctum-rpc)
(require 'herdr-sanctum-git)
(require 'herdr-sanctum-theme)
(require 'herdr-sanctum-ui)
(require 'herdr-sanctum-terminal)

(declare-function magit-diff-working-tree "magit-diff" (&optional rev args files))
(declare-function magit-status-setup-buffer "magit-status" (&optional directory))
(declare-function nerd-icons-icon-for-file "nerd-icons" (file &rest arg-overrides))

(defgroup herdr-sanctum-drawer nil
  "Extensible context drawer settings for `herdr-sanctum'."
  :group 'herdr-sanctum
  :prefix "herdr-sanctum-")

(defcustom herdr-sanctum-drawer-side 'right
  "Side of the frame where `*sanctum-drawer*' is displayed."
  :type '(choice (const :tag "Right" right)
                 (const :tag "Left" left))
  :group 'herdr-sanctum-drawer)

(defcustom herdr-sanctum-drawer-width 32
  "Column width of the `*sanctum-drawer*' side-window."
  :type 'integer
  :group 'herdr-sanctum-drawer)

(defcustom herdr-sanctum-drawer-default-open nil
  "When non-nil, `herdr-sanctum' opens `*sanctum-drawer*' alongside `*sanctum*'."
  :type 'boolean
  :group 'herdr-sanctum-drawer)

(defcustom herdr-sanctum-drawer-enabled-views '(overview changes files)
  "Ordered list of view IDs shown in `*sanctum-drawer*' and the top chrome bar."
  :type '(repeat symbol)
  :group 'herdr-sanctum-drawer)

(defcustom herdr-sanctum-open-file-function #'find-file
  "Function `(FILE)' called to open a file from `*sanctum-drawer*'."
  :type 'function
  :group 'herdr-sanctum-drawer)

(defcustom herdr-sanctum-diff-label "Diff"
  "Button label used for the working-tree diff action in `*sanctum-drawer*'."
  :type 'string
  :group 'herdr-sanctum-drawer)

(defcustom herdr-sanctum-diff-function #'herdr-sanctum-default-diff
  "Zero-argument function opening a diff for `default-directory'."
  :type 'function
  :group 'herdr-sanctum-drawer)

(defcustom herdr-sanctum-git-status-function #'herdr-sanctum-default-git-status
  "Function `(DIR)' opening a Git status buffer for DIR."
  :type 'function
  :group 'herdr-sanctum-drawer)

(defcustom herdr-sanctum-project-find-file-function #'project-find-file
  "Zero-argument function to find a file in the current project."
  :type 'function
  :group 'herdr-sanctum-drawer)

(defcustom herdr-sanctum-file-icon-function #'herdr-sanctum-default-file-icon
  "Function `(FILENAME &optional FRAME)' returning an icon prefix string.
One-argument `(FILENAME)' functions are also supported."
  :type 'function
  :group 'herdr-sanctum-drawer)

(defvar herdr-sanctum-drawer-change-hook nil
  "Hook run when drawer visibility or registered views change.")

(defvar herdr-sanctum-drawer-views nil
  "Alist of `(ID . PLIST)' registered drawer views.
Use `herdr-sanctum-register-drawer-view' to add or replace views.")

(defvar herdr-sanctum-drawer--enabled nil
  "Current session toggle state for the drawer (`nil' inherits default).")

(defvar herdr-sanctum-drawer--tab 'overview
  "Active view ID in `*sanctum-drawer*'.")

(defvar herdr-sanctum-drawer--files-dir nil
  "Current directory browsed by `*sanctum-drawer*' in `files' view.")

(defvar herdr-sanctum-drawer--last-focused-dir nil
  "Most recently focused Herdr pane directory synced into `*sanctum-drawer*'.")

(defvar herdr-sanctum-drawer--git-refresh-timer nil
  "Zero-delay timer coalescing Git metadata updates for `*sanctum-drawer*'.")

(defvar-local herdr-sanctum-drawer--files-cache nil
  "Cached `(DIR DIRS . FILES)' directory listing for the `files' view.")

(defvar-local herdr-sanctum-drawer--render-key nil
  "Cons `(VIEW-ID . DIR)' last rendered into the `*sanctum-drawer*' body.")

;;; Default External Actions

(defun herdr-sanctum-default-diff ()
  "Open a working-tree diff via Magit if installed, else built-in `vc-diff'."
  (if (require 'magit-diff nil t)
      (magit-diff-working-tree)
    (vc-diff nil t)))

(defun herdr-sanctum-default-git-status (dir)
  "Open Git status for DIR via Magit if installed, else built-in `vc-dir'."
  (if (require 'magit-status nil t)
      (magit-status-setup-buffer dir)
    (vc-dir dir)))

(defun herdr-sanctum-default-file-icon (filename &optional frame)
  "Return a `nerd-icons' prefix for FILENAME on GUI FRAME, else spacing."
  (if (and (display-graphic-p frame) (fboundp 'nerd-icons-icon-for-file))
      (concat (nerd-icons-icon-for-file filename) " ")
    "  "))

;;; View Registry & Shared Helpers

(defun herdr-sanctum-register-drawer-view (id &rest spec)
  "Register or replace drawer view ID with plist SPEC.
SPEC keys:
  `:label'     -- display string (required)
  `:render-fn' -- function `(CTX)' inserting buffer contents (required)
                  CTX is a plist with `:width', `:win', `:snapshot',
                  `:workspace', `:dir', and `:git'; interactive spans set
                  text property `herdr-sanctum-drawer-item' to a plist
                  `(:action ACT :value VAL :id ID :full-row BOOL)'
  `:badge-fn'  -- optional `(CTX)' returning a short badge string or nil
  `:help'      -- optional tooltip string
  `:action-fn' -- optional `(ACTION VALUE)' handler for custom view actions."
  (unless (and (plist-get spec :label) (plist-get spec :render-fn))
    (error "Drawer view %S requires :label and :render-fn" id))
  (setf (alist-get id herdr-sanctum-drawer-views) spec)
  (run-hooks 'herdr-sanctum-drawer-change-hook)
  id)

(defun herdr-sanctum-drawer-active-views ()
  "Return list of `(ID . SPEC)' pairs for currently enabled drawer views."
  (delq nil
        (mapcar (lambda (id)
                  (when-let* ((spec (alist-get id herdr-sanctum-drawer-views)))
                    (cons id spec)))
                herdr-sanctum-drawer-enabled-views)))

(defun herdr-sanctum-drawer-enabled-p ()
  "Return non-nil when the drawer should be shown when opening Sanctum."
  (if (eq herdr-sanctum-drawer--enabled nil)
      herdr-sanctum-drawer-default-open
    (eq herdr-sanctum-drawer--enabled 'on)))

(defun herdr-sanctum-drawer-visible-p (&optional frame)
  "Return non-nil when `*sanctum-drawer*' is visible on FRAME."
  (when-let* ((buf (get-buffer herdr-sanctum-drawer-buffer-name))
              (win (get-buffer-window buf (or frame (selected-frame)))))
    (window-live-p win)))

(defun herdr-sanctum-drawer-current-tab ()
  "Return the active drawer view ID, falling back to the first enabled view."
  (let ((views (herdr-sanctum-drawer-active-views)))
    (if (assq herdr-sanctum-drawer--tab views)
        herdr-sanctum-drawer--tab
      (caar views))))

(defun herdr-sanctum-drawer--plural (count singular &optional plural)
  "Format COUNT with SINGULAR or PLURAL (defaulting to SINGULAR + \"s\")."
  (format "%d %s" count (if (= count 1) singular (or plural (concat singular "s")))))

(defun herdr-sanctum-drawer--dirty-repos (repos)
  "Return subrepo plists in REPOS that have a positive `:dirty' count."
  (seq-filter (lambda (r) (> (or (plist-get r :dirty) 0) 0)) repos))

(defun herdr-sanctum-drawer--make-item (action &optional value id full-row)
  "Build a `herdr-sanctum-drawer-item' plist for ACTION and VALUE.
Optional ID disambiguates controls that share ACTION and VALUE.
When FULL-ROW is non-nil, clicking or activating the line's trailing newline
resolves to this item."
  (append (list :action action :value value)
          (when id (list :id id))
          (when full-row (list :full-row t))))

(defun herdr-sanctum-drawer--item-at (&optional pos)
  "Return item plist for the drawer control at POS (default point).
When POS rests on a line's trailing newline, resolve the line's `:full-row'
item if present without firing inline buttons."
  (let ((p (or pos (point))))
    (when (and (integerp p) (>= p (point-min)) (< p (point-max)))
      (or (get-text-property p 'herdr-sanctum-drawer-item)
          (when (eq (char-after p) ?\n)
            (save-excursion
              (goto-char p)
              (let ((bol (line-beginning-position)))
                (when (< bol p)
                  (let ((item (get-text-property bol 'herdr-sanctum-drawer-item)))
                    (when (plist-get item :full-row)
                      item))))))))))

(defun herdr-sanctum-drawer--find-item (item)
  "Return buffer position of the first actionable span matching ITEM, or nil."
  (when item
    (let ((target-action (plist-get item :action))
          (target-value (plist-get item :value))
          (target-id (plist-get item :id))
          (pos (point-min))
          (end (point-max))
          (found nil))
      (while (and (not found) (< pos end))
        (let ((cur (get-text-property pos 'herdr-sanctum-drawer-item)))
          (if (and cur
                   (eq (plist-get cur :action) target-action)
                   (equal (plist-get cur :value) target-value)
                   (equal (plist-get cur :id) target-id))
              (setq found pos)
            (setq pos (next-single-property-change
                       pos 'herdr-sanctum-drawer-item nil end)))))
      found)))

(defun herdr-sanctum-drawer--button (label action payload &optional help id)
  "Return a clickable inline pill button for LABEL dispatching ACTION/PAYLOAD.
Optional HELP sets `help-echo' and ID disambiguates duplicate controls."
  (propertize (format " %s " label)
              'face 'herdr-sanctum-drawer-button
              'mouse-face 'highlight
              'herdr-sanctum-drawer-item
              (herdr-sanctum-drawer--make-item action payload id)
              'help-echo help))

(defun herdr-sanctum-card-padding-line (&optional win)
  "Return a compact vertical padding line for card surfaces on GUI WIN."
  (if (and (window-live-p win) (display-graphic-p (window-frame win)))
      (propertize "\n" 'face '(:inherit herdr-sanctum-drawer-card :height 0.45))
    ""))

(defun herdr-sanctum-card-row (left right width &optional win no-truncate)
  "Format an elevated drawer card row aligning LEFT and RIGHT in WIDTH/WIN.
Appends `herdr-sanctum-drawer-card' as fallback face to LEFT and RIGHT and
ends with a card-styled newline so the card background extends across WIN.
When NO-TRUNCATE is non-nil, keep LEFT intact."
  (let* ((left-str (herdr-sanctum-apply-fallback-face
                    left 'herdr-sanctum-drawer-card))
         (right-str (herdr-sanctum-apply-fallback-face
                     right 'herdr-sanctum-drawer-card))
         (body
          (if (and (window-live-p win)
                   (display-graphic-p (window-frame win))
                   (fboundp 'string-pixel-width))
              (let* ((buf (current-buffer))
                     (usable-px (herdr-sanctum-usable-body-px win 80))
                     (has-right (not (string-empty-p right-str)))
                     (right-px (if has-right
                                   (herdr-sanctum-string-pixel-width right-str buf)
                                 0))
                     (align-px (max 0 (- usable-px right-px (if has-right 6 0))))
                     (max-left-px (if has-right (- align-px 10) usable-px))
                     (trimmed (if (or no-truncate (string-empty-p left-str))
                                  left-str
                                (herdr-sanctum-truncate-px
                                 left-str max-left-px 'herdr-sanctum-drawer-card))))
                (if (not has-right)
                    trimmed
                  (concat trimmed
                          (propertize " "
                                      'display `(space :align-to (,align-px))
                                      'face 'herdr-sanctum-drawer-card)
                          right-str)))
            (herdr-sanctum-format-row
             left-str right-str width win 'herdr-sanctum-drawer-card no-truncate))))
    (concat body (propertize "\n" 'face 'herdr-sanctum-drawer-card))))

(defun herdr-sanctum-drawer-build-context (&optional width win snapshot)
  "Build rendering/badge context plist for WIDTH, WIN, and SNAPSHOT."
  (let* ((snap (or snapshot (herdr-sanctum-snapshot)))
         (focused-ws-id (alist-get 'focused_workspace_id snap))
         (ws (and focused-ws-id
                  (seq-find (lambda (w) (equal (alist-get 'workspace_id w) focused-ws-id))
                            (alist-get 'workspaces snap))))
         (dir (herdr-sanctum-focused-dir snap))
         (git (and dir (herdr-sanctum-git-info-for-dir dir))))
    (list :width width
          :win win
          :snapshot snap
          :workspace ws
          :dir dir
          :git git)))

;;; Built-in Views: Overview, Changes, Files

(defun herdr-sanctum-drawer--insert-changed-file (entry git dir width win)
  "Insert a clickable changed-file row for porcelain ENTRY using GIT and DIR.
Resolves the target file path via `herdr-sanctum-git-file-path' and formats
the row within WIDTH and WIN."
  (let* ((xy (car entry))
         (rel (cdr entry))
         (full (herdr-sanctum-git-file-path git rel dir))
         (tag (cond
               ((equal xy "??") (propertize "?" 'face 'herdr-sanctum-sidebar-meta))
               ((string-search "D" xy) (propertize "D" 'face 'error))
               ((string-search "A" xy) (propertize "A" 'face 'success))
               (t (propertize "M" 'face 'warning))))
         (left (concat tag "  " (propertize rel 'face 'herdr-sanctum-sidebar-tab)))
         (right (propertize (string-trim xy) 'face 'herdr-sanctum-sidebar-meta))
         (start (point)))
    (insert (herdr-sanctum-format-row left right width win) "\n")
    (add-text-properties
     start (1- (point))
     `(herdr-sanctum-drawer-item ,(herdr-sanctum-drawer--make-item 'open-file full nil t)
       mouse-face highlight
       help-echo ,(format "Open %s" rel)))))

(defun herdr-sanctum-drawer--insert-repo-list (repos width win)
  "Insert a clickable `REPOSITORIES' list for subrepo plists REPOS.
Formats rows within WIDTH and WIN."
  (insert (propertize (format "REPOSITORIES · %d" (length repos))
                      'face 'herdr-sanctum-sidebar-header)
          "\n")
  (dolist (repo repos)
    (let* ((name (or (plist-get repo :name) ""))
           (r-dir (plist-get repo :dir))
           (r-branch (or (plist-get repo :branch) ""))
           (r-dirty (or (plist-get repo :dirty) 0))
           (branch-str (unless (string-empty-p r-branch)
                         (propertize (herdr-sanctum-truncate r-branch 12)
                                     'face 'herdr-sanctum-sidebar-meta)))
           (sync-str (herdr-sanctum-format-git-sync repo))
           (dirty-str (when (> r-dirty 0)
                        (propertize (format "•%d" r-dirty) 'face 'warning)))
           (right (string-join (delq nil (list branch-str sync-str dirty-str)) " "))
           (left (propertize name
                             'face (if (> r-dirty 0)
                                       '(:inherit herdr-sanctum-sidebar-tab :weight bold)
                                     'herdr-sanctum-sidebar-space)))
           (tip (format "%s (%s%s) — Open Git status"
                        name
                        (if (string-empty-p r-branch) "git" r-branch)
                        (if (> r-dirty 0)
                            (format ", %d dirty" r-dirty)
                          ", clean")))
           (start (point)))
      (insert (herdr-sanctum-format-row left right width win) "\n")
      (add-text-properties
       start (1- (point))
       `(herdr-sanctum-drawer-item
         ,(herdr-sanctum-drawer--make-item
           'open-git-status r-dir (cons 'repo-list r-dir) t)
         mouse-face highlight
         help-echo ,tip)))))

(defun herdr-sanctum-drawer--render-overview-summary (ctx)
  "Insert the compact overview statistics card for CTX.
Wraps cleanly onto two compact lines when the single-line summary exceeds
usable width so the clickable changes segment is never clipped."
  (let* ((width (plist-get ctx :width))
         (win (plist-get ctx :win))
         (snap (plist-get ctx :snapshot))
         (ws (plist-get ctx :workspace))
         (git (plist-get ctx :git))
         (ws-id (alist-get 'workspace_id ws))
         (ws-tabs (and ws-id
                       (seq-filter (lambda (tb) (equal (alist-get 'workspace_id tb) ws-id))
                                   (alist-get 'tabs snap))))
         (ws-panes (and ws-id
                        (seq-filter (lambda (p) (equal (alist-get 'workspace_id p) ws-id))
                                    (alist-get 'panes snap))))
         (ws-agents (and ws-id
                         (seq-filter (lambda (a) (equal (alist-get 'workspace_id a) ws-id))
                                     (alist-get 'agents snap))))
         (tab-count (if ws-tabs (length ws-tabs) (or (alist-get 'tab_count ws) 0)))
         (pane-count (if ws-panes
                         (length ws-panes)
                       (cl-loop for tb in ws-tabs
                                sum (or (alist-get 'pane_count tb) 1))))
         (agent-count (length ws-agents))
         (dirty (or (and git (plist-get git :dirty)) 0))
         (sep (propertize " · " 'face 'herdr-sanctum-sidebar-meta))
         (fmt (lambda (n singular &optional active-face)
                (propertize (herdr-sanctum-drawer--plural n singular)
                            'face (if (> n 0)
                                      (or active-face 'herdr-sanctum-drawer-card)
                                    'herdr-sanctum-sidebar-meta))))
         (seg-tabs (funcall fmt tab-count "tab"))
         (seg-panes (funcall fmt pane-count "pane"))
         (seg-agents (funcall fmt agent-count "agent" 'herdr-sanctum-sidebar-git-ahead))
         (seg-changes
          (propertize (herdr-sanctum-drawer--plural dirty "change")
                      'face (if (> dirty 0) 'warning 'herdr-sanctum-sidebar-meta)
                      'herdr-sanctum-drawer-item
                      (herdr-sanctum-drawer--make-item
                       'switch-tab 'changes 'summary-changes)
                      'mouse-face 'highlight
                      'help-echo "Show Changes view"))
         (single (concat " " (string-join (list seg-tabs seg-panes seg-agents seg-changes) sep)))
         (fits-one-line
          (if (and (window-live-p win)
                   (display-graphic-p (window-frame win))
                   (fboundp 'string-pixel-width))
              (let ((styled (herdr-sanctum-apply-fallback-face
                             single 'herdr-sanctum-drawer-card)))
                (<= (herdr-sanctum-string-pixel-width styled (current-buffer))
                    (herdr-sanctum-usable-body-px win 80)))
            (<= (string-width single) width))))
    (insert (herdr-sanctum-card-padding-line win))
    (if fits-one-line
        (insert (herdr-sanctum-card-row single "" width win t))
      (insert (herdr-sanctum-card-row (concat " " seg-tabs sep seg-panes) "" width win)
              (herdr-sanctum-card-row (concat " " seg-agents sep seg-changes) "" width win)))
    (insert (herdr-sanctum-card-padding-line win) "\n")))

(defun herdr-sanctum-drawer--git-card-detail (git dirty)
  "Format the secondary detail line for the Overview Git card from GIT and DIRTY."
  (cond
   ((null git) nil)
   ((> dirty 0)
    (let* ((added (plist-get git :added))
           (removed (plist-get git :removed))
           (staged (or (plist-get git :staged) 0))
           (unstaged (or (plist-get git :unstaged) 0))
           (untracked (or (plist-get git :untracked) 0))
           (diff-parts
            (delq nil
                  (list (when (and (integerp added) (> added 0))
                          (propertize (format "+%d" added) 'face 'success))
                        (when (and (integerp removed) (> removed 0))
                          (propertize (format "-%d" removed) 'face 'error)))))
           (count-parts
            (delq nil
                  (list (when (> staged 0) (format "%d staged" staged))
                        (when (> unstaged 0) (format "%d modified" unstaged))
                        (when (> untracked 0) (format "%d untracked" untracked)))))
           (parts
            (delq nil
                  (list (when diff-parts (string-join diff-parts " "))
                        (when count-parts
                          (propertize (string-join count-parts " · ")
                                      'face 'herdr-sanctum-sidebar-meta))))))
      (when parts
        (concat " "
                (string-join
                 parts
                 (propertize " · " 'face 'herdr-sanctum-sidebar-meta))))))
   (t
    (when-let* ((sync (herdr-sanctum-format-git-sync git nil nil t)))
      (concat " " sync)))))

(defun herdr-sanctum-drawer--render-overview-git-card (ctx)
  "Insert the cohesive Git status card for CTX."
  (let* ((width (plist-get ctx :width))
         (win (plist-get ctx :win))
         (git (plist-get ctx :git))
         (multi-repo (and git (plist-get git :multi-repo)))
         (repos (and multi-repo (plist-get git :repos)))
         (dirty-repos (and repos (herdr-sanctum-drawer--dirty-repos repos)))
         (dirty (or (and git (plist-get git :dirty)) 0))
         (status-action (if (> dirty 0) 'open-diff 'open-git-status))
         (status-value (when multi-repo
                         (let ((cands (if (> dirty 0) dirty-repos repos)))
                           (when (= (length cands) 1)
                             (plist-get (car cands) :dir)))))
         (status-help (if (> dirty 0)
                          (format "Open %s (d)" herdr-sanctum-diff-label)
                        "Open Git status (m)"))
         (status-left
          (concat
           " "
           (cond
            ((null git)
             (propertize "No Git repository" 'face 'herdr-sanctum-sidebar-meta))
            ((> dirty 0)
             (concat (propertize "● " 'face 'warning)
                     (propertize (format "%s uncommitted"
                                         (herdr-sanctum-drawer--plural dirty "file"))
                                 'face 'herdr-sanctum-drawer-card)))
            (multi-repo
             (concat (propertize "✓ " 'face 'success)
                     (propertize (format "%s clean"
                                         (herdr-sanctum-drawer--plural (length repos) "repo"))
                                 'face 'herdr-sanctum-drawer-card)))
            (t
             (concat (propertize "✓ " 'face 'success)
                     (propertize "Working tree clean"
                                 'face 'herdr-sanctum-drawer-card))))))
         (status-btn
          (when git
            (herdr-sanctum-drawer--button
             (if (> dirty 0) herdr-sanctum-diff-label "Git")
             status-action status-value status-help 'git-card))))
    (insert (herdr-sanctum-card-padding-line win))
    (let ((start (point)))
      (insert (herdr-sanctum-card-row status-left (or status-btn "") width win))
      (when git
        (add-text-properties
         start (1- (point))
         `(herdr-sanctum-drawer-item
           ,(herdr-sanctum-drawer--make-item
             status-action status-value 'git-card t)
           help-echo ,status-help))))
    (when-let* ((detail (herdr-sanctum-drawer--git-card-detail git dirty)))
      (insert (herdr-sanctum-card-row detail "" width win)))
    (insert (herdr-sanctum-card-padding-line win) "\n")))

(defun herdr-sanctum-drawer--render-overview-workspace (ctx)
  "Insert the WORKSPACE context and optional multi-repo sections for CTX."
  (let* ((width (plist-get ctx :width))
         (win (plist-get ctx :win))
         (ws (plist-get ctx :workspace))
         (dir (plist-get ctx :dir))
         (git (plist-get ctx :git))
         (multi-repo (and git (plist-get git :multi-repo)))
         (repos (and multi-repo (plist-get git :repos)))
         (branch (and git (plist-get git :branch)))
         (linked-wt (alist-get 'is_linked_worktree (alist-get 'worktree ws))))
    (insert (propertize "WORKSPACE" 'face 'herdr-sanctum-sidebar-header) "\n")
    (let ((short-dir (if dir
                         (directory-file-name (abbreviate-file-name dir))
                       "No directory"))
          (start (point)))
      (insert (herdr-sanctum-format-row
               (propertize short-dir
                           'face (if dir 'herdr-sanctum-sidebar-tab
                                   'herdr-sanctum-sidebar-meta))
               "" width win)
              "\n")
      (add-text-properties
       start (1- (point))
       `(herdr-sanctum-drawer-item
         ,(herdr-sanctum-drawer--make-item 'switch-tab 'files 'ws-dir t)
         mouse-face highlight
         help-echo "Browse files")))
    (when (and (not multi-repo) (or git linked-wt))
      (let* ((has-branch (and (stringp branch) (not (string-empty-p branch))))
             (sync (herdr-sanctum-format-git-sync git))
             (left (cond
                    (has-branch
                     (let ((b (propertize branch 'face 'herdr-sanctum-sidebar-space)))
                       (if sync (concat b " " sync) b)))
                    (sync sync)
                    (linked-wt (propertize "linked worktree" 'face 'herdr-sanctum-sidebar-meta))
                    (t (propertize "git" 'face 'herdr-sanctum-sidebar-meta))))
             (right (if (and linked-wt (or has-branch sync))
                        (propertize "linked worktree" 'face 'herdr-sanctum-sidebar-meta)
                      ""))
             (start (point)))
        (insert (herdr-sanctum-format-row left right width win) "\n")
        (add-text-properties
         start (1- (point))
         `(herdr-sanctum-drawer-item
           ,(herdr-sanctum-drawer--make-item 'open-git-status nil 'ws-branch t)
           mouse-face highlight
           help-echo "Open Git status (m)"))))
    (insert "\n")
    (when (and multi-repo repos)
      (herdr-sanctum-drawer--insert-repo-list repos width win)
      (insert "\n"))))

(defun herdr-sanctum-drawer--render-overview-actions (ctx)
  "Insert the CHANGED FILES preview (when dirty) and quick action buttons for CTX."
  (let* ((width (plist-get ctx :width))
         (win (plist-get ctx :win))
         (dir (plist-get ctx :dir))
         (git (plist-get ctx :git))
         (files (and git (plist-get git :files)))
         (total (and git (or (plist-get git :dirty) (length files))))
         (btn-find (herdr-sanctum-drawer--button
                    "Find file" 'find-file nil "Find file in project (f)" 'btn-find-file))
         (btn-git (herdr-sanctum-drawer--button
                   "Git Status" 'open-git-status nil "Open Git status (m)" 'btn-git-status)))
    (if (and git files)
        (progn
          (insert (propertize (format "CHANGED FILES · %d" total)
                              'face 'herdr-sanctum-sidebar-header)
                  "\n")
          (dolist (entry (seq-take files 5))
            (herdr-sanctum-drawer--insert-changed-file entry git dir width win))
          (when (> total 5)
            (let ((start (point)))
              (insert (propertize (format "View all %d changes →" total)
                                  'face 'herdr-sanctum-sidebar-git-ahead)
                      "\n")
              (add-text-properties
               start (1- (point))
               `(herdr-sanctum-drawer-item
                 ,(herdr-sanctum-drawer--make-item 'switch-tab 'changes 'all-changes t)
                 mouse-face highlight
                 help-echo "Open Changes view (2)"))))
          (insert "\n" btn-find " " btn-git "\n"))
      (insert (propertize "ACTIONS" 'face 'herdr-sanctum-sidebar-header) "\n"
              btn-find
              (if git (concat " " btn-git) "")
              "\n"))))

(defun herdr-sanctum-drawer--render-overview (ctx)
  "Render the `Overview' view inside `*sanctum-drawer*' using CTX."
  (herdr-sanctum-drawer--render-overview-summary ctx)
  (herdr-sanctum-drawer--render-overview-git-card ctx)
  (herdr-sanctum-drawer--render-overview-workspace ctx)
  (herdr-sanctum-drawer--render-overview-actions ctx))

(defun herdr-sanctum-drawer--render-changes-multi-repo (git width win)
  "Render multi-repo Git changes from GIT within WIDTH and WIN."
  (let ((repos (plist-get git :repos))
        (dirty (or (plist-get git :dirty) 0)))
    (if (zerop dirty)
        (progn
          (insert (propertize (format "✓ %s clean"
                                      (herdr-sanctum-drawer--plural (length repos) "repo"))
                              'face 'success)
                  "\n\n")
          (when repos
            (herdr-sanctum-drawer--insert-repo-list repos width win)))
      (let ((first t))
        (dolist (repo (herdr-sanctum-drawer--dirty-repos repos))
          (unless first (insert "\n"))
          (setq first nil)
          (let* ((r-name (or (plist-get repo :name) ""))
                 (r-dir (plist-get repo :dir))
                 (r-root (or (plist-get repo :root) r-dir))
                 (r-branch (or (plist-get repo :branch) ""))
                 (r-dirty (or (plist-get repo :dirty) 0))
                 (lbl (if (string-empty-p r-branch)
                          (format "%s (%d)" r-name r-dirty)
                        (format "%s · %s (%d)" r-name r-branch r-dirty)))
                 (diff-tip (format "Open %s for %s" herdr-sanctum-diff-label r-name))
                 (left (propertize
                        lbl
                        'face 'herdr-sanctum-sidebar-header
                        'mouse-face 'highlight
                        'herdr-sanctum-drawer-item
                        (herdr-sanctum-drawer--make-item
                         'open-diff r-dir (cons 'repo-header r-dir))
                        'help-echo diff-tip))
                 (right (concat
                         (herdr-sanctum-drawer--button
                          herdr-sanctum-diff-label 'open-diff r-dir diff-tip
                          (cons 'repo-diff-btn r-dir))
                         " "
                         (herdr-sanctum-drawer--button
                          "Git" 'open-git-status r-dir
                          (format "Open Git status for %s" r-name)
                          (cons 'repo-git-btn r-dir)))))
            (insert (herdr-sanctum-format-row left right width win) "\n")
            (dolist (entry (plist-get repo :files))
              (herdr-sanctum-drawer--insert-changed-file
               entry repo r-root width win))))))))

(defun herdr-sanctum-drawer--render-changes-single-repo (git dir width win)
  "Render single-repo Git changes from GIT and DIR within WIDTH and WIN."
  (if (zerop (or (plist-get git :dirty) 0))
      (insert (propertize "✓ Working tree clean" 'face 'success) "\n")
    (let* ((files (plist-get git :files))
           (total (or (plist-get git :dirty) (length files))))
      (insert (propertize (format "CHANGED FILES · %d" total)
                          'face 'herdr-sanctum-sidebar-header)
              "\n")
      (dolist (entry files)
        (herdr-sanctum-drawer--insert-changed-file entry git dir width win)))))

(defun herdr-sanctum-drawer--render-changes (ctx)
  "Render the `Changes' Git view inside `*sanctum-drawer*' using CTX."
  (let* ((width (plist-get ctx :width))
         (win (plist-get ctx :win))
         (dir (plist-get ctx :dir))
         (git (plist-get ctx :git))
         (btn-diff (herdr-sanctum-drawer--button
                    herdr-sanctum-diff-label 'open-diff nil
                    (format "Open %s (d)" herdr-sanctum-diff-label) 'btn-diff))
         (btn-git (herdr-sanctum-drawer--button
                   "Git Status" 'open-git-status nil
                   "Open Git status (m)" 'btn-git-status)))
    (insert btn-diff " " btn-git "\n\n")
    (cond
     ((null git)
      (insert (propertize "Focused pane is not inside a Git repository."
                          'face 'herdr-sanctum-sidebar-meta)
              "\n"))
     ((plist-get git :multi-repo)
      (herdr-sanctum-drawer--render-changes-multi-repo git width win))
     (t
      (herdr-sanctum-drawer--render-changes-single-repo git dir width win)))))

(defun herdr-sanctum-drawer--invalidate-files-cache ()
  "Invalidate the cached directory listing in `*sanctum-drawer*'."
  (when-let* ((buf (get-buffer herdr-sanctum-drawer-buffer-name)))
    (with-current-buffer buf
      (setq herdr-sanctum-drawer--files-cache nil))))

(defun herdr-sanctum-drawer--effective-files-dir (&optional dir)
  "Return the normalized active directory targeted by the `files' view."
  (file-name-as-directory
   (expand-file-name
    (or (and herdr-sanctum-drawer--files-dir
             (file-directory-p herdr-sanctum-drawer--files-dir)
             herdr-sanctum-drawer--files-dir)
        dir
        "~/"))))

(defun herdr-sanctum-drawer--string-ci< (a b)
  "Return non-nil when string A sorts before B case-insensitively."
  (let ((cmp (compare-strings a nil nil b nil nil t)))
    (if (eq cmp t)
        (string< a b)
      (< cmp 0))))

(defun herdr-sanctum-drawer--read-dir-entries (browse-dir)
  "Return cached `(DIRS . FILES)' for BROWSE-DIR, reading disk only on miss.
Uses `file-name-all-completions' so directory entries are classified in C
without per-entry `file-directory-p' stat calls.  Excludes `.' and `..',
sorts case-insensitively with directories first, and caps total entries at 150."
  (if (and herdr-sanctum-drawer--files-cache
           (equal (car herdr-sanctum-drawer--files-cache) browse-dir))
      (cdr herdr-sanctum-drawer--files-cache)
    (let ((dir-prefix (file-name-as-directory (expand-file-name browse-dir)))
          (raw (ignore-errors (file-name-all-completions "" browse-dir)))
          (dirs nil)
          (files nil))
      (dolist (name raw)
        (unless (member name '("./" "../" "." ".."))
          (if (string-suffix-p "/" name)
              (push (substring name 0 -1) dirs)
            (push name files))))
      (setq dirs (seq-take (sort dirs #'herdr-sanctum-drawer--string-ci<) 150))
      (let* ((rem (max 0 (- 150 (length dirs))))
             (kept-files
              (when (> rem 0)
                (seq-take (sort files #'herdr-sanctum-drawer--string-ci<) rem)))
             (result (cons (mapcar (lambda (d) (concat dir-prefix d)) dirs)
                           (mapcar (lambda (f) (concat dir-prefix f)) kept-files))))
        (setq herdr-sanctum-drawer--files-cache (cons browse-dir result))
        result))))

(defun herdr-sanctum-drawer--render-files (ctx)
  "Render the `Files' directory browser inside `*sanctum-drawer*' using CTX."
  (let* ((win (plist-get ctx :win))
         (frame (and (window-live-p win) (window-frame win)))
         (browse-dir (herdr-sanctum-drawer--effective-files-dir (plist-get ctx :dir)))
         (parent (file-name-directory (directory-file-name browse-dir)))
         (short (directory-file-name (abbreviate-file-name browse-dir)))
         (btn-up (when (and parent (not (equal parent browse-dir)))
                   (herdr-sanctum-drawer--button
                    ".. Up" 'browse-dir parent "Go to parent directory (^)" 'btn-up)))
         (btn-find (herdr-sanctum-drawer--button
                    "Find file" 'find-file nil "Find file in project (f)" 'btn-find-file))
         (entries (herdr-sanctum-drawer--read-dir-entries browse-dir))
         (dirs (car entries))
         (files (cdr entries))
         (icon-fn (and (functionp herdr-sanctum-file-icon-function)
                       herdr-sanctum-file-icon-function))
         (icon-takes-frame
          (when icon-fn
            (let ((max-args (cdr (ignore-errors (func-arity icon-fn)))))
              (or (eq max-args 'many)
                  (and (integerp max-args) (>= max-args 2)))))))
    (insert (propertize short 'face 'herdr-sanctum-sidebar-header) "\n")
    (insert (if btn-up (concat btn-up " ") "") btn-find "\n\n")
    (dolist (d dirs)
      (let* ((name (file-name-nondirectory d))
             (left (concat (propertize "▸ " 'face 'herdr-sanctum-sidebar-meta)
                           (propertize name 'face 'herdr-sanctum-sidebar-space)))
             (start (point)))
        (insert left "\n")
        (add-text-properties
         start (1- (point))
         `(herdr-sanctum-drawer-item
           ,(herdr-sanctum-drawer--make-item
             'browse-dir (file-name-as-directory d) nil t)
           mouse-face highlight
           help-echo ,(format "Browse %s" name)))))
    (dolist (f files)
      (let* ((name (file-name-nondirectory f))
             (icon (cond
                    ((not icon-fn) "  ")
                    (icon-takes-frame (funcall icon-fn name frame))
                    (t (funcall icon-fn name))))
             (left (concat icon (propertize name 'face 'herdr-sanctum-sidebar-tab)))
             (start (point)))
        (insert left "\n")
        (add-text-properties
         start (1- (point))
         `(herdr-sanctum-drawer-item
           ,(herdr-sanctum-drawer--make-item 'open-file f nil t)
           mouse-face highlight
           help-echo ,(format "Open %s" name)))))))

(herdr-sanctum-register-drawer-view
 'overview
 :label "Overview"
 :render-fn #'herdr-sanctum-drawer--render-overview
 :help "Toggle Sanctum Overview drawer")

(herdr-sanctum-register-drawer-view
 'changes
 :label "Changes"
 :badge-fn (lambda (ctx)
             (let ((dirty (or (and (plist-get ctx :git)
                                   (plist-get (plist-get ctx :git) :dirty))
                              0)))
               (when (> dirty 0) (number-to-string dirty))))
 :render-fn #'herdr-sanctum-drawer--render-changes
 :help "Toggle Git Changes drawer")

(herdr-sanctum-register-drawer-view
 'files
 :label "Files"
 :render-fn #'herdr-sanctum-drawer--render-files
 :help "Toggle project Files drawer")

;;; Chrome (`header-line', `tab-line', `mode-line') & Rendering

(defun herdr-sanctum-drawer--body-width (win)
  "Return effective column width for rendering drawer rows in WIN."
  (max 24 (if (window-live-p win)
              (1- (window-body-width win))
            (1- herdr-sanctum-drawer-width))))

(defun herdr-sanctum-drawer--format-header-line (ctx)
  "Pre-format the pinned top `header-line-format' string for CTX."
  (let* ((win (plist-get ctx :win))
         (ws (plist-get ctx :workspace))
         (git (plist-get ctx :git))
         (ws-label (or (alist-get 'label ws) "Sanctum"))
         (ws-id (alist-get 'workspace_id ws))
         (branch (and git (plist-get git :branch)))
         (badge (if (and (window-live-p win)
                         (not (eq (window-frame win) (selected-frame))))
                    (with-selected-frame (window-frame win)
                      (herdr-sanctum-space-badge ws-label ws-id nil 13))
                  (herdr-sanctum-space-badge ws-label ws-id nil 13)))
         (left (concat badge
                       (if (string-empty-p badge) "" " ")
                       (propertize ws-label
                                   'face '(:inherit herdr-sanctum-chrome-bar :weight semi-bold))
                       (if (and (stringp branch) (not (string-empty-p branch)))
                           (propertize (concat "  " branch) 'face 'herdr-sanctum-chrome-bar)
                         "")))
         (right (herdr-sanctum-make-chrome-button
                 " ✕ "
                 'header-line
                 #'herdr-sanctum-drawer-toggle
                 'herdr-sanctum-chrome-pill
                 "Close drawer (Q)")))
    (herdr-sanctum-format-header-line left right win)))

(defun herdr-sanctum-drawer--format-tab-line (ctx)
  "Pre-format the native `tab-line-format' view switcher for CTX."
  (let* ((win (plist-get ctx :win))
         (gui-p (and (window-live-p win) (display-graphic-p (window-frame win))))
         (has-left-inset (and gui-p
                              (or (> (or (car (window-fringes win)) 0) 0)
                                  (> (or (car (window-margins win)) 0) 0))))
         (cur (herdr-sanctum-drawer-current-tab))
         (sep (propertize "   " 'face 'herdr-sanctum-chrome-bar))
         (tabs
          (mapcar
           (lambda (entry)
             (let* ((key (car entry))
                    (spec (cdr entry))
                    (base-lbl (plist-get spec :label))
                    (badge-fn (plist-get spec :badge-fn))
                    (badge (and (functionp badge-fn) (funcall badge-fn ctx)))
                    (lbl (if (and (stringp badge) (not (string-empty-p badge)))
                             (format "%s (%s)" base-lbl badge)
                           base-lbl))
                    (safe-lbl (herdr-sanctum-escape-mode-line lbl))
                    (active (eq key cur)))
               (herdr-sanctum-make-chrome-button
                safe-lbl
                'tab-line
                (lambda () (herdr-sanctum-drawer--switch-tab key))
                (if active
                    'herdr-sanctum-drawer-tab-active
                  'herdr-sanctum-drawer-tab)
                (or (plist-get spec :help)
                    (format "Show %s view" base-lbl))
                (cons 'drawer-view key))))
           (herdr-sanctum-drawer-active-views))))
    (if (null tabs)
        ""
      (let ((lead (if has-left-inset
                      (propertize " "
                                  'display '(space :align-to (0))
                                  'face 'herdr-sanctum-chrome-bar)
                    (propertize " " 'face 'herdr-sanctum-chrome-bar))))
        (concat lead (string-join tabs sep))))))

(defun herdr-sanctum-drawer--format-status-bar (ctx)
  "Pre-format the bottom `mode-line-format' status strip for CTX."
  (let* ((win (plist-get ctx :win))
         (total-w (or (and (window-live-p win) (window-body-width win))
                      herdr-sanctum-drawer-width))
         (dir (plist-get ctx :dir))
         (max-w (max 8 (- total-w (+ (string-width "Refresh ") 3))))
         (raw (if dir
                  (directory-file-name (abbreviate-file-name dir))
                "Sanctum Drawer"))
         (trimmed
          (if (<= (string-width raw) max-w)
              raw
            (let ((tail (and dir (file-name-nondirectory raw))))
              (if (and tail (not (string-empty-p tail)))
                  (let ((short (concat "…/" tail)))
                    (if (<= (string-width short) max-w)
                        short
                      (herdr-sanctum-truncate short max-w)))
                (herdr-sanctum-truncate raw max-w)))))
         (escaped (herdr-sanctum-escape-mode-line trimmed))
         (left (propertize (concat " " escaped) 'face 'herdr-sanctum-status-bar))
         (right (concat
                 (herdr-sanctum-make-chrome-button
                  "Refresh"
                  'mode-line
                  #'herdr-sanctum-drawer-refresh-action
                  'herdr-sanctum-status-bar
                  "Refresh status (g)")
                 (propertize " " 'face 'herdr-sanctum-status-bar))))
    (list left 'mode-line-format-right-align right)))

(defun herdr-sanctum-drawer--update-chrome (&optional ctx win)
  "Pre-format header-line, tab-line, and status mode-line in the current buffer.
When CTX is nil, build context for WIN (default `herdr-sanctum-panel-window')."
  (let* ((ctx (or ctx
                  (let* ((w (or (and (window-live-p win) win)
                                (herdr-sanctum-panel-window)))
                         (width (herdr-sanctum-drawer--body-width w)))
                    (herdr-sanctum-drawer-build-context width w)))))
    (setq-local header-line-format (herdr-sanctum-drawer--format-header-line ctx)
                tab-line-format (herdr-sanctum-drawer--format-tab-line ctx)
                mode-line-format (herdr-sanctum-drawer--format-status-bar ctx))))

(defun herdr-sanctum-drawer--refresh-chrome ()
  "Update drawer chrome bars without modifying the `*sanctum-drawer*' body."
  (when-let* ((buf (get-buffer herdr-sanctum-drawer-buffer-name)))
    (with-current-buffer buf
      (if-let* ((win (herdr-sanctum-panel-window buf)))
          (progn
            (herdr-sanctum-drawer--update-chrome nil win)
            (force-mode-line-update))
        (herdr-sanctum-panel-mark-dirty buf)))))

(defun herdr-sanctum-drawer--render (&optional win)
  "Render the active view into the current `*sanctum-drawer*' buffer for WIN."
  (let* ((buf (current-buffer))
         (win (or (and (window-live-p win) win)
                  (herdr-sanctum-panel-window buf)))
         (frame (and (window-live-p win) (window-frame win)))
         (width (herdr-sanctum-drawer--body-width win))
         (ctx (herdr-sanctum-drawer-build-context width win))
         (active-id (herdr-sanctum-drawer-current-tab))
         (spec (alist-get active-id herdr-sanctum-drawer-views))
         (render-fn (plist-get spec :render-fn))
         (render-key (cons active-id
                           (if (eq active-id 'files)
                               (herdr-sanctum-drawer--effective-files-dir
                                (plist-get ctx :dir))
                             (plist-get ctx :dir))))
         (same-view-p (equal herdr-sanctum-drawer--render-key render-key))
         (win-state (when same-view-p
                      (herdr-sanctum-panel-save-windows #'herdr-sanctum-drawer--item-at)))
         (render-body
          (lambda ()
            (with-current-buffer buf
              (let ((inhibit-read-only t))
                (herdr-sanctum-panel-record-size win)
                (herdr-sanctum-drawer--update-chrome ctx win)
                (setq herdr-sanctum-drawer--render-key render-key)
                (erase-buffer)
                (if (functionp render-fn)
                    (funcall render-fn ctx)
                  (insert (propertize "No drawer view registered." 'face 'herdr-sanctum-sidebar-meta) "\n")))))))
    (if (and frame (not (eq frame (selected-frame))))
        (with-selected-frame frame
          (funcall render-body))
      (funcall render-body))
    (if win-state
        (herdr-sanctum-panel-restore-windows win-state #'herdr-sanctum-drawer--find-item)
      (goto-char (point-min))
      (dolist (w (get-buffer-window-list buf nil t))
        (when (window-live-p w)
          (set-window-start w (point-min) t)
          (set-window-point w (point-min)))))
    (force-mode-line-update)))

(defun herdr-sanctum-drawer--on-resize (&optional win)
  "Handle `*sanctum-drawer*' window width changes on WIN.
When `files' is already rendered for the same directory, refresh chrome only
to avoid re-rendering up to 150 file icons on every resize step."
  (let* ((active-id (herdr-sanctum-drawer-current-tab))
         (target (when (eq active-id 'files)
                   (herdr-sanctum-drawer--effective-files-dir
                    (herdr-sanctum-focused-dir)))))
    (if (and (eq active-id 'files)
             (equal herdr-sanctum-drawer--render-key (cons 'files target))
             (equal (car herdr-sanctum-drawer--files-cache) target))
        (progn
          (herdr-sanctum-drawer--update-chrome nil win)
          (force-mode-line-update))
      (herdr-sanctum-drawer--render win))))

(defun herdr-sanctum-drawer--on-flush ()
  "Refresh focused Git metadata after a deferred drawer render flushes."
  (when-let* ((dir (herdr-sanctum-focused-dir)))
    (herdr-sanctum-git-fetch-dir dir nil t)))

(defun herdr-sanctum-drawer--cancel-git-timer ()
  "Cancel any pending coalesced Git refresh timer for `*sanctum-drawer*'."
  (when (timerp herdr-sanctum-drawer--git-refresh-timer)
    (cancel-timer herdr-sanctum-drawer--git-refresh-timer)
    (setq herdr-sanctum-drawer--git-refresh-timer nil)))

(defun herdr-sanctum-drawer-refresh (&rest _)
  "Re-render `*sanctum-drawer*' when visible, or defer until shown."
  (herdr-sanctum-drawer--cancel-git-timer)
  (herdr-sanctum-panel-refresh-or-mark-dirty
   herdr-sanctum-drawer-buffer-name
   #'herdr-sanctum-drawer--render))

(defun herdr-sanctum-drawer--on-snapshot (&optional snapshot)
  "Handle Herdr SNAPSHOT updates in `*sanctum-drawer*'.
In the `files' view, update only chrome when the displayed directory is
unchanged instead of re-rendering the directory listing."
  (if (eq (herdr-sanctum-drawer-current-tab) 'files)
      (let* ((buf (get-buffer herdr-sanctum-drawer-buffer-name))
             (dir (herdr-sanctum-focused-dir snapshot))
             (target (herdr-sanctum-drawer--effective-files-dir dir)))
        (if (and buf
                 (or (null dir)
                     (null herdr-sanctum-drawer--last-focused-dir)
                     (equal dir herdr-sanctum-drawer--last-focused-dir))
                 (not (herdr-sanctum-panel-dirty-p buf))
                 (with-current-buffer buf
                   (and (equal herdr-sanctum-drawer--render-key (cons 'files target))
                        (equal (car herdr-sanctum-drawer--files-cache) target))))
            (herdr-sanctum-drawer--refresh-chrome)
          (herdr-sanctum-drawer-refresh)))
    (herdr-sanctum-drawer-refresh)))

(defun herdr-sanctum-drawer--on-git-update (dir _info)
  "Coalesce Git updates for focused DIR into a single `*sanctum-drawer*' refresh.
In the `files' view, update only chrome (`tab-line-format' badges and
header-line branch) without touching the buffer body."
  (when (or (null dir) (equal dir (herdr-sanctum-focused-dir)))
    (when-let* ((buf (get-buffer herdr-sanctum-drawer-buffer-name)))
      (if (not (herdr-sanctum-panel-window buf))
          (herdr-sanctum-panel-mark-dirty buf)
        (unless (timerp herdr-sanctum-drawer--git-refresh-timer)
          (setq herdr-sanctum-drawer--git-refresh-timer
                (run-with-timer
                 0 nil
                 (lambda ()
                   (setq herdr-sanctum-drawer--git-refresh-timer nil)
                   (if (eq (herdr-sanctum-drawer-current-tab) 'files)
                       (herdr-sanctum-drawer--refresh-chrome)
                     (herdr-sanctum-drawer-refresh))))))))))

(defun herdr-sanctum-drawer--on-duration-tick ()
  "Refresh `*sanctum-drawer*' on duration ticks only for custom views."
  (unless (memq (herdr-sanctum-drawer-current-tab) '(overview changes files))
    (herdr-sanctum-drawer-refresh)))

(defun herdr-sanctum-drawer--on-focus-change (dir _pane _ws _tab)
  "Sync `files' view directory when Herdr focus moves to a new DIR."
  (when (and dir (not (equal dir herdr-sanctum-drawer--last-focused-dir)))
    (setq herdr-sanctum-drawer--last-focused-dir dir
          herdr-sanctum-drawer--files-dir dir)
    (herdr-sanctum-drawer--invalidate-files-cache)))

;;; Actions, Commands, Keymap & Mode

(defun herdr-sanctum-drawer--resolve-repo-dir (value &optional dirty-only)
  "Resolve target repository directory from VALUE or focused Git context.
When VALUE is non-nil, return it as a directory name if it exists on disk,
otherwise message and return nil.  In multi-repo workspaces when VALUE is
nil, filter to existing subrepo directories and select the sole candidate
\(preferring dirty repositories when DIRTY-ONLY is non-nil) or prompt with
`completing-read'.  Return nil if no existing repo remains or selection is
cancelled."
  (if value
      (if (and (stringp value) (file-directory-p value))
          (file-name-as-directory value)
        (message "Repository directory no longer exists: %s" value)
        nil)
    (let* ((focused (herdr-sanctum-focused-dir))
           (git (and focused (herdr-sanctum-git-info-for-dir focused))))
      (if (and git (plist-get git :multi-repo))
          (let* ((repos (seq-filter
                         (lambda (r)
                           (let ((d (plist-get r :dir)))
                             (and (stringp d) (file-directory-p d))))
                         (plist-get git :repos)))
                 (cands (if dirty-only
                            (or (herdr-sanctum-drawer--dirty-repos repos) repos)
                          repos)))
            (cond
             ((null cands)
              (message "No existing repository directories found")
              nil)
             ((= (length cands) 1)
              (file-name-as-directory (plist-get (car cands) :dir)))
             (t
              (let* ((choices
                      (mapcar
                       (lambda (r)
                         (let* ((name (or (plist-get r :name) ""))
                                (branch (or (plist-get r :branch) ""))
                                (dirty (or (plist-get r :dirty) 0))
                                (detail (cond
                                         ((and (not (string-empty-p branch)) (> dirty 0))
                                          (format "%s · %d dirty" branch dirty))
                                         ((not (string-empty-p branch))
                                          branch)
                                         ((> dirty 0)
                                          (format "%d dirty" dirty))
                                         (t "")))
                                (label (if (string-empty-p detail)
                                           name
                                         (format "%s (%s)" name detail))))
                           (cons label (file-name-as-directory (plist-get r :dir)))))
                       cands))
                     (choice (condition-case nil
                                 (completing-read "Repository: " choices nil t)
                               (quit nil))))
                (and choice (cdr (assoc choice choices)))))))
        (or (and git (plist-get git :root))
            focused
            default-directory)))))

(defun herdr-sanctum-drawer--switch-tab (view)
  "Switch the active `*sanctum-drawer*' view to VIEW and refresh."
  (setq herdr-sanctum-drawer--tab view)
  (when (eq view 'files)
    (herdr-sanctum-drawer--invalidate-files-cache))
  (herdr-sanctum-drawer-refresh)
  (force-mode-line-update t))

(defun herdr-sanctum-drawer--browse-dir (dir)
  "Switch the `files' view directory to DIR and refresh."
  (when (and (stringp dir) (file-directory-p dir))
    (setq herdr-sanctum-drawer--files-dir (file-name-as-directory dir))
    (herdr-sanctum-drawer--invalidate-files-cache)
    (herdr-sanctum-drawer-refresh)))

(defun herdr-sanctum-drawer-refresh-action ()
  "Refresh directory listing, Git status, and Herdr state from `*sanctum-drawer*'."
  (interactive)
  (herdr-sanctum-drawer--invalidate-files-cache)
  (when-let* ((snap (herdr-sanctum-snapshot)))
    (herdr-sanctum-git-refresh-snapshot snap t))
  (when (herdr-sanctum-any-ui-buffer-live-p)
    (herdr-sanctum-refresh-state))
  (herdr-sanctum-drawer-refresh))

(defun herdr-sanctum-drawer-open-diff (&optional dir)
  "Open a working-tree diff for DIR (or focused repo) from `*sanctum-drawer*'."
  (interactive)
  (when-let* ((target-dir (herdr-sanctum-drawer--resolve-repo-dir dir t)))
    (herdr-sanctum-open-outside herdr-sanctum-diff-function target-dir)))

(defun herdr-sanctum-drawer-open-git-status (&optional dir)
  "Open Git status for DIR (or focused repo) from `*sanctum-drawer*'."
  (interactive)
  (when-let* ((target-dir (herdr-sanctum-drawer--resolve-repo-dir dir nil)))
    (herdr-sanctum-open-outside
     (lambda () (funcall herdr-sanctum-git-status-function target-dir))
     target-dir)))

(defun herdr-sanctum-drawer-find-file ()
  "Find a project file from `*sanctum-drawer*'."
  (interactive)
  (herdr-sanctum-open-outside herdr-sanctum-project-find-file-function))

(defun herdr-sanctum-drawer-up-dir ()
  "Navigate to parent directory in `*sanctum-drawer*' `files' view."
  (interactive)
  (when (eq (herdr-sanctum-drawer-current-tab) 'files)
    (let* ((cur (herdr-sanctum-drawer--effective-files-dir
                 (herdr-sanctum-focused-dir)))
           (parent (and cur (file-name-directory (directory-file-name cur)))))
      (when (and parent (not (equal parent cur)))
        (herdr-sanctum-drawer--browse-dir parent)))))

(defun herdr-sanctum-drawer--dispatch (action value)
  "Execute drawer ACTION with VALUE."
  (pcase action
    ('switch-tab (herdr-sanctum-drawer--switch-tab value))
    ('open-file
     (when (and (stringp value) (file-exists-p value))
       (herdr-sanctum-open-outside
        (lambda () (funcall herdr-sanctum-open-file-function value)))))
    ('browse-dir (herdr-sanctum-drawer--browse-dir value))
    ('find-file (herdr-sanctum-drawer-find-file))
    ('open-diff (herdr-sanctum-drawer-open-diff value))
    ('open-git-status (herdr-sanctum-drawer-open-git-status value))
    ('refresh (herdr-sanctum-drawer-refresh-action))
    (_
     (let* ((spec (alist-get (herdr-sanctum-drawer-current-tab)
                             herdr-sanctum-drawer-views))
            (handler (plist-get spec :action-fn)))
       (when (functionp handler)
         (funcall handler action value))))))

(defun herdr-sanctum-drawer-activate ()
  "Activate the item at point in `*sanctum-drawer*'."
  (interactive)
  (when-let* ((item (herdr-sanctum-drawer--item-at)))
    (herdr-sanctum-drawer--dispatch (plist-get item :action) (plist-get item :value))))

(defun herdr-sanctum-drawer-mouse-click (event)
  "Dispatch mouse-1 EVENT in `*sanctum-drawer*' without shadowing mode keymap."
  (interactive "e")
  (let* ((end (event-end event))
         (win (posn-window end))
         (pos (posn-point end)))
    (when (and (window-live-p win) (integerp pos))
      (when-let* ((item (with-current-buffer (window-buffer win)
                          (herdr-sanctum-drawer--item-at pos))))
        (herdr-sanctum-drawer--dispatch (plist-get item :action)
                                        (plist-get item :value))))))

(defun herdr-sanctum-drawer--step (delta)
  "Move point by DELTA `herdr-sanctum-drawer-item' spans in `*sanctum-drawer*'."
  (let ((prop 'herdr-sanctum-drawer-item)
        (forward-p (> delta 0))
        (moved nil))
    (dotimes (_ (abs delta))
      (let ((target
             (if forward-p
                 (let ((p (point))
                       (end (point-max)))
                   (when (< p end)
                     (when (get-text-property p prop)
                       (setq p (next-single-property-change p prop nil end)))
                     (while (and (< p end) (null (get-text-property p prop)))
                       (setq p (next-single-property-change p prop nil end)))
                     (when (and (< p end) (get-text-property p prop))
                       p)))
               (let ((p (point))
                     (beg (point-min)))
                 (when (and (< p (point-max))
                            (> p beg)
                            (or (and (get-text-property p prop)
                                     (eq (get-text-property (1- p) prop)
                                         (get-text-property p prop)))
                                (and (eq (char-after p) ?\n)
                                     (plist-get (get-text-property (1- p) prop)
                                                :full-row))))
                   (setq p (previous-single-property-change p prop nil beg)))
                 (while (and (> p beg) (null (get-text-property (1- p) prop)))
                   (setq p (previous-single-property-change p prop nil beg)))
                 (when (> p beg)
                   (previous-single-property-change p prop nil beg))))))
        (when target
          (goto-char target)
          (setq moved t))))
    moved))

(defun herdr-sanctum-drawer-next-item ()
  "Move to the next actionable item in `*sanctum-drawer*'."
  (interactive)
  (herdr-sanctum-drawer--step 1))

(defun herdr-sanctum-drawer-prev-item ()
  "Move to the previous actionable item in `*sanctum-drawer*'."
  (interactive)
  (herdr-sanctum-drawer--step -1))

(defun herdr-sanctum-drawer-cycle-tab ()
  "Cycle `*sanctum-drawer*' across enabled views."
  (interactive)
  (let* ((ids (mapcar #'car (herdr-sanctum-drawer-active-views)))
         (cur (herdr-sanctum-drawer-current-tab))
         (pos (cl-position cur ids)))
    (when ids
      (herdr-sanctum-drawer--switch-tab
       (nth (if pos (mod (1+ pos) (length ids)) 0) ids)))))

(defun herdr-sanctum-drawer-select-view-by-digit ()
  "Switch `*sanctum-drawer*' to the Nth enabled view matching the pressed digit."
  (interactive)
  (let* ((ev (event-basic-type last-command-event))
         (idx (and (integerp ev) (<= ?1 ev ?9) (- ev ?1)))
         (entry (and idx (nth idx (herdr-sanctum-drawer-active-views)))))
    (when entry
      (herdr-sanctum-drawer--switch-tab (car entry)))))

(defvar-keymap herdr-sanctum-drawer-mode-map
  :doc "Keymap for `*sanctum-drawer*'."
  :parent special-mode-map
  "<mouse-1>" #'herdr-sanctum-drawer-mouse-click
  "RET" #'herdr-sanctum-drawer-activate
  "j" #'herdr-sanctum-drawer-next-item
  "n" #'herdr-sanctum-drawer-next-item
  "C-n" #'herdr-sanctum-drawer-next-item
  "<down>" #'herdr-sanctum-drawer-next-item
  "k" #'herdr-sanctum-drawer-prev-item
  "p" #'herdr-sanctum-drawer-prev-item
  "C-p" #'herdr-sanctum-drawer-prev-item
  "<up>" #'herdr-sanctum-drawer-prev-item
  "TAB" #'herdr-sanctum-drawer-cycle-tab
  "<tab>" #'herdr-sanctum-drawer-cycle-tab
  "1" #'herdr-sanctum-drawer-select-view-by-digit
  "2" #'herdr-sanctum-drawer-select-view-by-digit
  "3" #'herdr-sanctum-drawer-select-view-by-digit
  "4" #'herdr-sanctum-drawer-select-view-by-digit
  "5" #'herdr-sanctum-drawer-select-view-by-digit
  "6" #'herdr-sanctum-drawer-select-view-by-digit
  "7" #'herdr-sanctum-drawer-select-view-by-digit
  "8" #'herdr-sanctum-drawer-select-view-by-digit
  "9" #'herdr-sanctum-drawer-select-view-by-digit
  "d" #'herdr-sanctum-drawer-open-diff
  "m" #'herdr-sanctum-drawer-open-git-status
  "f" #'herdr-sanctum-drawer-find-file
  "^" #'herdr-sanctum-drawer-up-dir
  "-" #'herdr-sanctum-drawer-up-dir
  "g" #'herdr-sanctum-drawer-refresh-action
  "q" #'herdr-sanctum-focus-terminal
  "<escape>" #'herdr-sanctum-focus-terminal
  "Q" #'herdr-sanctum-drawer-toggle)

(define-derived-mode herdr-sanctum-drawer-mode special-mode "Sanctum-Drawer"
  "Major mode for the toggleable Sanctum context drawer."
  :group 'herdr-sanctum-drawer
  (herdr-sanctum-init-panel-buffer
   #'herdr-sanctum-drawer--render
   #'herdr-sanctum-drawer--on-resize
   #'herdr-sanctum-drawer--on-flush)
  (add-hook 'kill-buffer-hook #'herdr-sanctum-drawer--cancel-git-timer nil t))

;;;###autoload
(defun herdr-sanctum-drawer-show (&optional select)
  "Ensure `*sanctum-drawer*' is shown in its side-window.
With SELECT non-nil, move focus to the drawer window."
  (interactive "P")
  (prog1
      (herdr-sanctum-show-panel
       herdr-sanctum-drawer-buffer-name
       #'herdr-sanctum-drawer-mode
       herdr-sanctum-drawer-side
       herdr-sanctum-drawer-width
       (lambda (win)
         (setq herdr-sanctum-drawer--files-cache nil)
         (herdr-sanctum-drawer--render win))
       select
       1)
    (run-hooks 'herdr-sanctum-drawer-change-hook)))

;;;###autoload
(defun herdr-sanctum-drawer-toggle ()
  "Toggle the `*sanctum-drawer*' context side-window."
  (interactive)
  (let ((shown nil))
    (herdr-sanctum-toggle-panel
     herdr-sanctum-drawer-buffer-name
     (lambda ()
       (prog1 (herdr-sanctum-drawer-show)
         (setq shown t)))
     (lambda (enabled)
       (setq herdr-sanctum-drawer--enabled (if enabled 'on 'off)))
     (lambda ()
       (when-let* ((dir (herdr-sanctum-focused-dir)))
         (herdr-sanctum-git-fetch-dir dir nil t)))
     (lambda ()
       (herdr-sanctum-terminal-resize)
       (unless shown
         (run-hooks 'herdr-sanctum-drawer-change-hook))))))

;;;###autoload
(defun herdr-sanctum-drawer-open-view (view)
  "Open `*sanctum-drawer*' on VIEW, or close it if VIEW is already active."
  (interactive
   (list (intern (completing-read "Drawer view: "
                                  (mapcar (lambda (v) (symbol-name (car v)))
                                          (herdr-sanctum-drawer-active-views))
                                  nil t))))
  (let ((existing (and (get-buffer herdr-sanctum-drawer-buffer-name)
                       (get-buffer-window herdr-sanctum-drawer-buffer-name
                                          (selected-frame)))))
    (cond
     ((and existing (eq (herdr-sanctum-drawer-current-tab) view))
      (herdr-sanctum-drawer-toggle))
     (existing
      (setq herdr-sanctum-drawer--enabled 'on)
      (when-let* ((dir (herdr-sanctum-focused-dir)))
        (herdr-sanctum-git-fetch-dir dir nil t))
      (herdr-sanctum-drawer--switch-tab view)
      (herdr-sanctum-ensure-connected))
     (t
      (setq herdr-sanctum-drawer--tab view)
      (herdr-sanctum-drawer-toggle)))))

(defun herdr-sanctum-drawer--cleanup ()
  "Delete drawer windows, kill `*sanctum-drawer*', and reset browse state."
  (herdr-sanctum-drawer--cancel-git-timer)
  (setq herdr-sanctum-drawer--files-dir nil
        herdr-sanctum-drawer--last-focused-dir nil)
  (herdr-sanctum-cleanup-panel herdr-sanctum-drawer-buffer-name))

(add-hook 'herdr-sanctum-focus-change-hook #'herdr-sanctum-drawer--on-focus-change)
(add-hook 'herdr-sanctum-snapshot-hook #'herdr-sanctum-drawer--on-snapshot)
(add-hook 'herdr-sanctum-git-update-hook #'herdr-sanctum-drawer--on-git-update)
(add-hook 'herdr-sanctum-duration-tick-hook #'herdr-sanctum-drawer--on-duration-tick)
(add-hook 'herdr-sanctum-theme-change-hook #'herdr-sanctum-drawer-refresh)
(add-hook 'herdr-sanctum-disconnect-hook #'herdr-sanctum-drawer--cleanup)

(provide 'herdr-sanctum-drawer)
;;; herdr-sanctum-drawer.el ends here
