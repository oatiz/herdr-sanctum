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
;; switched via buffer-local `header-line-format'.

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
(declare-function nerd-icons-icon-for-dir "nerd-icons" (dir &rest arg-overrides))
(declare-function vc-diff "vc" (&optional historic not-essential fileset))
(declare-function vc-responsible-backend "vc" (file &optional no-error))
(declare-function project-current "project" (&optional maybe-prompt directory))
(declare-function project-find-file "project" (&optional include-all))
(declare-function herdr-sanctum-doctor "herdr-sanctum" ())
(declare-function herdr-sanctum-sidebar-collapse "herdr-sanctum-sidebar" ())
(declare-function hl-line-highlight "hl-line" ())
(declare-function hl-line-unhighlight "hl-line" ())
(defvar herdr-sanctum-sidebar-side)
(defvar herdr-sanctum-sidebar-change-hook)
(defvar herdr-sanctum-drawer-mode-map)

(defgroup herdr-sanctum-drawer nil
  "Extensible context drawer settings for `herdr-sanctum'."
  :group 'herdr-sanctum
  :prefix "herdr-sanctum-")

(defcustom herdr-sanctum-drawer-side 'right
  "Side of the frame where `*sanctum-drawer*' is displayed."
  :type '(choice (const :tag "Right" right)
                 (const :tag "Left" left))
  :group 'herdr-sanctum-drawer)

(defcustom herdr-sanctum-drawer-width 34
  "Column width of the `*sanctum-drawer*' side-window."
  :type 'integer
  :group 'herdr-sanctum-drawer)

(defcustom herdr-sanctum-drawer-default-open nil
  "When non-nil, `herdr-sanctum' opens `*sanctum-drawer*' alongside `*sanctum*'."
  :type 'boolean
  :group 'herdr-sanctum-drawer)

(defun herdr-sanctum-drawer--set-refresh (sym val)
  "Set drawer option SYM to VAL and refresh `*sanctum-drawer*'."
  (set-default sym val)
  (when (featurep 'herdr-sanctum-drawer)
    (herdr-sanctum-drawer-refresh)))

(defun herdr-sanctum-drawer--set-enabled-views (sym val)
  "Set `herdr-sanctum-drawer-enabled-views' SYM to VAL and refresh UI."
  (set-default sym val)
  (when (featurep 'herdr-sanctum-drawer)
    (herdr-sanctum-drawer-refresh)
    (run-hooks 'herdr-sanctum-drawer-change-hook)))

(defun herdr-sanctum-drawer--set-files-option (sym val)
  "Set Files-view option SYM to VAL, clearing cache and refreshing drawer."
  (set-default sym val)
  (when (featurep 'herdr-sanctum-drawer)
    (herdr-sanctum-drawer--invalidate-files-cache)
    (herdr-sanctum-drawer-refresh)))

(defcustom herdr-sanctum-drawer-default-view nil
  "Initial view ID shown in `*sanctum-drawer*' before any view is selected.
When nil, defaults to the first view in `herdr-sanctum-drawer-enabled-views'."
  :type '(choice (const :tag "First enabled view" nil)
                 symbol)
  :set #'herdr-sanctum-drawer--set-refresh
  :group 'herdr-sanctum-drawer)

(defcustom herdr-sanctum-drawer-enabled-views '(overview changes files)
  "Ordered list of view IDs shown in `*sanctum-drawer*' and the top chrome bar."
  :type '(repeat symbol)
  :set #'herdr-sanctum-drawer--set-enabled-views
  :group 'herdr-sanctum-drawer)

(defcustom herdr-sanctum-drawer-overview-max-files 5
  "Maximum number of changed files previewed in the `overview' drawer view."
  :type 'natnum
  :set #'herdr-sanctum-drawer--set-refresh
  :group 'herdr-sanctum-drawer)

(defcustom herdr-sanctum-drawer-overview-actions nil
  "Custom quick-action buttons rendered in the `overview' drawer view.
When nil, render the default `Find file' and (inside Git repos) `Git Status'
buttons.  When non-nil, a list of `(LABEL . FUNCTION)' cons cells where each
FUNCTION is invoked via `herdr-sanctum-open-outside'."
  :type '(choice (const :tag "Default (Find file + Git Status)" nil)
                 (repeat (cons string function)))
  :set #'herdr-sanctum-drawer--set-refresh
  :group 'herdr-sanctum-drawer)

(defcustom herdr-sanctum-drawer-files-show-dotfiles t
  "When non-nil, show dotfile entries in the `*sanctum-drawer*' `files' view.
Entries matching `herdr-sanctum-drawer-files-ignore' remain hidden."
  :type 'boolean
  :set #'herdr-sanctum-drawer--set-files-option
  :group 'herdr-sanctum-drawer)

(defcustom herdr-sanctum-drawer-files-ignore '(".git" ".DS_Store")
  "Entries hidden from the `*sanctum-drawer*' `files' view.
Each element may be:
- an exact filename/directory basename string,
- a cons cell `(regexp . PATTERN)' matched against the entry basename, or
- a 1-argument predicate function called with the entry basename."
  :type '(repeat (choice (string :tag "Exact name")
                         (cons :tag "Regexp" (const regexp) regexp)
                         (function :tag "Predicate")))
  :set #'herdr-sanctum-drawer--set-files-option
  :group 'herdr-sanctum-drawer)

(defcustom herdr-sanctum-open-file-function #'find-file
  "Function `(FILE)' called to open a file from `*sanctum-drawer*'."
  :type 'function
  :group 'herdr-sanctum-drawer)

(defcustom herdr-sanctum-open-directory-function #'dired
  "Function `(DIR)' called to open a directory outside Sanctum."
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

(defcustom herdr-sanctum-file-diff-function #'herdr-sanctum-default-file-diff
  "Function `(FILE)' opening a single-file diff for FILE."
  :type 'function
  :group 'herdr-sanctum-drawer)

(defcustom herdr-sanctum-git-status-function #'herdr-sanctum-default-git-status
  "Function `(DIR)' opening a Git status buffer for DIR."
  :type 'function
  :group 'herdr-sanctum-drawer)

(defcustom herdr-sanctum-project-find-file-function
  #'herdr-sanctum-default-project-find-file
  "Zero-argument function to find a file in the current project."
  :type 'function
  :group 'herdr-sanctum-drawer)

(defcustom herdr-sanctum-file-icon-function #'herdr-sanctum-default-file-icon
  "Function `(FILENAME &optional FRAME)' returning an icon prefix string.
One-argument `(FILENAME)' functions are also supported."
  :type 'function
  :group 'herdr-sanctum-drawer)

(defcustom herdr-sanctum-dir-icon-function #'herdr-sanctum-default-dir-icon
  "Function `(DIRNAME &optional FRAME)' returning an icon prefix string.
One-argument `(DIRNAME)' functions are also supported."
  :type 'function
  :group 'herdr-sanctum-drawer)

(defcustom herdr-sanctum-drawer-change-hook nil
  "Hook run when drawer visibility, enabled views, or registered views change."
  :type 'hook
  :group 'herdr-sanctum-drawer)

(defvar herdr-sanctum-drawer-views nil
  "Alist of `(ID . PLIST)' registered drawer views.
Use `herdr-sanctum-register-drawer-view' to add or replace views.")

(defvar herdr-sanctum-drawer--enabled nil
  "Current session toggle state for the drawer (`nil' inherits default).")

(defvar herdr-sanctum-drawer--tab nil
  "Active view ID in `*sanctum-drawer*', or nil to use the default view.")

(defvar herdr-sanctum-drawer--files-dir nil
  "Current directory browsed by `*sanctum-drawer*' in `files' view.")

(defvar herdr-sanctum-drawer--last-focused-dir nil
  "Most recently focused Herdr pane directory synced into `*sanctum-drawer*'.")

(defvar herdr-sanctum-drawer--git-refresh-timer nil
  "Zero-delay timer coalescing Git metadata updates for `*sanctum-drawer*'.")

(defvar-local herdr-sanctum-drawer--files-cache nil
  "Cached `(KEY (DIRS . FILES) OMITTED)' directory listing for `files' view.")

(defvar-local herdr-sanctum-drawer--render-key nil
  "Cons `(VIEW-ID . KEY)' last rendered into the `*sanctum-drawer*' body.")

(defvar-local herdr-sanctum-drawer--active-keymap nil
  "Raw view `:keymap' currently installed in `*sanctum-drawer*', or nil.")

(defvar-local herdr-sanctum-drawer--focus-overlay nil
  "Overlay highlighting focused inline buttons or card rows in `*sanctum-drawer*'.")

;;; Default External Actions

(defun herdr-sanctum-drawer--existing-parent-dir (path)
  "Return the nearest existing parent directory containing PATH, or nil."
  (let ((dir (and (stringp path) (file-name-directory (expand-file-name path)))))
    (while (and dir (not (file-directory-p dir)))
      (let ((parent (file-name-directory (directory-file-name dir))))
        (setq dir (unless (equal parent dir) parent))))
    (and dir (file-directory-p dir) dir)))

(defun herdr-sanctum-default-diff ()
  "Open a working-tree diff via Magit if installed, else built-in `vc-diff'."
  (if (require 'magit-diff nil t)
      (magit-diff-working-tree)
    (unless (fboundp 'vc-responsible-backend) (require 'vc))
    (let ((backend (vc-responsible-backend default-directory t)))
      (unless backend
        (user-error "No VC repository at %s" default-directory))
      (vc-diff nil t (list backend (list default-directory))))))

(defun herdr-sanctum-default-file-diff (file)
  "Open a single-file VC diff for FILE (including deleted files)."
  (unless (fboundp 'vc-responsible-backend) (require 'vc))
  (let* ((target (expand-file-name file))
         (dir (file-name-directory target))
         (parent (herdr-sanctum-drawer--existing-parent-dir target))
         (backend (or (vc-responsible-backend target t)
                      (and dir (vc-responsible-backend dir t))
                      (and parent (vc-responsible-backend parent t)))))
    (unless backend
      (user-error "No VC repository for %s" file))
    (let ((default-directory (or parent default-directory)))
      (vc-diff nil t (list backend (list target))))))

(defun herdr-sanctum-default-git-status (dir)
  "Open Git status for DIR via Magit if installed, else built-in `vc-dir'."
  (if (require 'magit-status nil t)
      (magit-status-setup-buffer dir)
    (vc-dir dir)))

(defun herdr-sanctum-default-project-find-file ()
  "Find a file in the current project, or fall back to `find-file'."
  (unless (fboundp 'project-current) (require 'project))
  (if (project-current nil default-directory)
      (project-find-file)
    (call-interactively #'find-file)))

(defun herdr-sanctum-default-file-icon (filename &optional frame)
  "Return a `nerd-icons' prefix for FILENAME on GUI FRAME, else spacing."
  (if (and (display-graphic-p frame) (fboundp 'nerd-icons-icon-for-file))
      (concat (nerd-icons-icon-for-file filename) " ")
    (propertize "· " 'face 'herdr-sanctum-sidebar-meta)))

(defun herdr-sanctum-default-dir-icon (dirname &optional frame)
  "Return a `nerd-icons' prefix for DIRNAME on GUI FRAME, else a chevron."
  (if (and (display-graphic-p frame) (fboundp 'nerd-icons-icon-for-dir))
      (concat (nerd-icons-icon-for-dir dirname) " ")
    (propertize "▸ " 'face 'herdr-sanctum-sidebar-meta)))

;;; View Registry & Shared Helpers

(defun herdr-sanctum-register-drawer-view (id &rest spec)
  "Register or replace drawer view ID with plist SPEC.
SPEC keys:
  `:label'         -- display string (required)
  `:render-fn'     -- function `(CTX)' inserting buffer contents (required)
                      CTX is a plist with `:width', `:win', `:snapshot',
                      `:workspace', `:dir', and `:git'; interactive spans set
                      text property `herdr-sanctum-drawer-item' to a plist
                      built with `herdr-sanctum-drawer-make-item'
  `:badge-fn'      -- optional `(CTX)' returning a short badge string or nil
  `:help'          -- optional tooltip string
  `:action-fn'     -- optional `(ACTION VALUE)' or `(ACTION VALUE CTX)'
                      handler for custom view actions
  `:keymap'        -- optional keymap installed via `use-local-map' (with
                      `herdr-sanctum-drawer-mode-map' as parent)
  `:refresh-fn'    -- optional `(CTX)' or zero-arg function called on `g'
  `:render-key-fn' -- optional `(CTX)' returning a scroll-stability key
  `:tick-refresh'  -- when non-nil, refresh view on duration ticks."
  (unless (and (plist-get spec :label) (plist-get spec :render-fn))
    (error "Drawer view %S requires :label and :render-fn" id))
  (setf (alist-get id herdr-sanctum-drawer-views) spec)
  (run-hooks 'herdr-sanctum-drawer-change-hook)
  id)

(defun herdr-sanctum-unregister-drawer-view (id)
  "Unregister drawer view ID from `herdr-sanctum-drawer-views'.
Returns non-nil when ID was registered, nil otherwise.  Clears active tab
state if ID was active, refreshes the drawer, and runs
`herdr-sanctum-drawer-change-hook'."
  (when (assq id herdr-sanctum-drawer-views)
    (setq herdr-sanctum-drawer-views
          (cl-remove id herdr-sanctum-drawer-views :key #'car :test #'eq))
    (when (eq herdr-sanctum-drawer--tab id)
      (setq herdr-sanctum-drawer--tab nil))
    (herdr-sanctum-drawer-refresh)
    (run-hooks 'herdr-sanctum-drawer-change-hook)
    t))

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
  "Return the active drawer view ID.
Falls back to `herdr-sanctum-drawer-default-view' (when enabled) and then
to the first view in `herdr-sanctum-drawer-enabled-views'."
  (let ((views (herdr-sanctum-drawer-active-views)))
    (cond
     ((and herdr-sanctum-drawer--tab (assq herdr-sanctum-drawer--tab views))
      herdr-sanctum-drawer--tab)
     ((and herdr-sanctum-drawer-default-view
           (assq herdr-sanctum-drawer-default-view views))
      herdr-sanctum-drawer-default-view)
     (t (caar views)))))

(defun herdr-sanctum-drawer--plural (count singular &optional plural)
  "Format COUNT with SINGULAR or PLURAL (defaulting to SINGULAR + \"s\")."
  (format "%d %s" count (if (= count 1) singular (or plural (concat singular "s")))))

(defun herdr-sanctum-drawer--dirty-repos (repos)
  "Return subrepo plists in REPOS that have a positive `:dirty' count."
  (seq-filter (lambda (r) (> (or (plist-get r :dirty) 0) 0)) repos))

(defun herdr-sanctum-drawer-make-item (action &optional value id full-row)
  "Build a `herdr-sanctum-drawer-item' plist for ACTION and VALUE.
Optional ID disambiguates controls that share ACTION and VALUE.
When FULL-ROW is non-nil, clicking or activating the line's trailing newline
resolves to this item."
  (append (list :action action :value value)
          (when id (list :id id))
          (when full-row (list :full-row t))))

(defalias 'herdr-sanctum-drawer--make-item #'herdr-sanctum-drawer-make-item)

(defun herdr-sanctum-drawer-item-at (&optional pos)
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

(defalias 'herdr-sanctum-drawer--item-at #'herdr-sanctum-drawer-item-at)

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

(defun herdr-sanctum-drawer-button (label action payload &optional help id)
  "Return a clickable inline pill button for LABEL dispatching ACTION/PAYLOAD.
Optional HELP sets `help-echo' and ID disambiguates duplicate controls."
  (propertize (format " %s " label)
              'face 'herdr-sanctum-drawer-button
              'mouse-face 'highlight
              'herdr-sanctum-drawer-item
              (herdr-sanctum-drawer-make-item action payload id)
              'help-echo help))

(defalias 'herdr-sanctum-drawer--button #'herdr-sanctum-drawer-button)

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
         (deleted-p (and (stringp xy) (string-search "D" xy)))
         (tag (cond
               ((equal xy "??") (propertize "?" 'face 'herdr-sanctum-sidebar-meta))
               (deleted-p (propertize "D" 'face 'error))
               ((string-search "A" xy) (propertize "A" 'face 'success))
               (t (propertize "M" 'face 'warning))))
         (left (concat tag "  " (propertize rel 'face 'herdr-sanctum-sidebar-tab)))
         (right (propertize (string-trim xy) 'face 'herdr-sanctum-sidebar-meta))
         (start (point)))
    (insert (herdr-sanctum-format-row left right width win) "\n")
    (add-text-properties
     start (1- (point))
     `(herdr-sanctum-drawer-item ,(herdr-sanctum-drawer-make-item 'open-file full nil t)
       mouse-face highlight
       help-echo ,(format (if deleted-p "Diff deleted %s" "Open %s") rel)))))

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
           herdr-sanctum-card-row t
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
      (insert (if dir
                  (herdr-sanctum-truncate-path
                   short-dir width win 'herdr-sanctum-sidebar-tab)
                (propertize short-dir 'face 'herdr-sanctum-sidebar-meta))
              "\n")
      (add-text-properties
       start (1- (point))
       `(herdr-sanctum-drawer-item
         ,(herdr-sanctum-drawer--make-item 'switch-tab 'files 'ws-dir t)
         mouse-face highlight
         help-echo ,(if dir (format "Browse files (%s)" short-dir) "Browse files"))))
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

(defun herdr-sanctum-drawer--insert-button-row (buttons width)
  "Insert inline pill BUTTONS, wrapping lines when exceeding WIDTH columns."
  (let ((col 0))
    (dolist (btn buttons)
      (let ((bw (string-width btn)))
        (cond
         ((= col 0)
          (insert btn)
          (setq col bw))
         ((<= (+ col 1 bw) width)
          (insert " " btn)
          (setq col (+ col 1 bw)))
         (t
          (insert "\n" btn)
          (setq col bw)))))
    (insert "\n")))

(defun herdr-sanctum-drawer--render-overview-actions (ctx)
  "Insert the CHANGED FILES preview (when dirty) and quick action buttons for CTX."
  (let* ((width (plist-get ctx :width))
         (win (plist-get ctx :win))
         (dir (plist-get ctx :dir))
         (git (plist-get ctx :git))
         (files (and git (plist-get git :files)))
         (total (and git (or (plist-get git :dirty) (length files))))
         (max-files (max 0 (or herdr-sanctum-drawer-overview-max-files 0)))
         (custom-actions herdr-sanctum-drawer-overview-actions)
         (buttons
          (if custom-actions
              (let ((idx 0)
                    (res nil))
                (dolist (entry custom-actions)
                  (when (and (consp entry)
                             (stringp (car entry))
                             (functionp (cdr entry)))
                    (push (herdr-sanctum-drawer-button
                           (car entry) 'overview-action (cdr entry)
                           (car entry) (cons 'overview-action idx))
                          res)
                    (cl-incf idx)))
                (nreverse res))
            (delq nil
                  (list (herdr-sanctum-drawer-button
                         "Find file" 'find-file nil
                         "Find file in project (f)" 'btn-find-file)
                        (when git
                          (herdr-sanctum-drawer-button
                           "Git Status" 'open-git-status nil
                           "Open Git status (m)" 'btn-git-status)))))))
    (if (and git files (> max-files 0))
        (progn
          (insert (propertize (format "CHANGED FILES · %d" total)
                              'face 'herdr-sanctum-sidebar-header)
                  "\n")
          (let ((shown (seq-take files max-files)))
            (dolist (entry shown)
              (herdr-sanctum-drawer--insert-changed-file entry git dir width win))
            (when (> total (length shown))
              (let ((start (point)))
                (insert (propertize (format "View all %d changes →" total)
                                    'face 'herdr-sanctum-sidebar-git-ahead)
                        "\n")
                (add-text-properties
                 start (1- (point))
                 `(herdr-sanctum-drawer-item
                   ,(herdr-sanctum-drawer-make-item 'switch-tab 'changes 'all-changes t)
                   mouse-face highlight
                   help-echo "Open Changes view (2)")))))
          (when buttons
            (insert "\n")
            (herdr-sanctum-drawer--insert-button-row buttons width)))
      (when buttons
        (insert (propertize "ACTIONS" 'face 'herdr-sanctum-sidebar-header) "\n")
        (herdr-sanctum-drawer--insert-button-row buttons width)))))

(defun herdr-sanctum-drawer--render-overview-disconnected (ctx)
  "Render a helpful disconnected onboarding state in Overview for CTX."
  (let* ((width (plist-get ctx :width))
         (win (plist-get ctx :win))
         (btn-retry (herdr-sanctum-drawer--button
                     "Retry" 'refresh nil "Reconnect and refresh (g)" 'btn-retry))
         (btn-doctor (herdr-sanctum-drawer--button
                      "Doctor" 'open-doctor nil "Inspect Herdr setup (herdr-sanctum-doctor)"
                      'btn-doctor)))
    (insert (herdr-sanctum-card-padding-line win)
            (herdr-sanctum-card-row
             (concat " "
                     (propertize "● " 'face 'warning)
                     (propertize "Not connected to Herdr" 'face 'herdr-sanctum-drawer-card))
             "" width win)
            (herdr-sanctum-card-row
             (concat " "
                     (propertize "Waiting for herdr.sock…" 'face 'herdr-sanctum-sidebar-meta))
             "" width win)
            (herdr-sanctum-card-padding-line win)
            "\n"
            (propertize "ACTIONS" 'face 'herdr-sanctum-sidebar-header) "\n"
            btn-retry " " btn-doctor "\n")))

(defun herdr-sanctum-drawer--render-overview (ctx)
  "Render the `Overview' view inside `*sanctum-drawer*' using CTX."
  (if (null (plist-get ctx :snapshot))
      (herdr-sanctum-drawer--render-overview-disconnected ctx)
    (herdr-sanctum-drawer--render-overview-summary ctx)
    (herdr-sanctum-drawer--render-overview-git-card ctx)
    (herdr-sanctum-drawer--render-overview-workspace ctx)
    (herdr-sanctum-drawer--render-overview-actions ctx)))

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
            (let ((r-files (plist-get repo :files)))
              (dolist (entry r-files)
                (herdr-sanctum-drawer--insert-changed-file
                 entry repo r-root width win))
              (let ((omitted (- r-dirty (length r-files))))
                (when (> omitted 0)
                  (insert (propertize
                           (format "… %s"
                                   (herdr-sanctum-drawer--plural
                                    omitted "more changed file" "more changed files"))
                           'face 'herdr-sanctum-sidebar-meta)
                          "\n"))))))))))

(defun herdr-sanctum-drawer--render-changes-single-repo (git dir width win)
  "Render single-repo Git changes from GIT and DIR within WIDTH and WIN."
  (if (zerop (or (plist-get git :dirty) 0))
      (insert (propertize "✓ Working tree clean" 'face 'success) "\n")
    (let* ((files (plist-get git :files))
           (total (or (plist-get git :dirty) (length files)))
           (hdr (format "CHANGED FILES · %d" total))
           (branch (plist-get git :branch))
           (avail (max 0 (- width (string-width hdr) 2)))
           (right (if (and (stringp branch)
                           (not (string-empty-p branch))
                           (>= avail 6))
                      (propertize (herdr-sanctum-truncate branch (min 12 avail))
                                  'face 'herdr-sanctum-sidebar-meta)
                    "")))
      (insert (herdr-sanctum-format-row
               (propertize hdr 'face 'herdr-sanctum-sidebar-header)
               right width win)
              "\n")
      (dolist (entry files)
        (herdr-sanctum-drawer--insert-changed-file entry git dir width win))
      (let ((omitted (- total (length files))))
        (when (> omitted 0)
          (insert (propertize
                   (format "… %s"
                           (herdr-sanctum-drawer--plural
                            omitted "more changed file" "more changed files"))
                   'face 'herdr-sanctum-sidebar-meta)
                  "\n"))))))

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

(defconst herdr-sanctum-drawer--files-cap 150
  "Maximum combined directory and file entries displayed in `files' view.")

(defun herdr-sanctum-drawer--files-cache-key (browse-dir)
  "Return cache key for `files' view listing of BROWSE-DIR."
  (list browse-dir
        herdr-sanctum-drawer-files-show-dotfiles
        herdr-sanctum-drawer-files-ignore))

(defun herdr-sanctum-drawer--file-ignored-p (base)
  "Return non-nil when entry basename BASE should be hidden in `files' view."
  (or (and (not herdr-sanctum-drawer-files-show-dotfiles)
           (string-prefix-p "." base))
      (seq-some
       (lambda (rule)
         (cond
          ((stringp rule) (equal base rule))
          ((and (consp rule) (eq (car rule) 'regexp) (stringp (cdr rule)))
           (string-match-p (cdr rule) base))
          ((functionp rule) (funcall rule base))))
       herdr-sanctum-drawer-files-ignore)))

(defun herdr-sanctum-drawer--read-dir-entries (browse-dir)
  "Return cached `(DIRS . FILES)' for BROWSE-DIR, reading disk only on miss.
Uses `file-name-all-completions' so directory entries are classified in C
without per-entry `file-directory-p' stat calls.  Excludes `.' and `..',
applies `herdr-sanctum-drawer-files-show-dotfiles' and
`herdr-sanctum-drawer-files-ignore', sorts case-insensitively with
directories first, and caps total entries at `herdr-sanctum-drawer--files-cap'
without letting subdirectories starve regular files."
  (let ((key (herdr-sanctum-drawer--files-cache-key browse-dir)))
    (if (and herdr-sanctum-drawer--files-cache
             (equal (car herdr-sanctum-drawer--files-cache) key))
        (cadr herdr-sanctum-drawer--files-cache)
      (let ((dir-prefix (file-name-as-directory (expand-file-name browse-dir)))
            (raw (ignore-errors (file-name-all-completions "" browse-dir)))
            (dirs nil)
            (files nil))
        (dolist (name raw)
          (unless (member name '("./" "../" "." ".."))
            (let* ((is-dir (string-suffix-p "/" name))
                   (base (if is-dir (substring name 0 -1) name)))
              (unless (herdr-sanctum-drawer--file-ignored-p base)
                (if is-dir
                    (push base dirs)
                  (push base files))))))
        (let* ((cap herdr-sanctum-drawer--files-cap)
               (n-dirs (length dirs))
               (n-files (length files))
               (dir-cap (min n-dirs (max (- cap n-files) (/ cap 2))))
               (file-cap (min n-files (- cap dir-cap)))
               (omitted (- (+ n-dirs n-files) (+ dir-cap file-cap)))
               (kept-dirs (when (> dir-cap 0)
                            (seq-take (sort dirs #'herdr-sanctum-drawer--string-ci<)
                                      dir-cap)))
               (kept-files (when (> file-cap 0)
                             (seq-take (sort files #'herdr-sanctum-drawer--string-ci<)
                                       file-cap)))
               (result (cons (mapcar (lambda (d) (concat dir-prefix d)) kept-dirs)
                             (mapcar (lambda (f) (concat dir-prefix f)) kept-files))))
          (setq herdr-sanctum-drawer--files-cache (list key result omitted))
          result)))))

(defun herdr-sanctum-drawer--render-files (ctx)
  "Render the `Files' directory browser inside `*sanctum-drawer*' using CTX."
  (let* ((width (plist-get ctx :width))
         (win (plist-get ctx :win))
         (frame (and (window-live-p win) (window-frame win)))
         (browse-dir (herdr-sanctum-drawer--effective-files-dir (plist-get ctx :dir)))
         (parent (file-name-directory (directory-file-name browse-dir)))
         (short (directory-file-name (abbreviate-file-name browse-dir)))
         (git (plist-get ctx :git))
         (dirty-names
          (when-let* ((gfiles (and git (plist-get git :files)))
                      (root (or (plist-get git :root) (plist-get ctx :dir)))
                      (norm-root (file-name-as-directory (expand-file-name root)))
                      ((string-prefix-p norm-root browse-dir)))
            (let ((sub (substring browse-dir (length norm-root)))
                  (tbl (make-hash-table :test #'equal)))
              (dolist (entry gfiles)
                (let ((rel (cdr entry)))
                  (when (and (stringp rel) (string-prefix-p sub rel))
                    (let* ((rest (substring rel (length sub)))
                           (slash (string-search "/" rest))
                           (seg (if slash (substring rest 0 slash) rest)))
                      (unless (string-empty-p seg)
                        (puthash seg t tbl))))))
              tbl)))
         (btn-up (when (and parent (not (equal parent browse-dir)))
                   (herdr-sanctum-drawer-button
                    ".. Up" 'browse-dir parent "Go to parent directory (^)" 'btn-up)))
         (btn-find (herdr-sanctum-drawer-button
                    "Find file" 'find-file nil "Find file in project (f)" 'btn-find-file))
         (btn-dired (herdr-sanctum-drawer-button
                     "Dired" 'open-directory browse-dir
                     "Open directory in Dired (D)" 'btn-dired))
         (btns-row (concat (if btn-up (concat btn-up " ") "") btn-find))
         (entries (herdr-sanctum-drawer--read-dir-entries browse-dir))
         (dirs (car entries))
         (files (cdr entries))
         (omitted (or (nth 2 herdr-sanctum-drawer--files-cache) 0))
         (dir-icon-fn (and (functionp herdr-sanctum-dir-icon-function)
                           herdr-sanctum-dir-icon-function))
         (dir-icon-takes-frame
          (when dir-icon-fn
            (let ((max-args (cdr (ignore-errors (func-arity dir-icon-fn)))))
              (or (eq max-args 'many)
                  (and (integerp max-args) (>= max-args 2))))))
         (icon-fn (and (functionp herdr-sanctum-file-icon-function)
                       herdr-sanctum-file-icon-function))
         (icon-takes-frame
          (when icon-fn
            (let ((max-args (cdr (ignore-errors (func-arity icon-fn)))))
              (or (eq max-args 'many)
                  (and (integerp max-args) (>= max-args 2)))))))
    (insert (herdr-sanctum-truncate-path short width win 'herdr-sanctum-sidebar-header) "\n")
    (insert btns-row
            (if (<= (+ (string-width btns-row) 1 (string-width btn-dired)) width)
                (concat " " btn-dired)
              "")
            "\n\n")
    (dolist (d dirs)
      (let* ((name (file-name-nondirectory d))
             (dirty-p (and dirty-names (gethash name dirty-names)))
             (max-w (max 10 (- width (if dirty-p 5 3))))
             (shown (if (> (string-width name) max-w)
                        (herdr-sanctum-truncate name max-w)
                      name))
             (icon (cond
                    ((not dir-icon-fn)
                     (propertize "▸ " 'face 'herdr-sanctum-sidebar-meta))
                    (dir-icon-takes-frame (funcall dir-icon-fn name frame))
                    (t (funcall dir-icon-fn name))))
             (left (concat icon
                           (propertize shown 'face 'herdr-sanctum-sidebar-space)
                           (if dirty-p (propertize " •" 'face 'warning) "")))
             (start (point)))
        (insert left "\n")
        (add-text-properties
         start (1- (point))
         `(herdr-sanctum-drawer-item
           ,(herdr-sanctum-drawer-make-item
             'browse-dir (file-name-as-directory d) nil t)
           mouse-face highlight
           help-echo ,(format "Browse %s" name)))))
    (dolist (f files)
      (let* ((name (file-name-nondirectory f))
             (dirty-p (and dirty-names (gethash name dirty-names)))
             (max-w (max 10 (- width (if dirty-p 5 3))))
             (shown (if (> (string-width name) max-w)
                        (herdr-sanctum-truncate name max-w)
                      name))
             (icon (cond
                    ((not icon-fn) "  ")
                    (icon-takes-frame (funcall icon-fn name frame))
                    (t (funcall icon-fn name))))
             (left (concat icon
                           (propertize shown 'face 'herdr-sanctum-sidebar-tab)
                           (if dirty-p (propertize " •" 'face 'warning) "")))
             (start (point)))
        (insert left "\n")
        (add-text-properties
         start (1- (point))
         `(herdr-sanctum-drawer-item
           ,(herdr-sanctum-drawer-make-item 'open-file f nil t)
           mouse-face highlight
           help-echo ,(format "Open %s" name)))))
    (when (> omitted 0)
      (insert (propertize
               (format "… %s"
                       (herdr-sanctum-drawer--plural omitted "more item" "more items"))
               'face 'herdr-sanctum-sidebar-meta)
              "\n"))))

(herdr-sanctum-register-drawer-view
 'overview
 :label "Overview"
 :render-fn #'herdr-sanctum-drawer--render-overview
 :tick-refresh t
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

;;; Chrome (`header-line', `mode-line') & Rendering

(defun herdr-sanctum-drawer--body-width (win)
  "Return effective column width for rendering drawer rows in WIN."
  (max 24 (if (window-live-p win)
              (1- (window-body-width win))
            (1- herdr-sanctum-drawer-width))))

(defun herdr-sanctum-drawer--format-header-line (ctx)
  "Pre-format the unified single-row top `header-line-format' string for CTX.
Renders clickable view tabs on the left and a close button on the right,
aligned horizontally with `*sanctum-spaces*' and `*sanctum*'."
  (let* ((win (plist-get ctx :win))
         (cur (herdr-sanctum-drawer-current-tab))
         (sep (propertize "  " 'face 'herdr-sanctum-chrome-bar))
         (tabs
          (mapcar
           (lambda (entry)
             (let* ((key (car entry))
                    (spec (cdr entry))
                    (base-lbl (plist-get spec :label))
                    (badge-fn (plist-get spec :badge-fn))
                    (badge (and (functionp badge-fn) (funcall badge-fn ctx)))
                    (lbl (if (and (stringp badge) (not (string-empty-p badge)))
                             (format "%s %s" base-lbl badge)
                           base-lbl))
                    (active (eq key cur)))
               (herdr-sanctum-make-chrome-button
                lbl
                'header-line
                (lambda () (herdr-sanctum-drawer--switch-tab key))
                (if active
                    'herdr-sanctum-drawer-tab-active
                  'herdr-sanctum-drawer-tab)
                (or (plist-get spec :help)
                    (format "Show %s view" base-lbl))
                (cons 'drawer-view key))))
           (herdr-sanctum-drawer-active-views)))
         (left (if tabs (string-join tabs sep) ""))
         (right (herdr-sanctum-make-chrome-button
                 " ✕ "
                 'header-line
                 #'herdr-sanctum-drawer-toggle
                 'herdr-sanctum-chrome-pill
                 "Close drawer (Q)")))
    (herdr-sanctum-format-header-line left right win)))

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
         (left (propertize escaped 'face 'herdr-sanctum-status-bar))
         (right (herdr-sanctum-make-chrome-button
                 "Refresh"
                 'mode-line
                 #'herdr-sanctum-drawer-refresh-action
                 'herdr-sanctum-status-bar
                 "Refresh status (g)")))
    (herdr-sanctum-format-status-bar left right win)))

(defun herdr-sanctum-drawer--update-chrome (&optional ctx win)
  "Pre-format header-line and status mode-line in the current buffer.
When CTX is nil, build context for WIN (default `herdr-sanctum-panel-window')."
  (let* ((ctx (or ctx
                  (let* ((w (or (and (window-live-p win) win)
                                (herdr-sanctum-panel-window)))
                         (width (herdr-sanctum-drawer--body-width w)))
                    (herdr-sanctum-drawer-build-context width w)))))
    (setq-local header-line-format (herdr-sanctum-drawer--format-header-line ctx)
                tab-line-format nil
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

(defun herdr-sanctum-drawer--apply-view-keymap (spec)
  "Install SPEC's `:keymap' with `herdr-sanctum-drawer-mode-map' as parent."
  (let ((vmap (plist-get spec :keymap)))
    (unless (eq vmap herdr-sanctum-drawer--active-keymap)
      (setq herdr-sanctum-drawer--active-keymap vmap)
      (use-local-map
       (if (keymapp vmap)
           (make-composed-keymap vmap herdr-sanctum-drawer-mode-map)
         herdr-sanctum-drawer-mode-map)))))

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
         (render-key-fn (plist-get spec :render-key-fn))
         (render-key (cons active-id
                           (cond
                            ((functionp render-key-fn)
                             (funcall render-key-fn ctx))
                            ((eq active-id 'files)
                             (herdr-sanctum-drawer--effective-files-dir
                              (plist-get ctx :dir)))
                            (t (plist-get ctx :dir)))))
         (same-view-p (equal herdr-sanctum-drawer--render-key render-key))
         (win-state (when same-view-p
                      (herdr-sanctum-panel-save-windows #'herdr-sanctum-drawer-item-at)))
         (render-body
          (lambda ()
            (with-current-buffer buf
              (let ((inhibit-read-only t))
                (herdr-sanctum-drawer--apply-view-keymap spec)
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
    (herdr-sanctum-drawer--update-focus-overlay)
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
             (equal (car herdr-sanctum-drawer--files-cache)
                    (herdr-sanctum-drawer--files-cache-key target)))
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
                        (equal (car herdr-sanctum-drawer--files-cache)
                               (herdr-sanctum-drawer--files-cache-key target)))))
            (herdr-sanctum-drawer--refresh-chrome)
          (herdr-sanctum-drawer-refresh)))
    (herdr-sanctum-drawer-refresh)))

(defun herdr-sanctum-drawer--on-git-update (dir _info)
  "Coalesce Git updates for focused DIR into a single `*sanctum-drawer*' refresh.
In the `files' view, update only chrome (`header-line-format' view badges
and status bar) without touching the buffer body."
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
  "Refresh `*sanctum-drawer*' on duration ticks when the active view opts in."
  (let* ((active-id (herdr-sanctum-drawer-current-tab))
         (spec (alist-get active-id herdr-sanctum-drawer-views)))
    (when (plist-get spec :tick-refresh)
      (herdr-sanctum-drawer-refresh))))

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
  (when-let* ((buf (get-buffer herdr-sanctum-drawer-buffer-name)))
    (with-current-buffer buf
      (herdr-sanctum-drawer--apply-view-keymap
       (alist-get (herdr-sanctum-drawer-current-tab)
                  herdr-sanctum-drawer-views))))
  (herdr-sanctum-drawer-refresh)
  (force-mode-line-update t))

(defun herdr-sanctum-drawer--browse-dir (dir)
  "Switch the `files' view directory to DIR and refresh."
  (when (and (stringp dir) (file-directory-p dir))
    (setq herdr-sanctum-drawer--files-dir (file-name-as-directory dir))
    (herdr-sanctum-drawer--invalidate-files-cache)
    (herdr-sanctum-drawer-refresh)))

(defun herdr-sanctum-drawer-refresh-action ()
  "Refresh directory listing, Git status, active view, and Herdr state."
  (interactive)
  (herdr-sanctum-drawer--invalidate-files-cache)
  (when-let* ((snap (herdr-sanctum-snapshot)))
    (herdr-sanctum-git-refresh-snapshot snap t))
  (when-let* ((spec (alist-get (herdr-sanctum-drawer-current-tab)
                               herdr-sanctum-drawer-views))
              (refresh-fn (plist-get spec :refresh-fn))
              ((functionp refresh-fn)))
    (let ((max-args (cdr (ignore-errors (func-arity refresh-fn)))))
      (if (eql max-args 0)
          (funcall refresh-fn)
        (let* ((buf (get-buffer herdr-sanctum-drawer-buffer-name))
               (win (and buf (herdr-sanctum-panel-window buf)))
               (width (herdr-sanctum-drawer--body-width win)))
          (funcall refresh-fn (herdr-sanctum-drawer-build-context width win))))))
  (when (herdr-sanctum-any-ui-buffer-live-p)
    (herdr-sanctum-refresh-state))
  (herdr-sanctum-drawer-refresh))

(defun herdr-sanctum-drawer--open-repo-diff (&optional dir)
  "Open a whole-repository diff for DIR (or focused repo) outside Sanctum."
  (when-let* ((target-dir (herdr-sanctum-drawer--resolve-repo-dir dir t)))
    (herdr-sanctum-open-outside herdr-sanctum-diff-function target-dir)))

(defun herdr-sanctum-drawer--open-file-diff (file)
  "Open a single-file diff for FILE outside Sanctum."
  (when (stringp file)
    (let ((target-dir (or (herdr-sanctum-drawer--existing-parent-dir file)
                          (herdr-sanctum-focused-dir)
                          default-directory))
          (fn herdr-sanctum-file-diff-function))
      (herdr-sanctum-open-outside
       (lambda () (herdr-sanctum-invoke-command-or-fn fn file))
       target-dir))))

(defun herdr-sanctum-drawer-open-diff (&optional dir)
  "Open a diff for DIR (or focused repo) from `*sanctum-drawer*'.
When DIR is nil in `overview' or `changes' view and point is on a changed
file item, open a per-file diff via `herdr-sanctum-file-diff-function'."
  (interactive)
  (let* ((item (and (null dir)
                    (memq (herdr-sanctum-drawer-current-tab) '(overview changes))
                    (herdr-sanctum-drawer-item-at)))
         (action (plist-get item :action))
         (value (plist-get item :value)))
    (if (and (memq action '(open-file diff-file)) (stringp value))
        (herdr-sanctum-drawer--open-file-diff value)
      (herdr-sanctum-drawer--open-repo-diff dir))))

(defun herdr-sanctum-drawer-open-git-status (&optional dir)
  "Open Git status for DIR (or focused repo) from `*sanctum-drawer*'."
  (interactive)
  (when-let* ((target-dir (herdr-sanctum-drawer--resolve-repo-dir dir nil)))
    (let ((fn herdr-sanctum-git-status-function))
      (herdr-sanctum-open-outside
       (lambda ()
         (if (eql (cdr (ignore-errors (func-arity fn))) 0)
             (herdr-sanctum-invoke-command-or-fn fn)
           (herdr-sanctum-invoke-command-or-fn fn target-dir)))
       target-dir))))

(defun herdr-sanctum-drawer-find-file ()
  "Find a project file from `*sanctum-drawer*'."
  (interactive)
  (let ((dir (when (eq (herdr-sanctum-drawer-current-tab) 'files)
               (herdr-sanctum-drawer--effective-files-dir
                (herdr-sanctum-focused-dir)))))
    (herdr-sanctum-open-outside herdr-sanctum-project-find-file-function dir)))

(defun herdr-sanctum-drawer-open-directory (&optional dir)
  "Open DIR (or selected directory / current browsed directory) outside Sanctum."
  (interactive)
  (let* ((item (and (null dir) (herdr-sanctum-drawer-item-at)))
         (target
          (or (and (stringp dir) (file-directory-p dir) dir)
              (and (eq (plist-get item :action) 'browse-dir)
                   (stringp (plist-get item :value))
                   (file-directory-p (plist-get item :value))
                   (plist-get item :value))
              (herdr-sanctum-drawer--effective-files-dir
               (herdr-sanctum-focused-dir))))
         (norm (and target (file-name-as-directory (expand-file-name target))))
         (fn herdr-sanctum-open-directory-function))
    (when norm
      (herdr-sanctum-open-outside
       (lambda ()
         (if (eql (cdr (ignore-errors (func-arity fn))) 0)
             (herdr-sanctum-invoke-command-or-fn fn)
           (herdr-sanctum-invoke-command-or-fn fn norm)))
       norm))))

(defun herdr-sanctum-drawer-toggle-dotfiles ()
  "Toggle dotfile visibility in the `*sanctum-drawer*' `files' view."
  (interactive)
  (setq herdr-sanctum-drawer-files-show-dotfiles
        (not herdr-sanctum-drawer-files-show-dotfiles))
  (herdr-sanctum-drawer--invalidate-files-cache)
  (herdr-sanctum-drawer-refresh)
  (message "Drawer dotfiles %s"
           (if herdr-sanctum-drawer-files-show-dotfiles "shown" "hidden")))

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
     (when (stringp value)
       (if (file-exists-p value)
           (let ((fn herdr-sanctum-open-file-function))
             (herdr-sanctum-open-outside
              (lambda () (herdr-sanctum-invoke-command-or-fn fn value))))
         (herdr-sanctum-drawer--open-file-diff value))))
    ('diff-file (herdr-sanctum-drawer--open-file-diff value))
    ('browse-dir (herdr-sanctum-drawer--browse-dir value))
    ('open-directory (herdr-sanctum-drawer-open-directory value))
    ('find-file (herdr-sanctum-drawer-find-file))
    ('open-diff (herdr-sanctum-drawer--open-repo-diff value))
    ('open-git-status (herdr-sanctum-drawer-open-git-status value))
    ('overview-action
     (when (functionp value)
       (herdr-sanctum-open-outside value)))
    ('open-doctor (herdr-sanctum-doctor))
    ('refresh (herdr-sanctum-drawer-refresh-action))
    (_
     (let* ((spec (alist-get (herdr-sanctum-drawer-current-tab)
                             herdr-sanctum-drawer-views))
            (handler (plist-get spec :action-fn)))
       (when (functionp handler)
         (let ((max-args (cdr (ignore-errors (func-arity handler)))))
           (if (or (eq max-args 'many)
                   (and (integerp max-args) (>= max-args 3)))
               (let* ((buf (get-buffer herdr-sanctum-drawer-buffer-name))
                      (win (and buf (herdr-sanctum-panel-window buf)))
                      (width (herdr-sanctum-drawer--body-width win)))
                 (funcall handler action value
                          (herdr-sanctum-drawer-build-context width win)))
             (funcall handler action value))))))))

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

(defun herdr-sanctum-drawer-quit ()
  "Leave `*sanctum-drawer*', focusing `*sanctum*' or another non-side window.
Falls back to toggling the drawer off when no other non-side window exists
on the current frame."
  (interactive)
  (let* ((frame (selected-frame))
         (cur (selected-window))
         (term-win (get-buffer-window herdr-sanctum-buffer-name frame))
         (other-win (unless (window-live-p term-win)
                      (cl-find-if
                       (lambda (w)
                         (and (not (eq w cur))
                              (not (window-parameter w 'window-side))))
                       (window-list frame 'no-minibuf cur)))))
    (cond
     ((window-live-p term-win)
      (select-window term-win))
     ((window-live-p other-win)
      (select-window other-win))
     (t
      (herdr-sanctum-drawer-toggle)))))

(defun herdr-sanctum-drawer-help ()
  "Echo a concise summary of `*sanctum-drawer*' keybindings."
  (interactive)
  (message "Drawer: RET act · j/k/h/l move · TAB/1-3 views · d diff · m git · f find · D dired · . dotfiles · ^ up · g refresh · q leave · Q hide"))

(defun herdr-sanctum-drawer--update-focus-overlay (&optional _frame)
  "Highlight focused inline buttons or card rows in `*sanctum-drawer*'."
  (when-let* ((buf (get-buffer herdr-sanctum-drawer-buffer-name)))
    (with-current-buffer buf
      (let* ((sel-win (selected-window))
             (focused (and (window-live-p sel-win)
                           (eq (window-buffer sel-win) buf)))
             (pt (and focused (window-point sel-win)))
             (prop 'herdr-sanctum-drawer-item)
             (item (and pt (< pt (point-max)) (get-text-property pt prop)))
             (card-row (and pt (< pt (point-max))
                            (get-text-property pt 'herdr-sanctum-card-row))))
        (cond
         ((and item (not (plist-get item :full-row)))
          (let ((beg (if (and (> pt (point-min))
                              (eq (get-text-property (1- pt) prop) item))
                         (previous-single-property-change pt prop nil (point-min))
                       pt))
                (end (next-single-property-change pt prop nil (point-max))))
            (if (overlayp herdr-sanctum-drawer--focus-overlay)
                (move-overlay herdr-sanctum-drawer--focus-overlay beg end buf)
              (setq herdr-sanctum-drawer--focus-overlay
                    (make-overlay beg end buf)))
            (overlay-put herdr-sanctum-drawer--focus-overlay 'priority 100)
            (overlay-put herdr-sanctum-drawer--focus-overlay 'face
                         'herdr-sanctum-drawer-button-focus)))
         (card-row
          (save-excursion
            (goto-char pt)
            (let ((beg (line-beginning-position))
                  (end (min (point-max) (1+ (line-end-position)))))
              (if (overlayp herdr-sanctum-drawer--focus-overlay)
                  (move-overlay herdr-sanctum-drawer--focus-overlay beg end buf)
                (setq herdr-sanctum-drawer--focus-overlay
                      (make-overlay beg end buf)))
              (overlay-put herdr-sanctum-drawer--focus-overlay 'priority 100)
              (overlay-put herdr-sanctum-drawer--focus-overlay 'face 'hl-line))))
         (t
          (when (overlayp herdr-sanctum-drawer--focus-overlay)
            (delete-overlay herdr-sanctum-drawer--focus-overlay))))
        (when (bound-and-true-p hl-line-mode)
          (if focused
              (hl-line-highlight)
            (hl-line-unhighlight)))))))

(defvar-keymap herdr-sanctum-drawer-mode-map
  :doc "Keymap for `*sanctum-drawer*'."
  :parent special-mode-map
  "<mouse-1>" #'herdr-sanctum-drawer-mouse-click
  "RET" #'herdr-sanctum-drawer-activate
  "j" #'herdr-sanctum-drawer-next-item
  "n" #'herdr-sanctum-drawer-next-item
  "C-n" #'herdr-sanctum-drawer-next-item
  "<down>" #'herdr-sanctum-drawer-next-item
  "l" #'herdr-sanctum-drawer-next-item
  "<right>" #'herdr-sanctum-drawer-next-item
  "k" #'herdr-sanctum-drawer-prev-item
  "p" #'herdr-sanctum-drawer-prev-item
  "C-p" #'herdr-sanctum-drawer-prev-item
  "<up>" #'herdr-sanctum-drawer-prev-item
  "h" #'herdr-sanctum-drawer-prev-item
  "<left>" #'herdr-sanctum-drawer-prev-item
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
  "D" #'herdr-sanctum-drawer-open-directory
  "." #'herdr-sanctum-drawer-toggle-dotfiles
  "^" #'herdr-sanctum-drawer-up-dir
  "-" #'herdr-sanctum-drawer-up-dir
  "g" #'herdr-sanctum-drawer-refresh-action
  "?" #'herdr-sanctum-drawer-help
  "q" #'herdr-sanctum-drawer-quit
  "<escape>" #'herdr-sanctum-drawer-quit
  "Q" #'herdr-sanctum-drawer-toggle)

(define-derived-mode herdr-sanctum-drawer-mode special-mode "Sanctum-Drawer"
  "Major mode for the toggleable Sanctum context drawer."
  :group 'herdr-sanctum-drawer
  (herdr-sanctum-init-panel-buffer
   #'herdr-sanctum-drawer--render
   #'herdr-sanctum-drawer--on-resize
   #'herdr-sanctum-drawer--on-flush)
  (add-hook 'post-command-hook #'herdr-sanctum-drawer--update-focus-overlay nil t)
  (add-hook 'window-selection-change-functions
            #'herdr-sanctum-drawer--update-focus-overlay nil t)
  (add-hook 'kill-buffer-hook #'herdr-sanctum-drawer--cancel-git-timer nil t))

(herdr-sanctum-configure-evil-mode 'herdr-sanctum-drawer-mode)

(defun herdr-sanctum-drawer-collapse ()
  "Collapse `*sanctum-drawer*' on the current frame and mark it disabled."
  (setq herdr-sanctum-drawer--enabled 'off)
  (when-let* ((buf (get-buffer herdr-sanctum-drawer-buffer-name))
              (win (get-buffer-window buf (selected-frame))))
    (when (and (window-live-p win) (eq (window-deletable-p win) t))
      (delete-window win)))
  (run-hooks 'herdr-sanctum-drawer-change-hook))

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
       (herdr-sanctum-maybe-collapse-opposite-panel
        herdr-sanctum-drawer-side
        herdr-sanctum-drawer-width
        herdr-sanctum-sidebar-buffer-name
        (bound-and-true-p herdr-sanctum-sidebar-side)
        (lambda ()
          (if (fboundp 'herdr-sanctum-sidebar-collapse)
              (herdr-sanctum-sidebar-collapse)
            (when (boundp 'herdr-sanctum-sidebar-change-hook)
              (run-hooks 'herdr-sanctum-sidebar-change-hook)))))
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
(defun herdr-sanctum-drawer-open-view (view &optional no-toggle)
  "Open `*sanctum-drawer*' on VIEW, or close it if VIEW is already active.
When NO-TOGGLE is non-nil, keep `*sanctum-drawer*' open on VIEW instead of
closing it."
  (interactive
   (list (intern (completing-read "Drawer view: "
                                  (mapcar (lambda (v) (symbol-name (car v)))
                                          (herdr-sanctum-drawer-active-views))
                                  nil t))))
  (let ((existing (and (get-buffer herdr-sanctum-drawer-buffer-name)
                       (get-buffer-window herdr-sanctum-drawer-buffer-name
                                          (selected-frame)))))
    (cond
     ((and existing (not no-toggle) (eq (herdr-sanctum-drawer-current-tab) view))
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
