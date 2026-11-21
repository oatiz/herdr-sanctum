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
;; Ships with three built-in views: `overview', `changes', and `files'.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'herdr-sanctum-rpc)
(require 'herdr-sanctum-git)
(require 'herdr-sanctum-theme)
(require 'herdr-sanctum-terminal)

(declare-function magit-diff-working-tree "magit-diff" (&optional rev args files))
(declare-function magit-status-setup-buffer "magit-status" (&optional directory))
(declare-function nerd-icons-icon-for-file "nerd-icons" (file &rest arg-overrides))

(defcustom herdr-sanctum-drawer-side 'right
  "Side of the frame where `*sanctum-drawer*' is displayed."
  :type '(choice (const :tag "Right" right)
                 (const :tag "Left" left))
  :group 'herdr-sanctum)

(defcustom herdr-sanctum-drawer-width 38
  "Column width of the `*sanctum-drawer*' side-window."
  :type 'integer
  :group 'herdr-sanctum)

(defcustom herdr-sanctum-drawer-default-open nil
  "When non-nil, `herdr-sanctum' opens `*sanctum-drawer*' alongside `*sanctum*'."
  :type 'boolean
  :group 'herdr-sanctum)

(defcustom herdr-sanctum-drawer-enabled-views '(overview changes files)
  "Ordered list of view IDs shown in `*sanctum-drawer*' and the top chrome bar."
  :type '(repeat symbol)
  :group 'herdr-sanctum)

(defcustom herdr-sanctum-open-file-function #'find-file
  "Function `(FILE)' called to open a file from `*sanctum-drawer*'."
  :type 'function
  :group 'herdr-sanctum)

(defcustom herdr-sanctum-diff-label "Diff"
  "Button label used for the working-tree diff action in `*sanctum-drawer*'."
  :type 'string
  :group 'herdr-sanctum)

(defcustom herdr-sanctum-diff-function #'herdr-sanctum-default-diff
  "Zero-argument function opening a diff for `default-directory'."
  :type 'function
  :group 'herdr-sanctum)

(defcustom herdr-sanctum-git-status-function #'herdr-sanctum-default-git-status
  "Function `(DIR)' opening a Git status buffer for DIR."
  :type 'function
  :group 'herdr-sanctum)

(defcustom herdr-sanctum-project-find-file-function #'project-find-file
  "Zero-argument function to find a file in the current project."
  :type 'function
  :group 'herdr-sanctum)

(defcustom herdr-sanctum-file-icon-function #'herdr-sanctum-default-file-icon
  "Function `(FILENAME)' returning an icon prefix string (or empty string)."
  :type 'function
  :group 'herdr-sanctum)

(defvar herdr-sanctum-drawer--enabled nil
  "Current session toggle state for the drawer (`nil' inherits default).")

(defvar herdr-sanctum-drawer--tab 'overview
  "Active view ID in `*sanctum-drawer*'.")

(defvar herdr-sanctum-drawer--files-dir nil
  "Current directory browsed by `*sanctum-drawer*' in `files' view.")

(defvar herdr-sanctum-drawer--last-focused-dir nil
  "Most recently focused Herdr pane directory synced into `*sanctum-drawer*'.")

(defvar-local herdr-sanctum-drawer--last-size nil
  "Window `(WIDTH . HEIGHT)' last used to render `*sanctum-drawer*'.")

(defvar herdr-sanctum-drawer-views nil
  "Alist of `(ID . PLIST)' registered drawer views.
Use `herdr-sanctum-register-drawer-view' to add or replace views.")

;;; Default External Actions

(defun herdr-sanctum-default-diff ()
  "Open a working-tree diff via Magit if installed, else built-in `vc-diff'."
  (cond
   ((require 'magit-diff nil t)
    (magit-diff-working-tree))
   (t
    (vc-diff nil t))))

(defun herdr-sanctum-default-git-status (dir)
  "Open Git status for DIR via Magit if installed, else built-in `vc-dir'."
  (if (require 'magit-status nil t)
      (magit-status-setup-buffer dir)
    (vc-dir dir)))

(defun herdr-sanctum-default-file-icon (filename)
  "Return a `nerd-icons' prefix for FILENAME on GUI frames, else spacing."
  (if (and (display-graphic-p) (fboundp 'nerd-icons-icon-for-file))
      (concat (nerd-icons-icon-for-file filename) " ")
    "  "))

;;; View Registry

(defun herdr-sanctum-register-drawer-view (id &rest spec)
  "Register or replace drawer view ID with plist SPEC.
SPEC keys:
  `:label'     -- display string (required)
  `:render-fn' -- function `(CTX)' inserting buffer contents (required)
                  CTX is a plist with `:width', `:win', `:snapshot',
                  `:workspace', `:dir', and `:git'
  `:badge-fn'  -- optional `(CTX)' returning a short badge string or nil
  `:help'      -- optional tooltip string
  `:action-fn' -- optional `(ACTION VALUE)' handler for custom view actions."
  (unless (and (plist-get spec :label) (plist-get spec :render-fn))
    (error "Drawer view %S requires :label and :render-fn" id))
  (setf (alist-get id herdr-sanctum-drawer-views) spec)
  id)

(defun herdr-sanctum-unregister-drawer-view (id)
  "Remove drawer view ID from `herdr-sanctum-drawer-views'."
  (setq herdr-sanctum-drawer-views
        (assq-delete-all id herdr-sanctum-drawer-views)))

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

(defun herdr-sanctum-drawer--item-at (&optional pos)
  "Return action plist for the drawer row at POS (default point)."
  (let ((p (or pos (point))))
    (when-let* ((action (get-text-property p 'herdr-sanctum-drawer-action)))
      (list :action action
            :value (get-text-property p 'herdr-sanctum-drawer-value)))))

(defun herdr-sanctum-drawer--build-context (width win)
  "Build rendering context plist for WIDTH and WIN."
  (let* ((snap (herdr-sanctum-snapshot))
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

(defun herdr-sanctum-drawer--render-header (ctx)
  "Insert the shared drawer title row and registered view tabs for CTX."
  (let* ((width (plist-get ctx :width))
         (win (plist-get ctx :win))
         (ws (plist-get ctx :workspace))
         (git (plist-get ctx :git))
         (ws-label (or (alist-get 'label ws) "Sanctum"))
         (ws-id (alist-get 'workspace_id ws))
         (branch (and git (plist-get git :branch)))
         (badge (herdr-sanctum-space-badge ws-label ws-id))
         (left (concat badge (if (string-empty-p badge) "" " ")
                       (propertize ws-label 'face 'herdr-sanctum-sidebar-brand)
                       (if (and branch (not (string-empty-p branch)))
                           (propertize (concat "  " branch) 'face 'herdr-sanctum-sidebar-meta)
                         "")))
         (close-btn (propertize "×"
                                'face 'herdr-sanctum-sidebar-meta
                                'mouse-face 'highlight
                                'herdr-sanctum-drawer-action 'close-drawer
                                'help-echo "Close drawer (Q)")))
    (insert (herdr-sanctum-format-row left close-btn width win) "\n"))
  (let ((cur (herdr-sanctum-drawer-current-tab)))
    (dolist (entry (herdr-sanctum-drawer-active-views))
      (let* ((key (car entry))
             (spec (cdr entry))
             (base-lbl (plist-get spec :label))
             (badge-fn (plist-get spec :badge-fn))
             (badge (and (functionp badge-fn) (funcall badge-fn ctx)))
             (lbl (if (and (stringp badge) (not (string-empty-p badge)))
                      (format "%s (%s)" base-lbl badge)
                    base-lbl))
             (active (eq key cur)))
        (insert (propertize lbl
                            'face (if active
                                      'herdr-sanctum-drawer-tab-active
                                    'herdr-sanctum-drawer-tab)
                            'mouse-face 'highlight
                            'herdr-sanctum-drawer-action 'switch-tab
                            'herdr-sanctum-drawer-value key
                            'help-echo (or (plist-get spec :help)
                                           (format "Show %s view" base-lbl)))
                "   ")))
    (insert "\n" (herdr-sanctum-divider-line (plist-get ctx :width)
                                             (plist-get ctx :win))
            "\n")))

(defun herdr-sanctum-drawer--render-overview (ctx)
  "Render the `Overview' view inside `*sanctum-drawer*' using CTX."
  (let* ((width (plist-get ctx :width))
         (win (plist-get ctx :win))
         (snap (plist-get ctx :snapshot))
         (ws (plist-get ctx :workspace))
         (git (plist-get ctx :git))
         (ws-id (alist-get 'workspace_id ws))
         (ws-tabs (seq-filter (lambda (tb) (equal (alist-get 'workspace_id tb) ws-id))
                              (alist-get 'tabs snap)))
         (ws-panes (seq-filter (lambda (p) (equal (alist-get 'workspace_id p) ws-id))
                               (alist-get 'panes snap)))
         (ws-agents (seq-filter (lambda (a) (equal (alist-get 'workspace_id a) ws-id))
                                (alist-get 'agents snap)))
         (focused-tab (alist-get 'focused_tab_id snap))
         (focused-pane (alist-get 'focused_pane_id snap))
         (dirty (or (and git (plist-get git :dirty)) 0))
         (staged (or (and git (plist-get git :staged)) 0))
         (unstaged (or (and git (plist-get git :unstaged)) 0))
         (untracked (or (and git (plist-get git :untracked)) 0)))
    ;; Git status card
    (let* ((status-left
            (cond
             ((null git)
              (propertize "No Git repository in focused pane" 'face 'herdr-sanctum-sidebar-meta))
             ((> dirty 0)
              (concat (propertize "● " 'face 'warning)
                      (propertize (format "%d file%s uncommitted" dirty (if (= dirty 1) "" "s"))
                                  'face 'herdr-sanctum-drawer-card)))
             (t
              (concat (propertize "✓ " 'face 'success)
                      (propertize "Working tree clean" 'face 'herdr-sanctum-drawer-card)))))
           (status-btn
            (when git
              (propertize (if (> dirty 0) (format " %s " herdr-sanctum-diff-label) " Git ")
                          'face 'herdr-sanctum-drawer-button
                          'mouse-face 'highlight
                          'herdr-sanctum-drawer-action (if (> dirty 0) 'open-diff 'open-git-status)
                          'help-echo (if (> dirty 0)
                                         (format "Open %s (d)" herdr-sanctum-diff-label)
                                       "Open Git status (m)")))))
      (insert (herdr-sanctum-format-row status-left (or status-btn "") width win) "\n")
      (when (and git (> dirty 0))
        (insert (propertize (format "  %d staged · %d modified · %d untracked"
                                    staged unstaged untracked)
                            'face 'herdr-sanctum-sidebar-meta)
                "\n")))
    (insert "\n")
    ;; 4-tile compact metric strip
    (let* ((tiles (list (cons (number-to-string (length ws-tabs)) "Tabs")
                        (cons (number-to-string (length ws-panes)) "Panes")
                        (cons (number-to-string (length ws-agents)) "Agents")
                        (cons (number-to-string dirty) "Changes")))
           (card-bg (herdr-sanctum-palette-color 'bg-card))
           (meta-face `(:inherit herdr-sanctum-sidebar-meta :background ,card-bg))
           (num-face `(:inherit herdr-sanctum-drawer-card :weight bold))
           (col-w (if (and (display-graphic-p) (window-live-p win))
                      (/ (max 160 (window-body-width win t)) 4)
                    (/ (max 24 width) 4))))
      (cl-loop for idx from 0
               for tile in tiles
               do (progn
                    (when (> idx 0)
                      (insert (if (and (display-graphic-p) (window-live-p win))
                                  (propertize " " 'display `(space :align-to (,(* idx col-w)))
                                              'face 'herdr-sanctum-drawer-card)
                                (propertize " " 'face 'herdr-sanctum-drawer-card))))
                    (insert (propertize (concat "  " (car tile)) 'face num-face))))
      (insert (propertize "\n" 'face 'herdr-sanctum-drawer-card))
      (cl-loop for idx from 0
               for tile in tiles
               do (progn
                    (when (> idx 0)
                      (insert (if (and (display-graphic-p) (window-live-p win))
                                  (propertize " " 'display `(space :align-to (,(* idx col-w)))
                                              'face 'herdr-sanctum-drawer-card)
                                (propertize " " 'face 'herdr-sanctum-drawer-card))))
                    (insert (propertize (concat "  " (cdr tile)) 'face meta-face))))
      (insert (propertize "\n" 'face 'herdr-sanctum-drawer-card) "\n"))
    ;; Agents in space
    (insert (propertize (format "AGENTS IN SPACE · %d" (length ws-agents))
                        'face 'herdr-sanctum-sidebar-header)
            "\n")
    (if (null ws-agents)
        (insert (propertize "  No agents running in this space" 'face 'herdr-sanctum-sidebar-meta) "\n")
      (dolist (agent ws-agents)
        (let* ((pane-id (alist-get 'pane_id agent))
               (status (or (alist-get 'agent_status agent) "idle"))
               (topic (or (alist-get 'topic (alist-get 'tokens agent))
                          (alist-get 'agent agent) "agent"))
               (is-focused (equal pane-id focused-pane))
               (badge (herdr-sanctum-agent-badge (alist-get 'agent agent) status))
               (left (concat (herdr-sanctum-row-accent is-focused) badge " "
                             (propertize topic 'face (if is-focused 'bold 'herdr-sanctum-sidebar-tab))))
               (right (propertize (herdr-sanctum-agent-elapsed-string pane-id)
                                  'face 'herdr-sanctum-sidebar-meta))
               (start (point)))
          (insert (herdr-sanctum-format-row left right width win) "\n")
          (add-text-properties start (1- (point))
                               `(herdr-sanctum-drawer-action focus-pane
                                 herdr-sanctum-drawer-value ,pane-id
                                 mouse-face highlight
                                 help-echo "Focus agent pane in *sanctum*")))))
    (insert "\n")
    ;; Tabs in space
    (insert (propertize (format "TABS IN SPACE · %d" (length ws-tabs))
                        'face 'herdr-sanctum-sidebar-header)
            "\n")
    (dolist (tab ws-tabs)
      (let* ((tab-id (alist-get 'tab_id tab))
             (label (or (alist-get 'label tab) tab-id))
             (panes (or (alist-get 'pane_count tab) 1))
             (is-focused (equal tab-id focused-tab))
             (left (concat (herdr-sanctum-row-accent is-focused)
                           (propertize label 'face (if is-focused 'bold 'herdr-sanctum-sidebar-tab))))
             (right (if (> panes 1)
                        (propertize (format "%dp" panes) 'face 'herdr-sanctum-sidebar-meta)
                      ""))
             (start (point)))
        (insert (herdr-sanctum-format-row left right width win) "\n")
        (add-text-properties start (1- (point))
                             `(herdr-sanctum-drawer-action focus-tab
                               herdr-sanctum-drawer-value ,tab-id
                               mouse-face highlight
                               help-echo "Focus tab in *sanctum*"))))))

(defun herdr-sanctum-drawer--render-changes (ctx)
  "Render the `Changes' Git view inside `*sanctum-drawer*' using CTX."
  (let ((width (plist-get ctx :width))
        (win (plist-get ctx :win))
        (dir (plist-get ctx :dir))
        (git (plist-get ctx :git)))
    (let ((btn-diff (propertize (format " %s " herdr-sanctum-diff-label)
                                'face 'herdr-sanctum-drawer-button
                                'mouse-face 'highlight
                                'herdr-sanctum-drawer-action 'open-diff
                                'help-echo (format "Open %s (d)" herdr-sanctum-diff-label)))
          (btn-git (propertize " Git Status "
                               'face 'herdr-sanctum-drawer-button
                               'mouse-face 'highlight
                               'herdr-sanctum-drawer-action 'open-git-status
                               'help-echo "Open Git status (m)"))
          (btn-refresh (propertize " Refresh "
                                   'face 'herdr-sanctum-sidebar-meta
                                   'mouse-face 'highlight
                                   'herdr-sanctum-drawer-action 'refresh
                                   'help-echo "Refresh Git status (g)")))
      (insert btn-diff " " btn-git " " btn-refresh "\n\n"))
    (cond
     ((null git)
      (insert (propertize "Focused pane is not inside a Git repository."
                          'face 'herdr-sanctum-sidebar-meta)
              "\n"))
     ((zerop (or (plist-get git :dirty) 0))
      (insert (propertize "✓ Working tree clean" 'face 'success) "\n"))
     (t
      (let ((files (plist-get git :files)))
        (insert (propertize (format "CHANGED FILES · %d" (length files))
                            'face 'herdr-sanctum-sidebar-header)
                "\n")
        (dolist (entry files)
          (let* ((xy (car entry))
                 (rel (cdr entry))
                 (full (and dir (expand-file-name rel dir)))
                 (tag (cond
                       ((equal xy "??") (propertize "?" 'face 'herdr-sanctum-sidebar-meta))
                       ((string-search "D" xy) (propertize "D" 'face 'error))
                       ((string-search "A" xy) (propertize "A" 'face 'success))
                       (t (propertize "M" 'face 'warning))))
                 (left (concat " " tag "  " (propertize rel 'face 'herdr-sanctum-sidebar-tab)))
                 (right (propertize (string-trim xy) 'face 'herdr-sanctum-sidebar-meta))
                 (start (point)))
            (insert (herdr-sanctum-format-row left right width win) "\n")
            (add-text-properties start (1- (point))
                                 `(herdr-sanctum-drawer-action open-file
                                   herdr-sanctum-drawer-value ,full
                                   mouse-face highlight
                                   help-echo ,(format "Open %s" rel))))))))))

(defun herdr-sanctum-drawer--render-files (ctx)
  "Render the `Files' directory browser inside `*sanctum-drawer*' using CTX."
  (let* ((width (plist-get ctx :width))
         (win (plist-get ctx :win))
         (dir (plist-get ctx :dir))
         (browse-dir (or (and herdr-sanctum-drawer--files-dir
                              (file-directory-p herdr-sanctum-drawer--files-dir)
                              herdr-sanctum-drawer--files-dir)
                         dir
                         (expand-file-name "~/")))
         (parent (file-name-directory (directory-file-name browse-dir)))
         (short (directory-file-name (abbreviate-file-name browse-dir)))
         (btn-up (when (and parent (not (equal parent browse-dir)))
                   (propertize " .. Up "
                               'face 'herdr-sanctum-drawer-button
                               'mouse-face 'highlight
                               'herdr-sanctum-drawer-action 'browse-dir
                               'herdr-sanctum-drawer-value parent
                               'help-echo "Go to parent directory (^)")))
         (btn-find (propertize " Find file "
                               'face 'herdr-sanctum-drawer-button
                               'mouse-face 'highlight
                               'herdr-sanctum-drawer-action 'find-file
                               'help-echo "Find file in project (f)")))
    (insert (propertize short 'face 'herdr-sanctum-sidebar-header) "\n")
    (insert (if btn-up (concat btn-up " ") "") btn-find "\n\n")
    (let* ((raw (ignore-errors (directory-files browse-dir t "\\`[^.]" t)))
           (dirs (sort (seq-filter #'file-directory-p raw) #'string<))
           (files (sort (seq-remove #'file-directory-p raw) #'string<)))
      (dolist (d dirs)
        (let* ((name (file-name-nondirectory d))
               (left (concat " " (propertize "▸ " 'face 'herdr-sanctum-sidebar-meta)
                             (propertize name 'face 'herdr-sanctum-sidebar-space)))
               (start (point)))
          (insert (herdr-sanctum-format-row left "" width win) "\n")
          (add-text-properties start (1- (point))
                               `(herdr-sanctum-drawer-action browse-dir
                                 herdr-sanctum-drawer-value ,(file-name-as-directory d)
                                 mouse-face highlight
                                 help-echo ,(format "Browse %s" name)))))
      (dolist (f (seq-take files 150))
        (let* ((name (file-name-nondirectory f))
               (icon (if (functionp herdr-sanctum-file-icon-function)
                         (funcall herdr-sanctum-file-icon-function name)
                       "  "))
               (left (concat " " icon (propertize name 'face 'herdr-sanctum-sidebar-tab)))
               (start (point)))
          (insert (herdr-sanctum-format-row left "" width win) "\n")
          (add-text-properties start (1- (point))
                               `(herdr-sanctum-drawer-action open-file
                                 herdr-sanctum-drawer-value ,f
                                 mouse-face highlight
                                 help-echo ,(format "Open %s" name))))))))

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

;;; Status Strip, Rendering & Actions

(defun herdr-sanctum-drawer-status-left ()
  "Format the left segment of `*sanctum-drawer*' bottom status strip."
  (let* ((dir (herdr-sanctum-focused-dir))
         (git (and dir (herdr-sanctum-git-info-for-dir dir)))
         (dirty (or (and git (plist-get git :dirty)) 0)))
    (if git
        (propertize (format " %s · %d uncommitted"
                            (or (plist-get git :branch) "git") dirty)
                    'face 'herdr-sanctum-status-bar)
      (propertize " Sanctum Drawer" 'face 'herdr-sanctum-status-bar))))

(defun herdr-sanctum-drawer-status-right ()
  "Format the right action button of `*sanctum-drawer*' bottom status strip."
  (concat
   (herdr-sanctum-make-chrome-button
    herdr-sanctum-diff-label
    (lambda () (herdr-sanctum-open-outside herdr-sanctum-diff-function))
    'herdr-sanctum-status-bar
    (format "Open %s" herdr-sanctum-diff-label)
    'mode-line)
   " "))

(defconst herdr-sanctum-drawer-status-bar-format
  '((:eval (herdr-sanctum-drawer-status-left))
    mode-line-format-right-align
    (:eval (herdr-sanctum-drawer-status-right)))
  "Top-level `mode-line-format' for `*sanctum-drawer*'.")

(defun herdr-sanctum-drawer--render ()
  "Render the active view into the current `*sanctum-drawer*' buffer."
  (let* ((win (get-buffer-window (current-buffer)))
         (width (max 24 (if (window-live-p win)
                            (1- (window-body-width win))
                          (- herdr-sanctum-drawer-width 3))))
         (ctx (herdr-sanctum-drawer--build-context width win))
         (active-id (herdr-sanctum-drawer-current-tab))
         (spec (alist-get active-id herdr-sanctum-drawer-views))
         (render-fn (plist-get spec :render-fn))
         (pt (point))
         (inhibit-read-only t))
    (setq herdr-sanctum-drawer--last-size
          (if (window-live-p win)
              (cons (window-body-width win) (window-body-height win))
            (cons width 0)))
    (erase-buffer)
    (herdr-sanctum-drawer--render-header ctx)
    (if (functionp render-fn)
        (funcall render-fn ctx)
      (insert (propertize "No drawer view registered." 'face 'herdr-sanctum-sidebar-meta) "\n"))
    (goto-char (min pt (point-max)))
    (beginning-of-line)))

(defun herdr-sanctum-drawer-refresh (&rest _)
  "Re-render `*sanctum-drawer*' when its buffer is live."
  (when-let* ((buf (get-buffer herdr-sanctum-drawer-buffer-name)))
    (with-current-buffer buf
      (herdr-sanctum-drawer--render))))

(defun herdr-sanctum-drawer--on-focus-change (dir _pane _ws _tab)
  "Sync `files' view directory when Herdr focus moves to a new DIR."
  (when (and dir (not (equal dir herdr-sanctum-drawer--last-focused-dir)))
    (setq herdr-sanctum-drawer--last-focused-dir dir
          herdr-sanctum-drawer--files-dir dir)))

(defun herdr-sanctum-drawer--on-window-size-change (frame)
  "Re-render `*sanctum-drawer*' when its window geometry changes on FRAME."
  (when-let* ((buf (get-buffer herdr-sanctum-drawer-buffer-name))
              (win (get-buffer-window buf frame)))
    (let ((sz (cons (window-body-width win) (window-body-height win))))
      (with-current-buffer buf
        (unless (equal sz herdr-sanctum-drawer--last-size)
          (herdr-sanctum-drawer--render))))))

(defun herdr-sanctum-drawer--dispatch (action value)
  "Execute drawer ACTION with VALUE."
  (pcase action
    ('close-drawer (herdr-sanctum-drawer-toggle))
    ('switch-tab
     (setq herdr-sanctum-drawer--tab value)
     (herdr-sanctum-drawer-refresh)
     (force-mode-line-update t))
    ('focus-pane
     (herdr-sanctum-rpc-call "pane.focus" `((pane_id . ,value)))
     (herdr-sanctum-focus-terminal))
    ('focus-tab
     (herdr-sanctum-rpc-call "tab.focus" `((tab_id . ,value)))
     (herdr-sanctum-focus-terminal))
    ('open-file
     (when (and (stringp value) (file-exists-p value))
       (herdr-sanctum-open-outside
        (lambda () (funcall herdr-sanctum-open-file-function value)))))
    ('browse-dir
     (when (and (stringp value) (file-directory-p value))
       (setq herdr-sanctum-drawer--files-dir (file-name-as-directory value))
       (herdr-sanctum-drawer-refresh)))
    ('find-file
     (herdr-sanctum-open-outside herdr-sanctum-project-find-file-function))
    ('open-diff
     (herdr-sanctum-open-outside herdr-sanctum-diff-function))
    ('open-git-status
     (let ((dir (or (herdr-sanctum-focused-dir) default-directory)))
       (herdr-sanctum-open-outside
        (lambda () (funcall herdr-sanctum-git-status-function dir))
        dir)))
    ('refresh
     (when-let* ((snap (herdr-sanctum-snapshot)))
       (herdr-sanctum-git-refresh-snapshot snap t))
     (herdr-sanctum-refresh-state))
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

(defun herdr-sanctum-drawer--step-item (delta)
  "Move point by DELTA actionable rows in `*sanctum-drawer*'."
  (let ((origin (point))
        (moved nil))
    (while (and (not moved) (zerop (forward-line delta)))
      (when (herdr-sanctum-drawer--item-at)
        (setq moved t)))
    (unless moved (goto-char origin))
    (beginning-of-line)))

(defun herdr-sanctum-drawer-next-item ()
  "Move to the next actionable row in `*sanctum-drawer*'."
  (interactive)
  (herdr-sanctum-drawer--step-item 1))

(defun herdr-sanctum-drawer-prev-item ()
  "Move to the previous actionable row in `*sanctum-drawer*'."
  (interactive)
  (herdr-sanctum-drawer--step-item -1))

(defun herdr-sanctum-drawer-cycle-tab ()
  "Cycle `*sanctum-drawer*' across enabled views."
  (interactive)
  (let* ((ids (mapcar #'car (herdr-sanctum-drawer-active-views)))
         (cur (herdr-sanctum-drawer-current-tab))
         (pos (cl-position cur ids)))
    (when ids
      (setq herdr-sanctum-drawer--tab
            (nth (if pos (mod (1+ pos) (length ids)) 0) ids))
      (herdr-sanctum-drawer-refresh)
      (force-mode-line-update t))))

(defun herdr-sanctum-drawer-select-view-by-digit ()
  "Switch `*sanctum-drawer*' to the Nth enabled view matching the pressed digit."
  (interactive)
  (let* ((idx (1- (- (event-basic-type last-command-event) ?0)))
         (entry (and (>= idx 0) (nth idx (herdr-sanctum-drawer-active-views)))))
    (when entry
      (herdr-sanctum-drawer--dispatch 'switch-tab (car entry)))))

(defun herdr-sanctum-drawer-up-dir ()
  "Navigate to parent directory in `*sanctum-drawer*' `files' view."
  (interactive)
  (when (eq (herdr-sanctum-drawer-current-tab) 'files)
    (let* ((cur (or herdr-sanctum-drawer--files-dir
                    (herdr-sanctum-focused-dir)
                    default-directory))
           (parent (and cur (file-name-directory (directory-file-name cur)))))
      (when (and parent (not (equal parent cur)))
        (setq herdr-sanctum-drawer--files-dir parent)
        (herdr-sanctum-drawer-refresh)))))

(defvar-keymap herdr-sanctum-drawer-mode-map
  :doc "Keymap for `*sanctum-drawer*'."
  :parent special-mode-map
  "<mouse-1>" #'herdr-sanctum-drawer-mouse-click
  "RET" #'herdr-sanctum-drawer-activate
  "j" #'herdr-sanctum-drawer-next-item
  "n" #'herdr-sanctum-drawer-next-item
  "<down>" #'herdr-sanctum-drawer-next-item
  "k" #'herdr-sanctum-drawer-prev-item
  "p" #'herdr-sanctum-drawer-prev-item
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
  "d" (lambda () (interactive) (herdr-sanctum-drawer--dispatch 'open-diff nil))
  "m" (lambda () (interactive) (herdr-sanctum-drawer--dispatch 'open-git-status nil))
  "f" (lambda () (interactive) (herdr-sanctum-drawer--dispatch 'find-file nil))
  "^" #'herdr-sanctum-drawer-up-dir
  "-" #'herdr-sanctum-drawer-up-dir
  "g" (lambda () (interactive) (herdr-sanctum-drawer--dispatch 'refresh nil))
  "q" #'herdr-sanctum-focus-terminal
  "<escape>" #'herdr-sanctum-focus-terminal
  "Q" #'herdr-sanctum-drawer-toggle)

(define-derived-mode herdr-sanctum-drawer-mode special-mode "Sanctum-Drawer"
  "Major mode for the toggleable Sanctum context drawer."
  :group 'herdr-sanctum
  (let ((table (make-display-table)))
    (set-display-table-slot table 'truncation ?\s)
    (setq-local buffer-display-table table))
  (setq-local truncate-lines t
              cursor-type nil
              mode-line-format herdr-sanctum-drawer-status-bar-format
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
(defun herdr-sanctum-drawer-show (&optional select)
  "Ensure `*sanctum-drawer*' is shown in its side-window.
With SELECT non-nil, move focus to the drawer window."
  (interactive "P")
  (let* ((buf (get-buffer-create herdr-sanctum-drawer-buffer-name))
         (origin (selected-window))
         (action `((display-buffer-reuse-window display-buffer-in-side-window)
                   (side . ,herdr-sanctum-drawer-side)
                   (slot . 0)
                   (window-width . ,herdr-sanctum-drawer-width)
                   (dedicated . t)
                   (window-parameters . ((no-delete-other-windows . t))))))
    (with-current-buffer buf
      (unless (derived-mode-p 'herdr-sanctum-drawer-mode)
        (herdr-sanctum-drawer-mode))
      (herdr-sanctum-drawer--render))
    (let ((win (display-buffer buf action)))
      (when (window-live-p win)
        (set-window-fringes win 0 0)
        (set-window-margins win 1 1)
        (set-window-dedicated-p win t)
        (with-current-buffer buf
          (herdr-sanctum-drawer--render))
        (if select
            (select-window win)
          (when (window-live-p origin)
            (select-window origin))))
      win)))

;;;###autoload
(defun herdr-sanctum-drawer-toggle ()
  "Toggle the `*sanctum-drawer*' right context side-window."
  (interactive)
  (let ((existing (and (get-buffer herdr-sanctum-drawer-buffer-name)
                       (get-buffer-window herdr-sanctum-drawer-buffer-name (selected-frame)))))
    (if existing
        (progn
          (setq herdr-sanctum-drawer--enabled 'off)
          (delete-window existing))
      (setq herdr-sanctum-drawer--enabled 'on)
      (when-let* ((snap (herdr-sanctum-snapshot)))
        (herdr-sanctum-git-refresh-snapshot snap t))
      (herdr-sanctum-drawer-show)
      (herdr-sanctum-ensure-connected))
    (herdr-sanctum-terminal-resize)
    (force-mode-line-update t)))

;;;###autoload
(defun herdr-sanctum-drawer-open-view (view)
  "Open `*sanctum-drawer*' on VIEW, or close it if VIEW is already active."
  (interactive
   (list (intern (completing-read "Drawer view: "
                                  (mapcar (lambda (v) (symbol-name (car v)))
                                          (herdr-sanctum-drawer-active-views))
                                  nil t))))
  (let ((existing (and (get-buffer herdr-sanctum-drawer-buffer-name)
                       (get-buffer-window herdr-sanctum-drawer-buffer-name (selected-frame)))))
    (if (and existing (eq (herdr-sanctum-drawer-current-tab) view))
        (herdr-sanctum-drawer-toggle)
      (setq herdr-sanctum-drawer--tab view
            herdr-sanctum-drawer--enabled 'on)
      (when-let* ((snap (herdr-sanctum-snapshot)))
        (herdr-sanctum-git-refresh-snapshot snap t))
      (herdr-sanctum-drawer-show)
      (herdr-sanctum-ensure-connected)
      (herdr-sanctum-terminal-resize)
      (force-mode-line-update t))))

(defun herdr-sanctum-drawer--cleanup ()
  "Delete drawer windows and kill `*sanctum-drawer*' on disconnect."
  (when-let* ((buf (get-buffer herdr-sanctum-drawer-buffer-name)))
    (dolist (win (get-buffer-window-list buf nil t))
      (when (and (window-live-p win) (window-parent win))
        (delete-window win)))
    (kill-buffer buf)))

(add-hook 'window-size-change-functions #'herdr-sanctum-drawer--on-window-size-change)
(add-hook 'herdr-sanctum-focus-change-hook #'herdr-sanctum-drawer--on-focus-change)
(add-hook 'herdr-sanctum-snapshot-hook #'herdr-sanctum-drawer-refresh)
(add-hook 'herdr-sanctum-git-update-hook #'herdr-sanctum-drawer-refresh)
(add-hook 'herdr-sanctum-duration-tick-hook #'herdr-sanctum-drawer-refresh)
(add-hook 'herdr-sanctum-theme-change-hook #'herdr-sanctum-drawer-refresh)
(add-hook 'herdr-sanctum-disconnect-hook #'herdr-sanctum-drawer--cleanup)

(provide 'herdr-sanctum-drawer)
;;; herdr-sanctum-drawer.el ends here
