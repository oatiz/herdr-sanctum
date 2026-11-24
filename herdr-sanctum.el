;;; herdr-sanctum.el --- Native Emacs multi-agent workbench UI for Herdr -*- lexical-binding: t; -*-

;; Copyright (C) 2026 oatiz

;; Author: oatiz <iamoatiz@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.1"))
;; URL: https://github.com/oatiz/herdr-sanctum
;; Keywords: tools, terminals, processes

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

;; `herdr-sanctum' turns Emacs into a three-pane native workbench around the
;; Herdr terminal multiplexer (`herdr.sock'):
;;
;;  - Persistent Spaces & Agents sidebar (`*sanctum-spaces*') with inline SVG
;;    monograms, async Git branch/status, on-demand tab expansion, and
;;    compact agents grouped by Blocked / Working / Done / Idle.
;;  - Dedicated center terminal (`*sanctum*') hosted through a pluggable
;;    terminal backend (shipping with `ghostel') and topped with a quiet
;;    breadcrumb + action chrome header-line.
;;  - Toggleable right context drawer (`*sanctum-drawer*') backed by an
;;    extensible view registry (built-in Overview, Changes, and Files views).
;;  - Seamless directional navigation across Herdr panes and Emacs side
;;    windows, fuzzy jumping (`herdr-sanctum-jump'), DWIM split/zoom/close
;;    commands, and a quiet global attention badge.
;;
;; Optional companion modules:
;;  - `herdr-sanctum-ime': per-pane automatic input method switching.
;;  - `herdr-sanctum-tab-routing': declarative `display-buffer-alist' routing
;;    so code, diffs, dev tools, and Org buffers open outside `*sanctum*'.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'tab-bar)
(require 'windmove)
(require 'herdr-sanctum-rpc)
(require 'herdr-sanctum-git)
(require 'herdr-sanctum-theme)
(require 'herdr-sanctum-ui)
(require 'herdr-sanctum-terminal)
(require 'herdr-sanctum-sidebar)
(require 'herdr-sanctum-drawer)

(defcustom herdr-sanctum-display-mode 'tab
  "How `herdr-sanctum' hosts the `*sanctum*' workspace layout.
- `tab': use a dedicated `tab-bar-mode' tab named `herdr-sanctum-tab-name'.
- `current': open directly in the current tab/frame without changing tabs.
- `frame': open in a dedicated frame named `herdr-sanctum-frame-name'.
- A zero-argument function preparing the target frame/tab before switching
  to `*sanctum*'."
  :type '(choice (const :tag "Dedicated tab-bar tab" tab)
                 (const :tag "Current window/tab" current)
                 (const :tag "Dedicated frame" frame)
                 (function :tag "Custom host preparation function"))
  :group 'herdr-sanctum)

(defcustom herdr-sanctum-tab-name "Sanctum"
  "Tab name used when `herdr-sanctum-display-mode' is `tab'."
  :type 'string
  :group 'herdr-sanctum)

(defcustom herdr-sanctum-frame-name "Sanctum"
  "Frame name used when `herdr-sanctum-display-mode' is `frame'."
  :type 'string
  :group 'herdr-sanctum)

(defcustom herdr-sanctum-toggle-back-function #'herdr-sanctum-default-toggle-back
  "Function called when `herdr-sanctum' runs interactively while inside it.
Should switch away and return non-nil when toggle-back was performed, or
return nil to continue focusing `*sanctum*'."
  :type '(choice (const :tag "Disable toggle-back" nil)
                 (function :tag "Toggle-back function"))
  :group 'herdr-sanctum)

(defcustom herdr-sanctum-header-line-enable t
  "When non-nil, render the top breadcrumb and action bar in `*sanctum*'."
  :type 'boolean
  :group 'herdr-sanctum)

(defcustom herdr-sanctum-status-badge-enable t
  "When non-nil, publish a quiet blocked/done agent count in `global-mode-string'.
The badge is automatically hidden while `*sanctum-spaces*' is visible on
the active frame."
  :type 'boolean
  :group 'herdr-sanctum)

(defcustom herdr-sanctum-maximize-window-function #'herdr-sanctum-default-maximize-window
  "Fallback function called by `herdr-sanctum-zoom-or-maximize' outside Herdr
or when Herdr has only a single pane."
  :type 'function
  :group 'herdr-sanctum)

(defcustom herdr-sanctum-new-tab-fallback-function nil
  "Optional zero-argument interactive function called by
`herdr-sanctum-new-tab-dwim' when invoked outside `*sanctum*'."
  :type '(choice (const :tag "None" nil)
                 (function :tag "Fallback command"))
  :group 'herdr-sanctum)

(defvar herdr-sanctum--mode-line-string nil
  "Cached global status badge for Herdr agents needing attention.")

(defvar-local herdr-sanctum--header-line-string nil
  "Pre-formatted top chrome `header-line-format' string for `*sanctum*'.")

(defvar-local herdr-sanctum--header-line-dirty nil
  "Non-nil when `*sanctum*' header-line needs refresh on next display.")

;;; Global Attention Badge

(defun herdr-sanctum-badge-click ()
  "Open Sanctum when away from it, or run `herdr-sanctum-jump' when already there."
  (interactive)
  (if (member (buffer-name (window-buffer)) (herdr-sanctum-ui-buffers))
      (herdr-sanctum-jump)
    (herdr-sanctum '(4))))

(defvar herdr-sanctum--badge-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line mouse-1] #'herdr-sanctum-badge-click)
    (define-key map [tab-bar mouse-1] #'herdr-sanctum-badge-click)
    map)
  "Click keymap for the Sanctum status badge in both tab-bar and mode-line.")

(defun herdr-sanctum--format-status-badge (blocked done)
  "Format the quiet global badge for BLOCKED and DONE agent counts."
  (when (or (> blocked 0) (> done 0))
    (let* ((parts (delq nil
                        (list (when (> blocked 0)
                                (propertize (format "◆%d" blocked) 'face 'warning))
                              (when (> done 0)
                                (propertize (format "✓%d" done) 'face 'success)))))
           (text (string-join parts " "))
           (tip (format "Herdr agents: %d blocked, %d done (click to open)" blocked done)))
      (concat "  "
              (propertize text
                          'local-map herdr-sanctum--badge-map
                          'mouse-face 'mode-line-highlight
                          'help-echo tip)
              " "))))

(defun herdr-sanctum-mode-line ()
  "Return the global agent attention badge when appropriate."
  (when (and herdr-sanctum-status-badge-enable
             herdr-sanctum--mode-line-string)
    (unless (herdr-sanctum-sidebar-visible-p (window-frame))
      herdr-sanctum--mode-line-string)))

(defun herdr-sanctum--update-badge-from-snapshot (snapshot)
  "Recompute global attention badge string from SNAPSHOT."
  (let ((blocked 0)
        (done 0))
    (dolist (agent (alist-get 'agents snapshot))
      (pcase (alist-get 'agent_status agent)
        ("blocked" (cl-incf blocked))
        ("done" (cl-incf done))))
    (setq herdr-sanctum--mode-line-string
          (herdr-sanctum--format-status-badge blocked done))))

(defun herdr-sanctum--clear-badge ()
  "Clear cached global status badge on disconnect."
  (setq herdr-sanctum--mode-line-string nil))

(add-to-list 'global-mode-string '(:eval (herdr-sanctum-mode-line)) t)
(add-hook 'herdr-sanctum-snapshot-hook #'herdr-sanctum--update-badge-from-snapshot)
(add-hook 'herdr-sanctum-disconnect-hook #'herdr-sanctum--clear-badge)

;;; Top Header Line (*sanctum*)

(defun herdr-sanctum-header-line-left (&optional snap ws dir)
  "Format the left breadcrumb segment of `*sanctum*' top chrome bar.
Optional SNAP, WS, and DIR supply pre-resolved snapshot values."
  (let* ((snap (or snap (herdr-sanctum-snapshot)))
         (focused-ws-id (alist-get 'focused_workspace_id snap))
         (focused-tab-id (alist-get 'focused_tab_id snap))
         (ws (or ws
                 (and focused-ws-id
                      (seq-find (lambda (w) (equal (alist-get 'workspace_id w) focused-ws-id))
                                (alist-get 'workspaces snap)))))
         (tab (and focused-tab-id
                   (seq-find (lambda (tb) (equal (alist-get 'tab_id tb) focused-tab-id))
                             (alist-get 'tabs snap))))
         (ws-label (or (alist-get 'label ws) "Sanctum"))
         (tab-label (alist-get 'label tab))
         (dir (or dir (herdr-sanctum-focused-dir snap)))
         (short-dir (when dir (directory-file-name (abbreviate-file-name dir)))))
    (concat
     (propertize " " 'face 'herdr-sanctum-chrome-bar)
     (propertize ws-label 'face '(:inherit herdr-sanctum-chrome-bar :weight semi-bold))
     (if (and tab-label (not (equal tab-label ws-label)))
         (concat (propertize " / " 'face 'herdr-sanctum-chrome-bar)
                 (propertize tab-label 'face 'herdr-sanctum-chrome-pill))
       "")
     (if short-dir
         (propertize (concat "   " short-dir) 'face 'herdr-sanctum-chrome-bar)
       ""))))

(defun herdr-sanctum--create-focused-tab ()
  "Create a new Herdr tab in the currently focused workspace."
  (interactive)
  (let ((focused-ws-id (alist-get 'focused_workspace_id (herdr-sanctum-snapshot))))
    (herdr-sanctum-rpc-call
     "tab.create"
     (append `((focus . t))
             (when focused-ws-id
               `((workspace_id . ,focused-ws-id)))))))

(defun herdr-sanctum-header-line-right (&optional ctx drawer-open)
  "Format the right action buttons of `*sanctum*' top chrome bar.
Optional CTX supplies a pre-resolved drawer badge context plist, and
DRAWER-OPEN supplies pre-resolved drawer visibility when CTX is non-nil."
  (let* ((win (or (plist-get ctx :win)
                  (herdr-sanctum-panel-window)))
         (frame (and (window-live-p win) (window-frame win)))
         (drawer-open (if ctx
                          drawer-open
                        (herdr-sanctum-drawer-visible-p frame)))
         (btn-new (herdr-sanctum-make-chrome-button
                   " + "
                   'header-line
                   #'herdr-sanctum--create-focused-tab
                   'herdr-sanctum-chrome-pill
                   "New tab in focused space"))
         (sep (propertize " │ " 'face 'herdr-sanctum-chrome-sep))
         (sp (propertize " " 'face 'herdr-sanctum-chrome-bar))
         (drawer-controls
          (if drawer-open
              (herdr-sanctum-make-chrome-button
               " Hide "
               'header-line
               #'herdr-sanctum-drawer-toggle
               'herdr-sanctum-chrome-pill-active
               "Hide context drawer (C-c D)")
            (let* ((badge-ctx
                    (or ctx
                        (let ((width (if (window-live-p win) (window-body-width win) 80)))
                          (herdr-sanctum-drawer-build-context
                           width win (herdr-sanctum-snapshot)))))
                   (view-btns
                    (mapcar (lambda (entry)
                              (let* ((id (car entry))
                                     (spec (cdr entry))
                                     (label (plist-get spec :label))
                                     (badge-fn (plist-get spec :badge-fn))
                                     (badge (and (functionp badge-fn)
                                                 (funcall badge-fn badge-ctx)))
                                     (text (if (and (stringp badge)
                                                    (not (string-empty-p badge)))
                                               (format " %s %s " label badge)
                                             (format " %s " label))))
                                (herdr-sanctum-make-chrome-button
                                 text
                                 'header-line
                                 (lambda () (herdr-sanctum-drawer-open-view id))
                                 'herdr-sanctum-chrome-pill
                                 (or (plist-get spec :help)
                                     (format "Toggle %s drawer" label))
                                 (cons 'header-drawer-view id))))
                            (herdr-sanctum-drawer-active-views))))
              (string-join view-btns sp)))))
    (if (string-empty-p drawer-controls)
        (concat btn-new sp)
      (concat btn-new sep drawer-controls sp))))

(defun herdr-sanctum--refresh-header-line (&optional win)
  "Pre-format and cache `*sanctum*' top chrome header-line string.
Optional WIN specifies the live window displaying `*sanctum*'."
  (let ((buf (or (get-buffer herdr-sanctum-buffer-name) (current-buffer))))
    (with-current-buffer buf
      (let* ((snap (herdr-sanctum-snapshot))
             (win (or (and (window-live-p win)
                           (eq (window-buffer win) buf)
                           win)
                      (herdr-sanctum-panel-window buf)))
             (frame (and (window-live-p win) (window-frame win)))
             (drawer-open (and (herdr-sanctum-drawer-visible-p frame) t))
             (focused-ws-id (alist-get 'focused_workspace_id snap))
             (ws (and focused-ws-id
                      (seq-find (lambda (w) (equal (alist-get 'workspace_id w) focused-ws-id))
                                (alist-get 'workspaces snap))))
             (dir (herdr-sanctum-focused-dir snap))
             (git (and (not drawer-open) dir (herdr-sanctum-git-info-for-dir dir)))
             (width (if (window-live-p win) (window-body-width win) 80))
             (ctx (list :width width
                        :win win
                        :snapshot snap
                        :workspace ws
                        :dir dir
                        :git git))
             (formatted (herdr-sanctum-format-header-line
                         (herdr-sanctum-header-line-left snap ws dir)
                         (herdr-sanctum-header-line-right ctx drawer-open)
                         win)))
        (when (window-live-p win)
          (set-window-parameter
           win 'herdr-sanctum-header-width
           (window-body-width win (display-graphic-p (window-frame win)))))
        (setq herdr-sanctum--header-line-string formatted
              herdr-sanctum--header-line-dirty nil)
        (force-mode-line-update)
        formatted))))

(defun herdr-sanctum--sync-header-line (&rest _)
  "Refresh `*sanctum*' header-line when visible, or mark it dirty."
  (when herdr-sanctum-header-line-enable
    (when-let* ((buf (get-buffer herdr-sanctum-buffer-name)))
      (if-let* ((win (herdr-sanctum-panel-window buf)))
          (herdr-sanctum--refresh-header-line win)
        (with-current-buffer buf
          (setq herdr-sanctum--header-line-dirty t))))))

(defun herdr-sanctum--on-git-header-line (dir _info)
  "Refresh `*sanctum*' header-line when focused DIR's Git metadata updates."
  (when (or (null dir) (equal dir (herdr-sanctum-focused-dir)))
    (herdr-sanctum--sync-header-line)))

(defun herdr-sanctum--on-terminal-size-change (window-or-frame)
  "Refresh `*sanctum*' header-line when shown or when its width changes."
  (when herdr-sanctum-header-line-enable
    (let ((buf (if (and (windowp window-or-frame)
                        (window-live-p window-or-frame))
                   (window-buffer window-or-frame)
                 (current-buffer))))
      (when (and (buffer-live-p buf)
                 (eq buf (get-buffer herdr-sanctum-buffer-name)))
        (with-current-buffer buf
          (let ((win (if (windowp window-or-frame)
                         window-or-frame
                       (herdr-sanctum-panel-window buf))))
            (when (window-live-p win)
              (let ((width (window-body-width win (display-graphic-p (window-frame win)))))
                (when (or herdr-sanctum--header-line-dirty
                          (null herdr-sanctum--header-line-string)
                          (not (equal width (window-parameter win 'herdr-sanctum-header-width))))
                  (herdr-sanctum--refresh-header-line win))))))))))

(defun herdr-sanctum-header-line ()
  "Return `*sanctum*' top chrome bar or active terminal mode banner."
  (or (herdr-sanctum-terminal-mode-banner (current-buffer))
      (if (and herdr-sanctum--header-line-string
               (not herdr-sanctum--header-line-dirty))
          herdr-sanctum--header-line-string
        (herdr-sanctum--refresh-header-line))))

(add-hook 'herdr-sanctum-snapshot-hook #'herdr-sanctum--sync-header-line)
(add-hook 'herdr-sanctum-git-update-hook #'herdr-sanctum--on-git-header-line)
(add-hook 'herdr-sanctum-theme-change-hook #'herdr-sanctum--sync-header-line)
(add-hook 'herdr-sanctum-drawer-change-hook #'herdr-sanctum--sync-header-line)

;;; Fuzzy Jump

(defun herdr-sanctum--jump-candidates (snapshot)
  "Build ordered `(DISPLAY . TARGET)' jump entries from SNAPSHOT."
  (let* ((workspaces (alist-get 'workspaces snapshot))
         (tabs (alist-get 'tabs snapshot))
         (focused-ws (alist-get 'focused_workspace_id snapshot))
         (focused-tab (alist-get 'focused_tab_id snapshot))
         (ws-labels (make-hash-table :test #'equal))
         (tab-labels (make-hash-table :test #'equal))
         (tabs-by-ws (make-hash-table :test #'equal))
         (seen (make-hash-table :test #'equal))
         (entries nil)
         (add-entry
          (lambda (display target suffix)
            (let ((key display))
              (when (gethash key seen)
                (setq key (concat display (propertize (format " [%s]" suffix)
                                                      'face 'herdr-sanctum-sidebar-meta))))
              (puthash key t seen)
              (push (cons key target) entries)))))
    (dolist (ws workspaces)
      (when-let* ((ws-id (alist-get 'workspace_id ws)))
        (puthash ws-id (or (alist-get 'label ws) ws-id) ws-labels)))
    (dolist (tab tabs)
      (when-let* ((tab-id (alist-get 'tab_id tab)))
        (puthash tab-id (or (alist-get 'label tab) tab-id) tab-labels))
      (when-let* ((ws-id (alist-get 'workspace_id tab)))
        (puthash ws-id (cons tab (gethash ws-id tabs-by-ws)) tabs-by-ws)))
    (maphash (lambda (k v) (puthash k (nreverse v) tabs-by-ws)) tabs-by-ws)
    ;; 1. Attention agents (blocked -> done -> working)
    (dolist (agent (herdr-sanctum-attention-agents snapshot))
      (let* ((pane-id (alist-get 'pane_id agent))
             (ws-id (alist-get 'workspace_id agent))
             (tab-id (alist-get 'tab_id agent))
             (ws-label (or (gethash ws-id ws-labels) ws-id ""))
             (tab-label (or (gethash tab-id tab-labels) tab-id ""))
             (raw-topic (alist-get 'topic (alist-get 'tokens agent)))
             (topic (and (stringp raw-topic)
                         (let ((trimmed (string-trim raw-topic)))
                           (unless (string-empty-p trimmed) trimmed))))
             (glyph (or (herdr-sanctum-status-glyph (alist-get 'agent_status agent)) "●"))
             (display (concat glyph " "
                              (propertize ws-label 'face 'herdr-sanctum-sidebar-space-active)
                              (propertize " / " 'face 'herdr-sanctum-sidebar-meta)
                              (propertize tab-label 'face 'herdr-sanctum-sidebar-tab)
                              (if (and topic (not (equal topic tab-label)))
                                  (propertize (format " · %s" topic)
                                              'face 'herdr-sanctum-sidebar-tab-ws-active)
                                "")
                              (propertize (format "  %s" (or (alist-get 'agent agent) "agent"))
                                          'face 'herdr-sanctum-sidebar-meta))))
        (funcall add-entry display
                 (list :kind 'agent :pane-id pane-id :workspace-id ws-id :tab-id tab-id)
                 pane-id)))
    ;; 2. Tabs (focused workspace first, then remaining workspaces)
    (let ((ordered-ws (append (seq-filter (lambda (w) (equal (alist-get 'workspace_id w) focused-ws))
                                          workspaces)
                              (seq-remove (lambda (w) (equal (alist-get 'workspace_id w) focused-ws))
                                          workspaces))))
      (dolist (ws ordered-ws)
        (let* ((ws-id (alist-get 'workspace_id ws))
               (ws-label (or (alist-get 'label ws) ws-id))
               (ws-tabs (gethash ws-id tabs-by-ws)))
          (if ws-tabs
              (dolist (tab ws-tabs)
                (let* ((tab-id (alist-get 'tab_id tab))
                       (tab-label (or (alist-get 'label tab) tab-id))
                       (pane-count (or (alist-get 'pane_count tab) 1))
                       (glyph (or (herdr-sanctum-status-glyph (alist-get 'agent_status tab)) " "))
                       (display (concat glyph " "
                                        (propertize ws-label
                                                    'face (if (equal ws-id focused-ws)
                                                              'herdr-sanctum-sidebar-space-active
                                                            'herdr-sanctum-sidebar-meta))
                                        (propertize " / " 'face 'herdr-sanctum-sidebar-meta)
                                        (propertize tab-label
                                                    'face (if (equal tab-id focused-tab)
                                                              'bold
                                                            'herdr-sanctum-sidebar-tab))
                                        (if (> pane-count 1)
                                            (propertize (format "  %dp" pane-count)
                                                        'face 'herdr-sanctum-sidebar-meta)
                                          ""))))
                  (funcall add-entry display
                           (list :kind 'tab :tab-id tab-id :workspace-id ws-id)
                           tab-id)))
            (funcall add-entry
                     (concat "  " (propertize ws-label 'face 'herdr-sanctum-sidebar-space))
                     (list :kind 'workspace :workspace-id ws-id)
                     ws-id)))))
    (nreverse entries)))

(defun herdr-sanctum--fetch-jump-snapshot ()
  "Return a Herdr snapshot for `herdr-sanctum-jump' without leaking timers."
  (or (herdr-sanctum-snapshot)
      (let ((sock (herdr-sanctum-socket-path)))
        (unless (file-exists-p sock)
          (user-error "Herdr socket unavailable: %s" sock))
        (let ((herdr-sanctum-rpc-timeout 0.6)
              (done nil)
              (snap nil)
              (proc nil))
          (unwind-protect
              (progn
                (setq proc
                      (herdr-sanctum-rpc-call
                       "session.snapshot" nil
                       (lambda (result err)
                         (setq done t)
                         (unless err
                           (setq snap (alist-get 'snapshot result))
                           (when (and snap
                                      (or (herdr-sanctum-any-ui-buffer-live-p)
                                          (herdr-sanctum-connected-p)))
                             (herdr-sanctum-apply-snapshot snap))))
                       t))
                (let ((deadline (+ (float-time) 0.6)))
                  (while (and (not done) (< (float-time) deadline))
                    (accept-process-output nil 0.02)))
                snap)
            (when (and (not done) (processp proc))
              (when-let* ((timer (process-get proc 'herdr-sanctum-rpc-timer)))
                (when (timerp timer)
                  (cancel-timer timer))
                (process-put proc 'herdr-sanctum-rpc-timer nil))
              (when (process-live-p proc)
                (delete-process proc))))))))

;;;###autoload
(defun herdr-sanctum-jump ()
  "Fuzzy-jump to any Herdr attention agent, space, or tab."
  (interactive)
  (let ((snap (herdr-sanctum--fetch-jump-snapshot)))
    (unless snap
      (user-error "Herdr snapshot unavailable"))
    (let* ((candidates (herdr-sanctum--jump-candidates snap))
           (table (lambda (string pred action)
                    (if (eq action 'metadata)
                        '(metadata (category . herdr-sanctum-target)
                                   (display-sort-function . identity)
                                   (cycle-sort-function . identity))
                      (complete-with-action action candidates string pred))))
           (choice (completing-read "Sanctum jump: " table nil t))
           (target (cdr (assoc choice candidates))))
      (when target
        (if (get-buffer-window herdr-sanctum-buffer-name (selected-frame))
            (herdr-sanctum-focus-terminal)
          (herdr-sanctum '(4)))
        (herdr-sanctum-sidebar-focus-item target nil)))))

;;; Entry Point & Host Container

(defun herdr-sanctum--prepare-host ()
  "Prepare the host frame/tab according to `herdr-sanctum-display-mode'."
  (cond
   ((functionp herdr-sanctum-display-mode)
    (funcall herdr-sanctum-display-mode))
   ((eq herdr-sanctum-display-mode 'tab)
    (unless tab-bar-mode
      (tab-bar-mode 1))
    (if (seq-some (lambda (tab) (equal (alist-get 'name tab) herdr-sanctum-tab-name))
                  (tab-bar-tabs))
        (tab-bar-select-tab-by-name herdr-sanctum-tab-name)
      (tab-bar-new-tab)
      (tab-bar-rename-tab herdr-sanctum-tab-name)))
   ((eq herdr-sanctum-display-mode 'frame)
    (let ((existing (seq-find (lambda (f)
                                (equal (frame-parameter f 'name)
                                       herdr-sanctum-frame-name))
                              (frame-list))))
      (if (frame-live-p existing)
          (select-frame-set-input-focus existing)
        (select-frame-set-input-focus
         (make-frame `((name . ,herdr-sanctum-frame-name)))))))
   ((eq herdr-sanctum-display-mode 'current)
    nil)
   (t
    (user-error "Invalid `herdr-sanctum-display-mode': %S"
                herdr-sanctum-display-mode))))

(defun herdr-sanctum-default-toggle-back ()
  "Switch to the previous `tab-bar' tab when in `tab' display mode."
  (when (and (eq herdr-sanctum-display-mode 'tab)
             (> (length (tab-bar-tabs)) 1)
             (equal (alist-get 'name (tab-bar--current-tab))
                    herdr-sanctum-tab-name))
    (tab-bar-switch-to-recent-tab)
    t))

;;;###autoload
(defun herdr-sanctum (&optional arg)
  "Open the Sanctum workbench, or toggle back when already inside it.
With prefix ARG, always focus `*sanctum*' without toggling back."
  (interactive "P")
  (unless (and (called-interactively-p 'any)
               (not arg)
               (member (buffer-name) (herdr-sanctum-ui-buffers))
               (functionp herdr-sanctum-toggle-back-function)
               (funcall herdr-sanctum-toggle-back-function))
    (let* ((existing-buf (get-buffer herdr-sanctum-buffer-name))
           (resolved (herdr-sanctum-terminal-validate-start existing-buf)))
      (herdr-sanctum--prepare-host)
      (let ((visible-win (and existing-buf
                              (herdr-sanctum-terminal-live-p existing-buf)
                              (get-buffer-window existing-buf (selected-frame)))))
        (if (and (window-live-p visible-win)
                 (or (not (herdr-sanctum-sidebar-enabled-p))
                     (herdr-sanctum-sidebar-visible-p))
                 (or (not (herdr-sanctum-drawer-enabled-p))
                     (herdr-sanctum-drawer-visible-p)))
            (progn
              (select-window visible-win)
              (when (and herdr-sanctum-header-line-enable
                         (or (null (buffer-local-value 'herdr-sanctum--header-line-string existing-buf))
                             (buffer-local-value 'herdr-sanctum--header-line-dirty existing-buf)))
                (herdr-sanctum--refresh-header-line visible-win))
              (herdr-sanctum-ensure-connected))
          (let* ((init-dir (or (herdr-sanctum-focused-dir) (expand-file-name "~/")))
                 (default-directory init-dir)
                 (saved-wconf (current-window-configuration))
                 (created-buf (null existing-buf))
                 (buffer (or existing-buf (get-buffer-create herdr-sanctum-buffer-name))))
            (with-current-buffer buffer
              (setq-local default-directory init-dir))
            (condition-case err
                (progn
                  (switch-to-buffer buffer)
                  (delete-other-windows)
                  (herdr-sanctum-terminal-start buffer resolved))
              (error
               (set-window-configuration saved-wconf)
               (when (and created-buf (buffer-live-p buffer))
                 (kill-buffer buffer))
               (signal (car err) (cdr err))))
            (with-current-buffer buffer
              (herdr-sanctum-apply-terminal-header-remapping buffer)
              (setq-local mode-line-format nil
                          header-line-format
                          (when herdr-sanctum-header-line-enable
                            '(:eval (or (herdr-sanctum-terminal-mode-banner)
                                        herdr-sanctum--header-line-string)))
                          left-fringe-width 0
                          right-fringe-width 0
                          left-margin-width 0
                          right-margin-width 0)
              (add-hook 'kill-buffer-hook #'herdr-sanctum-disconnect nil t)
              (add-hook 'window-size-change-functions
                        #'herdr-sanctum--on-terminal-size-change nil t))
            (let ((term-win (selected-window)))
              (set-window-fringes term-win 0 0)
              (set-window-margins term-win 0 0)
              (when (herdr-sanctum-sidebar-enabled-p)
                (herdr-sanctum-sidebar-show))
              (when (herdr-sanctum-drawer-enabled-p)
                (herdr-sanctum-drawer-show))
              (select-window term-win)
              (herdr-sanctum-terminal-resize term-win)
              (set-window-dedicated-p term-win t)
              (when herdr-sanctum-header-line-enable
                (herdr-sanctum--refresh-header-line term-win)))
            (herdr-sanctum-ensure-connected)))))))

;;; Cross-Boundary Navigation & DWIM Window/Pane Commands

(defun herdr-sanctum--windmove (direction)
  "Select the adjacent non-minibuffer Emacs window in DIRECTION, if any."
  (when-let* ((target (windmove-find-other-window direction)))
    (unless (window-minibuffer-p target)
      (select-window target))))

(defun herdr-sanctum-navigate (direction)
  "Move focus in DIRECTION (`left', `down', `up', or `right').
Inside `*sanctum*', moves across Herdr panes via RPC and seamlessly crosses
into adjacent Emacs windows (such as `*sanctum-spaces*' or `*sanctum-drawer*')
at the Herdr edge.  Elsewhere, moves between Emacs windows."
  (if (not (herdr-sanctum-in-sanctum-p))
      (herdr-sanctum--windmove direction)
    (let ((origin (selected-window)))
      (herdr-sanctum-rpc-call
       "pane.focus_direction"
       `((direction . ,(symbol-name direction)))
       (lambda (result err)
         (cond
          ((and (not err) (alist-get 'changed result))
           (herdr-sanctum-schedule-snapshot))
          ((and (window-live-p origin)
                (eq (selected-window) origin))
           (herdr-sanctum--windmove direction))))
       t))))

;;;###autoload
(defun herdr-sanctum-nav-left ()
  "Move focus left across Herdr panes and Emacs windows."
  (interactive)
  (herdr-sanctum-navigate 'left))

;;;###autoload
(defun herdr-sanctum-nav-down ()
  "Move focus down across Herdr panes and Emacs windows."
  (interactive)
  (herdr-sanctum-navigate 'down))

;;;###autoload
(defun herdr-sanctum-nav-up ()
  "Move focus up across Herdr panes and Emacs windows."
  (interactive)
  (herdr-sanctum-navigate 'up))

;;;###autoload
(defun herdr-sanctum-nav-right ()
  "Move focus right across Herdr panes and Emacs windows."
  (interactive)
  (herdr-sanctum-navigate 'right))

;;;###autoload
(defun herdr-sanctum-split-right ()
  "Split Herdr pane right in `*sanctum*', or split the Emacs window right."
  (interactive)
  (if (herdr-sanctum-in-sanctum-p)
      (herdr-sanctum-rpc-call "pane.split" '((direction . "right") (focus . t)))
    (select-window (split-window-right))))

;;;###autoload
(defun herdr-sanctum-split-down ()
  "Split Herdr pane down in `*sanctum*', or split the Emacs window below."
  (interactive)
  (if (herdr-sanctum-in-sanctum-p)
      (herdr-sanctum-rpc-call "pane.split" '((direction . "down") (focus . t)))
    (select-window (split-window-below))))

(defun herdr-sanctum-default-maximize-window ()
  "Toggle maximizing the current non-side Emacs window."
  (interactive)
  (let* ((tab (and (bound-and-true-p tab-bar-mode)
                   (tab-bar--current-tab-find)))
         (saved (if tab
                    (alist-get 'herdr-sanctum-saved-window-config (cdr tab))
                  (frame-parameter nil 'herdr-sanctum-saved-window-config)))
         (valid-saved (and (window-configuration-p saved)
                           (eq (window-configuration-frame saved) (selected-frame))
                           saved))
         (non-side-count
          (seq-count (lambda (w) (not (window-parameter w 'window-side)))
                     (window-list nil 'no-minibuf))))
    (when (and saved (not valid-saved))
      (if tab
          (setf (alist-get 'herdr-sanctum-saved-window-config (cdr tab) nil t) nil)
        (set-frame-parameter nil 'herdr-sanctum-saved-window-config nil)))
    (if (and (= non-side-count 1) valid-saved)
        (progn
          (if tab
              (setf (alist-get 'herdr-sanctum-saved-window-config (cdr tab) nil t) nil)
            (set-frame-parameter nil 'herdr-sanctum-saved-window-config nil))
          (set-window-configuration valid-saved))
      (when (and (> non-side-count 1)
                 (not (window-parameter nil 'window-side)))
        (let ((wconf (current-window-configuration)))
          (if tab
              (setf (alist-get 'herdr-sanctum-saved-window-config (cdr tab)) wconf)
            (set-frame-parameter nil 'herdr-sanctum-saved-window-config wconf)))
        (delete-other-windows)))))

;;;###autoload
(defun herdr-sanctum-zoom-or-maximize ()
  "Toggle Herdr pane zoom inside `*sanctum*', falling back to Emacs maximize."
  (interactive)
  (if (not (herdr-sanctum-in-sanctum-p))
      (funcall herdr-sanctum-maximize-window-function)
    (let ((origin (selected-window)))
      (herdr-sanctum-rpc-call
       "pane.zoom"
       '((mode . "toggle"))
       (lambda (result err)
         (cond
          ((and (not err) (alist-get 'changed result))
           (herdr-sanctum-schedule-snapshot))
          ((and (window-live-p origin)
                (eq (selected-window) origin))
           (ignore-errors (funcall herdr-sanctum-maximize-window-function)))))
       t))))

(defun herdr-sanctum-tab-step (delta)
  "Step DELTA tabs in active Herdr space when in `*sanctum*', else Emacs."
  (if (not (herdr-sanctum-in-sanctum-p))
      (if (< delta 0) (tab-previous) (tab-next))
    (let* ((snap (herdr-sanctum-snapshot))
           (ws-id (alist-get 'focused_workspace_id snap))
           (cur-tab (alist-get 'focused_tab_id snap))
           (ws-tabs (seq-filter
                     (lambda (tab) (equal (alist-get 'workspace_id tab) ws-id))
                     (alist-get 'tabs snap)))
           (count (length ws-tabs)))
      (cond
       ((null snap)
        (herdr-sanctum-request-snapshot)
        (message "Refreshing Herdr state..."))
       ((<= count 1)
        (message "Single tab in current Herdr space"))
       (t
        (let* ((idx (or (seq-position ws-tabs cur-tab
                                      (lambda (tab id)
                                        (equal (alist-get 'tab_id tab) id)))
                        0))
               (target (nth (mod (+ idx delta) count) ws-tabs))
               (target-id (alist-get 'tab_id target)))
          (when target-id
            (herdr-sanctum-rpc-call "tab.focus" `((tab_id . ,target-id))))))))))

;;;###autoload
(defun herdr-sanctum-prev-tab ()
  "Switch to the previous Herdr tab (or Emacs tab outside `*sanctum*')."
  (interactive)
  (herdr-sanctum-tab-step -1))

;;;###autoload
(defun herdr-sanctum-next-tab ()
  "Switch to the next Herdr tab (or Emacs tab outside `*sanctum*')."
  (interactive)
  (herdr-sanctum-tab-step 1))

(defun herdr-sanctum-agent-step (delta)
  "Focus the next (DELTA > 0) or previous (DELTA < 0) Herdr agent pane."
  (let* ((snap (herdr-sanctum-snapshot))
         (agents (sort (copy-sequence (alist-get 'agents snap))
                       (lambda (a b)
                         (< (or (herdr-sanctum-agent-status-rank (alist-get 'agent_status a)) 3)
                            (or (herdr-sanctum-agent-status-rank (alist-get 'agent_status b)) 3)))))
         (count (length agents)))
    (cond
     ((null snap)
      (herdr-sanctum-request-snapshot)
      (message "Refreshing Herdr state..."))
     ((zerop count)
      (message "No active Herdr agents"))
     (t
      (let* ((focused (alist-get 'focused_pane_id snap))
             (idx (seq-position agents focused
                                (lambda (a id)
                                  (equal (alist-get 'pane_id a) id))))
             (target (if idx
                         (nth (mod (+ idx delta) count) agents)
                       (if (> delta 0) (car agents) (car (last agents)))))
             (pane-id (alist-get 'pane_id target)))
        (when pane-id
          (herdr-sanctum-rpc-call "pane.focus" `((pane_id . ,pane-id)))
          (if (get-buffer-window herdr-sanctum-buffer-name (selected-frame))
              (herdr-sanctum-focus-terminal)
            (herdr-sanctum '(4)))))))))

;;;###autoload
(defun herdr-sanctum-next-agent ()
  "Focus the next Herdr agent pane."
  (interactive)
  (herdr-sanctum-agent-step 1))

;;;###autoload
(defun herdr-sanctum-prev-agent ()
  "Focus the previous Herdr agent pane."
  (interactive)
  (herdr-sanctum-agent-step -1))

;;;###autoload
(defun herdr-sanctum-new-tab-dwim ()
  "Create a new Herdr tab inside `*sanctum*', else call fallback command."
  (interactive)
  (cond
   ((herdr-sanctum-in-sanctum-p)
    (herdr-sanctum--create-focused-tab))
   ((functionp herdr-sanctum-new-tab-fallback-function)
    (call-interactively herdr-sanctum-new-tab-fallback-function))
   (t
    (tab-bar-new-tab))))

;;;###autoload
(defun herdr-sanctum-close-dwim ()
  "Close focused Herdr pane in `*sanctum*', or close the window/buffer."
  (interactive)
  (cond
   ((herdr-sanctum-in-sanctum-p)
    (let ((pane-id (alist-get 'focused_pane_id (herdr-sanctum-snapshot))))
      (if pane-id
          (when (y-or-n-p (format "Close Herdr pane (%s)? "
                                  (herdr-sanctum-focused-pane-label)))
            (herdr-sanctum-rpc-call "pane.close" `((pane_id . ,pane-id))))
        (let ((origin (selected-window)))
          (herdr-sanctum-rpc-call
           "pane.current" nil
           (lambda (result err)
             (let ((id (and (not err) (alist-get 'pane_id result))))
               (if (not id)
                   (message "No focused Herdr pane to close")
                 (run-at-time
                  0 nil
                  (lambda ()
                    (when (and (window-live-p origin)
                               (eq (selected-window) origin)
                               (y-or-n-p (format "Close Herdr pane (%s)? " id)))
                      (herdr-sanctum-rpc-call "pane.close" `((pane_id . ,id)))))))))
           t)))))
   ((eq (window-deletable-p) t)
    (delete-window))
   (t
    (kill-current-buffer))))

;;; Prefix Keymap (Opt-in, Zero Global Pollution)

;;;###autoload
(defvar-keymap herdr-sanctum-command-map
  :doc "Prefix keymap for `herdr-sanctum' commands.
Bind it to a prefix of your choice, e.g.:
  (keymap-global-set \"C-c s\" herdr-sanctum-command-map)"
  :prefix 'herdr-sanctum-command-map
  "s" #'herdr-sanctum
  "b" #'herdr-sanctum-sidebar-toggle
  "d" #'herdr-sanctum-drawer-toggle
  "j" #'herdr-sanctum-jump
  "n" #'herdr-sanctum-next-agent
  "p" #'herdr-sanctum-prev-agent
  "t" #'herdr-sanctum-new-tab-dwim
  "g" #'herdr-sanctum-refresh-state
  "?" #'herdr-sanctum-doctor)

;;; Diagnostics

;;;###autoload
(defun herdr-sanctum-doctor ()
  "Inspect `herdr-sanctum' environment, socket health, and Herdr config."
  (interactive)
  (let* ((prog (executable-find herdr-sanctum-program))
         (sock (herdr-sanctum-socket-path))
         (sock-ok (file-exists-p sock))
         (backend (herdr-sanctum-terminal-resolve-backend t))
         (avail-fn (plist-get backend :available-p))
         (backend-ok (and backend (or (null avail-fn) (funcall avail-fn))))
         (svg-ok (herdr-sanctum-svg-available-p))
         (icons-ok (and herdr-sanctum-agent-icon-directory
                        (file-directory-p herdr-sanctum-agent-icon-directory)))
         (buf (get-buffer-create "*herdr-sanctum-doctor*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "herdr-sanctum doctor\n====================\n\n")
        (insert (format "  herdr executable : %s\n" (or prog "MISSING (not in exec-path)")))
        (insert (format "  herdr socket     : %s (%s)\n"
                        sock (if sock-ok "present" "not found — start herdr first")))
        (insert (format "  socket connected : %s\n" (if (herdr-sanctum-connected-p) "yes" "no")))
        (insert (format "  terminal backend : %S (%s)\n"
                        herdr-sanctum-terminal-backend
                        (if backend-ok "available" "unavailable")))
        (insert (format "  inline SVG       : %s\n" (if svg-ok "supported" "TTY/text fallback")))
        (insert (format "  agent SVG dir    : %s (%s)\n"
                        (or herdr-sanctum-agent-icon-directory "nil")
                        (if icons-ok "found" "fallback to SVG monograms")))
        (insert (format "  display mode     : %S\n" herdr-sanctum-display-mode))
        (insert (format "  drawer views     : %S\n\n"
                        (mapcar #'car (herdr-sanctum-drawer-active-views))))
        (insert "Recommended ~/.config/herdr/config.toml settings for seamless Emacs chrome:\n\n")
        (insert "  [theme]\n  name = \"terminal\"\n\n")
        (insert "  [ui]\n")
        (insert "  sidebar_start_collapsed = true\n")
        (insert "  sidebar_collapsed_mode = \"hidden\"\n")
        (insert "  hide_tab_bar = true\n")
        (insert "  pane_outer_borders = false\n")
        (insert "  redraw_on_focus_gained = false\n")
        (special-mode)
        (goto-char (point-min))))
    (pop-to-buffer buf)))

(provide 'herdr-sanctum)
;;; herdr-sanctum.el ends here
