;;; herdr-sanctum-rpc.el --- Local Unix socket client for Herdr -*- lexical-binding: t; -*-

;; Copyright (C) 2026 oatiz

;; Author: oatiz <iamoatiz@gmail.com>
;; URL: https://github.com/oatiz/herdr-sanctum
;; Keywords: tools, terminals, processes

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

;; Event-driven Unix socket bridge to Herdr (`herdr.sock').
;; Maintains a persistent event subscription, throttled session snapshots,
;; asynchronous JSON-RPC calls, agent elapsed-time tracking, and public
;; hooks so UI panels and optional extensions stay decoupled.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)

(defgroup herdr-sanctum nil
  "Native Emacs multi-agent workbench UI for Herdr."
  :group 'tools
  :prefix "herdr-sanctum-")

(defcustom herdr-sanctum-socket-path nil
  "Path to the local Herdr Unix socket (`herdr.sock').
When nil, resolve via the `HERDR_SOCKET_PATH' environment variable,
falling back to `$XDG_CONFIG_HOME/herdr/herdr.sock' or
`~/.config/herdr/herdr.sock'.  May also be a zero-argument function
returning a path string."
  :type '(choice (const :tag "Auto-detect" nil)
                 (file :tag "Socket path")
                 (function :tag "Function returning path"))
  :group 'herdr-sanctum)

(defcustom herdr-sanctum-buffer-name "*sanctum*"
  "Name of the dedicated Herdr terminal buffer."
  :type 'string
  :group 'herdr-sanctum)

(defcustom herdr-sanctum-sidebar-buffer-name "*sanctum-spaces*"
  "Name of the persistent Emacs sidebar displaying Herdr spaces and agents."
  :type 'string
  :group 'herdr-sanctum)

(defcustom herdr-sanctum-drawer-buffer-name "*sanctum-drawer*"
  "Name of the toggleable right context drawer buffer."
  :type 'string
  :group 'herdr-sanctum)

(defcustom herdr-sanctum-min-refresh-interval 0.25
  "Minimum seconds between event-triggered `session.snapshot' fetches."
  :type 'number
  :group 'herdr-sanctum)

(defcustom herdr-sanctum-reconnect-interval 5.0
  "Seconds between reconnection attempts when `herdr.sock' is unavailable."
  :type 'number
  :group 'herdr-sanctum)

(defcustom herdr-sanctum-rpc-timeout 3.0
  "Timeout in seconds for one-shot Herdr JSON-RPC requests."
  :type 'number
  :group 'herdr-sanctum)

(defcustom herdr-sanctum-duration-refresh-interval 30.0
  "Interval in seconds for refreshing relative agent elapsed durations."
  :type 'number
  :group 'herdr-sanctum)

(defcustom herdr-sanctum-event-hook nil
  "Hook run with `(KIND DATA)' for parsed Herdr subscription events.
KIND is the event type string (for example \"pane_focused\" or
\"pane_agent_detected\") and DATA is the parsed alist."
  :type 'hook
  :group 'herdr-sanctum)

(defcustom herdr-sanctum-snapshot-hook nil
  "Hook run with `(SNAPSHOT)' whenever a fresh Herdr snapshot is applied."
  :type 'hook
  :group 'herdr-sanctum)

(defcustom herdr-sanctum-focus-change-hook nil
  "Hook run with `(DIR PANE-ID WORKSPACE-ID TAB-ID)' on focus changes."
  :type 'hook
  :group 'herdr-sanctum)

(defcustom herdr-sanctum-agent-status-change-hook nil
  "Hook run with `(AGENT OLD-STATUS NEW-STATUS)' when an agent status changes.
AGENT is the agent alist from the Herdr snapshot."
  :type 'hook
  :group 'herdr-sanctum)

(defcustom herdr-sanctum-duration-tick-hook nil
  "Hook run periodically to refresh relative agent durations in visible UI."
  :type 'hook
  :group 'herdr-sanctum)

(defcustom herdr-sanctum-disconnect-hook nil
  "Hook run when the Herdr socket bridge disconnects."
  :type 'hook
  :group 'herdr-sanctum)

(defconst herdr-sanctum--max-line-bytes (* 256 1024)
  "Maximum unterminated line length buffered on a Herdr socket.")

(defconst herdr-sanctum--subscriptions
  '("workspace.created"
    "workspace.focused"
    "workspace.updated"
    "workspace.renamed"
    "workspace.moved"
    "workspace.reordered"
    "workspace.closed"
    "tab.created"
    "tab.closed"
    "tab.focused"
    "tab.renamed"
    "tab.moved"
    "pane.focused"
    "pane.updated"
    "pane.created"
    "pane.closed"
    "pane.exited"
    "pane.agent_detected"
    "pane.agent_status_changed")
  "Herdr event subscriptions registered on `events.subscribe'.")

(defvar herdr-sanctum--sub-process nil
  "Persistent local socket subscription to Herdr server events.")

(defvar herdr-sanctum--snap-process nil
  "In-flight local socket process for the current `session.snapshot' request.")

(defvar herdr-sanctum--snap-timeout-timer nil
  "Timeout timer closing a stalled `session.snapshot' socket.")

(defvar herdr-sanctum--refresh-timer nil
  "Throttled timer requesting a `session.snapshot' after Herdr events.")

(defvar herdr-sanctum--last-snap-time 0.0
  "Timestamp of the most recent `session.snapshot' dispatch.")

(defvar herdr-sanctum--reconnect-timer nil
  "Retry timer reconnecting to `herdr.sock' while Sanctum buffers are live.")

(defvar herdr-sanctum--snapshot nil
  "Latest parsed Herdr `session.snapshot' alist.")

(defvar herdr-sanctum--rpc-seq 0
  "Monotonic request counter for one-shot Herdr JSON-RPC calls.")

(defvar herdr-sanctum--agent-timestamps (make-hash-table :test #'equal)
  "Map `pane_id' -> `(STATUS SEQ . TIMESTAMP)' for relative elapsed durations.")

(defvar herdr-sanctum--duration-timer nil
  "Periodic timer refreshing relative agent durations while UI is visible.")

(defvar herdr-sanctum--last-focus-key nil
  "Last `(DIR PANE-ID WORKSPACE-ID TAB-ID)' tuple emitted to focus hook.")

;;; Public Query Helpers

(defun herdr-sanctum-socket-path ()
  "Return the resolved local Herdr API socket path."
  (cond
   ((functionp herdr-sanctum-socket-path)
    (funcall herdr-sanctum-socket-path))
   ((stringp herdr-sanctum-socket-path)
    (expand-file-name herdr-sanctum-socket-path))
   (t
    (or (getenv "HERDR_SOCKET_PATH")
        (expand-file-name "herdr/herdr.sock"
                          (or (getenv "XDG_CONFIG_HOME") "~/.config"))))))

(defun herdr-sanctum-snapshot ()
  "Return the latest parsed Herdr session snapshot alist, or nil."
  herdr-sanctum--snapshot)

(defun herdr-sanctum-connected-p ()
  "Return non-nil when the Herdr event subscription socket is live."
  (process-live-p herdr-sanctum--sub-process))

(defun herdr-sanctum-ui-buffers ()
  "Return the list of Sanctum UI buffer names."
  (list herdr-sanctum-buffer-name
        herdr-sanctum-sidebar-buffer-name
        herdr-sanctum-drawer-buffer-name))

(defun herdr-sanctum-any-ui-buffer-live-p ()
  "Return non-nil when any Sanctum terminal or side-panel buffer exists."
  (or (get-buffer herdr-sanctum-buffer-name)
      (get-buffer herdr-sanctum-sidebar-buffer-name)
      (get-buffer herdr-sanctum-drawer-buffer-name)))

(defun herdr-sanctum-workspace-dir (ws &optional snapshot)
  "Return normalized active directory for workspace WS in SNAPSHOT, or nil."
  (let* ((snap (or snapshot herdr-sanctum--snapshot))
         (ws-id (alist-get 'workspace_id ws))
         (active-tab (alist-get 'active_tab_id ws))
         (panes (alist-get 'panes snap))
         (pane (or (seq-find (lambda (p)
                               (and (equal (alist-get 'workspace_id p) ws-id)
                                    (equal (alist-get 'tab_id p) active-tab)
                                    (alist-get 'focused p)))
                             panes)
                   (seq-find (lambda (p)
                               (and (equal (alist-get 'workspace_id p) ws-id)
                                    (equal (alist-get 'tab_id p) active-tab)))
                             panes)
                   (seq-find (lambda (p)
                               (equal (alist-get 'workspace_id p) ws-id))
                             panes)))
         (raw (and pane (or (alist-get 'foreground_cwd pane)
                            (alist-get 'cwd pane)))))
    (when (and (stringp raw) (file-directory-p raw))
      (file-name-as-directory (expand-file-name raw)))))

(defun herdr-sanctum-focused-dir (&optional snapshot)
  "Return normalized directory of the globally focused Herdr pane, or nil."
  (let* ((snap (or snapshot herdr-sanctum--snapshot))
         (focused-id (alist-get 'focused_pane_id snap))
         (pane (and focused-id
                    (seq-find (lambda (p)
                                (equal (alist-get 'pane_id p) focused-id))
                              (alist-get 'panes snap))))
         (raw (and pane (or (alist-get 'foreground_cwd pane)
                            (alist-get 'cwd pane)))))
    (cond
     ((and (stringp raw) (file-directory-p raw))
      (file-name-as-directory (expand-file-name raw)))
     ((when-let* ((buf (get-buffer herdr-sanctum-buffer-name)))
        (with-current-buffer buf
          (when (and default-directory (file-directory-p default-directory))
            (file-name-as-directory (expand-file-name default-directory)))))))))

(defun herdr-sanctum-focused-pane-label (&optional snapshot)
  "Return a concise label describing the focused Herdr pane in SNAPSHOT."
  (let* ((snap (or snapshot herdr-sanctum--snapshot))
         (pane-id (alist-get 'focused_pane_id snap))
         (ws-id (alist-get 'focused_workspace_id snap))
         (tab-id (alist-get 'focused_tab_id snap))
         (ws (seq-find (lambda (w) (equal (alist-get 'workspace_id w) ws-id))
                       (alist-get 'workspaces snap)))
         (tab (seq-find (lambda (tb) (equal (alist-get 'tab_id tb) tab-id))
                        (alist-get 'tabs snap)))
         (pane (seq-find (lambda (p) (equal (alist-get 'pane_id p) pane-id))
                         (alist-get 'panes snap)))
         (ws-label (or (alist-get 'label ws) ws-id))
         (tab-label (alist-get 'label tab))
         (agent (or (alist-get 'display_agent pane) (alist-get 'agent pane))))
    (string-join (delq nil (list ws-label tab-label agent (unless ws-label pane-id)))
                 " / ")))

;;; Agent Helpers & Elapsed Duration Tracking

(defun herdr-sanctum-agent-status-rank (status)
  "Return sort priority for agent STATUS (lower is more urgent), or nil if idle."
  (pcase status
    ("blocked" 0)
    ("done"    1)
    ("working" 2)))

(defun herdr-sanctum-attention-agents (&optional snapshot)
  "Return non-idle agents from SNAPSHOT sorted by blocked -> done -> working."
  (let ((snap (or snapshot herdr-sanctum--snapshot))
        (items nil))
    (dolist (agent (alist-get 'agents snap))
      (when-let* ((rank (herdr-sanctum-agent-status-rank
                         (alist-get 'agent_status agent))))
        (push (cons rank agent) items)))
    (mapcar #'cdr
            (sort (nreverse items)
                  (lambda (a b)
                    (if (/= (car a) (car b))
                        (< (car a) (car b))
                      (> (or (alist-get 'state_change_seq (cdr a)) 0)
                         (or (alist-get 'state_change_seq (cdr b)) 0))))))))

(defun herdr-sanctum-grouped-agents (&optional snapshot)
  "Return agents from SNAPSHOT grouped as `((STATUS . LIST) ...)' in display order."
  (let ((snap (or snapshot herdr-sanctum--snapshot))
        (buckets (list (cons "blocked" nil)
                       (cons "working" nil)
                       (cons "done" nil)
                       (cons "idle" nil))))
    (dolist (agent (alist-get 'agents snap))
      (let* ((raw (or (alist-get 'agent_status agent) "idle"))
             (key (if (assoc raw buckets) raw "idle")))
        (push agent (cdr (assoc key buckets)))))
    (delq nil
          (mapcar (lambda (entry)
                    (when (cdr entry)
                      (cons (car entry)
                            (sort (nreverse (cdr entry))
                                  (lambda (a b)
                                    (> (or (alist-get 'state_change_seq a) 0)
                                       (or (alist-get 'state_change_seq b) 0)))))))
                  buckets))))

(defun herdr-sanctum-format-elapsed (seconds)
  "Format elapsed SECONDS as a compact human duration (`<1m', `7m', `2h', `3d')."
  (let ((secs (max 0 (floor seconds))))
    (cond
     ((< secs 60) "<1m")
     ((< secs 3600) (format "%dm" (/ secs 60)))
     ((< secs 86400) (format "%dh" (/ secs 3600)))
     (t (format "%dd" (/ secs 86400))))))

(defun herdr-sanctum-agent-elapsed-string (pane-id)
  "Return compact elapsed duration string for PANE-ID, or empty string."
  (if-let* ((entry (and pane-id (gethash pane-id herdr-sanctum--agent-timestamps)))
            (ts (cddr entry)))
      (herdr-sanctum-format-elapsed (- (float-time) ts))
    ""))

(defun herdr-sanctum--record-agent-timestamps (agents)
  "Track transition times for AGENTS and notify status changes."
  (let ((now (float-time))
        (live (make-hash-table :test #'equal))
        (changes nil))
    (dolist (agent agents)
      (when-let* ((pane-id (alist-get 'pane_id agent)))
        (puthash pane-id t live)
        (let* ((status (or (alist-get 'agent_status agent) "unknown"))
               (seq (alist-get 'state_change_seq agent))
               (prev (gethash pane-id herdr-sanctum--agent-timestamps))
               (old-status (car prev)))
          (unless (and prev
                       (equal old-status status)
                       (equal (cadr prev) seq))
            (puthash pane-id (cons status (cons seq now))
                     herdr-sanctum--agent-timestamps)
            (when (and prev (not (equal old-status status)))
              (push (list agent old-status status) changes))))))
    (let (stale)
      (maphash (lambda (k _v)
                 (unless (gethash k live) (push k stale)))
               herdr-sanctum--agent-timestamps)
      (dolist (k stale)
        (remhash k herdr-sanctum--agent-timestamps)))
    (dolist (change (nreverse changes))
      (run-hook-with-args 'herdr-sanctum-agent-status-change-hook
                          (nth 0 change) (nth 1 change) (nth 2 change)))))

(defun herdr-sanctum--ensure-duration-timer ()
  "Start the periodic relative-duration refresh timer when not already running."
  (unless (timerp herdr-sanctum--duration-timer)
    (setq herdr-sanctum--duration-timer
          (run-with-timer
           herdr-sanctum-duration-refresh-interval
           herdr-sanctum-duration-refresh-interval
           (lambda ()
             (when (or (get-buffer-window herdr-sanctum-sidebar-buffer-name t)
                       (get-buffer-window herdr-sanctum-drawer-buffer-name t))
               (run-hooks 'herdr-sanctum-duration-tick-hook)))))))

;;; Socket Lifecycle, Snapshot & RPC

(defun herdr-sanctum--handle-event-line (line)
  "Handle one Herdr subscription event LINE.
Return `immediate' for tab/workspace focus changes, `schedule' for other
events, or nil for subscription acknowledgments."
  (unless (string-search "subscription_started" line)
    (let* ((parsed (and (or (string-search "focused" line)
                            (string-search "agent_detected" line)
                            (string-search "agent_status" line)
                            (string-search "pane_closed" line)
                            (string-search "pane_exited" line)
                            (string-search "pane_created" line))
                        (ignore-errors
                          (json-parse-string line
                                             :object-type 'alist
                                             :array-type 'list
                                             :null-object nil
                                             :false-object nil))))
           (data (alist-get 'data parsed))
           (kind (alist-get 'type data)))
      (when kind
        (run-hook-with-args 'herdr-sanctum-event-hook kind data))
      (pcase kind
        ((or "tab_focused" "workspace_focused") 'immediate)
        (_ 'schedule)))))

(defun herdr-sanctum--apply-snapshot (snapshot)
  "Store SNAPSHOT, sync `*sanctum*' CWD, and dispatch update hooks."
  (setq herdr-sanctum--snapshot snapshot)
  (let* ((focused-id (alist-get 'focused_pane_id snapshot))
         (focused-ws (alist-get 'focused_workspace_id snapshot))
         (focused-tab (alist-get 'focused_tab_id snapshot))
         (dir (herdr-sanctum-focused-dir snapshot))
         (focus-key (list dir focused-id focused-ws focused-tab)))
    (herdr-sanctum--record-agent-timestamps (alist-get 'agents snapshot))
    (when dir
      (when-let* ((buf (get-buffer herdr-sanctum-buffer-name)))
        (with-current-buffer buf
          (unless (equal default-directory dir)
            (setq-local default-directory dir)))))
    (unless (equal focus-key herdr-sanctum--last-focus-key)
      (setq herdr-sanctum--last-focus-key focus-key)
      (run-hook-with-args 'herdr-sanctum-focus-change-hook
                          dir focused-id focused-ws focused-tab)))
  (herdr-sanctum--ensure-duration-timer)
  (run-hook-with-args 'herdr-sanctum-snapshot-hook snapshot)
  (force-mode-line-update t))

(defun herdr-sanctum--abort-snapshot ()
  "Close any in-flight Herdr snapshot socket and cancel its timeout."
  (when (timerp herdr-sanctum--snap-timeout-timer)
    (cancel-timer herdr-sanctum--snap-timeout-timer)
    (setq herdr-sanctum--snap-timeout-timer nil))
  (when (process-live-p herdr-sanctum--snap-process)
    (delete-process herdr-sanctum--snap-process))
  (setq herdr-sanctum--snap-process nil))

(defun herdr-sanctum-request-snapshot (&optional force)
  "Fetch a fresh Herdr session snapshot asynchronously over `herdr.sock'.
When FORCE is non-nil, query even if no Sanctum UI buffer is open yet."
  (when (timerp herdr-sanctum--refresh-timer)
    (cancel-timer herdr-sanctum--refresh-timer)
    (setq herdr-sanctum--refresh-timer nil))
  (herdr-sanctum--abort-snapshot)
  (let ((sock (herdr-sanctum-socket-path)))
    (when (and (or force (herdr-sanctum-any-ui-buffer-live-p))
               (file-exists-p sock))
      (setq herdr-sanctum--last-snap-time (float-time))
      (ignore-errors
        (let ((proc (make-network-process
                     :name "herdr-sanctum-snapshot"
                     :family 'local
                     :service sock
                     :coding 'utf-8-unix
                     :noquery t
                     :filter
                     (lambda (p chunk)
                       (when (eq p herdr-sanctum--snap-process)
                         (let ((acc (concat (or (process-get p 'herdr-sanctum-buf) "") chunk)))
                           (cond
                            ((when-let* ((nl (string-search "\n" acc)))
                               (let ((line (substring acc 0 nl)))
                                 (herdr-sanctum--abort-snapshot)
                                 (when (or force (herdr-sanctum-any-ui-buffer-live-p))
                                   (when-let* ((parsed (ignore-errors
                                                         (json-parse-string line
                                                                            :object-type 'alist
                                                                            :array-type 'list
                                                                            :null-object nil
                                                                            :false-object nil)))
                                               (snapshot (alist-get 'snapshot (alist-get 'result parsed))))
                                     (herdr-sanctum--apply-snapshot snapshot))))
                               t))
                            ((> (length acc) herdr-sanctum--max-line-bytes)
                             (herdr-sanctum--abort-snapshot))
                            (t
                             (process-put p 'herdr-sanctum-buf acc))))))
                     :sentinel
                     (lambda (p _event)
                       (when (and (eq p herdr-sanctum--snap-process)
                                  (not (process-live-p p)))
                         (herdr-sanctum--abort-snapshot))))))
          (setq herdr-sanctum--snap-process proc
                herdr-sanctum--snap-timeout-timer
                (run-with-timer herdr-sanctum-rpc-timeout nil
                                (lambda ()
                                  (when (eq proc herdr-sanctum--snap-process)
                                    (herdr-sanctum--abort-snapshot)))))
          (process-send-string
           proc "{\"id\":\"emacs:snap\",\"method\":\"session.snapshot\",\"params\":{}}\n"))))))

(defun herdr-sanctum--schedule-snapshot ()
  "Throttle bursts of Herdr events into bounded local snapshot queries."
  (unless (timerp herdr-sanctum--refresh-timer)
    (let* ((elapsed (- (float-time) herdr-sanctum--last-snap-time))
           (delay (max 0.08 (- herdr-sanctum-min-refresh-interval elapsed))))
      (setq herdr-sanctum--refresh-timer
            (run-with-timer delay nil #'herdr-sanctum-request-snapshot)))))

(defun herdr-sanctum--schedule-reconnect ()
  "Retry connecting to `herdr.sock' later while Sanctum UI buffers are alive."
  (when (and (herdr-sanctum-any-ui-buffer-live-p)
             (not (timerp herdr-sanctum--reconnect-timer)))
    (setq herdr-sanctum--reconnect-timer
          (run-with-timer herdr-sanctum-reconnect-interval nil
                          (lambda ()
                            (setq herdr-sanctum--reconnect-timer nil)
                            (when (herdr-sanctum-any-ui-buffer-live-p)
                              (herdr-sanctum-ensure-connected)))))))

(defun herdr-sanctum-ensure-connected ()
  "Connect to `herdr.sock' for focus and agent events when not already connected."
  (unless (process-live-p herdr-sanctum--sub-process)
    (let ((sock (herdr-sanctum-socket-path)))
      (if (not (file-exists-p sock))
          (herdr-sanctum--schedule-reconnect)
        (condition-case nil
            (let ((proc (make-network-process
                         :name "herdr-sanctum-events"
                         :family 'local
                         :service sock
                         :coding 'utf-8-unix
                         :noquery t
                         :filter
                         (lambda (p chunk)
                           (let* ((acc (concat (or (process-get p 'herdr-sanctum-buf) "") chunk))
                                  (start 0)
                                  (refresh nil)
                                  nl)
                             (while (setq nl (string-search "\n" acc start))
                               (let ((mode (herdr-sanctum--handle-event-line
                                            (substring acc start nl))))
                                 (cond
                                  ((eq mode 'immediate) (setq refresh 'immediate))
                                  ((and mode (null refresh)) (setq refresh 'schedule))))
                               (setq start (1+ nl)))
                             (let ((rem (if (> start 0) (substring acc start) acc)))
                               (process-put p 'herdr-sanctum-buf
                                            (if (> (length rem) herdr-sanctum--max-line-bytes) "" rem)))
                             (pcase refresh
                               ('immediate (herdr-sanctum-request-snapshot))
                               ('schedule (herdr-sanctum--schedule-snapshot)))))
                         :sentinel
                         (lambda (p _event)
                           (when (and (eq p herdr-sanctum--sub-process)
                                      (not (process-live-p p)))
                             (setq herdr-sanctum--sub-process nil)
                             (herdr-sanctum--schedule-reconnect)))))
                  (sub-payload
                   (concat (json-serialize
                            `((id . "emacs:sub")
                              (method . "events.subscribe")
                              (params . ((subscriptions
                                          . ,(vconcat
                                              (mapcar (lambda (typ) `((type . ,typ)))
                                                      herdr-sanctum--subscriptions)))))))
                           "\n")))
              (setq herdr-sanctum--sub-process proc)
              (process-send-string proc sub-payload)
              (herdr-sanctum-request-snapshot))
          (error (herdr-sanctum--schedule-reconnect)))))))

(defun herdr-sanctum-disconnect ()
  "Tear down the Herdr socket subscription and clear cached state."
  (when (timerp herdr-sanctum--refresh-timer)
    (cancel-timer herdr-sanctum--refresh-timer)
    (setq herdr-sanctum--refresh-timer nil))
  (when (timerp herdr-sanctum--reconnect-timer)
    (cancel-timer herdr-sanctum--reconnect-timer)
    (setq herdr-sanctum--reconnect-timer nil))
  (when (timerp herdr-sanctum--duration-timer)
    (cancel-timer herdr-sanctum--duration-timer)
    (setq herdr-sanctum--duration-timer nil))
  (clrhash herdr-sanctum--agent-timestamps)
  (herdr-sanctum--abort-snapshot)
  (when (process-live-p herdr-sanctum--sub-process)
    (delete-process herdr-sanctum--sub-process))
  (setq herdr-sanctum--sub-process nil
        herdr-sanctum--snapshot nil
        herdr-sanctum--last-focus-key nil)
  (run-hooks 'herdr-sanctum-disconnect-hook)
  (force-mode-line-update t))

;;;###autoload
(defun herdr-sanctum-refresh-state ()
  "Manually refresh Sanctum working directory and agent status from Herdr."
  (interactive)
  (if (process-live-p herdr-sanctum--sub-process)
      (herdr-sanctum-request-snapshot t)
    (herdr-sanctum-ensure-connected)))

(defun herdr-sanctum-rpc-call (method params &optional callback)
  "Send one asynchronous JSON-RPC request METHOD with PARAMS to `herdr.sock'.
CALLBACK, when non-nil, is called as `(CALLBACK RESULT ERROR)' where ERROR is
the error alist on failure or nil on success."
  (let ((sock (herdr-sanctum-socket-path)))
    (if (not (file-exists-p sock))
        (progn
          (message "Herdr socket unavailable: %s" sock)
          (when callback
            (funcall callback nil
                     '((code . "unavailable")
                       (message . "Herdr socket unavailable")))))
      (let* ((id (format "emacs:rpc:%d" (cl-incf herdr-sanctum--rpc-seq)))
             (payload (concat (json-serialize
                               `((id . ,id)
                                 (method . ,method)
                                 (params . ,(or params (make-hash-table)))))
                              "\n"))
             (done nil)
             timer
             (finish
              (lambda (proc result err)
                (unless done
                  (setq done t)
                  (when (timerp timer) (cancel-timer timer))
                  (when (process-live-p proc) (delete-process proc))
                  (when (and err (not callback))
                    (message "Herdr %s failed: %s"
                             method (or (alist-get 'message err)
                                        (alist-get 'code err)
                                        err)))
                  (when callback
                    (funcall callback result err))
                  (unless err
                    (herdr-sanctum--schedule-snapshot))))))
        (condition-case err
            (let ((proc (make-network-process
                         :name (concat "herdr-sanctum-rpc-" method)
                         :family 'local
                         :service sock
                         :coding 'utf-8-unix
                         :noquery t
                         :filter
                         (lambda (p chunk)
                           (let ((acc (concat (or (process-get p 'herdr-sanctum-buf) "") chunk)))
                             (cond
                              ((when-let* ((nl (string-search "\n" acc)))
                                 (let* ((line (substring acc 0 nl))
                                        (parsed (ignore-errors
                                                  (json-parse-string line
                                                                     :object-type 'alist
                                                                     :array-type 'list
                                                                     :null-object nil
                                                                     :false-object nil))))
                                   (funcall finish p
                                            (alist-get 'result parsed)
                                            (or (alist-get 'error parsed)
                                                (unless parsed
                                                  '((code . "parse_error")
                                                    (message . "Invalid JSON response"))))))
                                 t))
                              ((> (length acc) herdr-sanctum--max-line-bytes)
                               (funcall finish p nil
                                        '((code . "overflow")
                                          (message . "Response exceeded limit"))))
                              (t
                               (process-put p 'herdr-sanctum-buf acc)))))
                         :sentinel
                         (lambda (p _event)
                           (unless (process-live-p p)
                             (funcall finish p nil
                                      '((code . "closed")
                                        (message . "Socket closed"))))))))
              (setq timer (run-with-timer
                           herdr-sanctum-rpc-timeout nil
                           (lambda ()
                             (funcall finish proc nil
                                      '((code . "timeout")
                                        (message . "Request timed out"))))))
              (process-send-string proc payload))
          (error
           (let ((err-obj `((code . "connect_error")
                            (message . ,(error-message-string err)))))
             (if callback
                 (funcall callback nil err-obj)
               (message "Herdr %s failed: %s"
                        method (error-message-string err))))))))))

(provide 'herdr-sanctum-rpc)
;;; herdr-sanctum-rpc.el ends here
