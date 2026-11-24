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

(defgroup herdr-sanctum-rpc nil
  "Local Unix socket transport and polling settings for `herdr-sanctum'."
  :group 'herdr-sanctum
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
  :group 'herdr-sanctum-rpc)

(defcustom herdr-sanctum-min-refresh-interval 0.25
  "Minimum seconds between event-triggered `session.snapshot' fetches."
  :type 'number
  :group 'herdr-sanctum-rpc)

(defcustom herdr-sanctum-reconnect-interval 5.0
  "Seconds between reconnection attempts when `herdr.sock' is unavailable."
  :type 'number
  :group 'herdr-sanctum-rpc)

(defcustom herdr-sanctum-rpc-timeout 3.0
  "Timeout in seconds for one-shot Herdr JSON-RPC requests."
  :type 'number
  :group 'herdr-sanctum-rpc)

(defcustom herdr-sanctum-duration-refresh-interval 30.0
  "Interval in seconds for refreshing relative agent elapsed durations."
  :type 'number
  :group 'herdr-sanctum-rpc)

(defvar herdr-sanctum-buffer-name "*sanctum*"
  "Name of the dedicated Herdr terminal buffer.")

(defvar herdr-sanctum-sidebar-buffer-name "*sanctum-spaces*"
  "Name of the persistent Emacs sidebar displaying Herdr spaces and agents.")

(defvar herdr-sanctum-drawer-buffer-name "*sanctum-drawer*"
  "Name of the toggleable right context drawer buffer.")


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
  "Hook run periodically while Sanctum UI is visible.
Used to refresh relative agent durations and idle Git metadata."
  :type 'hook
  :group 'herdr-sanctum)

(defcustom herdr-sanctum-disconnect-hook nil
  "Hook run when the Herdr socket bridge disconnects."
  :type 'hook
  :group 'herdr-sanctum)

(defconst herdr-sanctum--max-line-bytes (* 256 1024)
  "Maximum unterminated line length buffered on a Herdr socket.")

(defconst herdr-sanctum--agent-sub-retry-delay 1.0
  "Backoff delay in seconds before retrying a failed agent-status subscription.")

(defconst herdr-sanctum--agent-sub-max-retry-delay 30.0
  "Maximum exponential backoff delay in seconds for agent-status retries.")

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
    "pane.agent_detected")
  "Herdr event subscriptions registered on `events.subscribe'.")

(defvar herdr-sanctum--sub-process nil
  "Persistent local socket subscription to Herdr server lifecycle events.")

(defvar herdr-sanctum--agent-sub-process nil
  "Persistent local socket subscription to per-pane `pane.agent_status_changed'.")

(defvar herdr-sanctum--agent-sub-panes nil
  "Sorted list of `pane_id' strings watched by `herdr-sanctum--agent-sub-process'.")

(defvar herdr-sanctum--agent-sub-retry-timer nil
  "Backoff timer retrying a failed per-pane agent-status subscription.")

(defvar herdr-sanctum--agent-sub-failed-panes nil
  "Last sorted `pane_id' list whose agent-status subscription errored.")
(defvar herdr-sanctum--agent-sub-retry-count 0
  "Consecutive failure count for per-pane agent-status subscription backoff.")

(defvar herdr-sanctum--agent-sub-ack-time nil
  "Timestamp (`float-time') of latest agent-sub `subscription_started' ack.")

(defvar herdr-sanctum--events-lost-retry-count 0
  "Consecutive `events_lost' count for main subscription reconnect backoff.")

(defvar herdr-sanctum--sub-ack-time nil
  "Timestamp (`float-time') of latest main `subscription_started' ack.")

(defvar herdr-sanctum--focused-dir-cache nil
  "Cached `(SNAPSHOT . NORMALIZED-DIR)' pair for `herdr-sanctum-focused-dir'.")

(defvar herdr-sanctum--ws-dir-cache nil
  "Cached `(SNAPSHOT . HASH-TABLE)' mapping `workspace_id' -> normalized dir.")

(defvar herdr-sanctum--snap-process nil
  "In-flight local socket process for the current `session.snapshot' request.")

(defvar herdr-sanctum--snap-pending nil
  "Pending snapshot request queued while another snapshot is in flight.
Values are nil, `normal', or `force'.")

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
  "Map `pane_id' -> plist `(:status S :seq N :terminal-id ID :ts T)'.")

(defvar herdr-sanctum--timestamps-socket nil
  "Socket path associated with in-memory `herdr-sanctum--agent-timestamps'.")

(defvar herdr-sanctum--duration-timer nil
  "Periodic timer running `herdr-sanctum-duration-tick-hook' while UI is visible.")

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

(defun herdr-sanctum-any-ui-window-visible-p ()
  "Return non-nil when any Sanctum UI buffer is visible in a live window."
  (or (get-buffer-window herdr-sanctum-buffer-name t)
      (get-buffer-window herdr-sanctum-sidebar-buffer-name t)
      (get-buffer-window herdr-sanctum-drawer-buffer-name t)))

(defun herdr-sanctum--build-ws-dir-table (snapshot)
  "Build a hash table mapping `workspace_id' -> normalized dir for SNAPSHOT."
  (let ((active-tabs (make-hash-table :test #'equal))
        (best (make-hash-table :test #'equal))
        (dirs (make-hash-table :test #'equal)))
    (dolist (ws (alist-get 'workspaces snapshot))
      (when-let* ((ws-id (alist-get 'workspace_id ws)))
        (puthash ws-id (alist-get 'active_tab_id ws) active-tabs)))
    (dolist (pane (alist-get 'panes snapshot))
      (when-let* ((ws-id (alist-get 'workspace_id pane)))
        (let* ((active-tab (gethash ws-id active-tabs))
               (same-tab (and active-tab (equal (alist-get 'tab_id pane) active-tab)))
               (rank (cond
                      ((and same-tab (alist-get 'focused pane)) 3)
                      (same-tab 2)
                      (t 1)))
               (cur (gethash ws-id best)))
          (when (or (null cur) (> rank (car cur)))
            (puthash ws-id (cons rank pane) best)))))
    (maphash (lambda (ws-id entry)
               (let* ((pane (cdr entry))
                      (raw (and pane (or (alist-get 'foreground_cwd pane)
                                         (alist-get 'cwd pane)))))
                 (when (and (stringp raw) (file-directory-p raw))
                   (puthash ws-id
                            (file-name-as-directory (expand-file-name raw))
                            dirs))))
             best)
    dirs))

(defun herdr-sanctum-workspace-dir (ws &optional snapshot)
  "Return normalized active directory for workspace WS in SNAPSHOT, or nil."
  (let* ((snap (or snapshot herdr-sanctum--snapshot))
         (ws-id (alist-get 'workspace_id ws)))
    (when (and snap ws-id)
      (let ((tbl (cond
                  ((not (eq snap herdr-sanctum--snapshot))
                   (herdr-sanctum--build-ws-dir-table snap))
                  ((eq (car-safe herdr-sanctum--ws-dir-cache) snap)
                   (cdr herdr-sanctum--ws-dir-cache))
                  (t
                   (let ((built (herdr-sanctum--build-ws-dir-table snap)))
                     (setq herdr-sanctum--ws-dir-cache (cons snap built))
                     built)))))
        (gethash ws-id tbl)))))

(defun herdr-sanctum-focused-dir (&optional snapshot)
  "Return normalized directory of the globally focused Herdr pane, or nil."
  (let ((snap (or snapshot herdr-sanctum--snapshot)))
    (if (and snap
             (eq snap herdr-sanctum--snapshot)
             (eq (car-safe herdr-sanctum--focused-dir-cache) snap))
        (cdr herdr-sanctum--focused-dir-cache)
      (let* ((focused-id (alist-get 'focused_pane_id snap))
             (pane (and focused-id
                        (seq-find (lambda (p)
                                    (equal (alist-get 'pane_id p) focused-id))
                                  (alist-get 'panes snap))))
             (raw (and pane (or (alist-get 'foreground_cwd pane)
                                (alist-get 'cwd pane))))
             (resolved
              (cond
               ((and (stringp raw) (file-directory-p raw))
                (file-name-as-directory (expand-file-name raw)))
               ((when-let* ((buf (get-buffer herdr-sanctum-buffer-name)))
                  (with-current-buffer buf
                    (when (and default-directory (file-directory-p default-directory))
                      (file-name-as-directory (expand-file-name default-directory)))))))))
        (when (and snap (eq snap herdr-sanctum--snapshot))
          (setq herdr-sanctum--focused-dir-cache (cons snap resolved)))
        resolved))))

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
         (non-empty (lambda (s) (and (stringp s) (not (string-empty-p s)) s)))
         (ws-label (or (funcall non-empty (alist-get 'label ws))
                       (funcall non-empty ws-id)))
         (tab-label (funcall non-empty (alist-get 'label tab)))
         (agent (or (funcall non-empty (alist-get 'display_agent pane))
                    (funcall non-empty (alist-get 'agent pane)))))
    (string-join (delq nil (list ws-label tab-label agent
                                 (unless ws-label (funcall non-empty pane-id))))
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
            (ts (plist-get entry :ts)))
      (herdr-sanctum-format-elapsed (- (float-time) ts))
    ""))

(defun herdr-sanctum--record-agent-timestamps (agents)
  "Track in-memory transition times for AGENTS and notify live status changes."
  (let ((sock (herdr-sanctum-socket-path)))
    (unless (equal herdr-sanctum--timestamps-socket sock)
      (clrhash herdr-sanctum--agent-timestamps)
      (setq herdr-sanctum--timestamps-socket sock)))
  (let ((now (float-time))
        (live (make-hash-table :test #'equal))
        (changes nil))
    (dolist (agent agents)
      (when-let* ((pane-id (alist-get 'pane_id agent)))
        (puthash pane-id t live)
        (let* ((status (or (alist-get 'agent_status agent) "unknown"))
               (seq (alist-get 'state_change_seq agent))
               (term-id (alist-get 'terminal_id agent))
               (prev (gethash pane-id herdr-sanctum--agent-timestamps))
               (old-status (plist-get prev :status))
               (old-seq (plist-get prev :seq))
               (old-term-id (plist-get prev :terminal-id)))
          (unless (and prev
                       (equal old-status status)
                       (equal old-seq seq)
                       (equal old-term-id term-id))
            (puthash pane-id
                     (list :status status
                           :seq seq
                           :terminal-id term-id
                           :ts now)
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
  "Start the periodic tick timer while a Sanctum UI buffer or session is live."
  (when (and (or (herdr-sanctum-any-ui-buffer-live-p)
                 (process-live-p herdr-sanctum--sub-process))
             (not (timerp herdr-sanctum--duration-timer)))
    (setq herdr-sanctum--duration-timer
          (run-with-timer
           herdr-sanctum-duration-refresh-interval
           herdr-sanctum-duration-refresh-interval
           (lambda ()
             (when (herdr-sanctum-any-ui-window-visible-p)
               (run-hooks 'herdr-sanctum-duration-tick-hook)))))))

;;; Socket Lifecycle, Snapshot & RPC

(defun herdr-sanctum--frame-push (proc chunk)
  "Append CHUNK to PROC's line framer and return `(LINES . OVERFLOW)'.
Enforces `herdr-sanctum--max-line-bytes' using actual UTF-8 byte counts
and avoids quadratic string copying on fragmented chunks."
  (let* ((state (or (process-get proc 'herdr-sanctum-frame)
                    (vector nil 0 nil)))
         (chunks (aref state 0))
         (bytes (aref state 1))
         (dropping (aref state 2))
         (start 0)
         (lines nil)
         (overflow nil)
         nl)
    (while (setq nl (string-search "\n" chunk start))
      (if dropping
          (setq dropping nil
                chunks nil
                bytes 0
                overflow t)
        (let* ((piece (substring chunk start nl))
               (total (+ bytes (string-bytes piece))))
          (if (> total herdr-sanctum--max-line-bytes)
              (setq overflow t)
            (push (if chunks
                      (apply #'concat (nreverse (cons piece chunks)))
                    piece)
                  lines))
          (setq chunks nil
                bytes 0)))
      (setq start (1+ nl)))
    (when (< start (length chunk))
      (if dropping
          (setq overflow t)
        (let* ((rem (if (> start 0) (substring chunk start) chunk))
               (new-bytes (+ bytes (string-bytes rem))))
          (if (> new-bytes herdr-sanctum--max-line-bytes)
              (setq chunks nil
                    bytes 0
                    dropping t
                    overflow t)
            (push rem chunks)
            (setq bytes new-bytes)))))
    (aset state 0 chunks)
    (aset state 1 bytes)
    (aset state 2 dropping)
    (process-put proc 'herdr-sanctum-frame state)
    (cons (nreverse lines) overflow)))

(defun herdr-sanctum--parse-json-line (line)
  "Parse JSON LINE into an alist, returning nil on syntax errors."
  (ignore-errors
    (json-parse-string line
                       :object-type 'alist
                       :array-type 'list
                       :null-object nil
                       :false-object nil)))

(defun herdr-sanctum--handle-event-line (line)
  "Handle one Herdr subscription event LINE.
Return `ack' for `subscription_started' acknowledgments, `immediate' for
tab/workspace focus changes, `schedule' for other events, `(error . ERR)'
for JSON-RPC error responses, or nil for blank/unrecognized lines."
  (unless (string-blank-p line)
    (when-let* ((parsed (herdr-sanctum--parse-json-line line)))
      (let ((err (alist-get 'error parsed))
            (result (alist-get 'result parsed)))
        (cond
         (err (cons 'error err))
         ((and result (equal (alist-get 'type result) "subscription_started"))
          'ack)
         (t
          (let* ((data (alist-get 'data parsed))
                 (kind (or (alist-get 'type data)
                           (alist-get 'event parsed))))
            (when (stringp kind)
              (run-hook-with-args 'herdr-sanctum-event-hook kind data)
              (pcase kind
                ((or "tab_focused" "workspace_focused") 'immediate)
                (_ 'schedule))))))))))

(defun herdr-sanctum--consume-event-chunk (proc chunk &optional on-error on-ack)
  "Buffer CHUNK on subscription PROC, dispatch complete lines, and refresh.
When an error response is decoded, call ON-ERROR with `(PROC ERR)' instead
of scheduling a snapshot.  When a `subscription_started' acknowledgment is
seen, call ON-ACK with no arguments."
  (let* ((framed (herdr-sanctum--frame-push proc chunk))
         (lines (car framed))
         (overflow (cdr framed))
         (refresh nil)
         (err-obj nil))
    (while (and lines (null err-obj))
      (let ((res (herdr-sanctum--handle-event-line (pop lines))))
        (cond
         ((eq res 'ack)
          (when (functionp on-ack) (funcall on-ack)))
         ((eq (car-safe res) 'error)
          (setq err-obj (cdr res)))
         ((eq res 'immediate)
          (setq refresh 'immediate))
         ((and res (null refresh))
          (setq refresh 'schedule)))))
    (when (and overflow (null err-obj) (null refresh))
      (setq refresh 'schedule))
    (cond
     (err-obj
      (when (functionp on-error)
        (funcall on-error proc err-obj)))
     ((eq refresh 'immediate)
      (herdr-sanctum-request-snapshot))
     ((eq refresh 'schedule)
      (herdr-sanctum-schedule-snapshot)))))

(defun herdr-sanctum--agent-pane-ids (snapshot)
  "Return sorted unique `pane_id' strings for agents in SNAPSHOT."
  (let ((ids nil))
    (dolist (agent (alist-get 'agents snapshot))
      (when-let* ((pane-id (alist-get 'pane_id agent)))
        (when (and (stringp pane-id) (not (member pane-id ids)))
          (push pane-id ids))))
    (sort ids #'string<)))

(cl-defun herdr-sanctum--open-subscription
    (&key name sock id subscriptions proc-var on-error on-ack on-close)
  "Open a local Herdr `events.subscribe' socket and store it in PROC-VAR.
NAME is the process name, SOCK is the Unix socket path, ID is the JSON-RPC
request id, and SUBSCRIPTIONS is the vector of subscription alists.
ON-ERROR, ON-ACK, and ON-CLOSE are optional callbacks.
Return the live network process on success, or nil on connection error."
  (let ((proc nil))
    (condition-case nil
        (let ((payload (concat (json-serialize
                                `((id . ,id)
                                  (method . "events.subscribe")
                                  (params . ((subscriptions . ,subscriptions)))))
                               "\n")))
          (setq proc
                (make-network-process
                 :name name
                 :family 'local
                 :service sock
                 :coding 'utf-8-unix
                 :noquery t
                 :filter
                 (lambda (p chunk)
                   (when (eq p (symbol-value proc-var))
                     (herdr-sanctum--consume-event-chunk
                      p chunk on-error on-ack)))
                 :sentinel
                 (lambda (p _event)
                   (when (and (eq p (symbol-value proc-var))
                              (not (process-live-p p)))
                     (set proc-var nil)
                     (when (functionp on-close)
                       (funcall on-close p))))))
          (set proc-var proc)
          (process-send-string proc payload)
          proc)
      (error
       (when (and proc (eq (symbol-value proc-var) proc))
         (set proc-var nil))
       (when (and (processp proc) (process-live-p proc))
         (delete-process proc))
       nil))))

(defun herdr-sanctum--close-agent-subscription (&optional keep-retry)
  "Close the per-pane agent-status subscription socket.
When KEEP-RETRY is nil, also cancel any pending retry timer and backoff state."
  (unless keep-retry
    (when (timerp herdr-sanctum--agent-sub-retry-timer)
      (cancel-timer herdr-sanctum--agent-sub-retry-timer)
      (setq herdr-sanctum--agent-sub-retry-timer nil))
    (setq herdr-sanctum--agent-sub-failed-panes nil
          herdr-sanctum--agent-sub-retry-count 0))
  (setq herdr-sanctum--agent-sub-ack-time nil)
  (let ((proc herdr-sanctum--agent-sub-process))
    (setq herdr-sanctum--agent-sub-process nil
          herdr-sanctum--agent-sub-panes nil)
    (when (process-live-p proc)
      (delete-process proc))))

(defun herdr-sanctum--schedule-agent-sub-retry (failed-panes &optional base-delay)
  "Schedule a snapshot refresh with exponential backoff after FAILED-PANES failed."
  (let* ((same-panes (equal failed-panes herdr-sanctum--agent-sub-failed-panes))
         (attempt (if same-panes herdr-sanctum--agent-sub-retry-count 0))
         (base (or base-delay herdr-sanctum--agent-sub-retry-delay))
         (delay (min herdr-sanctum--agent-sub-max-retry-delay
                     (* base (expt 2 (min attempt 8))))))
    (setq herdr-sanctum--agent-sub-failed-panes failed-panes
          herdr-sanctum--agent-sub-retry-count (1+ attempt))
    (unless (timerp herdr-sanctum--agent-sub-retry-timer)
      (setq herdr-sanctum--agent-sub-retry-timer
            (run-with-timer
             delay nil
             (lambda ()
               (setq herdr-sanctum--agent-sub-retry-timer nil)
               (when (process-live-p herdr-sanctum--sub-process)
                 (herdr-sanctum-request-snapshot))))))))

(defun herdr-sanctum--sync-agent-subscription (snapshot)
  "Reconcile per-pane `pane.agent_status_changed' subscription with SNAPSHOT."
  (let ((pane-ids (herdr-sanctum--agent-pane-ids snapshot)))
    (cond
     ((or (null pane-ids)
          (not (process-live-p herdr-sanctum--sub-process)))
      (herdr-sanctum--close-agent-subscription))
     ((and (process-live-p herdr-sanctum--agent-sub-process)
           (equal pane-ids herdr-sanctum--agent-sub-panes))
      nil)
     ((and (timerp herdr-sanctum--agent-sub-retry-timer)
           (equal pane-ids herdr-sanctum--agent-sub-failed-panes))
      nil)
     (t
      (let ((sock (herdr-sanctum-socket-path)))
        (if (not (file-exists-p sock))
            (herdr-sanctum--close-agent-subscription)
          (let ((keep-backoff (equal pane-ids herdr-sanctum--agent-sub-failed-panes)))
            (herdr-sanctum--close-agent-subscription keep-backoff))
          (if (herdr-sanctum--open-subscription
               :name "herdr-sanctum-agent-events"
               :sock sock
               :id "emacs:agent-sub"
               :subscriptions (vconcat
                               (mapcar (lambda (pane-id)
                                         `((type . "pane.agent_status_changed")
                                           (pane_id . ,pane-id)))
                                       pane-ids))
               :proc-var 'herdr-sanctum--agent-sub-process
               :on-error
               (lambda (_proc err)
                 (let ((code (alist-get 'code err))
                       (ack-time herdr-sanctum--agent-sub-ack-time))
                   (when (and (equal code "events_lost")
                              ack-time
                              (> (- (float-time) ack-time)
                                 herdr-sanctum-reconnect-interval))
                     (setq herdr-sanctum--agent-sub-failed-panes nil
                           herdr-sanctum--agent-sub-retry-count 0))
                   (setq herdr-sanctum--agent-sub-ack-time nil)
                   (herdr-sanctum--close-agent-subscription t)
                   (herdr-sanctum--schedule-agent-sub-retry
                    pane-ids
                    (when (member code '("pane_not_found" "events_lost"))
                      0.25))))
               :on-ack
               (lambda ()
                 (setq herdr-sanctum--agent-sub-ack-time (float-time)))
               :on-close
               (lambda (_p)
                 (when (and herdr-sanctum--agent-sub-ack-time
                            (> (- (float-time) herdr-sanctum--agent-sub-ack-time)
                               herdr-sanctum-reconnect-interval))
                   (setq herdr-sanctum--agent-sub-failed-panes nil
                         herdr-sanctum--agent-sub-retry-count 0))
                 (setq herdr-sanctum--agent-sub-panes nil
                       herdr-sanctum--agent-sub-ack-time nil)
                 (when (process-live-p herdr-sanctum--sub-process)
                   (herdr-sanctum--schedule-agent-sub-retry pane-ids))))
              (setq herdr-sanctum--agent-sub-panes pane-ids)
            (herdr-sanctum--schedule-agent-sub-retry pane-ids))))))))

(defun herdr-sanctum-apply-snapshot (snapshot &optional skip-if-unchanged)
  "Store SNAPSHOT, sync `*sanctum*' CWD, and dispatch update hooks.
When SKIP-IF-UNCHANGED is non-nil and SNAPSHOT is `equal' to the current
snapshot, skip redundant UI re-render hooks while keeping subscriptions
and timers reconciled."
  (if (and skip-if-unchanged
           herdr-sanctum--snapshot
           (equal snapshot herdr-sanctum--snapshot))
      (progn
        (herdr-sanctum--sync-agent-subscription herdr-sanctum--snapshot)
        (herdr-sanctum--ensure-duration-timer))
    (setq herdr-sanctum--snapshot snapshot
          herdr-sanctum--focused-dir-cache nil
          herdr-sanctum--ws-dir-cache nil)
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
    (herdr-sanctum--sync-agent-subscription snapshot)
    (herdr-sanctum--ensure-duration-timer)
    (run-hook-with-args 'herdr-sanctum-snapshot-hook snapshot)
    (force-mode-line-update t)))

(defun herdr-sanctum--rpc-request (method params callback &optional name)
  "Send one asynchronous JSON-RPC request METHOD with PARAMS over `herdr.sock'.
CALLBACK, when non-nil, is called strictly once as `(CALLBACK RESULT ERROR)',
where ERROR is an error alist on failure or nil on success.
Optional NAME overrides the process name (default `herdr-sanctum-rpc-METHOD').
Return the live network process, or nil if the request failed immediately."
  (let ((sock (herdr-sanctum-socket-path)))
    (if (not (file-exists-p sock))
        (progn
          (when callback
            (funcall callback nil
                     '((code . "unavailable")
                       (message . "Herdr socket unavailable"))))
          nil)
      (let* ((id (format "emacs:rpc:%d" (cl-incf herdr-sanctum--rpc-seq)))
             (payload (concat (json-serialize
                               `((id . ,id)
                                 (method . ,method)
                                 (params . ,(or params (make-hash-table)))))
                              "\n"))
             (done nil)
             (proc nil)
             (timer nil)
             (finish
              (lambda (p result err)
                (unless done
                  (setq done t)
                  (when (timerp timer)
                    (cancel-timer timer)
                    (setq timer nil))
                  (when (processp p)
                    (process-put p 'herdr-sanctum-rpc-timer nil))
                  (when (process-live-p p)
                    (delete-process p))
                  (when callback
                    (funcall callback result err))))))
        (condition-case err
            (progn
              (setq proc
                    (make-network-process
                     :name (or name (concat "herdr-sanctum-rpc-" method))
                     :family 'local
                     :service sock
                     :coding 'utf-8-unix
                     :noquery t
                     :filter
                     (lambda (p chunk)
                       (let* ((framed (herdr-sanctum--frame-push p chunk))
                              (lines (car framed))
                              (overflow (cdr framed)))
                         (cond
                          (lines
                           (let ((parsed (herdr-sanctum--parse-json-line (car lines))))
                             (funcall finish p
                                      (alist-get 'result parsed)
                                      (or (alist-get 'error parsed)
                                          (unless parsed
                                            '((code . "parse_error")
                                              (message . "Invalid JSON response")))))))
                          (overflow
                           (funcall finish p nil
                                    '((code . "overflow")
                                      (message . "Response exceeded limit")))))))
                     :sentinel
                     (lambda (p _event)
                       (unless (process-live-p p)
                         (funcall finish p nil
                                  '((code . "closed")
                                    (message . "Socket closed")))))))
              (setq timer (run-with-timer
                           herdr-sanctum-rpc-timeout nil
                           (lambda ()
                             (funcall finish proc nil
                                      '((code . "timeout")
                                        (message . "Request timed out"))))))
              (process-put proc 'herdr-sanctum-rpc-timer timer)
              (process-send-string proc payload)
              (and (process-live-p proc) proc))
          (error
           (funcall finish proc nil
                    `((code . "connect_error")
                      (message . ,(error-message-string err))))
           nil))))))

(defun herdr-sanctum--abort-snapshot (&optional clear-pending)
  "Close any in-flight Herdr snapshot socket and cancel its timeout.
When CLEAR-PENDING is non-nil, also drop any queued follow-up snapshot."
  (when clear-pending
    (setq herdr-sanctum--snap-pending nil))
  (let ((proc herdr-sanctum--snap-process))
    (setq herdr-sanctum--snap-process nil)
    (when (processp proc)
      (when-let* ((timer (process-get proc 'herdr-sanctum-rpc-timer)))
        (when (timerp timer)
          (cancel-timer timer))
        (process-put proc 'herdr-sanctum-rpc-timer nil))
      (when (process-live-p proc)
        (delete-process proc)))))

(defun herdr-sanctum--drain-pending-snapshot ()
  "Start a follow-up snapshot if one was queued while a request was in flight."
  (when-let* ((pending herdr-sanctum--snap-pending))
    (setq herdr-sanctum--snap-pending nil)
    (when (or (eq pending 'force)
              (herdr-sanctum-any-ui-buffer-live-p))
      (herdr-sanctum-request-snapshot (eq pending 'force)))))

(defun herdr-sanctum-request-snapshot (&optional force)
  "Fetch a fresh Herdr session snapshot asynchronously over `herdr.sock'.
When FORCE is non-nil, query even if no Sanctum UI buffer is open yet.
If another snapshot request is already in flight, coalesce into a follow-up
fetch instead of aborting the in-flight request."
  (when (timerp herdr-sanctum--refresh-timer)
    (cancel-timer herdr-sanctum--refresh-timer)
    (setq herdr-sanctum--refresh-timer nil))
  (cond
   ((process-live-p herdr-sanctum--snap-process)
    (when (or force (herdr-sanctum-any-ui-buffer-live-p))
      (setq herdr-sanctum--snap-pending
            (if (or force (eq herdr-sanctum--snap-pending 'force))
                'force
              'normal))))
   ((or force (herdr-sanctum-any-ui-buffer-live-p))
    (let ((req-proc nil))
      (setq req-proc
            (herdr-sanctum--rpc-request
             "session.snapshot" nil
             (lambda (result err)
               (when (and req-proc (eq req-proc herdr-sanctum--snap-process))
                 (setq herdr-sanctum--snap-process nil)
                 (let ((overflow (equal (alist-get 'code err) "overflow")))
                   (cond
                    (overflow
                     (setq herdr-sanctum--snap-pending nil)
                     (message "Herdr snapshot exceeded %d-byte limit"
                              herdr-sanctum--max-line-bytes))
                    ((and (not err)
                          (or force (herdr-sanctum-any-ui-buffer-live-p)))
                     (when-let* ((snapshot (alist-get 'snapshot result)))
                       (herdr-sanctum-apply-snapshot snapshot (not force)))))
                   (unless overflow
                     (herdr-sanctum--drain-pending-snapshot)))))
             "herdr-sanctum-snapshot"))
      (when (process-live-p req-proc)
        (setq herdr-sanctum--snap-process req-proc
              herdr-sanctum--last-snap-time (float-time)))))))

(defun herdr-sanctum-schedule-snapshot ()
  "Throttle bursts of Herdr events into bounded local snapshot queries."
  (when (and (herdr-sanctum-any-ui-buffer-live-p)
             (not (timerp herdr-sanctum--refresh-timer)))
    (let* ((elapsed (- (float-time) herdr-sanctum--last-snap-time))
           (delay (max 0.08 (- herdr-sanctum-min-refresh-interval elapsed))))
      (setq herdr-sanctum--refresh-timer
            (run-with-timer delay nil #'herdr-sanctum-request-snapshot)))))

(defun herdr-sanctum--schedule-reconnect (&optional delay)
  "Retry connecting to `herdr.sock' after DELAY while Sanctum buffers are live."
  (when (and (herdr-sanctum-any-ui-buffer-live-p)
             (not (timerp herdr-sanctum--reconnect-timer)))
    (setq herdr-sanctum--reconnect-timer
          (run-with-timer (or delay herdr-sanctum-reconnect-interval) nil
                          (lambda ()
                            (setq herdr-sanctum--reconnect-timer nil)
                            (when (herdr-sanctum-any-ui-buffer-live-p)
                              (herdr-sanctum-ensure-connected)))))))

(defun herdr-sanctum-ensure-connected (&optional force)
  "Connect to `herdr.sock' for focus and agent events when not already connected.
Optional FORCE is passed through to `herdr-sanctum-request-snapshot'."
  (unless (process-live-p herdr-sanctum--sub-process)
    (let ((sock (herdr-sanctum-socket-path)))
      (if (not (file-exists-p sock))
          (herdr-sanctum--schedule-reconnect)
        (progn
          (setq herdr-sanctum--sub-ack-time nil)
          (if (herdr-sanctum--open-subscription
               :name "herdr-sanctum-events"
               :sock sock
               :id "emacs:sub"
               :subscriptions (vconcat
                               (mapcar (lambda (typ) `((type . ,typ)))
                                       herdr-sanctum--subscriptions))
               :proc-var 'herdr-sanctum--sub-process
               :on-error
               (lambda (proc err)
                 (let* ((fast (equal (alist-get 'code err) "events_lost"))
                        (ack-time herdr-sanctum--sub-ack-time)
                        (delay nil))
                   (setq herdr-sanctum--sub-ack-time nil)
                   (when fast
                     (when (and ack-time
                                (> (- (float-time) ack-time)
                                   herdr-sanctum-reconnect-interval))
                       (setq herdr-sanctum--events-lost-retry-count 0))
                     (setq delay (min herdr-sanctum-reconnect-interval
                                      (* 0.25 (expt 2 (min herdr-sanctum--events-lost-retry-count 5)))))
                     (cl-incf herdr-sanctum--events-lost-retry-count))
                   (setq herdr-sanctum--sub-process nil)
                   (herdr-sanctum--close-agent-subscription)
                   (when (process-live-p proc)
                     (delete-process proc))
                   (herdr-sanctum--schedule-reconnect delay)))
               :on-ack
               (lambda ()
                 (setq herdr-sanctum--sub-ack-time (float-time)))
               :on-close
               (lambda (_p)
                 (when (and herdr-sanctum--sub-ack-time
                            (> (- (float-time) herdr-sanctum--sub-ack-time)
                               herdr-sanctum-reconnect-interval))
                   (setq herdr-sanctum--events-lost-retry-count 0))
                 (setq herdr-sanctum--sub-ack-time nil)
                 (herdr-sanctum--close-agent-subscription)
                 (herdr-sanctum--schedule-reconnect)))
            (progn
              (when (timerp herdr-sanctum--reconnect-timer)
                (cancel-timer herdr-sanctum--reconnect-timer)
                (setq herdr-sanctum--reconnect-timer nil))
              (herdr-sanctum--ensure-duration-timer)
              (herdr-sanctum-request-snapshot force))
            (herdr-sanctum--schedule-reconnect)))))))

(defun herdr-sanctum-disconnect ()
  "Tear down Herdr socket subscriptions and clear in-memory cached state."
  (when (timerp herdr-sanctum--refresh-timer)
    (cancel-timer herdr-sanctum--refresh-timer)
    (setq herdr-sanctum--refresh-timer nil))
  (when (timerp herdr-sanctum--reconnect-timer)
    (cancel-timer herdr-sanctum--reconnect-timer)
    (setq herdr-sanctum--reconnect-timer nil))
  (when (timerp herdr-sanctum--duration-timer)
    (cancel-timer herdr-sanctum--duration-timer)
    (setq herdr-sanctum--duration-timer nil))
  (herdr-sanctum--close-agent-subscription)
  (clrhash herdr-sanctum--agent-timestamps)
  (setq herdr-sanctum--timestamps-socket nil)
  (herdr-sanctum--abort-snapshot t)
  (let ((proc herdr-sanctum--sub-process))
    (setq herdr-sanctum--sub-process nil)
    (when (process-live-p proc)
      (delete-process proc)))
  (setq herdr-sanctum--snapshot nil
        herdr-sanctum--focused-dir-cache nil
        herdr-sanctum--ws-dir-cache nil
        herdr-sanctum--events-lost-retry-count 0
        herdr-sanctum--sub-ack-time nil
        herdr-sanctum--last-focus-key nil)
  (run-hooks 'herdr-sanctum-disconnect-hook)
  (force-mode-line-update t))

(defun herdr-sanctum-maybe-disconnect-on-kill ()
  "Disconnect from Herdr when the last Sanctum UI buffer is killed."
  (when (or (process-live-p herdr-sanctum--sub-process)
            (process-live-p herdr-sanctum--agent-sub-process)
            (timerp herdr-sanctum--reconnect-timer)
            (timerp herdr-sanctum--duration-timer))
    (let ((dying (current-buffer)))
      (unless (seq-some (lambda (name)
                          (when-let* ((b (get-buffer name)))
                            (not (eq b dying))))
                        (herdr-sanctum-ui-buffers))
        (herdr-sanctum-disconnect)))))

;;;###autoload
(defun herdr-sanctum-refresh-state ()
  "Manually refresh Sanctum working directory and agent status from Herdr."
  (interactive)
  (cond
   ((process-live-p herdr-sanctum--sub-process)
    (herdr-sanctum-request-snapshot t))
   ((herdr-sanctum-any-ui-buffer-live-p)
    (herdr-sanctum-ensure-connected t))
   (t
    (user-error "Sanctum is not open; run `herdr-sanctum' first"))))

(defun herdr-sanctum-rpc-call (method &optional params callback no-refresh)
  "Send one asynchronous JSON-RPC request METHOD with PARAMS to `herdr.sock'.
CALLBACK, when non-nil, is called as `(CALLBACK RESULT ERROR)' where ERROR is
the error alist on failure or nil on success.
Unless NO-REFRESH is non-nil, a successful call schedules a throttled
`session.snapshot' refresh."
  (herdr-sanctum--rpc-request
   method params
   (lambda (result err)
     (when err
       (cond
        ((equal (alist-get 'code err) "unavailable")
         (message "Herdr socket unavailable: %s" (herdr-sanctum-socket-path)))
        ((not callback)
         (message "Herdr %s failed: %s"
                  method (or (alist-get 'message err)
                             (alist-get 'code err)
                             err)))))
     (when callback
       (funcall callback result err))
     (when (and (not err) (not no-refresh))
       (herdr-sanctum-schedule-snapshot)))))

(provide 'herdr-sanctum-rpc)
;;; herdr-sanctum-rpc.el ends here
