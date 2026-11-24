;;; herdr-sanctum-rpc-test.el --- RPC & socket tests for herdr-sanctum -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT tests covering `herdr-sanctum-rpc': framing, event parsing, JSON-RPC
;; single-flight & `no-refresh', event stream & error recovery, agent
;; subscription sync, in-memory timestamps, dir resolution, and `refresh-state'.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'herdr-sanctum-test-helper)

(ert-deftest herdr-sanctum-rpc-test/frame-push-utf8-and-overflow ()
  "Line framer handles fragmented UTF-8 chunks and byte-accurate overflow."
  (let ((proc (make-pipe-process :name "herdr-frame-test" :buffer nil :noquery t)))
    (unwind-protect
        (let ((herdr-sanctum--max-line-bytes 12))
          ;; "你好" is 6 UTF-8 bytes; two of them = 12 bytes (fits).
          (should (equal (herdr-sanctum--frame-push proc "你") '(nil . nil)))
          (should (equal (herdr-sanctum--frame-push proc "好你好\n")
                         '(("你好你好") . nil)))
          ;; 5 Chinese chars = 15 bytes (> 12 bytes) -> overflow, tail dropped cleanly.
          (should (equal (herdr-sanctum--frame-push proc "你好你好你") '(nil . t)))
          (should (equal (herdr-sanctum--frame-push proc "尾\nok\n")
                         '(("ok") . t))))
      (when (process-live-p proc) (delete-process proc)))))

(ert-deftest herdr-sanctum-rpc-test/event-line-parsing ()
  "Event line parser classifies acks, focus events, envelopes, errors, and blanks."
  (should-not (member "pane.agent_status_changed" herdr-sanctum--subscriptions))
  (should (member "pane.updated" herdr-sanctum--subscriptions))
  (should (member "pane.agent_detected" herdr-sanctum--subscriptions))
  (should-not (herdr-sanctum--handle-event-line ""))
  (should-not (herdr-sanctum--handle-event-line "   "))
  (should (eq (herdr-sanctum--handle-event-line
               "{\"id\":\"s\",\"result\":{\"type\":\"subscription_started\"}}")
              'ack))
  (let ((err-res (herdr-sanctum--handle-event-line
                  "{\"id\":\"emacs:agent-sub\",\"error\":{\"code\":\"pane_not_found\",\"message\":\"missing\"}}")))
    (should (eq (car-safe err-res) 'error))
    (should (equal (alist-get 'code (cdr err-res)) "pane_not_found")))
  (let* ((events nil)
         (hook (lambda (kind data) (push (cons kind data) events))))
    (unwind-protect
        (progn
          (add-hook 'herdr-sanctum-event-hook hook)
          ;; Event data mentioning "subscription_started" must NOT be dropped as an ACK.
          (should (eq (herdr-sanctum--handle-event-line
                       "{\"event\":\"workspace.renamed\",\"data\":{\"type\":\"workspace_renamed\",\"label\":\"subscription_started\"}}")
                      'schedule))
          (should (equal (caar events) "workspace_renamed"))
          ;; Tab and workspace focus trigger immediate snapshot.
          (should (eq (herdr-sanctum--handle-event-line
                       "{\"event\":\"tab.focused\",\"data\":{\"type\":\"tab_focused\",\"tab_id\":\"w1:t2\"}}")
                      'immediate))
          (should (eq (herdr-sanctum--handle-event-line
                       "{\"event\":\"workspace.focused\",\"data\":{\"type\":\"workspace_focused\",\"workspace_id\":\"w2\"}}")
                      'immediate))
          ;; SubscriptionEventEnvelope without `data.type' uses top-level `event'.
          (should (eq (herdr-sanctum--handle-event-line
                       "{\"event\":\"pane.agent_status_changed\",\"data\":{\"pane_id\":\"w1:p1\",\"agent_status\":\"idle\"}}")
                      'schedule))
          (should (equal (caar events) "pane.agent_status_changed")))
      (remove-hook 'herdr-sanctum-event-hook hook))))

(ert-deftest herdr-sanctum-rpc-test/snapshot-coalescing ()
  "Concurrent snapshot requests coalesce into one follow-up fetch."
  (herdr-sanctum-test-with-clean-state
    (let ((snap-count 0)
          (pending-snaps nil))
      (herdr-sanctum-test-with-mock-server (_sock)
          (lambda (client data)
            (dolist (req (herdr-sanctum-test--parse-json-lines data))
              (when (equal (alist-get 'method req) "session.snapshot")
                (cl-incf snap-count)
                (push (cons snap-count client) pending-snaps))))
        (get-buffer-create herdr-sanctum-buffer-name)
        (herdr-sanctum-request-snapshot)
        (should (herdr-sanctum-test--wait-until (lambda () (= snap-count 1)) 1.0))
        (herdr-sanctum-request-snapshot)
        (should (eq herdr-sanctum--snap-pending 'normal))
        (herdr-sanctum-request-snapshot t)
        (should (eq herdr-sanctum--snap-pending 'force))
        (should (= snap-count 1))
        (herdr-sanctum-test--send-json
         (cdr (assoc 1 pending-snaps))
         '((id . "r1") (result . ((snapshot . ((focused_workspace_id . "w1")))))))
        (should (herdr-sanctum-test--wait-until
                 (lambda ()
                   (and (equal (alist-get 'focused_workspace_id (herdr-sanctum-snapshot)) "w1")
                        (= snap-count 2)))
                 1.5))
        (herdr-sanctum-test--send-json
         (cdr (assoc 2 pending-snaps))
         '((id . "r2") (result . ((snapshot . ((focused_workspace_id . "w2")))))))
        (should (herdr-sanctum-test--wait-until
                 (lambda ()
                   (equal (alist-get 'focused_workspace_id (herdr-sanctum-snapshot)) "w2"))
                 1.5))))))

(ert-deftest herdr-sanctum-rpc-test/rpc-call-no-refresh ()
  "`herdr-sanctum-rpc-call' honors the NO-REFRESH argument."
  (herdr-sanctum-test-with-clean-state
    (herdr-sanctum-test-with-mock-server (_sock)
        (lambda (client data)
          (dolist (req (herdr-sanctum-test--parse-json-lines data))
            (herdr-sanctum-test--send-json
             client `((id . ,(alist-get 'id req)) (result . ((ok . t)))))))
      (get-buffer-create herdr-sanctum-buffer-name)
      (let ((done nil))
        (herdr-sanctum-rpc-call "tab.create" nil (lambda (_r err) (should-not err) (setq done t)) t)
        (should (herdr-sanctum-test--wait-until (lambda () done) 1.0))
        (should-not (timerp herdr-sanctum--refresh-timer)))
      (let ((done nil))
        (herdr-sanctum-rpc-call "tab.create" nil (lambda (_r err) (should-not err) (setq done t)) nil)
        (should (herdr-sanctum-test--wait-until (lambda () done) 1.0))
        (should (timerp herdr-sanctum--refresh-timer))
        (cancel-timer herdr-sanctum--refresh-timer)
        (setq herdr-sanctum--refresh-timer nil)))))

(ert-deftest herdr-sanctum-rpc-test/rpc-call-error-callbacks-once ()
  "RPC error callbacks fire strictly once across timeout, close, parse, overflow, and missing socket."
  (herdr-sanctum-test-with-clean-state
    (let ((mode 'stall))
      (herdr-sanctum-test-with-mock-server (_sock)
          (lambda (client data)
            (dolist (_req (herdr-sanctum-test--parse-json-lines data))
              (pcase mode
                ('stall nil)
                ('close (delete-process client))
                ('bad-json (process-send-string client "{not-json\n"))
                ('overflow (process-send-string client (concat (make-string 128 ?x) "\n"))))))
        (let ((calls 0) (err-code nil) (herdr-sanctum-rpc-timeout 0.08))
          (setq mode 'stall)
          (herdr-sanctum-rpc-call "pane.current" nil
                                  (lambda (_r err) (cl-incf calls) (setq err-code (alist-get 'code err)))
                                  t)
          (should (herdr-sanctum-test--wait-until (lambda () (= calls 1)) 1.0))
          (should (equal err-code "timeout"))
          (sleep-for 0.05)
          (should (= calls 1)))
        (let ((calls 0) (err-code nil))
          (setq mode 'close)
          (herdr-sanctum-rpc-call "pane.current" nil
                                  (lambda (_r err) (cl-incf calls) (setq err-code (alist-get 'code err)))
                                  t)
          (should (herdr-sanctum-test--wait-until (lambda () (= calls 1)) 1.0))
          (should (equal err-code "closed"))
          (should (= calls 1)))
        (let ((calls 0) (err-code nil))
          (setq mode 'bad-json)
          (herdr-sanctum-rpc-call "pane.current" nil
                                  (lambda (_r err) (cl-incf calls) (setq err-code (alist-get 'code err)))
                                  t)
          (should (herdr-sanctum-test--wait-until (lambda () (= calls 1)) 1.0))
          (should (equal err-code "parse_error")))
        (let ((calls 0) (err-code nil) (herdr-sanctum--max-line-bytes 32))
          (setq mode 'overflow)
          (herdr-sanctum-rpc-call "pane.current" nil
                                  (lambda (_r err) (cl-incf calls) (setq err-code (alist-get 'code err)))
                                  t)
          (should (herdr-sanctum-test--wait-until (lambda () (= calls 1)) 1.0))
          (should (equal err-code "overflow")))
        (let ((herdr-sanctum-socket-path (expand-file-name "missing.sock" herdr-sanctum-test--root))
              (calls 0) (err-code nil))
          (herdr-sanctum-rpc-call "pane.current" nil
                                  (lambda (_r err) (cl-incf calls) (setq err-code (alist-get 'code err)))
                                  t)
          (should (= calls 1))
          (should (equal err-code "unavailable")))))))

(ert-deftest herdr-sanctum-rpc-test/socket-cwd-hooks-and-skip-unchanged ()
  "Socket bridge syncs `*sanctum*' CWD, fires hooks, and skips unchanged snapshots."
  (herdr-sanctum-test-with-clean-state
    (herdr-sanctum-test-with-temp-dir project-dir
      (let* ((buf (get-buffer-create herdr-sanctum-buffer-name))
             (focus-events nil)
             (status-changes nil)
             (snap-hooks 0)
             (agent-status "working")
             (seq-num 1)
             (focus-hook (lambda (dir pane ws tab)
                           (push (list dir pane ws tab) focus-events)))
             (status-hook (lambda (agent old new)
                            (push (list (alist-get 'pane_id agent) old new) status-changes)))
             (snap-hook (lambda (_s) (cl-incf snap-hooks))))
        (unwind-protect
            (herdr-sanctum-test-with-mock-server (_sock)
                (lambda (client data)
                  (dolist (req (herdr-sanctum-test--parse-json-lines data))
                    (let ((id (alist-get 'id req))
                          (method (alist-get 'method req)))
                      (cond
                       ((equal method "events.subscribe")
                        (herdr-sanctum-test--send-json
                         client `((id . ,id) (result . ((type . "subscription_started"))))))
                       ((equal method "session.snapshot")
                        (herdr-sanctum-test--send-json
                         client
                         `((id . ,id)
                           (result
                            . ((type . "session_snapshot")
                               (snapshot
                                . ((focused_workspace_id . "w1")
                                   (focused_tab_id . "w1:t1")
                                   (focused_pane_id . "w1:p2")
                                   (panes . [((pane_id . "w1:p2")
                                              (foreground_cwd . ,project-dir))])
                                   (agents . [((pane_id . "w1:p1")
                                               (agent . "pi")
                                               (agent_status . ,agent-status)
                                               (state_change_seq . ,seq-num))
                                              ((pane_id . "w1:p2")
                                               (agent . "claude")
                                               (agent_status . "done")
                                               (state_change_seq . 1))]))))))))))))
              (add-hook 'herdr-sanctum-focus-change-hook focus-hook)
              (add-hook 'herdr-sanctum-agent-status-change-hook status-hook)
              (add-hook 'herdr-sanctum-snapshot-hook snap-hook)
              (herdr-sanctum-ensure-connected)
              (should (herdr-sanctum-test--wait-until
                       (lambda ()
                         (equal (buffer-local-value 'default-directory buf)
                                (file-name-as-directory project-dir)))))
              (should (= (length focus-events) 1))
              (should (equal (nth 1 (car focus-events)) "w1:p2"))
              (should (= snap-hooks 1))
              ;; Identical snapshot with skip-if-unchanged skips snapshot hook.
              (herdr-sanctum-apply-snapshot (copy-tree (herdr-sanctum-snapshot)) t)
              (should (= snap-hooks 1))
              ;; Transition agent w1:p1 from working -> blocked.
              (setq agent-status "blocked" seq-num 2)
              (herdr-sanctum-request-snapshot t)
              (should (herdr-sanctum-test--wait-until (lambda () (consp status-changes))))
              (should (equal (car status-changes) '("w1:p1" "working" "blocked")))
              ;; Disconnect cleans up processes and timers.
              (herdr-sanctum-disconnect)
              (should-not (process-live-p herdr-sanctum--sub-process))
              (should-not (process-live-p herdr-sanctum--snap-process))
              (should-not (timerp herdr-sanctum--duration-timer)))
          (remove-hook 'herdr-sanctum-focus-change-hook focus-hook)
          (remove-hook 'herdr-sanctum-agent-status-change-hook status-hook)
          (remove-hook 'herdr-sanctum-snapshot-hook snap-hook))))))

(ert-deftest herdr-sanctum-rpc-test/agent-subscription-sync ()
  "Per-pane `pane.agent_status_changed' pushes sync status and reconcile agents."
  (herdr-sanctum-test-with-clean-state
    (let* ((main-client nil)
           (agent-client nil)
           (agent-sub-history nil)
           (events-seen nil)
           (status-changes nil)
           (agents `[((pane_id . "w1:p1") (terminal_id . "term_1")
                      (workspace_id . "w1") (tab_id . "w1:t1")
                      (agent . "pi") (agent_status . "working") (state_change_seq . 1))])
           (herdr-sanctum-min-refresh-interval 0.05)
           (event-hook (lambda (kind data) (push (cons kind data) events-seen)))
           (status-hook (lambda (agent old new)
                          (push (list (alist-get 'pane_id agent) old new) status-changes))))
      (unwind-protect
          (herdr-sanctum-test-with-mock-server (_sock)
              (lambda (client data)
                (dolist (req (herdr-sanctum-test--parse-json-lines data))
                  (let ((id (alist-get 'id req))
                        (method (alist-get 'method req)))
                    (cond
                     ((equal method "events.subscribe")
                      (if (equal id "emacs:agent-sub")
                          (let* ((subs (alist-get 'subscriptions (alist-get 'params req)))
                                 (pane-ids (mapcar (lambda (s)
                                                     (should (equal (alist-get 'type s)
                                                                    "pane.agent_status_changed"))
                                                     (alist-get 'pane_id s))
                                                   subs)))
                            (setq agent-client client)
                            (push pane-ids agent-sub-history))
                        (setq main-client client))
                      (herdr-sanctum-test--send-json
                       client `((id . ,id) (result . ((type . "subscription_started"))))))
                     ((equal method "session.snapshot")
                      (herdr-sanctum-test--send-json
                       client
                       `((id . ,id)
                         (result . ((type . "session_snapshot")
                                    (snapshot . ((focused_workspace_id . "w1")
                                                 (focused_tab_id . "w1:t1")
                                                 (focused_pane_id . "w1:p1")
                                                 (workspaces . [])
                                                 (tabs . [])
                                                 (panes . [])
                                                 (agents . ,agents))))))))))))
            (get-buffer-create herdr-sanctum-buffer-name)
            (add-hook 'herdr-sanctum-event-hook event-hook)
            (add-hook 'herdr-sanctum-agent-status-change-hook status-hook)
            (herdr-sanctum-ensure-connected)
            (should (herdr-sanctum-test--wait-until
                     (lambda ()
                       (and (process-live-p herdr-sanctum--agent-sub-process)
                            (equal herdr-sanctum--agent-sub-panes '("w1:p1"))
                            (equal (car agent-sub-history) '("w1:p1"))))))
            ;; Push status transition over per-pane subscription.
            (setq agents `[((pane_id . "w1:p1") (terminal_id . "term_1")
                            (workspace_id . "w1") (tab_id . "w1:t1")
                            (agent . "pi") (agent_status . "done") (state_change_seq . 2))])
            (herdr-sanctum-test--send-json
             agent-client
             '((event . "pane.agent_status_changed")
               (data . ((pane_id . "w1:p1") (workspace_id . "w1")
                        (agent_status . "done") (agent . "pi")))))
            (should (herdr-sanctum-test--wait-until
                     (lambda ()
                       (equal (alist-get 'agent_status
                                         (car (alist-get 'agents (herdr-sanctum-snapshot))))
                              "done"))))
            (should (assoc "pane.agent_status_changed" events-seen))
            (should (equal (car status-changes) '("w1:p1" "working" "done")))
            ;; Adding a new agent reconciles agent-sub without dropping main-client.
            (let ((orig-main herdr-sanctum--sub-process))
              (setq agents `[((pane_id . "w1:p1") (terminal_id . "term_1")
                              (workspace_id . "w1") (tab_id . "w1:t1")
                              (agent . "pi") (agent_status . "done") (state_change_seq . 2))
                             ((pane_id . "w1:p2") (terminal_id . "term_2")
                              (workspace_id . "w1") (tab_id . "w1:t1")
                              (agent . "claude") (agent_status . "working") (state_change_seq . 1))])
              (herdr-sanctum-test--send-json
               main-client
               '((event . "pane.agent_detected")
                 (data . ((type . "pane_agent_detected") (pane_id . "w1:p2")
                          (agent . "claude") (released . :false)))))
              (should (herdr-sanctum-test--wait-until
                       (lambda ()
                         (and (equal herdr-sanctum--agent-sub-panes '("w1:p1" "w1:p2"))
                              (equal (car agent-sub-history) '("w1:p1" "w1:p2"))))))
              (should (eq herdr-sanctum--sub-process orig-main)))
            ;; Empty agents closes per-pane subscription cleanly.
            (setq agents `[])
            (herdr-sanctum-request-snapshot t)
            (should (herdr-sanctum-test--wait-until
                     (lambda ()
                       (and (null (alist-get 'agents (herdr-sanctum-snapshot)))
                            (null herdr-sanctum--agent-sub-process)
                            (null herdr-sanctum--agent-sub-panes))))))
        (remove-hook 'herdr-sanctum-event-hook event-hook)
        (remove-hook 'herdr-sanctum-agent-status-change-hook status-hook)))))

(ert-deftest herdr-sanctum-rpc-test/agent-sub-retry-backoff ()
  "Agent subscription retry scheduling advances exponentially."
  (herdr-sanctum-test-with-clean-state
    (let ((delays nil))
      (cl-letf (((symbol-function 'run-with-timer)
                 (lambda (secs _rep _fn &rest _args)
                   (push secs delays)
                   'fake-timer)))
        (setq herdr-sanctum--agent-sub-retry-timer nil
              herdr-sanctum--agent-sub-failed-panes nil
              herdr-sanctum--agent-sub-retry-count 0)
        (dotimes (_ 4)
          (setq herdr-sanctum--agent-sub-retry-timer nil)
          (herdr-sanctum--schedule-agent-sub-retry '("w1:p1") 0.25))
        (setq herdr-sanctum--agent-sub-retry-timer nil))
      (should (equal (nreverse delays) '(0.25 0.5 1.0 2.0))))))

(ert-deftest herdr-sanctum-rpc-test/subscription-error-recovery ()
  "Recover from `pane_not_found' and `events_lost' errors end to end."
  (herdr-sanctum-test-with-clean-state
    (let ((main-client nil)
          (main-sub-count 0)
          (stale-pane-active t)
          (status "working"))
      (herdr-sanctum-test-with-mock-server (_sock)
          (lambda (client data)
            (dolist (req (herdr-sanctum-test--parse-json-lines data))
              (let ((id (alist-get 'id req))
                    (method (alist-get 'method req)))
                (cond
                 ((equal method "events.subscribe")
                  (if (equal id "emacs:agent-sub")
                      (let* ((subs (alist-get 'subscriptions (alist-get 'params req)))
                             (pane-ids (mapcar (lambda (s) (alist-get 'pane_id s)) subs)))
                        (if (member "w1:p9" pane-ids)
                            (progn
                              (setq stale-pane-active nil)
                              (herdr-sanctum-test--send-json
                               client '((id . "emacs:agent-sub")
                                        (error . ((code . "pane_not_found")
                                                  (message . "pane w1:p9 not found")))))
                              (delete-process client))
                          (herdr-sanctum-test--send-json
                           client '((id . "emacs:agent-sub")
                                    (result . ((type . "subscription_started")))))))
                    (cl-incf main-sub-count)
                    (setq main-client client)
                    (herdr-sanctum-test--send-json
                     client '((id . "emacs:sub")
                              (result . ((type . "subscription_started")))))))
                 ((equal method "session.snapshot")
                  (let ((agents (if stale-pane-active
                                    `[((pane_id . "w1:p1") (agent . "pi")
                                       (agent_status . ,status) (state_change_seq . 1))
                                      ((pane_id . "w1:p9") (agent . "pi")
                                       (agent_status . "working") (state_change_seq . 1))]
                                  `[((pane_id . "w1:p1") (agent . "pi")
                                     (agent_status . ,status) (state_change_seq . 2))])))
                    (herdr-sanctum-test--send-json
                     client `((id . ,id)
                              (result . ((type . "session_snapshot")
                                         (snapshot . ((workspaces . []) (tabs . [])
                                                      (panes . []) (agents . ,agents)))))))))))))
        (get-buffer-create herdr-sanctum-buffer-name)
        (herdr-sanctum-ensure-connected)
        (should (herdr-sanctum-test--wait-until
                 (lambda ()
                   (and (process-live-p herdr-sanctum--agent-sub-process)
                        (equal herdr-sanctum--agent-sub-panes '("w1:p1"))))
                 2.0))
        (setq status "idle")
        (herdr-sanctum-test--send-json
         main-client '((id . "emacs:sub")
                       (error . ((code . "events_lost") (message . "overrun")))))
        (should (herdr-sanctum-test--wait-until
                 (lambda ()
                   (and (>= main-sub-count 2)
                        (equal (alist-get 'agent_status
                                          (car (alist-get 'agents (herdr-sanctum-snapshot))))
                               "idle")))
                 2.0))))))

(ert-deftest herdr-sanctum-rpc-test/events-lost-backoff-across-acks ()
  "Repeated `events_lost' advances backoff even when reconnects receive `subscription_started'."
  (herdr-sanctum-test-with-clean-state
    (let* ((main-client nil)
           (ack-markers 0)
           (delays nil)
           (fake-offset 0.0)
           (orig-float-time (symbol-function 'float-time))
           (timer-delays (make-hash-table :test #'eq))
           (orig-rwt (symbol-function 'run-with-timer))
           (event-hook (lambda (kind _data)
                         (when (equal kind "ack_barrier")
                           (cl-incf ack-markers)))))
      (unwind-protect
          (herdr-sanctum-test-with-mock-server (_sock)
              (lambda (client data)
                (dolist (req (herdr-sanctum-test--parse-json-lines data))
                  (let ((id (alist-get 'id req))
                        (method (alist-get 'method req)))
                    (cond
                     ((equal method "events.subscribe")
                      (setq main-client client)
                      (herdr-sanctum-test--send-json
                       client `((id . ,id) (result . ((type . "subscription_started")))))
                      (herdr-sanctum-test--send-json
                       client '((event . "ack.barrier") (data . ((type . "ack_barrier"))))))
                     ((equal method "session.snapshot")
                      (herdr-sanctum-test--send-json
                       client `((id . ,id)
                                (result . ((type . "session_snapshot")
                                           (snapshot . ((workspaces . []) (tabs . [])
                                                        (panes . []) (agents . []))))))))))))
            (get-buffer-create herdr-sanctum-buffer-name)
            (add-hook 'herdr-sanctum-event-hook event-hook)
            (cl-letf (((symbol-function 'float-time)
                       (lambda (&optional time)
                         (if time
                             (funcall orig-float-time time)
                           (+ (funcall orig-float-time) fake-offset))))
                      ((symbol-function 'run-with-timer)
                       (lambda (secs repeat fn &rest args)
                         (let ((tm (apply orig-rwt secs repeat fn args)))
                           (puthash tm secs timer-delays)
                           tm))))
              (let ((herdr-sanctum-reconnect-interval 5.0))
                ;; 1. Repeated immediate `events_lost' advances to 5.0s cap and stays capped
                ;;    even when simulated backoff sleep exceeds `reconnect-interval'.
                (dotimes (i 8)
                  (herdr-sanctum-ensure-connected)
                  (should (herdr-sanctum-test--wait-until
                           (lambda () (>= ack-markers (1+ i))) 1.5))
                  (should (numberp herdr-sanctum--sub-ack-time))
                  (herdr-sanctum-test--send-json
                   main-client '((id . "emacs:sub")
                                 (error . ((code . "events_lost") (message . "overrun")))))
                  (should (herdr-sanctum-test--wait-until
                           (lambda () (timerp herdr-sanctum--reconnect-timer)) 1.5))
                  (let ((delay (gethash herdr-sanctum--reconnect-timer timer-delays)))
                    (push delay delays)
                    (should-not herdr-sanctum--sub-ack-time)
                    (cancel-timer herdr-sanctum--reconnect-timer)
                    (setq herdr-sanctum--reconnect-timer nil)
                    ;; Simulate sleeping for the scheduled backoff delay (+0.1s so 5.0s > 5.0s).
                    (cl-incf fake-offset (+ delay 0.1))))
                (should (equal (nreverse delays) '(0.25 0.5 1.0 2.0 4.0 5.0 5.0 5.0)))
                ;; 2. Staying healthy > `reconnect-interval' AFTER `subscription_started'
                ;;    resets backoff back to 0.25s on a subsequent `events_lost'.
                (herdr-sanctum-ensure-connected)
                (should (herdr-sanctum-test--wait-until (lambda () (>= ack-markers 9)) 1.5))
                (should (numberp herdr-sanctum--sub-ack-time))
                (should (= herdr-sanctum--events-lost-retry-count 8))
                (cl-incf fake-offset 6.0)
                (herdr-sanctum-test--send-json
                 main-client '((id . "emacs:sub")
                               (error . ((code . "events_lost") (message . "overrun")))))
                (should (herdr-sanctum-test--wait-until
                         (lambda () (timerp herdr-sanctum--reconnect-timer)) 1.5))
                (should (= (gethash herdr-sanctum--reconnect-timer timer-delays) 0.25))
                (should (= herdr-sanctum--events-lost-retry-count 1))
                (should-not herdr-sanctum--sub-ack-time)
                ;; 3. Clean disconnect resets both retry counter and ack timestamp.
                (herdr-sanctum-disconnect)
                (should (= herdr-sanctum--events-lost-retry-count 0))
                (should-not herdr-sanctum--sub-ack-time))))
        (remove-hook 'herdr-sanctum-event-hook event-hook)))))

(ert-deftest herdr-sanctum-rpc-test/agent-sub-events-lost-backoff ()
  "Agent-sub `events_lost' backoff advances past 8.0s to 16.0s and caps at 30.0s."
  (herdr-sanctum-test-with-clean-state
    (let* ((agent-client nil)
           (delays nil)
           (fake-offset 0.0)
           (orig-float-time (symbol-function 'float-time))
           (timer-delays (make-hash-table :test #'eq))
           (orig-rwt (symbol-function 'run-with-timer)))
      (herdr-sanctum-test-with-mock-server (_sock)
          (lambda (client data)
            (dolist (req (herdr-sanctum-test--parse-json-lines data))
              (let ((id (alist-get 'id req))
                    (method (alist-get 'method req)))
                (cond
                 ((equal method "events.subscribe")
                  (when (equal id "emacs:agent-sub")
                    (setq agent-client client))
                  (herdr-sanctum-test--send-json
                   client `((id . ,id) (result . ((type . "subscription_started"))))))
                 ((equal method "session.snapshot")
                  (herdr-sanctum-test--send-json
                   client `((id . ,id)
                            (result . ((type . "session_snapshot")
                                       (snapshot
                                        . ((workspaces . []) (tabs . []) (panes . [])
                                           (agents . [((pane_id . "w1:p1")
                                                       (agent . "pi")
                                                       (agent_status . "working")
                                                       (state_change_seq . 1))]))))))))))))
        (get-buffer-create herdr-sanctum-buffer-name)
        (cl-letf (((symbol-function 'float-time)
                   (lambda (&optional time)
                     (if time
                         (funcall orig-float-time time)
                       (+ (funcall orig-float-time) fake-offset))))
                  ((symbol-function 'run-with-timer)
                   (lambda (secs repeat fn &rest args)
                     (let ((tm (apply orig-rwt secs repeat fn args)))
                       (puthash tm secs timer-delays)
                       tm))))
          (let ((herdr-sanctum-reconnect-interval 5.0))
            (herdr-sanctum-ensure-connected)
            (dotimes (_ 9)
              (should (herdr-sanctum-test--wait-until
                       (lambda ()
                         (and agent-client
                              (process-live-p herdr-sanctum--agent-sub-process)
                              (numberp herdr-sanctum--agent-sub-ack-time)))
                       1.5))
              (herdr-sanctum-test--send-json
               agent-client '((id . "emacs:agent-sub")
                              (error . ((code . "events_lost") (message . "overrun")))))
              (should (herdr-sanctum-test--wait-until
                       (lambda () (timerp herdr-sanctum--agent-sub-retry-timer)) 1.5))
              (let ((delay (gethash herdr-sanctum--agent-sub-retry-timer timer-delays)))
                (push delay delays)
                (should-not herdr-sanctum--agent-sub-ack-time)
                (cancel-timer herdr-sanctum--agent-sub-retry-timer)
                (setq herdr-sanctum--agent-sub-retry-timer nil
                      agent-client nil)
                ;; Advance past each backoff delay (including 8s and 16s > 5s reconnect-interval).
                (cl-incf fake-offset (+ delay 0.1))
                (herdr-sanctum--sync-agent-subscription (herdr-sanctum-snapshot))))
            (should (equal (nreverse delays)
                           '(0.25 0.5 1.0 2.0 4.0 8.0 16.0 30.0 30.0)))
            ;; Staying healthy > `reconnect-interval' AFTER `subscription_started' resets to 0.25s.
            (should (herdr-sanctum-test--wait-until
                     (lambda ()
                       (and agent-client
                            (process-live-p herdr-sanctum--agent-sub-process)
                            (numberp herdr-sanctum--agent-sub-ack-time)))
                     1.5))
            (cl-incf fake-offset 6.0)
            (herdr-sanctum-test--send-json
             agent-client '((id . "emacs:agent-sub")
                            (error . ((code . "events_lost") (message . "overrun")))))
            (should (herdr-sanctum-test--wait-until
                     (lambda () (timerp herdr-sanctum--agent-sub-retry-timer)) 1.5))
            (should (= (gethash herdr-sanctum--agent-sub-retry-timer timer-delays) 0.25))
            (should-not herdr-sanctum--agent-sub-ack-time)))))))

(ert-deftest herdr-sanctum-rpc-test/manual-connect-cancels-reconnect-timer ()
  "Manual `herdr-sanctum-ensure-connected' cancels any pending reconnect timer."
  (herdr-sanctum-test-with-clean-state
    (get-buffer-create herdr-sanctum-buffer-name)
    (let ((herdr-sanctum-socket-path (expand-file-name "absent.sock" herdr-sanctum-test--root))
          (herdr-sanctum-reconnect-interval 60.0))
      (herdr-sanctum-ensure-connected))
    (let ((stale-timer herdr-sanctum--reconnect-timer))
      (should (timerp stale-timer))
      (should (memq stale-timer timer-list))
      (herdr-sanctum-test-with-mock-server (_sock)
          (lambda (client data)
            (dolist (req (herdr-sanctum-test--parse-json-lines data))
              (let ((id (alist-get 'id req))
                    (method (alist-get 'method req)))
                (cond
                 ((equal method "events.subscribe")
                  (herdr-sanctum-test--send-json
                   client `((id . ,id) (result . ((type . "subscription_started"))))))
                 ((equal method "session.snapshot")
                  (herdr-sanctum-test--send-json
                   client `((id . ,id)
                            (result . ((type . "session_snapshot")
                                       (snapshot . ((focused_workspace_id . "w1"))))))))))))
        (herdr-sanctum-ensure-connected)
        (should (herdr-sanctum-connected-p))
        (should-not herdr-sanctum--reconnect-timer)
        (should-not (memq stale-timer timer-list))))))

(ert-deftest herdr-sanctum-rpc-test/focused-pane-label-empty-fields ()
  "`herdr-sanctum-focused-pane-label' ignores empty strings on each field individually."
  (let ((snap-empty-display
         (herdr-sanctum-test-make-snapshot
          :panes '(((pane_id . "w1:p1") (workspace_id . "w1") (tab_id . "w1:t1")
                    (display_agent . "") (agent . "pi")))))
        (snap-empty-agent
         (herdr-sanctum-test-make-snapshot
          :panes '(((pane_id . "w1:p1") (workspace_id . "w1") (tab_id . "w1:t1")
                    (display_agent . "Claude") (agent . "")))))
        (snap-both-empty
         (herdr-sanctum-test-make-snapshot
          :workspaces '(((workspace_id . "w1") (label . "") (active_tab_id . "w1:t1")))
          :tabs '(((tab_id . "w1:t1") (workspace_id . "w1") (label . "")))
          :panes '(((pane_id . "w1:p1") (workspace_id . "w1") (tab_id . "w1:t1")
                    (display_agent . "") (agent . ""))))))
    (should (equal (herdr-sanctum-focused-pane-label snap-empty-display)
                   "alpha / work / pi"))
    (should (equal (herdr-sanctum-focused-pane-label snap-empty-agent)
                   "alpha / work / Claude"))
    (should (equal (herdr-sanctum-focused-pane-label snap-both-empty)
                   "w1"))))

(ert-deftest herdr-sanctum-rpc-test/agent-timestamps-and-socket-reset ()
  "In-memory agent timestamps track durations, fire on live changes, and reset on socket change."
  (herdr-sanctum-test-with-clean-state
    (should (equal (herdr-sanctum-format-elapsed 15) "<1m"))
    (should (equal (herdr-sanctum-format-elapsed 420) "7m"))
    (should (equal (herdr-sanctum-format-elapsed 7200) "2h"))
    (should (equal (herdr-sanctum-format-elapsed 172800) "2d"))
    (let* ((sock-a (expand-file-name "ts-a.sock" herdr-sanctum-test--root))
           (sock-b (expand-file-name "ts-b.sock" herdr-sanctum-test--root))
           (herdr-sanctum-socket-path sock-a)
           (changes nil)
           (hook (lambda (agent old new)
                   (push (list (alist-get 'pane_id agent) old new) changes))))
      (unwind-protect
          (progn
            (add-hook 'herdr-sanctum-agent-status-change-hook hook)
            (herdr-sanctum-apply-snapshot
             (herdr-sanctum-test-make-snapshot
              :agents '(((pane_id . "w1:p1") (terminal_id . "t1") (agent . "pi")
                         (agent_status . "working") (state_change_seq . 1))
                        ((pane_id . "w1:p2") (terminal_id . "t2") (agent . "pi")
                         (agent_status . "working") (state_change_seq . 2)))))
            (should (equal herdr-sanctum--timestamps-socket sock-a))
            (should (null changes))
            ;; Backdate timestamps by 10 minutes.
            (let ((ten-min (- (float-time) 600.0)))
              (dolist (id '("w1:p1" "w1:p2"))
                (let ((e (gethash id herdr-sanctum--agent-timestamps)))
                  (puthash id (plist-put (copy-sequence e) :ts ten-min)
                           herdr-sanctum--agent-timestamps))))
            ;; Unchanged w1:p1 keeps 10m; new terminal_id on w1:p2 resets without status hook.
            (herdr-sanctum-apply-snapshot
             (herdr-sanctum-test-make-snapshot
              :agents '(((pane_id . "w1:p1") (terminal_id . "t1") (agent . "pi")
                         (agent_status . "working") (state_change_seq . 1))
                        ((pane_id . "w1:p2") (terminal_id . "t2-new") (agent . "pi")
                         (agent_status . "working") (state_change_seq . 2)))))
            (should (equal (herdr-sanctum-agent-elapsed-string "w1:p1") "10m"))
            (should (equal (herdr-sanctum-agent-elapsed-string "w1:p2") "<1m"))
            (should (null changes))
            ;; Status change fires hook & resets elapsed; removed w1:p2 is pruned.
            (herdr-sanctum-apply-snapshot
             (herdr-sanctum-test-make-snapshot
              :agents '(((pane_id . "w1:p1") (terminal_id . "t1") (agent . "pi")
                         (agent_status . "done") (state_change_seq . 2)))))
            (should (equal (herdr-sanctum-agent-elapsed-string "w1:p1") "<1m"))
            (should (equal (herdr-sanctum-agent-elapsed-string "w1:p2") ""))
            (should (equal changes '(("w1:p1" "working" "done"))))
            ;; Switching socket resets in-memory table without firing spurious change hooks.
            (setq changes nil)
            (let ((herdr-sanctum-socket-path sock-b))
              (herdr-sanctum-apply-snapshot
               (herdr-sanctum-test-make-snapshot
                :agents '(((pane_id . "w1:p1") (terminal_id . "t1") (agent . "pi")
                           (agent_status . "blocked") (state_change_seq . 9)))))
              (should (equal herdr-sanctum--timestamps-socket sock-b))
              (should (null changes)))
            ;; Disconnect clears timestamps and socket marker.
            (herdr-sanctum-disconnect)
            (should (= (hash-table-count herdr-sanctum--agent-timestamps) 0))
            (should-not herdr-sanctum--timestamps-socket))
        (remove-hook 'herdr-sanctum-agent-status-change-hook hook)))))

(ert-deftest herdr-sanctum-rpc-test/dir-resolution-and-agent-sorting ()
  "Workspace/focused dir resolution caches cleanly; agent helpers sort deterministically."
  (herdr-sanctum-test-with-clean-state
    (herdr-sanctum-test-with-temp-dir dir-a
      (herdr-sanctum-test-with-temp-dir dir-b
        (let* ((snap (herdr-sanctum-test-make-snapshot
                      :workspaces '(((workspace_id . "w1") (label . "alpha") (active_tab_id . "w1:t1")))
                      :tabs '(((tab_id . "w1:t1") (workspace_id . "w1") (label . "main"))
                              ((tab_id . "w1:t2") (workspace_id . "w1") (label . "other")))
                      :panes `(((pane_id . "w1:p0") (workspace_id . "w1") (tab_id . "w1:t2")
                                (cwd . ,dir-b))
                               ((pane_id . "w1:p1") (workspace_id . "w1") (tab_id . "w1:t1")
                                (focused . t) (foreground_cwd . ,dir-a) (cwd . ,dir-b)))
                      :agents '(((pane_id . "p-idle") (agent_status . "idle") (state_change_seq . 99))
                                ((pane_id . "p-work") (agent_status . "working") (state_change_seq . 5))
                                ((pane_id . "p-done") (agent_status . "done") (state_change_seq . 3))
                                ((pane_id . "p-blk1") (agent_status . "blocked") (state_change_seq . 1))
                                ((pane_id . "p-blk2") (agent_status . "blocked") (state_change_seq . 7)))))
               (norm-a (file-name-as-directory (expand-file-name dir-a)))
               (stat-calls 0)
               (orig-dir-p (symbol-function 'file-directory-p)))
          (herdr-sanctum-apply-snapshot snap)
          ;; Workspace dir ranks active-tab+focused highest and caches on snapshot.
          (should (equal (herdr-sanctum-workspace-dir (car (alist-get 'workspaces snap))) norm-a))
          ;; Focused dir hits snapshot cache with 0 extra `file-directory-p' calls.
          (cl-letf (((symbol-function 'file-directory-p)
                     (lambda (p) (cl-incf stat-calls) (funcall orig-dir-p p))))
            (should (equal (herdr-sanctum-focused-dir) norm-a))
            (should (equal (herdr-sanctum-focused-dir) norm-a))
            (should (equal (herdr-sanctum-workspace-dir (car (alist-get 'workspaces snap))) norm-a))
            (should (= stat-calls 0)))
          ;; Attention agents sorted blocked (seq desc) -> done -> working, excluding idle.
          (should (equal (mapcar (lambda (a) (alist-get 'pane_id a))
                                 (herdr-sanctum-attention-agents snap))
                         '("p-blk2" "p-blk1" "p-done" "p-work")))
          ;; Grouped agents ordered blocked -> working -> done -> idle.
          (should (equal (mapcar #'car (herdr-sanctum-grouped-agents snap))
                         '("blocked" "working" "done" "idle"))))))))

(ert-deftest herdr-sanctum-rpc-test/refresh-state-guard-and-kill-disconnect ()
  "`refresh-state' refuses to leak when closed; killing last UI buffer disconnects."
  (herdr-sanctum-test-with-clean-state
    ;; 1. Disconnected + no UI buffer live -> signals user-error with zero leaked timers.
    (should-error (herdr-sanctum-refresh-state) :type 'user-error)
    (should-not (timerp herdr-sanctum--reconnect-timer))
    (should-not (timerp herdr-sanctum--duration-timer))
    (should-not (process-live-p herdr-sanctum--sub-process))
    ;; 2. Live UI buffer + socket -> refresh-state connects and fetches snapshot;
    ;;    killing the last UI buffer triggers disconnect automatically.
    (let ((snap-count 0))
      (herdr-sanctum-test-with-mock-server (_sock)
          (lambda (client data)
            (dolist (req (herdr-sanctum-test--parse-json-lines data))
              (let ((id (alist-get 'id req))
                    (method (alist-get 'method req)))
                (cond
                 ((equal method "events.subscribe")
                  (herdr-sanctum-test--send-json
                   client `((id . ,id) (result . ((type . "subscription_started"))))))
                 ((equal method "session.snapshot")
                  (cl-incf snap-count)
                  (herdr-sanctum-test--send-json
                   client `((id . ,id)
                            (result . ((type . "session_snapshot")
                                       (snapshot . ((focused_workspace_id . "w1"))))))))))))
        (let ((buf (get-buffer-create herdr-sanctum-sidebar-buffer-name)))
          (with-current-buffer buf
            (add-hook 'kill-buffer-hook #'herdr-sanctum-maybe-disconnect-on-kill nil t))
          (herdr-sanctum-refresh-state)
          (should (herdr-sanctum-test--wait-until (lambda () (= snap-count 1)) 1.5))
          (should (herdr-sanctum-connected-p))
          (should (timerp herdr-sanctum--duration-timer))
          ;; Refresh while already connected forces a fresh snapshot.
          (herdr-sanctum-refresh-state)
          (should (herdr-sanctum-test--wait-until (lambda () (= snap-count 2)) 1.5))
          ;; Killing the last UI buffer disconnects cleanly.
          (kill-buffer buf)
          (should-not (herdr-sanctum-connected-p))
          (should-not (timerp herdr-sanctum--duration-timer)))))))

(provide 'herdr-sanctum-rpc-test)
;;; herdr-sanctum-rpc-test.el ends here
