;;; herdr-sanctum-test-helper.el --- Shared ERT test utilities -*- lexical-binding: t; -*-

;;; Commentary:

;; Shared fixtures, process/socket lifecycle guards, and state reset helpers
;; for the `herdr-sanctum' ERT test suite.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'tab-bar)
(require 'herdr-sanctum)
(require 'herdr-sanctum-ime)
(require 'herdr-sanctum-tab-routing)

(when (boundp 'native-comp-enable-subr-trampolines)
  (setq native-comp-enable-subr-trampolines nil))

(defvar-local herdr-sanctum-test--live nil
  "Buffer-local liveness flag for mock terminal backends.")

(defvar herdr-sanctum-test--root
  (let ((temporary-file-directory "/tmp/"))
    (make-temp-file "hs-" t))
  "Short temporary root directory for Unix sockets and test fixtures.")

(defvar herdr-sanctum-test--seq 0
  "Monotonic counter for unique temporary socket filenames.")

(add-hook 'kill-emacs-hook
          (lambda ()
            (when (file-directory-p herdr-sanctum-test--root)
              (ignore-errors (delete-directory herdr-sanctum-test--root t)))))

(defun herdr-sanctum-test--wait-until (pred &optional timeout)
  "Pump process output until PRED returns non-nil or TIMEOUT seconds elapse."
  (let ((deadline (+ (float-time) (or timeout 2.0))))
    (while (and (< (float-time) deadline) (not (funcall pred)))
      (accept-process-output nil 0.02))
    (funcall pred)))

(defun herdr-sanctum-test--init-git-repo (dir file content &optional branch)
  "Initialize a Git repo at DIR with initial committed FILE containing CONTENT.
Return the absolute path to FILE."
  (make-directory dir t)
  (should (zerop (call-process "git" nil nil nil "-C" dir "init" "-b" (or branch "main"))))
  (let ((abs (expand-file-name file dir)))
    (make-directory (file-name-directory abs) t)
    (with-temp-file abs (insert content))
    (should (zerop (call-process "git" nil nil nil "-C" dir "add" file)))
    (should (zerop (call-process "git" nil nil nil
                                 "-c" "user.name=Test"
                                 "-c" "user.email=test@example.com"
                                 "-C" dir "commit" "-m" "init")))
    abs))

(defun herdr-sanctum-test--parse-json-lines (data)
  "Parse newline-delimited JSON objects in DATA into a list of alists."
  (delq nil
        (mapcar (lambda (line)
                  (ignore-errors
                    (json-parse-string line
                                       :object-type 'alist
                                       :array-type 'list
                                       :null-object nil
                                       :false-object nil)))
                (split-string data "\n" t))))

(defun herdr-sanctum-test--send-json (client obj)
  "Serialize OBJ as a newline-terminated JSON object to CLIENT."
  (when (process-live-p client)
    (process-send-string client (concat (json-serialize obj) "\n"))))

(defun herdr-sanctum-test--put-git-cache (dir info &optional meta)
  "Store pure Git INFO and optional META for DIR directly in cache.
Populates `herdr-sanctum-git--cache' without running update hooks."
  (let* ((norm (file-name-as-directory (expand-file-name dir)))
         (full-meta (if (plist-get meta :updated-at)
                        meta
                      (append (list :updated-at (float-time)) meta))))
    (puthash norm (cons info full-meta) herdr-sanctum-git--cache)
    info))

(cl-defun herdr-sanctum-test-make-snapshot
    (&key (focused-ws "w1")
          (focused-tab "w1:t1")
          (focused-pane "w1:p1")
          (workspaces nil workspaces-p)
          (tabs nil tabs-p)
          (panes nil panes-p)
          (agents nil)
          dir)
  "Build a Herdr snapshot alist with sensible defaults.
WORKSPACES, TABS, PANES, and AGENTS default to a single workspace/tab/pane
unless explicitly supplied.  When DIR is non-nil and PANES is not supplied,
the default pane uses DIR as `foreground_cwd'."
  `((focused_workspace_id . ,focused-ws)
    (focused_tab_id . ,focused-tab)
    (focused_pane_id . ,focused-pane)
    (workspaces . ,(if workspaces-p
                       workspaces
                     (when focused-ws
                       `(((workspace_id . ,focused-ws)
                          (number . 1)
                          (label . "alpha")
                          (tab_count . 1)
                          (active_tab_id . ,focused-tab))))))
    (tabs . ,(if tabs-p
                 tabs
               (when focused-tab
                 `(((tab_id . ,focused-tab)
                    (workspace_id . ,focused-ws)
                    (number . 1)
                    (label . "work")
                    (pane_count . 1))))))
    (panes . ,(if panes-p
                  panes
                (when focused-pane
                  `(((pane_id . ,focused-pane)
                     (workspace_id . ,focused-ws)
                     (tab_id . ,focused-tab)
                     (focused . t)
                     ,@(when dir `((foreground_cwd . ,dir))))))))
    (agents . ,agents)))

(defun herdr-sanctum-test--reset-state ()
  "Reset all Sanctum global state, UI buffers, timers, caches, and modes."
  (when (bound-and-true-p herdr-sanctum-tab-routing-mode)
    (herdr-sanctum-tab-routing-mode -1))
  (when (bound-and-true-p herdr-sanctum-ime-mode)
    (herdr-sanctum-ime-mode -1))
  (herdr-sanctum-disconnect)
  (herdr-sanctum-git-clear)
  (when (timerp herdr-sanctum-drawer--git-refresh-timer)
    (cancel-timer herdr-sanctum-drawer--git-refresh-timer)
    (setq herdr-sanctum-drawer--git-refresh-timer nil))
  (when (timerp herdr-sanctum-sidebar--git-refresh-timer)
    (cancel-timer herdr-sanctum-sidebar--git-refresh-timer)
    (setq herdr-sanctum-sidebar--git-refresh-timer nil))
  (when-let* ((drawer-buf (get-buffer herdr-sanctum-drawer-buffer-name)))
    (with-current-buffer drawer-buf
      (setq herdr-sanctum-drawer--files-cache nil
            herdr-sanctum-drawer--render-key nil
            herdr-sanctum-drawer--active-keymap nil)))
  (dolist (name (list herdr-sanctum-buffer-name
                      herdr-sanctum-sidebar-buffer-name
                      herdr-sanctum-drawer-buffer-name
                      "*herdr-sanctum-doctor*"))
    (when-let* ((buf (get-buffer name)))
      (dolist (win (get-buffer-window-list buf nil t))
        (when (and (window-live-p win) (eq (window-deletable-p win) t))
          (delete-window win)))
      (when (buffer-live-p buf)
        (kill-buffer buf))))
  (dolist (frame (frame-list))
    (when (frame-live-p frame)
      (set-frame-parameter frame 'herdr-sanctum-saved-window-config nil)
      (dolist (tab (frame-parameter frame 'tabs))
        (when (consp tab)
          (setf (alist-get 'herdr-sanctum-saved-window-config (cdr tab) nil t) nil)))
      (when (bound-and-true-p tab-bar-mode)
        (dolist (tab (tab-bar-tabs frame))
          (when (consp tab)
            (setf (alist-get 'herdr-sanctum-saved-window-config (cdr tab) nil t) nil))))
      (dolist (win (window-list frame 'no-minibuf))
        (set-window-parameter win 'herdr-sanctum-panel-width nil)
        (set-window-parameter win 'herdr-sanctum-header-width nil))))
  (clrhash herdr-sanctum-sidebar--expanded-spaces)
  (clrhash herdr-sanctum--svg-cache)
  (clrhash herdr-sanctum--svg-file-cache)
  (clrhash herdr-sanctum--palette-cache)
  (clrhash herdr-sanctum--chrome-keymap-cache)
  (when (fboundp 'herdr-sanctum-ime--clear)
    (herdr-sanctum-ime--clear))
  (setq-default herdr-sanctum-drawer--files-cache nil
                herdr-sanctum-drawer--render-key nil
                herdr-sanctum-drawer--active-keymap nil
                herdr-sanctum--header-line-string nil
                herdr-sanctum--header-line-dirty nil
                herdr-sanctum--ellipsis-px-cache nil
                herdr-sanctum--panel-remap-cookies nil)
  (setq herdr-sanctum-sidebar--enabled nil
        herdr-sanctum-drawer--enabled nil
        herdr-sanctum-drawer--tab nil
        herdr-sanctum-drawer--files-dir nil
        herdr-sanctum-drawer--last-focused-dir nil
        herdr-sanctum--last-theme-state nil
        herdr-sanctum-git--executable nil
        herdr-sanctum-git--executable-program nil
        herdr-sanctum-tab-routing--last-match nil
        herdr-sanctum--mode-line-string nil
        herdr-sanctum--header-badge-string nil
        herdr-sanctum--previous-frame nil
        herdr-sanctum--last-step-pane-id nil
        herdr-sanctum--last-step-time 0.0
        herdr-sanctum--connected-socket-path nil
        herdr-sanctum--reconnect-attempt 0
        herdr-sanctum--last-focus-key nil
        herdr-sanctum--focused-dir-cache nil
        herdr-sanctum--ws-dir-cache nil
        herdr-sanctum--timestamps-socket nil
        herdr-sanctum--sub-ack-time nil
        herdr-sanctum--agent-sub-ack-time nil
        herdr-sanctum--events-lost-retry-count 0
        herdr-sanctum--agent-sub-retry-count 0
        herdr-sanctum--agent-sub-failed-panes nil
        herdr-sanctum--last-snap-time 0.0))

(defmacro herdr-sanctum-test-with-clean-state (&rest body)
  "Execute BODY with clean Sanctum state before and after."
  (declare (indent 0) (debug t))
  `(let ((orig-wconf (current-window-configuration)))
     (herdr-sanctum-test--reset-state)
     (unwind-protect
         (progn ,@body)
       (herdr-sanctum-test--reset-state)
       (ignore-errors (set-window-configuration orig-wconf)))))

(defmacro herdr-sanctum-test-with-temp-dir (var &rest body)
  "Bind VAR to a fresh temporary directory deleted in `unwind-protect'."
  (declare (indent 1) (debug (symbolp body)))
  `(let ((,var (make-temp-file "herdr-sanctum-tmp-" t)))
     (unwind-protect
         (progn ,@body)
       (when (file-directory-p ,var)
         (ignore-errors (delete-directory ,var t))))))

(defmacro herdr-sanctum-test-with-mock-server (spec handler &rest body)
  "Run BODY against a local Unix socket server invoking HANDLER `(CLIENT DATA)'.
SPEC is nil, `(SOCK-VAR)', or `(SOCK-VAR SERVER-VAR)'.
Binds `herdr-sanctum-socket-path' inside BODY and guarantees that the server,
every accepted client process, and the socket file are deleted in
`unwind-protect'."
  (declare (indent 2) (debug (sexp form body)))
  (let* ((sock-sym (gensym "sock-"))
         (server-sym (gensym "server-"))
         (clients-var (gensym "clients-"))
         (handler-var (gensym "handler-"))
         (user-sock (car-safe spec))
         (user-server (cadr spec)))
    `(let* ((,sock-sym (expand-file-name
                        (format "herdr-test-%d.sock"
                                (cl-incf herdr-sanctum-test--seq))
                        herdr-sanctum-test--root))
            (,clients-var nil)
            (,handler-var ,handler)
            (,server-sym
             (make-network-process
              :name (format "herdr-sanctum-mock-%d" herdr-sanctum-test--seq)
              :server t
              :family 'local
              :service ,sock-sym
              :coding 'utf-8-unix
              :noquery t
              :log (lambda (_srv client _msg)
                     (when (processp client)
                       (push client ,clients-var)))
              :filter (lambda (client data)
                        (unless (memq client ,clients-var)
                          (push client ,clients-var))
                        (let* ((herdr-sanctum--max-line-bytes (* 256 1024))
                               (lines (car (herdr-sanctum--frame-push client data))))
                          (when lines
                            (funcall ,handler-var client
                                     (concat (string-join lines "\n") "\n")))))))
            (herdr-sanctum-socket-path ,sock-sym)
            ,@(when user-sock `((,user-sock ,sock-sym)))
            ,@(when user-server `((,user-server ,server-sym))))
       (unwind-protect
           (progn ,@body)
         (herdr-sanctum-disconnect)
         (dolist (c ,clients-var)
           (when (process-live-p c)
             (delete-process c)))
         (when (process-live-p ,server-sym)
           (delete-process ,server-sym))
         (when (file-exists-p ,sock-sym)
           (ignore-errors (delete-file ,sock-sym)))))))

(provide 'herdr-sanctum-test-helper)
;;; herdr-sanctum-test-helper.el ends here
