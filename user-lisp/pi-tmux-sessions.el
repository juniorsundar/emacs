;; -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'project)
(require 'subr-x)

(defgroup pi-tmux nil
  "Attach Emacs buffers and projects to Pi sessions in tmux."
  :group 'tools)

(defcustom pi-tmux-process-regexp "\\_<pi\\_>"
  "Regexp used to identify a Pi process."
  :type 'regexp
  :group 'pi-tmux)

(defcustom pi-tmux-submit-by-default nil
  "Whether Pi messages are submitted immediately after pasting.

A prefix argument to a send command reverses this setting."
  :type 'boolean
  :group 'pi-tmux)

(defvar pi-tmux--project-attachments (make-hash-table :test #'equal)
  "Project roots mapped to attached tmux pane IDs.")

(defvar-local pi-tmux--buffer-attachment nil
  "Tmux pane ID attached specifically to the current buffer.")

(defun pi-tmux--panes ()
  "Return all tmux panes."
  (mapcar
   (lambda (line)
     (pcase-let ((`(,session ,window ,pane-index ,pane-id ,pid ,cwd)
                  (split-string line "\t" nil)))
       (list :session session
             :window window
             :pane-index pane-index
             :pane pane-id
             :pane-pid (string-to-number pid)
             :cwd cwd)))
   (process-lines-ignore-status
    "tmux"
    "list-panes" "-a"
    "-F"
    (concat
     "#{session_name}\t"
     "#{window_index}\t"
     "#{pane_index}\t"
     "#{pane_id}\t"
     "#{pane_pid}\t"
     "#{pane_current_path}"))))

(defun pi-tmux--process-tree ()
  "Return (PROCS . CHILDREN).

PROCS maps PID to plists.  CHILDREN maps parent PIDs to child PIDs."
  (let ((procs (make-hash-table :test #'eql))
        (children (make-hash-table :test #'eql)))
    ;; `ps -u USER' can omit tmux-pane children when invoked by an Emacs
    ;; daemon.  Enumerate all visible processes; pane roots still constrain
    ;; discovery to the current user's tmux server.
    (dolist (line (process-lines-ignore-status
                   "ps" "-e" "-ww" "-o" "pid=,ppid=,args="))
      (when (string-match
             "\\`[[:space:]]*\\([0-9]+\\)[[:space:]]+\\([0-9]+\\)[[:space:]]+\\(.*\\)\\'"
             line)
        (let ((pid (string-to-number (match-string 1 line)))
              (ppid (string-to-number (match-string 2 line)))
              (args (match-string 3 line)))
          (puthash pid (list :pid pid :ppid ppid :args args) procs)
          (push pid (gethash ppid children)))))
    (cons procs children)))

(defun pi-tmux--descendants (pid children)
  "Return PID and all descendants of PID in CHILDREN."
  (let ((todo (list pid))
        result)
    (while todo
      (let ((current (pop todo)))
        (push current result)
        (setq todo (append (gethash current children) todo))))
    (nreverse result)))

(defun pi-tmux-sessions ()
  "Return active Pi sessions running in tmux."
  (let* ((tree (pi-tmux--process-tree))
         (procs (car tree))
         (children (cdr tree))
         result)
    (dolist (pane (pi-tmux--panes))
      (let* ((root (plist-get pane :pane-pid))
             (pi-proc
              (cl-loop
               for pid in (pi-tmux--descendants root children)
               for proc = (gethash pid procs)
               when (and proc
                         (string-match-p pi-tmux-process-regexp
                                         (plist-get proc :args)))
               return proc)))
        (when pi-proc
          (push (append pane
                        (list :pi-pid (plist-get pi-proc :pid)
                              :pi-command (plist-get pi-proc :args)))
                result))))
    (nreverse result)))

(defun pi-tmux--project-root ()
  "Return the canonical root of the current project, or nil."
  (when-let* ((project (project-current nil)))
    (file-truename (project-root project))))

(defun pi-tmux--label (session)
  "Return a completion label for SESSION."
  (format "%s:%s.%s  %s  %s"
          (plist-get session :session)
          (plist-get session :window)
          (plist-get session :pane-index)
          (plist-get session :cwd)
          (plist-get session :pi-command)))

(defun pi-tmux--session-cwd-matches-project-p (session project-root)
  "Whether SESSION's cwd is PROJECT-ROOT."
  (and project-root
       (equal (file-truename (plist-get session :cwd)) project-root)))

(defun pi-tmux--sort-sessions (sessions)
  "Sort SESSIONS with current-project sessions first."
  (let ((project-root (pi-tmux--project-root)))
    (sort (copy-sequence sessions)
          (lambda (left right)
            (and (pi-tmux--session-cwd-matches-project-p left project-root)
                 (not (pi-tmux--session-cwd-matches-project-p right project-root)))))))

(defun pi-tmux-read-session (&optional sessions)
  "Interactively select a Pi session from SESSIONS.

When SESSIONS is nil, discover active Pi sessions first."
  (let ((sessions (pi-tmux--sort-sessions (or sessions (pi-tmux-sessions))))
        choices)
    (unless sessions
      (user-error "No active Pi tmux sessions found"))
    (setq choices (mapcar (lambda (session)
                            (cons (pi-tmux--label session) session))
                          sessions))
    (cdr (assoc (completing-read "Pi session: " choices nil t) choices))))

(defun pi-tmux--session-for-pane (pane sessions)
  "Return PANE's Pi session from SESSIONS, or nil."
  (cl-find pane sessions :key (lambda (session) (plist-get session :pane))
           :test #'equal))

(defun pi-tmux--attachments ()
  "Return current attachments in buffer-then-project precedence order."
  (delq nil
        (list (and pi-tmux--buffer-attachment
                   (cons :buffer pi-tmux--buffer-attachment))
              (when-let* ((root (pi-tmux--project-root))
                          (pane (gethash root pi-tmux--project-attachments)))
                (cons :project pane)))))

(defun pi-tmux--store-attachment (scope pane)
  "Attach PANE at SCOPE for the current buffer or project."
  (pcase scope
    (:buffer (setq-local pi-tmux--buffer-attachment pane))
    (:project
     (if-let* ((root (pi-tmux--project-root)))
         (puthash root pane pi-tmux--project-attachments)
       (user-error "Current buffer does not belong to a project")))))

(defun pi-tmux--clear-attachment (scope)
  "Clear the attachment at SCOPE for the current buffer or project."
  (pcase scope
    (:buffer (setq-local pi-tmux--buffer-attachment nil))
    (:project
     (when-let* ((root (pi-tmux--project-root)))
       (remhash root pi-tmux--project-attachments)))))

(defun pi-tmux--read-scope (prompt scopes default)
  "Read a scope for PROMPT from SCOPES, defaulting to DEFAULT."
  (intern (downcase
           (completing-read prompt
                            (mapcar (lambda (scope) (symbol-name scope)) scopes)
                            nil t nil nil (symbol-name default)))))

(defun pi-tmux-attach ()
  "Attach a discovered Pi tmux pane to the current project or buffer."
  (interactive)
  (let* ((session (pi-tmux-read-session))
         (scopes (if (pi-tmux--project-root) '(:project :buffer) '(:buffer)))
         (scope (if (= (length scopes) 1)
                    :buffer
                  (pi-tmux--read-scope "Attach Pi session to: " scopes :project)))
         (pane (plist-get session :pane)))
    (pi-tmux--store-attachment scope pane)
    (message "Attached Pi pane %s to %s" pane (substring (symbol-name scope) 1))))

(defun pi-tmux-detach ()
  "Detach the current buffer or project from its Pi tmux pane."
  (interactive)
  (let* ((root (pi-tmux--project-root))
         (scopes (delq nil (list (and pi-tmux--buffer-attachment :buffer)
                                 (and root (gethash root pi-tmux--project-attachments)
                                      :project)))))
    (pcase scopes
      (`() (user-error "No Pi attachment for this buffer or project"))
      (`(,scope) (pi-tmux--clear-attachment scope)
                 (message "Detached Pi from %s" (substring (symbol-name scope) 1)))
      (_ (let ((scope (pi-tmux--read-scope "Detach Pi from: " scopes :buffer)))
           (pi-tmux--clear-attachment scope)
           (message "Detached Pi from %s" (substring (symbol-name scope) 1)))))))

(defun pi-tmux--resolve-session ()
  "Return a valid Pi session for the current buffer.

Use buffer attachments before project attachments, then prompt for a discovered
session.  Clear stale attachments while continuing through that precedence.
"
  (let ((sessions (pi-tmux-sessions)))
    (or (cl-loop for attachment in (pi-tmux--attachments)
                 for session = (pi-tmux--session-for-pane (cdr attachment) sessions)
                 if session return session
                 else do (progn
                           (pi-tmux--clear-attachment (car attachment))
                           (message "Removed stale Pi %s attachment"
                                    (substring (symbol-name (car attachment)) 1))))
        (pi-tmux-read-session sessions))))

(defun pi-tmux--submit-p (prefix)
  "Return whether PREFIX requests submitting a pasted Pi message."
  (if prefix
      (not pi-tmux-submit-by-default)
    pi-tmux-submit-by-default))

(defun pi-tmux--run (&rest arguments)
  "Run tmux with ARGUMENTS or signal a user-facing error."
  (unless (zerop (apply #'process-file "tmux" nil nil nil arguments))
    (user-error "tmux command failed: tmux %s" (string-join arguments " "))))

(defun pi-tmux--send (pane text submit)
  "Paste TEXT into PANE and submit it when SUBMIT is non-nil."
  (let ((buffer-name (format "pi-tmux-emacs-%d-%d" (emacs-pid) (random most-positive-fixnum))))
    (unwind-protect
        (with-temp-buffer
          (insert text)
          (unless (zerop (call-process-region
                          (point-min) (point-max) "tmux" nil nil nil
                          "load-buffer" "-b" buffer-name "-"))
            (user-error "tmux could not load the Pi message"))
          (pi-tmux--run "paste-buffer" "-d" "-r" "-b" buffer-name "-t" pane)
          (when submit
            (pi-tmux--run "send-keys" "-t" pane "Enter")))
      ;; `paste-buffer -d' normally removes it; this also cleans up after errors.
      (process-file "tmux" nil nil nil "delete-buffer" "-b" buffer-name))))

(defun pi-tmux-send-text (text &optional prefix)
  "Paste TEXT into the resolved Pi session.

With PREFIX, reverse `pi-tmux-submit-by-default'."
  (interactive (list (read-string "Pi prompt: ") current-prefix-arg))
  (let ((session (pi-tmux--resolve-session)))
    (pi-tmux--send (plist-get session :pane) text (pi-tmux--submit-p prefix))
    (message "Sent to Pi in %s" (plist-get session :pane))))

(defun pi-tmux--file-reference (&optional require-region)
  "Return an @-file reference for the current file or its active region.

When REQUIRE-REGION is non-nil, signal an error unless a region is active."
  (unless buffer-file-name
    (user-error "Current buffer does not visit a file"))
  (when (and require-region (not (use-region-p)))
    (user-error "No active region to send to Pi"))
  (let* ((root (pi-tmux--project-root))
         (file (if root
                   (file-relative-name buffer-file-name root)
                 (abbreviate-file-name buffer-file-name))))
    (if (use-region-p)
        (format "@%s:%d-%d"
                file
                (line-number-at-pos (region-beginning))
                (line-number-at-pos (max (region-beginning) (1- (region-end)))))
      (format "@%s" file))))

(defun pi-tmux-send-region (&optional prefix)
  "Send the active region as an @-file line-range reference to Pi.

With PREFIX, reverse `pi-tmux-submit-by-default'."
  (interactive "P")
  (pi-tmux-send-text (pi-tmux--file-reference t) prefix))

(defun pi-tmux-send-context (&optional prefix)
  "Send the current file or active region as an @-file reference to Pi.

With PREFIX, reverse `pi-tmux-submit-by-default'."
  (interactive "P")
  (pi-tmux-send-text (pi-tmux--file-reference) prefix))

(defun pi-tmux-focus ()
  "Focus the resolved Pi pane from an Emacs process running inside tmux."
  (interactive)
  (let ((session (pi-tmux--resolve-session)))
    (unless (getenv "TMUX")
      (user-error "Cannot focus Pi pane %s: Emacs is not running inside tmux"
                  (plist-get session :pane)))
    (pi-tmux--run "switch-client" "-t"
                  (format "%s:%s" (plist-get session :session)
                          (plist-get session :window)))
    (pi-tmux--run "select-pane" "-t" (plist-get session :pane))))

(provide 'pi-tmux-sessions)
