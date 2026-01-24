;;; proc-manager.el -*- lexical-binding: t; -*-

;; Internal helper functions

(defun yaz/kill-process-internal (proc &optional force)
  "internal helper to kill a process by pid or name.
If FORCE is non-nil, sends SIGKILL (-9)"
  (let* ((signal (if force "-9" ""))
         (cmd (if (string-match-p "^[0-9]+$" proc)
                  (format "kill %s %s" signal proc)
                (format "pkill %s %s" signal proc))))
    (message "debug: executing command: %s" cmd)
    (shell-command cmd)
    (message "sent sig%s to %s" (if force "kill" "term") proc)))

(defun yaz/signal-process-internal (proc signal)
  "internal helper to send a specific signal to a process proc."
  (let* ((cmd (if (string-match-p "^[0-9]+$" proc)
                  (format "kill -s %s %s" signal proc)
                (format "pkill -%s %s" signal proc))))
    (message "debug: executing command: %s" cmd)
    (shell-command cmd)
    (message "sent %s to %s" signal proc)))

(defun yaz/kill-process ()
  "kill a process by pid or name (sigterm)."
  (interactive)
  (let ((proc (read-string "process (pid or name): ")))
    (message "debug: yaz/kill-process called with: %s" proc)
    (yaz/kill-process-internal proc)))

(defun yaz/kill-process-force ()
  "force kill a process by pid or name (sigkill)."
  (interactive)
  (let ((proc (read-string "process (pid or name): ")))
    (message "debug: yaz/kill-process-force called with: %s" proc)
    (yaz/kill-process-internal proc t)))

(defun yaz/signal-process ()
  "send a specific signal to a process."
  (interactive)
  (let* ((proc (read-string "process (pid or name): "))
         (signal (completing-read "signal: "
                                  '("sigterm" "sigkill" "sigstop" "sigcont"
                                    "sighup" "sigint" "sigusr1" "sigusr2")
                                  nil t)))
    (message "debug: yaz/signal-process called with proc=%s signal=%s" proc signal)
    (yaz/signal-process-internal proc signal)))

(defvar yaz/process-buffer-name "*processes*"
  "name of the buffer used to display process list.")

(define-derived-mode yaz/processes-mode special-mode "processes"
  "major mode for viewing and killing processes.

Key bindings:
\\{yaz/processes-mode-map}"
  (message "debug: yaz/processes-mode activated")
  (setq truncate-lines t))

(define-key yaz/processes-mode-map (kbd "g r") #'yaz/list-processes)
(define-key yaz/processes-mode-map (kbd "d d") #'yaz/kill-process-at-point)
(define-key yaz/processes-mode-map (kbd "d d") #'yaz/kill-process-at-point-force)
(define-key yaz/processes-mode-map (kbd "d s") #'yaz/signal-process-at-point)
(define-key yaz/processes-mode-map (kbd "q") #'quit-window)

(with-eval-after-load 'evil
  (evil-define-key 'normal yaz/processes-mode-map
    (kbd "g r") #'yaz/list-processes
    (kbd "d d") #'yaz/kill-process-at-point
    (kbd "d d") #'yaz/kill-process-at-point-force
    (kbd "d s") #'yaz/signal-process-at-point
    (kbd "ret") #'yaz/kill-process-at-point
    (kbd "x") #'yaz/kill-process-at-point-force
    (kbd "s") #'yaz/signal-process-at-point
    (kbd "q") #'quit-window)

  (evil-set-initial-state 'yaz/processes-mode 'normal))

(defun yaz/list-processes ()
  "list processes using ps sorted by memory usage.

Key bindings:
  g - refresh process list
  k - send SIGTERM to process at point
  K - send SIGKILL to process at point
  s - send custom signal to process at point
  q - quit window"
  (interactive)
  (message "debug: starting yaz/list-processes")
  (let ((buf (get-buffer-create yaz/process-buffer-name))
        (total-procs 0)
        (timestamp (format-time-string "%y-%m-%d %h:%m:%s")))
    (message "debug: buffer created/retrieved: %s" (buffer-name buf))
    (with-current-buffer buf
      (message "debug: inside buffer, current major-mode: %s" major-mode)
      (let ((inhibit-read-only t))
        (message "debug: erasing buffer contents")
        (erase-buffer)

        (insert (propertize "-----------------------------------------------------------------\n"
                           'face 'shadow))
        (insert (propertize "                    process manager\n"
                           'face '(:weight bold :height 1.2 :inherit font-lock-keyword-face)))
        (insert (propertize "-----------------------------------------------------------------\n"
                           'face 'shadow))
        (insert "\n")

        (insert (propertize "key bindings:\n" 'face '(:weight bold :inherit font-lock-type-face)))
        (insert (propertize "  gr" 'face '(:weight bold :inherit success))
                "      - refresh process list\n")
        (insert (propertize "  dd" 'face '(:weight bold :inherit warning))
                "      - kill process (sigterm)\n")
        (insert (propertize "  dd" 'face '(:weight bold :inherit error))
                "      - force kill process (sigkill -9)\n")
        (insert (propertize "  ds" 'face '(:weight bold :inherit font-lock-constant-face))
                "      - send custom signal to process\n")
        (insert (propertize "  ret" 'face '(:weight bold :inherit warning))
                "     - quick kill (sigterm)\n")
        (insert (propertize "  x" 'face '(:weight bold :inherit error))
                "       - force kill (sigkill -9)\n")
        (insert (propertize "  s" 'face '(:weight bold :inherit font-lock-constant-face))
                "       - send signal\n")
        (insert (propertize "  q" 'face '(:weight bold :inherit font-lock-function-name-face))
                "       - quit window\n")
        (insert (propertize "\n  navigation: j/k to move, / to search\n"
                           'face '(:italic t :inherit shadow)))
        (insert "\n")

        (insert (propertize "info:\n" 'face '(:weight bold :inherit font-lock-type-face)))
        (insert (format "  last updated: %s\n"
                       (propertize timestamp 'face 'font-lock-string-face)))
        (insert (format "  hostname: %s\n"
                       (propertize (system-name) 'face 'font-lock-string-face)))
        (insert (format "  user: %s\n"
                       (propertize (user-login-name) 'face 'font-lock-string-face)))

        (insert "\n")
        (insert (propertize "-----------------------------------------------------------------\n"
                           'face 'shadow))
        (insert "\n")

        (insert (propertize "pid     user        cmd               %cpu   %mem\n"
                           'face '(:weight bold :inherit font-lock-builtin-face)))
        (insert (propertize "-----------------------------------------------------------------\n"
                           'face 'shadow))

        (message "debug: running ps command...")
        (let ((before-line (line-number-at-pos))
              (result (call-process-shell-command
                       "ps -e -o pid=,user=,comm=,%cpu=,%mem= --sort=-%mem"
                       nil buf t)))
          (setq total-procs (- (line-number-at-pos) before-line))
          (message "debug: ps command returned: %s, added %d processes"
                   result total-procs))

        (goto-char (point-max))
        (insert "\n")
        (insert (propertize "-----------------------------------------------------------------\n"
                           'face 'shadow))
        (insert (format "total processes: %s\n"
                       (propertize (number-to-string total-procs)
                                   'face '(:weight bold :inherit font-lock-constant-face)))))

      (goto-char (point-min))
      (search-forward "pid     user" nil t)
      (forward-line 2)
      (message "debug: point is now at line %d" (line-number-at-pos))
      (message "debug: activating yaz/processes-mode")
      (yaz/processes-mode)
      (setq buffer-read-only t)
      (message "debug: mode activated, buffer-read-only: %s" buffer-read-only))
    (message "debug: switching to buffer %s" (buffer-name buf))
    (switch-to-buffer buf)
    (message "debug: yaz/list-processes complete")))

(defun yaz/pid-at-point ()
  "return pid at current line, or nil if none found."
  (save-excursion
    (beginning-of-line)
    (let ((found (when (looking-at "^\\s-*\\([0-9]+\\)")
                   (match-string 1))))
      (message "debug: yaz/pid-at-point found: %s" (or found "nil"))
      found)))

(defun yaz/kill-process-at-point ()
  "send sigterm to process at point."
  (interactive)
  (message "debug: yaz/kill-process-at-point called")
  (let ((pid (yaz/pid-at-point)))
    (if pid
        (when (y-or-n-p (format "kill process %s (sigterm)? " pid))
          (message "debug: user confirmed kill for pid %s" pid)
          (yaz/kill-process-internal pid)
          (sit-for 0.5)
          (yaz/list-processes))
      (message "no pid on this line"))))

(defun yaz/kill-process-at-point-force ()
  "send sigkill to process at point."
  (interactive)
  (message "debug: yaz/kill-process-at-point-force called")
  (let ((pid (yaz/pid-at-point)))
    (if pid
        (when (y-or-n-p (format "force kill process %s (sigkill)? " pid))
          (message "debug: user confirmed force kill for pid %s" pid)
          (yaz/kill-process-internal pid t)
          (sit-for 0.5)
          (yaz/list-processes))
      (message "no pid on this line"))))

(defun yaz/signal-process-at-point ()
  "send a custom signal to process at point."
  (interactive)
  (message "debug: yaz/signal-process-at-point called")
  (let ((pid (yaz/pid-at-point)))
    (if pid
        (let ((signal (completing-read
                       (format "send signal to process %s: " pid)
                       '("sigterm" "sigkill" "sigstop" "sigcont"
                         "sighup" "sigint" "sigusr1" "sigusr2")
                       nil t)))
          (message "debug: sending signal %s to pid %s" signal pid)
          (yaz/signal-process-internal pid signal)
          (sit-for 0.5)
          (yaz/list-processes))
      (message "no pid on this line"))))

(provide 'proc-manager)
;;; proc-manager.el ends here
