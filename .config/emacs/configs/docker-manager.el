;;; docker-manager.el -*- lexical-binding: t; -*-


;;; code

(defun yaz/docker-command-internal (container command)
  "execute a docker command on container."
  (let ((cmd (format "docker %s %s" command container)))
    (message "debug: executing: %s" cmd)
    (let ((result (shell-command-to-string cmd)))
      (message "%s" (string-trim result))
      result)))

(defun yaz/docker-stop-internal (container)
  "stop a docker container."
  (yaz/docker-command-internal container "stop"))

(defun yaz/docker-kill-internal (container)
  "kill a docker container."
  (yaz/docker-command-internal container "kill"))

(defun yaz/docker-restart-internal (container)
  "restart a docker container."
  (yaz/docker-command-internal container "restart"))

(defun yaz/docker-pause-internal (container)
  "pause a docker container."
  (yaz/docker-command-internal container "pause"))

(defun yaz/docker-unpause-internal (container)
  "unpause a docker container."
  (yaz/docker-command-internal container "unpause"))

(defun yaz/docker-remove-internal (container &optional force)
  "remove a docker container. if force is non-nil, use -f flag."
  (let ((cmd (if force
                 (format "docker rm -f %s" container)
               (format "docker rm %s" container))))
    (message "debug: executing: %s" cmd)
    (let ((result (shell-command-to-string cmd)))
      (message "%s" (string-trim result))
      result)))

(defun yaz/docker-ps ()
  "show running docker containers in interactive buffer."
  (interactive)
  (yaz/list-containers))

(defun yaz/docker-stop ()
  "stop a docker container by name."
  (interactive)
  (let ((container (read-string "container to stop: ")))
    (message "debug: yaz/docker-stop called with: %s" container)
    (yaz/docker-stop-internal container)))

(defun yaz/docker-kill ()
  "kill a docker container by name."
  (interactive)
  (let ((container (read-string "container to kill: ")))
    (message "debug: yaz/docker-kill called with: %s" container)
    (yaz/docker-kill-internal container)))

(defun yaz/docker-logs ()
  "follow docker logs for a container."
  (interactive)
  (let ((container (read-string "container name: ")))
    (message "debug: following logs for: %s" container)
    (let ((buf (get-buffer-create (format "*docker-logs-%s*" container))))
      (with-current-buffer buf
        (erase-buffer)
        (insert (format " following logs for container: %s\n" container))
        (insert "prsess q to quit\n\n"))
      (start-process-shell-command
       "docker-logs"
       buf
       (format "docker logs -f %s" container))
      (switch-to-buffer buf)
      (read-only-mode 1)
      (local-set-key (kbd "q") 'quit-window))))

(defun yaz/docker-restart ()
  "restart a docker container by name."
  (interactive)
  (let ((container (read-string "container to restart: ")))
    (message "debug: yaz/docker-restart called with: %s" container)
    (yaz/docker-restart-internal container)))

(defvar yaz/docker-buffer-name "*docker containers*"
  "name of the buffer used to display docker containers.")

(define-derived-mode yaz/docker-mode special-mode "docker"
  "major mode for viewing and managing docker containers.

Key bindings:
\\{yaz/docker-mode-map}"
  (message "debug: yaz/docker-mode activated")
  (setq truncate-lines t))

(define-key yaz/docker-mode-map (kbd "g r") #'yaz/list-containers)
(define-key yaz/docker-mode-map (kbd "s s") #'yaz/docker-stop-at-point)
(define-key yaz/docker-mode-map (kbd "s k") #'yaz/docker-kill-at-point)
(define-key yaz/docker-mode-map (kbd "s r") #'yaz/docker-restart-at-point)
(define-key yaz/docker-mode-map (kbd "s p") #'yaz/docker-pause-at-point)
(define-key yaz/docker-mode-map (kbd "s u") #'yaz/docker-unpause-at-point)
(define-key yaz/docker-mode-map (kbd "d d") #'yaz/docker-remove-at-point)
(define-key yaz/docker-mode-map (kbd "d d") #'yaz/docker-remove-at-point-force)
(define-key yaz/docker-mode-map (kbd "l") #'yaz/docker-logs-at-point)
(define-key yaz/docker-mode-map (kbd "i") #'yaz/docker-inspect-at-point)
(define-key yaz/docker-mode-map (kbd "e") #'yaz/docker-exec-at-point)
(define-key yaz/docker-mode-map (kbd "RET") #'yaz/docker-logs-at-point)
(define-key yaz/docker-mode-map (kbd "q") #'quit-window)

(with-eval-after-load 'evil
  (evil-define-key 'normal yaz/docker-mode-map
    (kbd "g r") #'yaz/list-containers
    (kbd "s s") #'yaz/docker-stop-at-point
    (kbd "s k") #'yaz/docker-kill-at-point
    (kbd "s r") #'yaz/docker-restart-at-point
    (kbd "s p") #'yaz/docker-pause-at-point
    (kbd "s u") #'yaz/docker-unpause-at-point
    (kbd "d d") #'yaz/docker-remove-at-point
    (kbd "d d") #'yaz/docker-remove-at-point-force
    (kbd "l") #'yaz/docker-logs-at-point
    (kbd "i") #'yaz/docker-inspect-at-point
    (kbd "e") #'yaz/docker-exec-at-point
    (kbd "RET") #'yaz/docker-logs-at-point
    (kbd "q") #'quit-window)

  (evil-set-initial-state 'yaz/docker-mode 'normal))

(defun yaz/list-containers ()
  "List docker containers with status and ports.

Key bindings:
  gr  - refresh container list
  ss  - stop container
  sk  - kill container
  sr  - restart container
  sp  - pause container
  su  - unpause container
  dd  - remove container
  dD  - force remove container
  l   - view logs
  i   - inspect container
  e   - exec into container
  RET - view logs
  q   - quit window"
  (interactive)
  (message "debug: starting yaz/list-containers")
  (let ((buf (get-buffer-create yaz/docker-buffer-name))
        (total-containers 0)
        (timestamp (format-time-string "%Y-%M-%d %H:%m:%S")))
    (message "debug: buffer created/retrieved: %s" (buffer-name buf))
    (with-current-buffer buf
      (message "debug: inside buffer, current major-mode: %s" major-mode)
      (let ((inhibit-read-only t))
        (message "debug: erasing buffer contents")
        (erase-buffer)

        (insert (propertize "------------------------------------------------------------------\n"
                           'face 'shadow))
        (insert (propertize "                   docker manager\n"
                           'face '(:weight bold :height 1.2 :inherit font-lock-keyword-face)))
        (insert (propertize "------------------------------------------------------------------\n"
                           'face 'shadow))
        (insert "\n")

        (insert (propertize "key bindings:\n" 'face '(:weight bold :inherit font-lock-type-face)))
        (insert (propertize "  gr" 'face '(:weight bold :inherit success))
                "      - refresh container list\n")
        (insert (propertize "  ss" 'face '(:weight bold :inherit warning))
                "      - stop container\n")
        (insert (propertize "  sk" 'face '(:weight bold :inherit error))
                "      - kill container\n")
        (insert (propertize "  sr" 'face '(:weight bold :inherit font-lock-function-name-face))
                "      - restart container\n")
        (insert (propertize "  sp" 'face '(:weight bold :inherit font-lock-constant-face))
                "      - pause container\n")
        (insert (propertize "  su" 'face '(:weight bold :inherit font-lock-constant-face))
                "      - unpause container\n")
        (insert (propertize "  dd" 'face '(:weight bold :inherit warning))
                "      - remove container\n")
        (insert (propertize "  dd" 'face '(:weight bold :inherit error))
                "      - force remove container\n")
        (insert (propertize "  l" 'face '(:weight bold :inherit font-lock-string-face))
                "       - view logs\n")
        (insert (propertize "  i" 'face '(:weight bold :inherit font-lock-builtin-face))
                "       - inspect container (json)\n")
        (insert (propertize "  e" 'face '(:weight bold :inherit font-lock-variable-name-face))
                "       - exec into container (/bin/bash)\n")
        (insert (propertize "  RET" 'face '(:weight bold :inherit font-lock-string-face))
                "     - view logs\n")
        (insert (propertize "  q" 'face '(:weight bold :inherit font-lock-function-name-face))
                "       - quit window\n")
        (insert (propertize "\n  navigation: j/k to move, / to search\n"
                           'face '(:italic t :inherit shadow)))
        (insert "\n")
        (insert (propertize "info:\n" 'face '(:weight bold :inherit font-lock-type-face)))
        (insert (format "  last updated: %s\n"
                       (propertize timestamp 'face 'font-lock-string-face)))
        (insert (format "  docker version: %s\n"
                       (propertize (string-trim (shell-command-to-string "docker --version 2>/dev/null || echo 'not installed'"))
                                   'face 'font-lock-string-face)))
        (insert (format "  user: %s\n"
                       (propertize (user-login-name) 'face 'font-lock-string-face)))

        (insert "\n")
        (insert (propertize "------------------------------------------------------------------\n"
                           'face 'shadow))
        (insert "\n")

        (insert (propertize "container id  name              status           ports\n"
                           'face '(:weight bold :inherit font-lock-builtin-face)))
        (insert (propertize "------------------------------------------------------------------\n"
                           'face 'shadow))

        (message "debug: running docker ps command...")
        (let ((before-line (line-number-at-pos))
              (result (call-process-shell-command
                       "docker ps -a --format '{{.id}}  {{.names}}  {{.status}}  {{.ports}}' 2>/dev/null || echo 'error: docker not running or not installed'"
                       nil buf t)))
          (setq total-containers (- (line-number-at-pos) before-line))
          (message "debug: docker ps returned: %s, added %d containers"
                   result total-containers))

        (goto-char (point-max))
        (insert "\n")
        (insert (propertize "------------------------------------------------------------------\n"
                           'face 'shadow))
        (insert (format "total containers: %s\n"
                       (propertize (number-to-string total-containers)
                                   'face '(:weight bold :inherit font-lock-constant-face)))))

      (goto-char (point-min))
      (search-forward "container id" nil t)
      (forward-line 2)
      (message "debug: point is now at line %d" (line-number-at-pos))
      (message "debug: activating yaz/docker-mode")
      (yaz/docker-mode)
      (setq buffer-read-only t)
      (message "debug: mode activated, buffer-read-only: %s" buffer-read-only))
    (message "debug: switching to buffer %s" (buffer-name buf))
    (switch-to-buffer buf)
    (message "debug: yaz/list-containers complete")))

(defun yaz/container-at-point ()
  (save-excursion
    (beginning-of-line)
    (let ((found (when (looking-at "^\\([a-f0-9]+\\)\\s-+\\([^ \t]+\\)")
                   (cons (match-string 1) (match-string 2)))))
      (message "debug: yaz/container-at-point found: %s" (or found "nil"))
      found)))

(defun yaz/docker-stop-at-point ()
  (interactive)
  (message "debug: yaz/docker-stop-at-point called")
  (let ((container (yaz/container-at-point)))
    (if container
        (when (y-or-n-p (format "stop container %s? " (cdr container)))
          (message "debug: user confirmed stop for container %s" (cdr container))
          (yaz/docker-stop-internal (cdr container))
          (sit-for 1)
          (yaz/list-containers))
      (message "no container on this line"))))

(defun yaz/docker-kill-at-point ()
  (interactive)
  (message "debug: yaz/docker-kill-at-point called")
  (let ((container (yaz/container-at-point)))
    (if container
        (when (y-or-n-p (format "kill container %s? " (cdr container)))
          (message "debug: user confirmed kill for container %s" (cdr container))
          (yaz/docker-kill-internal (cdr container))
          (sit-for 1)
          (yaz/list-containers))
      (message "no container on this line"))))

(defun yaz/docker-restart-at-point ()
  (interactive)
  (message "debug: yaz/docker-restart-at-point called")
  (let ((container (yaz/container-at-point)))
    (if container
        (when (y-or-n-p (format "restart container %s? " (cdr container)))
          (message "debug: user confirmed restart for container %s" (cdr container))
          (yaz/docker-restart-internal (cdr container))
          (sit-for 1)
          (yaz/list-containers))
      (message "no container on this line"))))

(defun yaz/docker-pause-at-point ()
  (interactive)
  (message "debug: yaz/docker-pause-at-point called")
  (let ((container (yaz/container-at-point)))
    (if container
        (progn
          (message "debug: pausing container %s" (cdr container))
          (yaz/docker-pause-internal (cdr container))
          (sit-for 1)
          (yaz/list-containers))
      (message "no container on this line"))))

(defun yaz/docker-unpause-at-point ()
  (interactive)
  (message "debug: yaz/docker-unpause-at-point called")
  (let ((container (yaz/container-at-point)))
    (if container
        (progn
          (message "debug: unpausing container %s" (cdr container))
          (yaz/docker-unpause-internal (cdr container))
          (sit-for 1)
          (yaz/list-containers))
      (message "no container on this line"))))

(defun yaz/docker-remove-at-point ()
  (interactive)
  (message "debug: yaz/docker-remove-at-point called")
  (let ((container (yaz/container-at-point)))
    (if container
        (when (y-or-n-p (format "remove container %s? " (cdr container)))
          (message "debug: user confirmed remove for container %s" (cdr container))
          (yaz/docker-remove-internal (cdr container))
          (sit-for 1)
          (yaz/list-containers))
      (message "no container on this line"))))

(defun yaz/docker-remove-at-point-force ()
  (interactive)
  (message "debug: yaz/docker-remove-at-point-force called")
  (let ((container (yaz/container-at-point)))
    (if container
        (when (y-or-n-p (format "force remove container %s? " (cdr container)))
          (message "debug: user confirmed force remove for container %s" (cdr container))
          (yaz/docker-remove-internal (cdr container) t)
          (sit-for 1)
          (yaz/list-containers))
      (message "no container on this line"))))

(defun yaz/docker-logs-at-point ()
  (interactive)
  (message "debug: yaz/docker-logs-at-point called")
  (let ((container (yaz/container-at-point)))
    (if container
        (let* ((name (cdr container))
               (buf (get-buffer-create (format "*docker-logs-%s*" name))))
          (message "debug: opening logs for container %s" name)
          (with-current-buffer buf
            (erase-buffer)
            (insert (propertize (format "logs for container: %s\n" name)
                               'face '(:weight bold :inherit font-lock-keyword-face)))
            (insert (propertize "press q to quit, C-c C-c to stop following\n\n"
                               'face 'shadow)))
          (start-process-shell-command
           "docker-logs"
           buf
           (format "docker logs -f --tail 100 %s" name))
          (switch-to-buffer buf)
          (read-only-mode 1)
          (local-set-key (kbd "q") 'quit-window))
      (message "no container on this line"))))

(defun yaz/docker-inspect-at-point ()
  (interactive)
  (message "debug: yaz/docker-inspect-at-point called")
  (let ((container (yaz/container-at-point)))
    (if container
        (let* ((name (cdr container))
               (buf (get-buffer-create (format "*docker-inspect-%s*" name))))
          (message "debug: inspecting container %s" name)
          (with-current-buffer buf
            (erase-buffer)
            (insert (shell-command-to-string (format "docker inspect %s" name)))
            (json-mode)
            (goto-char (point-min)))
          (switch-to-buffer buf)
          (read-only-mode 1)
          (local-set-key (kbd "q") 'quit-window))
      (message "no container on this line"))))

(defun yaz/docker-exec-at-point ()
  (interactive)
  (message "debug: yaz/docker-exec-at-point called")
  (let ((container (yaz/container-at-point)))
    (if container
        (let ((name (cdr container)))
          (message "debug: exec into container %s" name)
          (if (fboundp 'vterm)
              (let ((vterm-shell (format "docker exec -it %s /bin/bash" name)))
                (vterm (format "*docker-exec-%s*" name)))
            (ansi-term "/bin/bash" (format "docker-exec-%s" name))
            (comint-send-string (current-buffer)
                               (format "docker exec -it %s /bin/bash\n" name))))
      (message "no container on this line"))))

(provide 'docker-manager)
;;; docker-manager.el ends here
