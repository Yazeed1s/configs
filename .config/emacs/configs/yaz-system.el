;;; yaz-system.el --- System utilities and TUI applications -*- lexical-binding: t; -*-

;;; Commentary:
;; System information, TUI applications, and shell integration utilities.

;;; Code:

(defvar yaz/terminal-command "alacritty"
  "terminal emulator command to use.")

(defvar yaz/terminal-args '("--working-directory")
  "arguments for terminal command (before directory).")

(defun yaz/run-tui (cmd buffer-name)
  (let ((buf (get-buffer buffer-name)))
    (if (and buf (get-buffer-process buf))
        (switch-to-buffer buf)
      (when buf (kill-buffer buf))
      (ansi-term cmd buffer-name)
      (set-process-sentinel
       (get-buffer-process (current-buffer))
       (lambda (proc _event)
         (when (memq (process-status proc) '(exit signal))
           (kill-buffer (process-buffer proc))))))))

(defun yaz/shell-command-to-string-or-buffer (cmd &optional buffer-name)
  (let* ((buf-name (or buffer-name "*shell output*"))
         (output (string-trim (shell-command-to-string cmd))))
    (if (and (< (length output) 200)
             (not (string-match-p "\n.*\n" output)))
        (message "%s" output)
      (with-current-buffer (get-buffer-create buf-name)
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert (propertize (format "command: %s\n" cmd)
                             'face '(:weight bold :inherit font-lock-keyword-face)))
          (insert (propertize "----------------------------\n"
                             'face 'shadow))
          (insert output)
          (goto-char (point-min))
          (read-only-mode 1)
          (local-set-key (kbd "q") 'quit-window)
          (display-buffer (current-buffer)))))))

(defun yaz/async-shell-reuse (cmd buffer-name)
  "run cmd async in buffer-name"
  (let ((buf (get-buffer buffer-name)))
    (when buf (kill-buffer buf)))
  (async-shell-command cmd buffer-name))

(defun yaz/htop ()
  "open htop in ansi-term."
  (interactive)
  (yaz/run-tui "/usr/bin/htop" "*htop*"))

(defun yaz/btop ()
  "open btop in ansi-term."
  (interactive)
  (yaz/run-tui "/usr/bin/btop" "*btop*"))

(defun yaz/ncdu ()
  "open ncdu (disk usage) in current directory."
  (interactive)
  (yaz/run-tui (format "/usr/bin/ncdu %s" default-directory) "*ncdu*"))

(defun yaz/lazygit ()
  "open lazygit in current directory."
  (interactive)
  (if (executable-find "lazygit")
      (yaz/run-tui "lazygit" "*lazygit*")
    (message "lazygit not found. install with: sudo pacman -s lazygit")))

(defun yaz/ranger ()
  "open ranger file manager."
  (interactive)
  (if (executable-find "ranger")
      (yaz/run-tui (format "ranger %s" default-directory) "*ranger*")
    (message "ranger not found. install with: sudo pacman -s ranger")))

(defun yaz/disk-usage ()
  "show disk usage summary."
  (interactive)
  (yaz/shell-command-to-string-or-buffer "df -h --output=source,fstype,size,used,avail,pcent,target | grep -v tmpfs" "*disk usage*"))

(defun yaz/disk-usage-here ()
  "show disk usage for current directory."
  (interactive)
  (yaz/shell-command-to-string-or-buffer
   (format "du -h --max-depth=1 %s | sort -hr" default-directory)
   "*disk usage (current)*"))

(defun yaz/memory-usage ()
  "show memory usage."
  (interactive)
  (yaz/shell-command-to-string-or-buffer "free -h" "*memory*"))

(defun yaz/cpu-info ()
  "show cpu information."
  (interactive)
  (yaz/shell-command-to-string-or-buffer "lscpu | grep -e '(model name|architecture|cpu\\(s\\)|thread|core|socket|mhz)'" "*cpu info*"))

(defun yaz/system-info ()
  "show comprehensive system information."
  (interactive)
  (let ((cmd "echo 'system' && uname -a && echo '\ncpu\n' && lscpu | grep -e '(model name|cpu\\(s\\))' && echo '\nmemory\n' && free -h && echo '\ndisk\n' && df -h / && echo '\nuptime\n' && uptime"))
    (yaz/shell-command-to-string-or-buffer cmd "*system info*")))

(defun yaz/ip-info ()
  "show network interface information."
  (interactive)
  (yaz/shell-command-to-string-or-buffer "ip -br -c addr" "*ip info*"))

(defun yaz/ip-public ()
  "show public ip address."
  (interactive)
  (yaz/shell-command-to-string-or-buffer "curl -s ifconfig.me" "*public ip*"))

(defun yaz/ports ()
  "show listening ports."
  (interactive)
  (yaz/shell-command-to-string-or-buffer "ss -tulpn" "*ports*"))

(defun yaz/network-connections ()
  "show active network connections."
  (interactive)
  (yaz/shell-command-to-string-or-buffer "ss -tunapo" "*network connections*"))

(defun yaz/ping-host ()
  "ping pong a host."
  (interactive)
  (let ((host (read-string "host to ping: " "8.8.8.8")))
    (yaz/async-shell-reuse (format "ping -c 4 %s" host) "*ping*")))

(defun yaz/open-terminal-here ()
  "open external terminal in current directory."
  (interactive)
  (let ((dir (or default-directory "~")))
    (apply #'start-process "terminal" nil yaz/terminal-command
           (append yaz/terminal-args (list dir)))))

(defun yaz/copy-to-clipboard (text)
  "copy text to system clipboard using xclip or wl-copy."
  (cond
   ((executable-find "xclip")
    (with-temp-buffer
      (insert text)
      (call-process-region (point-min) (point-max) "xclip" nil nil nil "-selection" "clipboard")))
   ((executable-find "wl-copy")
    (with-temp-buffer
      (insert text)
      (call-process-region (point-min) (point-max) "wl-copy")))
   (t (message "no clipboard utility found (xclip or wl-copy)"))))

(defun yaz/update-system ()
  "update system packages (arch linux)."
  (interactive)
  (when (y-or-n-p "update system packages? ")
    (yaz/async-shell-reuse "sudo pacman -syu" "*system update*")))

(defun yaz/journal-logs ()
  "show recent systemd journal logs."
  (interactive)
  (yaz/shell-command-to-string-or-buffer "journalctl -xe -n 100 --no-pager" "*journal*"))

(provide 'yaz-system)
;;; yaz-system.el ends here
