;;; yaz-file-utils.el --- file operation utilities -*- lexical-binding: t; -*-

;;; Code:

(defun yaz/copy-file-path ()
  "copy current buffer's file path to clipboard."
  (interactive)
  (if-let ((path (or buffer-file-name default-directory)))
      (progn
        (kill-new path)
        (message "copied: %s" path))
    (message "no file associated with buffer")))

(defun yaz/copy-file-name ()
  "copy current buffer's file name to clipboard."
  (interactive)
  (if-let ((name (file-name-nondirectory (or buffer-file-name (buffer-name)))))
      (progn
        (kill-new name)
        (message "copied: %s" name))
    (message "no file name available")))

(defun yaz/copy-file-name-base ()
  "copy current file name without extension."
  (interactive)
  (if-let ((name (file-name-sans-extension
                  (file-name-nondirectory (or buffer-file-name (buffer-name))))))
      (progn
        (kill-new name)
        (message "copied: %s" name))
    (message "no file name available")))

(defun yaz/copy-directory-path ()
  "copy current file's directory path."
  (interactive)
  (if-let ((dir (file-name-directory (or buffer-file-name default-directory))))
      (progn
        (kill-new dir)
        (message "copied: %s" dir))
    (message "no directory available")))

(defun yaz/delete-file-and-buffer ()
  "kill buffer and delete associated file."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not filename)
        (message "buffer not visiting a file")
      (when (y-or-n-p (format "delete %s? " (file-name-nondirectory filename)))
        (delete-file filename)
        (kill-buffer (current-buffer))
        (message "deleted: %s" filename)))))

(defun yaz/rename-file-and-buffer ()
  "rename current file and buffer."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not filename)
        (message "buffer not visiting a file")
      (let ((new-name (read-file-name "new name: "
                                      (file-name-directory filename)
                                      nil nil
                                      (file-name-nondirectory filename))))
        (cond
         ((vc-backend filename)
          (vc-rename-file filename new-name))
         (t
          (rename-file filename new-name t)
          (set-visited-file-name new-name t t)))
        (message "renamed to: %s" new-name)))))

(defun yaz/move-file-to-trash ()
  "move current file to trash."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not filename)
        (message "buffer not visiting a file")
      (when (y-or-n-p (format "move %s to trash? " (file-name-nondirectory filename)))
        (move-file-to-trash filename)
        (kill-buffer (current-buffer))
        (message "moved to trash: %s" filename)))))

(defun yaz/make-file-executable ()
  "make current file executable."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not filename)
        (message "buffer not visiting a file")
      (chmod filename (logior (file-modes filename) #o111))
      (message "made executable: %s" filename))))

(defun yaz/open-file-externally ()
  "open current file with external application."
  (interactive)
  (let ((file (or buffer-file-name
                  (read-file-name "file: "))))
    (start-process "open-external" nil "xdg-open" file)
    (message "opened externally: %s" file)))

(defun yaz/reveal-in-file-manager ()
  "reveal current file in system file manager."
  (interactive)
  (let ((file (or buffer-file-name default-directory)))
    (start-process "reveal-file" nil "xdg-open" (file-name-directory file))
    (message "here: %s" file)))

(provide 'yaz-file-utils)
;;; yaz-file-utils.el ends here
