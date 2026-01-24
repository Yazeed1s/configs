;;; yaz-utils.el --- General utility functions -*- lexical-binding: t; -*-

;;; Code:

(defun yaz/increment-number-at-point (&optional arg)
  "increment number at point by arg (default 1)."
  (interactive "p")
  (save-excursion
    (skip-chars-backward "0-9")
    (or (looking-at "[0-9]+")
        (error "no number at point"))
    (replace-match (number-to-string (+ (or arg 1) (string-to-number (match-string 0)))))))

(defun yaz/decrement-number-at-point (&optional arg)
  "decrement number at point by arg (default 1)."
  (interactive "p")
  (yaz/increment-number-at-point (- (or arg 1))))

(defun yaz/duplicate-line ()
  "duplicate current line."
  (interactive)
  (let ((col (current-column)))
    (save-excursion
      (move-beginning-of-line 1)
      (kill-line)
      (yank)
      (newline)
      (yank))
    (forward-line 1)
    (move-to-column col)))

(defun yaz/move-line-up ()
  "move current line up."
  (interactive)
  (transpose-lines 1)
  (forward-line -2)
  (indent-according-to-mode))

(defun yaz/move-line-down ()
  "move current line down."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
  (indent-according-to-mode))

(defun yaz/join-line-below ()
  "join line below to current line."
  (interactive)
  (forward-line 1)
  (join-line))

(defun yaz/join-line-above ()
  "join current line to line above."
  (interactive)
  (join-line))

(defun yaz/indent-buffer ()
  "indent entire buffer."
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max)))
  (message "buffer indented"))

(defun yaz/cleanup-buffer ()
  "cleanup buffer: indent, untabify, delete trailing whitespace."
  (interactive)
  (save-excursion
    (yaz/indent-buffer)
    (untabify (point-min) (point-max))
    (delete-trailing-whitespace))
  (message "buffer cleaned up"))

(defun yaz/count-words-buffer ()
  "count words, characters, and lines in buffer."
  (interactive)
  (let ((words (count-words (point-min) (point-max)))
        (chars (- (point-max) (point-min)))
        (lines (count-lines (point-min) (point-max))))
    (message "words: %d | chars: %d | lines: %d" words chars lines)))

(defun yaz/delete-blank-lines ()
  "delete all blank lines in buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (flush-lines "^\\s-*$"))
  (message "deleted all blank lines"))

(defun yaz/sort-lines-buffer ()
  "sort all lines in buffer."
  (interactive)
  (save-excursion
    (sort-lines nil (point-min) (point-max)))
  (message "buffer lines sorted"))

(defun yaz/uniquify-lines ()
  "remove duplicate lines in buffer."
  (interactive)
  (save-excursion
    (delete-duplicate-lines (point-min) (point-max)))
  (message "removed duplicate lines"))

(provide 'yaz-utils)
;;; yaz-utils.el ends here
