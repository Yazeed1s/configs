;;; writing.el --- Markdown, LaTeX, and document writing -*- lexical-binding: t; -*-

;;; Code:

(use-package markdown-mode
  :ensure t
  :mode (("readme\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init
  (setq markdown-command "pandoc -f markdown -t html5 --mathjax --highlight-style=pygments")
  :custom
  (markdown-hide-urls nil)
  (markdown-hide-markup nil)
  (markdown-fontify-code-blocks-natively t)
  (markdown-enable-math t)
  (markdown-enable-wiki-links t)

  (markdown-code-lang-modes
   '(("ocaml" . tuareg-mode)
     ("elisp" . emacs-lisp-mode)
     ("ditaa" . artist-mode)
     ("asymptote" . asy-mode)
     ("dot" . fundamental-mode)
     ("sqlite" . sql-mode)
     ("calc" . fundamental-mode)
     ("c" . C-mode)
     ("cpp" . c++-mode)
     ("c++" . c++-mode)
     ("screen" . shell-script-mode)
     ("shell" . sh-mode)
     ("bash" . sh-mode)
     ("python" . python-mode)
     ("py" . python-mode)
     ("javascript" . js-mode)
     ("js" . js-mode)
     ("typescript" . typescript-mode)
     ("ts" . typescript-mode)
     ("rust" . rust-mode)
     ("go" . go-mode)
     ("json" . json-mode)
     ("yaml" . yaml-mode)
     ("html" . html-mode)
     ("css" . css-mode)))

  (markdown-indent-on-enter 'indent-and-new-item)
  (markdown-asymmetric-header t)
  (markdown-nested-imenu-heading-index t)
  (markdown-list-indent-width 2)

  :config
  (defun yaz/markdown-setup ()
    (setq-local fill-column 80)
    (auto-fill-mode -1)
    (visual-line-mode 1)
    (flyspell-mode 1))

  (add-hook 'markdown-mode-hook #'yaz/markdown-setup)
  (add-hook 'gfm-mode-hook #'yaz/markdown-setup))



(use-package tex
  :ensure auctex
  :mode ("\\.tex\\'" . LaTeX-mode)
  :hook ((LaTeX-mode . turn-on-reftex)
         (LaTeX-mode . LaTeX-math-mode)
         (LaTeX-mode . flyspell-mode)
         (LaTeX-mode . TeX-source-correlate-mode)
         (LaTeX-mode . visual-line-mode))
  :custom
  (TeX-auto-save t)
  (TeX-parse-self t)
  (TeX-master nil)
  (TeX-engine 'xetex)

  (TeX-PDF-mode t)
  (TeX-view-program-selection '((output-pdf "pdf tools")))
  (TeX-source-correlate-start-server t)

  (TeX-error-overview-open-after-TeX-run t)

  (LaTeX-electric-left-right-brace t)
  (TeX-electric-sub-and-superscript t)
  (TeX-electric-math '("$" . "$"))

  (LaTeX-indent-level 2)
  (LaTeX-item-indent 0)
  (TeX-brace-indent-level 2)

  :config
  (setq-default TeX-master nil)

  (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)

  (defun yaz/latex-compile ()
    "compile latex document."
    (interactive)
    (save-buffer)
    (TeX-command "latex" 'TeX-master-file)))

(use-package latex-preview-pane
  :ensure t
  :after tex
  :commands latex-preview-pane-mode
  :custom
  (latex-preview-pane-multifile-mode 'auctex))

(use-package pdf-tools
  :ensure t
  :magic ("%pdf" . pdf-view-mode)
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :hook (pdf-view-mode . (lambda ()
                           (display-line-numbers-mode -1)
                           (pdf-view-midnight-minor-mode 1)))
  :bind (:map pdf-view-mode-map
              ("j" . pdf-view-next-line-or-next-page)
              ("k" . pdf-view-previous-line-or-previous-page)
              ("j" . pdf-view-next-page)
              ("k" . pdf-view-previous-page)
              ("h" . image-backward-hscroll)
              ("l" . image-forward-hscroll)
              ("g" . pdf-view-first-page)
              ("g" . pdf-view-last-page)
              ("/" . isearch-forward)
              ("?" . isearch-backward)
              ("n" . isearch-repeat-forward)
              ("n" . isearch-repeat-backward)
              ("o" . pdf-outline)
              ("m" . yaz/pdf-toggle-comfortable-theme)
              ("f" . pdf-view-fit-page-to-window)
              ("w" . pdf-view-fit-width-to-window)
              ("+" . pdf-view-enlarge)
              ("-" . pdf-view-shrink)
              ("0" . pdf-view-scale-reset)
              ("r" . pdf-view-revert-buffer)
              ("C-s" . isearch-forward)
              ("a" . pdf-annot-add-highlight-markup-annotation)
              ("d" . pdf-annot-delete)
              ("t" . pdf-annot-add-text-annotation))
  :custom
  (pdf-view-display-size 'fit-width)
  (pdf-view-external-viewer "zathura")
  (pdf-view-image-width nil)
  (pdf-view-use-imagemagick nil)
  (pdf-view-resize-factor 1)
  (pdf-view-use-scaling t)
  (pdf-view-use-imagemagick nil)

  (pdf-view-midnight-colors '("#1a1410" . "#d4cec0"))

  (pdf-annot-default-annotation-properties
   '((t (label . "yaz"))
     (text (icon . "note") (color . "#e07a5f"))
     (highlight (color . "#ffd966"))
     (underline (color . "#4a7c8e"))
     (squiggly (color . "#f4a261"))
     (strike-out (color . "#c55a5a"))))

  :config
  (setq pdf-info-epdfinfo-program (expand-file-name "build/server/epdfinfo" (file-name-directory (locate-library "pdf-tools"))))
  (pdf-tools-install)

  (setq pdf-view-continuous t))

(use-package olivetti
  :ensure t
  :custom
  (olivetti-body-width 0.6)
  (olivetti-minimum-body-width 80))

(defun yaz/pdf-toggle-comfortable-theme ()
  (interactive)
  (if (equal pdf-view-midnight-colors '("#1a1410" . "#d4cec0"))
      (progn
        (setq pdf-view-midnight-colors '("#f4f1ea" . "#2f2b28"))
        (pdf-view-redisplay)
        (message "pdf: warm dark mode"))
    (progn
      (setq pdf-view-midnight-colors '("#1a1410" . "#d4cec0"))
      (pdf-view-redisplay)
      (message "pdf: dimmer white mode"))))

(with-eval-after-load 'pdf-view
  (define-key pdf-view-mode-map (kbd "C-c m") #'yaz/pdf-toggle-comfortable-theme)
  (define-key pdf-view-mode-map (kbd "M-m") #'yaz/pdf-toggle-comfortable-theme))

(defun yaz/pdf-open-in-zathura ()
  (interactive)
  (let ((file (or (buffer-file-name)
                  (and (derived-mode-p 'pdf-view-mode)
                       (pdf-view-buffer-file-name)))))
    (unless (and file (file-exists-p file))
      (user-error "no pdf file associated with this buffer"))
    (start-process "zathura" nil "zathura" file)))

(with-eval-after-load 'pdf-view
  (define-key pdf-view-mode-map (kbd "C-c z")
              #'yaz/pdf-open-in-zathura))

(with-eval-after-load 'pdf-view
  (add-hook 'pdf-view-mode-hook
            (lambda ()
              (setq-local evil-normal-state-cursor nil)
              (setq-local evil-insert-state-cursor nil)
              (setq-local evil-visual-state-cursor nil)
              (setq-local evil-motion-state-cursor nil)

              (set-window-parameter nil 'cursor-type nil)

              (blink-cursor-mode -1))))

(pixel-scroll-precision-mode 1)
(setq auto-window-vscroll nil
      scroll-conservatively 101)

(with-eval-after-load 'pdf-view
  (setq pdf-view-scroll-step 1.0))

(with-eval-after-load 'evil
  (evil-set-initial-state 'pdf-view-mode 'emacs))

(with-eval-after-load 'pdf-view
  (add-hook 'pdf-view-mode-hook
            (lambda ()
              (display-line-numbers-mode -1)
              (hl-line-mode -1)
              (setq-local show-trailing-whitespace nil))))
(defun yaz/pdf-export-annotations ()
  (interactive)
  (let* ((annots (pdf-annot-getannots))
         (buf (get-buffer-create "*pdf annotations*")))
    (with-current-buffer buf
      (erase-buffer)
      (org-mode)
      (insert "#+title: pdf annotations\n")
      (insert (format "#+date: %s\n\n" (format-time-string "%Y-%M-%d")))
      (dolist (annot annots)
        (let ((type (pdf-annot-get annot 'type))
              (page (pdf-annot-get annot 'page))
              (contents (pdf-annot-get annot 'contents)))
          (when contents
            (insert (format "* page %d - %s\n%s\n\n" page type contents))))))
    (switch-to-buffer buf)))

(use-package pandoc-mode
  :ensure t
  :config
  (add-hook 'pandoc-mode-hook 'pandoc-load-default-settings))

(defun yaz/markdown-to-pdf ()
  "export current markdown buffer to pdf using pandoc."
  (interactive)
  (let* ((input (buffer-file-name))
         (output (concat (file-name-sans-extension input) ".pdf")))
    (shell-command
     (format "pandoc '%s' -o '%s' --pdf-engine=xelatex -v geometry:margin=1in"
             input output))
    (message "exported to %s" output)
    (when (y-or-n-p "open pdf? ")
      (find-file output))))

(defun yaz/markdown-to-docx ()
  "export current markdown buffer to docx using pandoc."
  (interactive)
  (let* ((input (buffer-file-name))
         (output (concat (file-name-sans-extension input) ".docx")))
    (shell-command
     (format "pandoc '%s' -o '%s'" input output))
    (message "exported to %s" output)))

(defun yaz/markdown-to-html ()
  "export current markdown buffer to html using pandoc."
  (interactive)
  (let* ((input (buffer-file-name))
         (output (concat (file-name-sans-extension input) ".html")))
    (shell-command
     (format "pandoc '%s' -o '%s' --standalone --toc --css=https://cdnjs.cloudflare.com/ajax/libs/github-markdown-css/5.2.0/github-markdown.min.css"
             input output))
    (message "exported to %s" output)
    (when (y-or-n-p "open in browser? ")
      (browse-url output))))

(defun yaz/latex-to-org ()
  "export current latex buffer to org mode using pandoc."
  (interactive)
  (let* ((input (buffer-file-name))
         (output (concat (file-name-sans-extension input) ".org")))
    (shell-command
     (format "pandoc '%s' -f latex -t org -o '%s' --wrap=none"
             input output))
    (message "exported to %s" output)
    (when (y-or-n-p "open org file? ")
      (find-file output))))

(with-eval-after-load 'markdown-mode
  (define-key markdown-mode-map (kbd "C-c C-e p") #'yaz/markdown-to-pdf)
  (define-key markdown-mode-map (kbd "C-c C-e d") #'yaz/markdown-to-docx)
  (define-key markdown-mode-map (kbd "C-c C-e h") #'yaz/markdown-to-html)
)

(with-eval-after-load 'tex
  (define-key LaTeX-mode-map (kbd "C-c C-c") #'TeX-command-master)
  (define-key LaTeX-mode-map (kbd "C-c C-v") #'TeX-view))

(with-eval-after-load 'imenu-list
  (global-set-key (kbd "C-'") #'imenu-list-smart-toggle))

(provide 'writing)
;;; writing.el ends here
