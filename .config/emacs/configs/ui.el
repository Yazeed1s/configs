;;; ui.el --- UI settings -*- lexical-binding: t; -*-

;;; Code:

(global-display-line-numbers-mode 1)
(setq display-line-numbers-type 'relative)

(global-visual-line-mode 1)
(column-number-mode 1)
(pixel-scroll-mode 1)
(pixel-scroll-precision-mode t)

(electric-pair-mode 1)
(add-to-list 'default-frame-alist '(alpha-background . 97))
(set-face-italic 'italic nil)
(setq-default indicate-empty-lines t)
(define-fringe-bitmap 'tilde [0 0 0 113 219 142 0 0] nil nil 'center)
(setcdr (assq 'empty-line fringe-indicator-alist) 'tilde)
(set-fringe-bitmap-face 'tilde 'font-lock-comment-face)

(set-face-attribute 'default nil
                    :family "jetbrainsmono nf"
                    :height 110
                    :weight 'regular)

(set-face-attribute 'mode-line nil
                    :background "#212121"
                    :family "jetbrainsmono nf"
                    :height 110
                    :box '(:line-width 4 :color "#212121")
                    :overline nil
                    :underline nil)

(add-to-list 'custom-theme-load-path "~/.config/emacs/themes/")
(setq custom-safe-themes t)

(use-package base16-theme :ensure t :defer t)
(load-theme 'gruvbox-mat-medium t)

(defvar yaz/no-line-numbers-modes
  '(org-mode-hook
    pdf-view-mode-hook
    term-mode-hook
    eshell-mode-hook
    dashboard-mode-hook
    nov-mode-hook
    markdown-mode-hook
    gfm-mode-hook
    markdown-view-mode-hook
    gfm-view-mode-hook)
  "modes where line numbers should be disabled.")

(dolist (mode yaz/no-line-numbers-modes)
  (add-hook mode (lambda () (display-line-numbers-mode -1))))

(use-package dashboard
  :ensure t
  :init
  (setq dashboard-set-heading-icons nil
        dashboard-set-file-icons nil
        dashboard-projects-backend 'project-el
        dashboard-banner-logo-title "hey!"
        dashboard-startup-banner 2
        dashboard-center-content nil
        dashboard-items '((recents   . 5)
                          (projects  . 5)
                          (agenda    . 10))
        dashboard-item-shortcuts '((recents   . "r")
                                   (projects  . "p")
                                   (agenda    . "a"))
        dashboard-item-generators
        '((recents . dashboard-insert-recents)
          (projects . dashboard-insert-projects)
          (agenda . dashboard-insert-agenda))
        dashboard-week-agenda t
        dashboard-filter-agenda-entry 'dashboard-no-filter-agenda
        dashboard-agenda-sort-strategy '(time-up priority-down)
        dashboard-agenda-prefix-format " %i %-12:c %s "
        dashboard-match-agenda-entry "TODO=\"TODO\"|TODO=\"IN-PROGRESS\"|scheduled|deadline")
  :config
  (dashboard-setup-startup-hook))

(use-package diminish
  :ensure t)

(with-eval-after-load 'diminish
  (diminish 'abbrev-mode)
  (diminish 'visual-line-mode)
  (with-eval-after-load 'which-key (diminish 'which-key-mode))
  (with-eval-after-load 'yasnippet (diminish 'yas-minor-mode))
  (with-eval-after-load 'git-gutter (diminish 'git-gutter-mode))
  (with-eval-after-load 'tree-sitter (diminish 'tree-sitter-mode))
  (with-eval-after-load 'evil-collection (diminish 'evil-collection-unimpaired-mode))
  (with-eval-after-load 'drag-stuff (diminish 'drag-stuff-mode)))


(provide 'ui)
;;; ui.el ends here
