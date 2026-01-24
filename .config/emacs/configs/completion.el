;;; completion.el -*- lexical-binding: t; -*-


;;; code

(setq tab-always-indent 'complete)
(setq completion-cycle-threshold 3)



(use-package orderless
  :ensure t
  :defer t
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package multiple-cursors
  :ensure t
  :defer t)
  


(use-package emacs
  :init
  (setq tab-always-indent 'complete)
  (setq completion-cycle-threshold 3)
  :config
  (setq tab-always-indent 'complete))

(use-package ivy
  :ensure t
  :diminish
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t
        ivy-count-format "(%d/%d) "
        ivy-wrap t
        ivy-height 15
        ivy-fixed-height-minibuffer t
        ivy-initial-inputs-alist nil
        ivy-re-builders-alist '((t . ivy--regex-ignore-order))))

(use-package counsel
  :ensure t
  :after ivy
  :diminish
  :config
  (counsel-mode 1))

(use-package swiper
  :ensure t
  :after ivy
  :bind (("C-s" . swiper)
         ("C-r" . swiper-backward)))

(use-package company
  :ensure t
  :diminish
  :hook (prog-mode . company-mode)
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0)
  (company-tooltip-align-annotations t)
  (company-show-numbers t))

(use-package which-key
  :ensure t
  :defer 0.3
  :config
  (which-key-mode 1)
  (setq which-key-side-window-location 'bottom
        which-key-sort-order 'which-key-local-then-key-order
        which-key-sort-uppercase-first nil
        which-key-add-column-padding 3
        which-key-max-display-columns nil
        which-key-min-display-lines 4
        which-key-side-window-slot 10
        which-key-side-window-max-height 0.25
        which-key-idle-delay 0.15
        which-key-max-description-length 25
        which-key-allow-imprecise-window-fit nil
        which-key-separator " -> "
        which-key-prefix-prefix "+"))

(use-package imenu-list
  :ensure t
  :defer t
  :config
  (setq imenu-list-position 'right
        imenu-list-auto-resize t
        imenu-list-idle-update-delay 1
        imenu-list-focus-after-activation t))

(use-package yasnippet
  :ensure t
  :hook ((prog-mode . yas-minor-mode)
         (text-mode . yas-minor-mode)
         (conf-mode . yas-minor-mode))
  :config
  (yas-reload-all))

(use-package yasnippet-snippets
  :ensure t
  :after yasnippet)

(provide 'completion)
;;; completion.el ends here
