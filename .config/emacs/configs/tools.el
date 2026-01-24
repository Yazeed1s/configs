;;; tools.el -*- lexical-binding: t; -*-
;;; Code:

(use-package nov
  :ensure t
  :defer t
  :mode ("\\.epub\\'" . nov-mode)
  :hook (nov-mode . (lambda () (display-line-numbers-mode -1)))
  :custom
  (nov-text-width 90)
  :config
  (defun yaz/nov-font-setup ()
    (face-remap-add-relative 'variable-pitch
                             :family "jetbrainsmono nf"
                             :height 110
                             :weight 'regular)
    (visual-line-mode 1)
    (olivetti-mode 1))
  (add-hook 'nov-mode-hook #'yaz/nov-font-setup))



(add-hook 'text-mode-hook #'flyspell-mode)

(provide 'tools)
;;; tools.el ends here
