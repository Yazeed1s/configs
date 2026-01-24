;;; evil-config.el -*- lexical-binding: t; -*-

;;; Code:

(use-package evil
  :ensure t
  :demand t
  :init
  (setq evil-want-integration t
        evil-want-keybinding nil
        evil-want-C-u-scroll t
        evil-want-C-i-jump t
        evil-want-Y-yank-to-eol t
        evil-symbol-word-search t)
  :config
  (evil-mode 1))

(use-package evil-surround
  :ensure t
  :after evil
  :config
  (global-evil-surround-mode 1))

(use-package evil-multiedit
  :ensure t
  :after evil
  :config
  (setq evil-multiedit-scope 'buffer
        evil-multiedit-follow-matches t)
  :bind (:map evil-normal-state-map
              ("C->" . evil-multiedit-match-and-next)
              ("C-<" . evil-multiedit-match-and-prev)
              ("C-." . evil-multiedit-match-all)
         :map evil-visual-state-map
              ("M->" . evil-multiedit-match-and-next)
              ("M-<" . evil-multiedit-match-and-prev)
              ("M-." . evil-multiedit-match-all)))

(use-package evil-collection
  :ensure t
  :after evil
  :config
  (evil-collection-init))

(with-eval-after-load 'dired
  (evil-define-key 'normal dired-mode-map
    (kbd "h") 'dired-up-directory
    (kbd "l") 'dired-open-file))

(use-package drag-stuff
  :ensure t
  :config
  (drag-stuff-global-mode 1)
  (drag-stuff-define-keys))

(provide 'evil-config)
;;; evil-config.el ends here
