;;; git-config.el -*- lexical-binding: t; -*-

(use-package git-gutter
  :ensure t
  :init
  (setq git-gutter:modified-sign "~"
        git-gutter:added-sign "+"
        git-gutter:deleted-sign "-"
        git-gutter:update-interval 0.02)
  :config
  (global-git-gutter-mode 1)
  (set-face-foreground 'git-gutter:modified "#b5c2b5")
  (set-face-foreground 'git-gutter:added "#a7bc99")
  (set-face-foreground 'git-gutter:deleted "#cc9393"))

(provide 'git-config)
;;; git-config.el ends here
