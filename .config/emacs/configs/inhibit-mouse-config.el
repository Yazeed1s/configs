;;; inhibit-mouse-config.el -*- lexical-binding: t; -*-

(use-package inhibit-mouse
  :ensure t
  :custom
  (inhibit-mouse-adjust-mouse-highlight t)
  (inhibit-mouse-adjust-show-help-function t)
  :config
  (if (daemonp)
      (add-hook 'server-after-make-frame-hook #'inhibit-mouse-mode)
    (inhibit-mouse-mode 1)))

(setq inhibit-mouse-button-numbers '(1 2 3 4 5))

(setq inhibit-mouse-button-events '("mouse"
                                    "up-mouse"
                                    "down-mouse"
                                    "drag-mouse"))

(setq inhibit-mouse-misc-events '("wheel-up"
                                  "wheel-down"
                                  "wheel-left"
                                  "wheel-right"
                                  "pinch"))

(setq inhibit-mouse-multipliers '("double" "triple"))

(setq inhibit-mouse-key-modifiers '((control)
                                    (meta)
                                    (shift)
                                    (control meta shift)
                                    (control meta)
                                    (control shift)
                                    (meta shift)))

(add-hook 'inhibit-mouse-mode-hook
          #'(lambda()
              (when (fboundp 'context-menu-mode)
                (if (bound-and-true-p inhibit-mouse-mode)
                    (context-menu-mode -1)
                  (context-menu-mode 1)))))

(add-hook 'inhibit-mouse-mode-hook
          #'(lambda()
              (when (fboundp 'tooltip-mode)
                (if (bound-and-true-p inhibit-mouse-mode)
                    (tooltip-mode -1)
                  (tooltip-mode 1)))))

(add-hook 'inhibit-mouse-mode-hook
          #'(lambda()
              (unless (and
                       (eq window-system 'mac)
                       (bound-and-true-p mac-carbon-version-string))
                (when (fboundp 'pixel-scroll-precision-mode)
                  (if (bound-and-true-p inhibit-mouse-mode)
                      (pixel-scroll-precision-mode -1)
                    (pixel-scroll-precision-mode 1))))))

(defun yaz/toggle-mouse-inhibit ()
  (interactive)
  (if (bound-and-true-p inhibit-mouse-mode)
      (progn
        (inhibit-mouse-mode -1)
        (message "mouse enabled"))
    (progn
      (inhibit-mouse-mode 1)
      (message "mouse disabled"))))

(provide 'inhibit-mouse-config)
;;; inhibit-mouse-config.el ends here
