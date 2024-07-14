;; (setq idle-update-delay 2)
(setq inhibit-startup-message t)
(setq gc-cons-threshold 63000000
      gc-cons-percentage 0.6)

(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
;;(add-to-list 'custom-theme-load-path "~/.config/emacs/themes/")
;;(setq custom-safe-themes t)
;;(load-theme 'zenburn t)
