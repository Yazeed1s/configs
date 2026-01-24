;;; early-init.el -*- lexical-binding: t; -*-


(setq gc-cons-threshold most-positive-fixnum  ; GC threshold during startup
      gc-cons-percentage 0.6)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 16 1024 1024)  ; 16MB
                  gc-cons-percentage 0.1)
            (run-with-idle-timer 5 t #'garbage-collect)))

(setq inhibit-startup-message t
      inhibit-startup-echo-area-message t
      initial-scratch-message nil
      inhibit-compacting-font-caches t
      read-process-output-max (* 1024 1024)
      process-adaptive-read-buffering nil)
      
(setq-default bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right)
(setq bidi-inhibit-bpa t)


(defvar yaz/file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq file-name-handler-alist yaz/file-name-handler-alist)))

(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq package-enable-at-startup nil)
(setq package-quickstart nil)

;;; early-init.el ends here
