;;; init.el -*- lexical-binding: t; -*-


(require 'package)

(setq package-archives
      '(("melpa"        . "https://melpa.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")
        ("gnu"          . "https://elpa.gnu.org/packages/")
        ("nongnu"       . "https://elpa.nongnu.org/nongnu/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)

(setq use-package-always-ensure t        
      use-package-verbose nil             
      use-package-compute-statistics nil  
      use-package-expand-minimally t)     

(defvar yaz/config-dir (expand-file-name "configs" user-emacs-directory)
  "where the config modules live")

(add-to-list 'load-path yaz/config-dir)

(defvar yaz/lisp-dir (expand-file-name "lisp" user-emacs-directory)
  "where my custom functions live")

(add-to-list 'load-path yaz/lisp-dir)

;; load everything
;; order matters here
(require 'core)              
(require 'completion)        
(require 'yaz-utils)         
(require 'yaz-file-utils)    
(require 'yaz-system)        
(require 'proc-manager)      
(require 'docker-manager)    
(require 'ui)                
(require 'evil-config)       
(require 'keybindings)       
(require 'git-config)        
;; (require 'lsp-config)     
(require 'eglot-config)      
(require 'writing)           
(require 'tools)             
(require 'tree-config)           
;; (require 'inhibit-mouse-config)
(require 'org-config)     
       
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(when (file-exists-p custom-file)
  (load custom-file 'noerror 'nomessage))

(message "loaded in %.2fs with %d gc runs"
         (float-time (time-subtract after-init-time before-init-time))
         gcs-done)

;;; init.el ends here
