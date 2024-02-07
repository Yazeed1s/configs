;;; init.el starts here


;;----------------------------------------------------
;; general configs
;;----------------------------------------------------
(abbrev-mode -1)
(global-set-key [escape] 'keyboard-escape-quit)
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(delete-selection-mode 1)    
(electric-pair-mode 1)         
(global-display-line-numbers-mode 1) 
(global-visual-line-mode -1)
(menu-bar-mode -1)           
(scroll-bar-mode -1)         
(tool-bar-mode -1)
(add-to-list 'default-frame-alist '(alpha-background . 95)) 
(global-eldoc-mode -1)
(eldoc-mode -1)
(set-face-italic-p 'italic nil)
(pixel-scroll-precision-mode t)
(visual-line-mode -1)
(fset 'yes-or-no-p 'y-or-n-p)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-language-environment 'utf-8)
(prefer-coding-system 'utf-8)
;; vim tilde '~'
(define-fringe-bitmap 'tilde [0 0 0 113 219 142 0 0] nil nil 'center)
(setcdr (assq 'empty-line fringe-indicator-alist) 'tilde)
(set-fringe-bitmap-face 'tilde 'font-lock-comment-face)
(set-face-attribute 'default nil :family "JetBrainsMono NF" :height 130 :weight 'regular)
;; (set-frame-font "JetBrainsMono NF 13" nil t)
;; (add-to-list 'default-frame-alist '(font . "JetBrainsMono NF 13"))
(set-face-attribute 'mode-line nil
                    :background "#212121"
		            :family "JetBrainsMono NF"
		            :height 130
                    :box '(:line-width 4 :color "#212121")
                    :overline nil
                    :underline nil)

(set-face-attribute 'mode-line-inactive nil
                    :background "#212121"
		            :family "JetBrainsMono NF"
		            :height 130
                    :box '(:line-width 4 :color "#212121")
                    :overline nil
                    :underline nil)

(set-face-attribute 'minibuffer-prompt nil
                    :background "#212121"
		            :family "JetBrainsMono NF"
		            :height 130
                    :box '(:line-width 4 :color "#212121")
                    :overline nil
                    :underline nil)


(setq ring-bell-function 'ignore
      org-edit-src-content-indentation 0
      use-file-dialog nil  
      use-dialog-box nil  
      pop-up-windows nil 
      eldoc-echo-area-use-multiline-p nil
      eldoc-documentation-strategy 'eldoc-documentation-compose
      eglot-ignored-server-capabilities '(:documentHighlightProvider)
      eglot-ignored-server-capabilities '(:inlayHintProvider)
      eglot-ignored-server-capabilities '(:hoverProvider)
      eldoc-echo-area-prefer-doc-buffer t
      display-line-numbers-type 'relative
      confirm-kill-emacs nil
      backup-inhibited t
      create-lockfiles nil
      locale-coding-system 'utf-8
      c-tab-always-indent t)

(setq-default indent-tabs-mode t
	      line-spacing 0.17
	      abbrev-mode -1
	      indicate-empty-lines t
	      tab-width 4
	      standard-indent 4)

;;(setq display-line-numbers-type :relative)
;;(keymap-global-set "c-<right>" 'next-buffer)
;;(keymap-global-set "c-<left>" 'previous-buffer)
;;(setq eglot-managed-mode-hook (list (lambda () (eldoc-mode -1))))

;; (defun indent-relative (&optional arg)
;; 	"Newline and indent 4 spaces relative to previous line.  With
;; 	C-u, indent to same level as previous line."
;; 	(interactive "P")
;; 	(let* ((amount (if arg 0 4))
;; 			(indent (+ amount (save-excursion
;; 								(back-to-indentation)
;; 								(current-column)))))
;; 		(newline 1)
;; 		(insert (make-string indent ?\s))))
;; (global-set-key (kbd "<return>") #'indent-relative)

;;----------------------------------------------------
;; package config
;;----------------------------------------------------
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)
(unless package-archive-contents
	(package-refresh-contents))

(unless (package-installed-p 'use-package)
	(package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)
(setq use-package-verbose t)
(setq use-package-compute-statistics t)
(use-package auto-package-update
  :defer t
  :custom
  (auto-package-update-interval 7)
  (auto-package-update-prompt-before-update t)
  (auto-package-update-hide-results t)
  :config
  (auto-package-update-maybe)
  (auto-package-update-at-time "09:00"))


;;----------------------------------------------------
;; ui tweaks
;;----------------------------------------------------
(use-package doom-themes
	:demand
	:config
	(setq doom-themes-enable-bold t   
	doom-themes-enable-italic nil)) 

(add-to-list 'custom-theme-load-path "~/.config/emacs/themes/")
(setq custom-safe-themes t)
(load-theme 'doom-zenburnv2)

;;-----------------------------------------------------
;; mode-line
;;-----------------------------------------------------
(use-package mood-line
  :ensure t
  :init 
  :config
  (mood-line-mode)
  ;; Use pretty Fira Code-compatible glyphs
  :custom
  (mood-line-glyph-alist mood-line-glyphs-fira-code))

(setq mood-line-format
     (mood-line-defformat
   :left
   (((mood-line-segment-modal)            . " ")
    ((or (mood-line-segment-buffer-status)
         (mood-line-segment-client)
         " ")                             . " ")
    ((mood-line-segment-project)          . "/")
    ((mood-line-segment-buffer-name)      . "  ")
    ((mood-line-segment-anzu)             . "  ")
    ((mood-line-segment-multiple-cursors) . "  ")
    (mood-line-segment-cursor-position)
    #(":" 0 1 (face mood-line-unimportant))
    ((mood-line-segment-cursor-point)     . " ")
    ((mood-line-segment-region)           . " ")
    (mood-line-segment-scroll))
   :right
   (((mood-line-segment-indentation) . "  ")
    ((mood-line-segment-eol)         . "  ")
    ((mood-line-segment-encoding)    . "  ")
    ((mood-line-segment-vc)          . "  ")
    ((mood-line-segment-major-mode)  . "  ")
    ((mood-line-segment-misc-info)   . "  ")
    ((mood-line-segment-checker)     . "  ")
    ((mood-line-segment-process)     . "  "))))

;; (setq mood-line-glyph-alist mood-line-glyphs-ascii)
;; (use-package doom-modeline
;; 	;;:ensure t
;; 	:init (doom-modeline-mode -1)
;; 	:config
;; 	(setq doom-modeline-height 25      ;; sets modeline height
;; 		;;doom-modeline-bar-width 2    ;; sets right bar width
;; 		doom-modeline-buffer-file-name-style 'relative-from-project))
;; 		;; doom-modeline-buffer-name t
;; 		;; doom-modeline-icon t
;; 		;;doom-modeline-github t
;; 		;;doom-modeline-github-interval (* 30 60)))

;;----------------------------------------------------
;; key table -> on SPC
;;----------------------------------------------------
(use-package which-key
	:init
	(which-key-mode 1)
	;; :diminish
	:config
	(setq which-key-side-window-location 'bottom
		which-key-sort-order #'which-key-key-order-alpha
		which-key-allow-imprecise-window-fit nil
		which-key-sort-uppercase-first nil
		which-key-add-column-padding 3
		which-key-max-display-columns nil
		which-key-min-display-lines 4
		which-key-side-window-slot 10
		which-key-side-window-max-height 0.25
		which-key-idle-delay 0.15
		which-key-max-description-length 25
		which-key-allow-imprecise-window-fit nil
		which-key-separator " → " ))


;;----------------------------------------------------
;; vim emulator
;;----------------------------------------------------
(use-package evil
	:init
	(setq evil-want-integration t)
	(setq evil-want-keybinding nil)
	(setq evil-want-C-u-scroll t)
	(setq evil-want-C-i-jump nil)
	:demand ; No lazy loading
	;;:defer t
	:config
    (evil-mode 1))

(use-package evil-collection
	:after evil
	:config
	(evil-collection-init))

;;----------------------------------------------------
;; some completion packages
;;----------------------------------------------------
(use-package counsel
	:after ivy
	:config 
	(counsel-mode)
	(setq ivy-initial-inputs-alist nil))

(use-package ivy
	:bind
	(("C-c C-r" . ivy-resume)
	("C-x B" . ivy-switch-buffer-other-window))
	:custom
	(setq ivy-use-virtual-buffers t)
	(setq ivy-count-format "(%d/%d) ")
	(setq enable-recursive-minibuffers t)
	:config
	(ivy-mode))

;; (use-package all-the-icons-ivy-rich
;;  	:ensure t
;;  	:init (all-the-icons-ivy-rich-mode 1))

(use-package ivy-rich
	:after ivy
	:ensure t
	:init (ivy-rich-mode 1))

(use-package company
	:defer nil
	:diminish
	:custom
	(company-begin-commands '(self-insert-command))
	(company-idle-delay .1)
	(company-minimum-prefix-length 2)
	(company-show-numbers t)
	(company-tooltip-align-annotations 't)
	(global-company-mode t))

;; (use-package company-box
;; 	:after company
;; 	:diminish
;; 	:hook (company-mode . company-box-mode))


;;----------------------------------------------------
;; key mappings
;;----------------------------------------------------
(use-package general
    ;; :after evil
    :demand t
 	:config
  	;; (general-evil-setup)
  	;; set up 'SPC' as the global leader key
	(general-create-definer leader-keys
    	:states '(normal insert visual emacs)
    	:keymaps 'override
    	:prefix "SPC"
    	:global-prefix "M-SPC")

	(leader-keys
    	"SPC" '(counsel-M-x :wk "Counsel M-x")
    	"." '(find-file :wk "Find file")
	    "," '(ivy-switch-buffer :wk "Switch to buffer")
    	"=" '(perspective-map :wk "Perspective")
    	"/" '(comment-line :wk "Comment lines")
    	"\\" '(counsel-grep-or-swiper :wk "Grep")
    	"u" '(universal-argument :wk "Universal argument"))

	(leader-keys
    	"b" '(:ignore t :wk "Buffers")
    	"b b" '(switch-to-buffer :wk "Switch to buffer")
    	"b c" '(clone-indirect-buffer :wk "Create indirect buffer copy in a split")
    	"b C" '(clone-indirect-buffer-other-window :wk "Clone indirect buffer in new window")
    	"b d" '(bookmark-delete :wk "Delete bookmark")
    	"b i" '(ibuffer :wk "Ibuffer")
    	"b k" '(kill-current-buffer :wk "Kill current buffer")
    	"b K" '(kill-some-buffers :wk "Kill multiple buffers")
    	"b l" '(list-bookmarks :wk "List bookmarks")
    	"b m" '(bookmark-set :wk "Set bookmark")
    	"b n" '(next-buffer :wk "Next buffer")
    	"b p" '(previous-buffer :wk "Previous buffer")
    	"b r" '(revert-buffer :wk "Reload buffer")
    	"b R" '(rename-buffer :wk "Rename buffer")
    	"b s" '(basic-save-buffer :wk "Save buffer")
    	"b S" '(save-some-buffers :wk "Save multiple buffers")
    	"b w" '(bookmark-save :wk "Save current bookmarks to bookmark file"))
	
	(leader-keys
		"d" '(:ignore t :wk "Dired")
		"d e" '(counsel-dired :wk "Open dired")
		"d f" '(wdired-finish-edit :wk "Writable dired finish edit")
		"d j" '(counsel-dired-jump :wk "Dired jump to current")
		"d n" '(neotree-dir :wk "Open directory in neotree")
		"d p" '(peep-dired :wk "Peep-dired")
		"d w" '(wdired-change-to-wdired-mode :wk "Writable dired"))
		
	
	(leader-keys
		"f" '(:ignore t :wk "Files")    
		"f p" '((lambda () (interactive)
				(dired "~/.config/emacs/")) 
				:wk "Open private config")
		"f t" '(find-grep-dired :wk "Search for string in files in DIR")
		"f g" '(counsel-grep-or-swiper :wk "Search for string current file")
		"f i" '((lambda () (interactive)
				(find-file "~/.config/emacs/init.el")) 
				:wk "Open emacs init.el")
		"f j" '(counsel-file-jump :wk "Jump to a file below current directory")
		"f l" '(counsel-locate :wk "Locate a file")
		"f r" '(counsel-recentf :wk "Find recent files")
		"f u" '(sudo-edit-find-file :wk "Sudo find file")
		"f U" '(sudo-edit :wk "Sudo edit file"))
	
	(leader-keys
		"h" '(:ignore t :wk "Help")
		"h r" '(:ignore t :wk "Reload")
		"h r r" '((lambda () (interactive)
					(load-file "~/.config/emacs/init.el")
					(ignore (elpaca-process-queues)))
				:wk "Reload emacs config")
		"h t" '(load-theme :wk "Load theme"))
	
	(leader-keys
    	"p" '(projectile-command-map :wk "Projectile"))


	(leader-keys
		"s" '(:ignore t :wk "Search")
		"s d" '(dictionary-search :wk "Search dictionary")
		"s m" '(man :wk "Man pages")
		"s o" '(pdf-occur :wk "Pdf search lines matching STRING")
		"s t" '(tldr :wk "Lookup TLDR docs for a command")
		"s w" '(woman :wk "Similar to man but doesn't require man"))
	
	(leader-keys
		"w" '(:ignore t :wk "Windows/Words")
		"w c" '(evil-window-delete :wk "Close window")
		"w n" '(evil-window-new :wk "New window")
		"w s" '(evil-window-split :wk "Horizontal split window")
		"w v" '(evil-window-vsplit :wk "Vertical split window")
		"w h" '(evil-window-left :wk "Window left")
		"w j" '(evil-window-down :wk "Window down")
		"w k" '(evil-window-up :wk "Window up")
		"w l" '(evil-window-right :wk "Window right")
		"w w" '(evil-window-next :wk "Goto next window")
		"w H" '(buf-move-left :wk "Buffer move left")
		"w J" '(buf-move-down :wk "Buffer move down")
		"w K" '(buf-move-up :wk "Buffer move up")
		"w L" '(buf-move-right :wk "Buffer move right")
		"w d" '(downcase-word :wk "Downcase word")
		"w u" '(upcase-word :wk "Upcase word")
		"w =" '(count-words :wk "Count words/lines for buffer"))

	(leader-keys
		"l" '(:ignore t :wk "Eglot/LSP")
		"l e" '(eglot :wk "Eglot")
		"l r" '(eglot-reconnect :wk "Eglot reconnect")
		"l a" '(eglot-code-actions :wk "Eglot code actions")
		"l x" '(eglot-shutdown :wk "Eglot shutdown lang server")
		"l h" '(eglot-inlay-hints-mode :wk "Eglot inlay hints mode")
		"l d" '(eldoc-doc-buffer :wk "Eglot show docs")
		"l s" '(:ignore t :wk "Show")
		"l s s" '(imenu :wk "Imenu")
		"l s b" '(flymake-show-buffer-diagnostics :wk "Buffer diagnostics")
		"l s p" '(flymake-show-project-diagnostics :wk "Project diagnostics")
		"l f" '(eglot-format :wk "Eglot format"))

	(leader-keys
		"g" '(:ignore t :wk "Git")    
		"g /" '(magit-displatch :wk "Magit dispatch")
		"g ." '(magit-file-displatch :wk "Magit file dispatch")
		"g b" '(magit-branch-checkout :wk "Switch branch")
		"g c" '(:ignore t :wk "Create") 
		"g c b" '(magit-branch-and-checkout :wk "Create branch and checkout")
		"g c c" '(magit-commit-create :wk "Create commit")
		"g c f" '(magit-commit-fixup :wk "Create fixup commit")
		"g C" '(magit-clone :wk "Clone repo")
		"g f" '(:ignore t :wk "Find") 
		"g f c" '(magit-show-commit :wk "Show commit")
		"g f f" '(magit-find-file :wk "Magit find file")
		"g f g" '(magit-find-git-config-file :wk "Find gitconfig file")
		"g F" '(magit-fetch :wk "Git fetch")
		"g g" '(magit-status :wk "Magit status")
		"g i" '(magit-init :wk "Initialize git repo")
		"g l" '(magit-log-buffer-file :wk "Magit buffer log")
		"g r" '(vc-revert :wk "Git revert file")
		"g s" '(magit-stage-file :wk "Git stage file")
		"g t" '(git-timemachine :wk "Git time machine")
		"g u" '(magit-stage-file :wk "Git unstage file"))
)

;;----------------------------------------------------
;; project manager
;;----------------------------------------------------
(use-package projectile
	:defer t
	:general
	:init
	(projectile-mode +1))

(defun projectile-proj-find-function (dir)
	(let ((root (projectile-project-root dir)))
		(and root (cons 'transient root))))

;;(use-package magit :defer t)

;;----------------------------------------------------
;; startup dashboard
;;----------------------------------------------------
(use-package dashboard
	:ensure t 
	:init
	(setq initial-buffer-choice 'dashboard-open)
	(setq dashboard-set-heading-icons nil)
	(setq dashboard-set-file-icons t)
	(setq dashboard-banner-logo-title "Hey!")
	(setq dashboard-startup-banner nil) 
	(setq dashboard-center-content nil) 
	(setq dashboard-items '((recents . 5)
			                (projects . 3)))
	:custom 
	(dashboard-modify-heading-icons '((recents . "file-text")
						              (bookmarks . "book")))
	:config
	(dashboard-setup-startup-hook))

;;----------------------------------------------------
;; syntax highlighting  
;;----------------------------------------------------
(use-package tree-sitter
  	:ensure t)
(use-package tree-sitter-langs
  	:defer t)

(tree-sitter-require 'rust)
(tree-sitter-require 'c)
(tree-sitter-require 'cpp)
(tree-sitter-require 'go)
(tree-sitter-require 'java)
(tree-sitter-require 'zig)

(global-tree-sitter-mode)
(add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)

;;----------------------------------------------------
;; Eglot (not lsp-mode)
;;----------------------------------------------------
(use-package eglot
  :hook ((go-mode . eglot-ensure)
         (c++-mode . eglot-ensure)
         (c-mode . eglot-ensure)
         (rust-mode . eglot-ensure))
  :custom
  (eglot-autoshutdown t))

;; docs viewer
(use-package eldoc
  :commands (eldoc-mode)
  :custom
  (setq eldoc-documentation-strategy 'eldoc-documentation-default)
  :diminish eldoc-mode)
(set-face-attribute 'eldoc-highlight-function-argument nil)

;; holy c
(use-package c-mode
  :ensure nil
  :mode (("\\.c\\'" . c-mode)
	     ("\\.h\\'" . c-mode))
  :defer t
  :custom
  (add-to-list 'eglot-server-programs '((c-mode) . ("clangd")))
  :hook (c-mode . eglot-ensure))

;; chaotic c++
(use-package c++-mode
  :ensure nil
  :mode (("\\.cc\\'" . c++-mode)
	     ("\\.cpp\\'" . c++-mode)
	     ;; ("\\.h\\'" . c++-mode)
	     ("\\.hh\\'" . c++-mode)
	     ("\\.hpp\\'" . c++-mode))
  :defer t
  :custom
  (add-to-list 'eglot-server-programs '((c++-mode) . ("clangd")))
  :hook (c++-mode . eglot-ensure))

;; simple go
(use-package go-mode
  :defer t
  :config
  :mode ("\\.go\\'" . go-mode)
  :custom
  (add-to-list 'eglot-server-programs '((go-mode) . ("gopls")))
  :hook
  (go-mode . eglot-ensure)
  (before-save-hook . gofmt-before-save))

;; borrow-checker fight
(use-package rustic
  :defer t
  :mode ("\\.rs$" . rust-mode)
  :custom
  (rustic-format-on-save t)
  (rustic-indent-method-chain t)
  (rustic-lsp-server 'rust-analyzer))

;; (add-hook 'c-mode-hook 'eglot-ensure)
;; (add-hook 'c++-mode-hook 'eglot-ensure)
;; (add-hook 'go-mode-hook 'eglot-ensure)
;; (add-hook 'rust-mode-hook 'eglot-ensure)

;; elsip
(add-hook 'emacs-lisp-mode-hook
              (lambda ()
                ;; Use spaces, not tabs.
                (setq indent-tabs-mode nil)
                ;; Keep M-TAB for `completion-at-point'
                (define-key flyspell-mode-map "\M-\t" nil)
                ;; Pretty-print eval'd expressions.
                (define-key emacs-lisp-mode-map
                            "\C-x\C-e" 'pp-eval-last-sexp)
                ;; Recompile if .elc exists.
                (add-hook (make-local-variable 'after-save-hook)
                          (lambda ()
                            (byte-force-recompile default-directory)))
                (define-key emacs-lisp-mode-map
                            "\r" 'reindent-then-newline-and-indent)))
(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)

;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(mood-line bash-mode rustic go-mode flycheck which-key tree-sitter-langs projectile ivy-rich general evil-collection doom-themes dashboard counsel auto-package-update)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
