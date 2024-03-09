;;; :init.el starts here  -*- lexical-binding: t; -*-

;;----------------------------------------------------
;; general configs
;;----------------------------------------------------
(electric-pair-mode 1)
(global-display-line-numbers-mode 1)
(global-visual-line-mode -1)
(add-to-list 'default-frame-alist '(alpha-background . 95))
(set-face-italic 'italic nil)
(pixel-scroll-mode 1)
(pixel-scroll-precision-mode t)
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

(defun display-packages-load-time ()
  (with-current-buffer "*scratch*"
    (insert (format "\n;; %d packages loaded in %.6f seconds\n"
                    (length package-activated-list)
                    (float-time (time-subtract after-init-time before-init-time))))))

(add-hook 'emacs-startup-hook 'display-packages-load-time)

;; font
(set-face-attribute 'default nil :family "JetBrainsMono NF" :height 130 :weight 'regular)


(set-face-attribute 'mode-line nil
                    :background "#212121"
		            :family "JetBrainsMono NF"
		            :height 130
                    :box '(:line-width 4 :color "#212121")
                    :overline nil
                    :underline nil)

;; eglot-ignored-server-capabilities '(:inlayHintProvider)
;; eglot-ignored-server-capabilities '(:documentHighlightProvider)
;; eglot-ignored-server-capabilities '(:hoverProvider)

(setq ring-bell-function 'ignore
	  compile-command "make all"
	  flymake-no-changes-timeout 10
      flymake-start-syntax-check-on-newline nil
      org-edit-src-content-indentation 0
      use-file-dialog nil  
      use-dialog-box nil  
      pop-up-windows nil 
      eldoc-echo-area-use-multiline-p nil
      eldoc-documentation-strategy 'eldoc-documentation-compose
      eldoc-echo-area-prefer-doc-buffer t
      display-line-numbers-type 'relative
      confirm-kill-emacs nil
      backup-inhibited t
      Man-notify-method 'pushy
      create-lockfiles nil
	  frame-resize-pixelwise t
      locale-coding-system 'utf-8
      c-tab-always-indent t)

(setq-default indent-tabs-mode t
	          line-spacing 0.17
	          abbrev-mode -1
	          indicate-empty-lines t
	          tab-width 4
              c-basic-offset 4
	          standard-indent 4)

;; search google
(defun search-on-browser (term)
  "Search TERM on preferred engine on browser.
   If no search TERM is entered interactively, the current
   buffer selection is used as the TERM."
  (interactive "sSearch term (default to selection): ")

  (when (eq term "")
    (setq term (buffer-substring (region-beginning) (region-end))))

  (setq term (replace-regexp-in-string " +" "+" term))

  (unless (boundp 'search-engine-query-url)
    (setq search-engine-query-url "https://google.com/search?q="))

  (unless (boundp 'browser-command)
    (setq browser-command "firefox"))

  (let ((full_query_url (concat search-engine-query-url "'" term "'")))
    (shell-command (concat browser-command " '" full_query_url "'") nil nil)))


;;---------------------------------------------------
;; global keybindings
;;---------------------------------------------------
(global-set-key (kbd "C-g") 'search-on-browser)
(global-set-key (kbd "TAB") 'self-insert-command)
(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key [escape] 'keyboard-escape-quit)
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(global-set-key (kbd "M-/") 'completion-at-point)
(global-set-key (kbd "<C-right>") 'next-buffer)
(global-set-key (kbd "<C-left>") 'previous-buffer)

;;----------------------------------------------------
;; package config
;;----------------------------------------------------
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)

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

;;---------------------------------------------------
;; hide minor modes
;;---------------------------------------------------
(use-package diminish
  :ensure t
  :demand
  :diminish abbrev-mode
  :diminish eldoc-mode
  :diminish which-key-mode
  :diminish ivy-mode
  :diminish lsp-mode
  :diminish projectile-mode
  :diminish evil-collection-unimpaired-mode
  :diminish visual-line-mode)

;;----------------------------------------------------
;; ui tweaks
;;----------------------------------------------------
(use-package doom-themes
  :ensure t
  :defer t
  :config
  (setq doom-themes-enable-bold t   
	    doom-themes-enable-italic nil)) 

(add-to-list 'custom-theme-load-path "~/.config/emacs/themes/")
(setq custom-safe-themes t)
(load-theme 'doom-zenburnv2)
;; (load-theme 'doom-gruvbox-material)

(use-package base16-theme
  :ensure t
  :defer t)

(use-package catppuccin-theme
  :ensure t
  :defer t)

;;-----------------------------------------------------
;; mode-line
;;-----------------------------------------------------
;; (use-package mood-line
;;   :ensure t
;;   :init 
;;   :config
;;   (mood-line-mode)
;;   :custom
;;   (mood-line-glyph-alist mood-line-glyphs-fira-code))

;; (setq mood-line-format
;;      (mood-line-defformat
;;    :left
;;    (((mood-line-segment-modal)            . " ")
;;     ((or (mood-line-segment-buffer-status)
;;          (mood-line-segment-client)
;;          " ")                             . " ")
;;     ((mood-line-segment-project)          . "/")
;;     ((mood-line-segment-buffer-name)      . "  ")
;;     ((mood-line-segment-anzu)             . "  ")
;;     ((mood-line-segment-multiple-cursors) . "  ")
;;     (mood-line-segment-cursor-position)
;;     #(":" 0 1 (face mood-line-unimportant))
;;     ((mood-line-segment-cursor-point)     . " ")
;;     ((mood-line-segment-region)           . " ")
;;     (mood-line-segment-scroll))
;;    :right
;;    (((mood-line-segment-indentation) . "  ")
;;     ((mood-line-segment-eol)         . "  ")
;;     ((mood-line-segment-encoding)    . "  ")
;;     ((mood-line-segment-vc)          . "  ")
;;     ((mood-line-segment-major-mode)  . "  ")
;;     ((mood-line-segment-misc-info)   . "  ")
;;     ((mood-line-segment-checker)     . "  ")
;;     ((mood-line-segment-process)     . "  "))))


;;----------------------------------------------------
;; drag stuff - used to move lines and words around
;;----------------------------------------------------
(use-package drag-stuff
  :ensure t
  :init
  (drag-stuff-global-mode 1)
  (drag-stuff-define-keys))


;;----------------------------------------------------
;; pdf tools
;;----------------------------------------------------
;; (use-package pdf-tools
;;   :ensure t
;;   :defer t
;;   :commands (pdf-loader-install)
;;   :mode "\\.pdf\\'"
;;   :bind (:map pdf-view-mode-map
;;               ("j" . pdf-view-next-line-or-next-page)
;;               ("k" . pdf-view-previous-line-or-previous-page)
;;               ("C-=" . pdf-view-enlarge)
;;               ("C--" . pdf-view-shrink))
;;   :init (pdf-loader-install)
;;   :config (add-to-list 'revert-without-query ".pdf"))

;; (add-hook 'pdf-view-mode-hook #'(lambda () (interactive) (display-line-numbers-mode -1)
;;                                                          (blink-cursor-mode -1)))

;; ;;----------------------------------------------------
;; terminal
;;----------------------------------------------------
;; (use-package vterm
;;   :ensure t
;;   :defer t)

;;----------------------------------------------------
;; key table -> on SPC
;;----------------------------------------------------
(use-package which-key
  :ensure t
  :demand t
  :init
  (which-key-mode 1)
  :diminish which-key-mode
  :config
  (setq which-key-side-window-location 'bottom
		which-key-sort-order 'which-key-local-then-key-order
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
		which-key-separator " → "
        which-key-prefix-prefix "+"))

;;----------------------------------------------------
;; vim emulator
;;----------------------------------------------------
(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  :demand
  :config
  (evil-mode 1))

(use-package evil-collection
  :ensure t
  :after evil
  :diminish evil-collection-unimpaired-mode
  :config
  (evil-collection-init))

(with-eval-after-load 'dired
  (evil-define-key 'normal dired-mode-map (kbd "h") 'dired-up-directory)
  (evil-define-key 'normal dired-mode-map (kbd "l") 'dired-open-file))

;;----------------------------------------------------
;; some completion packages
;;----------------------------------------------------
(use-package counsel
  :ensure t
  :after ivy
  :diminish counsel-mode
  :config 
  (counsel-mode)
  (setq ivy-initial-inputs-alist nil))

(use-package ivy
  :ensure t
  :demand
  :diminish ivy-mode
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

;; (use-package company
;;   :ensure t
;;   :defer t
;;   :diminish company-mode
;;   :custom
;;   (company-begin-commands '(self-insert-command))
;;   (company-idle-delay .1)
;;   (company-minimum-prefix-length 2)
;;   (company-show-numbers t)
;;   (company-tooltip-align-annotations 't)
;;   (global-company-mode t))

;; (use-package company-box
;;  :ensure t
;; 	:after company
;; 	:diminish
;; 	:hook (company-mode . company-box-mode))


;;----------------------------------------------------
;; key mappings
;;----------------------------------------------------
(use-package general
  :ensure t
  :demand t
  :config
  ;; set up 'SPC' as the global leader key
  (general-create-definer leader-keys
    :states '(normal insert visual emacs)
    :keymaps 'override
    :prefix "SPC"
    :global-prefix "M-SPC")

  (leader-keys
    "SPC" '(counsel-M-x :wk "Counsel M-x")
    "." '(find-file :wk "Find file")
	"e" '(dired-jump :wk "Dired jump to current")
    "c" '(project-compile :wk "Compile")
    "\\" '(swiper-all :wk "Global grep")
    "t" '(term :wk "Open terminal")
    "z" '(counsel-fzf :wk "Fuzzy finder")
	"x" '(flymake-show-project-diagnostics :wk "Diagnostics")
	;; "," '(ivy-switch-buffer :wk "Switch to buffer")
	"," '(counsel-switch-buffer :wk "Switch to buffer")
    "=" '(perspective-map :wk "Perspective")
    "/" '(comment-line :wk "Comment lines")
    "g" '(counsel-grep-or-swiper :wk "Grep")
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
	"d e" '(dired :wk "Open dired")
	"d d" '(counsel-dired :wk "Open dired")
	"d f" '(wdired-finish-edit :wk "Writable dired finish edit")
	"d j" '(counsel-dired-jump :wk "Dired jump to current"))
  
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
				(load-file "~/.config/emacs/init.el"))
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
	"l e" '(lsp :wk "Eglot/LSP")
	;; "l r" '(eglot-reconnect :wk "Eglot reconnect")
	"l a" '(lsp-execute-code-action :wk "Code actions")
	;; "l x" '(eglot-shutdown :wk "Eglot shutdown lang server")
	"l h" '(lsp-inlay-hints-mode :wk "Inlay hints mode")
	"l d" '(lsp-signature-toggle-full-docs :wk "Eglot show docs")
	"l s" '(:ignore t :wk "Show")
	"l s s" '(lsp-ui-imenu--view :wk "Imenu")
	"l s b" '(flymake-show-buffer-diagnostics :wk "Buffer diagnostics")
	"l s p" '(flymake-show-project-diagnostics :wk "Project diagnostics")
	"l f" '(lsp-format-buffer :wk "Format"))

  ;; (leader-keys
  ;;   "g" '(:ignore t :wk "Git")    
  ;;   "g /" '(magit-displatch :wk "Magit dispatch")
  ;;   "g ." '(magit-file-displatch :wk "Magit file dispatch")
  ;;   "g b" '(magit-branch-checkout :wk "Switch branch")
  ;;   "g c" '(:ignore t :wk "Create") 
  ;;   "g c b" '(magit-branch-and-checkout :wk "Create branch and checkout")
  ;;   "g c c" '(magit-commit-create :wk "Create commit")
  ;;   "g c f" '(magit-commit-fixup :wk "Create fixup commit")
  ;;   "g C" '(magit-clone :wk "Clone repo")
  ;;   "g f" '(:ignore t :wk "Find") 
  ;;   "g f c" '(magit-show-commit :wk "Show commit")
  ;;   "g f f" '(magit-find-file :wk "Magit find file")
  ;;   "g f g" '(magit-find-git-config-file :wk "Find gitconfig file")
  ;;   "g F" '(magit-fetch :wk "Git fetch")
  ;;   "g g" '(magit-status :wk "Magit status")
  ;;   "g i" '(magit-init :wk "Initialize git repo")
  ;;   "g l" '(magit-log-buffer-file :wk "Magit buffer log")
  ;;   "g r" '(vc-revert :wk "Git revert file")
  ;;   "g s" '(magit-stage-file :wk "Git stage file")
  ;;   "g t" '(git-timemachine :wk "Git time machine")
  ;;   "g u" '(magit-stage-file :wk "Git unstage file"))
  )

;;----------------------------------------------------
;; project manager
;;----------------------------------------------------
(use-package projectile
  :ensure t
  :defer t
  :diminish projectile-mode
  :general
  :init
  (projectile-mode +1))

(defun projectile-proj-find-function (dir)
  (let ((root (projectile-project-root dir)))
	(and root (cons 'transient root))))

;;---------------------------------------------------
;; git
;;---------------------------------------------------
;; (use-package magit
;;   :ensure t
;;   :init
;;   :defer t)

;; (use-package git-gutter
;;   :ensure t
;;   :defer t)

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
  (setq dashboard-startup-banner 2) 
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
  :ensure t
  :diminish tree-sitter-mode
  :defer t)

(use-package tree-sitter-langs
  :ensure t
  :defer t)

(tree-sitter-require 'rust)
(tree-sitter-require 'c)
(tree-sitter-require 'cpp)
(tree-sitter-require 'go)
(tree-sitter-require 'java)
(tree-sitter-require 'zig)
(tree-sitter-require 'elisp)

(global-tree-sitter-mode)
(add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)

;;----------------------------------------------------
;; Eglot 
;;----------------------------------------------------
;; (use-package eglot
;;   :ensure t
;;   ;; :hook ((go-mode . eglot-ensure)
;;   ;;        (c++-mode . eglot-ensure)
;;   ;;        (c-mode . eglot-ensure)
;;   ;;        (rust-mode . eglot-ensure))
;;   :custom
;;   (eglot-autoshutdown t))

;; ;; docs viewer
;; (use-package eldoc
;;   :ensure t
;;   :commands (eldoc-mode)
;;   :custom
;;   (setq eldoc-documentation-strategy 'eldoc-documentation-default)
;;   :diminish eldoc-mode)

;; (set-face-attribute 'eldoc-highlight-function-argument nil)

;;---------------------------------------------------------
;; lsp-mode
;;---------------------------------------------------------
(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :diminish lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")  ;; Or 'C-l', 's-l'
  :config
  (lsp-enable-which-key-integration t))

(setq lsp-diagnostics-provider :flymake)
(setq lsp-headerline-breadcrumb-enable nil)
(setq lsp-lens-enable nil)
(setq lsp-enable-xref t)
(setq lsp-enable-text-document-color t)
(setq lsp-enable-symbol-highlighting t)
(setq lsp-enable-text-document-color t)
(setq lsp-inlay-hint-enable nil)
(setq lsp-signature-render-documentation t)
(setq lsp-modeline-code-actions-enable t)
(setq lsp-eldoc-enable-hover t)
(setq lsp-modeline-diagnostics-enable t)
(setq lsp-completion-show-detail t)
(setq lsp-completion-show-kind t)
(setq lsp-completion-show-label-description t)
(setq lsp-idle-delay 10)
(setq lsp-auto-execute-action nil)
;; (setq flymake-start-on-save-buffer t)
;; (setq lsp-diagnostics-provider :none)

;; c/c++
(use-package cc-mode
  :ensure t
  :defer t
  :mode (("\\.c\\'" . c-mode)
         ("\\.h\\'" . c-mode)
         ("\\.cc\\'" . c++-mode)
         ("\\.cpp\\'" . c++-mode)
         ("\\.hh\\'" . c++-mode)
         ("\\.hpp\\'" . c++-mode))
  :hook (c-mode . lsp-deferred)
  :hook (c++-mode . lsp-deferred))

;; holy c
;; (use-package c-mode
;;   :defer t
;;   :mode (("\\.c\\'" . c-mode)
;; 	     ("\\.h\\'" . c-mode))
;;   :custom
;;   (add-to-list 'eglot-server-programs '((c-mode) . ("clangd")))
;;   :hook (c-mode . eglot-ensure))

;; chaotic c++
;; (use-package c++-mode
;;   :defer t
;;   :mode (("\\.cc\\'" . c++-mode)
;; 	     ("\\.cpp\\'" . c++-mode)
;; 	     ;; ("\\.h\\'" . c++-mode)
;; 	     ("\\.hh\\'" . c++-mode)
;; 	     ("\\.hpp\\'" . c++-mode))
;;   :custom
;;   (add-to-list 'eglot-server-programs '((c++-mode) . ("clangd")))
;;   :hook (c++-mode . eglot-ensure))

;; simple go
(use-package go-mode
  :ensure t
  :defer t
  :mode ("\\.go\\'" . go-mode)
  :hook
  (go-mode . lsp-deferred))

;; markdown
(use-package markdown-mode
  :defer t
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown"))

;;; init.el ends here
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(which-key tree-sitter-langs projectile pdf-tools lsp-mode ivy-rich go-mode general evil-collection drag-stuff doom-themes diminish dashboard counsel base16-theme)))
