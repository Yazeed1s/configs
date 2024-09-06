;;; :init.el starts here  -*- lexical-binding: t; -*-

;; general configs
(electric-pair-mode 1)
(global-display-line-numbers-mode 1)
(global-visual-line-mode -1)
(add-to-list 'default-frame-alist '(alpha-background . 97))
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


;; emacs 28+ native compilation
(setq package-native-compile t)
(setq native-comp-async-report-warnings-errors nil)

;; enable default windmove keybindings (shift + arrows)
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

;; vim-like tilde '~' for empty lines
(define-fringe-bitmap 'tilde [0 0 0 113 219 142 0 0] nil nil 'center)
(setcdr (assq 'empty-line fringe-indicator-alist) 'tilde)
(set-fringe-bitmap-face 'tilde 'font-lock-comment-face)

;; display package load time on startup in **scratch** buffer
(defun display-packages-load-time ()
  (with-current-buffer "*scratch*"
    (insert (format "\n;; %d packages loaded in %.6f seconds\n"
                    (length package-activated-list)
                    (float-time (time-subtract after-init-time before-init-time))))))
(add-hook 'emacs-startup-hook 'display-packages-load-time)

;; font
(set-face-attribute 'default nil :family "JetBrainsMono NF" :height 110 :weight 'regular)
(set-face-attribute 'mode-line nil
                    :background "#212121"
		            :family "JetBrainsMono NF"
		            :height 110
                    :box '(:line-width 4 :color "#212121")
                    :overline nil
                    :underline nil)

;; some config
(setq ring-bell-function 'ignore
	  compile-command "make all"
	  flymake-no-changes-timeout 10
      flymake-start-syntax-check-on-newline nil
      org-edit-src-content-indentation 0
      use-file-dialog nil  
      use-dialog-box nil  
      pop-up-windows nil 
      display-line-numbers-type 'relative
      confirm-kill-emacs nil
      backup-inhibited t
      Man-notify-method 'pushy
      create-lockfiles nil
	  frame-resize-pixelwise t
      locale-coding-system 'utf-8
      c-tab-always-indent t)


(setq-default indent-tabs-mode t
	          ;; line-spacing 2
              ;; line-height 2
	          abbrev-mode -1
	          indicate-empty-lines t
	          tab-width 4
              c-basic-offset 4
	          standard-indent 4)

;; completion
(ido-mode 1)
(icomplete-mode 1)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(setq ido-use-filename-at-point 'guess)

;; function to choose from recent files using IDO
(defun ido-choose-from-recentf ()
  "Use ido to select a recently visited file from the `recentf-list'"
  (interactive)
  (find-file (ido-completing-read "Open file: " recentf-list nil t)))

;; spell-checking
(require 'flyspell)
(flyspell-mode +1)

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

;; global keybindings
(global-set-key (kbd "C-g") 'search-on-browser)
(global-set-key (kbd "TAB") 'self-insert-command)
(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key [escape] 'keyboard-escape-quit)
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(global-set-key (kbd "M-/") 'completion-at-point)
(global-set-key (kbd "<C-right>") 'next-buffer)
(global-set-key (kbd "<C-left>") 'previous-buffer)
(global-set-key (kbd "C-.") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-\"") 'mc/mark-all-like-this)

;; package config
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

;; hide minor modes
(use-package diminish
  :ensure t
  :demand
  :diminish abbrev-mode
  :diminish eldoc-mode
  :diminish which-key-mode
  :diminish drag-stuff-mode
  :diminish evil-collection-unimpaired-mode
  :diminish visual-line-mode)

;; ui tweaks
(use-package doom-themes
  :ensure t
  :defer t
  :config
  (setq doom-themes-enable-bold t   
	    doom-themes-enable-italic nil)) 

(add-to-list 'custom-theme-load-path "~/.config/emacs/themes/")
(setq custom-safe-themes t)
(load-theme 'doom-zenburnv2)
;; (setq doom-gruvbox-material-background  "medium")
;; (load-theme 'doom-gruvbox-material)

(use-package base16-theme
  :ensure t
  :defer t)

(use-package catppuccin-theme
  :ensure t
  :defer t)

;; drag stuff - used to move lines and words around
(use-package drag-stuff
  :ensure t
  :init
  (drag-stuff-global-mode 1)
  (drag-stuff-define-keys))

;; key table -> on SPC
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


;; multiple cursors
(use-package multiple-cursors
  :ensure t)

;; vim emulator
(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump t)
  (setq evil-want-Y-yank-to-eol t)
  (setq evil-symbol-word-search t)
  :demand
  :config
  (evil-mode 1))

(use-package evil-surround
  :ensure t
  :after evil
  :config
  (global-evil-surround-mode 1))

(use-package evil-collection
  :ensure t
  :after evil
  :diminish evil-collection-unimpaired-mode
  :config
  (evil-collection-init))

(with-eval-after-load 'dired
  (evil-define-key 'normal dired-mode-map (kbd "h") 'dired-up-directory)
  (evil-define-key 'normal dired-mode-map (kbd "l") 'dired-open-file))

;; git stuff
(use-package magit
  :ensure t
  :after eglot)

(use-package git-gutter
  :ensure t
  :diminish
  :init
  (setq git-gutter:modified-sign "~"
        git-gutter:added-sign "+"
        git-gutter:deleted-sign "-"
		git-gutter:update-interval 0.02)
  :config
  (global-git-gutter-mode 't))

(set-face-foreground 'git-gutter:modified "#b5c2b5")
(set-face-foreground 'git-gutter:added "#a7bc99")
(set-face-foreground 'git-gutter:deleted "#CC9393")

;; fzf
(use-package fzf
  :ensure t
  :defer t
  :bind
    ;; keys go here
  :config
  (setq fzf/args "-x --color bw --print-query --margin=1,0 --no-hscroll"
        fzf/executable "fzf"
        fzf/git-grep-args "-i --line-number %s"
        ;; command used for `fzf-grep-*` functions
        ;; example usage for ripgrep:
        ;; fzf/grep-command "rg --no-heading -nH"
        fzf/grep-command "grep -nrH"
        ;; If nil, the fzf buffer will appear at the top of the window
        fzf/position-bottom t
        fzf/window-height 15))

;; imenu-list
(use-package imenu-list
  :ensure t
  :defer t
  :hook (imenu-mode . imenu-list-mode)
  :config
  (setq imenu-list-position 'right
		imenu-list-auto-resize t
        imenu-list-idle-update-delay 1
        imenu-list-focus-after-activation t))

;; completeion helper
(use-package company
  :ensure t
  :init (global-company-mode t)
  :diminish company-mode
  :defer t
  :config
  (setq company-minimum-prefix-length 1
        company-idle-delay 0.1
        company-selection-wrap-around t
        company-tooltip-align-annotations t
        company-frontends '(company-pseudo-tooltip-frontend ; show tooltip even for single candidate
                            company-echo-metadata-frontend))) ;; key mappings

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
    "SPC" '(execute-extended-command :wk "M-x")
    "." '(ido-find-file :wk "Find file")
	"e" '(dired-jump :wk "Dired jump to current")
    "c" '(eshell-command :wk "Command")
    "i" '(imenu-list-smart-toggle :wk "Definitions")
    "\\" '(rgrep :wk "Global grep")
    "j" '(term :wk "Open terminal")
    "t" '(project-eshell :wk "Project shell")
    "z" '(fzf-find-in-buffer :wk "Fuzzy finder")
	"x" '(flymake-show-project-diagnostics :wk "Diagnostics")
	"," '(ido-switch-buffer :wk "Switch to buffer")
    "/" '(comment-line :wk "Comment lines")
    ";" '(rgrep :wk "Grep")
    "r" '(query-replace :wk "Search and replace")
    "o" '(isearch-occur :wk "Isearch occur"))

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
	"d f" '(wdired-finish-edit :wk "Writable dired finish edit")
	"d j" '(dired-jump :wk "Dired jump to current"))
  
  (leader-keys
	"f" '(:ignore t :wk "Files")    
	"f c" '((lambda () (interactive)
			  (dired "~/.config/emacs/")) 
			:wk "Open private config")
	"f t" '(find-grep-dired :wk "Search for string in files in DIR")
	"f g" '(rgrep :wk "Search for string current file")
	"f i" '((lambda () (interactive)
              (find-file "~/.config/emacs/init.el")) 
			:wk "Open emacs init.el")
	"f j" '(dired-jump :wk "Jump to a file below current directory")
	"f r" '(ido-choose-from-recentf :wk "Find recent files")
	"f p" '(project-switch-project :wk "Find project")
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
	"s" '(:ignore t :wk "Search")
	"s d" '(dictionary-search :wk "Search dictionary")
	"s m" '(man :wk "Man pages")
	"s o" '(pdf-occur :wk "Pdf search lines matching STRING")
	"s t" '(tldr :wk "Lookup TLDR docs for a command")
	"s w" '(woman :wk "Similar to man but doesn't require man"))
  
  (leader-keys
	"w" '(:ignore t :wk "Windows/Words")
	"w c" '(delete-window :wk "Close window")
	"w n" '(new-window-on-right :wk "New window")
	"w s" '(split-window-vertically :wk "Horizontal split window")
	"w v" '(split-window-horizontally :wk "Vertical split window")
	"w w" '(next-window :wk "Goto next window")
	"w d" '(downcase-word :wk "Downcase word")
	"w u" '(upcase-word :wk "Upcase word")
	"w =" '(count-words :wk "Count words/lines for buffer"))
  
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
  
  (leader-keys
	"l" '(:ignore t :wk "Eglot/LSP")
	"l e" '(lsp :wk "Eglot/LSP")
	"l r" '(eglot-reconnect :wk "Eglot reconnect")
	"l a" '(eglot-code-actions :wk "Code actions")
	"l x" '(eglot-shutdown :wk "Eglot shutdown lang server")
	"l h" '(eglot-inlay-hints-mode :wk "Inlay hints mode")
	"l s" '(:ignore t :wk "Show")
	"l s b" '(flymake-show-buffer-diagnostics :wk "Buffer diagnostics")
	"l s p" '(flymake-show-project-diagnostics :wk "Project diagnostics")
	"l f" '(eglot-format-buffer :wk "Format"))
  )

;; startup dashboard
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

;; syntax highlighting  
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

;; Eglot 
(use-package eglot
  :ensure t
  :custom
  (eglot-autoshutdown t))

(setq eglot-ignored-server-capabilities '(:inlayHintProvider))
(setq eglot-ignored-server-capabilities '(:hoverProvider))
(setq eglot-ignored-server-capabilities '(:documentHighlightProvider))
(add-hook 'eglot-managed-mode-hook (lambda () (eglot-inlay-hints-mode -1)))

;; docs viewer
(use-package eldoc
  :ensure t
  :commands (eldoc-mode)
  :diminish eldoc-mode
  :preface
  (defun mp-eglot-eldoc ()
    (setq eldoc-documentation-strategy
            'eldoc-documentation-compose-eagerly))
  :hook ((eglot-managed-mode . mp-eglot-eldoc)))

(setq eldoc-echo-area-use-multiline-p nil)
;; (setq eldoc-echo-area-display-truncation-message nil)
;; (setq eldoc-echo-area-prefer-doc-buffer t)

;; assemply view for c/c++
(use-package disaster
  :ensure t
  :commands (disaster)
  :init
  ;; If you prefer viewing assembly code in `nasm-mode` instead of `asm-mode`
  (setq disaster-assembly-mode 'nasm-mode))

;; holy c
(use-package c-mode
  :defer t
  :mode (("\\.c\\'" . c-mode)
	     ("\\.h\\'" . c-mode))
  :custom
  (add-to-list 'eglot-server-programs '((c-mode) . ("clangd")))
  :hook (c-mode . eglot-ensure))

;; chaotic c++
(use-package c++-mode
  :defer t
  :mode (("\\.cc\\'" . c++-mode)
	     ("\\.cpp\\'" . c++-mode)
	     ("\\.hh\\'" . c++-mode)
	     ("\\.hpp\\'" . c++-mode))
  :custom
  (add-to-list 'eglot-server-programs '((c++-mode) . ("clangd")))
  :hook (c++-mode . eglot-ensure))

;; simple go
(use-package go-mode
  :ensure t
  :defer t
  :mode ("\\.go\\'" . go-mode)
  :custom
  (add-to-list 'eglot-server-programs '((go-mode) . ("gopls")))
  :hook (go-mode . eglot-ensure))

;; markdown
(use-package markdown-mode
  :defer t
  :ensure t
  :mode (("README\\.md\\'" . gfm-mode)
        ("\\.md\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown")
  :custom
  (markdown-header-scaling t)
  (markdown-hide-urls t)
  (markdown-fontify-code-blocks-natively t)
  (markdown-fontify-gfm-code-blocks t)
  :config
  (defun no-line-number ()
    (display-line-numbers-mode 0))
  (add-hook 'markdown-mode-hook 'no-line-number)
  (add-hook 'gfm-mode-hook 'no-line-number)
  (add-hook 'gfm-view-mode-hook 'no-line-number)
  (add-hook 'markdown-view-mode-hook 'no-line-number)
  (defun md-width ()
       (set-fill-column 40))
  (add-hook 'markdown-mode-hook 'md-width)
  (add-hook 'gfm-mode-hook 'md-width)
  (add-hook 'markdown-view-mode-hook 'md-width)
  (add-hook 'gfm-view-mode-hook 'md-width))

;; ebooks
(use-package nov
  :ensure t
  :defer t
  :mode ("\\.epub\\'" . nov-mode)
  :custom (nov-text-width 90)
  :config
  (defun nov-font-setup ()
    (face-remap-add-relative 'variable-pitch  :family "JetBrainsMono NF" :height 110 :weight 'regular))
  (defun no-line-number ()
    (display-line-numbers-mode 0))
  (add-hook 'nov-mode-hook 'nov-font-setup)
  (add-hook 'nov-mode-hook 'no-line-number))

;; pdf tools
(use-package pdf-tools
  :ensure t
  :defer t
  :mode (("\\.pdf\\'" . pdf-view-mode))
  :bind ((:map pdf-view-mode-map ("C--" . pdf-view-shrink))
         (:map pdf-view-mode-map ("C-=" . pdf-view-enlarge))
         (:map pdf-view-mode-map ("C-0" . pdf-view-scale-reset))
		 (:map pdf-view-mode-map ("i"   . 'pdf-outline)))
  :config
  (defun no-line-number ()
    (display-line-numbers-mode 0))
  (add-hook 'pdf-view-mode-hook  'no-line-number)
  (add-hook 'pdf-view-mode-hook 'pdf-view-fit-page-to-window)
  (pdf-loader-install)
  (setq pdf-info-epdfinfo-program "/usr/bin/epdfinfo"))
(add-hook 'doc-view-mode (lambda () (display-line-numbers-mode -1)))

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
   '(org-bullets nov magit imenu-list fzf disaster git-gutter-fringe git-gutter evil-surround evil-mc evil-multiedit multiple-cursors company-box company which-key tree-sitter-langs projectile pdf-tools lsp-mode ivy-rich go-mode general evil-collection drag-stuff doom-themes diminish dashboard counsel base16-theme))
 '(warning-suppress-log-types '((pdf-view))))
(put 'dired-find-alternate-file 'disabled nil)
