;;; eglot-config.el -*- lexical-binding: t; -*-

(setq read-process-output-max (* 1024 1024))

(use-package eglot
  :ensure t
  :defer t
  :commands (eglot eglot-ensure)
  :config
  (setq eglot-events-buffer-size 0
        eglot-autoshutdown t
        eglot-sync-connect nil
        eglot-connect-timeout 30
        eglot-send-changes-idle-time 0.5)

  (setq eglot-ignored-server-capabilities
        '(:documentHighlightProvider :inlayHintProvider))
  (setq eldoc-echo-area-use-multiline-p t)
  :hook
  (eglot-managed-mode . (lambda ()
                          (setq-local eldoc-documentation-function
                                      #'eldoc-documentation-compose))))
(add-hook 'eglot-managed-mode-hook
          (lambda ()
            (add-hook 'before-save-hook #'eglot-format nil t)))

(use-package tree-sitter
  :ensure t
  :config
  (global-tree-sitter-mode)

  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(use-package tree-sitter-langs
  :ensure t
  :after tree-sitter
  :demand t
  :config
  (add-to-list 'tree-sitter-major-mode-language-alist '(c-mode . c))
  (add-to-list 'tree-sitter-major-mode-language-alist '(c++-mode . cpp))
  (add-to-list 'tree-sitter-major-mode-language-alist '(rust-mode . rust))
  (add-to-list 'tree-sitter-major-mode-language-alist '(go-mode . go))
  (add-to-list 'tree-sitter-major-mode-language-alist '(zig-mode . zig))

  (add-to-list 'tree-sitter-major-mode-language-alist '(python-mode . python))
  (add-to-list 'tree-sitter-major-mode-language-alist '(ruby-mode . ruby))
  (add-to-list 'tree-sitter-major-mode-language-alist '(perl-mode . perl))
  (add-to-list 'tree-sitter-major-mode-language-alist '(lua-mode . lua))
  (add-to-list 'tree-sitter-major-mode-language-alist '(php-mode . php))

  (add-to-list 'tree-sitter-major-mode-language-alist '(sh-mode . bash))
  (add-to-list 'tree-sitter-major-mode-language-alist '(bash-mode . bash))
  (add-to-list 'tree-sitter-major-mode-language-alist '(fish-mode . fish))

  (add-to-list 'tree-sitter-major-mode-language-alist '(typescript-mode . typescript))
  (add-to-list 'tree-sitter-major-mode-language-alist '(js-mode . javascript))
  (add-to-list 'tree-sitter-major-mode-language-alist '(javascript-mode . javascript))
  (add-to-list 'tree-sitter-major-mode-language-alist '(json-mode . json))
  (add-to-list 'tree-sitter-major-mode-language-alist '(css-mode . css))
  (add-to-list 'tree-sitter-major-mode-language-alist '(scss-mode . scss))
  (add-to-list 'tree-sitter-major-mode-language-alist '(html-mode . html))
  (add-to-list 'tree-sitter-major-mode-language-alist '(web-mode . html))
  (add-to-list 'tree-sitter-major-mode-language-alist '(svelte-mode . svelte))

  (add-to-list 'tree-sitter-major-mode-language-alist '(java-mode . java))
  (add-to-list 'tree-sitter-major-mode-language-alist '(kotlin-mode . kotlin))
  (add-to-list 'tree-sitter-major-mode-language-alist '(scala-mode . scala))
  (add-to-list 'tree-sitter-major-mode-language-alist '(clojure-mode . clojure))
  (add-to-list 'tree-sitter-major-mode-language-alist '(groovy-mode . groovy))

  (add-to-list 'tree-sitter-major-mode-language-alist '(haskell-mode . haskell))
  (add-to-list 'tree-sitter-major-mode-language-alist '(ocaml-mode . ocaml))
  (add-to-list 'tree-sitter-major-mode-language-alist '(tuareg-mode . ocaml))
  (add-to-list 'tree-sitter-major-mode-language-alist '(elm-mode . elm))
  (add-to-list 'tree-sitter-major-mode-language-alist '(elixir-mode . elixir))
  (add-to-list 'tree-sitter-major-mode-language-alist '(erlang-mode . erlang))
  (add-to-list 'tree-sitter-major-mode-language-alist '(racket-mode . racket))
  (add-to-list 'tree-sitter-major-mode-language-alist '(scheme-mode . scheme))

  (add-to-list 'tree-sitter-major-mode-language-alist '(julia-mode . julia))
  (add-to-list 'tree-sitter-major-mode-language-alist '(ess-r-mode . r))
  (add-to-list 'tree-sitter-major-mode-language-alist '(r-mode . r))
  (add-to-list 'tree-sitter-major-mode-language-alist '(swift-mode . swift))
  (add-to-list 'tree-sitter-major-mode-language-alist '(dart-mode . dart))

  (add-to-list 'tree-sitter-major-mode-language-alist '(yaml-mode . yaml))
  (add-to-list 'tree-sitter-major-mode-language-alist '(toml-mode . toml))
  (add-to-list 'tree-sitter-major-mode-language-alist '(cmake-mode . cmake))
  (add-to-list 'tree-sitter-major-mode-language-alist '(dockerfile-mode . dockerfile))
  (add-to-list 'tree-sitter-major-mode-language-alist '(makefile-mode . make)))

(add-hook 'c-mode-hook #'eglot-ensure)
(add-hook 'c++-mode-hook #'eglot-ensure)

(use-package disaster
  :ensure t
  :defer t
  :commands disaster
  :init
  (setq disaster-assembly-mode 'nasm-mode))

(use-package go-mode
  :ensure t
  :defer t
  :mode "\\.go\\'"
  :hook (go-mode . eglot-ensure))

(use-package python-mode
  :ensure t
  :defer t
  :mode "\\.py\\'"
  :hook (python-mode . eglot-ensure)
  :config
  (setq python-shell-interpreter "python3"))

(use-package typescript-mode
  :ensure t
  :defer t
  :mode ("\\.ts\\'" "\\.tsx\\'")
  :hook (typescript-mode . eglot-ensure)
  :config
  (setq typescript-indent-level 4))

(add-hook 'js-mode-hook #'eglot-ensure)
(setq js-indent-level 4)

(use-package web-mode
  :ensure t
  :defer t
  :mode ("\\.html\\'" "\\.htm\\'" "\\.vue\\'" "\\.jsx\\'" "\\.tsx\\'"
         "\\.erb\\'" "\\.hbs\\'" "\\.mustache\\'" "\\.djhtml\\'"
         "\\.php\\'" "\\.twig\\'" "\\.jinja2?\\'")
  :hook (web-mode . eglot-ensure)
  :config
  (setq web-mode-markup-indent-offset 4
        web-mode-css-indent-offset 4
        web-mode-code-indent-offset 4
        web-mode-enable-auto-pairing t
        web-mode-enable-auto-closing t
        web-mode-enable-current-element-highlight t
        web-mode-enable-current-column-highlight t))

(use-package svelte-mode
  :ensure t
  :defer t
  :mode "\\.svelte\\'"
  :hook (svelte-mode . eglot-ensure))

(use-package json-mode
  :ensure t
  :defer t
  :mode ("\\.json\\'" "\\.jsonc\\'")
  :hook (json-mode . eglot-ensure)
  :config
  (setq json-reformat:indent-width 4))

(add-hook 'css-mode-hook #'eglot-ensure)
(setq css-indent-offset 4)

(use-package scss-mode
  :ensure t
  :defer t
  :mode "\\.scss\\'"
  :hook (scss-mode . eglot-ensure))

(setq TeX-source-correlate-mode t
      TeX-source-correlate-method 'synctex)

(provide 'eglot-config)
;;;;; eglot-config.el ends here

(use-package flymake
  :ensure nil
  :hook (prog-mode . flymake-mode)
  :config
  (setq flymake-no-changes-timeout nil)
  (setq flymake-fringe-indicator-position 'right-fringe))
