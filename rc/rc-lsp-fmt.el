;;; rc-lsp-fmt.el --- LSP & Formatters -*- lexical-binding: t; -*-

;;; Eglot
(use-package eglot
  :ensure nil
  :hook
  (python-ts-mode . eglot-ensure)
  (go-ts-mode . eglot-ensure)
  (tsx-ts-mode . eglot-ensure)
  (typescript-ts-mode . eglot-ensure)
  :config
  (setq eglot-sync-connect 0
        eglot-autoshutdown t
        eglot-extend-to-xref t
        jsonrpc-event-hook nil
        eglot-events-buffer-config '(:size 0 :format lisp)
	eglot-server-programs
	'( (python-ts-mode . ("pyright-langserver" "--stdio"))
	   (go-ts-mode . ("gopls"))
	   (typescript-ts-mode . ("typescript-language-server" "--stdio"))
	   (tsx-ts-mode . ("typescript-language-server" "--stdio"))))
  (setq-default eglot-workspace-configuration
                '( :pyright ( :disableOrganizeImports t)
		   :python.analysis ( :autoSearchPaths t
				      :useLibraryCodeForTypes t
				      :diagnosticMode "openFilesOnly")
		   :gopls ( :gofumpt t
			    :staticcheck t
			    :completeUnimported t))))

;;; Apheleia
(use-package apheleia
  :ensure t
  :delight
  :hook (prog-mode . apheleia-mode)
  :config
  (setf (alist-get 'python-ts-mode apheleia-mode-alist) '(ruff-isort ruff))
  (setf (alist-get 'go-ts-mode apheleia-mode-alist) '(gofumpt)))

(provide 'rc-lsp-fmt)
