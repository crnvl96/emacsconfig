;;; -*- lexical-binding: t; -*-

(use-package eglot
  :ensure nil
  :hook
  (python-ts-mode . eglot-ensure)
  (go-ts-mode . eglot-ensure)
  (tsx-ts-mode . eglot-ensure)
  (typescript-ts-mode . eglot-ensure)
  :config
  (setq eglot-sync-connect 0)
  (setq eglot-autoshutdown t)
  (setq eglot-extend-to-xref t)
  (setq jsonrpc-event-hook nil)
  (setq eglot-events-buffer-config '(:size 0 :format lisp))
  (setq eglot-server-programs
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
                            :completeUnimported t)))
  (defun my/eglot-capf ()
    (setq-local completion-at-point-functions
                (list (cape-capf-super
                       #'eglot-completion-at-point))))
  (add-hook 'eglot-managed-mode-hook #'my/eglot-capf)
  (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster))

(provide 'rc-eglot)
