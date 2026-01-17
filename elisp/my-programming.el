;;; my-programming.el --- Programming -*- lexical-binding: t -*-

;;; Commentary:

;; Programming support: eglot LSP, apheleia formatting, mise, and indent-bars.

;;; Code:

(use-package eglot
  :ensure nil
  :after cape
  :config
  ;; Performance optimizations
  (fset #'jsonrpc--log-event #'ignore)
  (setq eglot-events-buffer-config '(:size 0 :format lisp))
  (setq eglot-send-changes-idle-time 0.1)
  (setq eglot-extend-to-xref t)

  ;; Cape integration for better completion
  (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)

  ;; Disable noisy/unneeded capabilities
  (setq eglot-ignored-server-capabilities
        '(:signatureHelpProvider
          :documentHighlightProvider
          :codeLensProvider
          :documentRangeFormattingProvider
          :documentOnTypeFormattingProvider
          :documentLinkProvider
          :foldingRangeProvider
          :inlayHintProvider))

  ;; Language server configuration
  (setq eglot-server-programs
        '((python-ts-mode . ("rass" "--" "pyright-langserver" "--stdio" "--" "ruff" "server"))
          ;; (python-ts-mode . ("pyright-langserver" "--stdio"))
          (c-ts-mode      . ("clangd"))
          (go-ts-mode     . ("gopls"))))

  ;; Workspace-specific settings
  (setq-default eglot-workspace-configuration
                '( :pyright ( :disableOrganizeImports t)
                   :python.analysis ( :autoSearchPaths t
                                      :useLibraryCodeForTypes t
                                      :diagnosticMode "openFilesOnly")
                   :gopls ( :gofumpt t))))

(use-package apheleia
  :ensure t
  :hook ((c-ts-mode go-ts-mode python-ts-mode emacs-lisp-mode) . apheleia-mode)
  :preface
  ;; Load clang-format if available
  (let ((clang-format-file "/home/linuxbrew/.linuxbrew/Cellar/llvm/21.1.8/share/emacs/site-lisp/llvm/clang-format.el"))
    (when (file-exists-p clang-format-file)
      (load clang-format-file)))

  (defun my-create-clang-format ()
    "Create .clang-format file at project root with LLVM style."
    (interactive)
    (let ((dir (expand-file-name default-directory))
          (project-root nil))
      (while (and dir (not (string= dir "/")) (not project-root))
        (if (file-directory-p (expand-file-name ".git" dir))
            (setq project-root dir)
          (setq dir (file-name-directory (directory-file-name dir)))))
      (if (not project-root)
          (message "No git repository found.")
        (let ((clang-format-path (expand-file-name ".clang-format" project-root)))
          (if (file-exists-p clang-format-path)
              (message ".clang-format already exists at %s" clang-format-path)
            (let ((default-directory project-root))
              (shell-command "clang-format -style=llvm -dump-config > .clang-format")
              (message ".clang-format created at %s" clang-format-path)))))))

  :config
  ;; Custom formatters
  (push '(my-clang-format . (clang-format-buffer)) apheleia-formatters)
  (push '(my-gofumpt . ("gofumpt")) apheleia-formatters)

  ;; Mode-specific formatter assignments
  (setf (alist-get 'python-ts-mode apheleia-mode-alist) '(ruff-isort ruff)
        (alist-get 'go-ts-mode apheleia-mode-alist)     '(my-gofumpt)
        (alist-get 'c-ts-mode apheleia-mode-alist)      '(my-clang-format)))

(use-package mise
  :ensure t
  :hook (elpaca-after-init . global-mise-mode))

(use-package indent-bars
  :ensure t
  :hook ((python-ts-mode yaml-ts-mode) . indent-bars-mode))

;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:

(provide 'my-programming)
;;; my-programming.el ends here
