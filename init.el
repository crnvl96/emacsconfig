;;; init.el --- Init -*- lexical-binding: t; -*-

(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)
(add-hook 'after-init-hook
          (lambda ()
            (setq gc-cons-threshold (* 64 1024 1024)  ; 64 MB
                  gc-cons-percentage 0.1)
            (message "Garbage collection thresholds reset after init.")))

(mapc #'disable-theme custom-enabled-themes)
(load-theme 'modus-vivendi t)
(set-face-attribute 'default nil :height 240 :weight 'normal :family "Iosevka")

(add-to-list 'display-buffer-alist
             '("\\`\\*\\(Warnings\\|Compile-Log\\)\\*\\'"
               (display-buffer-no-window)
               (allow-no-window . t)))

(setq-default display-line-numbers-type 'relative)

(if (boundp 'use-short-answers)
    (setq use-short-answers t)
  (advice-add 'yes-or-no-p :override #'y-or-n-p))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(setq native-comp-async-query-on-exit t)
(setq package-install-upgrade-built-in t)
(setq completion-ignore-case t)
(setq whitespace-style '(face trailing empty))
(setq make-backup-files nil)

(add-hook 'compilation-filter-hook
          (lambda ()
            (ansi-color-apply-on-region compilation-filter-start (point-max))))
(add-hook 'after-init-hook #'global-display-line-numbers-mode)
(add-hook 'after-init-hook #'display-time-mode)
(add-hook 'after-init-hook #'delete-selection-mode)
(add-hook 'after-init-hook #'winner-mode)
(add-hook 'after-init-hook #'global-whitespace-mode)
(add-hook 'after-init-hook #'save-place-mode)
(add-hook 'after-init-hook #'savehist-mode)
(add-hook 'after-init-hook #'recentf-mode)
(add-hook 'after-init-hook #'global-auto-revert-mode)

(global-set-key (kbd "C-x ;") 'comment-or-uncomment-region)
(global-set-key (kbd "M-n") 'forward-paragraph)
(global-set-key (kbd "M-p") 'backward-paragraph)
(global-set-key (kbd "C-x 2") (lambda ()
				(interactive)
				(split-window-vertically)
				(other-window 1)))
(global-set-key (kbd "C-x 3") (lambda ()
				(interactive)
				(split-window-horizontally)
				(other-window 1)))
(define-key minibuffer-local-map (kbd "C-/") (lambda ()
					       (interactive)
					       (let ((file-name (with-current-buffer (window-buffer (minibuffer-selected-window))
								  (file-name-nondirectory (buffer-file-name)))))
						 (insert file-name))))

(which-key-add-key-based-replacements
  "C-x p" "Project"
  "C-c c" "Crux"
  "C-c f" "Find")
(add-hook 'after-init-hook #'which-key-mode)

(use-package delight
  :ensure t
  :demand t
  :config
  (delight 'whitespace-mode nil "whitespace")
  (delight 'which-key-mode nil "which-key")
  (delight 'zoom-mode nil "zoom")
  (delight 'visual-line-mode nil "simple")
  (delight 'eldoc-mode nil "eldoc"))

(use-package zoom
  :ensure t
  :commands (zoom)
  :hook (after-init . zoom-mode)
  :config
  (setq zoom-size '(0.618 . 0.618))
  :bind (("C-c w o" . zoom)
         ("C-c w u" . winner-undo)))

(use-package ace-window
  :ensure t
  :config
  (setq aw-dispatch-always t)
  (setq aw-keys '(?h ?j ?k ?l))
  :bind ("M-o" . ace-window))

(use-package avy
  :ensure t
  :config
  (setq avy-keys (number-sequence ?a ?y))
  :bind (("M-i" . avy-goto-char)
         ("M-e" . avy-goto-word-0)))

(use-package expand-region
  :ensure t
  :bind (("C-=" . er/expand-region)))

(use-package multiple-cursors
  :ensure t
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)))

(use-package easy-kill
  :ensure t
  :bind (([remap kill-ring-save] . easy-kill)
         ([remap mark-sexp] . easy-mark)))

(use-package marginalia
  :ensure t
  :hook (after-init . marginalia-mode))

(use-package vertico
  :ensure t
  :hook (after-init . vertico-mode)
  :bind ( :map vertico-map
          ("<backspace>" . vertico-directory-delete-char)
          ("C-j" . vertico-next)
          ("C-k" . vertico-previous)
          ("C-w" . vertico-directory-delete-word )
          ("RET" . vertico-directory-enter)))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package corfu
  :ensure t
  :hook
  (after-init . global-corfu-mode)
  (after-init . corfu-history-mode)
  (after-init . corfu-popupinfo-mode)
  :config
  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (setq tab-always-indent 'complete)
  ;; Emacs 30 and newer: Disable Ispell completion function.
  ;; Try `cape-dict' as an alternative.
  (setq text-mode-ispell-word-completion nil)
  ;; Hide commands in M-x which do not apply to the current mode.  Corfu
  ;; commands are hidden, since they are not used via M-x. This setting is
  ;; useful beyond Corfu.
  (setq read-extended-command-predicate #'command-completion-default-include-p)
  (setq corfu-cycle t)
  (setq corfu-preselect 'prompt)
  :bind ( :map corfu-map
	  ("RET" . nil)
	  ([ret] . nil)
          ("TAB" . corfu-next)
          ([tab] . corfu-next)
          ("S-TAB" . corfu-previous)
          ([backtab] . corfu-previous)))

(use-package consult
  :ensure t
  :config
  (setq consult-async-min-input 2
        consult-narrow-key "<")
  (consult-customize
   consult-buffer  consult-yank-pop consult-fd consult-outline
   consult-imenu consult-info consult-flymake consult-history
   consult-focus-lines consult-line consult-ripgrep consult-goto-line
   :preview-key nil)
  :bind (("C-c f b" . consult-buffer)
         ("C-c f f" . consult-fd)
         ("C-c f o" . consult-outline)
         ("C-c f k" . consult-flymake)
         ("C-c f l" . consult-line)
         ("C-c f g" . consult-ripgrep)
         ("C-c f L" . consult-goto-line)))

(use-package rg
  :ensure t
  :defer t
  :config
  (rg-enable-menu))

(setq treesit-language-source-alist
      '(  ; use `sort-lines' to sort
        (bash . ("https://github.com/tree-sitter/tree-sitter-bash"))
        (d2 . ("https://github.com/ravsii/tree-sitter-d2"))
        (c . ("https://github.com/tree-sitter/tree-sitter-c"))
        (html . ("https://github.com/tree-sitter/tree-sitter-html"))
        (lua . ("https://github.com/tree-sitter-grammars/tree-sitter-lua"))
        (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript"))
        (go . ("https://github.com/tree-sitter/tree-sitter-go"))
        (jsdoc . ("https://github.com/tree-sitter/tree-sitter-jsdoc"))
        (json . ("https://github.com/tree-sitter/tree-sitter-json"))
        (python . ("https://github.com/tree-sitter/tree-sitter-python"))
        (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" nil "tsx/src"))
        (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" nil "typescript/src"))
        (typst . ("https://github.com/uben0/tree-sitter-typst"))
        (yaml . ("https://github.com/ikatyang/tree-sitter-yaml"))
        (toml . ("https://github.com/ikatyang/tree-sitter-toml"))
        (dockerfile . ("https://github.com/camdencheek/tree-sitter-dockerfile"))
        (markdown . ("https://github.com/tree-sitter-grammars/tree-sitter-markdown" nil "tree-sitter-markdown/src"))
        (markdown-inline . ("https://github.com/tree-sitter-grammars/tree-sitter-markdown" nil "tree-sitter-markdown-inline/src"))
        ))

(defun my/treesit-install-all-languages ()
  "Install all languages specified by `treesit-language-source-alist'."
  (interactive)
  (let ((languages (mapcar 'car treesit-language-source-alist)))
    (dolist (lang languages)
      (treesit-install-language-grammar lang)
      (message "`%s' parser was installed." lang)
      (sit-for 0.75))))

(add-to-list 'major-mode-remap-alist '(js-json-mode . json-ts-mode))
(add-to-list 'auto-mode-alist '("\\.ya?ml\\'" . yaml-ts-mode))
(add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))
(add-to-list 'major-mode-remap-alist '(go-mode . go-ts-mode))
(add-to-list 'major-mode-remap-alist '(lua-mode . lua-ts-mode))
(add-to-list 'major-mode-remap-alist '(d2-mode . d2-ts-mode))
(add-to-list 'auto-mode-alist '("\\.m?js\\'" . js-ts-mode))

(use-package apheleia
  :ensure t
  :delight
  :hook (prog-mode . apheleia-mode)
  :config
  (setf (alist-get 'python-ts-mode apheleia-mode-alist) '(ruff-isort ruff))
  (setf (alist-get 'go-ts-mode apheleia-mode-alist) '(gofumpt)))

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

(use-package pyvenv
  :ensure t
  :defer t)

(use-package json-mode
  :ensure t
  :mode (("\\.json\\'" . json-mode)))

(use-package d2-mode
  :ensure t
  :defer t)

(use-package go-mode
  :ensure t
  :defer t)

(use-package
  typst-ts-mode
  :vc ( :url "https://codeberg.org/meow_king/typst-ts-mode.git"
        :rev :newest)
  :mode (("\\.typ\\'" . typst-ts-mode))
  :config
  (setq typst-ts-watch-options "--open")
  (setq typst-ts-mode-grammar-location (expand-file-name "tree-sitter/libtree-sitter-typst.so" user-emacs-directory))
  (setq typst-ts-mode-enable-raw-blocks-highlight t)
  :bind ( :map typst-ts-mode-map
          ("C-c C-c" . typst-ts-tmenu)))

(use-package lua-mode
  :ensure t
  :defer t)

(use-package elisp-mode
  :ensure nil
  :delight (emacs-lisp-mode "Elisp" :major))

(use-package markdown-mode
  :ensure t
  :commands (gfm-mode
             gfm-view-mode
             markdown-mode
             markdown-view-mode)
  :mode (("\\.markdown\\'" . markdown-mode)
         ("\\.md\\'" . markdown-mode)
         ("README\\.md\\'" . gfm-mode)))

(use-package org
  :ensure nil
  :delight (org-indent-mode "" "org-indent")
  :commands (org-mode org-version)
  :mode (("\\.org\\'" . org-mode))
  :hook (org-mode . org-indent-mode)
  :config
  (setq org-M-RET-may-split-line '((default . nil))
        org-insert-heading-respect-content t
        org-log-done 'time
        org-log-into-drawer t
        org-directory "~/Developer/personal/notes/agenda/"
        org-agenda-files (list org-directory)
        org-todo-keywords '((sequence "TODO(t)" "WAIT(w!)" "|" "CANCEL(c!)" "DONE(d!)"))
        org-confirm-babel-evaluate nil)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '( (emacs-lisp . t)
      (python . t)
      (shell . t)
      (js . t)
      (d2 . t)
      (sql . t)))
  :bind
  ("C-c a" . org-agenda))

(use-package ob-d2
  :ensure t
  :defer t
  :vc ( :url "https://github.com/dmacvicar/ob-d2"
        :rev :newest))

(use-package trashed
  :ensure t
  :commands (trashed)
  :config
  (setq trashed-action-confirmer 'y-or-n-p)
  (setq trashed-use-header-line t)
  (setq trashed-sort-key '("Date deleted" . t))
  (setq trashed-date-format "%Y-%m-%d %H:%M:%S"))

(use-package exec-path-from-shell
  :ensure t
  :demand t
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package buffer-terminator
  :ensure t
  :delight
  :hook (after-init . buffer-terminator-mode)
  :config (setq buffer-terminator-verbose nil
                buffer-terminator-inactivity-timeout (* 20 60)
                buffer-terminator-interval (* 20 60)))

(use-package super-save
  :ensure t
  :delight
  :hook (after-init . super-save-mode)
  :init (setq auto-save-default nil)
  :config (setq super-save-auto-save-when-idle t
                super-save-delete-trailing-whitespace t
                super-save-all-buffers t)
  (add-to-list 'super-save-triggers 'ace-window)
  (add-to-list 'super-save-hook-triggers 'find-file-hook))

(use-package undo-fu
  :demand t
  :ensure t
  :commands (undo-fu-only-undo
             undo-fu-only-redo
             undo-fu-only-redo-all
             undo-fu-disable-checkpoint)
  :config
  (global-unset-key (kbd "C-z"))
  (global-set-key (kbd "C-z") 'undo-fu-only-undo)
  (global-set-key (kbd "C-S-z") 'undo-fu-only-redo))

(use-package undo-fu-session
  :ensure t
  :hook (after-init . undo-fu-session-global-mode)
  :commands (undo-fu-session-global-mode))

(use-package helpful
  :ensure t
  :commands (helpful-callable
             helpful-variable
             helpful-key
             helpful-command
             helpful-at-point
             helpful-function)
  :config
  (setq helpful-max-buffers 3)
  :bind
  ([remap describe-command] . helpful-command)
  ([remap describe-function] . helpful-callable)
  ([remap describe-key] . helpful-key)
  ([remap describe-symbol] . helpful-symbol)
  ([remap describe-variable] . helpful-variable))

(use-package crux
  :ensure t
  :demand t
  :bind (([remap move-beginning-of-line] . crux-move-beginning-of-line)
         ([remap kill-whole-line] . crux-kill-whole-line)
         ([remap keyboard-quit]  . crux-keyboard-quit-dwin)
         ([remap upcase-region] . crux-upcase-region)
         ([remap downcase-region] . crux-downcase-region)
         ("C-c c j" . crux-top-join-line)
         ("C-c c n" . crux-cleanup-buffer-or-region)
         ("C-c c t" . crux-visit-term-buffer)
         ("C-c c d" . crux-duplicate-current-line-or-region)
         ("C-k" . crux-smart-kill-line)
         ("S-<return>" . crux-smart-open-line)
         ("C-S-<return>" . crux-smart-open-line-above)
         ("C-c c o" . crux-open-with)
         ("C-c c u" . crux-view-url)
         ("C-c c k" . crux-kill-other-buffers))
  :config
  (crux-with-region-or-line comment-or-uncomment-region)
  (crux-with-region-or-sexp-or-line kill-region)
  (crux-with-region-or-point-to-eol kill-ring-save))

(use-package nov
  :ensure t
  :mode (("\\.epub\\'" . nov-mode)))

(use-package editorconfig
  :ensure t
  :hook (after-init . editorconfig-mode))

(use-package mise
  :ensure t
  :hook (after-init . global-mise-mode))

(use-package magit
  :ensure t
  :defer t)
