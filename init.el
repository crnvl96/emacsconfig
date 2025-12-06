;;; init.el --- Init -*- lexical-binding: t; -*-

;;; Garbage Collection

(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)
(add-hook 'after-init-hook
          (lambda ()
            (setq gc-cons-threshold (* 64 1024 1024)  ; 64 MB
                  gc-cons-percentage 0.1)
            (message "Garbage collection thresholds reset after init.")))

;;; Themes and Fonts

(mapc #'disable-theme custom-enabled-themes)
(load-theme 'modus-vivendi t)
(set-face-attribute 'default nil :height 160 :weight 'normal :family "Iosevka")

;;; Scrolling

(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)
(when (fboundp 'pixel-scroll-precision-mode)
  (pixel-scroll-precision-mode t))

;;; Frames

(add-to-list 'initial-frame-alist '(fullscreen . maximized))
(add-to-list 'display-buffer-alist
             '("\\`\\*\\(Warnings\\|Compile-Log\\)\\*\\'"
               (display-buffer-no-window)
               (allow-no-window . t)))

;;; Options

(fset 'yes-or-no-p 'y-or-n-p)

(setq-default display-line-numbers-type 'relative)
(setq-default fill-column 80)
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(setq native-comp-async-query-on-exit t)
(setq package-install-upgrade-built-in t)
(setq completion-ignore-case t)
(setq whitespace-style '(face trailing empty))
(setq make-backup-files nil)
(setq ring-bell-function 'ignore)
(setq inhibit-startup-screen t)
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;;; Hooks

(defun my-compilation-filter-hook ()
  (ansi-color-apply-on-region compilation-filter-start (point-max)))

(add-hook 'compilation-filter-hook #'my-compilation-filter-hook)
(add-hook 'after-init-hook #'which-key-mode)
(add-hook 'after-init-hook #'global-display-line-numbers-mode)
(add-hook 'after-init-hook #'display-time-mode)
(add-hook 'after-init-hook #'delete-selection-mode)
(add-hook 'after-init-hook #'winner-mode)
(add-hook 'after-init-hook #'global-whitespace-mode)
(add-hook 'after-init-hook #'save-place-mode)
(add-hook 'after-init-hook #'savehist-mode)
(add-hook 'after-init-hook #'recentf-mode)
(add-hook 'after-init-hook #'global-auto-revert-mode)

;;; Keymaps

(global-set-key (kbd "C-x ;") 'comment-or-uncomment-region)
(global-set-key (kbd "M-n") 'forward-paragraph)
(global-set-key (kbd "M-p") 'backward-paragraph)
(global-set-key (kbd "C-x C-b") #'ibuffer)

(defun my-split-window-vertically ()
  (interactive)
  (split-window-vertically) (other-window 1))

(global-set-key (kbd "C-x 2") #'my-split-window-vertically)

(defun my-split-word-horizontally ()
  (interactive)
  (split-window-horizontally) (other-window 1))

(global-set-key (kbd "C-x 3") #'my-split-window-horizontally)

(defun my-forward-word ()
  "Move forward to the next syntax change, like Vim word movement."
  (interactive)
  (let ((start-syntax (if (eobp) nil (char-syntax (char-after)))))
    (if start-syntax
        (progn
          (forward-char 1)
          (while (and (not (eobp)) (eq (char-syntax (char-after)) start-syntax))
            (forward-char 1)))
      (forward-char 1))))

(global-set-key (kbd "M-f") #'my-forward-word)

(defun my-backward-word ()
  "Move backward to the previous syntax change, like Vim word movement."
  (interactive)
  (let ((start-syntax (if (bobp) nil (char-syntax (char-before)))))
    (if start-syntax
        (progn
          (backward-char 1)
          (while (and (not (bobp)) (eq (char-syntax (char-before)) start-syntax))
            (backward-char 1)))
      (backward-char 1))))

(global-set-key (kbd "M-b") #'my-backward-word)

;;; Delight

(use-package delight
  :ensure t
  :demand t
  :config
  (delight 'whitespace-mode nil "whitespace")
  (delight 'which-key-mode nil "which-key")
  (delight 'visual-line-mode nil "simple")
  (delight 'eldoc-mode nil "eldoc"))

;;; Multiple-cursors

(use-package multiple-cursors
  :ensure t
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)))

;;; Marginalia

(use-package marginalia
  :ensure t
  :hook (after-init . marginalia-mode))

;;; Vertico

(use-package vertico
  :ensure t
  :hook (after-init . vertico-mode)
  :bind ( :map vertico-map
          ("<backspace>" . vertico-directory-delete-char)
          ("C-j" . vertico-next)
          ("C-k" . vertico-previous)
          ("C-w" . vertico-directory-delete-word )
          ("RET" . vertico-directory-enter)))

;;; Orderless

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles partial-completion)))))

;;; Corfu

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

;;; Cape

(use-package cape
  :ensure t
  :init
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block)
  (add-to-list 'completion-at-point-functions #'cape-elisp-symbol)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev))

;;; Consult

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

;;; Rg

(use-package rg
  :ensure t
  :defer t
  :config (rg-enable-menu))

;;; Treesitter

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

;;; Apheleia
(use-package apheleia
  :ensure t
  :delight
  :hook (prog-mode . apheleia-mode)
  :config
  (setf (alist-get 'python-ts-mode apheleia-mode-alist) '(ruff-isort ruff))
  (setf (alist-get 'go-ts-mode apheleia-mode-alist) '(gofumpt)))

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

;;; Pyvenv

(use-package pyvenv
  :ensure t
  :defer t)

;;; Json-mode

(use-package json-mode
  :ensure t
  :mode (("\\.json\\'" . json-mode)))

;;; D2-mode

(use-package d2-mode
  :ensure t
  :defer t)

;;; Go-mode

(use-package go-mode
  :ensure t
  :defer t)

;;; Typst-ts-mode

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

;;; Lua-mode

(use-package lua-mode
  :ensure t
  :defer t)

;;; Elisp-mode

(use-package elisp-mode
  :ensure nil
  :delight (emacs-lisp-mode "Elisp" :major))

;;; Markdown-mode

(use-package markdown-mode
  :ensure t
  :commands (gfm-mode
             gfm-view-mode
             markdown-mode
             markdown-view-mode)
  :mode (("\\.markdown\\'" . markdown-mode)
         ("\\.md\\'" . markdown-mode)
         ("README\\.md\\'" . gfm-mode)))

;;; Org-mode

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

;;; Ob-d2 (Org babel for D2)

(use-package ob-d2
  :ensure t
  :defer t
  :vc ( :url "https://github.com/dmacvicar/ob-d2"
        :rev :newest))

;;; Trashed

(use-package trashed
  :ensure t
  :commands (trashed)
  :config
  (setq trashed-action-confirmer 'y-or-n-p)
  (setq trashed-use-header-line t)
  (setq trashed-sort-key '("Date deleted" . t))
  (setq trashed-date-format "%Y-%m-%d %H:%M:%S"))

;;; Exec-path-from-shell

(use-package exec-path-from-shell
  :ensure t
  :demand t
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

;;; Undo-fu

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

;;; Undo-fu-session

(use-package undo-fu-session
  :ensure t
  :hook (after-init . undo-fu-session-global-mode)
  :commands (undo-fu-session-global-mode))

;;; Helpful

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

;;; Crux

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

;;; Nov

(use-package nov
  :ensure t
  :mode (("\\.epub\\'" . nov-mode)))

;;; Editorconfig

(use-package editorconfig
  :ensure t
  :hook (after-init . editorconfig-mode))

;;; Mise

(use-package mise
  :ensure t
  :hook (after-init . global-mise-mode))

;;; Magit

(use-package magit
  :ensure t
  :defer t)
