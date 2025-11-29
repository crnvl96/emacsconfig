;;; init.el --- Init -*- lexical-binding: t; -*-

;;; Early Initialization

;;; Garbage collection management - performance optimization

;; Increase garbage collection thresholds during startup for better performance.
;; This prevents Emacs from running garbage collection during initialization,
;; which can significantly slow down startup time. We set the threshold to a
;; very high value and increase the percentage to reduce GC frequency.
;; After initialization, we reset to more reasonable values.
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

;; Reset garbage collection thresholds after initialization to prevent excessive
;; memory usage during normal operation.
(add-hook 'after-init-hook
          (lambda ()
            (setq gc-cons-threshold (* 64 1024 1024)  ; 64 MB
                  gc-cons-percentage 0.1)
            (message "Garbage collection thresholds reset after init.")))

;;; Use-package

(package-initialize)
(unless (package-installed-p 'use-package)
  (unless (seq-empty-p package-archive-contents)
    (package-refresh-contents))
  (package-install 'use-package))

;; This is done to ensure that use-package is available for use throughout the config
(require 'use-package)
(setq package-archives '( ("melpa"        . "https://melpa.org/packages/")
                          ("gnu"          . "https://elpa.gnu.org/packages/")
                          ("nongnu"       . "https://elpa.nongnu.org/nongnu/")
                          ("melpa-stable" . "https://stable.melpa.org/packages/"))
      package-archive-priorities '( ("melpa"        . 90)
                                    ("gnu"          . 70)
                                    ("nongnu"       . 60)
                                    ("melpa-stable" . 50)))

;;; UI and Appearance

;;; UI elements - declutter interface

(setq inhibit-splash-screen t ; Disable splash screen
      use-file-dialog nil     ; Disable file dialog
      use-dialog-box nil)     ; Disable dialog box

(menu-bar-mode -1)   ; No menu bar
(tool-bar-mode -1)   ; No tool bar
(scroll-bar-mode -1) ; No scroll bar
(tooltip-mode -1)    ; No tooltips
(prefer-coding-system 'utf-8)

;;; Theme

(mapc #'disable-theme custom-enabled-themes)
(load-theme 'modus-vivendi t)

;;; Fonts

(set-face-attribute 'default nil :height 200 :weight 'normal :family "Iosevka")
(set-face-attribute 'variable-pitch nil :height 200 :weight 'normal :family "Iosevka Aile")

;;; General Settings and Hooks

;;; General

(setq-default display-line-numbers-type 'relative ; Relative lines
              word-wrap t                         ; Wrap lines at word boundaries
              indent-tabs-mode nil                ; Don't indent with tabs
              truncate-lines t                    ; Don't display continuation lines
              tab-width 4)                        ; How much spaces a tab count for

(add-hook 'after-init-hook #'global-display-line-numbers-mode)

;; Eval this only if we aren't on MacOS
(unless (and (eq window-system 'mac)
             (bound-and-true-p mac-carbon-version-string))
  (setq pixel-scroll-precision-use-momentum nil)
  (pixel-scroll-precision-mode 1))

(setq visible-bell nil             ; No visible bell
      ring-bell-function #'ignore) ; No ring bell

(if (boundp 'use-short-answers)
    (setq use-short-answers t)
  (advice-add 'yes-or-no-p :override #'y-or-n-p)) ; Allow single letter responses

(when (bound-and-true-p blink-cursor-mode)
  (blink-cursor-mode -1)) ; No blinking

(setq custom-file "~/.emacs.d/custom.el") ; Define custom file path
(setq native-comp-async-query-on-exit t)  ; Ask before killing async compilations
(setq package-install-upgrade-built-in t) ; Allow upgrading builtin packages
(setq compile-command nil)                ; Dont auto populate the compile command prompt
(setq kill-do-not-save-duplicates t)      ; Avoid duplications on kill-ring
(setq completion-ignore-case t)           ; Case insensitive completion

;;; Hooks

;; Enhance the compilation buffer with ansi colors
(add-hook 'compilation-filter-hook
          (lambda ()
            (ansi-color-apply-on-region compilation-filter-start (point-max))))

;; Display current time on modeline
(add-hook 'after-init-hook #'display-time-mode)
;; Highlight the current line
(add-hook 'after-init-hook #'global-hl-line-mode)
;; When Delete Selection mode is enabled, typed text replaces the selection
;; if the selection is active.  Otherwise, typed text is just inserted at
;; point regardless of any selection.
(add-hook 'after-init-hook #'delete-selection-mode)
;; Record the changes on window configuration
(add-hook 'after-init-hook #'winner-mode)

;;; Whitespace mode

;; Define the kinds of whitespaces that we want highlighted
(setq whitespace-style '(face trailing empty))
;; Enable whitespace mode
(add-hook 'after-init-hook #'global-whitespace-mode)

;;; Saveplace mode

;; Define target location under custom directory
(setq save-place-file (expand-file-name "saveplace" user-emacs-directory))
;; Increase the limit of entries
(setq save-place-limit 600)
;; Enable saveplace
(add-hook 'after-init-hook #'save-place-mode)

;;; Savehist mode

;; Increase the limit of entries
(setq history-length 300)
;; Also save the minibuffer history
(setq savehist-save-minibuffer-history t)
;; Also save additional variables
(setq savehist-additional-variables
      '(kill-ring                        ; clipboard
        register-alist                   ; macros
        mark-ring global-mark-ring       ; marks
        search-ring regexp-search-ring)) ; searches
;; Enable savehist
(add-hook 'after-init-hook #'savehist-mode)

;;; File and Buffer Management

;;; Buffer organization

(add-to-list 'display-buffer-alist
             '("\\`\\*\\(Warnings\\|Compile-Log\\)\\*\\'"
               (display-buffer-no-window)
               (allow-no-window . t)))

;;; Dired

(setq dired-recursive-copies 'always
      dired-recursive-deletes 'always
      delete-by-moving-to-trash t
      dired-dwim-target t
      dired-free-space nil
      dired-dwim-target t
      dired-deletion-confirmer 'y-or-n-p
      dired-filter-verbose nil
      dired-vc-rename-file t
      dired-create-destination-dirs 'ask
      dired-clean-confirm-killing-deleted-buffers nil)

(add-hook 'dired-mode-hook #'dired-hide-details-mode)
(add-hook 'dired-mode-hook  #'hl-line-mode)

;;; Recentf mode

;; Increase the limit of saved entries
(setq recentf-max-saved-items 300)
;; Increase the number of items in the menu
(setq recentf-max-menu-items 15)
;; Enable recentf
(add-hook 'after-init-hook #'recentf-mode)

;;; Autorevert mode

;; decrease the trigger interval
(setq auto-revert-interval 2)
;; The value is a list of regular expressions.
;; If the file name matches one of these regular expressions,
;; then revert-buffer reverts the file without querying
;; if the file has changed on disk and you have not edited the buffer.
(setq revert-without-query (list "."))
;; When non-nil, user input temporarily interrupts Auto-Revert Mode.
(setq auto-revert-stop-on-user-input nil)
(setq auto-revert-verbose t)
;; When nil, Global Auto-Revert Mode operates only on file-visiting buffers.
(setq global-auto-revert-non-file-buffers t)
;; List of major modes Global Auto-Revert Mode should not check.
(setq global-auto-revert-ignore-modes '(Buffer-menu-mode))
;; Enable autorevert
(add-hook 'after-init-hook #'global-auto-revert-mode)

;;; Version Control

;;; Ediff

;; Setup windows in a single frame
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
;; The function used to split the main window between buffer-A and buffer-B.
(setq ediff-split-window-function 'split-window-horizontally)

;;; VC (Version Control)

;; If non-nil, use the flag --follow when producing single file logs.
(setq vc-git-print-log-follow t)
;; Don't backup version controlled files
(setq vc-make-backup-files nil)
;; Algorithm for diffs
(setq vc-git-diff-switches '("--histogram"))

;;; Keybindings and Navigation

;;; Custom keymaps

(global-set-key (kbd "C-x ;") 'comment-or-uncomment-region)
(global-set-key (kbd "M-n") 'forward-paragraph)
(global-set-key (kbd "M-p") 'backward-paragraph)

(global-set-key (kbd "C-x 2")
                (lambda ()
                  (interactive)
                  (split-window-vertically)
                  (other-window 1)))

(global-set-key (kbd "C-x 3")
                (lambda ()
                  (interactive)
                  (split-window-horizontally)
                  (other-window 1)))

(define-key minibuffer-local-map (kbd "C-f")
            (lambda ()
              (interactive)
              (let ((file-name (with-current-buffer (window-buffer (minibuffer-selected-window))
                                 (file-name-nondirectory (buffer-file-name)))))
                (insert file-name))))

;;; Which-Key - labels and hints for emacs keymaps

(which-key-add-key-based-replacements
  "C-x p" "Project"
  "C-c c" "Crux"
  "C-c f" "Find")

(add-hook 'after-init-hook #'which-key-mode)

;;; Ace-window - Easy window management

(use-package ace-window
  :ensure t
  :bind ("M-o" . ace-window))

;;; Avy - Easy buffer navigation

(use-package avy
  :ensure t
  :config
  (setq avy-keys (number-sequence ?a ?y))
  :bind (("M-i" . avy-goto-char)
         ("M-e" . avy-goto-word-0)))

;;; Expand-region - A more convenient way to select text

(use-package expand-region
  :ensure t
  :bind (("C-=" . er/expand-region)))

;;; Multiple cursors

(use-package multiple-cursors
  :ensure t
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)))

;;; Better kill ring and mark

(use-package easy-kill
  :ensure t
  :bind (([remap kill-ring-save] . easy-kill)
         ([remap mark-sexp] . easy-mark)))

;;; Completion and Search

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
  :config (setq completion-styles '(orderless partial-completion basic)
                completion-category-defaults nil
                completion-category-overrides '((eglot (styles orderless))
                                                (eglot-capf (styles orderless)))))

;;; Corfu

(use-package corfu
  :ensure t
  :hook
  (after-init . global-corfu-mode)
  (after-init . corfu-history-mode)
  (after-init . corfu-popupinfo-mode)
  (minibuffer-setup . my/corfu-enable-always-in-minibuffer)
  :init
  (defun my/corfu-enable-always-in-minibuffer ()
    "Enable Corfu in the minibuffer if Vertico/Mct are not active."
    (unless (or (bound-and-true-p mct--active)
                (bound-and-true-p vertico--input))
      (setq-local corfu-auto nil)
      (corfu-mode 1)))
  (setq read-extended-command-predicate #'command-completion-default-include-p
        text-mode-ispell-word-completion nil
        tab-always-indent 'complete)
  (setq tab-first-completion 'word-or-paren-or-punct)
  :config
  (setq corfu-cycle t)
  (setq corfu-preselect 'prompt)
  (keymap-set corfu-map "RET" `( menu-item "" nil :filter
                                 ,(lambda (&optional _)
                                    (and (derived-mode-p 'eshell-mode 'comint-mode)))))
  :bind ( :map corfu-map
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
  :init
  (defun consult--orderless-regexp-compiler (input type &rest _config)
    (setq input (cdr (orderless-compile input)))
    (cons (mapcar (lambda (r) (consult--convert-regexp r type)) input)
          (lambda (str) (orderless--highlight input t str))))
  :config (setq consult-async-min-input 2
                consult-narrow-key "<"
                xref-show-xrefs-function #'consult-xref
                xref-show-definitions-function #'consult-xref
                consult--regexp-compiler #'consult--orderless-regexp-compiler)
  (consult-customize
   consult-buffer  consult-yank-pop consult-fd consult-outline
   consult-imenu consult-info consult-flymake consult-history
   consult-focus-lines consult-line consult-ripgrep consult-goto-line
   :preview-key nil)
  :bind (("C-c f b" . consult-buffer)
         ("C-c f y" . consult-yank-pop)
         ("C-c f f" . consult-fd)
         ("C-c f o" . consult-outline)
         ("C-c f i" . consult-imenu)
         ("C-c f I" . consult-info)
         ("C-c f k" . consult-flymake)
         ("C-c f h" . consult-history)
         ("C-c f u" . consult-focus-lines)
         ("C-c f l" . consult-line)
         ("C-c f g" . consult-ripgrep)
         ("C-c f L" . consult-goto-line)))

;;; RG.el

(use-package rg
  :ensure t
  :config
  (rg-enable-menu))

;;; Development Tools

;;; Treesit

(setq treesit-language-source-alist
      '(  ; use `sort-lines' to sort
        (bash . ("https://github.com/tree-sitter/tree-sitter-bash"))
        (c . ("https://github.com/tree-sitter/tree-sitter-c"))
        (html . ("https://github.com/tree-sitter/tree-sitter-html" "v0.23.0"))
        (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript"))
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
(add-to-list 'auto-mode-alist '("\\.m?js\\'" . js-ts-mode))

;;; Apheleia - Code formatter

(use-package apheleia
  :ensure t
  :delight
  :hook (prog-mode . apheleia-mode)
  :config
  (setf (alist-get 'python-ts-mode apheleia-mode-alist) '(ruff-isort ruff))
  (setf (alist-get 'go-ts-mode apheleia-mode-alist) '(gofumpt)))

;;; Eglot - LSP config

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
        eglot-events-buffer-config '(:size 0 :format lisp))
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

;;; Language Modes

;;; Python mode

(use-package pyvenv
  :ensure t)

;;; JSON mode

(use-package json-mode
  :ensure t
  :mode (("\\.json\\'" . json-mode)))

;;; Go mode

(use-package go-mode
  :ensure t)

;;; Elisp mode

(use-package elisp-mode
  :ensure nil
  :delight (emacs-lisp-mode "Elisp" :major))

;;; Markdown mode

(use-package markdown-mode
  :ensure t
  :commands (gfm-mode
             gfm-view-mode
             markdown-mode
             markdown-view-mode)
  :mode (("\\.markdown\\'" . markdown-mode)
         ("\\.md\\'" . markdown-mode)
         ("README\\.md\\'" . gfm-mode)))

;;; Org mode

(use-package org
  :ensure nil
  :delight (org-indent-mode "" "org-indent")
  :commands (org-mode org-version)
  :mode (("\\.org\\'" . org-mode))
  :hook (org-mode . org-indent-mode)
  :config (setq org-M-RET-may-split-line '((default . nil))
                org-insert-heading-respect-content t
                org-log-done 'time
                org-log-into-drawer t
                org-directory "~/Developer/personal/notes/agenda/"
                org-agenda-files (list org-directory)
                org-todo-keywords '((sequence "TODO(t)" "WAIT(w!)" "|" "CANCEL(c!)" "DONE(d!)")))
  :bind
  ("C-c a" . org-agenda))

;;; Utilities

;;; Delight - declutter the modeline

(use-package delight
  :ensure t
  :demand t
  :config
  (delight 'whitespace-mode nil "whitespace")
  (delight 'which-key-mode nil "which-key")
  (delight 'devil-mode nil "devil")
  (delight 'visual-line-mode nil "simple")
  (delight 'eldoc-mode nil "eldoc"))

;;; Thrashed - manage files on the thrash dir

(use-package trashed
  :ensure t
  :commands (trashed)
  :config
  (setq trashed-action-confirmer 'y-or-n-p)
  (setq trashed-use-header-line t)
  (setq trashed-sort-key '("Date deleted" . t))
  (setq trashed-date-format "%Y-%m-%d %H:%M:%S"))

;;; Make the shell path also available in Emacs

(use-package exec-path-from-shell
  :ensure t
  :demand t
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

;;; Auto kill unused buffers

(use-package buffer-terminator
  :ensure t
  :delight
  :hook (after-init . buffer-terminator-mode)
  :config (setq buffer-terminator-verbose nil
                buffer-terminator-inactivity-timeout (* 20 60)
                buffer-terminator-interval (* 20 60)))

;;; Auto save files

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

;;; Auto manage undo

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

;;; A better help menu

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

;;; A plethora of utilities

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

;;; Epub reader

(use-package nov
  :ensure t
  :mode (("\\.epub\\'" . nov-mode)))

;;; Editorconfig support

(use-package editorconfig
  :ensure t
  :hook (after-init . editorconfig-mode))

;;; Mise support

(use-package mise
  :ensure t
  :hook (after-init . global-mise-mode))

;;; Magit

(use-package magit
  :ensure t)
