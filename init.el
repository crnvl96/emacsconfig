;;; init.el --- Init -*- lexical-binding: t; -*-

;;; Commentary:

;; My personal Emacs configuration.

;;; Code:

;;; UI
(menu-bar-mode -1)
(tool-bar-mode -1)
(column-number-mode t)
(size-indication-mode t)
(setq use-dialog-box nil)
(setq inhibit-startup-screen t)
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)
(setq inhibit-compacting-font-caches t)

;;; Package initialization
(package-initialize)
(unless (package-installed-p 'use-package)
  (unless (seq-empty-p package-archive-contents)
    (package-refresh-contents))
  (package-install 'use-package))
(require 'use-package)
(setq package-archives '( ("melpa"        . "https://melpa.org/packages/")
                          ("gnu"          . "https://elpa.gnu.org/packages/")
                          ("nongnu"       . "https://elpa.nongnu.org/nongnu/")
                          ("melpa-stable" . "https://stable.melpa.org/packages/"))
      package-archive-priorities '( ("melpa"        . 90)
                                    ("gnu"          . 70)
                                    ("nongnu"       . 60)
                                    ("melpa-stable" . 50)))

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
(set-face-attribute 'default nil :height 240 :weight 'normal :family "Iosevka")

;;; Scrolling
(setq scroll-margin 8
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

;;; Edit mode modification
(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)

;; As the side line number increases in digits, the whole window
;; needs to be shifted right, which cost computation.
;; This is why as least some space need to be separated from start.
(setq-default display-line-numbers-width 5)

(setq tab-width 4)

(setq-default display-line-numbers-type 'relative)
(add-hook 'after-init-hook #'global-display-line-numbers-mode)

;; Note:
;; Not really sure if we want to load the custom file.
;; (if (file-exists-p custom-file) (load custom-file)
;;   (progn
;;     (make-empty-file custom-file)
;;     (load custom-file )))

(setq native-comp-async-query-on-exit t)
(setq package-install-upgrade-built-in t)
(setq read-process-output-max (* 2 1024 1024))  ; 2mb
(setq initial-major-mode 'fundamental-mode)
(setq completion-ignore-case t)
(setq tab-always-indent 'complete)

;; Save existing clipboard text into the kill ring before replacing it.
(setq save-interprogram-paste-before-kill t)

(setq auto-revert-interval 3)
(setq auto-revert-remote-files nil)
(setq auto-revert-use-notify t)
(setq auto-revert-avoid-polling nil)
(setq auto-revert-verbose t)
(add-hook 'after-init-hook #'global-auto-revert-mode)

;; Don't split lines if they're too big
(setq truncate-lines t)

(setq-default fill-column 120)
(setq whitespace-line-column 120)
(setq whitespace-style '(face tabs empty trailing lines-tail))
(dolist (hook '(prog-mode-hook text-mode-hook))
  (add-hook hook #'whitespace-mode))
(add-hook 'before-save-hook #'whitespace-cleanup)

(setq make-backup-files nil)
(setq ring-bell-function 'ignore)

(setq save-place-limit 400)
(setq-default save-place t)
(add-hook 'after-init-hook #'save-place-mode)

;; When auto-save-visited-mode is enabled, Emacs will auto-save file-visiting
;; buffers after a certain amount of idle time if the user forgets to save it
;; with save-buffer or C-x s for example.
;;
;; This is different from auto-save-mode: auto-save-mode periodically saves
;; all modified buffers, creating backup files, including those not associated
;; with a file, while auto-save-visited-mode only saves file-visiting buffers
;; after a period of idle time, directly saving to the file itself without
;; creating backup files.
(setq auto-save-visited-interval 5)   ; Save after 5 seconds if inactivity
(add-hook 'after-init-hook #'auto-save-visited-mode)

(setq savehist-autosave-interval 60)
(setq savehist-additional-variables
      '(kill-ring                        ; clipboard
	register-alist                   ; macros
	mark-ring global-mark-ring       ; marks
	search-ring regexp-search-ring))
(add-hook 'after-init-hook #'savehist-mode)

(setq recentf-max-saved-items 500)
(setq recentf-max-menu-items 15)
(setq recentf-auto-cleanup (if (daemonp) 300 'never))
(setq recentf-exclude
      (list "\\.tar$" "\\.tbz2$" "\\.tbz$" "\\.tgz$" "\\.bz2$"
	    "\\.bz$" "\\.gz$" "\\.gzip$" "\\.xz$" "\\.zip$"
	    "\\.7z$" "\\.rar$"
	    "COMMIT_EDITMSG\\'"
	    "\\.\\(?:gz\\|gif\\|svg\\|png\\|jpe?g\\|bmp\\|xpm\\)$"
	    "-autoloads\\.el$" "autoload\\.el$"))
;; A cleanup depth of -90 ensures that `recentf-cleanup' runs before
;; `recentf-save-list', allowing stale entries to be removed before the list
;; is saved by `recentf-save-list', which is automatically added to
;; `kill-emacs-hook' by `recentf-mode'.
(require 'recentf)
(add-hook 'kill-emacs-hook #'recentf-cleanup -90)
(add-hook 'after-init-hook #'recentf-mode)

(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

;; dired - reuse current buffer by pressing 'a'
(put 'dired-find-alternate-file 'disabled nil)
;; always delete and copy recursively
(setq dired-recursive-deletes 'always)
(setq dired-recursive-copies 'always)
;; if there is a dired buffer displayed in the next window, use its
;; current subdir, instead of the current subdir of this dired buffer
(setq dired-dwim-target t)
;; enable some really cool extensions like C-x C-j (dired-jump)
(require 'dired-x)

(fset 'yes-or-no-p 'y-or-n-p)
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

(defun my-compilation-filter-hook ()
  "Allow rendering ansi symbols and colors."
  (ansi-color-apply-on-region compilation-filter-start (point-max)))

(add-hook 'compilation-filter-hook #'my-compilation-filter-hook)

(add-hook 'isearch-update-post-hook
          (lambda ()
            (when (memq this-command '(isearch-repeat-forward isearch-repeat-backward))
              (recenter))))
(add-hook 'isearch-mode-end-hook #'recenter)

(add-hook 'after-init-hook #'display-time-mode)
(add-hook 'after-init-hook #'delete-selection-mode)
(add-hook 'after-init-hook #'winner-mode)
(add-hook 'after-init-hook #'global-hl-line-mode)

;;; Which Key
(use-package which-key
  :ensure t
  :hook (after-init . which-key-mode)
  :config
  ;; Time after the suggestion show
  (setq which-key-idle-delay 0.5)
  (setq which-key-sort-order 'which-key-key-order-alpha)
  (setq which-key-min-display-lines 3)
  (setq which-key-max-display-columns 8)
  (setq which-key-add-column-padding 4)
  (setq which-key-unicode-correction 3)
  (setq which-key-max-description-length 30)
  (which-key-setup-side-window-bottom))

;;; Keymaps
(global-set-key (kbd "C-x ;") 'comment-or-uncomment-region)
(global-set-key (kbd "C-M-j") 'forward-paragraph)
(global-set-key (kbd "C-M-k") 'backward-paragraph)
(global-set-key (kbd "C-x C-b") #'ibuffer)

;; Some useful remappings
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(global-set-key (kbd "C-g") 'keyboard-escape-quit)
(global-set-key (kbd "M-j") 'next-line)
(global-set-key (kbd "M-k") 'previous-line)

(defun my-split-window-vertically ()
  "Keep the cursor on the current window when splitting it vertically."
  (interactive)
  (split-window-vertically) (other-window 1))
(global-set-key (kbd "C-x 2") #'my-split-window-vertically)

(defun my-split-window-horizontally ()
  "Keep the cursor on the current window when splitting it horizontally."
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

(defun my-scroll-up-half ()
  "Scroll up half screen and center the cursor."
  (interactive)
  (scroll-up (/ (window-height) 2))
  (recenter))
(global-set-key (kbd "C-v") #'my-scroll-up-half)

(defun my-scroll-down-half ()
  "Scroll down half screen and center the cursor."
  (interactive)
  (scroll-down (/ (window-height) 2))
  (recenter))
(global-set-key (kbd "M-v") #'my-scroll-down-half)

;;; Treesit
(setq treesit-language-source-alist
      '(  ; use `sort-lines' to sort
        (bash . ("https://github.com/tree-sitter/tree-sitter-bash"))
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
(add-to-list 'auto-mode-alist '("\\.m?js\\'" . js-ts-mode))

;;; Eglot & Formatters
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
      (sql . t)))
  :bind
  ("C-c a" . org-agenda))

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

;;; Json-mode
(use-package json-mode
  :ensure t
  :mode (("\\.json\\'" . json-mode)))

;;; Go-mode
(use-package go-mode
  :ensure t
  :defer t)

;;; Typst-ts-mode
(use-package typst-ts-mode
  :vc ( :url "https://codeberg.org/meow_king/typst-ts-mode.git"
        :rev :newest)
  :mode (("\\.typ\\'" . typst-ts-mode))
  :config
  (setq typst-ts-watch-options "--open")
  (setq typst-ts-mode-grammar-location (expand-file-name "tree-sitter/libtree-sitter-typst.so" user-emacs-directory))
  (setq typst-ts-mode-enable-raw-blocks-highlight t)
  :bind ( :map typst-ts-mode-map
          ("C-c C-c" . typst-ts-tmenu)))

;;; Delight
(use-package delight
  :ensure t
  :demand t
  :config
  (delight 'whitespace-mode nil "whitespace")
  (delight 'which-key-mode nil "which-key")
  (delight 'visual-line-mode nil "simple")
  (delight 'eldoc-mode nil "eldoc"))

;;; Beacon

(use-package beacon
  :ensure t
  :hook (after-init . beacon-mode))

;;; Spacious padding
(use-package spacious-padding
  :ensure t
  :hook (after-init . spacious-padding-mode)
  :config
  ;; These are the default values, but I keep them here for visibility.
  (setq spacious-padding-widths
	'( :internal-border-width 15
           :header-line-width 4
           :mode-line-width 6
           :tab-width 4
           :right-divider-width 30
           :scroll-bar-width 8
           :fringe-width 8))
  ;; Read the doc string of `spacious-padding-subtle-mode-line'
  (setq spacious-padding-subtle-frame-lines
	`( :mode-line-active 'default
           :mode-line-inactive vertical-border)))

;;; Buffer Terminator
(use-package buffer-terminator
  :ensure t
  :hook (after-init . buffer-terminator-mode)
  :config
  (setq buffer-terminator-verbose nil)
  (setq buffer-terminator-inactivity-timeout (* 30 60)) ; 30 minutes
  (setq buffer-terminator-interval (* 10 60))) ; 10 minutes

;;; Rainbow Delimiters
(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

;;; Rainbow mode
(use-package rainbow-mode
  :ensure t
  :hook (prog-mode . rainbow-mode))

;;; Ace window
(use-package ace-window
  :ensure t
  :config
  (setq aw-keys '(?h ?j ?k ?l))
  :bind
  (("M-o" . ace-window)
   ([remap other-window] . ace-window)))

;;; Avy
(use-package avy
  :ensure t
  :bind (("M-e" . avy-goto-char-timer))
  :config
  (setq avy-background t))

;;; Zop
(use-package zop-to-char
  :ensure t
  :bind (("M-z" . zop-to-char)
         ("M-Z" . zop-up-to-char)))

;;; Multiple-cursors
(use-package multiple-cursors
  :ensure t
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)))

;;; Expand region

(use-package expand-region
  :ensure t
  :bind (("C-=" . er/expand-region)))

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
  :config
  (setq completion-styles '(orderless basic))
  (setq completion-category-overrides '((file (styles partial-completion))
					(eglot (styles orderless))
					(eglot-capf (styles orderless)))))

;;; Corfu
(use-package corfu
  :ensure t
  :hook
  (after-init . global-corfu-mode)
  (after-init . corfu-history-mode)
  (after-init . corfu-popupinfo-mode)
  (eshell-mode . (lambda ()
		   (setq-local corfu-auto nil)
		   (corfu-mode)))
  :config
  (setq corfu-cycle t
	corfu-preselect 'prompt)
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
  :hook ((emacs-lisp-mode .  my/cape-capf-setup-elisp)
	 (eglot-managed-mode . my/cape-capf-setup-eglot))
  :init
  (defun my/cape-capf-setup-elisp ()
    (let ((result nil))
      (dolist (element '(cape-file cape-dabbrev cape-elisp-symbol) result)
	(add-to-list 'completion-at-point-functions element))))
  (defun my/cape-capf-setup-eglot ()
    (let ((result nil))
      (dolist (element '(eglot-completion-at-point cape-file) result)
	(add-to-list 'completion-at-point-functions element))))
  :config
  (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster))

;;; Consult
(use-package consult
  :ensure t
  :config
  (setq consult-async-min-input 2
        consult-narrow-key "<")
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

;;; Jinx - spell checker
(use-package jinx
  :ensure t
  :bind (("M-$" . jinx-correct)
	 ("C-." . jinx-correct)
	 ("C-M-$" . jinx-languages)))

;;; Pyvenv
(use-package pyvenv
  :ensure t
  :commands (pyvenv-activate))

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
  :bind (([remap move-beginning-of-line] . crux-move-beginning-of-line)
         ([remap kill-whole-line] . crux-kill-whole-line)
         ([remap keyboard-quit]  . crux-keyboard-quit-dwin)
         ([remap upcase-region] . crux-upcase-region)
         ([remap downcase-region] . crux-downcase-region)
         ("C-c j" . crux-top-join-line)
         ("C-c n" . crux-cleanup-buffer-or-region)
         ("C-c t" . crux-visit-term-buffer)
         ("C-c d" . crux-duplicate-current-line-or-region)
         ("C-k" . crux-smart-kill-line)
         ("S-<return>" . crux-smart-open-line)
         ("C-S-<return>" . crux-smart-open-line-above)
         ("C-c o" . crux-open-with)
         ("C-c u" . crux-view-url)
         ("C-c k" . crux-kill-other-buffers))
  :config
  (crux-with-region-or-line comment-or-uncomment-region)
  (crux-with-region-or-sexp-or-line kill-region)
  (crux-with-region-or-point-to-eol kill-ring-save))

;;; Flycheck
(use-package flycheck
  :ensure t
  :hook (after-init . global-flycheck-mode)
  :config
  (setq flymake-fringe-indicator-position 'left-fringe)
  (setq flymake-start-on-save-buffer t))

;;; Flyckeck-Eglot
(use-package flycheck-eglot
  :ensure t
  :after (flycheck eglot)
  :config
  (global-flycheck-eglot-mode 1))

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

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; End:

;;; init.el ends here
