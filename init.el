;;; init.el --- Init -*- lexical-binding: t -*-

;;; Commentary:

;; My personal Emacs configuration.

;;; Code:

(require 'package)
(package-initialize)

(when (< emacs-major-version 29)
  (unless (package-installed-p 'use-package)
    (unless (seq-empty-p package-archive-contents)
      (package-refresh-contents))
    (package-install 'use-package)))

(require 'use-package)
(setq package-archives
      '(("melpa"        . "https://melpa.org/packages/")
        ("gnu"          . "https://elpa.gnu.org/packages/")
        ("nongnu"       . "https://elpa.nongnu.org/nongnu/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")))
(setq package-archive-priorities
      '(("melpa"        . 90)
        ("gnu"          . 70)
        ("nongnu"       . 60)
        ("melpa-stable" . 50)))

(setq gc-cons-threshold most-positive-fixnum)
(setq gc-cons-percentage 0.6)

(add-hook 'after-init-hook
          (lambda ()
            (setq gc-cons-threshold (* 256 1024 1024)
                  gc-cons-percentage 0.1)
            (message "Garbage collection thresholds reset after init.")))

(use-package delight :ensure t)
(require 'delight)

(delight 'eldoc-mode nil "eldoc")
(delight 'emacs-lisp-mode "Elisp" :major)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode -1)
(global-display-line-numbers-mode +1)
(global-auto-revert-mode +1)
(save-place-mode +1)
(savehist-mode +1)
(recentf-mode +1)
(display-time-mode +1)
(delete-selection-mode +1)
(winner-mode +1)
(window-divider-mode +1)

(mapc #'disable-theme custom-enabled-themes)
(load-theme 'modus-vivendi t)

(let ((mono-spaced-font "IosevkaNerdFontMono")
      (proportionately-spaced-font "IosevkaNerdFontMono"))
  (set-face-attribute 'default nil :family mono-spaced-font :height 140)
  (set-face-attribute 'fixed-pitch nil :family mono-spaced-font :height 1.0)
  (set-face-attribute 'variable-pitch nil :family proportionately-spaced-font :height 1.0))

(if (boundp 'use-short-answers)
    (setq use-short-answers t)
  (advice-add 'yes-or-no-p :override #'y-or-n-p))

(setq-default fill-column 88)
(global-display-fill-column-indicator-mode 1)

(custom-set-faces
 '(fill-column-indicator-face ((t (:foreground "gray" :background nil)))))

(setq-default truncate-lines t)
(setq-default display-line-numbers-width 5)

(setq tab-width 4)
(setq inhibit-splash-screen 1)
(setq read-process-output-max (* 4 1024 1024))
(setq custom-file (expand-file-name "custom.el" cr-user-directory))
(setq scroll-margin 0)
(setq hscroll-margin 24)
(setq scroll-preserve-screen-position 1)
(setq native-comp-async-query-on-exit t)
(setq package-install-upgrade-built-in t)
(setq completion-ignore-case t)
(setq tab-always-indent 'complete)

(setq whitespace-style '(face tabs empty trailing))
(global-whitespace-mode +1)
(add-hook 'before-save-hook #'whitespace-cleanup)
(delight 'whitespace-mode nil "whitespace")

(unless (and
	 (eq window-system 'mac)
	 (bound-and-true-p mac-carbon-version-string))
  (setq pixel-scroll-precision-use-momentum nil)
  (pixel-scroll-precision-mode 1))

(add-to-list 'initial-frame-alist
	     '(fullscreen . maximized))

(add-to-list 'display-buffer-alist
	     '("\\`\\*\\(Warnings\\|Compile-Log\\)\\*\\'"
	       (display-buffer-no-window)
	       (allow-no-window . t)))

(keymap-global-set "C-x ;" 'comment-or-uncomment-region)
(keymap-global-set "M-n" 'forward-paragraph)
(keymap-global-set "M-p" 'backward-paragraph)
(keymap-global-set "<escape>" 'keyboard-escape-quit)
(keymap-global-set "C-d" 'delete-backward-char)
(keymap-global-set "C-S-d" 'backward-kill-word)

(keymap-global-set
 "C-x 2"
 (lambda ()
   (interactive)
   (split-window-vertically)
   (other-window 1)))

(keymap-global-set
 "C-x 3"
 (lambda ()
   (interactive)
   (split-window-horizontally)
   (other-window 1)))

(keymap-global-set
 "M-f"
 (lambda ()
   (interactive)
   (let ((start-syntax (if (eobp) nil (char-syntax (char-after)))))
     (if start-syntax
         (progn (forward-char 1)
		(while (and (not (eobp)) (eq (char-syntax (char-after)) start-syntax))
		  (forward-char 1)))
       (forward-char 1)))))

(keymap-global-set
 "M-b"
 (lambda ()
   (interactive)
   (let ((start-syntax (if (bobp) nil (char-syntax (char-before)))))
     (if start-syntax
         (progn (backward-char 1)
		(while (and (not (bobp)) (eq (char-syntax (char-before)) start-syntax))
		  (backward-char 1)))
       (backward-char 1)))))

(keymap-global-set
 "C-v"
 (lambda ()
   (interactive)
   (scroll-up (/ (window-height) 2))))

(keymap-global-set
 "M-v"
 (lambda ()
   (interactive)
   (scroll-down (/ (window-height) 2))))

(keymap-global-set
 "C-g"
 (lambda ()
   (interactive)
   (cond ((region-active-p) (keyboard-quit))
	 ((derived-mode-p 'completion-list-mode) (delete-completion-window))
	 ((> (minibuffer-depth) 0) (abort-recursive-edit))
	 (t (keyboard-quit)))))

(require 'ansi-color)
(add-hook
 'compilation-filter-hook
 (lambda ()
   (ansi-color-apply-on-region
    compilation-filter-start (point-max))))

(setq treesit-language-source-alist
      '((bash . ("https://github.com/tree-sitter/tree-sitter-bash"))
        (dockerfile . ("https://github.com/camdencheek/tree-sitter-dockerfile"))
        (html . ("https://github.com/tree-sitter/tree-sitter-html"))
        (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript"))
        (json . ("https://github.com/tree-sitter/tree-sitter-json"))
        (markdown . ("https://github.com/tree-sitter-grammars/tree-sitter-markdown" nil "tree-sitter-markdown/src"))
        (markdown-inline . ("https://github.com/tree-sitter-grammars/tree-sitter-markdown" nil "tree-sitter-markdown-inline/src"))
        (python . ("https://github.com/tree-sitter/tree-sitter-python"))
        (toml . ("https://github.com/ikatyang/tree-sitter-toml"))
        (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" nil "tsx/src"))
        (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" nil "typescript/src"))
        (yaml . ("https://github.com/ikatyang/tree-sitter-yaml"))))

(add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))
(add-to-list 'auto-mode-alist '("\\.ya?ml\\'" . yaml-ts-mode))
(add-to-list 'auto-mode-alist '("\\.jsonc?\\'" . json-ts-mode))
(add-to-list 'auto-mode-alist '("\\.m?jsx?\\'" . js-ts-mode))
(add-to-list 'auto-mode-alist '("\\.tsx?\\'" . js-ts-mode))

(defun cr/treesit-install-all-languages ()
  "Install all languages specified by `treesit-language-source-alist'."
  (interactive)
  (let ((languages (mapcar 'car treesit-language-source-alist)))
    (dolist (lang languages)
      (treesit-install-language-grammar lang)
      (message "`%s' parser was installed." lang)
      (sit-for 0.75))))

(use-package eglot
  :ensure nil
  :hook (

	 ;; Python
	 (python-ts-mode . (lambda ()
			     (eglot-ensure)))

	 )
  :config
  (setq eglot-events-buffer-config '(:size 0 :format lisp)
	eglot-ignored-server-capabilities '( :signatureHelpProvider
					     :documentHighlightProvider
					     :codeLensProvider
					     :documentRangeFormattingProvider
					     :documentOnTypeFormattingProvider
					     :documentLinkProvider
					     :foldingRangeProvider
					     :inlayHintProvider)
	eglot-server-programs '( (python-ts-mode . ("pyright-langserver" "--stdio"))))
  (setq-default eglot-workspace-configuration '( :pyright ( :disableOrganizeImports t)
						 :python.analysis ( :autoSearchPaths t
								    :useLibraryCodeForTypes t
								    :diagnosticMode "openFilesOnly"))))

(use-package lsp-mode
  :ensure t
  :init
  (defun my/lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults)) '(flex)))
  :hook ((lsp-completion-mode . my/lsp-mode-setup-completion)

	 ;; ;; Python
	 ;; (python-ts-mode . (lambda ()
         ;;                     (require 'lsp-pyright)
         ;;                     (lsp-deferred)))

	 )
  :config
  (setq lsp-keymap-prefix "C-l")
  (setq lsp-enable-file-watchers nil)
  (setq lsp-completion-provider :none)
  (setq lsp-enable-symbol-highlighting nil)
  (setq lsp-ui-doc-enable nil)
  (setq lsp-lens-enable nil)
  (setq lsp-headerline-breadcrumb-enable nil)
  (setq lsp-ui-sideline-enable nil)
  (setq lsp-modeline-code-actions-enable nil)
  (setq lsp-eldoc-enable-hover nil)
  (setq lsp-modeline-diagnostics-enable nil)
  (setq lsp-signature-auto-activate nil)
  (setq lsp-signature-render-documentation nil)
  (setq lsp-completion-provider :none))

(use-package lsp-ui
  :ensure t
  :hook (lsp-mode . lsp-ui-mode))

(use-package lsp-pyright
  :ensure t
  :config
  (setq lsp-pyright-langserver-command "pyright"))

(use-package flycheck
  :ensure t
  :hook (after-init . global-flycheck-mode)
  :config
  (define-key flycheck-mode-map flycheck-keymap-prefix nil)
  (setq flycheck-keymap-prefix (kbd "C-c c"))
  (define-key flycheck-mode-map flycheck-keymap-prefix flycheck-command-map))

(use-package flycheck-eglot
  :ensure t
  :config
  (global-flycheck-eglot-mode 1))

(use-package pyvenv
  :ensure t
  :init
  (defun cr/venv ()
    "Scan upwards from current directory for .venv/."
    (interactive)
    (let ((dir (expand-file-name default-directory))
          (venv-dir nil)
          (stopped-dir nil))
      (while (and dir (not (string= dir "/")) (not venv-dir))
	(let ((candidate (expand-file-name ".venv" dir)))
          (if (file-directory-p candidate)
	      (setq venv-dir candidate)
            (if (or (file-exists-p (expand-file-name "pyproject.toml" dir))
                    (file-directory-p (expand-file-name ".git" dir)))
		(progn (setq stopped-dir dir) (setq dir nil))
	      (setq dir (file-name-directory (directory-file-name dir)))))))
      (if venv-dir
          (progn
            (message "Venv found at %s, activating..." venv-dir)
            (pyvenv-activate venv-dir))
        (message "No venv found. Search started from %s and stopped at %s"
                 default-directory
                 (or stopped-dir "/"))))))

(use-package beacon
  :ensure t
  :delight
  :hook (after-init . beacon-mode))

(use-package spacious-padding
  :ensure t
  :hook (after-init . spacious-padding-mode)
  :config
  (setq spacious-padding-subtle-frame-lines t)
  :bind ([f8] . spacious-padding-mode))

(use-package apheleia
  :ensure t
  :delight
  :hook ((emacs-lisp-mode . apheleia-mode)
	 (python-ts-mode . apheleia-mode))
  :config
  (setf (alist-get 'python-ts-mode apheleia-mode-alist) '(ruff-isort ruff)))

(use-package eat
  :ensure t)

(use-package zop-to-char
  :ensure t
  :bind
  (("M-z" . zop-up-to-char)
   ("M-Z" . zop-to-char)))

(use-package ace-window
  :ensure t
  :config (setq aw-keys '(?h ?j ?k ?l))
  :bind (([remap other-window] . ace-window)
	 ("M-o" . ace-window)))

(use-package avy
  :ensure t
  :config (setq avy-background t)
  :bind ("M-e" . avy-goto-char-timer))

(use-package multiple-cursors
  :ensure t
  :bind (("C->" . mc/mark-next-like-this)
	 ("C-<" . mc/mark-previous-like-this)))

(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

(use-package marginalia
  :ensure t
  :hook (after-init . marginalia-mode))

(use-package vertico
  :ensure t
  :hook (after-init . vertico-mode)
  :config
  (vertico-multiform-mode)
  (add-to-list 'vertico-multiform-categories '(embark-keybinding grid))
  (setq vertico-cycle t)
  :bind ( :map vertico-map
	  ("<backspace>" . vertico-directory-delete-char)
	  ("C-w" . vertico-directory-delete-word)
	  ("RET" . vertico-directory-enter)))

(use-package orderless
  :ensure t
  :init
  (setq completion-styles '(orderless basic))
  (setq completion-category-defaults nil)
  (setq completion-category-overrides '( (file (styles partial-completion))
					 (embark-keybinding (styles flex))))
  (setq completion-pcm-leading-wildcard t))

(use-package corfu
  :ensure t
  :hook (after-init . global-corfu-mode)
  :config
  (setq corfu-cycle t))

(use-package projectile
  :ensure t
  :delight
  :hook (after-init . projectile-mode)
  :config
  (setq projectile-project-search-path '( "~/Developer/work/"
					  "~/Developer/personal/"
					  "~/.emacs.d/"))
  (setq projectile-cleanup-known-projects t)
  :bind-keymap ("C-x p" . projectile-command-map)
  :bind ( :map projectile-mode-map
	  ("C-c j" . projectile-command-map)))

(use-package consult-projectile
  :ensure t
  :after (consult projectile))

(use-package consult
  :ensure t
  :config
  (setq consult-async-min-input 2)
  (setq consult-narrow-key "<")
  (setq consult-fd-args "fd --type f --hidden --follow --exclude .git")
  :bind (("C-c f f" . consult-projectile)
	 ("C-c f o" . consult-outline)
	 ("C-c f k" . consult-flymake)
	 ("C-c f l" . consult-line)
	 ("C-c f ." . consult-goto-line)))

(use-package marginalia
  :ensure t
  :config
  (marginalia-mode))

(use-package embark
  :ensure t
  :demand t
  :bind
  (("C-." . embark-act)
   ("C-;" . embark-dwim)
   ("C-h B" . embark-bindings))
  :config
  (setq prefix-help-command #'embark-prefix-help-command)
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :ensure t
  :after (embark consult)
  :hook (embark-collect-mode . consult-preview-at-point-mode))

(use-package rg
  :ensure t
  :config
  (rg-enable-menu))

(use-package jinx
  :ensure t
  :bind (("M-$" . jinx-correct)
	 ("C-M-$" . jinx-languages)))

(use-package exec-path-from-shell
  :ensure t
  :demand t
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

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
  :bind (([remap describe-command] . helpful-command)
	 ([remap describe-function] . helpful-callable)
	 ([remap describe-key] . helpful-key)
	 ([remap describe-symbol] . helpful-symbol)
	 ([remap describe-variable] . helpful-variable)))

(use-package crux
  :ensure t
  :config
  (crux-with-region-or-line comment-or-uncomment-region)
  (crux-with-region-or-sexp-or-line kill-region)
  (crux-with-region-or-point-to-eol kill-ring-save)
  :bind (([remap move-beginning-of-line] . crux-move-beginning-of-line)
	 ([remap kill-whole-line] . crux-kill-whole-line)
	 ([remap keyboard-quit]  . crux-keyboard-quit-dwin)
	 ([remap upcase-region] . crux-upcase-region)
	 ([remap downcase-region] . crux-downcase-region)
	 ("s-j" . crux-top-join-line)
	 ("s-l" . crux-duplicate-current-line-or-region)
	 ("C-k" . crux-smart-kill-line)
	 ("s-n" . crux-cleanup-buffer-or-region)
	 ("s-m" . crux-smart-open-line)
	 ("s-M-m" . crux-smart-open-line-above)
	 ("s-o" . crux-open-with)
	 ("s-u" . crux-view-url)
	 ("s-M-k" . crux-kill-other-buffers)))

(use-package mise
  :ensure t
  :hook (after-init . global-mise-mode))

(use-package magit
  :ensure t)

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; End:

;;; init.el ends here
