;;; init.el --- Init -*- lexical-binding: t -*-

;;; Commentary:

;; My personal Emacs configuration.

;;; Code:

;;; Options

(menu-bar-mode -1) ; Disable the menu bar
(tool-bar-mode -1) ; Disable the tool bar
(scroll-bar-mode -1) ; Disable the scroll bar
(blink-cursor-mode -1) ; Disable the cursor blinking

;; Load and activate Emacs Lisp packages
(require 'package)
(package-initialize)
;; If we're on emacs >= 29, skip
(when (< emacs-major-version 29)
  ;; If `use-package' is already installed, skip
  (unless (package-installed-p 'use-package)
    ;; If `package-archive-contents' exists, skip
    (unless (seq-empty-p package-archive-contents)
      ;; Download descriptions of ELPA packages
      (package-refresh-contents))
    ;; Install `use-package'
    (package-install 'use-package)))
(require 'use-package)
;; Set package sources and priorities
(setq package-archives '( ("melpa"        . "https://melpa.org/packages/")
                          ("gnu"          . "https://elpa.gnu.org/packages/")
                          ("nongnu"       . "https://elpa.nongnu.org/nongnu/")
                          ("melpa-stable" . "https://stable.melpa.org/packages/"))
      package-archive-priorities '( ("melpa"        . 90)
                                    ("gnu"          . 70)
                                    ("nongnu"       . 60)
                                    ("melpa-stable" . 50)))

;; Temporarily increase the limits of garbage colecction trigger
;; to increase Emacs performance on startup
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)
;; When Emacs finishes loading, set it back
(add-hook 'after-init-hook
          (lambda ()
            (setq gc-cons-threshold (* 256 1024 1024) ; 256 MB
                  gc-cons-percentage 0.1)
            (message "Garbage collection thresholds reset after init.")))

;; Disable any custom theme that might be loaded
(mapc #'disable-theme custom-enabled-themes)
;; Load out preferred theme
(load-theme 'modus-vivendi t)
;; Set default font to be used
(let ((mono-spaced-font "Iosevka")
      (proportionately-spaced-font "Iosevka Aile"))
  (set-face-attribute 'default nil :family mono-spaced-font :height 200)
  (set-face-attribute 'fixed-pitch nil :family mono-spaced-font :height 1.0)
  (set-face-attribute 'variable-pitch nil :family proportionately-spaced-font :height 1.0))

;; If use short answers ('y' or 'n') to answer prompts
(if (boundp 'use-short-answers)
    (setq use-short-answers t)
  ;; Adapt the prompt shown on the modeline to reflect that
  (advice-add 'yes-or-no-p :override #'y-or-n-p))

(setq-default truncate-lines t)  ; Don't split lines

(setq tab-width 4 ; How many spaces count as a <Tab>
      inhibit-splash-screen 1 ; don't show the startup screen
      read-process-output-max (* 4 1024 1024) ;; 4mb
      custom-file (expand-file-name "custom.el" cr-user-directory) ; Avoid polluting our init file with custom settings
      scroll-margin 0 ; Leave a Gap between cursor and window boundaries (vert.)
      hscroll-margin 24 ; Leave a Gap between cursor and window boundaries (vert.)
      scroll-preserve-screen-position 1 ; I don't know why, but this is needed to our
					; custom scroll keybindings (M-v and C-v) to work properly
      native-comp-async-query-on-exit t ; Prompt the user to confirm quitting Emacs if
					; there are compilation processes running
      package-install-upgrade-built-in t ; Allow upgrading builtin packages as well
      completion-ignore-case t ; Make completion algorithm to be case insensitive
      tab-always-indent 'complete ; Make <Tab> assume both behaviors of indenting and completing
      whitespace-style '(face tabs empty trailing)) ; Configure the kinds of whitespace we want to highlight

(unless (and (eq window-system 'mac)
             (bound-and-true-p mac-carbon-version-string))
  (setq pixel-scroll-precision-use-momentum nil)
  (pixel-scroll-precision-mode 1))

(add-to-list 'initial-frame-alist
	     '(fullscreen . maximized))

(add-to-list 'display-buffer-alist
	     '("\\`\\*\\(Warnings\\|Compile-Log\\)\\*\\'"
	       (display-buffer-no-window)
	       (allow-no-window . t)))

;;; Functions

(require 'ansi-color)
(defun cr/compilation-filter-hook ()
  "Allow rendering ansi symbols and colors."
  (ansi-color-apply-on-region compilation-filter-start (point-max)))

(defun cr/split-window-vertically ()
  "Keep the cursor on the current window when splitting it vertically."
  (interactive)
  (split-window-vertically) (other-window 1))

(defun cr/split-window-horizontally ()
  "Keep the cursor on the current window when splitting it horizontally."
  (interactive)
  (split-window-horizontally) (other-window 1))

(defun cr/forward-word ()
  "Move forward to the next syntax change, like Vim word movement."
  (interactive)
  (let ((start-syntax (if (eobp) nil (char-syntax (char-after)))))
    (if start-syntax
        (progn
          (forward-char 1)
          (while (and (not (eobp)) (eq (char-syntax (char-after)) start-syntax))
            (forward-char 1)))
      (forward-char 1))))

(defun cr/backward-word ()
  "Move backward to the previous syntax change, like Vim word movement."
  (interactive)
  (let ((start-syntax (if (bobp) nil (char-syntax (char-before)))))
    (if start-syntax
        (progn
          (backward-char 1)
          (while (and (not (bobp)) (eq (char-syntax (char-before)) start-syntax))
            (backward-char 1)))
      (backward-char 1))))

(defun cr/scroll-up-half ()
  "Scroll up half screen and center the cursor."
  (interactive)
  (scroll-up (/ (window-height) 2))
  (recenter))

(defun cr/scroll-down-half ()
  "Scroll down half screen and center the cursor."
  (interactive)
  (scroll-down (/ (window-height) 2))
  (recenter))

(defun cr/keyboard-quit-dwim ()
  "Do-What-I-Mean behaviour for a general `keyboard-quit'.

The generic `keyboard-quit' does not do the expected thing when
the minibuffer is open.  Whereas we want it to close the
minibuffer, even without explicitly focusing it.

The DWIM behaviour of this command is as follows:

- When the region is active, disable it.
- When a minibuffer is open, but not focused, close the minibuffer.
- When the Completions buffer is selected, close it.
- In every other case use the regular `keyboard-quit'."
  (interactive)
  (cond
   ((region-active-p)
    (keyboard-quit))
   ((derived-mode-p 'completion-list-mode)
    (delete-completion-window))
   ((> (minibuffer-depth) 0)
    (abort-recursive-edit))
   (t
    (keyboard-quit))))

(defun cr/treesit-install-all-languages ()
  "Install all languages specified by `treesit-language-source-alist'."
  (interactive)
  (let ((languages (mapcar 'car treesit-language-source-alist)))
    (dolist (lang languages)
      (treesit-install-language-grammar lang)
      (message "`%s' parser was installed." lang)
      (sit-for 0.75))))

(defun cr/venv ()
  "Scan upwards from current directory for .venv/, pyproject.toml, or .git/.
If .venv/ is found, activate it.
If pyproject.toml or .git/ is found first, do nothing."
  (interactive)
  (let ((dir (expand-file-name default-directory))
        (found nil))
    (while (and dir (not (string= dir "/")) (not found))
      (cond ((file-directory-p (expand-file-name ".venv" dir)) (pyvenv-activate (expand-file-name ".venv" dir))
	     (setq found t))
            ((or (file-exists-p (expand-file-name "pyproject.toml" dir)) (file-directory-p (expand-file-name ".git" dir)))
	     (setq found t))
	    (t
	     (setq dir (file-name-directory (directory-file-name dir))))))))

;;; Hooks

(add-hook 'after-init-hook #'global-display-line-numbers-mode)
(add-hook 'after-init-hook #'global-auto-revert-mode)
(add-hook 'after-init-hook #'global-whitespace-mode)
(add-hook 'after-init-hook #'save-place-mode)
(add-hook 'after-init-hook #'savehist-mode)
(add-hook 'after-init-hook #'recentf-mode)
(add-hook 'after-init-hook #'display-time-mode)
(add-hook 'after-init-hook #'delete-selection-mode)
(add-hook 'after-init-hook #'winner-mode)
(add-hook 'after-init-hook #'global-hl-line-mode)
(add-hook 'after-init-hook #'which-key-mode)
(add-hook 'before-save-hook #'whitespace-cleanup)
(add-hook 'after-init-hook #'window-divider-mode)
(add-hook 'compilation-filter-hook #'cr/compilation-filter-hook)

;;; Delight

(use-package delight :ensure t)
(require 'delight)
(delight 'whitespace-mode nil "whitespace")
(delight 'which-key-mode nil "which-key")
(delight 'eldoc-mode nil "eldoc")
(delight 'emacs-lisp-mode "Elisp" :major)

;;; Keymaps

(global-set-key (kbd "C-x ;") 'comment-or-uncomment-region)
(global-set-key (kbd "C-j") 'next-line)
(global-set-key (kbd "C-k") 'previous-line)
(global-set-key (kbd "M-j") 'forward-paragraph)
(global-set-key (kbd "M-k") 'backward-paragraph)
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(global-set-key (kbd "C-x 2") #'cr/split-window-vertically)
(global-set-key (kbd "C-x 3") #'cr/split-window-horizontally)
(global-set-key (kbd "M-f") #'cr/forward-word)
(global-set-key (kbd "M-b") #'cr/backward-word)
(global-set-key (kbd "C-v") #'cr/scroll-up-half)
(global-set-key (kbd "M-v") #'cr/scroll-down-half)
(global-set-key (kbd "C-g") #'cr/keyboard-quit-dwim)

;;; Treesit

(setq treesit-language-source-alist
      '(  ; use `sort-lines' to sort
        (bash . ("https://github.com/tree-sitter/tree-sitter-bash"))
        (c . ("https://github.com/tree-sitter/tree-sitter-c"))
        (html . ("https://github.com/tree-sitter/tree-sitter-html"))
        (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript"))
        (jsdoc . ("https://github.com/tree-sitter/tree-sitter-jsdoc"))
        (json . ("https://github.com/tree-sitter/tree-sitter-json"))
        (python . ("https://github.com/tree-sitter/tree-sitter-python"))
        (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" nil "tsx/src"))
        (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" nil "typescript/src"))
        (yaml . ("https://github.com/ikatyang/tree-sitter-yaml"))
        (toml . ("https://github.com/ikatyang/tree-sitter-toml"))
        (dockerfile . ("https://github.com/camdencheek/tree-sitter-dockerfile"))
        (markdown . ("https://github.com/tree-sitter-grammars/tree-sitter-markdown" nil "tree-sitter-markdown/src"))
        (markdown-inline . ("https://github.com/tree-sitter-grammars/tree-sitter-markdown" nil "tree-sitter-markdown-inline/src"))
        ))

(add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))
(add-to-list 'auto-mode-alist '("\\.ya?ml\\'" . yaml-ts-mode))
(add-to-list 'auto-mode-alist '("\\.jsonc?\\'" . json-ts-mode))
(add-to-list 'auto-mode-alist '("\\.m?jsx?\\'" . js-ts-mode))
(add-to-list 'auto-mode-alist '("\\.tsx?\\'" . js-ts-mode))

;;; Eglot

;; (add-hook 'python-ts-mode-hook #'eglot-ensure)

;; (setq eglot-events-buffer-config '(:size 0 :format lisp)
;;       eglot-ignored-server-capabilities '( :signatureHelpProvider
;; 					   :documentHighlightProvider
;; 					   :codeLensProvider
;; 					   :documentRangeFormattingProvider
;; 					   :documentOnTypeFormattingProvider
;; 					   :documentLinkProvider
;; 					   :foldingRangeProvider
;; 					   :inlayHintProvider)
;;       eglot-server-programs '( (python-ts-mode . ("~/.local/bin/pyright-langserver" "--stdio"))))

;; (setq-default eglot-workspace-configuration '( :pyright ( :disableOrganizeImports t)
;; 					       :python.analysis ( :autoSearchPaths t
;; 								  :useLibraryCodeForTypes t
;; 								  :diagnosticMode "openFilesOnly")))

;;; Packages

(use-package beacon
  :ensure t
  :delight
  :hook (after-init . beacon-mode))

(use-package spacious-padding
  :ensure t
  :hook (after-init . spacious-padding-mode)
  :config
  (setq spacious-padding-widths
        '( :internal-border-width 15
           :header-line-width 4
           :mode-line-width 6
           :custom-button-width 3
           :tab-width 4
           :right-divider-width 30
           :scroll-bar-width 8
           :fringe-width 8))
  (setq spacious-padding-subtle-frame-lines t)
  :bind (([f8] . spacious-padding-mode)))

(use-package apheleia
  :ensure t
  :delight
  :hook (after-init . apheleia-global-mode)
  :config (setf (alist-get 'python-ts-mode apheleia-mode-alist) '(ruff-isort ruff)))

(use-package eat
  :ensure t)

(use-package zop-to-char
  :ensure t
  :bind (("M-z" . zop-up-to-char)
         ("M-Z" . zop-to-char)))

(use-package ace-window
  :ensure t
  :config
  (setq aw-keys '(?h ?j ?k ?l))
  :bind
  (("M-o" . ace-window)
   ([remap other-window] . ace-window)))

(use-package avy
  :ensure t
  :bind (("M-e" . avy-goto-char-timer))
  :config (setq avy-background t))

(use-package multiple-cursors
  :ensure t
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)))

(use-package expand-region
  :ensure t
  :bind (("C-=" . er/expand-region)))

(use-package marginalia
  :ensure t
  :hook (after-init . marginalia-mode))

(use-package vertico
  :ensure t
  :hook (after-init . vertico-mode)
  :config
  (setq vertico-cycle t)
  :bind ( :map vertico-map
          ("<backspace>" . vertico-directory-delete-char)
          ("C-w" . vertico-directory-delete-word)
          ("C-j" . vertico-next)
          ("C-k" . vertico-previous)
          ("RET" . vertico-directory-enter)))

(use-package orderless
  :ensure t
  :init
  (setq completion-styles '(orderless partial-completion basic)
        completion-category-defaults nil
        completion-category-overrides nil))

(use-package cape
  :ensure t
  :hook ((git-commit-mode . cr/cape-capf-setup-git-commit)
	 (emacs-lisp-mode . cr/cape-capf-setup-elisp))
  :init
  (defun cr/cape-capf-setup-git-commit ()
    (let ((result nil))
      (dolist (element '(cape-dabbrev cape-file) result)
	(add-to-list 'completion-at-point-functions element))))
  (defun cr/cape-capf-setup-elisp ()
    (let ((result nil))
      (dolist (element '(cape-elisp-symbol cape-dabbrev cape-file) result)
	(add-to-list 'completion-at-point-functions element))))
  :config
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent)
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-purify))

(use-package lsp-mode
  :ensure t
  :hook ((lsp-mode . lsp-enable-which-key-integration)
	 (lsp-completion-mode . cr/lsp-mode-setup-completion)
	 (python-ts-mode . (lambda () (require 'lsp-pyright) (lsp-deferred))))
  :init
  (defun cr/lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(flex)))
  (setq lsp-keymap-prefix "C-l"
	lsp-enable-symbol-highlighting nil
	lsp-ui-doc-enable nil
	lsp-lens-enable nil
	lsp-headerline-breadcrumb-enable nil
	lsp-ui-sideline-enable nil
	lsp-modeline-code-actions-enable nil
	lsp-ui-sideline-enable nil
	lsp-eldoc-enable-hover nil
	lsp-enable-indentation nil
        lsp-enable-on-type-formatting nil
        lsp-auto-execute-action nil
        lsp-before-save-edits nil
	lsp-modeline-diagnostics-enable t
	lsp-signature-auto-activate nil ;; you could manually request them via `lsp-signature-activate`
	lsp-signature-render-documentation nil
	lsp-completion-provider :none)) ;; we use Corfu

(use-package lsp-pyright
  :ensure t
  :config (setq lsp-pyright-langserver-command "pyright")) ;; or basedpyright

(use-package lsp-ui
  :ensure t
  :hook (lsp-mode . lsp-ui-mode))

(use-package corfu
  :ensure t
  :hook
  (after-init . global-corfu-mode)
  :config
  (setq corfu-cycle t)
  :bind ( :map corfu-map
	  ("C-j" . corfu-next)
          ("C-k" . corfu-previous)))

(use-package projectile
  :ensure t
  :hook (after-init . projectile-mode)
  :delight
  :config
  (setq projectile-project-search-path '("~/Developer/work/" "~/Developer/personal/" "~/.emacs.d/"))
  (setq projectile-cleanup-known-projects t)
  (add-hook 'project-find-functions #'project-projectile)
  :bind-keymap ("C-x p" . projectile-command-map)
  :bind ( :map projectile-mode-map
	  ("C-c j" . projectile-command-map)))

(use-package consult-projectile
  :ensure t
  :after (consult projectile))

(use-package embark
  :ensure t
  :bind
  (("C-." . embark-act)
   ("M-." . embark-dwim)
   ("C-h B" . embark-bindings))
  :config
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :ensure t
  :hook (embark-collect-mode . consult-preview-at-point-mode))

(use-package consult
  :ensure t
  :config
  (setq consult-async-min-input 2
        consult-narrow-key "<")
  (setq consult-fd-args "fd --type f --hidden --follow --exclude .git")
  :bind (("C-c f f" . consult-projectile)
         ("C-c f o" . consult-outline)
         ("C-c f k" . consult-flymake)
         ("C-c f l" . consult-line)
         ("C-c f ." . consult-goto-line)))

(use-package rg
  :ensure t
  :config
  (rg-enable-menu))

(use-package jinx
  :ensure t
  :bind (("M-$" . jinx-correct)
	 ("C-M-$" . jinx-languages)))

(use-package pyvenv
  :ensure t
  :commands (pyvenv-activate))

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
  :bind
  ([remap describe-command] . helpful-command)
  ([remap describe-function] . helpful-callable)
  ([remap describe-key] . helpful-key)
  ([remap describe-symbol] . helpful-symbol)
  ([remap describe-variable] . helpful-variable))

(use-package crux
  :ensure t
  :bind (([remap move-beginning-of-line] . crux-move-beginning-of-line)
         ([remap kill-whole-line] . crux-kill-whole-line)
         ([remap keyboard-quit]  . crux-keyboard-quit-dwin)
         ([remap upcase-region] . crux-upcase-region)
         ([remap downcase-region] . crux-downcase-region)
         ("s-j" . crux-top-join-line)
	 ("s-l" . crux-duplicate-current-line-or-region)
	 ("s-k" . crux-smart-kill-line)
         ("s-n" . crux-cleanup-buffer-or-region)
         ("s-m" . crux-smart-open-line)
         ("s-M-m" . crux-smart-open-line-above)
	 ("s-o" . crux-open-with)
         ("s-u" . crux-view-url)
         ("s-M-k" . crux-kill-other-buffers))
  :config
  (crux-with-region-or-line comment-or-uncomment-region)
  (crux-with-region-or-sexp-or-line kill-region)
  (crux-with-region-or-point-to-eol kill-ring-save))

(use-package flycheck
  :ensure t
  :hook (after-init . global-flycheck-mode)
  :config
  (define-key flycheck-mode-map flycheck-keymap-prefix nil)
  (setq flycheck-keymap-prefix (kbd "C-c c"))
  (define-key flycheck-mode-map flycheck-keymap-prefix flycheck-command-map))

(use-package mise
  :ensure t
  :hook (after-init . global-mise-mode))

(use-package magit
  :ensure t)

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; End:

;;; init.el ends here
