;;; init.el --- Init -*- lexical-binding: t -*-

;;; Commentary:

;; My personal Emacs configuration.

;;; Code:

;;; Imports
(require 'ansi-color)

;;; Options
(menu-bar-mode -1)
(tool-bar-mode -1)

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

(setq read-answer-short t)
(if (boundp 'use-short-answers)
    (setq use-short-answers t)
  (advice-add 'yes-or-no-p :override #'y-or-n-p))

(setq tab-width 4
      inhibit-splash-screen 1
      custom-file (expand-file-name "custom.el" cr-user-directory)
      scroll-margin 0
      scroll-preserve-screen-position 1
      native-comp-async-query-on-exit t
      package-install-upgrade-built-in t
      completion-ignore-case t
      tab-always-indent 'complete
      whitespace-style '(face tabs empty trailing))

(add-to-list 'initial-frame-alist
	     '(fullscreen . maximized))

(add-to-list 'display-buffer-alist
	     '("\\`\\*\\(Warnings\\|Compile-Log\\)\\*\\'"
	       (display-buffer-no-window) (allow-no-window . t)))

;;; Functions
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

(defun cr/treesit-install-all-languages ()
  "Install all languages specified by `treesit-language-source-alist'."
  (interactive)
  (let ((languages (mapcar 'car treesit-language-source-alist)))
    (dolist (lang languages)
      (treesit-install-language-grammar lang)
      (message "`%s' parser was installed." lang)
      (sit-for 0.75))))

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
(global-set-key (kbd "M-n") 'forward-paragraph)
(global-set-key (kbd "M-p") 'backward-paragraph)
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(global-set-key (kbd "C-x 2") #'cr/split-window-vertically)
(global-set-key (kbd "C-x 3") #'cr/split-window-horizontally)
(global-set-key (kbd "M-f") #'cr/forward-word)
(global-set-key (kbd "M-b") #'cr/backward-word)
(global-set-key (kbd "C-v") #'cr/scroll-up-half)
(global-set-key (kbd "M-v") #'cr/scroll-down-half)

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
(add-hook 'python-ts-mode-hook #'eglot-ensure)

(setq eglot-events-buffer-config '(:size 0 :format lisp)
      eglot-ignored-server-capabilities '( :signatureHelpProvider
					   :documentHighlightProvider
					   :codeLensProvider
					   :documentRangeFormattingProvider
					   :documentOnTypeFormattingProvider
					   :documentLinkProvider
					   :foldingRangeProvider
					   :inlayHintProvider)
      eglot-server-programs '( (python-ts-mode . ("~/.local/bin/pyright-langserver" "--stdio"))))

(setq-default eglot-workspace-configuration '( :pyright ( :disableOrganizeImports t)
					       :python.analysis ( :autoSearchPaths t
								  :useLibraryCodeForTypes t
								  :diagnosticMode "openFilesOnly")))

;;; Packages
(use-package apheleia
  :ensure t
  :delight (apheleia-mode " aph")
  :hook (after-init . apheleia-global-mode)
  :config (setf (alist-get 'python-ts-mode apheleia-mode-alist) '(ruff-isort ruff)))

(use-package eat
  :ensure t)

(use-package ace-window
  :ensure t
  :config
  (setq aw-keys '(?h ?j ?k ?l))
  :bind
  (("M-o" . ace-window)
   ([remap other-window] . ace-window)))

(use-package avy
  :ensure t
  :bind (("M-e" . avy-goto-char-timer)
	 ("M-l" . avy-goto-line))
  :config
  (setq avy-background t))

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
  :bind ( :map vertico-map
          ("<backspace>" . vertico-directory-delete-char)
          ("C-w" . vertico-directory-delete-word )
          ("RET" . vertico-directory-enter)))

(use-package orderless
  :ensure t
  :config
  (setq completion-styles '(orderless basic))
  (setq completion-category-overrides '((file (styles partial-completion))))
  (setq completion-category-defaults nil))

(use-package corfu
  :ensure t
  :hook
  (after-init . global-corfu-mode)
  :config
  (setq corfu-auto        t
	corfu-auto-delay  0.2
	corfu-auto-prefix 3)
  (add-hook 'corfu-mode-hook
            (lambda ()
	      (setq-local completion-styles '(basic)
                          completion-category-overrides nil
                          completion-category-defaults nil))))

(use-package consult
  :ensure t
  :config
  (setq consult-async-min-input 2
        consult-narrow-key "<")
  (setq consult-fd-args "fd --type f --hidden --follow --exclude .git")
  :bind (("C-c f b" . consult-buffer)
         ("C-c f f" . consult-fd)
         ("C-c f o" . consult-outline)
         ("C-c f k" . consult-flymake)
         ("C-c f l" . consult-line)
         ("C-c f g" . consult-ripgrep)
         ("C-c f L" . consult-goto-line)))

(use-package rg
  :ensure t
  :config (rg-enable-menu))

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

(use-package flycheck
  :ensure t
  :hook (after-init . global-flycheck-mode)
  :config
  (define-key flycheck-mode-map flycheck-keymap-prefix nil)
  (setq flycheck-keymap-prefix (kbd "C-c c"))
  (define-key flycheck-mode-map flycheck-keymap-prefix flycheck-command-map))

(use-package flycheck-eglot
  :ensure t
  :after (flycheck eglot)
  :config
  (global-flycheck-eglot-mode 1))

(use-package mise
  :ensure t
  :hook (after-init . global-mise-mode))

(use-package magit
  :ensure t)

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; End:

;;; init.el ends here
