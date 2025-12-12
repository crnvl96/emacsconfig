;;; init.el --- Init -*- lexical-binding: t; -*-

;;; Commentary:

;; My personal Emacs configuration.

;;; Code:

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

(setq tab-width 4)
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(setq scroll-margin 0
      scroll-preserve-screen-position 1)

(add-to-list 'initial-frame-alist
	     '(fullscreen . maximized))
(add-to-list 'display-buffer-alist
	     '("\\`\\*\\(Warnings\\|Compile-Log\\)\\*\\'" (display-buffer-no-window) (allow-no-window . t)))

(fset 'yes-or-no-p 'y-or-n-p)

(setq-default display-line-numbers-type 'relative)
(setq native-comp-async-query-on-exit t)
(setq package-install-upgrade-built-in t)
(setq completion-ignore-case t)
(setq tab-always-indent 'complete)
(setq whitespace-style '(face tabs empty trailing))

(add-hook 'after-init-hook #'global-display-line-numbers-mode)
(add-hook 'after-init-hook #'global-auto-revert-mode)
(add-hook 'after-init-hook #'global-whitespace-mode)
(add-hook 'before-save-hook #'whitespace-cleanup)
(add-hook 'after-init-hook #'save-place-mode)
(add-hook 'after-init-hook #'savehist-mode)
(add-hook 'after-init-hook #'recentf-mode)
(add-hook 'after-init-hook #'display-time-mode)
(add-hook 'after-init-hook #'delete-selection-mode)
(add-hook 'after-init-hook #'winner-mode)
(add-hook 'after-init-hook #'global-hl-line-mode)
(add-hook 'after-init-hook #'which-key-mode)

(require 'ansi-color)
(defun my-compilation-filter-hook ()
  "Allow rendering ansi symbols and colors."
  (ansi-color-apply-on-region compilation-filter-start (point-max)))
(add-hook 'compilation-filter-hook #'my-compilation-filter-hook)

;;; Keymaps

(defun my-split-window-vertically ()
  "Keep the cursor on the current window when splitting it vertically."
  (interactive)
  (split-window-vertically) (other-window 1))

(defun my-split-window-horizontally ()
  "Keep the cursor on the current window when splitting it horizontally."
  (interactive)
  (split-window-horizontally) (other-window 1))

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

(defun my-scroll-up-half ()
  "Scroll up half screen and center the cursor."
  (interactive)
  (scroll-up (/ (window-height) 2))
  (recenter))

(defun my-scroll-down-half ()
  "Scroll down half screen and center the cursor."
  (interactive)
  (scroll-down (/ (window-height) 2))
  (recenter))

(global-set-key (kbd "C-x ;") 'comment-or-uncomment-region)
(global-set-key (kbd "M-n") 'forward-paragraph)
(global-set-key (kbd "M-p") 'backward-paragraph)
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(global-set-key (kbd "C-x 2") #'my-split-window-vertically)
(global-set-key (kbd "C-x 3") #'my-split-window-horizontally)
(global-set-key (kbd "M-f") #'my-forward-word)
(global-set-key (kbd "M-b") #'my-backward-word)
(global-set-key (kbd "C-v") #'my-scroll-up-half)
(global-set-key (kbd "M-v") #'my-scroll-down-half)

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

;;; Eglot
(use-package eglot
  :ensure nil
  :hook
  (python-ts-mode . eglot-ensure)
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
	eglot-server-programs '( (python-ts-mode . ("~/.local/bin/pyright-langserver" "--stdio"))))
  (setq-default eglot-workspace-configuration '( :pyright ( :disableOrganizeImports t)
						 :python.analysis ( :autoSearchPaths t
								    :useLibraryCodeForTypes t
								    :diagnosticMode "openFilesOnly"))))

;;; Apheleia
(use-package apheleia
  :ensure t
  :hook (after-init . apheleia-global-mode)
  :config
  (setf (alist-get 'python-ts-mode apheleia-mode-alist) '(ruff-isort ruff))
  :bind (("C-c c f" . apheleia-format-buffer)))

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

;;; Eat
(use-package eat
  :ensure t
  :commands (eat))

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
          ("C-w" . vertico-directory-delete-word )
          ("RET" . vertico-directory-enter)))

;;; Orderless
(use-package orderless
  :ensure t
  :config
  (setq completion-styles '(orderless basic))
  (setq completion-category-overrides '((file (styles partial-completion)))))

;;; Corfu
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

;;; Consult
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

;;; Rg
(use-package rg
  :ensure t
  :config (rg-enable-menu))

;;; Jinx - spell checker
(use-package jinx
  :ensure t
  :bind (("M-$" . jinx-correct)
	 ("C-M-$" . jinx-languages)))

;;; Pyvenv
(use-package pyvenv
  :ensure t
  :commands (pyvenv-activate))

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
  (define-key flycheck-mode-map flycheck-keymap-prefix nil)
  (setq flycheck-keymap-prefix (kbd "C-c c"))
  (define-key flycheck-mode-map flycheck-keymap-prefix flycheck-command-map))

;;; Flycheck-Eglot
(use-package flycheck-eglot
  :ensure t
  :after (flycheck eglot)
  :config
  (global-flycheck-eglot-mode 1))

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
