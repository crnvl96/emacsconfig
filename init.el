;;; init.el --- Init -*- lexical-binding: t; -*-

;;; Garbage Collection
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)
(add-hook 'after-init-hook
          (lambda ()
            (setq gc-cons-threshold (* 64 1024 1024)  ; 64 MB
                  gc-cons-percentage 0.1)
            (message "Garbage collection thresholds reset after init.")))

;;; Load Path
(add-to-list 'load-path (expand-file-name "rc" (file-name-parent-directory user-emacs-directory)))
(let ((default-directory (expand-file-name "rc" (file-name-parent-directory user-emacs-directory))))
  (normal-top-level-add-subdirs-to-load-path))

;;; Options
(require 'rc-opts)

;;; Keymaps
(require 'rc-keymaps)

;;; Treesit
(require 'rc-treesit)

;;; Eglot & Formatters
(require 'rc-lsp-fmt)

;;; Lang modes
(require 'rc-modes)

;;; Delight
(use-package delight
  :ensure t
  :demand t
  :config
  (delight 'whitespace-mode nil "whitespace")
  (delight 'which-key-mode nil "which-key")
  (delight 'visual-line-mode nil "simple")
  (delight 'eldoc-mode nil "eldoc"))

;;; Move text
(use-package move-text
  :ensure t
  :bind (("C-S-p" . move-text-up)
	 ("C-S-n" . move-text-down)))

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

;;; Volatile highlights
(use-package volatile-highlights
  :ensure t
  :hook (after-init . volatile-highlights-mode))

;;; Zop
(use-package zop-to-char
  :ensure t
  :bind (("M-z" . zop-up-to-char)
         ("M-Z" . zop-to-char)))

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

;;; Cape
(use-package cape
  :ensure t
  :demand t
  :bind ("C-c p" . cape-prefix-map)
  :init
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block))

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
  (setq corfu-cycle t)
  (setq corfu-preselect 'prompt)
  (add-hook 'eshell-mode-hook (lambda ()
				(setq-local corfu-auto nil)
				(corfu-mode)))
  ;; When pressing RET while the Corfu popup is visible, the corfu-insert
  ;; command will be invoked. This command does inserts the currently selected
  ;; candidate, but it does not send the prompt input to Eshell or the
  ;; Comint process. Therefore you often have to press RET twice which feels
  ;; like an unnecessary double confirmation. Fortunately it is easy to
  ;; improve this by using the command corfu-send instead.
  (keymap-set corfu-map "RET" #'corfu-send)
  :bind ( :map corfu-map
	  ("RET" . nil)
	  ([ret] . nil)
          ("TAB" . corfu-next)
          ([tab] . corfu-next)
          ("S-TAB" . corfu-previous)
          ([backtab] . corfu-previous)))

;;; Consult
(use-package consult
  :ensure t
  :config
  (setq consult-async-min-input 2
        consult-narrow-key "<")
  ;; (consult-customize
  ;;  consult-buffer  consult-yank-pop consult-fd consult-outline
  ;;  consult-imenu consult-info consult-flymake consult-history
  ;;  consult-focus-lines consult-line consult-ripgrep consult-goto-line
  ;;  :preview-key nil)
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
  :defer t)

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
