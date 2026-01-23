;;; init.el --- Personal Emacs Configuration -*- lexical-binding: t -*-

(require 'package)
(require 'use-package)
(package-initialize)

(setq package-archives '(("melpa"        . "https://melpa.org/packages/")
                         ("gnu"          . "https://elpa.gnu.org/packages/")
                         ("nongnu"       . "https://elpa.nongnu.org/nongnu/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")))
(setq package-archive-priorities '(("melpa"        . 90)
                                   ("gnu"          . 70)
                                   ("nongnu"       . 60)
                                   ("melpa-stable" . 50)))
(setq custom-file (expand-file-name "custom.el" my-user-directory))
(setq use-short-answers t)
(setq confirm-kill-emacs 'y-or-n-p)
(setq enable-recursive-minibuffers t)
(setq make-backup-files nil)
(setq visible-bell nil)
(setq ring-bell-function #'ignore)
(setq-default truncate-lines t)
(setq-default display-line-numbers-width 3)
(setq indicate-buffer-boundaries 'left)
(setq x-underline-at-descent-line nil)
(setq pixel-scroll-precision-use-momentum nil)
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)
(setq tab-always-indent 'complete)
(setq completion-ignore-case t)
(add-hook 'after-init-hook
          (lambda ()
            (setq gc-cons-threshold (* 64 1024 1024))  ; 64MB
            (setq gc-cons-percentage 0.1)))

(global-hl-line-mode -1)
(global-display-line-numbers-mode 1)
(column-number-mode 1)
(save-place-mode 1)
(savehist-mode 1)
(recentf-mode 1)
(display-time-mode 1)
(delete-selection-mode 1)
(winner-mode 1)
(window-divider-mode 1)
(pixel-scroll-precision-mode 1)
(when (display-graphic-p)
  (context-menu-mode 1))

(let ((mono-font "HackNerdFontMono")
      (prop-font "HackNerdFontPropo"))
  (set-face-attribute 'default nil :family mono-font :height 110 :weight 'regular)
  (set-face-attribute 'fixed-pitch nil :family mono-font :height 1.0 :weight 'regular)
  (set-face-attribute 'variable-pitch nil :family prop-font :height 1.0 :weight 'regular))

;; We want emacs to always look at a default NodeJS installations to find
;; programs that were globally installed
(let ((global-nodejs-path (concat (getenv "HOME") "/.local/share/mise/installs/node/24/bin")))
  (setenv "PATH" (concat (getenv "PATH") ":" global-nodejs-path))
  (add-to-list 'exec-path global-nodejs-path))

(use-package emacs
  :ensure nil
  :hook (after-init . minibuffer-depth-indicate-mode)
  :config (add-to-list 'display-buffer-alist
                       '("\\`\\*\\(Warnings\\|Compile-Log\\)\\*\\'" (display-buffer-no-window) (allow-no-window . t))))

(use-package uniquify
  :ensure nil
  :config
  (setq uniquify-buffer-name-style 'reverse)
  (setq uniquify-separator "•")
  (setq uniquify-after-kill-buffer-p t))

(use-package autorevert
  :ensure nil
  :hook (after-init . global-auto-revert-mode)
  :config
  ;; When nil, buffers in Auto-Revert Mode will always be polled for
  ;; changes to their files on disk every `auto-revert-interval'
  ;; seconds, in addition to using notification for those files.
  ;;
  ;; Set this variable to a non-nil value to save power by avoiding
  ;; polling when possible.
  (setq auto-revert-avoid-polling t)
  ;; Time, in seconds, between Auto-Revert Mode file checks.
  (setq auto-revert-interval 2)
  ;; If non-nil Auto-Revert Mode reliably updates version control info.
  ;;
  ;; Auto-Revert Mode updates version control info whenever the buffer
  ;; needs reverting, regardless of the value of this variable.
  (setq auto-revert-check-vc-info t))

(use-package whitespace
  :ensure nil
  :hook ((after-init . global-whitespace-mode)
         (before-save . whitespace-cleanup))
  :config (setq whitespace-style '(face tabs empty trailing)))

(use-package dired
  :ensure nil
  :hook (dired-mode . dired-hide-details-mode)
  :config (setq dired-movement-style 'bounded-files)
  :bind (:map dired-mode-map ("-" . dired-up-directory)))

(use-package compile
  :ensure nil
  :preface
  (require 'ansi-color)
  (defun my-colorize-compilation ()
    "Apply ANSI color codes in compilation buffer."
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region compilation-filter-start (point))))
  :hook (compilation-filter . my-colorize-compilation))

(use-package python
  :ensure nil
  :config (setq python-indent-guess-indent-offset nil))

(use-package server
  :ensure nil
  :hook (after-init . server-start))

(use-package treesit
  :ensure nil
  :preface
  (defun my-treesit-install-all-languages ()
    "Install all languages specified by `treesit-language-source-alist'."
    (interactive)
    (dolist (lang (mapcar #'car treesit-language-source-alist))
      (treesit-install-language-grammar lang)
      (message "`%s' parser was installed." lang)
      (sit-for 0.75)))
  :config
  (setq treesit-font-lock-level 4)
  (setq treesit-language-source-alist '((bash       . ("https://github.com/tree-sitter/tree-sitter-bash" "v0.25.1" "src"))
                                        (css        . ("https://github.com/tree-sitter/tree-sitter-css" "v0.20.0"))
                                        (html       . ("https://github.com/tree-sitter/tree-sitter-html" "v0.20.1"))
                                        (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript" "v0.20.1" "src"))
                                        (json       . ("https://github.com/tree-sitter/tree-sitter-json" "v0.20.2"))
                                        (markdown   . ("https://github.com/ikatyang/tree-sitter-markdown" "v0.7.1"))
                                        (python     . ("https://github.com/tree-sitter/tree-sitter-python" "v0.20.4"))
                                        (toml       . ("https://github.com/tree-sitter/tree-sitter-toml" "v0.5.1"))
                                        (tsx        . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "tsx/src"))
                                        (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "typescript/src"))
                                        (yaml       . ("https://github.com/ikatyang/tree-sitter-yaml" "v0.5.0"))))
  (dolist (mapping '((conf-toml-mode  . toml-ts-mode)
                     (css-mode        . css-ts-mode)
                     (js-json-mode    . json-ts-mode)
                     (js2-mode        . js-ts-mode)
                     (json-mode       . json-ts-mode)
                     (python-mode     . python-ts-mode)
                     (typescript-mode . typescript-ts-mode)
                     (bash-mode       . bash-ts-mode)))
    (add-to-list 'major-mode-remap-alist mapping))
  (dolist (mapping '(("\\.go\\'"     . go-ts-mode)
                     ("\\.jsonc?\\'" . json-ts-mode)
                     ("\\.m?jsx?\\'" . js-ts-mode)
                     ("\\.tsx?\\'"   . js-ts-mode)
                     ("\\.ya?ml\\'"  . yaml-ts-mode)))
    (add-to-list 'auto-mode-alist mapping)))

(use-package golden-ratio-scroll-screen
  :ensure t
  :bind (([remap scroll-down-command] . golden-ratio-scroll-screen-down)
         ([remap scroll-up-command] . golden-ratio-scroll-screen-up)))

(use-package md-mermaid
  :vc ( :url "https://github.com/ahmetus/md-mermaid"
        :rev
        :newest)
  :commands (md-mermaid-render-current
             md-mermaid-transient
             md-mermaid-live-mode))

(use-package rg
  :ensure t
  :config
  (rg-enable-default-bindings))

(use-package eglot
  :ensure nil
  :after cape
  :config
  (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)
  (setq eglot-server-programs '((python-ts-mode . ("rass" "python"))))
  (setq-default eglot-workspace-configuration '( :pyright ( :disableOrganizeImports t)
                                                 :python.analysis ( :autoSearchPaths t
                                                                    :useLibraryCodeForTypes t
                                                                    :diagnosticMode "openFilesOnly"))))

(use-package apheleia
  :ensure t
  :hook ((python-ts-mode . apheleia-mode)
         (emacs-lisp-mode . apheleia-mode))
  :config
  (setf (alist-get 'python-mode apheleia-mode-alist) '(ruff-isort ruff))
  (setf (alist-get 'python-ts-mode apheleia-mode-alist) '(ruff-isort ruff)))

(use-package delight
  :ensure t
  :config
  (delight '((eldoc-mode nil "eldoc")
             (anzu-mode nil "anzu")
             (buffer-terminator-mode nil "buffer-terminator")
             (projectile-mode nil "projectile")
             (whitespace-mode nil "whitespace")
             (aggressive-indent-mode nil "aggressive-indent")
             (apheleia-mode nil "apheleia")
             (emacs-lisp-mode "Elisp" :major))))

(use-package ef-themes
  :ensure t
  :init
  (ef-themes-take-over-modus-themes-mode 1)
  :config
  ;; All customisations here.
  (setq modus-themes-mixed-fonts t)
  (setq modus-themes-italic-constructs t)

  ;; Finally, load your theme of choice (or a random one with
  ;; `modus-themes-load-random', `modus-themes-load-random-dark',
  ;; `modus-themes-load-random-light').
  (modus-themes-load-theme 'ef-melissa-light))

(use-package buffer-terminator
  :ensure t
  :hook (after-init . buffer-terminator-mode)
  :config
  (setq buffer-terminator-verbose t)
  ;; Set the inactivity timeout (in seconds) after which buffers are considered
  ;; inactive
  (setq buffer-terminator-inactivity-timeout (* 15 60)) ; 15 minutes
  ;; Define how frequently the cleanup process should run
  (setq buffer-terminator-interval (* 10 60))) ; 10 minutes

(use-package vertico
  :ensure t
  :hook (after-init . vertico-mode)
  :config
  (vertico-multiform-mode)
  (setq vertico-cycle t)
  (dolist (category '((embark-keybinding grid)))
    (add-to-list 'vertico-multiform-categories category))
  :bind ( :map vertico-map
          ("<backspace>" . vertico-directory-delete-char)
          ("C-w"         . vertico-directory-delete-word)
          ("RET"         . vertico-directory-enter)))

(use-package orderless
  :ensure t
  :config
  (setq completion-styles '(orderless basic))
  (setq completion-category-defaults nil)
  (setq completion-category-overrides '((file (styles partial-completion))
                                        (embark-keybinding (styles flex))
                                        (eglot (styles orderless))
                                        (eglot-capf (styles orderless)))))

(use-package marginalia
  :ensure t
  :hook (after-init . marginalia-mode))

(use-package corfu
  :ensure t
  :hook (after-init . global-corfu-mode)
  :config
  (setq corfu-cycle t)
  (corfu-popupinfo-mode 1))

(use-package cape
  :ensure t
  :init
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-symbol))

(use-package consult
  :ensure t
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :init
  ;; Tweak the register preview for `consult-register-load',
  ;; `consult-register-store' and the built-in commands.  This improves the
  ;; register formatting, adds thin separator lines, register sorting and hides
  ;; the window mode line.
  (advice-add #'register-preview :override #'consult-register-window)
  (setq register-preview-delay 0.5)
  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref)
  (setq xref-show-definitions-function #'consult-xref)
  :config
  (setq consult-async-min-input 2)
  (setq consult-narrow-key "<")
  (setq consult-fd-args "fd --type f --hidden --follow --exclude .git")
  :bind (([remap Info-search] . consult-info)
         ("C-x b" . consult-buffer)
         ("M-y" . consult-yank-pop)
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)
         ("M-g l" . consult-line)
         ("M-g g" . consult-goto-line)
         ("M-g M-g" . consult-goto-line)
         ("M-g o" . consult-outline)
         ("M-g i" . consult-imenu)
         ("M-s f" . consult-fd)
         ("M-s g" . consult-ripgrep)
         :map minibuffer-local-map
         ("M-s" . consult-history)))

(use-package embark
  :ensure t
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  :config (add-to-list 'display-buffer-alist
                       '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                         nil
                         (window-parameters (mode-line-format . none)))))

(use-package crux
  :ensure t
  :config
  (crux-with-region-or-line comment-or-uncomment-region)
  (crux-with-region-or-sexp-or-line kill-region)
  (crux-with-region-or-point-to-eol kill-ring-save)
  :bind (("<escape>"                . keyboard-escape-quit)
         ("C-x ;"                   . comment-or-uncomment-region)
         ("C-S-j"                   . crux-top-join-line)
         ("C-S-d"                   . crux-duplicate-current-line-or-region)
         ("C-g"                     . crux-keyboard-quit-dwim)
         ("M-o"                     . crux-other-window-or-switch-buffer)
         ([remap move-beginning-of-line] . crux-move-beginning-of-line)
         ([remap kill-whole-line]   . crux-kill-whole-line)
         ([remap upcase-region]     . crux-upcase-region)
         ([remap downcase-region]   . crux-downcase-region)
         ([remap kill-line]         . crux-smart-kill-line)
         ([(shift return)]          . crux-smart-open-line)
         ([(control shift return)]  . crux-smart-open-line-above)))

;; (use-package mason
;;   :ensure t
;;   :config (mason-setup))

(use-package projectile
  :ensure t
  :hook (after-init . projectile-mode)
  :config
  (setq projectile-project-search-path '("~/Developer/work"
                                         "~/Developer/personal"
                                         "~/Developer/personal/dotfiles"
                                         "~/.emacs.d"
                                         "~/.config/nvim"))
  (setq projectile-cleanup-known-projects t)
  :bind-keymap ("C-x p" . projectile-command-map))

(use-package zoom
  :ensure t
  :config (setq zoom-size '(0.618 . 0.618)))

(use-package multiple-cursors
  :ensure t
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)))

(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

(use-package undo-fu
  :ensure t
  :bind (("C-z"   . undo-fu-only-undo)
         ("C-S-z" . undo-fu-only-redo)))

(use-package magit
  :ensure t)

(use-package helpful
  :ensure t
  :config
  (setq helpful-max-buffers 1)
  :bind (([remap describe-command]  . helpful-command)
         ([remap describe-function] . helpful-callable)
         ([remap describe-key]      . helpful-key)
         ([remap describe-symbol]   . helpful-symbol)
         ([remap describe-variable] . helpful-variable)))

(use-package vterm
  :ensure t)

(use-package mise
  :ensure t
  :hook (after-init . global-mise-mode))

(use-package indent-bars
  :ensure t
  :hook ((python-ts-mode yaml-ts-mode) . indent-bars-mode))

(use-package pyvenv
  :ensure t
  :config
  (pyvenv-mode 1))

;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:

;;; init.el ends here
