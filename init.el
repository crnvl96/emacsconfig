;;; init.el --- Personal Emacs Configuration -*- lexical-binding: t -*-

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
(add-hook 'elpaca-after-init-hook
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
  (setq auto-revert-avoid-polling t)
  (setq auto-revert-interval 2)
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
  (setq treesit-language-source-alist '((css        . ("https://github.com/tree-sitter/tree-sitter-css" "v0.20.0"))
                                        (go         . ("https://github.com/tree-sitter/tree-sitter-go" "v0.20.0"))
                                        (html       . ("https://github.com/tree-sitter/tree-sitter-html" "v0.20.1"))
                                        (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript" "v0.20.1" "src"))
                                        (json       . ("https://github.com/tree-sitter/tree-sitter-json" "v0.20.2"))
                                        (markdown   . ("https://github.com/ikatyang/tree-sitter-markdown" "v0.7.1"))
                                        (python     . ("https://github.com/tree-sitter/tree-sitter-python" "v0.20.4"))
                                        (rust       . ("https://github.com/tree-sitter/tree-sitter-rust" "v0.21.2"))
                                        (toml       . ("https://github.com/tree-sitter/tree-sitter-toml" "v0.5.1"))
                                        (tsx        . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "tsx/src"))
                                        (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "typescript/src"))
                                        (yaml       . ("https://github.com/ikatyang/tree-sitter-yaml" "v0.5.0"))))
  (dolist (mapping '((c-mode          . c-ts-mode)
                     (conf-toml-mode  . toml-ts-mode)
                     (css-mode        . css-ts-mode)
                     (go-mode         . go-ts-mode)
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

(use-package delight
  :ensure t
  :vc (:url "https://savannah.nongnu.org/projects/delight" :rev :newest)
  :config
  (delight '((eldoc-mode nil "eldoc")
             (apheleia-mode nil "apheleia")
             (projectile-mode nil "projectile")
             (anzu-mode nil "anzu")
             (buffer-terminator-mode " ~" "buffer-terminator")
             (whitespace-mode nil "whitespace")
             (emacs-lisp-mode "Emacs Lisp" :major))))

(use-package ef-themes
  :ensure t
  :vc (:url "https://github.com/protesilaos/ef-themes" :rev :newest)
  :config
  (setq ef-themes-mixed-fonts t)
  (setq ef-themes-variable-pitch-ui nil)
  (mapc #'disable-theme custom-enabled-themes)
  (load-theme 'ef-melissa-light t))

(use-package vertico
  :ensure t
  :vc (:url "https://github.com/minad/vertico" :rev :newest)
  :hook (elpaca-after-init . vertico-mode)
  :config
  (vertico-multiform-mode)
  (setq vertico-cycle t)
  (dolist (category '((embark-keybinding grid)
                      (jinx grid (vertico-grid-annotate . 20) (vertico-count . 4))))
    (add-to-list 'vertico-multiform-categories category))
  :bind ( :map vertico-map
          ("<backspace>" . vertico-directory-delete-char)
          ("C-w"         . vertico-directory-delete-word)
          ("RET"         . vertico-directory-enter)))

(use-package orderless
  :ensure t
  :vc (:url "https://github.com/oantolin/orderless" :rev :newest)
  :config
  (setq completion-styles '(orderless basic))
  (setq completion-category-defaults nil)
  (setq completion-category-overrides '((file (styles partial-completion))
                                        (embark-keybinding (styles flex))
                                        (eglot (styles orderless))
                                        (eglot-capf (styles orderless)))))

(use-package marginalia
  :ensure t
  :vc (:url "https://github.com/minad/marginalia" :rev :newest)
  :hook (elpaca-after-init . marginalia-mode))

(use-package corfu
  :ensure t
  :vc (:url "https://github.com/minad/corfu" :rev :newest)
  :hook (elpaca-after-init . global-corfu-mode)
  :config
  (setq corfu-cycle t)
  (corfu-popupinfo-mode 1))

(use-package cape
  :ensure t
  :vc (:url "https://github.com/minad/cape" :rev :newest)
  :init
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-symbol))

(use-package consult
  :ensure t
  :vc (:url "https://github.com/minad/consult" :rev :newest)
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :init
  (advice-add #'register-preview :override #'consult-register-window)
  (setq register-preview-delay 0.5)
  (setq register-preview-function #'consult-register-format)
  (setq xref-show-xrefs-function #'consult-xref)
  (setq xref-show-definitions-function #'consult-xref)
  :config
  (setq consult-async-min-input 2)
  (setq consult-narrow-key "<")
  (setq consult-fd-args "fd --type f --hidden --follow --exclude .git")
  (consult-customize consult-theme consult-ripgrep
                     consult-git-grep
                     consult-grep
                     consult-xref
                     :preview-key "M-.")
  :bind (("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ("C-x M-:" . consult-complex-command)
         ("C-x b" . consult-buffer)
         ("C-x 4 b" . consult-buffer-other-window)
         ("C-x 5 b" . consult-buffer-other-frame)
         ("C-x t b" . consult-buffer-other-tab)
         ("C-x r b" . consult-bookmark)
         ("C-x p b" . consult-project-buffer)
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)
         ("C-M-#" . consult-register)
         ("M-y" . consult-yank-pop)
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)
         ("M-g g" . consult-goto-line)
         ("M-g M-g" . consult-goto-line)
         ("M-g o" . consult-outline)
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ("M-s d" . consult-find)
         ("M-s c" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)
         ("M-s e" . consult-isearch-history)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         :map minibuffer-local-map
         ("M-s" . consult-history)
         ("M-r" . consult-history)))

(use-package embark
  :ensure t
  :vc (:url "https://github.com/oantolin/embark" :rev :newest)
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  :config (add-to-list 'display-buffer-alist
                       '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                         nil
                         (window-parameters (mode-line-format . none))))
  :bind (("C-."   . embark-act)
         ("C-;"   . embark-dwim)
         ("C-h B" . embark-bindings)))

(use-package embark-consult
  :ensure t
  :vc (:url "https://github.com/oantolin/embark" :rev :newest)
  :after (embark consult)
  :hook (embark-collect-mode . consult-preview-at-point-mode))

(use-package wgrep
  :ensure t
  :vc (:url "https://github.com/mhayashi1120/Emacs-wgrep" :rev :newest))

(use-package avy
  :ensure t
  :vc (:url "https://github.com/abo-abo/avy" :rev :newest)
  :config
  (setq avy-background t))

(use-package ace-window
  :ensure t
  :vc (:url "https://github.com/abo-abo/ace-window" :rev :newest)
  :config
  (setq aw-keys '(?h ?j ?k ?l))
  :bind (([remap other-window] . ace-window)
         ("M-o"                 . ace-window)))

(use-package buffer-terminator
  :ensure t
  :vc (:url "https://github.com/jamescherti/buffer-terminator.el" :rev :newest)
  :hook (after-init . buffer-terminator-mode)
  :config
  (setq buffer-terminator-verbose nil)
  (setq buffer-terminator-inactivity-timeout (* 30 60))
  (setq buffer-terminator-interval (* 10 60)))

(use-package aggressive-indent
  :ensure t
  :vc (:url "https://github.com/Malabarba/aggressive-indent-mode" :rev :newest)
  :hook (emacs-lisp-mode . aggressive-indent-mode))

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
         ([remap move-beginning-of-line] . crux-move-beginning-of-line)
         ([remap kill-whole-line]   . crux-kill-whole-line)
         ([remap upcase-region]     . crux-upcase-region)
         ([remap downcase-region]   . crux-downcase-region)
         ([remap kill-line]         . crux-smart-kill-line)
         ([(shift return)]          . crux-smart-open-line)
         ([(control shift return)]  . crux-smart-open-line-above)))

(use-package zoom
  :ensure t
  :config
  (setq zoom-size '(0.618 . 0.618)))

(use-package key-chord
  :ensure t
  :hook (elpaca-after-init . key-chord-mode)
  :config
  (setq key-chord-typing-detection t)
  (key-chord-define-global "jj" 'avy-goto-char-timer)
  (key-chord-define-global "KK" 'backward-paragraph)
  (key-chord-define-global "JJ" 'forward-paragraph))

(use-package multiple-cursors
  :ensure t
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)))

(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

(use-package anzu
  :ensure t
  :hook (elpaca-after-init . global-anzu-mode)
  :bind (("M-%"   . anzu-query-replace)
         ("C-M-%" . anzu-query-replace-regexp)))

(use-package undo-fu
  :ensure t
  :bind (("C-z"   . undo-fu-only-undo)
         ("C-S-z" . undo-fu-only-redo)))

(use-package undo-fu-session
  :ensure t
  :hook (elpaca-after-init . undo-fu-session-global-mode))

(use-package jinx
  :ensure t
  :config
  (setq jinx-languages "pt_BR en_US")
  :bind (("M-$"   . jinx-correct)
         ("C-M-$" . jinx-languages)))

(use-package projectile
  :ensure t
  :hook (elpaca-after-init . projectile-mode)
  :config
  (setq projectile-project-search-path '("~/Developer/work" "~/Developer/personal" "~/Developer/personal/dotfiles" "~/.emacs.d" "~/.config/nvim"))
  (setq projectile-cleanup-known-projects t)
  :bind-keymap ("C-x p" . projectile-command-map))

(use-package consult-projectile
  :ensure t
  :after (consult projectile))

(use-package magit
  :ensure t)

(use-package transient
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

(use-package exec-path-from-shell
  :ensure t
  :when (memq window-system '(mac ns x))
  :config
  (exec-path-from-shell-initialize))

(use-package eglot
  :ensure nil
  :after cape
  :config
  (fset #'jsonrpc--log-event #'ignore)
  (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)
  (setq eglot-events-buffer-config '(:size 0 :format lisp))
  (setq eglot-send-changes-idle-time 0.1)
  (setq eglot-extend-to-xref t)
  (setq eglot-ignored-server-capabilities '( :signatureHelpProvider
                                             :documentHighlightProvider
                                             :codeLensProvider
                                             :documentRangeFormattingProvider
                                             :documentOnTypeFormattingProvider
                                             :documentLinkProvider
                                             :foldingRangeProvider
                                             :inlayHintProvider))
  (setq eglot-server-programs '((python-ts-mode . ("rass" "python"))
                                (c-ts-mode      . ("clangd"))
                                (go-ts-mode     . ("gopls"))))
  (setq-default eglot-workspace-configuration '( :pyright ( :disableOrganizeImports t)
                                                 :python.analysis ( :autoSearchPaths t
                                                                    :useLibraryCodeForTypes t
                                                                    :diagnosticMode "openFilesOnly")
                                                 :gopls ( :gofumpt t))))

(use-package apheleia
  :ensure t
  :hook ((c-ts-mode go-ts-mode python-ts-mode emacs-lisp-mode) . apheleia-mode)
  :preface
  (let ((clang-format-file "/home/linuxbrew/.linuxbrew/Cellar/llvm/21.1.8/share/emacs/site-lisp/llvm/clang-format.el"))
    (when (file-exists-p clang-format-file)
      (load clang-format-file)))
  (defun my-create-clang-format ()
    "Create .clang-format file at project root with LLVM style."
    (interactive)
    (let ((dir (expand-file-name default-directory))
          (project-root nil))
      (while (and dir (not (string= dir "/")) (not project-root))
        (if (file-directory-p (expand-file-name ".git" dir))
            (setq project-root dir)
          (setq dir (file-name-directory (directory-file-name dir)))))
      (if (not project-root)
          (message "No git repository found.")
        (let ((clang-format-path (expand-file-name ".clang-format" project-root)))
          (if (file-exists-p clang-format-path)
              (message ".clang-format already exists at %s" clang-format-path)
            (let ((default-directory project-root))
              (shell-command "clang-format -style=llvm -dump-config > .clang-format")
              (message ".clang-format created at %s" clang-format-path)))))))
  :config
  (push '(my-clang-format . (clang-format-buffer)) apheleia-formatters)
  (push '(my-gofumpt . ("gofumpt")) apheleia-formatters)
  (setf (alist-get 'python-ts-mode apheleia-mode-alist) '(ruff-isort ruff)
        (alist-get 'go-ts-mode apheleia-mode-alist)     '(my-gofumpt)
        (alist-get 'c-ts-mode apheleia-mode-alist)      '(my-clang-format)))

(use-package mise
  :ensure t
  :hook (elpaca-after-init . global-mise-mode))

(use-package indent-bars
  :ensure t
  :hook ((python-ts-mode yaml-ts-mode) . indent-bars-mode))

(use-package pyvenv
  :ensure t
  :preface
  (defun my-find-and-activate-venv ()
    "Find and activate .venv directory by scanning upward from current directory."
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
                (progn
                  (setq stopped-dir dir)
                  (setq dir nil))
              (setq dir (file-name-directory (directory-file-name dir)))))))
      (if venv-dir
          (progn
            (message "Venv found at %s, activating..." venv-dir)
            (pyvenv-activate venv-dir)
            (when pyvenv-virtual-env
              (message "Venv activated at %s" venv-dir)))
        (message "No venv found. Search started from %s and stopped at %s"
                 default-directory (or stopped-dir "/"))))))

;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:

;;; init.el ends here
