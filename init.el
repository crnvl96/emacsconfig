;;; init.el --- Personal Emacs Configuration -*- lexical-binding: t -*-

;;; Commentary:

;; A clean, readable Emacs configuration focused on simplicity and smooth UX.
;;
;; Structure:
;; 1. Package Manager Bootstrap (Elpaca)
;; 2. Core Settings & Defaults
;; 3. Built-in Packages
;; 4. Appearance & Theme
;; 5. Completion Framework (Vertico, Corfu, etc.)
;; 6. Navigation & Search
;; 7. Editing Enhancements
;; 8. Project Management
;; 9. Programming & LSP
;; 10. Language-Specific Configuration

;;; Code:

;; ============================================================================
;; Package Manager Bootstrap (Elpaca)
;; ============================================================================

(defvar elpaca-installer-version 0.11)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1 :inherit ignore
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))

(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (<= emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
                                                      (list (format "--depth=%d" depth) "--no-single-branch"))
                                                  ,(plist-get order :repo) ,repo))))
                  ((zerop (call-process "git" nil buffer t "checkout"
                                        (or (plist-get order :ref) "--"))))
                  (emacs (concat invocation-directory invocation-name))
                  ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                        "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                  ((require 'elpaca))
                  ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (let ((load-source-file-function nil)) (load "./elpaca-autoloads"))))

(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;; Enable use-package integration with Elpaca
(elpaca elpaca-use-package (elpaca-use-package-mode))

;; ============================================================================
;; Core Settings & Defaults
;; ============================================================================

;; Custom file location (keep init.el clean)
(setq custom-file (expand-file-name "custom.el" my-user-directory))
(when (file-exists-p custom-file)
  (load custom-file 'noerror 'nomessage))

;; -----------------------------------------------------------------------------
;; General Behavior
;; -----------------------------------------------------------------------------

(setq use-short-answers t)                      ; y/n instead of yes/no
(setq confirm-kill-emacs 'y-or-n-p)             ; Confirm before exiting
(setq enable-recursive-minibuffers t)           ; Allow nested minibuffers
(setq sentence-end-double-space nil)            ; Single space after period
(setq make-backup-files nil)                    ; Don't create backup files
(setq visible-bell nil)                         ; No visual bell
(setq ring-bell-function #'ignore)              ; No audible bell
(setq switch-to-buffer-obey-display-actions t)  ; Respect display-buffer rules

;; -----------------------------------------------------------------------------
;; Display Settings
;; -----------------------------------------------------------------------------

(setq-default truncate-lines t)                 ; Don't wrap long lines
(setq-default display-line-numbers-width 3)     ; Consistent line number width
(setq indicate-buffer-boundaries 'left)         ; Show buffer boundaries
(setq x-underline-at-descent-line nil)          ; Better underline positioning
(setq display-time-default-load-average nil)    ; Don't show load average

;; -----------------------------------------------------------------------------
;; Scrolling
;; -----------------------------------------------------------------------------

(setq scroll-margin 0)
(setq hscroll-margin 24)
(setq scroll-preserve-screen-position t)
(setq pixel-scroll-precision-use-momentum nil)

;; -----------------------------------------------------------------------------
;; Editing Defaults
;; -----------------------------------------------------------------------------

(setq-default tab-width 4)                      ; Tab display width
(setq-default indent-tabs-mode nil)             ; Use spaces, not tabs
(setq tab-always-indent 'complete)              ; Tab indents or completes
(setq completion-ignore-case t)                 ; Case-insensitive completion

;; -----------------------------------------------------------------------------
;; Performance Tuning (Post-Startup)
;; -----------------------------------------------------------------------------

(add-hook 'elpaca-after-init-hook
          (lambda ()
            (setq gc-cons-threshold (* 64 1024 1024))  ; 64MB
            (setq gc-cons-percentage 0.1)))

;; ============================================================================
;; Fonts
;; ============================================================================

(let ((mono-font "HackNerdFontMono")
      (prop-font "HackNerdFontPropo"))
  (set-face-attribute 'default nil
                      :family mono-font
                      :height 140
                      :weight 'regular)
  (set-face-attribute 'fixed-pitch nil
                      :family mono-font
                      :height 1.0
                      :weight 'regular)
  (set-face-attribute 'variable-pitch nil
                      :family prop-font
                      :height 1.0
                      :weight 'regular))

;; ============================================================================
;; Global Modes
;; ============================================================================

(global-display-line-numbers-mode 1)  ; Show line numbers
(global-hl-line-mode 1)               ; Highlight current line
(column-number-mode 1)                ; Show column number in modeline
(save-place-mode 1)                   ; Remember cursor position
(savehist-mode 1)                     ; Remember minibuffer history
(recentf-mode 1)                      ; Track recent files
(display-time-mode 1)                 ; Show time in modeline
(delete-selection-mode 1)             ; Replace selection when typing
(winner-mode 1)                       ; Window configuration undo/redo
(window-divider-mode 1)               ; Show window dividers
(pixel-scroll-precision-mode 1)       ; Smooth scrolling

(when (display-graphic-p)
  (context-menu-mode 1))              ; Right-click context menu

;; ============================================================================
;; Built-in Packages
;; ============================================================================

;; -----------------------------------------------------------------------------
;; Emacs Core (Custom Functions & Keybindings)
;; -----------------------------------------------------------------------------

(use-package emacs
  :ensure nil
  :preface
  (defun my-scroll-window-halfway-down ()
    "Scroll window down by half of the total window height."
    (interactive)
    (scroll-up (/ (window-height) 2)))

  (defun my-scroll-window-halfway-up ()
    "Scroll window up by half of the total window height."
    (interactive)
    (scroll-down (/ (window-height) 2)))

  (defun my-toggle-window-split ()
    "Toggle between horizontal and vertical window split."
    (interactive)
    (when (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (when this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (when this-win-2nd (other-window 1))))))

  :config
  ;; Suppress certain buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*\\(Warnings\\|Compile-Log\\)\\*\\'"
                 (display-buffer-no-window)
                 (allow-no-window . t)))

  :bind (("C-v" . my-scroll-window-halfway-down)
         ("M-v" . my-scroll-window-halfway-up)))

;; -----------------------------------------------------------------------------
;; Auto-Revert (Keep Buffers in Sync with Disk)
;; -----------------------------------------------------------------------------

(use-package autorevert
  :ensure nil
  :hook (elpaca-after-init . global-auto-revert-mode)
  :custom
  (auto-revert-avoid-polling t)
  (auto-revert-interval 2)
  (auto-revert-check-vc-info t))

;; -----------------------------------------------------------------------------
;; Whitespace (Visualize and Clean Up)
;; -----------------------------------------------------------------------------

(use-package whitespace
  :ensure nil
  :hook ((elpaca-after-init . global-whitespace-mode)
         (before-save . whitespace-cleanup))
  :custom
  (whitespace-style '(face tabs empty trailing)))

;; -----------------------------------------------------------------------------
;; Dired (File Manager)
;; -----------------------------------------------------------------------------

(use-package dired
  :ensure nil
  :bind (:map dired-mode-map
              ("-" . dired-up-directory)))

;; -----------------------------------------------------------------------------
;; Compilation
;; -----------------------------------------------------------------------------

(use-package compile
  :ensure nil
  :preface
  (require 'ansi-color)
  (defun my-colorize-compilation ()
    "Apply ANSI color codes in compilation buffer."
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region compilation-filter-start (point))))
  :hook (compilation-filter . my-colorize-compilation)
  :bind ("C-c `" . compile))

;; -----------------------------------------------------------------------------
;; Tree-sitter
;; -----------------------------------------------------------------------------

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

  :custom
  (treesit-font-lock-level 4)

  :config
  ;; Language grammar sources
  (setq treesit-language-source-alist
        '((css        . ("https://github.com/tree-sitter/tree-sitter-css" "v0.20.0"))
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

  ;; Remap traditional modes to tree-sitter modes
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

  ;; File associations for tree-sitter modes
  (dolist (mapping '(("\\.go\\'"     . go-ts-mode)
                     ("\\.jsonc?\\'" . json-ts-mode)
                     ("\\.m?jsx?\\'" . js-ts-mode)
                     ("\\.tsx?\\'"   . js-ts-mode)
                     ("\\.ya?ml\\'"  . yaml-ts-mode)))
    (add-to-list 'auto-mode-alist mapping)))

;; ============================================================================
;; Appearance & Theme
;; ============================================================================

(use-package ef-themes
  :ensure t
  :init
  (ef-themes-take-over-modus-themes-mode 1)
  :config
  (setq modus-themes-mixed-fonts t)
  (setq modus-themes-italic-constructs t)
  (mapc #'disable-theme custom-enabled-themes)
  (load-theme 'ef-melissa-light t))

(use-package delight
  :ensure t
  :config
  (delight '((eldoc-mode nil "eldoc")
             (apheleia-mode nil "apheleia")
             (projectile-mode nil "projectile")
             (global-anzu-mode nil "anzu")
             (whitespace-mode nil "whitespace")
             (emacs-lisp-mode "Elisp" :major))))

;; ============================================================================
;; Completion Framework
;; ============================================================================

;; -----------------------------------------------------------------------------
;; Vertico (Vertical Completion UI)
;; -----------------------------------------------------------------------------

(use-package vertico
  :ensure t
  :hook (elpaca-after-init . vertico-mode)
  :custom
  (vertico-cycle t)
  :config
  (vertico-multiform-mode)
  (dolist (category '((embark-keybinding grid)
                      (jinx grid (vertico-grid-annotate . 20) (vertico-count . 4))))
    (add-to-list 'vertico-multiform-categories category))
  :bind (:map vertico-map
              ("<backspace>" . vertico-directory-delete-char)
              ("C-w"         . vertico-directory-delete-word)
              ("RET"         . vertico-directory-enter)))

;; -----------------------------------------------------------------------------
;; Orderless (Completion Matching Style)
;; -----------------------------------------------------------------------------

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion))
                                   (embark-keybinding (styles flex))
                                   (eglot (styles orderless))
                                   (eglot-capf (styles orderless))))
  (completion-pcm-leading-wildcard t))

;; -----------------------------------------------------------------------------
;; Marginalia (Completion Annotations)
;; -----------------------------------------------------------------------------

(use-package marginalia
  :ensure t
  :hook (elpaca-after-init . marginalia-mode))

;; -----------------------------------------------------------------------------
;; Corfu (In-Buffer Completion)
;; -----------------------------------------------------------------------------

(use-package corfu
  :ensure t
  :hook (elpaca-after-init . global-corfu-mode)
  :custom
  (corfu-cycle t)
  :config
  (corfu-popupinfo-mode 1))

;; -----------------------------------------------------------------------------
;; Cape (Completion-at-Point Extensions)
;; -----------------------------------------------------------------------------

(use-package cape
  :ensure t
  :init
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-symbol))

;; ============================================================================
;; Navigation & Search
;; ============================================================================

;; -----------------------------------------------------------------------------
;; Consult (Enhanced Search Commands)
;; -----------------------------------------------------------------------------

(use-package consult
  :ensure t
  :custom
  (consult-async-min-input 2)
  (consult-narrow-key "<")
  (consult-fd-args "fd --type f --hidden --follow --exclude .git")
  :bind (("C-c f f" . consult-projectile)
         ("C-c f F" . consult-fd)
         ("C-c f G" . consult-ripgrep)
         ("C-c f o" . consult-outline)
         ("C-c f k" . consult-flymake)
         ("C-c f l" . consult-line)
         ("C-c f ." . consult-goto-line)))

;; -----------------------------------------------------------------------------
;; Embark (Contextual Actions)
;; -----------------------------------------------------------------------------

(use-package embark
  :ensure t
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none))))
  :bind (("C-."   . embark-act)
         ("C-;"   . embark-dwim)
         ("C-h B" . embark-bindings)))

(use-package embark-consult
  :ensure t
  :after (embark consult)
  :hook (embark-collect-mode . consult-preview-at-point-mode))

;; -----------------------------------------------------------------------------
;; Avy (Jump to Visible Text)
;; -----------------------------------------------------------------------------

(use-package avy
  :ensure t
  :custom
  (avy-background t))

;; -----------------------------------------------------------------------------
;; Ace-Window (Quick Window Switching)
;; -----------------------------------------------------------------------------

(use-package ace-window
  :ensure t
  :custom
  (aw-keys '(?h ?j ?k ?l))
  :bind (([remap other-window] . ace-window)
         ("M-o"                 . ace-window)))

;; -----------------------------------------------------------------------------
;; Ripgrep (Fast Search)
;; -----------------------------------------------------------------------------

(use-package rg
  :ensure t
  :bind ("C-c f g" . rg-menu))

;; ============================================================================
;; Editing Enhancements
;; ============================================================================

;; -----------------------------------------------------------------------------
;; Crux (Essential Editing Commands)
;; -----------------------------------------------------------------------------

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

;; -----------------------------------------------------------------------------
;; Key-Chord (Multi-Key Bindings)
;; -----------------------------------------------------------------------------

(use-package key-chord
  :ensure t
  :hook (elpaca-after-init . key-chord-mode)
  :config
  (setq key-chord-typing-detection t)
  (key-chord-define-global "jj" 'avy-goto-char-2)
  (key-chord-define-global "JJ" 'crux-switch-to-previous-buffer))

;; -----------------------------------------------------------------------------
;; Multiple Cursors
;; -----------------------------------------------------------------------------

(use-package multiple-cursors
  :ensure t
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)))

;; -----------------------------------------------------------------------------
;; Expand Region (Smart Selection)
;; -----------------------------------------------------------------------------

(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

;; -----------------------------------------------------------------------------
;; Anzu (Search Match Counter)
;; -----------------------------------------------------------------------------

(use-package anzu
  :ensure t
  :hook (elpaca-after-init . global-anzu-mode)
  :bind (("M-%"   . anzu-query-replace)
         ("C-M-%" . anzu-query-replace-regexp)))

;; -----------------------------------------------------------------------------
;; Undo-Fu (Better Undo/Redo)
;; -----------------------------------------------------------------------------

(use-package undo-fu
  :ensure t
  :bind (("C-z"   . undo-fu-only-undo)
         ("C-S-z" . undo-fu-only-redo)))

(use-package undo-fu-session
  :ensure t
  :hook (elpaca-after-init . undo-fu-session-global-mode))

;; -----------------------------------------------------------------------------
;; Jinx (Spell Checking)
;; -----------------------------------------------------------------------------

(use-package jinx
  :ensure t
  :custom
  (jinx-languages "pt_BR en_US")
  :bind (("M-$"   . jinx-correct)
         ("C-M-$" . jinx-languages)))

;; ============================================================================
;; Project Management
;; ============================================================================

(use-package projectile
  :ensure t
  :hook (elpaca-after-init . projectile-mode)
  :preface
  (defun my-insert-relative-file-name ()
    "Insert the current file's path relative to the project root."
    (interactive)
    (when-let ((fname (buffer-file-name (window-buffer (minibuffer-selected-window)))))
      (insert (file-relative-name fname (projectile-project-root)))))
  :custom
  (projectile-project-search-path '("~/Developer/work"
                                    "~/Developer/personal"
                                    "~/Developer/personal/dotfiles"
                                    "~/.emacs.d"
                                    "~/.config/nvim"))
  (projectile-cleanup-known-projects t)
  :bind-keymap ("C-x p" . projectile-command-map)
  :bind (:map projectile-mode-map
              ("C-c j"   . projectile-command-map)
              ("C-c j ." . my-insert-relative-file-name)))

(use-package consult-projectile
  :ensure t
  :after (consult projectile))

;; ============================================================================
;; Version Control
;; ============================================================================

(use-package magit
  :ensure t
  :defer t)

(use-package transient
  :ensure t)

;; ============================================================================
;; Help & Documentation
;; ============================================================================

(use-package helpful
  :ensure t
  :custom
  (helpful-max-buffers 3)
  :bind (([remap describe-command]  . helpful-command)
         ([remap describe-function] . helpful-callable)
         ([remap describe-key]      . helpful-key)
         ([remap describe-symbol]   . helpful-symbol)
         ([remap describe-variable] . helpful-variable)))

;; ============================================================================
;; Shell & Terminal
;; ============================================================================

(use-package eat
  :ensure t
  :custom
  (eat-term-name "ghostty")
  (eat-kill-buffer-on-exit t))

(use-package exec-path-from-shell
  :ensure t
  :when (memq window-system '(mac ns x))
  :config
  (exec-path-from-shell-initialize))

;; ============================================================================
;; Programming - General
;; ============================================================================

;; -----------------------------------------------------------------------------
;; Eglot (LSP Client)
;; -----------------------------------------------------------------------------

(use-package eglot
  :ensure nil
  :after cape
  :config
  ;; Performance optimizations
  (fset #'jsonrpc--log-event #'ignore)
  (setq eglot-events-buffer-config '(:size 0 :format lisp))
  (setq eglot-send-changes-idle-time 0.1)
  (setq eglot-extend-to-xref t)

  ;; Cape integration for better completion
  (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)

  ;; Disable noisy/unneeded capabilities
  (setq eglot-ignored-server-capabilities
        '(:signatureHelpProvider
          :documentHighlightProvider
          :codeLensProvider
          :documentRangeFormattingProvider
          :documentOnTypeFormattingProvider
          :documentLinkProvider
          :foldingRangeProvider
          :inlayHintProvider))

  ;; Language server configuration
  (setq eglot-server-programs
        '((python-ts-mode . ("pyright-langserver" "--stdio"))
          (c-ts-mode      . ("clangd"))
          (go-ts-mode     . ("gopls"))))

  ;; Workspace-specific settings
  (setq-default eglot-workspace-configuration
                '(:pyright (:disableOrganizeImports t)
                  :python.analysis (:autoSearchPaths t
                                    :useLibraryCodeForTypes t
                                    :diagnosticMode "openFilesOnly")
                  :gopls (:gofumpt t))))

;; -----------------------------------------------------------------------------
;; Apheleia (Code Formatting)
;; -----------------------------------------------------------------------------

(use-package apheleia
  :ensure t
  :hook ((c-ts-mode go-ts-mode python-ts-mode emacs-lisp-mode) . apheleia-mode)
  :preface
  ;; Load clang-format if available
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
  ;; Custom formatters
  (push '(my-clang-format . (clang-format-buffer)) apheleia-formatters)
  (push '(my-gofumpt . ("gofumpt")) apheleia-formatters)

  ;; Mode-specific formatter assignments
  (setf (alist-get 'python-ts-mode apheleia-mode-alist) '(ruff-isort ruff)
        (alist-get 'go-ts-mode apheleia-mode-alist)     '(my-gofumpt)
        (alist-get 'c-ts-mode apheleia-mode-alist)      '(my-clang-format)))

;; -----------------------------------------------------------------------------
;; Indent Bars (Visual Indentation Guides)
;; -----------------------------------------------------------------------------

(use-package indent-bars
  :ensure t
  :hook ((python-ts-mode yaml-ts-mode) . indent-bars-mode))

;; ============================================================================
;; Programming - Python
;; ============================================================================

(use-package python
  :ensure nil
  :custom
  (python-indent-guess-indent-offset nil))

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

(use-package mise
  :ensure t
  :hook (elpaca-after-init . global-mise-mode))

;; ============================================================================
;; Local Packages
;; ============================================================================

(use-package walnut
  :after (treesit transient)
  :load-path "/home/adr/Developer/personal/walnut")

(use-package agent-shell
  :ensure t)

;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:

;;; init.el ends here
