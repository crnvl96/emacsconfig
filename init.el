;;; init.el --- Init -*- lexical-binding: t -*-

;;; Commentary:

;; My personal Emacs configuration.

;;; Code:

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

(elpaca elpaca-use-package
  (elpaca-use-package-mode))

(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6
      auto-revert-interval 2
      whitespace-style '(face tabs empty trailing)
      tab-width 4
      inhibit-splash-screen 1
      read-process-output-max (* 4 1024 1024)
      custom-file (expand-file-name "custom.el" my-user-directory)
      scroll-margin 0
      hscroll-margin 24
      scroll-preserve-screen-position 1
      native-comp-async-query-on-exit t
      package-install-upgrade-built-in t
      completion-ignore-case t
      tab-always-indent 'complete
      make-backup-files nil
      visible-bell nil
      ring-bell-function #'ignore)

(setq-default fill-column 88
	      truncate-lines t
	      display-line-numbers-width)

(unless (and (eq window-system 'mac) (bound-and-true-p mac-carbon-version-string))
  (setq pixel-scroll-precision-use-momentum nil)
  (pixel-scroll-precision-mode 1))

(if (boundp 'use-short-answers)
    (setq use-short-answers t)
  (advice-add 'yes-or-no-p :override #'y-or-n-p))

(require 'package)
(package-initialize)
(require 'use-package)
(setq package-archives
      '(("melpa"        . "https://melpa.org/packages/")
        ("gnu"          . "https://elpa.gnu.org/packages/")
        ("nongnu"       . "https://elpa.nongnu.org/nongnu/")
        ("melpa-stable" . "https://stable.melpa.org/packages/"))
      package-archive-priorities
      '(("melpa"        . 90)
        ("gnu"          . 70)
        ("nongnu"       . 60)
        ("melpa-stable" . 50)))

(let ((mono-spaced-font "HackNerdFontMono")
      (proportionately-spaced-font "HackNerdFontPropo"))
  (set-face-attribute 'default nil
		      :weight 'regular
		      :family mono-spaced-font
		      :height 140)
  (set-face-attribute 'fixed-pitch nil
		      :weight 'regular
		      :family mono-spaced-font
		      :height 1.0)
  (set-face-attribute 'variable-pitch nil
		      :weight 'regular
		      :family proportionately-spaced-font
		      :height 1.0))

(require 'ansi-color)
(add-hook 'compilation-filter-hook
	  (lambda ()
	    (ansi-color-apply-on-region
	     compilation-filter-start (point-max))))

(custom-set-faces '(fill-column-indicator-face
		    ((t ( :foreground "gray"
			  :background nil)))))

(dolist (el
	 '((undecorated . t)))
  (add-to-list 'default-frame-alist el))

(dolist (el
	 '(("\\`\\*\\(Warnings\\|Compile-Log\\)\\*\\'" (display-buffer-no-window) (allow-no-window . t))))
  (add-to-list 'display-buffer-alist el))

(dolist (el
	 '((fullscreen . maximized)))
  (add-to-list 'initial-frame-alist el))

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode -1)
(global-display-line-numbers-mode +1)
(global-hl-line-mode +1)
(save-place-mode +1)
(savehist-mode +1)
(recentf-mode +1)
(display-time-mode +1)
(delete-selection-mode +1)
(winner-mode +1)
(window-divider-mode +1)
(global-display-fill-column-indicator-mode 1)
(global-auto-revert-mode +1)
(global-whitespace-mode +1)

(defun scroll-window-halfway-down ()
  "Scroll window down by half of the total window height."
  (interactive)
  (scroll-up (/ (window-height) 2)))
(keymap-global-set "C-v" 'scroll-window-halfway-down)

(defun scroll-window-halfway-up ()
  "Scroll window down by half of the total window height."
  (interactive)
  (scroll-down (/ (window-height) 2)))
(keymap-global-set "M-v" 'scroll-window-halfway-up)

(defun ctrl-g-dwim ()
  "A smarter version of the Ctrl+g keymap."
  (interactive)
  (cond ((region-active-p) (keyboard-quit))
	((derived-mode-p 'completion-list-mode) (delete-completion-window))
	((> (minibuffer-depth) 0) (abort-recursive-edit))
	(t (keyboard-quit))))
(keymap-global-set "C-g" 'ctrl-g-dwim)

(defun toggle-window-split ()
  "Toggle the state of split windows."
  (interactive)
  (if (= (count-windows) 2)
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
	  (if this-win-2nd (other-window 1))
	  (set-window-buffer (selected-window) this-win-buffer)
	  (set-window-buffer (next-window) next-win-buffer)
	  (select-window first-win)
	  (if this-win-2nd (other-window 1))))))
(keymap-global-set "C-x |" 'toggle-window-split)

(add-hook 'before-save-hook #'whitespace-cleanup)
(add-hook 'after-init-hook (lambda ()
			     (setq gc-cons-threshold (* 256 1024 1024)
				   gc-cons-percentage 0.1)
			     (message "Garbage collection thresholds reset after init.")))

(use-package dired
  :ensure nil
  :bind ( :map dired-mode-map
	  ("-" . dired-up-directory)))

(use-package compile
  :ensure nil
  :bind (("C-c c" . compile)))

(use-package delight
  :ensure t
  :demand t
  :config
  (delight 'eldoc-mode nil "eldoc")
  (delight 'emacs-lisp-mode "Elisp" :major)
  (delight 'whitespace-mode nil "whitespace"))

(use-package transient
  :ensure t)

(use-package mise
  :ensure t
  :hook (elpaca-after-init . global-mise-mode))

(use-package magit
  :ensure t)

(use-package eat
  :ensure t)

(use-package indent-bars
  :ensure t
  :hook ((python-ts-mode yaml-ts-mode) . indent-bars-mode))

(use-package combobulate
  :hook ((prog-mode . combobulate-mode))
  :config
  (setq combobulate-key-prefix "C-c o"
	combobulate-cursor-tool 'multiple-cursors)
  :load-path ("~/Developer/personal/combobulate"))

(use-package treesit
  :ensure nil
  :preface
  (defun my-treesit-install-all-languages ()
    "Install all languages specified by `treesit-language-source-alist'."
    (interactive)
    (let ((languages (mapcar 'car treesit-language-source-alist)))
      (dolist (lang languages)
	(treesit-install-language-grammar lang)
	(message "`%s' parser was installed." lang)
	(sit-for 0.75))))
  :config
  (setq treesit-language-source-alist
	'((bash . ("https://github.com/tree-sitter/tree-sitter-bash" "v0.25.1"))
	  (c . ("https://github.com/tree-sitter/tree-sitter-c" "0.24.1"))
	  (css . ("https://github.com/tree-sitter/tree-sitter-css" "v0.20.0"))
          (dockerfile . ("https://github.com/camdencheek/tree-sitter-dockerfile" "v0.2.0"))
          (go . ("https://github.com/tree-sitter/tree-sitter-go" "v0.20.0"))
          (html . ("https://github.com/tree-sitter/tree-sitter-html" "v0.20.1"))
          (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript" "v0.20.1" "src"))
          (json . ("https://github.com/tree-sitter/tree-sitter-json" "v0.20.2"))
          (markdown . ("https://github.com/ikatyang/tree-sitter-markdown" "v0.7.1"))
          (python . ("https://github.com/tree-sitter/tree-sitter-python" "v0.20.4"))
          (rust . ("https://github.com/tree-sitter/tree-sitter-rust" "v0.21.2"))
          (toml . ("https://github.com/tree-sitter/tree-sitter-toml" "v0.5.1"))
          (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "tsx/src"))
          (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "typescript/src"))
          (yaml . ("https://github.com/ikatyang/tree-sitter-yaml" "v0.5.0"))))
  (dolist (mapping
	   '((c-mode . c-ts-mode)
	     (conf-toml-mode . toml-ts-mode)
	     (css-mode . css-ts-mode)
	     (css-mode . css-ts-mode)
	     (go-mode . go-ts-mode)
	     (js-json-mode . json-ts-mode)
	     (js2-mode . js-ts-mode)
	     (json-mode . json-ts-mode)
	     (python-mode . python-ts-mode)
	     (typescript-mode . typescript-ts-mode)
	     (bash-mode . bash-ts-mode)))
    (add-to-list 'major-mode-remap-alist mapping))
  (dolist (mapping
	   '(("\\.go\\'" . go-ts-mode)
	     ("\\.jsonc?\\'" . json-ts-mode)
	     ("\\.m?jsx?\\'" . js-ts-mode)
	     ("\\.tsx?\\'" . js-ts-mode)
	     ("\\.ya?ml\\'" . yaml-ts-mode)))
    (add-to-list 'auto-mode-alist mapping)))

(use-package ef-themes
  :ensure t
  :init
  (ef-themes-take-over-modus-themes-mode 1)
  :config
  (setq modus-themes-mixed-fonts t
	modus-themes-italic-constructs t)
  (mapc #'disable-theme custom-enabled-themes)
  (load-theme 'ef-elea-dark t))

(use-package pyvenv
  :ensure t
  :preface
  (defun my-find-and-activate-venv ()
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
		 default-directory
		 (or stopped-dir "/"))))))

(use-package eglot
  :ensure nil
  :config
  (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)
  (setq eglot-events-buffer-config
	'( :size 0
	   :format lisp)
	eglot-ignored-server-capabilities
	'( :signatureHelpProvider
	   :documentHighlightProvider
	   :codeLensProvider
	   :documentRangeFormattingProvider
	   :documentOnTypeFormattingProvider
	   :documentLinkProvider
	   :foldingRangeProvider
	   :inlayHintProvider)
	eglot-server-programs
	'((python-ts-mode . ("pyright-langserver" "--stdio"))
	  (c-ts-mode . ("clangd"))
	  (go-ts-mode . ("gopls"))))
  (setq-default eglot-workspace-configuration
		'( :pyright ( :disableOrganizeImports t)
		   :python.analysis ( :autoSearchPaths t
				      :useLibraryCodeForTypes t
				      :diagnosticMode "openFilesOnly")
		   :gopls ( :gofumpt t))))

(use-package apheleia
  :ensure t
  :delight
  :hook (((c-ts-mode go-ts-mode python-ts-mode emacs-lisp-mode) . apheleia-mode))
  :preface
  (load "/home/linuxbrew/.linuxbrew/Cellar/llvm/21.1.8/share/emacs/site-lisp/llvm/clang-format.el")
  (defun my-set-clang-format ()
    "Scan upwards for .clang-format file. If not found, create it at project root."
    (interactive)
    (let ((dir (expand-file-name default-directory))
          (project-root nil)
          (clang-format-path nil))
      (while (and dir (not (string= dir "/")) (not project-root))
	(if (file-directory-p (expand-file-name ".git" dir))
            (setq project-root dir)
          (setq dir (file-name-directory (directory-file-name dir)))))
      (if (not project-root)
          (message "No git repository found.")
	(setq clang-format-path (expand-file-name ".clang-format" project-root))
	(if (file-exists-p clang-format-path)
            (message ".clang-format already exists at %s" clang-format-path)
          (let ((default-directory project-root))
            (shell-command "clang-format -style=llvm -dump-config > .clang-format")
            (message ".clang-format created at %s" clang-format-path))))))
  :config
  (push '(my-clang-format (clang-format-buffer)) apheleia-formatters)
  (push '(my-gofumpt-format . ("gofumpt")) apheleia-formatters)
  (setf (alist-get 'python-ts-mode apheleia-mode-alist) '(ruff-isort ruff)
	(alist-get 'go-ts-mode apheleia-mode-alist) '(my-gofumpt-format)
	(alist-get 'c-ts-mode apheleia-mode-alist) '(my-clang-format)))

(use-package ace-window
  :ensure t
  :config (setq aw-keys '(?h ?j ?k ?l))
  :bind (([remap other-window] . ace-window)
	 ("M-o" . ace-window)))

(use-package jinx
  :ensure t
  :config
  (setq jinx-languages "pt_BR" "en_US")
  :bind (("M-$" . jinx-correct)
	 ("C-M-$" . jinx-languages)))

(use-package avy
  :ensure t
  :config (setq avy-background t)
  :bind ("M-i" . avy-goto-char-timer))

(use-package multiple-cursors
  :ensure t
  :bind (("C->" . mc/mark-next-like-this)
	 ("C-<" . mc/mark-previous-like-this)))

(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

(use-package marginalia
  :ensure t
  :hook (elpaca-after-init . marginalia-mode))

(use-package anzu
  :ensure t
  :delight
  :hook (elpaca-after-init . global-anzu-mode)
  :bind (("M-%" . anzu-query-replace)
	 ("C-M-%" . anzu-query-replace-regexp)))

(use-package vertico
  :ensure t
  :hook (elpaca-after-init . vertico-mode)
  :config
  (vertico-multiform-mode)
  (dolist (category
	   '((embark-keybinding-grid)
	     (jinx grid (vertico-grid-annotate . 20) (vertico-count . 4))))
    (add-to-list 'vertico-multiform-categories category))
  (setq vertico-cycle t)
  :bind ( :map vertico-map
	  ("<backspace>" . vertico-directory-delete-char)
	  ("C-w" . vertico-directory-delete-word)
	  ("RET" . vertico-directory-enter)))

(use-package orderless
  :ensure t
  :init
  (setq completion-styles '(orderless basic)
	completion-category-defaults nil
	completion-category-overrides '( (file (styles partial-completion))
  					 (embark-keybinding (styles flex))
					 (eglot (styles orderless))
					 (eglot-capf (styles orderless)))
	completion-pcm-leading-wildcard t))

(use-package cape
  :ensure t
  :demand t
  :init
  ;; Add to the global default value of `completion-at-point-functions' which is
  ;; used by `completion-at-point'.  The order of the functions matters, the
  ;; first function returning a result wins.  Note that the list of buffer-local
  ;; completion functions takes precedence over the global list.
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block))

(use-package corfu
  :ensure t
  :hook
  (elpaca-after-init . global-corfu-mode)
  :init
  (corfu-popupinfo-mode)
  :config
  (setq corfu-cycle t))

(use-package projectile
  :ensure t
  :delight
  :hook (elpaca-after-init . projectile-mode)
  :preface
  (defun put-current-file-name-into-current-buffer ()
    "Put the name of the currently opened file into the current buffer."
    (interactive)
    (let ((fname (buffer-file-name (window-buffer (minibuffer-selected-window)))))
      (when fname
	(insert (file-relative-name fname (projectile-project-root))))))
  :config
  (setq projectile-project-search-path '("~/Developer/work"
					 "~/Developer/personal"
					 "~/Developer/personal/dotfiles"
					 "~/.emacs.d"
					 "~/.config/nvim")
	projectile-cleanup-known-projects t)
  :bind-keymap ("C-x p" . projectile-command-map)
  :bind ( :map projectile-mode-map
	  ("C-c j" . projectile-command-map)
	  ("C-c j ." . put-current-file-name-into-current-buffer)))

(use-package consult-projectile
  :ensure t
  :after (consult projectile))

(use-package consult
  :ensure t
  :config
  (setq consult-async-min-input 2
	consult-narrow-key "<"
	consult-fd-args "fd --type f --hidden --follow --exclude .git")
  :bind (("C-c f f" . consult-projectile)
	 ("C-c f o" . consult-outline)
	 ("C-c f k" . consult-flymake)
	 ("C-c f l" . consult-line)
	 ("C-c f ." . consult-goto-line)))

(use-package embark
  :ensure t
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  (add-to-list 'display-buffer-alist
	       '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
		 nil
		 (window-parameters (mode-line-format . none))))
  :bind (("C-." . embark-act)
	 ("C-;" . embark-dwim)
	 ("C-h B" . embark-bindings)))

(use-package embark-consult
  :ensure t
  :after (embark consult)
  :hook (embark-collect-mode . consult-preview-at-point-mode))

(use-package rg
  :ensure t
  :bind (("C-c f g" . rg-menu)))

(use-package exec-path-from-shell
  :ensure t
  :demand t
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package undo-fu
  :demand t
  :ensure t
  :bind (("C-z" . undo-fu-only-undo)
	 ("C-S-z" . undo-fu-only-redo)))

(use-package undo-fu-session
  :ensure t
  :hook (elpaca-after-init . undo-fu-session-global-mode)
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
  :demand t
  :config
  (crux-with-region-or-line comment-or-uncomment-region)
  (crux-with-region-or-sexp-or-line kill-region)
  (crux-with-region-or-point-to-eol kill-ring-save)
  :bind (("<escape>" . keyboard-escape-quit)
	 ("C-x ;" . comment-or-uncomment-region)
	 ("C-S-j" . crux-top-join-line)
	 ("C-S-d" . crux-duplicate-current-line-or-region)
	 ([remap move-beginning-of-line] . crux-move-beginning-of-line)
	 ([remap kill-whole-line] . crux-kill-whole-line)
	 ([remap upcase-region] . crux-upcase-region)
	 ([remap downcase-region] . crux-downcase-region)
	 ([remap kill-line] . crux-smart-kill-line)
	 ([(shift return)] . crux-smart-open-line)
	 ([(control shift return)] . crux-smart-open-line-above)))

;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; byte-compile-warnings: (not obsolete free-vars)
;; End:

;;; init.el ends here
