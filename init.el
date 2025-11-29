 ;;; init.el --- Init -*- lexical-binding: t; -*-

(add-to-list 'load-path (expand-file-name "~/.emacs.d/rc/"))

;;; UI elements - declutter interface

(setq inhibit-splash-screen t ; Disable splash screen
      use-file-dialog nil     ; Disable file dialog
      use-dialog-box nil)     ; Disable dialog box

(menu-bar-mode -1)   ; No menu bar
(tool-bar-mode -1)   ; No tool bar
(scroll-bar-mode -1) ; No scroll bar
(tooltip-mode -1)    ; No tooltips

;;; Garbage collection management - performance optimization

;; Increase garbage collection thresholds during startup for better performance.
;; This prevents Emacs from running garbage collection during initialization,
;; which can significantly slow down startup time. We set the threshold to a
;; very high value and increase the percentage to reduce GC frequency.
;; After initialization, we reset to more reasonable values.
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

;; Reset garbage collection thresholds after initialization to prevent excessive
;; memory usage during normal operation.
(add-hook 'after-init-hook
          (lambda ()
            (setq gc-cons-threshold (* 64 1024 1024)  ; 64 MB
                  gc-cons-percentage 0.1)
            (message "Garbage collection thresholds reset after init.")))

;;; Use-package

(package-initialize)

;; Install use-package if necessary
(unless (package-installed-p 'use-package)
  (unless (seq-empty-p package-archive-contents)
    (package-refresh-contents))
  (package-install 'use-package))

;; Ensure use-package is available
(require 'use-package)

;; Define sources
(setq package-archives '(("melpa"        . "https://melpa.org/packages/")
                         ("gnu"          . "https://elpa.gnu.org/packages/")
                         ("nongnu"       . "https://elpa.nongnu.org/nongnu/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")))

;; Define priorities
(setq package-archive-priorities '(("melpa"        . 90)
                                   ("gnu"          . 70)
                                   ("nongnu"       . 60)
                                   ("melpa-stable" . 50)))

;;; Buffer organization

(add-to-list 'display-buffer-alist
             '("\\`\\*\\(Warnings\\|Compile-Log\\)\\*\\'"
               (display-buffer-no-window)
               (allow-no-window . t)))

;;; General

(setq-default
 ;; Display relative lines
 display-line-numbers-type 'relative
 ;; Don't "cut" words when wrapping lines
 word-wrap t
 ;; Indentation can insert tabs if this is non-nil.
 indent-tabs-mode nil
 ;; Distance between tab stops (for display of tab characters), in columns
 tab-width 4)
;; Display line numbers
(add-hook 'after-init-hook #'global-display-line-numbers-mode)
;; Define location of custom file
(setq custom-file "~/.emacs.d/custom.el")
;; Conditionally enable scroll-precision-mode
(unless (and (eq window-system 'mac)
             (bound-and-true-p mac-carbon-version-string))
  (setq pixel-scroll-precision-use-momentum nil)
  (pixel-scroll-precision-mode 1))
;; Prefer UTF-8 encoding
(prefer-coding-system 'utf-8)
;; Disable distracting notifications
(setq visible-bell nil
      ring-bell-function #'ignore)
;; Accept single-character answers
(setq read-answer-short t)
(if (boundp 'use-short-answers)
    (setq use-short-answers t)
  ;; Answer prompts with a single letter
  (advice-add 'yes-or-no-p :override #'y-or-n-p))
(when (bound-and-true-p blink-cursor-mode)
  ;; Disable cursor blinking
  (blink-cursor-mode -1))
;; Give each line of test just one screen line
(setq-default truncate-lines t)
;; Query the user about killing async compilations when exiting
(setq native-comp-async-query-on-exit t)
;; Allow emacs to upgrade builtin packages with use-package
(setq package-install-upgrade-built-in t)
;; Don't auto populate the compile command with anything
(setq compile-command nil)
;; If non-nil, don't add a string to kill-ring if it duplicates the last one.
(setq kill-do-not-save-duplicates t)

;;; Hooks

;; Enhance the compilation buffer with ansi colors
(add-hook 'compilation-filter-hook (lambda () (ansi-color-apply-on-region compilation-filter-start (point-max))))
;; Display current time on modeline
(add-hook 'after-init-hook #'display-time-mode)
;; Highlight the current line
(add-hook 'after-init-hook #'global-hl-line-mode)
;; When Delete Selection mode is enabled, typed text replaces the selection
;; if the selection is active.  Otherwise, typed text is just inserted at
;; point regardless of any selection.
(add-hook 'after-init-hook #'delete-selection-mode)
;; Record the changes on window configuration
(add-hook 'after-init-hook #'winner-mode)

;;; Whitespace mode

;; Define the kinds of whitespaces that we want highlighted
(setq whitespace-style '(face trailing empty))
;; Enable whitespace mode
(add-hook 'after-init-hook #'global-whitespace-mode)

;;; Saveplace mode

;; Define target location under custom directory
(setq save-place-file (expand-file-name "saveplace" user-emacs-directory))
;; Increase the limit of entries
(setq save-place-limit 600)
;; Enable saveplace
(add-hook 'after-init-hook #'save-place-mode)

;; Savehist mode

;; Increase the limit of entries
(setq history-length 300)
;; Also save the minibuffer history
(setq savehist-save-minibuffer-history t)
;; Also save additional variables
(setq savehist-additional-variables
      '(kill-ring                        ; clipboard
        register-alist                   ; macros
        mark-ring global-mark-ring       ; marks
        search-ring regexp-search-ring)) ; searches
;; Enable savehist
(add-hook 'after-init-hook #'savehist-mode)

;;; Recentf mode

;; Increase the limit of saved entries
(setq recentf-max-saved-items 300)
;; Increase the number of items in the menu
(setq recentf-max-menu-items 15)
;; Enable recentf
(add-hook 'after-init-hook #'recentf-mode)

;;; Autorevert mode

;; decrease the trigger interval
(setq auto-revert-interval 2)
;; The value is a list of regular expressions.
;; If the file name matches one of these regular expressions,
;; then revert-buffer reverts the file without querying
;; if the file has changed on disk and you have not edited the buffer.
(setq revert-without-query (list "."))
;; When non-nil, user input temporarily interrupts Auto-Revert Mode.
(setq auto-revert-stop-on-user-input nil)
(setq auto-revert-verbose t)
;; When nil, Global Auto-Revert Mode operates only on file-visiting buffers.
(setq global-auto-revert-non-file-buffers t)
;; List of major modes Global Auto-Revert Mode should not check.
(setq global-auto-revert-ignore-modes '(Buffer-menu-mode))
;; Enable autorevert
(add-hook 'after-init-hook #'global-auto-revert-mode)

;;; Ediff

;; Setup windows in a single frame
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
;; The function used to split the main window between buffer-A and buffer-B.
(setq ediff-split-window-function 'split-window-horizontally)

;;; VC (Version Control)

;; If non-nil, use the flag --follow when producing single file logs.
(setq vc-git-print-log-follow t)
;; Don't backup version controlled files
(setq vc-make-backup-files nil)
;; Algorithm for diffs
(setq vc-git-diff-switches '("--histogram"))

;;; Delight - declutter the modeline

(use-package delight
  :ensure t
  :demand t
  :config
  (delight 'whitespace-mode nil "whitespace")
  (delight 'which-key-mode nil "which-key")
  (delight 'devil-mode nil "devil")
  (delight 'visual-line-mode nil "simple")
  (delight 'eldoc-mode nil "eldoc"))

(require 'rc-fonts)
(require 'rc-keymaps)
(require 'rc-which-key)
(require 'rc-theme)

;;; File management
(require 'rc-dired)
(require 'rc-trashed)
(require 'rc-treesit)

;;; Navigation
(require 'rc-ace-window)
(require 'rc-avy)
(require 'rc-expand-region)

;;; Editing
(require 'rc-editorconfig)
(require 'rc-mise)
(require 'rc-exec-path-from-shell)
(require 'rc-buffer-terminator)
(require 'rc-super-save)
(require 'rc-undo-fu)
(require 'rc-helpful)
(require 'rc-crux)

;;; Reading
(require 'rc-nov)

;;; Completion
(require 'rc-completion)
(require 'rc-consult)
(require 'rc-rg)

;;; Version control
(require 'rc-magit)

;;; Code formatting
(require 'rc-apheleia)

;;; LSP
(require 'rc-eglot)

;;; Languages
(require 'rc-python)
(require 'rc-json)
(require 'rc-go)
(require 'rc-elisp)
(require 'rc-markdown)
(require 'rc-org)
