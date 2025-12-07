;;; rc-opts.el --- Opts -*- lexical-binding: t; -*-

;;; Themes and Fonts
(mapc #'disable-theme custom-enabled-themes)
(load-theme 'modus-vivendi t)
(set-face-attribute 'default nil :height 160 :weight 'normal :family "Iosevka")

;;; Scrolling
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)
(when (fboundp 'pixel-scroll-precision-mode)
  (pixel-scroll-precision-mode t))

;;; Frames
(add-to-list 'initial-frame-alist '(fullscreen . maximized))
(add-to-list 'display-buffer-alist
             '("\\`\\*\\(Warnings\\|Compile-Log\\)\\*\\'"
               (display-buffer-no-window)
               (allow-no-window . t)))

;;; Options
(setq-default display-line-numbers-type 'relative)
(add-hook 'after-init-hook #'global-display-line-numbers-mode)

(setq-default fill-column 80)
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(setq native-comp-async-query-on-exit t)
(setq package-install-upgrade-built-in t)
(setq read-process-output-max (* 2 1024 1024))  ; 1024kb
(setq initial-major-mode 'fundamental-mode)
(setq initial-scratch-message nil)
(setq completion-ignore-case t)

(setq whitespace-line-column 80)
(setq whitespace-style '(face tabs empty trailing lines-tail))
(dolist (hook '(prog-mode-hook text-mode-hook))
  (add-hook hook #'whitespace-mode))
(add-hook 'before-save-hook #'whitespace-cleanup)

(setq make-backup-files nil)
(setq ring-bell-function 'ignore)
(setq inhibit-startup-screen t)

(setq save-place-file (expand-file-name "saveplace" user-emacs-directory))
(setq-default save-place t)
(add-hook 'after-init-hook #'save-place-mode)

(setq savehist-additional-variables '(search-ring regexp-search-ring))
(setq savehist-autosave-interval 60)
(setq savehist-file (expand-file-name "savehist" user-emacs-directory))
(add-hook 'after-init-hook #'savehist-mode)

(setq recentf-save-file (expand-file-name "recentf" user-emacs-directory))
(setq recentf-max-saved-items 500)
(setq recentf-max-menu-items 15)
;; disable recentf-cleanup on Emacs start, because it can cause
;; problems with remote files
(setq recentf-auto-cleanup 'never)
(add-hook 'after-init-hook #'recentf-mode)

(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

;; dired - reuse current buffer by pressing 'a'
(put 'dired-find-alternate-file 'disabled nil)
;; always delete and copy recursively
(setq dired-recursive-deletes 'always)
(setq dired-recursive-copies 'always)
;; if there is a dired buffer displayed in the next window, use its
;; current subdir, instead of the current subdir of this dired buffer
(setq dired-dwim-target t)
;; enable some really cool extensions like C-x C-j(dired-jump)
(require 'dired-x)

(fset 'yes-or-no-p 'y-or-n-p)
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

(defun my-compilation-filter-hook ()
  (ansi-color-apply-on-region compilation-filter-start (point-max)))
(add-hook 'compilation-filter-hook #'my-compilation-filter-hook)
(add-hook 'after-init-hook #'which-key-mode)
(add-hook 'after-init-hook #'display-time-mode)
(add-hook 'after-init-hook #'delete-selection-mode)
(add-hook 'after-init-hook #'winner-mode)
(add-hook 'after-init-hook #'global-auto-revert-mode)

(provide 'rc-opts)
