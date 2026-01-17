;;; my-core.el --- Core settings -*- lexical-binding: t -*-

;;; Commentary:

;; Core Emacs settings: custom file, defaults, modes, and GC configuration.

;;; Code:

;; Custom file location (keep init.el clean)
(setq custom-file (expand-file-name "custom.el" my-user-directory))
(when (file-exists-p custom-file)
  (load custom-file 'noerror 'nomessage))

(setq use-short-answers t)                      ; y/n instead of yes/no
(setq confirm-kill-emacs 'y-or-n-p)             ; Confirm before exiting
(setq enable-recursive-minibuffers t)           ; Allow nested minibuffers
(setq sentence-end-double-space nil)            ; Single space after period
(setq make-backup-files nil)                    ; Don't create backup files
(setq visible-bell nil)                         ; No visual bell
(setq ring-bell-function #'ignore)              ; No audible bell
(setq switch-to-buffer-obey-display-actions t)  ; Respect display-buffer rules

(setq-default truncate-lines t)                 ; Don't wrap long lines
(setq-default display-line-numbers-width 3)     ; Consistent line number width
(setq indicate-buffer-boundaries 'left)         ; Show buffer boundaries
(setq x-underline-at-descent-line nil)          ; Better underline positioning
(setq display-time-default-load-average nil)    ; Don't show load average

(setq scroll-margin 0)
(setq hscroll-margin 24)
(setq scroll-preserve-screen-position t)
(setq pixel-scroll-precision-use-momentum nil)

(setq-default tab-width 4)                      ; Tab display width
(setq-default indent-tabs-mode nil)             ; Use spaces, not tabs
(setq tab-always-indent 'complete)              ; Tab indents or completes
(setq completion-ignore-case t)                 ; Case-insensitive completion

(add-hook 'elpaca-after-init-hook
          (lambda ()
            (setq gc-cons-threshold (* 64 1024 1024))  ; 64MB
            (setq gc-cons-percentage 0.1)))

(global-display-line-numbers-mode 1)  ; Show line numbers
(global-hl-line-mode -1)              ; Highlight current line
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

;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:

(provide 'my-core)
;;; my-core.el ends here
