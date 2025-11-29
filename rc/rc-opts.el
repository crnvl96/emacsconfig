;;; -*- lexical-binding: t; -*-

(setq which-func-update-delay 1.0)

(setq visible-bell nil)
(setq ring-bell-function #'ignore)

(setq-default display-line-numbers-type 'relative)
(setq-default word-wrap t)

(setq split-width-threshold 170
      split-height-threshold nil)

(setq read-answer-short t)
(if (boundp 'use-short-answers)
    (setq use-short-answers t)
  (advice-add 'yes-or-no-p :override #'y-or-n-p))

(when (bound-and-true-p blink-cursor-mode)
  (blink-cursor-mode -1))

(setq-default truncate-lines t)
(setq native-comp-async-query-on-exit t)
(setq package-install-upgrade-built-in t)
(setq compile-command nil)

(setq whitespace-style '(face trailing empty))
(setq whitespace-highlight-on-current-line t)

(setq save-place-file (expand-file-name "saveplace" user-emacs-directory))
(setq save-place-limit 600)

(setq history-length 300)
(setq savehist-save-minibuffer-history t)
(setq savehist-additional-variables
      '(kill-ring                        ; clipboard
        register-alist                   ; macros
        mark-ring global-mark-ring       ; marks
        search-ring regexp-search-ring)) ; searches

(setq recentf-max-saved-items 300)
(setq recentf-max-menu-items 15)
(setq recentf-auto-cleanup 'mode)
(setq recentf-exclude nil)

(setq auto-revert-interval 2)
(setq revert-without-query (list ".")
      auto-revert-stop-on-user-input nil
      auto-revert-verbose t)
(setq global-auto-revert-non-file-buffers t)
(setq global-auto-revert-ignore-modes '(Buffer-menu-mode))  ; Resolve issue #29

(setq save-place-limit 150)

(setq-default indent-tabs-mode nil
              tab-width 4)

(setq ediff-window-setup-function 'ediff-setup-windows-plain
      ediff-split-window-function 'split-window-horizontally)

(setq xref-show-definitions-function 'xref-show-definitions-completing-read
      xref-show-xrefs-function 'xref-show-definitions-completing-read)

(setq dabbrev-upcase-means-case-search t)

(unless (and (eq window-system 'mac)
             (bound-and-true-p mac-carbon-version-string))
  (setq pixel-scroll-precision-use-momentum nil)
  (pixel-scroll-precision-mode 1))

;;; VC

(setq vc-git-print-log-follow t)
(setq vc-make-backup-files nil)  ; Do not backup version controlled files
(setq vc-git-diff-switches '("--histogram"))  ; Faster algorithm for diffing.

;;; Auto save

;; Enable auto-save to safeguard against crashes or data loss. The
;; `recover-file' or `recover-session' functions can be used to restore
;; auto-saved data.
(setq auto-save-default nil)
(setq auto-save-no-message t)

;; Do not auto-disable auto-save after deleting large chunks of
;; text.
(setq auto-save-include-big-deletions t)

(setq auto-save-list-file-prefix
      (expand-file-name "autosave/" user-emacs-directory))
(setq tramp-auto-save-directory
      (expand-file-name "tramp-autosave/" user-emacs-directory))

;; Auto save options
(setq kill-buffer-delete-auto-save-files t)

;; Remove duplicates from the kill ring to reduce clutter
(setq kill-do-not-save-duplicates t)

(add-hook 'compilation-filter-hook (lambda () (ansi-color-apply-on-region compilation-filter-start (point-max))))
(add-hook 'after-init-hook #'save-place-mode)
(add-hook 'after-init-hook #'savehist-mode)
(add-hook 'after-init-hook #'global-display-line-numbers-mode)
(add-hook 'after-init-hook #'display-time-mode)
(add-hook 'after-init-hook #'global-hl-line-mode)
(add-hook 'after-init-hook #'delete-selection-mode)
(add-hook 'after-init-hook #'winner-mode)
(add-hook 'after-init-hook #'global-auto-revert-mode)
(add-hook 'after-init-hook #'recentf-mode)
(add-hook 'after-init-hook #'global-whitespace-mode)

;; Prefer UTF-8 encoding
(prefer-coding-system 'utf-8)

(provide 'rc-opts)
