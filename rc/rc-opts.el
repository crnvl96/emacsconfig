;;; -*- lexical-binding: t; -*-

(setq-default
 ;; Display relative lines
 display-line-numbers-type 'relative
 ;; Don't "cut" words when wrapping lines
 word-wrap t
 ;; Indentation can insert tabs if this is non-nil.
 indent-tabs-mode nil
 ;; Distance between tab stops (for display of tab characters), in columns
 tab-width 4)

;; Increase the delay for Emacs to update its display
(setq which-func-update-delay 1.0)

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

;; Define the kinds of whitespaces that we want highlighted
(setq whitespace-style '(face trailing empty))

;; Define target location under custom directory
(setq save-place-file (expand-file-name "saveplace" user-emacs-directory))
;; Increase the limit of entries
(setq save-place-limit 600)

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

;; Increase the limit of saved entries
(setq recentf-max-saved-items 300)
;; Increase the number of items in the menu
(setq recentf-max-menu-items 15)

;; Decrease the trigger interval
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

;; Maximum number of entries to retain in the list; nil means no limit.
(setq save-place-limit 150)

;; Setup windows in a single frame
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
;; The function used to split the main window between buffer-A and buffer-B.
(setq ediff-split-window-function 'split-window-horizontally)

(setq xref-show-definitions-function 'xref-show-definitions-completing-read)
(setq xref-show-xrefs-function 'xref-show-definitions-completing-read)

(setq dabbrev-upcase-means-case-search t)

(unless (and (eq window-system 'mac)
             (bound-and-true-p mac-carbon-version-string))
  (setq pixel-scroll-precision-use-momentum nil)
  (pixel-scroll-precision-mode 1))

(setq vc-git-print-log-follow t)
(setq vc-make-backup-files nil)  ; Do not backup version controlled files
(setq vc-git-diff-switches '("--histogram"))  ; Faster algorithm for diffing.

;;; Auto save

;; Enable auto-save to safeguard against crashes or data loss. The
;; `recover-file' or `recover-session' functions can be used to restore
;; auto-saved data.
(setq auto-save-default nil)
(setq auto-save-no-message t)

;; Do not auto-disable auto-save after deleting large chunks of text.
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
