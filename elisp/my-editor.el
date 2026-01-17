;;; my-editor.el --- Editing facilities -*- lexical-binding: t -*-

;;; Commentary:

;; Editing enhancements: buffer management, indentation, cursors, undo, spelling.

;;; Code:

(use-package buffer-terminator
  :ensure t
  :custom
  ;; Enable/Disable verbose mode to log buffer cleanup events
  (buffer-terminator-verbose nil)
  ;; Set the inactivity timeout (in seconds) after which buffers are considered
  ;; inactive (default is 30 minutes):
  (buffer-terminator-inactivity-timeout (* 30 60)) ; 30 minutes
  ;; Define how frequently the cleanup process should run (default is every 10
  ;; minutes):
  (buffer-terminator-interval (* 10 60)) ; 10 minutes
  :config
  (buffer-terminator-mode 1))

;; Enables automatic indentation of code while typing
(use-package aggressive-indent
  :ensure t
  :commands aggressive-indent-mode
  :hook
  (emacs-lisp-mode . aggressive-indent-mode))

;; Highlights function and variable definitions in Emacs Lisp mode
(use-package highlight-defined
  :ensure t
  :commands highlight-defined-mode
  :hook
  (emacs-lisp-mode . highlight-defined-mode))

;; Configure the built-in Emacs server to start after initialization,
;; allowing the use of the emacsclient command to open files in the
;; current session.
(use-package server
  :ensure nil
  :commands server-start
  :hook
  (after-init . server-start))

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
  :custom
  (zoom-size '(0.618 . 0.618)))

(use-package key-chord
  :ensure t
  :hook (elpaca-after-init . key-chord-mode)
  :config
  (setq key-chord-typing-detection t)
  (key-chord-define-global "jj" 'avy-goto-char)
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
  :custom
  (jinx-languages "pt_BR en_US")
  :bind (("M-$"   . jinx-correct)
         ("C-M-$" . jinx-languages)))

;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:

(provide 'my-editor)
;;; my-editor.el ends here
