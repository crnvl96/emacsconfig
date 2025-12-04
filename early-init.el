;;; early-init.el --- Early Init -*- lexical-binding: t; -*-

;; Prefer the newer versions of files
(setq load-prefer-newer t)

;; ;;; Native compilation and Byte compilation
;; (if (and (featurep 'native-compile)
;;          (fboundp 'native-comp-available-p)
;;          (native-comp-available-p))
;;       ;; Activate `native-compile'
;;       (setq package-native-compile t))

;; Deactivate the `native-compile' feature if it is not available
;; (setq features (delq 'native-compile features))

;;; Miscellaneous

(setq inhibit-splash-screen t
      use-file-dialog nil
      use-dialog-box nil)

;; Declutter the UI
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(tooltip-mode -1)

(set-language-environment "UTF-8")

;; Set-language-environment sets default-input-method, which is unwanted.
(setq default-input-method nil)
;; Increase how much is read from processes in a single chunk
(setq read-process-output-max (* 2 1024 1024))  ; 1024kb
(setq process-adaptive-read-buffering nil)

;;; Package initialization

(package-initialize)
(unless (package-installed-p 'use-package)
  (unless (seq-empty-p package-archive-contents)
    (package-refresh-contents))
  (package-install 'use-package))
(require 'use-package)
(setq package-archives '( ("melpa"        . "https://melpa.org/packages/")
                         ("gnu"          . "https://elpa.gnu.org/packages/")
                         ("nongnu"       . "https://elpa.nongnu.org/nongnu/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/"))
      package-archive-priorities '( ("melpa"        . 90)
                                   ("gnu"          . 70)
                                   ("nongnu"       . 60)
                                   ("melpa-stable" . 50)))
