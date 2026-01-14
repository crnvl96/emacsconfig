;;; early-init.el --- Early Init -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(defvar my-user-directory user-emacs-directory)

(setq user-emacs-directory (expand-file-name "var/" my-user-directory))
(setq package-user-dir (expand-file-name "elpa" user-emacs-directory))
(setq package-enable-at-startup nil) ;; For elpaca
(setq byte-compile-warnings '(not obsolete))
(setq warning-suppress-log-types '((comp) (bytecomp)))
(setq native-comp-async-report-warnings-errors 'silent)
(setq inhibit-startup-echo-area-message (user-login-name))
(setq frame-resize-pixelwise t)
(setq default-frame-alist '( (fullscreen . maximized)
                             (vertical-scroll-bars . nil)
                             (horizontal-scroll-bars . nil)
                             ;; Setting the face in here prevents flashes of
                             ;; color as the theme gets activated
                             (background-color . "#000000")
                             (foreground-color . "#ffffff")
                             (ns-appearance . dark)
                             (ns-transparent-titlebar . t)))

(tool-bar-mode -1)
(menu-bar-mode -1)
(blink-cursor-mode -1)

;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; byte-compile-warnings: (not obsolete free-vars)
;; End:

;;; early-init.el ends here
