;;; early-init.el --- Early Init -*- lexical-binding: t; -*-

;;; Commentary:

;; This file is loaded before init.el, before package and UI initialization.
;; Use it for settings that must be applied before the GUI is drawn or
;; packages are loaded to ensure a fast, flicker-free startup.

;;; Code:

;; Increase garbage collection threshold during startup for faster loading.
;; This will be reset to a reasonable value after init completes.
(setq gc-cons-threshold most-positive-fixnum)
(setq gc-cons-percentage 0.6)

;; Increase read buffer size for better subprocess performance
(setq read-process-output-max (* 4 1024 1024))

;; Store the original emacs directory for reference
(defvar my-user-directory user-emacs-directory
  "The original user-emacs-directory before redirection.")

;; Redirect generated files to a separate directory to keep config clean
(setq user-emacs-directory (expand-file-name "var/" my-user-directory))
(setq package-user-dir (expand-file-name "elpa" user-emacs-directory))

;; Disable package.el at startup (using Elpaca instead)
(setq package-enable-at-startup nil)

;; Suppress byte-compilation and native-compilation warnings
(setq byte-compile-warnings '(not obsolete))
(setq warning-suppress-log-types '((comp) (bytecomp)))
(setq native-comp-async-report-warnings-errors 'silent)
(setq native-comp-async-query-on-exit t)

;; Suppress startup echo area message
(setq inhibit-startup-echo-area-message (user-login-name))

;; Enable pixel-precise frame resizing
(setq frame-resize-pixelwise t)

;; Default frame settings - applied to all frames including the initial one.
;; Setting colors here prevents flash of default colors during theme load.
;; Using light theme colors to match ef-melissa-light theme.
(setq default-frame-alist
      '((fullscreen . maximized)
        (vertical-scroll-bars . nil)
        (horizontal-scroll-bars . nil)
        (background-color . "#fff6e8")
        (foreground-color . "#5f4400")
        (ns-appearance . light)
        (ns-transparent-titlebar . t)))

;; Disable these before frame creation to avoid momentary display
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode -1)

;; Disable startup screen and splash
(setq inhibit-splash-screen t)
(setq inhibit-startup-screen t)
(setq inhibit-startup-message t)

;; Start with a minimal scratch buffer
(setq initial-major-mode 'fundamental-mode)
(setq initial-scratch-message nil)

;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:

;;; early-init.el ends here
