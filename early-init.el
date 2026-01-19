;;; early-init.el --- Early Init -*- lexical-binding: t; -*-

;;; Commentary:

;; This file is loaded before init.el, before package and UI initialization.
;; Use it for settings that must be applied before the GUI is drawn or
;; packages are loaded to ensure a fast, flicker-free startup.

;;; Code:

(defvar my-user-directory user-emacs-directory
  "The original user-emacs-directory before redirection.")

(setq gc-cons-threshold most-positive-fixnum)
(setq gc-cons-percentage 0.6)
(setq read-process-output-max (* 4 1024 1024))
(setq user-emacs-directory (expand-file-name "var/" my-user-directory))
(setq package-user-dir (expand-file-name "elpa" user-emacs-directory))
(setq package-enable-at-startup nil)
(setq byte-compile-warnings '(not obsolete))
(setq warning-suppress-log-types '((comp) (bytecomp)))
(setq native-comp-async-report-warnings-errors 'silent)
(setq native-comp-async-query-on-exit t)
(setq inhibit-startup-echo-area-message (user-login-name))
(setq inhibit-splash-screen t)
(setq inhibit-startup-screen t)
(setq inhibit-startup-message t)
(setq initial-major-mode 'fundamental-mode)
(setq initial-scratch-message nil)
(setq frame-resize-pixelwise t)
(setq default-frame-alist '((fullscreen . maximized)
                            (vertical-scroll-bars . nil)
                            (horizontal-scroll-bars . nil)
                            (background-color . "#fff6e8")
                            (foreground-color . "#5f4400")
                            (ns-appearance . light)
                            (ns-transparent-titlebar . t)))

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode -1)

;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:

;;; early-init.el ends here
