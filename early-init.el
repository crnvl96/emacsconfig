;;; early-init.el --- Early Init -*- lexical-binding: t; -*-

;;; Commentary:

;; This file is loaded before init.el, before package and UI initialization.
;; Use it for settings that must be applied before the GUI is drawn or
;; packages are loaded to ensure a fast, flicker-free startup.

;;; Code:

(defvar my-user-directory user-emacs-directory
  "The original user-emacs-directory before redirection.")

(setq user-emacs-directory (expand-file-name "var/" my-user-directory))
(setq package-user-dir (expand-file-name "elpa" user-emacs-directory))

;; Number of bytes of consing between garbage collections.
;;
;; Garbage collection can happen automatically once this many bytes have been
;; allocated since the last garbage collection.  All data types count.
;;
;; By binding this temporarily to a large number, you can effectively
;; prevent garbage collection during a part of the program.
(setq gc-cons-threshold most-positive-fixnum)

;; Portion of the heap used for allocation.
;;
;; Garbage collection can happen automatically once this portion of the heap
;; has been allocated since the last garbage collection.
;;
;; By binding this temporarily to a large number, you can effectively
;; prevent garbage collection during a part of the program.
(setq gc-cons-percentage 0.6)

;; Maximum number of bytes to read from subprocess in a single chunk.
(setq read-process-output-max (* 4 1024 1024))

;; List of warnings that the byte-compiler should issue.
;;
;; Obsolete: obsolete variables and functions.
;;
;; If the list begins with not, then the remaining elements specify warnings to
;; suppress.  For example, (not free-vars) will suppress the free-vars warning.
(setq byte-compile-warnings '(not obsolete))

;; List of warning types that should not be logged.
(setq warning-suppress-log-types '((comp) (bytecomp)))

;; When native compilation happens asynchronously, it can produce
;; warnings and errors, some of which might not be emitted by a
;; byte-compilation.  The typical case for that is native-compiling
;; a file that is missing some require of a necessary feature,
;; while having it already loaded into the environment when
;; byte-compiling.
;;
;; Set this variable to nil to suppress warnings altogether, or to
;; the symbol silent to log warnings but not pop up the *Warnings*
;; buffer.
(setq native-comp-async-report-warnings-errors 'silent)

;; Whether to query the user about killing async compilations when exiting.
;;
;; If this is non-nil, Emacs will ask for confirmation to exit and kill the
;; asynchronous native compilations if any are running.
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
