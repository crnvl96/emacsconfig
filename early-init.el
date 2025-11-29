;;; early-init.el --- Early Init -*- lexical-binding: t; -*-

(add-to-list 'display-buffer-alist
             '("\\`\\*\\(Warnings\\|Compile-Log\\)\\*\\'"
               (display-buffer-no-window)
               (allow-no-window . t)))

(defvar user-data-directory user-emacs-directory
  "Directory beneath which Emacs data files are placed.")

(setq user-emacs-directory (expand-file-name "var/" user-data-directory))
(setq package-user-dir (expand-file-name "elpa" user-emacs-directory))
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(setq package-quickstart-file (expand-file-name "package-quickstart.el" user-emacs-directory))

(setq inhibit-splash-screen t)
(setq menu-bar-mode nil)
(setq tool-bar-mode nil)
(setq scroll-bar-mode nil)
(setq tooltip-mode nil)
(setq use-file-dialog nil)
(setq use-dialog-box nil)

(setq use-package-expand-minimally t)
(setq package-enable-at-startup t)
(setq package-archives '(("melpa"        . "https://melpa.org/packages/")
                         ("gnu"          . "https://elpa.gnu.org/packages/")
                         ("nongnu"       . "https://elpa.nongnu.org/nongnu/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")))
(setq package-archive-priorities '(("melpa"        . 90)
                                   ("gnu"          . 70)
                                   ("nongnu"       . 60)
                                   ("melpa-stable" . 50)))
