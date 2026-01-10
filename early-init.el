;;; early-init.el --- Early Init -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;;; Declutter the ~/emacs.d/ folder

(defvar crnvl-user-directory user-emacs-directory)
(setq user-emacs-directory (expand-file-name "var/" crnvl-user-directory))
(setq package-user-dir (expand-file-name "elpa" user-emacs-directory))
(setq package-enable-at-startup nil)

;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; byte-compile-warnings: (not obsolete free-vars)
;; End:

;;; early-init.el ends here
