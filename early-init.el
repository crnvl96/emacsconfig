;;; early-init.el --- Early Init -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;;; Declutter the ~/emacs.d/ folder

(defvar cr-user-directory user-emacs-directory)
(setq user-emacs-directory (expand-file-name "var/" cr-user-directory))
(setq package-user-dir (expand-file-name "elpa" user-emacs-directory))

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; End:

;;; early-init.el ends here
