;;; early-init.el --- Early Init -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;;; Declutter the ~/emacs.d/ folder

(setq user-emacs-directory (expand-file-name "var/" user-emacs-directory))
(setq package-user-dir (expand-file-name "elpa" user-emacs-directory))
(setq inhibit-compacting-font-caches t)

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; End:

;;; early-init.el ends here
