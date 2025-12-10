;;; early-init.el --- Early Init -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;;; Declutter the ~/emacs.d/ folder

(setq user-emacs-directory (expand-file-name "var/" user-emacs-directory))
(setq package-user-dir (expand-file-name "elpa" user-emacs-directory))
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(setq save-place-file (expand-file-name "saveplace" user-emacs-directory))
(setq savehist-file (expand-file-name "savehist" user-emacs-directory))
(setq recentf-save-file (expand-file-name "recentf" user-emacs-directory))

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; End:

;;; early-init.el ends here
