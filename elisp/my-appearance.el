;;; my-appearance.el --- Appearance & Theme -*- lexical-binding: t -*-

;;; Commentary:

;; Visual appearance configuration: ef-themes and delight for mode-line cleanup.

;;; Code:

(use-package ef-themes
  :ensure t
  :custom
  (ef-themes-mixed-fonts t)
  (ef-themes-variable-pitch-ui nil)
  :config
  (mapc #'disable-theme custom-enabled-themes)
  (load-theme 'ef-melissa-light t))

(use-package delight
  :ensure t
  :config
  (delight '((eldoc-mode nil "eldoc")
             (apheleia-mode nil "apheleia")
             (projectile-mode nil "projectile")
             (global-anzu-mode nil "anzu")
             (whitespace-mode nil "whitespace")
             (emacs-lisp-mode "Elisp" :major))))

;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:

(provide 'my-appearance)
;;; my-appearance.el ends here
