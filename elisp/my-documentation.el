;;; my-documentation.el --- Help & Docs -*- lexical-binding: t -*-

;;; Commentary:

;; Enhanced help system with helpful package.

;;; Code:

(use-package helpful
  :ensure t
  :custom
  (helpful-max-buffers 3)
  :bind (([remap describe-command]  . helpful-command)
         ([remap describe-function] . helpful-callable)
         ([remap describe-key]      . helpful-key)
         ([remap describe-symbol]   . helpful-symbol)
         ([remap describe-variable] . helpful-variable)))

;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:

(provide 'my-documentation)
;;; my-documentation.el ends here
