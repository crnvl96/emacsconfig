;;; my-version-control.el --- Version Control -*- lexical-binding: t -*-

;;; Commentary:

;; Version control with magit and transient.

;;; Code:

(use-package magit
  :ensure t
  :defer t)

(use-package transient
  :ensure t)

;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:

(provide 'my-version-control)
;;; my-version-control.el ends here
