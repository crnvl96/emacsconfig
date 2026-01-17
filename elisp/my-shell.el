;;; my-shell.el --- Shell -*- lexical-binding: t -*-

;;; Commentary:

;; Shell integration with vterm and exec-path-from-shell.

;;; Code:

(use-package vterm
  :ensure t)

(use-package exec-path-from-shell
  :ensure t
  :when (memq window-system '(mac ns x))
  :config
  (exec-path-from-shell-initialize))

;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:

(provide 'my-shell)
;;; my-shell.el ends here
