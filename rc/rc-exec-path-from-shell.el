;;; -*- lexical-binding: t; -*-

(use-package exec-path-from-shell
  :ensure t
  :demand t
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(provide 'rc-exec-path-from-shell)
