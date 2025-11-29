;;; -*- lexical-binding: t; -*-

(use-package buffer-terminator
  :ensure t
  :delight
  :hook (after-init . buffer-terminator-mode)
  :config (setq buffer-terminator-verbose nil
                buffer-terminator-inactivity-timeout (* 20 60)
                buffer-terminator-interval (* 20 60)))

(provide 'rc-buffer-terminator)
