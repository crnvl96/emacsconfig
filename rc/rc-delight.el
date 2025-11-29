;;; -*- lexical-binding: t; -*-

(use-package delight
  :ensure t
  :demand t
  :config
  (delight 'whitespace-mode nil "whitespace")
  (delight 'which-key-mode nil "which-key")
  (delight 'devil-mode nil "devil")
  (delight 'visual-line-mode nil "simple")
  (delight 'eldoc-mode nil "eldoc"))

(provide 'rc-delight)
