;;; -*- lexical-binding: t; -*-

(use-package avy
  :ensure t
  :config
  (setq avy-keys (number-sequence ?a ?y))
  :bind (("M-i" . avy-goto-char-2)
         ("M-e" . avy-goto-word-0)))

(provide 'rc-avy)
