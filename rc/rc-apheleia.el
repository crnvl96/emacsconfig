;;; -*- lexical-binding: t; -*-

(use-package apheleia
  :ensure t
  :delight
  :hook (prog-mode . apheleia-mode)
  :config
  (setf (alist-get 'python-ts-mode apheleia-mode-alist) '(ruff-isort ruff))
  (setf (alist-get 'go-ts-mode apheleia-mode-alist) '(gofumpt)))

(provide 'rc-apheleia)
