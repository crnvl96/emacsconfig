;;; -*- lexical-binding: t; -*-

(use-package typst-ts-mode
  :vc (:url "https://codeberg.org/meow_king/typst-ts-mode.git")
  :ensure t
  :mode (("\\.typ\\'" . typst-ts-mode))
  :config (setq typst-ts-watch-options "--open"
                typst-ts-mode-enable-raw-blocks-highlight t
                typst-ts-mode-grammar-location (expand-file-name "tree-sitter/libtree-sitter-typst.so" user-emacs-directory))
  :bind (:map typst-ts-mode-map
              ("C-c C-c" . typst-ts-menu)))

(provide 'rc-typst)
