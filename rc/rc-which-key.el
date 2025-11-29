;;; -*- lexical-binding: t; -*-

(which-key-add-key-based-replacements
  "C-x p" "Project"
  "C-c c" "Crux"
  "C-c f" "Find")

(add-hook 'after-init-hook #'which-key-mode)

(provide 'rc-which-key)
