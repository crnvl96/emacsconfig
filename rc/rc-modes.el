;;; rc-modes.el --- Lang modes -*- lexical-binding: t; -*-

;;; Org-mode
(use-package org
  :ensure nil
  :delight (org-indent-mode "" "org-indent")
  :commands (org-mode org-version)
  :mode (("\\.org\\'" . org-mode))
  :hook (org-mode . org-indent-mode)
  :config
  (setq org-M-RET-may-split-line '((default . nil))
        org-insert-heading-respect-content t
        org-log-done 'time
        org-log-into-drawer t
        org-directory "~/Developer/personal/notes/agenda/"
        org-agenda-files (list org-directory)
        org-todo-keywords '((sequence "TODO(t)" "WAIT(w!)" "|" "CANCEL(c!)" "DONE(d!)"))
        org-confirm-babel-evaluate nil)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '( (emacs-lisp . t)
      (python . t)
      (shell . t)
      (js . t)
      (d2 . t)
      (sql . t)))
  :bind
  ("C-c a" . org-agenda))

;; Ob-d2 (Org babel for D2)
(use-package ob-d2
  :ensure t
  :defer t
  :vc ( :url "https://github.com/dmacvicar/ob-d2"
        :rev :newest))

;;; Lua-mode
(use-package lua-mode
  :ensure t
  :defer t)

;;; Elisp-mode
(use-package elisp-mode
  :ensure nil
  :delight (emacs-lisp-mode "Elisp" :major))

;;; Markdown-mode
(use-package markdown-mode
  :ensure t
  :commands (gfm-mode
             gfm-view-mode
             markdown-mode
             markdown-view-mode)
  :mode (("\\.markdown\\'" . markdown-mode)
         ("\\.md\\'" . markdown-mode)
         ("README\\.md\\'" . gfm-mode)))

;;; Json-mode
(use-package json-mode
  :ensure t
  :mode (("\\.json\\'" . json-mode)))

;;; D2-mode
(use-package d2-mode
  :ensure t
  :defer t)

;;; Go-mode
(use-package go-mode
  :ensure t
  :defer t)

;;; Typst-ts-mode
(use-package
  typst-ts-mode
  :vc ( :url "https://codeberg.org/meow_king/typst-ts-mode.git"
        :rev :newest)
  :mode (("\\.typ\\'" . typst-ts-mode))
  :config
  (setq typst-ts-watch-options "--open")
  (setq typst-ts-mode-grammar-location (expand-file-name "tree-sitter/libtree-sitter-typst.so" user-emacs-directory))
  (setq typst-ts-mode-enable-raw-blocks-highlight t)
  :bind ( :map typst-ts-mode-map
          ("C-c C-c" . typst-ts-tmenu)))

(provide 'rc-modes)
