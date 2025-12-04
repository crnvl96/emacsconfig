(require 'org)

(let ((tangled-file (expand-file-name "init-tangled.el" user-emacs-directory)))
  (org-babel-tangle-file (expand-file-name "init.org" user-emacs-directory) tangled-file)
  (load tangled-file))
