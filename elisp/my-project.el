;;; my-project.el --- Project management -*- lexical-binding: t -*-

;;; Commentary:

;; Project management with projectile and consult-projectile integration.

;;; Code:

(use-package projectile
  :ensure t
  :hook (elpaca-after-init . projectile-mode)
  :preface
  (defun my-insert-relative-file-name ()
    "Insert the current file's path relative to the project root."
    (interactive)
    (when-let ((fname (buffer-file-name (window-buffer (minibuffer-selected-window)))))
      (insert (file-relative-name fname (projectile-project-root)))))
  :custom
  (projectile-project-search-path '("~/Developer/work"
                                    "~/Developer/personal"
                                    "~/Developer/personal/dotfiles"
                                    "~/.emacs.d"
                                    "~/.config/nvim"))
  (projectile-cleanup-known-projects t)
  :bind-keymap ("C-x p" . projectile-command-map)
  :bind (:map projectile-mode-map
              ("C-c j"   . projectile-command-map)
              ("C-c j ." . my-insert-relative-file-name)))

(use-package consult-projectile
  :ensure t
  :after (consult projectile))

;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:

(provide 'my-project)
;;; my-project.el ends here
