;;; -*- lexical-binding: t; -*-

(use-package super-save
  :ensure t
  :delight
  :hook (after-init . super-save-mode)
  :init (setq auto-save-default nil)
  :config (setq super-save-auto-save-when-idle t
                super-save-delete-trailing-whitespace t
                super-save-all-buffers t)
  (add-to-list 'super-save-triggers 'ace-window)
  (add-to-list 'super-save-hook-triggers 'find-file-hook))

(provide 'rc-super-save)
