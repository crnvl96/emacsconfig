;;; -*- lexical-binding: t; -*-

(use-package dired
  :ensure nil
  :commands (dired)
  :hook
  ((dired-mode . dired-hide-details-mode)
   (dired-mode . hl-line-mode))
  :config
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'always)
  (setq delete-by-moving-to-trash t)
  (setq dired-dwim-target t)

  (setq dired-free-space nil
        dired-dwim-target t  ; Propose a target for intelligent moving/copying
        dired-deletion-confirmer 'y-or-n-p
        dired-filter-verbose nil
        dired-vc-rename-file t
        dired-create-destination-dirs 'ask
        ;; Suppress Dired buffer kill prompt for deleted dirs
        dired-clean-confirm-killing-deleted-buffers nil))

(provide 'rc-dired)
