;;; -*- lexical-binding: t; -*-

(use-package crux
  :ensure t
  :demand t
  :bind (([remap move-beginning-of-line] . crux-move-beginning-of-line)
         ([remap kill-whole-line] . crux-kill-whole-line)
         ([remap keyboard-quit]  . crux-keyboard-quit-dwin)
         ([remap upcase-region] . crux-upcase-region)
         ([remap downcase-region] . crux-downcase-region)
         ("C-c c j" . crux-top-join-line)
         ("C-c c n" . crux-cleanup-buffer-or-region)
         ("C-c c t" . crux-visit-term-buffer)
         ("C-c c d" . crux-duplicate-current-line-or-region)
         ("C-k" . crux-smart-kill-line)
         ("S-<return>" . crux-smart-open-line)
         ("C-S-<return>" . crux-smart-open-line-above)
         ("C-c c o" . crux-open-with)
         ("C-c c u" . crux-view-url)
         ("C-c c k" . crux-kill-other-buffers))
  :config
  (crux-with-region-or-line comment-or-uncomment-region)
  (crux-with-region-or-sexp-or-line kill-region)
  (crux-with-region-or-point-to-eol kill-ring-save))

(provide 'rc-crux)
