;;; my-navigation.el --- Navigation & Search -*- lexical-binding: t -*-

;;; Commentary:

;; Navigation and search tools: consult, embark, avy, ace-window, and rg.

;;; Code:

(use-package consult
  :ensure t
  :init
  ;; Optionally configure the register formatting. This improves the register
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  :custom
  (consult-async-min-input 2)
  (consult-narrow-key "<")
  (consult-fd-args "fd --type f --hidden --follow --exclude .git")
  :config
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   :preview-key '(:debounce 0.4 "M-."))
  :bind (("C-c f f" . consult-projectile)
         ("C-c f F" . consult-fd)
         ("C-c f G" . consult-ripgrep)
         ("C-c f o" . consult-outline)
         ("C-c f k" . consult-flymake)
         ("C-c f l" . consult-line)
         ("C-c f ." . consult-goto-line)))

(use-package embark
  :ensure t
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none))))
  :bind (("C-."   . embark-act)
         ("C-;"   . embark-dwim)
         ("C-h B" . embark-bindings)))

(use-package embark-consult
  :ensure t
  :after (embark consult)
  :hook (embark-collect-mode . consult-preview-at-point-mode))

(use-package avy
  :ensure t
  :custom
  (avy-background t))

(use-package ace-window
  :ensure t
  :custom
  (aw-keys '(?h ?j ?k ?l))
  :bind (([remap other-window] . ace-window)
         ("M-o"                 . ace-window)))

(use-package rg
  :ensure t
  :bind ("C-c f g" . rg-menu))

;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:

(provide 'my-navigation)
;;; my-navigation.el ends here
