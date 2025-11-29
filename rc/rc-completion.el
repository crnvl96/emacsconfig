;;; -*- lexical-binding: t; -*-

(setq completion-ignore-case t)

(use-package marginalia
  :ensure t
  :hook (after-init . marginalia-mode))

(use-package vertico
  :ensure t
  :hook (after-init . vertico-mode)
  :bind ( :map vertico-map
          ("<backspace>" . vertico-directory-delete-char)
          ("C-j" . vertico-next)
          ("C-k" . vertico-previous)
          ("C-w" . vertico-directory-delete-word )
          ("RET" . vertico-directory-enter)))

(use-package orderless
  :ensure t
  :config (setq completion-styles '(orderless partial-completion basic)
                completion-category-defaults nil
                completion-category-overrides '((eglot (styles orderless))
                                                (eglot-capf (styles orderless)))))

(use-package corfu
  :ensure t
  :hook
  (after-init . global-corfu-mode)
  (after-init . corfu-history-mode)
  (after-init . corfu-popupinfo-mode)
  (minibuffer-setup . my/corfu-enable-always-in-minibuffer)
  :init
  (defun my/corfu-enable-always-in-minibuffer ()
    "Enable Corfu in the minibuffer if Vertico/Mct are not active."
    (unless (or (bound-and-true-p mct--active)
                (bound-and-true-p vertico--input))
      (setq-local corfu-auto nil)
      (corfu-mode 1)))
  (setq read-extended-command-predicate #'command-completion-default-include-p
        text-mode-ispell-word-completion nil
        tab-always-indent 'complete)
  (setq tab-first-completion 'word-or-paren-or-punct)
  :config
  (setq corfu-cycle t)
  (setq corfu-preselect 'prompt)
  (keymap-set corfu-map "RET" `( menu-item "" nil :filter
                                 ,(lambda (&optional _)
                                    (and (derived-mode-p 'eshell-mode 'comint-mode)))))
  :bind ( :map corfu-map
          ("TAB" . corfu-next)
          ([tab] . corfu-next)
          ("S-TAB" . corfu-previous)
          ([backtab] . corfu-previous)))

(use-package cape
  :ensure t
  :init
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block)
  (add-to-list 'completion-at-point-functions #'cape-elisp-symbol)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev))

(provide 'rc-completion)
