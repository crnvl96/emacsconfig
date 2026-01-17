;;; my-completion.el --- Completion -*- lexical-binding: t -*-

;;; Commentary:

;; Completion framework: vertico, orderless, marginalia, corfu, and cape.

;;; Code:

(use-package vertico
  :ensure t
  :hook (elpaca-after-init . vertico-mode)
  :custom
  (vertico-cycle t)
  :config
  (vertico-multiform-mode)
  (dolist (category '((embark-keybinding grid)
                      (jinx grid (vertico-grid-annotate . 20) (vertico-count . 4))))
    (add-to-list 'vertico-multiform-categories category))
  :bind (:map vertico-map
              ("<backspace>" . vertico-directory-delete-char)
              ("C-w"         . vertico-directory-delete-word)
              ("RET"         . vertico-directory-enter)))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion))
                                   (embark-keybinding (styles flex))
                                   (eglot (styles orderless))
                                   (eglot-capf (styles orderless))))
  (completion-pcm-leading-wildcard t))

(use-package marginalia
  :ensure t
  :hook (elpaca-after-init . marginalia-mode))

(use-package corfu
  :ensure t
  :hook (elpaca-after-init . global-corfu-mode)
  :custom
  (corfu-cycle t)
  :config
  (corfu-popupinfo-mode 1))

(use-package cape
  :ensure t
  :init
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-symbol))

;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:

(provide 'my-completion)
;;; my-completion.el ends here
