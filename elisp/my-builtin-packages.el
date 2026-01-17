;;; my-builtin-packages.el --- Builtin Packages -*- lexical-binding: t -*-

;;; Commentary:

;; Configuration for built-in Emacs packages: emacs, autorevert, whitespace,
;; dired, compile, and treesit.

;;; Code:

(use-package emacs
  :ensure nil
  :preface
  (defun my-scroll-window-halfway-down ()
    "Scroll window down by half of the total window height."
    (interactive)
    (scroll-up (/ (window-height) 2)))

  (defun my-scroll-window-halfway-up ()
    "Scroll window up by half of the total window height."
    (interactive)
    (scroll-down (/ (window-height) 2)))

  (defun my-toggle-window-split ()
    "Toggle between horizontal and vertical window split."
    (interactive)
    (when (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (when this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (when this-win-2nd (other-window 1))))))

  :config
  ;; Suppress certain buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*\\(Warnings\\|Compile-Log\\)\\*\\'"
                 (display-buffer-no-window)
                 (allow-no-window . t)))

  :bind (         ("C-v" . my-scroll-window-halfway-down)
         ("M-v" . my-scroll-window-halfway-up)))

(use-package autorevert
  :ensure nil
  :hook (elpaca-after-init . global-auto-revert-mode)
  :custom
  (auto-revert-avoid-polling t)
  (auto-revert-interval 2)
  (auto-revert-check-vc-info t))

(use-package whitespace
  :ensure nil
  :hook ((elpaca-after-init . global-whitespace-mode)
         (before-save . whitespace-cleanup))
  :custom
  (whitespace-style '(face tabs empty trailing)))

(use-package dired
  :ensure nil
  :bind (:map dired-mode-map
              ("-" . dired-up-directory)))

(use-package compile
  :ensure nil
  :preface
  (require 'ansi-color)
  (defun my-colorize-compilation ()
    "Apply ANSI color codes in compilation buffer."
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region compilation-filter-start (point))))
  :hook (compilation-filter . my-colorize-compilation)
  :bind ("C-c `" . compile))

(use-package treesit
  :ensure nil
  :preface
  (defun my-treesit-install-all-languages ()
    "Install all languages specified by `treesit-language-source-alist'."
    (interactive)
    (dolist (lang (mapcar #'car treesit-language-source-alist))
      (treesit-install-language-grammar lang)
      (message "`%s' parser was installed." lang)
      (sit-for 0.75)))

  :custom
  (treesit-font-lock-level 4)

  :config
  ;; Language grammar sources
  (setq treesit-language-source-alist
        '((css        . ("https://github.com/tree-sitter/tree-sitter-css" "v0.20.0"))
          (go         . ("https://github.com/tree-sitter/tree-sitter-go" "v0.20.0"))
          (html       . ("https://github.com/tree-sitter/tree-sitter-html" "v0.20.1"))
          (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript" "v0.20.1" "src"))
          (json       . ("https://github.com/tree-sitter/tree-sitter-json" "v0.20.2"))
          (markdown   . ("https://github.com/ikatyang/tree-sitter-markdown" "v0.7.1"))
          (python     . ("https://github.com/tree-sitter/tree-sitter-python" "v0.20.4"))
          (rust       . ("https://github.com/tree-sitter/tree-sitter-rust" "v0.21.2"))
          (toml       . ("https://github.com/tree-sitter/tree-sitter-toml" "v0.5.1"))
          (tsx        . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "tsx/src"))
          (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "typescript/src"))
          (yaml       . ("https://github.com/ikatyang/tree-sitter-yaml" "v0.5.0"))))

  ;; Remap traditional modes to tree-sitter modes
  (dolist (mapping '((c-mode          . c-ts-mode)
                     (conf-toml-mode  . toml-ts-mode)
                     (css-mode        . css-ts-mode)
                     (go-mode         . go-ts-mode)
                     (js-json-mode    . json-ts-mode)
                     (js2-mode        . js-ts-mode)
                     (json-mode       . json-ts-mode)
                     (python-mode     . python-ts-mode)
                     (typescript-mode . typescript-ts-mode)
                     (bash-mode       . bash-ts-mode)))
    (add-to-list 'major-mode-remap-alist mapping))

  ;; File associations for tree-sitter modes
  (dolist (mapping '(("\\.go\\'"     . go-ts-mode)
                     ("\\.jsonc?\\'" . json-ts-mode)
                     ("\\.m?jsx?\\'" . js-ts-mode)
                     ("\\.tsx?\\'"   . js-ts-mode)
                     ("\\.ya?ml\\'"  . yaml-ts-mode)))
    (add-to-list 'auto-mode-alist mapping)))

;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:

(provide 'my-builtin-packages)
;;; my-builtin-packages.el ends here
