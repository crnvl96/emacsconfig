 ;;; init.el --- Init -*- lexical-binding: t; -*-

(add-to-list 'load-path (expand-file-name "~/.emacs.d/rc/"))

;; Core settings
(require 'rc-delight)
(require 'rc-opts)
(require 'rc-fonts)
(require 'rc-keymaps)
(require 'rc-which-key)
(require 'rc-theme)

;; File management
(require 'rc-dired)
(require 'rc-trashed)
(require 'rc-treesit)

;; Navigation
(require 'rc-ace-window)
(require 'rc-avy)
(require 'rc-expand-region)

;; Editing
(require 'rc-editorconfig)
(require 'rc-mise)
(require 'rc-exec-path-from-shell)
(require 'rc-buffer-terminator)
(require 'rc-super-save)
(require 'rc-undo-fu)
(require 'rc-helpful)
(require 'rc-crux)

;; Reading
(require 'rc-nov)

;; Completion
(require 'rc-completion)
(require 'rc-consult)
(require 'rc-rg)

;; Version control
(require 'rc-magit)

;; Code formatting
(require 'rc-apheleia)

;; LSP
(require 'rc-eglot)

;; Languages
(require 'rc-python)
(require 'rc-json)
(require 'rc-go)
(require 'rc-elisp)
(require 'rc-markdown)
(require 'rc-org)
