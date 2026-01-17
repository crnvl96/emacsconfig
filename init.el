;;; init.el --- Personal Emacs Configuration -*- lexical-binding: t -*-

;; Add elisp directory to load-path for require
(add-to-list 'load-path (expand-file-name "elisp" my-user-directory))

(require 'my-elpaca)
(require 'my-core)
(require 'my-fonts)
(require 'my-builtin-packages)
(require 'my-appearance)
(require 'my-completion)
(require 'my-navigation)
(require 'my-editor)
(require 'my-project)
(require 'my-version-control)
(require 'my-documentation)
(require 'my-shell)
(require 'my-programming)
(require 'my-python)

;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:

;;; init.el ends here
