;;; early-init.el --- Early Init -*- lexical-binding: t; -*-

(menu-bar-mode -1)
(tool-bar-mode -1)

(package-initialize)
(unless (package-installed-p 'use-package)
  (unless (seq-empty-p package-archive-contents)
    (package-refresh-contents))
  (package-install 'use-package))
(require 'use-package)
(setq package-archives '( ("melpa"        . "https://melpa.org/packages/")
                          ("gnu"          . "https://elpa.gnu.org/packages/")
                          ("nongnu"       . "https://elpa.nongnu.org/nongnu/")
                          ("melpa-stable" . "https://stable.melpa.org/packages/"))
      package-archive-priorities '( ("melpa"        . 90)
                                    ("gnu"          . 70)
                                    ("nongnu"       . 60)
                                    ("melpa-stable" . 50)))
