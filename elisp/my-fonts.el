;;; my-fonts.el --- Fonts config -*- lexical-binding: t -*-

;;; Commentary:

;; Font configuration for default, fixed-pitch, and variable-pitch faces.

;;; Code:

(let ((mono-font "HackNerdFontMono")
      (prop-font "HackNerdFontPropo"))
  (set-face-attribute 'default nil
                      :family mono-font
                      :height 110
                      :weight 'regular)
  (set-face-attribute 'fixed-pitch nil
                      :family mono-font
                      :height 1.0
                      :weight 'regular)
  (set-face-attribute 'variable-pitch nil
                      :family prop-font
                      :height 1.0
                      :weight 'regular))

;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:

(provide 'my-fonts)
;;; my-fonts.el ends here
