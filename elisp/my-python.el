;;; my-python.el --- Python -*- lexical-binding: t -*-

;;; Commentary:

;; Python development configuration with pyvenv for virtual environment support.

;;; Code:

(use-package python
  :ensure nil
  :custom
  (python-indent-guess-indent-offset nil))

(use-package pyvenv
  :ensure t
  :preface
  (defun my-find-and-activate-venv ()
    "Find and activate .venv directory by scanning upward from current directory."
    (interactive)
    (let ((dir (expand-file-name default-directory))
          (venv-dir nil)
          (stopped-dir nil))
      (while (and dir (not (string= dir "/")) (not venv-dir))
        (let ((candidate (expand-file-name ".venv" dir)))
          (if (file-directory-p candidate)
              (setq venv-dir candidate)
            (if (or (file-exists-p (expand-file-name "pyproject.toml" dir))
                    (file-directory-p (expand-file-name ".git" dir)))
                (progn
                  (setq stopped-dir dir)
                  (setq dir nil))
              (setq dir (file-name-directory (directory-file-name dir)))))))
      (if venv-dir
          (progn
            (message "Venv found at %s, activating..." venv-dir)
            (pyvenv-activate venv-dir)
            (when pyvenv-virtual-env
              (message "Venv activated at %s" venv-dir)))
        (message "No venv found. Search started from %s and stopped at %s"
                 default-directory (or stopped-dir "/"))))))

;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:

(provide 'my-python)
;;; my-python.el ends here
