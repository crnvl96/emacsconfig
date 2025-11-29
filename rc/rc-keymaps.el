;;; -*- lexical-binding: t; -*-

(global-set-key (kbd "C-x ;") 'comment-or-uncomment-region)
(global-set-key (kbd "M-n") 'forward-paragraph)
(global-set-key (kbd "M-p") 'backward-paragraph)
(global-set-key (kbd "C-x 2") (lambda () (interactive) (split-window-vertically) (other-window 1)))
(global-set-key (kbd "C-x 3") (lambda () (interactive) (split-window-horizontally) (other-window 1)))

(defun insert-previous-file-name ()
  (interactive)
  (let ((file-name (with-current-buffer (window-buffer (minibuffer-selected-window))
                     (file-name-nondirectory (buffer-file-name)))))
    (insert file-name)))

(define-key minibuffer-local-map (kbd "C-f") 'insert-previous-file-name)

(provide 'rc-keymaps)
