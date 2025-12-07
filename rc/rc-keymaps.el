;;; rc-keymaps.el --- Keymaps -*- lexical-binding: t; -*-

;;; Keymaps
(global-set-key (kbd "C-x ;") 'comment-or-uncomment-region)
(global-set-key (kbd "M-n") 'forward-paragraph)
(global-set-key (kbd "M-p") 'backward-paragraph)
(global-set-key (kbd "C-x C-b") #'ibuffer)

(defun my-split-window-vertically ()
  (interactive)
  (split-window-vertically) (other-window 1))
(global-set-key (kbd "C-x 2") #'my-split-window-vertically)

(defun my-split-window-horizontally ()
  (interactive)
  (split-window-horizontally) (other-window 1))
(global-set-key (kbd "C-x 3") #'my-split-window-horizontally)

(defun my-forward-word ()
  "Move forward to the next syntax change, like Vim word movement."
  (interactive)
  (let ((start-syntax (if (eobp) nil (char-syntax (char-after)))))
    (if start-syntax
        (progn
          (forward-char 1)
          (while (and (not (eobp)) (eq (char-syntax (char-after)) start-syntax))
            (forward-char 1)))
      (forward-char 1))))
(global-set-key (kbd "M-f") #'my-forward-word)


(defun my-backward-word ()
  "Move backward to the previous syntax change, like Vim word movement."
  (interactive)
  (let ((start-syntax (if (bobp) nil (char-syntax (char-before)))))
    (if start-syntax
        (progn
          (backward-char 1)
          (while (and (not (bobp)) (eq (char-syntax (char-before)) start-syntax))
            (backward-char 1)))
      (backward-char 1))))
(global-set-key (kbd "M-b") #'my-backward-word)

(provide 'rc-keymaps)
