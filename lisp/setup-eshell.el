;; -*- lexical-binding: t; -*-

(defvar my-eshell-prompt-limit 30)

(defun my-eshell-prompt ()
  (let ((path (abbreviate-file-name (eshell/pwd)))
        (limit my-eshell-prompt-limit)
        (limited nil)
        (project (project-current)))
    (let* ((shortened-path
            (if (> (length path) my-eshell-prompt-limit)
                (concat "…" (substring path (- (length path) my-eshell-prompt-limit) (length path)))
              path))
           (branch
            (car (vc-git-branches)))
           (suffix ?$)
           (prompt
            (with-output-to-string
              (princ shortened-path)
              (when branch (princ " ") (princ branch))
              (princ (format " %c " suffix)))))
      (when branch
        (let ((pos (1+ (cl-search (format " %s " branch) prompt))))
          (put-text-property pos (+ pos (length branch)) 'face '(eshell-prompt italic) prompt)))
      (put-text-property (1+ (cl-position suffix prompt)) (length prompt) 'face '(default) prompt)
      prompt)))

(use-package eshell
  :defer t
  :config (require 'vc-git))

(add-hook 'eshell-mode-hook #'toggle-truncate-lines)
(add-hook 'eshell-mode-hook #'set-hbar-cursor-type)

(provide 'setup-eshell)
