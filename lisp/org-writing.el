;; -*- lexical-binding: t; -*-

(defvar-local org-writing-face-remap-cookies nil)

(define-minor-mode org-writing-mode
  "Collection of settings for my writing needs."
  :global nil
  (cond
   (org-writing-mode
    (setq-local olivetti-body-width 90)
    (let ((cookies (list (face-remap-add-relative 'outline-1 '(:height 1.2 :weight bold))
                         (face-remap-add-relative 'outline-2 '(:height 1.1 :weight bold))
                         (face-remap-add-relative 'variable-pitch '(:height 1.4))
                         (face-remap-add-relative 'fixed-pitch '(:height 0.8)))))
      (setq-local org-writing-face-remap-cookies cookies))
    (variable-pitch-mode t)
    (when org-indent-mode
      (org-indent-mode -1))
    (olivetti-mode t)
    (flymake-proselint-setup)
    (flymake-mode t))
   (t
    (kill-local-variable 'olivetti-body-width)
    (variable-pitch-mode -1)
    (mapc #'face-remap-remove-relative org-writing-face-remap-cookies)
    (olivetti-mode -1)
    (flymake-mode -1))))

(provide 'org-writing)
