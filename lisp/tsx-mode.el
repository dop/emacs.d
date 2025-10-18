;; -*- lexical-binding: t; -*-
(define-derived-mode tsx-mode typescript-mode "tsx")
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-mode))

(provide 'tsx-mode)
