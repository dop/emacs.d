;; -*- lexical-binding: t; -*-

(use-package org
  :mode ("\\.org\\'" . org-mode)
  :config
  (require 'org-tempo)
  (dolist (fn (list #'turn-on-show-trailing-whitespace
                    #'visual-line-mode
                    #'org-indent-mode
                    #'flyspell-mode
                    #'turn-off-local-electric-pair-mode))
    (add-hook 'org-mode-hook fn))
  (org-babel-do-load-languages 'org-babel-load-languages
                               '((emacs-lisp . t) (shell . t) (lisp . t) (plantuml . t))))

(use-package org-writing :commands org-writing-mode)

(provide 'setup-org)
