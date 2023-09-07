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

(use-package org-download
  :after org
  :commands (org-download-screenshot)
  :custom
  (org-download-method 'directory)
  (org-download-image-dir "images")
  (org-download-heading-lvl nil)
  (org-download-timestamp "%Y%m%d-%H%M%S_")
  (org-image-actual-width 300)
  (org-download-screenshot-method "pngpaste %s")
  ;; :bind
  ;; ("C-M-y" . org-download-screenshot)
  ;; :config
  ;; (require 'org-download)
  )

(provide 'setup-org)
