;; -*- lexical-binding: t; -*-

(use-package org
  :mode ("\\.org\\'" . org-mode)
  :hook (org-mode . turn-on-show-trailing-whitespace)
  :hook (org-mode . visual-line-mode)
  :hook (org-mode . org-indent-mode)
  :hook (org-mode . flyspell-mode)
  :hook (org-mode . turn-off-local-electric-pair-mode)
  :config
  (org-babel-do-load-languages 'org-babel-load-languages
                               '((emacs-lisp . t) (shell . t) (lisp . t) (plantuml . t))))

(use-package org-tempo :after org)
(use-package org-modern :commands org-modern-mode)
(use-package org-writing :commands org-writing-mode)

(use-package org-download
  :commands org-download-screenshot
  :custom
  (org-download-method 'directory)
  (org-download-image-dir "images")
  (org-download-heading-lvl nil)
  (org-download-timestamp "%Y%m%d-%H%M%S_")
  (org-image-actual-width 300)
  (org-download-screenshot-method "pngpaste %s"))

(provide 'setup-org)
