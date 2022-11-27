;; -*- lexical-binding: t; -*-

(use-package dired
  :hook ((dired-mode . dired-hide-details-mode)
         (dired-mode . hl-line-mode))
  :bind (:map dired-mode-map
              ("i" . dired-subtree-insert)))

(use-package dired-subtree
  :commands (dired-subtree-insert))

(use-package dired-collapse
  :hook ((dired-mode . dired-collapse-mode)))

(provide 'setup-dired)
