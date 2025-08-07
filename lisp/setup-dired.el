;; -*- lexical-binding: t; -*-

(defun ffplay (file)
  "Play file under point."
  (interactive (list (dired-copy-filename-as-kill)))
  (async-shell-command (format "ffplay -infbuf \"%s\"" file)
                       (format "*ffplay %s*" file)))

(use-package dired
  :hook ((dired-mode . dired-hide-details-mode)
         (dired-mode . hl-line-mode))
  :bind (:map dired-mode-map
              ("i" . dired-subtree-insert)
              ("r" . dired-subtree-remove))
  :init (require 'ls-lisp))

(use-package dired-subtree
  :commands (dired-subtree-insert))

(use-package dired-collapse
  :disabled t
  :hook ((dired-mode . dired-collapse-mode)))

(use-package dired-auto-readme
  :hook ((dired-mode . dired-auto-readme-mode)))

(use-package dired-git-info
  :hook ((dired-after-readin . dired-git-info-auto-enable)))

(provide 'setup-dired)
