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
              ("r" . dired-subtree-remove)
              (")" . dired-git-info-mode))
  :init (require 'ls-lisp))

(use-package dired-subtree
  :commands (dired-subtree-insert))

(use-package dired-collapse
  :disabled t
  :hook ((dired-mode . dired-collapse-mode)))

(use-package dired-auto-readme
  :disabled t
  :hook ((dired-mode . dired-auto-readme-mode)))

(use-package dired-git-info
  :commands dired-git-info-mode
  ;; :hook ((dired-after-readin . dired-git-info-auto-enable))
  :config (setq dgi-auto-hide-details-p nil))

(provide 'setup-dired)
