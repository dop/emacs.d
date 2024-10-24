;; -*- lexical-binding: t; -*-

(use-package flymake-proselint
  :commands flymake-proselint-setup)

(use-package flymake
  :pin gnu
  :hook ((typescript-mode . flymake-mode))
  :bind (:map flymake-mode-map ("C-x `" . flymake-goto-next-error)))

(use-package flymake-eslint :pin melpa)

(provide 'setup-flymake)
