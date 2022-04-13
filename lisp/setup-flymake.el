;; -*- lexical-binding: t; -*-

(defun flymake-diagnostics-fit-window-to-buffer (&rest ignore)
  (when (member major-mode '(flymake-project-diagnostics-mode flymake-diagnostics-buffer-mode))
    (fit-window-to-buffer (get-buffer-window (current-buffer)) 10 5)))

(use-package flymake-proselint
  :commands flymake-proselint-setup)

(use-package flymake
  :pin gnu
  :hook ((typescript-mode . flymake-mode))
  :bind (:map flymake-mode-map ("C-x `" . flymake-goto-next-error))
  :config
  (with-eval-after-load 'tabulated-list
    (advice-add 'tabulated-list-print :after #'flymake-diagnostics-fit-window-to-buffer)))

(provide 'setup-flymake)
