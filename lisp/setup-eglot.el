;; -*- lexical-binding: t; -*-

(use-package eglot
  :commands eglot
  :config (setq eglot-stay-out-of '(flymake-diagnostic-functions)))

(provide 'setup-eglot)
