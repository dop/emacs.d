(defvar whitespace-characters
  '(?\n ?\r ?\t ?\ ))

(defun whitespace-char-p (c)
  (member c whitespace-characters))

(defun paredit-kill-dwim (fn &rest args)
  (if (use-region-p)
      (call-interactively #'kill-region)
    (apply fn args)))

(defun paredit-forward-delete-whitespace (fn &rest args)
  (if (whitespace-char-p (char-after))
      (while (whitespace-char-p (char-after))
        (delete-char 1))
    (apply fn args)))

(defun paredit-backward-delete-whitespace (fn &rest args)
  (if (whitespace-char-p (char-before))
      (while (whitespace-char-p (char-before))
        (delete-char -1))
    (apply fn args)))

(use-package paredit
  :commands paredit-mode
  :hook
  ((inferior-emacs-lisp-mode . paredit-mode)
   (lisp-data-mode . paredit-mode)
   (sly-mrepl-mode . repl-paredit-mode)
   (eval-expression-minibuffer-setup . repl-paredit-mode))
  :config

  (keymap-set paredit-mode-map "C-j" 'paredit-newline)
  (keymap-set paredit-mode-map "RET" 'paredit-newline)

  (keymap-set paredit-mode-map "C-w" 'paredit-backward-kill-word)
  (keymap-set paredit-mode-map "C-c [" 'paredit-forward-slurp-sexp)
  (keymap-set paredit-mode-map "C-c ]" 'paredit-forward-barf-sexp)

  (keymap-set paredit-mode-map "M-[" 'paredit-wrap-square)
  (keymap-set paredit-mode-map "M-{" 'paredit-wrap-curly)

  (keymap-set paredit-mode-map "{" 'paredit-open-curly)
  (keymap-set paredit-mode-map "[" 'paredit-open-square)

  (advice-add 'paredit-kill :around #'paredit-kill-dwim)
  (advice-add 'paredit-backward-kill-word :around #'paredit-kill-dwim)
  (advice-add 'paredit-forward-delete :around #'paredit-forward-delete-whitespace)
  (advice-add 'paredit-backward-delete :around #'paredit-backward-delete-whitespace)

  (define-minor-mode repl-paredit-mode "Paredit in the REPL."
    :keymap (let ((map (copy-keymap paredit-mode-map)))
              (keymap-unset map "RET" t)
              (keymap-unset map "<return>" t)
              (keymap-set map "C-j" 'paredit-newline)
              map)
    (turn-off-local-electric-indent-mode)))

(use-package paredit-everywhere
  :hook ((typescript-mode . paredit-everywhere-mode)
         (js-mode . paredit-everywhere-mode)))

(provide 'setup-paredit)
