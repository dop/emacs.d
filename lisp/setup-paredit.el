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
  :hook (lisp-data-mode . paredit-mode)
  :hook (inferior-emacs-lisp-mode . repl-paredit-mode)
  :hook (sly-mrepl-mode . repl-paredit-mode)
  :hook (eval-expression-minibuffer-setup . repl-paredit-mode)
  :bind (:map paredit-mode-map
              ("C-j" . paredit-newline)
              ("RET" . paredit-newline)
              ("C-w" . paredit-backward-kill-word)
              ("C-c [" . paredit-forward-slurp-sexp)
              ("C-c ]" . paredit-forward-barf-sexp)
              ("M-[" . paredit-wrap-square)
              ("M-{" . paredit-wrap-curly)
              ("{" . paredit-open-curly)
              ("[" . paredit-open-square))
  :config
  (advice-add 'paredit-kill :around #'paredit-kill-dwim)
  (advice-add 'paredit-backward-kill-word :around #'paredit-kill-dwim)
  (advice-add 'paredit-forward-delete :around #'paredit-forward-delete-whitespace)
  (advice-add 'paredit-backward-delete :around #'paredit-backward-delete-whitespace)
  ;; Make sure electric pair mode is turned off. I usually add it to all prog and text modes.
  (add-hook 'paredit-mode-hook #'turn-off-local-electric-pair-mode)
  (define-minor-mode repl-paredit-mode "Paredit in the REPL."
    :keymap (let ((map (copy-keymap paredit-mode-map)))
              (keymap-unset map "RET" t)
              (keymap-unset map "<return>" t)
              (keymap-set map "C-j" 'paredit-newline)
              map)
    (turn-off-local-electric-indent-mode)))

(use-package paredit-everywhere
  :hook ((typescript-mode . paredit-everywhere-mode)
         (js-mode . paredit-everywhere-mode)
         (js-ts-mode . paredit-everywhere-mode)
         (typescript-ts-base-mode . paredit-everywhere-mode)
         (typescript-ts-base-mode . electric-pair-mode))
  :bind (:map paredit-everywhere-mode-map
              ("C-k" . paredit-kill)
              ("M-[" . paredit-wrap-square)
              ("M-{" . paredit-wrap-curly)))

(provide 'setup-paredit)
