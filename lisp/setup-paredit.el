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
  ((ielm-mode . paredit-mode)
   (emacs-lisp-mode . paredit-mode)
   (lisp-mode . paredit-mode)
   (lisp-data-mode . paredit-mode)
   (eval-expression-minibuffer-setup . paredit-mode)
   (sly-mrepl-mode . paredit-mode))
  :config
  (define-key paredit-mode-map (kbd "C-w") #'paredit-backward-kill-word)
  (define-key paredit-mode-map (kbd "C-c [") #'paredit-forward-slurp-sexp)
  (define-key paredit-mode-map (kbd "C-c ]") #'paredit-forward-barf-sexp)

  (define-key paredit-mode-map (kbd "M-[") #'paredit-wrap-square)
  (define-key paredit-mode-map (kbd "M-{") #'paredit-wrap-curly)

  (define-key paredit-mode-map "{" #'paredit-open-curly)
  (define-key paredit-mode-map "[" #'paredit-open-square)

  (advice-add 'paredit-kill :around #'paredit-kill-dwim)
  (advice-add 'paredit-backward-kill-word :around #'paredit-kill-dwim)
  (advice-add 'paredit-forward-delete :around #'paredit-forward-delete-whitespace)
  (advice-add 'paredit-backward-delete :around #'paredit-backward-delete-whitespace))

(use-package paredit-everywhere
  :hook ((typescript-mode . paredit-everywhere-mode)
         (js-mode . paredit-everywhere-mode)))

(provide 'setup-paredit)
