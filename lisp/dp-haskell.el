;;; Haskell

(use-package haskell-cabal-mode
  :mode "\\.cabal$"
  :config (add-hook 'haskel-cabal-mode-hook
                    (lambda ()
                      (setq indent-tabs-mode nil))))

(use-package haskell-mode
  :mode "\\.l?hs$"
  :config (progn
            (bind-keys :map haskell-mode-map
                       ("C-c m" . haskell-align-imports)
                       ("C-c s" . haskell-sort-imports))
            (setq haskell-program-name "ghci" haskell-indent-offset 4)
            (add-hook 'haskell-mode-hook 'capitalized-words-mode)
            (add-hook 'haskell-mode-hook 'haskell-indentation-mode)
            (add-hook 'haskell-mode-hook 'inf-haskell-mode)))

(define-project-type haskell (generic)
  (look-for ".haskell")
  :config-file ".haskell"
  :relevant-files ("\\.l?hs$" "\\.hsc$" "\\.cabal$"))

(defun haskell-eproject-extensions ()
  (when (eproject-attribute :extensions)
    (eval `(concat ,@(mapcar (lambda (extension)
                               (concat "-X" extension " "))
                             (eproject-attribute :extensions))))))

(defun haskell-eproject-setup ()
  "Setup haskell project."
  (let ((pkg-db (eproject-attribute :package-db))
        (src-dir (eproject-attribute :src-dir))
        (usr-pkgs (eproject-attribute :user-packages)))
    (setq haskell-program-name
          (concat "ghci -cpp "
                  (haskell-eproject-extensions)
                  (and src-dir (concat "-i" src-dir " "))
                  (or usr-pkgs "-no-user-package-conf ")
                  (and pkg-db (concat "-package-conf " (eproject-root) pkg-db)))))
  ;; some key bindings
  (local-set-key "\C-c\C-r" 'inferior-haskell-reload-and-run-main))

(provide 'dp-haskell)
