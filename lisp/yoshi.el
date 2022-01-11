;; -*- lexical-binding: t; -*-

(require 'eglot)
(require 'flymake-eslint)
(require 'project-tools)

(defvar-local yoshi-local-project-overrides nil
  "Saves a list of overriden buffer variables so that they can be
clear when mode is turned off.")

(defmacro yoshi-set (&rest pairs)
  `(progn
     ,@(cl-loop for (name _) on pairs by #'cddr
                collect `(push ',name yoshi-local-project-overrides))
     (setq-local ,@pairs)))

(defun yoshi--setup-compilation (project)
  (let ((yoshi (or (project-has-node-script-p project "yoshi-flow-editor")
                   (project-has-node-script-p project "yoshi-flow-bm")
                   (error "Cannot find yoshi in %s." (cdr project))))
        (sled (or (project-has-node-script-p project "sled-test-runner")
                  (error "Cannot find sled in %s." (cdr project)))))
    (yoshi-set
     compilation-error-regexp-alist '(jest)
     compile-command (if (locate-dominating-file "." "sled.config")
                         (concat sled " local -f ")
                       (concat yoshi " test --color ")))))

(defun yoshi--setup-typescript (project)
  (yoshi-set
   eglot-stay-out-of '(flymake-diagnostic-functions)
   eglot-server-programs (list (list '(typescript-mode tsx-mode) "typescript-language-server" "--stdio" "--tsserver-path"
                                     (project-has-file-p project "node_modules/typescript/lib")))
   flymake-eslint-executable-name (project-has-node-script-p project "eslint")
   flymake-diagnostic-functions '(flymake-eslint--checker eglot-flymake-backend))
  (prettier-mode t)
  (eglot-ensure))

(define-minor-mode yoshi-project-mode "Yoshi for Emacs."
  :lighter " 🦖"
  (cond
   (yoshi-project-mode
    (when-let ((project (project-current)))
      (yoshi--setup-compilation project)
      (when (derived-mode-p 'typescript-mode)
        (yoshi--setup-typescript project))))
   (t
    (mapc #'kill-local-variable yoshi-local-project-overrides))))

(provide 'yoshi)
