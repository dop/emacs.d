;; -*- lexical-binding: t; -*-

(require 'project-tools)

(defmacro with-npm-project (expr &rest body)
  (declare (indent 1))
  (cond
   ((null expr)        `(when (project-npm-p) ,@body))
   ((consp (car expr)) `(when (project-npm-p) (when-let ,expr ,@body)))
   (t                  `(when (and (project-npm-p) ,expr) ,@body))))

(defun yoshi-prettier ()
  (with-npm-project (project-has-node-script-p "prettier")
    (prettier-mode t)))

(defun yoshi-eslint ()
  (with-npm-project ((eslint (project-has-node-script-p "eslint")))
    (setq-local flymake-eslint-executable-name eslint)
    (flymake-eslint-enable)))

(defun yoshi-set-compile-command ()
  (with-npm-project ((yfe (project-has-node-script-p "yoshi-flow-editor")))
    (setq-local compile-command (concat yfe " test --color "))))

(defun yoshi-set-eglot-server-program ()
  (with-npm-project ((tslib (project-has-file-p "node_modules/typescript/lib")))
    (setq-local eglot-server-programs
                (cons (list 'typescript-mode
                            "typescript-language-server"
                            "--stdio"
                            "--tsserver-path" tslib)
                      eglot-server-programs))))

(define-minor-mode yoshi-project-mode "Yoshi for Emacs."
  :lighter " 🦖"
  (when yoshi-project-mode
    (yoshi-prettier)
    (yoshi-eslint)
    (yoshi-set-compile-command)
    (yoshi-set-eglot-server-program)))

(provide 'yoshi)
