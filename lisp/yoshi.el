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

(defun yoshi--test-command (project)
  (when-let ((package-json (expand-file-name "package.json" (cdr project)))
             (_ (file-exists-p package-json)))
    (let-alist (json-read-file package-json)
      (if-let ((test-command .scripts.test:unit)
               (npxed (replace-regexp-in-string "\\byoshi\\([^ ]+\\)" "npx --no-install \\&" test-command)))
        (replace-regexp-in-string "\\btest\\b" "test --color" npxed)))))

(defun yoshi--setup-compilation (project)
  (let ((yoshi (or (project-npx project "yoshi-flow-editor")
                   (project-npx project "yoshi-flow-bm")))
        (sled (project-npx project "sled-test-runner")))
    (yoshi-set
     compilation-error-regexp-alist '(jest typescript-tsc-pretty typescript-tsc)
     compile-command (if (locate-dominating-file "." "sled.json")
                         (concat sled " local -f ")
                       (concat (yoshi--test-command project) " ")))))

(defun yoshi--setup-typescript (project)
  (yoshi-set
   eglot-stay-out-of '(flymake-diagnostic-functions)
   eglot-server-programs (list (list '(typescript-mode tsx-mode) "typescript-language-server" "--stdio" "--tsserver-path"
                                     (project-has-file-p project "node_modules/typescript/lib")))
   flymake-eslint-executable-name (project-has-node-script-p project "eslint")
   flymake-diagnostic-functions '(flymake-eslint--checker eglot-flymake-backend))
  (when (project-has-node-script-p project "prettier")
    (prettier-mode t))
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
