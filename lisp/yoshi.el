;; -*- lexical-binding: t; -*-

(require 'eglot)
(require 'flymake-eslint)
(require 'project-tools)

(defvar-local yoshi-local-project-overrides nil
  "Saves a list of overriden buffer variables so that they can be
clear when mode is turned off.")

(defmacro yoshi-set (&rest pairs)
  `(progn
     ,@(cl-loop for (name value) on pairs by #'cddr
                collect `(push ',name yoshi-local-project-overrides))
     (setq-local ,@pairs)))

(defun yoshi--enhance-test-script (test-command)
  (when-let ((npxed (replace-regexp-in-string "\\byoshi"
                                              "npx --no-install \\&"
                                              test-command)))
    (replace-regexp-in-string "\\btest\\b" "test --color" npxed)))

(ert-deftest yoshi--enhance-test-script ()
  (should (equal "npx --no-install yoshi test --color"
                 (yoshi--enhance-test-script "yoshi test")))
  (should (equal "npx --no-install yoshi-flow-editor test --color"
                 (yoshi--enhance-test-script "yoshi-flow-editor test")))
  (should (equal "FLAG=true npx --no-install yoshi test --color"
                 (yoshi--enhance-test-script "FLAG=true yoshi test"))))

(defun yoshi--test-command (project)
  (when-let ((package-json (expand-file-name "package.json" (cdr project)))
             (_ (file-exists-p package-json)))
    (let-alist (json-read-file package-json)
      (yoshi--enhance-test-script
       (or .scripts.test:unit .scripts.test:jest .scripts.test)))))

(defun --npx (command)
  (concat "npx --no-install " command))

(defun yoshi--extract-yoshi-command (command)
  (replace-regexp-in-string "\\(.*\\)\\byoshi\\(-[-a-z]+\\)?\\b\\(.*\\)"
                            "yoshi\\2"
                            command))

(defun yoshi--flow-command (project)
  (when-let ((package-json (expand-file-name "package.json" (cdr project)))
             (_ (file-exists-p package-json)))
    (let-alist (json-read-file package-json)
      (yoshi--extract-yoshi-command .scripts.start))))

(defun yoshi--setup-compilation (project)
  (let ((yoshi (--npx (yoshi--flow-command project)))
        (sled (--npx "sled-test-runner")))
    (yoshi-set
     compilation-error-regexp-alist '(jest typescript-tsc-pretty typescript-tsc)
     compile-command (if (locate-dominating-file "." "sled.json")
                         (concat sled " local -f ")
                       (concat (yoshi--test-command project) " ")))))

(defun yoshi--locate-outer-dominating-file (project file)
  (or (project-has-file-p project file)
      (let ((outer (locate-dominating-file (project-root project) file)))
        (expand-file-name file outer))))

(defun yoshi--locate-typescript-lib (project)
  (yoshi--locate-outer-dominating-file project "node_modules/typescript/lib"))

(defun yoshi--locate-prettier (project)
  (yoshi--locate-outer-dominating-file project "node_modules/.bin/prettier"))

(defun yoshi--locate-eslint (project)
  (yoshi--locate-outer-dominating-file project "node_modules/.bin/eslint"))

(defun yoshi--setup-typescript (project)
  (yoshi-set
   eglot-stay-out-of '(flymake-diagnostic-functions)
   eglot-server-programs `(((typescript-mode tsx-mode) "typescript-language-server" "--stdio"
                            "--tsserver-path" ,(yoshi--locate-typescript-lib project)))
   flymake-eslint-executable-name (yoshi--locate-eslint project)
   flymake-diagnostic-functions '(flymake-eslint--checker eglot-flymake-backend))
  (when (yoshi--locate-prettier project)
    (prettier-mode t))
  (eglot-ensure))

(define-minor-mode yoshi-project-mode "Yoshi for Emacs."
  :lighter " yo"
  (cond
   (yoshi-project-mode
    (when-let ((project (project-current)))
      (yoshi--setup-compilation project)
      (when (derived-mode-p 'typescript-mode)
        (yoshi--setup-typescript project))))
   (t
    (mapc #'kill-local-variable yoshi-local-project-overrides))))

(provide 'yoshi)
