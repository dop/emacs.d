;; -*- lexical-binding: t; -*-

(require 'eglot)
(require 'flymake-eslint)
(require 'project-tools)

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
  (when-let ((package-json (expand-file-name "package.json" (project-root project)))
             (_ (file-exists-p package-json)))
    (let-alist (json-read-file package-json)
      (when-let ((test (or .scripts.test:unit .scripts.test:jest .scripts.test)))
        (yoshi--enhance-test-script test)))))

(defun --npx (command)
  (concat "npx --no-install " command))

(defun yoshi--extract-yoshi-command (command)
  (replace-regexp-in-string "\\(.*\\)\\byoshi\\(-[-a-z]+\\)?\\b\\(.*\\)"
                            "yoshi\\2"
                            command))

(defun yoshi--flow-command (project)
  (when-let ((package-json (expand-file-name "package.json" (project-root project)))
             (_ (file-exists-p package-json)))
    (let-alist (json-read-file package-json)
      (when-let ((start .scripts.start))
        (yoshi--extract-yoshi-command start)))))

(defun yoshi--setup-compilation (project)
  (let ((yoshi (--npx (yoshi--flow-command project)))
        (sled (--npx "sled-test-runner")))
    (setq-local
     compilation-error-regexp-alist '(jest typescript-tsc-pretty typescript-tsc)
     compile-command (if (locate-dominating-file "." "sled.json")
                         (concat sled " remote -f ")
                       (concat (yoshi--test-command project) " ")))))

(defun yoshi--setup-typescript (project)
  (setq-local eglot-stay-out-of '(flymake-diagnostic-functions))
  (setq-local flymake-diagnostic-functions '(flymake-eslint--checker eglot-flymake-backend))
  (prettier-mode t)
  (eglot-ensure))

(define-minor-mode yoshi-project-mode "Yoshi for Emacs."
  :keymap (let ((prefix (kbd "C-c y"))
                (map (make-sparse-keymap)))
            (define-key map (kbd "t") #'yoshi-extract-translation)
            (list (cons prefix map)))
  :lighter " yo"
  (cond
   (yoshi-project-mode
    (when-let ((project (project-current)))
      (yoshi--setup-compilation project)
      (yoshi--setup-typescript project)))
   (t
    (kill-all-local-variables))))

(provide 'yoshi)
