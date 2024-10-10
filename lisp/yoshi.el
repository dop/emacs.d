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
   flymake-eslint-executable-name (yoshi--locate-eslint project)
   flymake-diagnostic-functions '(flymake-eslint--checker eglot-flymake-backend))
  (when (yoshi--locate-prettier project)
    (prettier-mode t))
  (eglot-ensure))

(defun yoshi-extract-translation ()
  "Place a translation in message_en.json file.

Kill region or a string (sexp) and ask for key to place it under
in message_en.json for current project. Put {t('KEY')} in place
of killed text."
  (interactive)
  (let* ((string (if (use-region-p)
                     (progn
                       (call-interactively #'kill-region)
                       (car kill-ring))
                   (kill-sexp)
                   (let ((yanked (car kill-ring)))
                     (substring yanked 1 (1- (length yanked))))))
         (english
          (seq-filter (lambda (filename)
                        (string-suffix-p "messages_en.json" filename))
                      (project-files (project-current))))
         (len (length english)))
    (cond ((= 1 len)
           (let ((key (read-string "Key: ")))
             (insert (format "{t('%s')}" key))
             (save-excursion
               (with-current-buffer (find-file-noselect (car english) t t)
                 (let ((value (read-string "Value: " string)))
                   (replace-regexp "\"[\n\r]*}\s*$" (format "\",\n\"%s\": \"%s\"\n}" key value))
                   (previous-line)
                   (indent-for-tab-command)
                   (save-buffer))))))
          ((= 0 len)
           (warn "No message_en.json found."))
          (t
           (warn "%n message_en.json files found." len)))))

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
      (when (derived-mode-p 'typescript-ts-base-mode)
        (yoshi--setup-typescript project))))
   (t
    (mapc #'kill-local-variable yoshi-local-project-overrides))))

(provide 'yoshi)
