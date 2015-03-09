;;; Drupal

(define-project-type drupal (generic)
  (look-for ".drupal")
  :config-file ".drupal"
  :relevant-files ("\\.php$" "\\.inc$" "\\.module$" "\\.install$"
                   "\\.profile$" "\\.info$" "\\.test$"))
;; Set up drupal project with local variables.
(add-hook 'drupal-project-file-visit-hook 'drupal-eproject-setup)


(defun drupal-module-name (&rest file-name)
  (let ((safe-file-name (file-name-nondirectory (file-name-sans-extension (or buffer-file-name file-name "")))))
    (file-name-nondirectory
     (replace-regexp-in-string "\\(\\.[a-zA-Z0-9_]+\\)*$" "" safe-file-name))))

(defun drupal-make-function-comment (params-string)
  "Given PARAMS-STRING parses and produces function comment
according to Drupal requirements."
  (concat
   "/**\n"
   " * TODO: Describe function.\n"
   " *\n"
   (eval (cons 'concat
               (loop
                for arg in (split-string params-string ",[ \f\t\n\r\v]*" t)
                collect (if (string-match "^\\([a-zA-Z_0-9]+ \\)? *&?\\(\\$[a-zA-Z_0-9]+\\) *\\(= *[^\n]+\\)?$" arg)
                            (let* ((type   (match-string 1 arg))
                                   (name   (match-string 2 arg))
                                   (value? (match-string 3 arg))
                                   (value  (and value? (replace-regexp-in-string "^= *" "" value?))))
                              (concat
                               (format " * @param %s%s\n" (or type "") name)
                               (if value?
                                   (format " *    TODO: Describe. Optional. Default value is %s.\n *\n" value)
                                 (format " *   TODO: Describe parameter.\n *\n"))))))))
   " * @return\n *   TODO: Describe return value.\n */"))


(defvar drupal-schema-types
  (mapcar 'symbol-name '(blob char datetime float int numeric serial text varchar))
  "Available values for 'type' key in schema definition.")

(defun drupal-schema-type-sizes (type)
  (let ((type-sizes '(("blob"     "normal" "big")
                      ("char"     "normal")
                      ("datetime" "normal")
                      ("float"    "normal" "tiny" "small" "medium" "big")
                      ("int"      "normal" "tiny" "small" "medium" "big")
                      ("numeric"  "normal")
                      ("serial"   "normal" "tiny" "small" "medium" "big")
                      ("text"     "normal" "tiny" "small" "medium" "big")
                      ("varchar"  "normal"))))
    (reverse (cdr (assoc type type-sizes)))))

(defun drupal-schema-type-fields (type)
  (let* ((type-fields '(("varchar" . (length 50))
                        ("numeric" . (percision 10 scale 2))
                        ("int" . (unsigned "TRUE"))))
         (fields (cdr (assoc type type-fields))))
    (if fields
        (eval (cons 'concat (loop for prop in (property-list-keys fields)
                                  collect (format "'%s' => %s, " (symbol-name prop) (plist-get fields prop))))))))

(defvar drupal-menu-types
  '("MENU_CALLBACK"
    "MENU_SUGGESTED_ITEM"
    "MENU_LOCAL_TASK"
    "MENU_DEFAULT_LOCAL_TASK"
    "MENU_NORMAL_ITEM")
  "Drupal menu item types.")


(defun drupal-php-settings ()
  "Adjust some settings in accordance to Drupal Coding Standards."
  ;; Indentation settings.
  (setq c-basic-offset   2
        indent-tabs-mode nil
        fill-column      78)
  (c-set-offset 'substatement-open 0)
  (c-set-offset 'case-label 2)
  (c-set-offset 'arglist-cont 0)
  (c-set-offset 'arglist-close 0)
  (c-set-offset 'arglist-intro '+)
  (c-set-offset 'arglist-cont-nonempty 0))

(defun dp/drupal-settings ()
  ;; Commenting
  (setq comment-start "//"
        comment-end "")
  ;; Local bindings
  (local-set-key (kbd "C-c t") 'drupal-create-tags)
  (local-set-key (kbd "C-c i") 'drupal-browse-online-docs)
  (local-set-key (kbd "C-c v f") 'drupal-view-functions)
  (local-set-key (kbd "C-c d") 'drupal-open-mysql-buffer))

;; Copy from php-mode.el, but remove `delete-trailing-whitestape' because we
;; have whitespace cleanup mode.
(defun dp/php-enable-drupal-coding-style ()
  "Makes php-mode use coding styles that are preferable for
working with Drupal."
  (interactive)
  (setq tab-width 2
        indent-tabs-mode nil
        fill-column 78
        show-trailing-whitespace t)
  (c-set-style "drupal"))

;; Set up drupal indentation when in php mode and eproject attribute
;; :drupal-version is set.
(defun dp/php-maybe-drupal-settings ()
  "Check if file belongs to Drupal EProject and run `drupal-php-settings` if so."
  (ignore-errors
    (eproject-maybe-turn-on)
    (when (eproject-attribute :version)
      (dp/drupal-settings)
      (dp/php-enable-drupal-coding-style))))

(defun drupal-browse-online-docs ()
  "Open api.drupal.org documentation about current symbol."
  (interactive)
  (browse-url
   (concat "http://api.drupal.org/api/function/"
           (format "%s/%i"
                   (symbol-at-point)
                   (eproject-attribute :version)))))

(defun drupal-view-functions ()
  (interactive)
  (occur "^function"))

(defun drupal-open-mysql-buffer ()
  "Open mysql buffer."
  (interactive)
  (require 'sql)
  (let* ((project-name (eproject-attribute :name))
         (db-info      (eproject-attribute :database))
         (buf-name     (format "*SQL: %s*" project-name))
         (buf          (get-buffer buf-name)))
    (if buf
        (switch-to-buffer buf)
      (if (and buf-name project-name db-info)
          (progn
            (let ((sql-user     (plist-get db-info :user))
                  (sql-password (plist-get db-info :password))
                  (sql-database (plist-get db-info :database))
                  (sql-server   (plist-get db-info :server))
                  (sql-interactive-product 'mysql))
              (sql-comint-mysql 'mysql nil)
              (sql-interactive-mode)
              (sql-rename-buffer project-name)
              (pop-to-buffer (current-buffer))))
        (message ":name and :database are needed in .drupal file.")))))

(defun drupal-create-tags ()
  "Create tags file for Drupal modules, includes, and classes."
  (interactive)
  (let* ((files (eproject-list-project-files))
         (php-files (remove-if-not
                     (lambda (file)
                       (string-match
                        "\\.\\(module\\|php\\|install\\|inc\\|test\\|profile\\)$"
                        file))
                     files))
         (etags-executable "etags")
         (tags-file (concat eproject-root "TAGS")))
    (let ((result (eval
                   `(call-process etags-executable
                                  nil
                                  '(:file ,tags-file)
                                  nil
                                  "--lang=php"
                                  ,@php-files))))
      (if (= 0 result)
          (with-temp-buffer
            (insert-file-contents-literally tags-file)
            (goto-char (point-min))
            (replace-regexp "\n.*,0$" "")
            (delete-trailing-whitespace)
            (write-file tags-file nil)
            (setq tags-file-name tags-file))
        (message "Could not create TAGS file.")))))

(defun drush-command (&optional root site &rest arguments)
  "Generate drush command."
  (let ((s (or site (eproject-attribute :site)))
        (r (or root (eproject-root))))

    (let ((command `("drush" nil 0 nil "-r" ,r ,@(if s
                                                     (cons "-l" (cons s arguments))
                                                   arguments))))
      (message (format "%s" (mapconcat 'identity command " ")))
      command)))

(defun drush (&rest arguments)
  (eval `(drush-command nil nil ,@arguments)))

(defun drupal-drush-enable-module (module)
  "Run `drush -y en ...`"
  (interactive "MModule: ")
  (eval `(call-process ,@(drush "-y" "en" module))))

(defun drupal-drush-clear-cache-all ()
  "Run `drush cc all` process."
  (interactive)
  (eval `(call-process ,@(drush "cc" "all"))))

(defun drupal-eproject-setup ()
  "Set up Drupal project."
  (add-to-list 'auto-mode-alist
               '("\\(\\.php\\|\\.inc\\|\\.module\\|\\.install\\|\\.profile\\|\\.test\\)$" . php-mode))
  (setq flycheck-phpcs-standard "Drupal")
  (set (make-local-variable 'flycheck-checkers) '(php phpcs-drupal))
  (let ((tags-files (directory-files (eproject-root) t "TAGS" t)))
    (if (> (length tags-files) 0)
        (if (not (equal tags-file-name (nth 0 tags-files)))
            (setq tags-file-name (nth 0 tags-files)))
      (message "No TAGS file"))))
