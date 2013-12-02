;;; init.el

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(set-frame-font "Source Code Pro-14")
(scroll-bar-mode -1)
;; (menu-bar-mode t)
(tool-bar-mode -1)

(global-set-key (kbd "C-x w j") 'windmove-down)
(global-set-key (kbd "C-x w k") 'windmove-up)
(global-set-key (kbd "C-x w l") 'windmove-right)
(global-set-key (kbd "C-x w h") 'windmove-left)

(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key (kbd "C-x a r") 'align-regexp)
(global-set-key (kbd "C-z") nil)

(global-set-key (kbd "C-k") 'kill-line)

(global-set-key (kbd "C-\\") 'comment-region)
(global-set-key (kbd "C-M-\\") 'uncomment-region)
(global-set-key (kbd "C-.") 'repeat)
(global-set-key (kbd "RET") 'newline-and-indent)

(global-set-key [f5]  'whitespace-mode)
(global-set-key [f7]  'cua-mode)

(require 'server)
(unless (server-running-p) (server-start))

(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)

(setq inhibit-startup-screen t ; Do not dispay startup buffer.
      initial-scratch-message nil ; No message in *scratch* buffer.
      fill-column 80 ; Up to 80 charecters per line.
      org-startup-folded t
      query-replace-highlight t
      require-final-newline nil
      search-highlight t
      delete-selection-mode nil
      display-time-24hr-format t
      display-time-day-and-date t
      epg-gpg-program "/usr/local/bin/gpg"
      backup-directory-alist (\` (("." \, (expand-file-name "~/.emacs.d/backups"))))
      backup-by-copying t
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

(set-variable 'require-final-newline nil)

(set-variable 'js-indent-level 2)

(set-variable 'magit-emacsclient-executable
              "/usr/local/Cellar/emacs/24.3/bin/emacsclient")

;; (setq exec-path '("/usr/local/Library/ENV/4.3"
;;                   "/usr/local/opt/autoconf/bin"
;;                   "/usr/local/opt/automake/bin"
;;                   "/usr/local/opt/pkg-config/bin"
;;                   "/usr/local/opt/libtasn1/bin"
;;                   "/usr/local/opt/p11-kit/bin"
;;                   "/usr/local/opt/nettle/bin"
;;                   "/usr/local/opt/gnutls/bin"
;;                   "/opt/X11/bin"
;;                   "/usr/local/MacGPG2/bin"
;;                   "/usr/bin"
;;                   "/bin"
;;                   "/usr/sbin"
;;                   "/sbin"
;;                   "/private/tmp/emacs-BLVz/emacs-24.3/lib-src"
;;                   "/usr/local/Cellar/emacs/24.3/libexec/emacs/24.3/x86_64-apple-darwin13.0.1"
;;                   "/usr/local/bin"
;;                   "/Users/donatas/bin"
;;                   "/usr/local/ghc/bin"
;;                   "/usr/local/go/bin"
;;                   "/Users/donatas/.cabal/bin"))

;; (setq eshell-path-env (mapconcat (lambda (dir) (or dir ".")) exec-path path-separator))

;; (setenv "PATH" (mapconcat (lambda (dir) (or dir ".")) exec-path ":"))

(require 'cl)
(require 'package)

(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa" . "http://melpa.milkbox.net/packages/")
        ("org" . "http://orgmode.org/elpa/")
        ("marmalade" . "http://marmalade-repo.org/packages/")))

(setq package-enable-at-startup nil)
(package-initialize)

(require 'exec-path-from-shell)
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;; (defvar *my-packages*
;;   '(yasnippet
;;     whitespace-cleanup-mode
;;     solarized-theme
;;     smex
;;     skewer-mode
;;     simple-httpd
;;     s
;;     popup
;;     pkg-info
;;     php-mode
;;     php-extras
;;     org-plus-contrib
;;     magit-svn
;;     magit
;;     js2-mode
;;     ido-ubiquitous
;;     haskell-mode
;;     git-rebase-mode
;;     git-commit-mode
;;     flycheck
;;     f
;;     expand-region
;;     eproject
;;     epl
;;     drag-stuff
;;     dired-details
;;     diminish
;;     dash
;;     cyberpunk-theme
;;     auto-complete
;;     clojure-mode
;;     clojure-test-mode
;;     cider))

;; (dolist (p *my-packages*)
;;   (when (not (package-installed-p p))
;;     (package-install p)))



(require 'ido)
(require 'ido-ubiquitous)

(eval-after-load "ido"
  '(progn
     (icomplete-mode 1)))

;; Show line and column in mode line.
(line-number-mode t)
(column-number-mode t)

;; Allow answering with y/n instead of yes/no
(fset 'yes-or-no-p 'y-or-n-p)

;; Alias for usefull function.
(defalias 'qrr 'query-replace-regexp)

;; Set up spelling.
(setq ispell-program-name "/usr/local/bin/aspell")
(eval-after-load 'ispell
  '(setq ispell-local-dictionary-alist
	 '((nil "[a-zA-Z]" "[^a-zA-Z]" "[']" nil ("-l" "en") nil utf-8)
	   ("english" "[a-zA-Z]" "[^a-zA-Z]" "[']" nil ("-l" "en") nil utf-8)
	   ; ("british" "[a-zA-Z]" "[^a-zA-Z]" "[']" nil ("-l" "en") nil utf-8)
	   ("lithuanian" "[a-zA-ZąčęėįšųūžĄČĘĖĮŠŲŪŽ]" "[^a-zA-ZąčęėįšųūžĄČĘĖĮŠŲŪŽ]" "" nil ("-l" "lt") nil utf-8))))

;; UTF-8 please
(set-language-environment       "UTF-8")
(setq locale-coding-system      'utf-8)
(set-terminal-coding-system     'utf-8)
(set-keyboard-coding-system     'utf-8)
(set-selection-coding-system    'utf-8)
(prefer-coding-system           'utf-8)

(show-paren-mode 1)
(setq show-paren-style 'parenthesis)

;; Lets ask for newline at the end of file.
(setq-default require-final-newline nil)

;; Change selected text with new input.
(delete-selection-mode t)

;; Show time and load.
(display-time-mode t)

;; Show end of file.
(setq-default indicate-empty-lines t)
(setq-default indicate-buffer-boundaries 'left)

;; By default use spaces instead of tabs
(setq-default indent-tabs-mode nil)

(setq-default show-trailing-whitespace t)
(require 'whitespace-cleanup-mode)
(global-whitespace-cleanup-mode 1)
(diminish 'whitespace-cleanup-mode)

;; Also auto refresh dired, but be quiet about it
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

;; Show keystrokes in progress
(setq echo-keystrokes 0.1)

;; Transparently open compressed files
(auto-compression-mode t)

;; Undo/redo window configuration with C-c <left>/<right>
(winner-mode 1)

;; Real emacs knights don't use shift to mark things
(setq shift-select-mode nil)

(require 'saveplace)
(setq-default save-place t)
(setq save-place-file "~/.emacs.d/data/places")

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

(require 'drag-stuff)
(drag-stuff-global-mode t)
(diminish 'drag-stuff-mode)

(require 'smex)
(smex-initialize)

(autoload 'dictionary-search "dictionary"
  "Ask for a word and search it in all dictionaries" t)
(autoload 'dictionary-match-words "dictionary"
  "Ask for a word and search all matching words in the dictionaries" t)
(autoload 'dictionary-lookup-definition "dictionary"
  "Unconditionally lookup the word at point." t)
(autoload 'dictionary "dictionary"
  "Create a new dictionary buffer" t)
(autoload 'dictionary-mouse-popup-matching-words "dictionary"
  "Display entries matching the word at the cursor" t)
(autoload 'dictionary-popup-matching-words "dictionary"
  "Display entries matching the word at the point" t)
(autoload 'dictionary-tooltip-mode "dictionary"
  "Display tooltips for the current word" t)
(autoload 'global-dictionary-tooltip-mode "dictionary"
  "Enable/disable dictionary-tooltip-mode for all buffers" t)

(global-set-key "\C-cs" 'dictionary-search)
(global-set-key "\C-cm" 'dictionary-match-words)



;;; Ido
(require 'ido)
(ido-mode t)
(ido-everywhere t)

(eval-after-load "ido"
  '(progn
     (icomplete-mode)
     (require 'ido-ubiquitous)
     (ido-ubiquitous-mode 1)
					; (require 'ido-vertical-mode)
					; (ido-vertical-mode 1)
     ))

(setq ido-enable-flex-matching t
      ido-create-new-buffer 'always
      ido-use-filename-at-point 'guess)



;;; Org mode
(require 'org)
(setq org-startup-indented t)
(setq org-hide-leading-stars t)

(setq org-todo-keywords
      '((sequence "TODO(t)" "FEEDBACK(f@)" "RECURRING(r)" "APPROVE(a)"
		  "|"
		  "DONE(d!/!)" "CANCELLED(c@/!)")))
(setq org-todo-keyword-faces
      '(("CANCELLED" :foreground "dark gray" :weight bold)
	("FEEDBACK" :foreground "dark orange" :weight bold)
	("APPROVE" :foreground "cornflower blue" :weight bold)))

(setq org-use-fast-todo-selection t)
(setq org-treat-S-cursor-todo-selection-as-state-change nil)

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

(setq org-clock-persist t)
(org-clock-persistence-insinuate)
(setq org-clock-in-resume t)
(setq org-clock-out-remove-zero-time-clocks t)
(setq org-clock-out-when-done t)
(setq org-clock-persist-query-resume t)
(setq org-clock-auto-clock-resolution
      'when-no-clock-is-running)
(setq org-clock-report-include-clocking-task t)
(setq org-time-clocksum-format
      '(:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t))

(setq org-src-fontify-natively t)
(setq org-src-preserve-indentation t)

(require 'org-contacts)
(setq org-contacts-files '("~/Org/contacts.org"))

(add-hook 'message-mode-hook
	  (lambda ()
	    (flyspell-mode 1)
	    (orgstruct++-mode 1)))

(add-hook 'org-mode-hook
	  (lambda ()
	    (when (fboundp 'drag-stuff-mode)
	      (drag-stuff-mode 0))))

(defun count-days-between (from to)
  "FROM and TO are lists of form (month day year)."
  (+ 1 (abs (- (calendar-absolute-from-gregorian to)
	       (calendar-absolute-from-gregorian from)))))



;; Magit

(require 'magit)

(add-hook 'magit-mode-hook
	  '(lambda ()
	     (when (file-exists-p ".git/svn")
	       (require 'magit-svn)
	       (turn-on-magit-svn))))

(global-set-key [f12] 'magit-status)
(global-set-key "\C-xg" 'magit-status)

;; full screen magit-status

(defadvice magit-status (around magit-fullscreen activate)
  (window-configuration-to-register :magit-fullscreen)
  ad-do-it
  (delete-other-windows))

(defun magit-quit-session ()
  "Restores the previous window configuration and kills the magit buffer"
  (interactive)
  (kill-buffer)
  (jump-to-register :magit-fullscreen))

(define-key magit-status-mode-map (kbd "q") 'magit-quit-session)

;; ignore whitespace

(defun magit-toggle-whitespace ()
  (interactive)
  (if (member "-w" magit-diff-options)
      (magit-dont-ignore-whitespace)
    (magit-ignore-whitespace)))

(defun magit-ignore-whitespace ()
  (interactive)
  (add-to-list 'magit-diff-options "-w")
  (magit-refresh))

(defun magit-dont-ignore-whitespace ()
  (interactive)
  (setq magit-diff-options (remove "-w" magit-diff-options))
  (magit-refresh))

(define-key magit-status-mode-map (kbd "W") 'magit-toggle-whitespace)



;;; CSS

(autoload 'rainbow-mode "rainbow-mode" nil t)

(autoload 'css-mode "css-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.css$" . css-mode))
(add-hook 'css-mode-hook '(lambda () (rainbow-mode 1)))

(setq css-indent-offset 2)

(autoload 'less-css-mode "less-css-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.less$" . less-css-mode))
(add-hook 'less-css-mode-hook '(lambda () (rainbow-mode 1)))

(setq less-css-compile-at-save t)
(setq less-css-lessc-options '("-O2"))



;;; Dired

(require 'dired)

(add-hook 'dired-mode-hook (lambda () (drag-stuff-mode 0)))

(require 'dired-details)
(setq-default dired-details-hidden-string "--- ")
(dired-details-install)

;; Reload dired after creating a directory
(defadvice dired-create-directory (after revert-buffer-after-create activate)
  (revert-buffer))

;; Reload dired after quitting wdired
(defadvice wdired-abort-changes (after revert-buffer-after-abort activate)
  (revert-buffer))

;; C-a is nicer in dired if it moves back to start of files
(defun dired-back-to-start-of-files ()
  (interactive)
  (backward-char (- (current-column) 2)))

(define-key dired-mode-map (kbd "C-a") 'dired-back-to-start-of-files)

;; M-up is nicer in dired if it moves to the third line - straight to the ".."
(defun dired-back-to-top ()
  (interactive)
  (beginning-of-buffer)
  (next-line 2)
  (dired-back-to-start-of-files))

(define-key dired-mode-map (kbd "M-<up>") 'dired-back-to-top)

;; M-down is nicer in dired if it moves to the last file
(defun dired-jump-to-bottom ()
  (interactive)
  (end-of-buffer)
  (next-line -1)
  (dired-back-to-start-of-files))

(define-key dired-mode-map (kbd "M-<down>") 'dired-jump-to-bottom)

;; Delete with C-x C-k to match file buffers and magit
(define-key dired-mode-map (kbd "C-x C-k") 'dired-do-delete)

(eval-after-load "wdired"
  '(progn
     (define-key wdired-mode-map (kbd "C-a") 'dired-back-to-start-of-files)
     (define-key wdired-mode-map (kbd "M-<up>") 'dired-back-to-top)
     (define-key wdired-mode-map (kbd "M-<down>") 'dired-jump-to-bottom)))



;;; Javascript

(autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

(setq js2-basic-offset 2)

(defun dp/setup-js2-mode ()
  (setq indent-tabs-mode nil))

(add-hook 'js2-mode-hook 'dp/setup-js2-mode)

(defalias 'javascript-mode 'js2-mode)

;; After js2 has parsed a js file, we look for jslint globals decl comment ("/* global Fred, _, Harry */") and
;; add any symbols to a buffer-local var of acceptable global vars
(add-hook
 'js2-post-parse-callbacks
 (lambda ()
   ;; strip newlines etc so the regexp below will match a multiline comment
   (let ((btext (replace-regexp-in-string
		 "[\n\t ]+" " "
		 (buffer-substring-no-properties 1 (buffer-size)) t t)))
     (setq js2-additional-externs
	   (split-string
	    (if (string-match "/\\* *global \\(.*?\\)\\*/" btext)
		(match-string-no-properties 1 btext)
	      "")
	    "[ ,]+" t)))))



;;; Eproject

(require 'eproject)
(require 'eproject-extras)
; (require 'eproject-tasks)

;; eproject global bindings
(defmacro .emacs-curry (function &rest args)
  `(lambda () (interactive)
     (,function ,@args)))

(defmacro .emacs-eproject-key (key command)
  (cons 'progn
	(loop for (k . p) in (list (cons key 4) (cons (upcase key) 1))
	      collect
	      `(global-set-key
		(kbd ,(format "C-x p %s" k))
		(.emacs-curry ,command ,p)))))

(.emacs-eproject-key "k" eproject-kill-project-buffers)
(.emacs-eproject-key "v" eproject-revisit-project)
(.emacs-eproject-key "b" eproject-ibuffer)
(.emacs-eproject-key "o" eproject-open-all-project-files)



;;; Drupal and PHP

(autoload 'php-mode "php-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.\\(php[s345]?\\|inc\\|phtml\\)" . php-mode))

(require 'php-extras)

(defvar dp/php-function-def-regexp
  "\\b\\(\\(protected\\|private\\|public\\)\s+\\)?\\(static\s+\\)?function\s+[a-zA-Z0-9_]+\s*?(")

(defmacro dp/php-find-function-or-method (&optional direction)
  "Jump to next or previous function in current buffer."
  `(let ((jump-to)
	 (jump-from)
	 (search-fun ',(case direction
			 (backward 're-search-backward)
			 (t        're-search-forward))))
     (cl-flet ((match-at () ,(case direction
                               (backward '(match-end 0))
                               (t        '(match-beginning 0)))))
       (save-excursion
	 (setq jump-from (point))
	 (apply search-fun dp/php-function-def-regexp '(nil t))
	 (if (= jump-from (match-at))
	     (apply search-fun dp/php-function-def-regexp '(nil t)))
	 (when ,(case direction
		  (backward '(> jump-from (match-at)))
		  (t        '(< jump-from (match-at))))
	   (setq jump-to (match-beginning 0))))
       (when jump-to
	 (goto-char jump-to)
	 (recenter)))))

(defun dp/php-next-function ()
  "Jump to next the function in current buffer."
  (interactive)
  (dp/php-find-function-or-method forward))

(defun dp/php-previous-function ()
  "Jump to the previous function in current buffer."
  (interactive)
  (dp/php-find-function-or-method backward))

(defvar point-stack nil)

(defun point-stack-push ()
  "Push current location and buffer info onto stack."
  (interactive)
  (message "Location marked.")
  (setq point-stack (cons (list (current-buffer) (point)) point-stack)))

(defun point-stack-pop ()
  "Pop a location off the stack and move to buffer"
  (interactive)
  (if (null point-stack)
      (message "Stack is empty.")
    (switch-to-buffer (caar point-stack))
    (goto-char (cadar point-stack))
    (setq point-stack (cdr point-stack))))

(defun dp/php-jump-to-function-definition (&optional read-name)
  "Find definition of method or function in a buffer."
  (interactive "P")
  (let (name)
    (setq name (if read-name
		   (read-from-minibuffer "Name: ")
		 (symbol-at-point)))
    (when (symbolp name)
      (setq name (symbol-name name)))
    (if name
	(let ((re (concat "^\s*\\(protected\\|private\\|public\\)?\s*function\s+"
			  name
			  "\s*?("))
	      (matched-at))
	  (save-excursion
	    (goto-char (point-min))
	    (re-search-forward re nil t)
	    (setq matched-at (match-beginning 0)))
	  (when matched-at
	    (point-stack-push)
	    (goto-char matched-at)))
      (message "No name given."))))

(defun dp/php-init-keybindings ()
  "My PHP keybindings."
  (local-set-key (kbd "C-c j") 'dp/php-jump-to-function-definition)
  (local-set-key (kbd "C-*") 'point-stack-pop)
  (local-set-key (kbd "C-}") 'dp/php-next-function)
  (local-set-key (kbd "C-{") 'dp/php-previous-function))

(add-hook 'php-mode-hook 'dp/php-init-keybindings)

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

(define-project-type drupal (generic)
  (look-for ".drupal")
  :config-file ".drupal"
  :relevant-files ("\\.php$" "\\.inc$" "\\.module$" "\\.install$"
		   "\\.profile$" "\\.info$" "\\.test$"))

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

;; Set up drupal project with local variables.
(add-hook 'drupal-project-file-visit-hook 'drupal-eproject-setup)

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

(add-hook 'php-mode-hook 'dp/php-maybe-drupal-settings)

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



;;; Haskell

(require 'haskell-mode-autoloads)

;; Haskell cabal project
(define-project-type haskell (generic)
  (look-for ".haskell")
  :config-file ".haskell"
  :relevant-files ("\\.l?hs$" "\\.hsc$" "\\.cabal$"))

(setq haskell-program-name "ghci")
(setq haskell-indent-offset 4)

(add-hook 'haskel-cabal-mode-hook
	  (lambda ()
	    (setq indent-tabs-mode nil)))

                                        ; (setq haskell-mode-hook nil)
(add-hook 'haskell-mode-hook
	  (lambda ()
	    (turn-on-haskell-indent)
	    (turn-on-haskell-doc-mode)
	    (turn-on-haskell-decl-scan)
	    (capitalized-words-mode)
	    (local-set-key (kbd "C-c m") 'haskell-align-imports)
	    (local-set-key (kbd "C-c s") 'haskell-sort-imports)))

(setq inferior-haskell-mode-hook nil)
(add-hook 'inferior-haskell-mode-hook
	  (lambda ()
	    (setq compilation-first-column 1
		  compilation-error-regexp-alist inferior-haskell-error-regexp-alist)
	    (setf (nth 1 compilation-error-regexp-alist)
		  `("^\\(.+?\\):\\([0-9]+\\):\\(\\([0-9]+\\):\\)?\\( \\|\n *\\)\\(Warning\\)?"
		    1 2 4 ,@(if (fboundp 'compilation-fake-loc)
				'((6) nil))))))

(defun inferior-haskell-reload-and-run-main ()
  "Pass the current buffer's file to haskell and then run a :main."
  (interactive)
  (let ((proc (inferior-haskell-process)))
    (when proc
      (with-current-buffer inferior-haskell-buffer
	(comint-interrupt-subjob)
	(inferior-haskell-send-command proc ":reload")
	(inferior-haskell-send-command proc ":main")
                                        ; (inferior-haskell-send-command proc "\n"))
	(goto-char (point-max)))
      (switch-to-haskell))))

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

(add-hook 'haskell-project-file-visit-hook 'haskell-eproject-setup)

                                        ; (process-buffer (inferior-haskell-process))
                                        ; (inferior-haskell-buffer)

                                        ; (inferior-haskell-send-command (inferior-haskell-process) "\C-c")



;;; Nginx

(eval-after-load "nginx-mode"
  '(progn
     (add-hook 'nginx-mode-hook 'setup-nginx-mode)))

(defun setup-nginx-mode ()
  (setq comment-use-syntax nil)
  (local-set-key (kbd "C-x a d") 'nginx-align-directives))

(defun nginx-align-directives ()
  (interactive)
  (align-regexp (region-beginning) (region-end)
		"[a-z_]+\\(\\s-*\\)\\(.*\\);" 1 2 nil))


;;; SQL

(add-hook 'sql-mode-hook '(lambda nil
			    (sql-highlight-ansi-keywords)
			    (sql-highlight-mysql-keywords)))

(autoload 'plsql-mode "plsql.el" nil t)
(setq auto-mode-alist
      (append '(("\\.pk[sb]$" . plsql-mode)
		("\\.plsql" . plsql-mode)
		("\\.prc$" . plsql-mode))
	      auto-mode-alist))

(eval-after-load "plsql" '(progn (unless (featurep 'sqlplus) (require 'sqlplus))))

(autoload 'sqlplus-mode "sqlplus.el" nil t)
(add-to-list 'auto-mode-alist '("\\.sqp$" . init-sqlplus))
(defun init-sqlplus ()
  "load plsql-mode before sqlplus"
  (unless (featurep 'plsql)
    (require 'plsql)
    (plsql-mode))
  (sqlplus-mode)
  (setq sqlplus-html-output-sql t)
  (setq sqlplus-html-output-encoding 'utf-8)
  (sql-highlight-ansi-keywords)
  (sql-highlight-oracle-keywords))



;;; Yasnippet

(eval-after-load "yasnippet"
  '(progn
     (yas/global-mode 1)
     (setq yas-prompt-functions
	   '(yas/dropdown-prompt yas/completing-prompt yas/ido-prompt))
     (define-key yas/keymap (kbd "<return>") 'yas/exit-all-snippets)
     '(if (fboundp 'diminish)
	  (diminish 'yas-minor-mode))))


;;; Ocaml

(setq auto-mode-alist (cons '("\\.ml[iylp]?" . tuareg-mode) auto-mode-alist))
(autoload 'tuareg-mode "tuareg" "Major mode for editing Caml code" t)
(autoload 'camldebug "camldebug" "Run the Caml debugger" t)



;;; Flycheck

(require 'flycheck)
(push 'haskell-ghc flycheck-checkers)

(add-hook 'php-mode-hook 'flycheck-mode)



;;; Version Control

;; I like my commit messages without typos.
(add-hook 'log-edit-mode-hook (lambda () (flyspell-mode t)))

(defadvice vc-svn-checkin (before escape-filepaths
				  (files rev comment &optional extra-args-ignored)
				  activate)
  "If filename includes @, add @ at the end of it."
  (ad-set-arg 0 (mapcar (lambda (file)
			  (if (string-match "@" file)
			      (concat file "@")
			    file))
			(ad-get-arg 0))))

(defadvice vc-svn-register (before escape-filepaths
				   (files rev comment &optional extra-args-ignored)
				   activate)
  "If filename includes @, add @ at the end of it."
  (ad-set-arg 0 (mapcar (lambda (file)
			  (if (string-match "@" file)
			      (concat file "@")
			    file))
			(ad-get-arg 0))))


;;; Auto-Complete

(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/data/auto-complete-dict")
(ac-config-default)
(diminish 'auto-complete-mode)



;;; Donations

(define-project-type wix/ui (generic)
  (look-for "Gruntfile.js")
  :irrelevant-files ("dist/" "node_modules/" "app/bower_components/" "^.")
  :relevant-files ("\\.js$" "\\.html$" "\\.scss$"
                   "translations/.*\\.json"
                   "images/.*\\.\\(gif\\|jpe?g\\|png\\)"))

(add-hook 'wix/ui-project-file-visit-hook 'setup-wix-app-ui-project)

(defun setup-wix-app-ui-project ()
  "Setup some variables for wix/ui project"
  (set-variable 'flycheck-jshintrc (expand-file-name ".jshintrc" (eproject-root)))
  (set-variable 'dp-jshint-command (or (eproject-attribute :jshint) "jshint"))
  (when (eq major-mode 'js2-mode)
    (flycheck-mode 1)
    (set-variable 'flycheck-checker 'wix/jshint)))

(defcustom dp-jshint-command "jshint" "Path to jshint command.")
(make-variable-buffer-local 'dp-jshint-command)

(flycheck-define-checker dp-jshint
  "A JavaScript syntax and style checker using jshint.

See URL `http://www.jshint.com'."
  :command (dp-jshint-command
            "--reporter=checkstyle"
            (config-file "--config" flycheck-jshintrc)
            source)
  :error-parser flycheck-parse-checkstyle
  :modes (js-mode js2-mode js3-mode))



;;; init.el ends here

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(scss-compile-at-save nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
