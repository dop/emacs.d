;;; init.el

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(add-to-list 'load-path "~/.emacs.d/lisp")
(add-to-list 'custom-theme-load-path "~/.emacs.d/theme")

(defun set-global-font (font-name &optional keep-size)
  (set-frame-font font-name keep-size t))

;; (set-global-font "Consolas-12")
;; (set-global-font "Droid Sans Mono Slashed-11")
;; (set-global-font "Fira Mono-11")
;; (set-global-font "Inconsolata-13")
;; (set-global-font "Andale Mono-12")
;; (set-global-font "M+ 1mn-12")
;; (set-global-font "Menlo-11")
(set-global-font "Monaco-11")
;; (set-global-font "Oxygen Mono-12")
;; (set-global-font "Source Code Pro-12:regular")
;; (set-global-font "Ubuntu Mono-13")

(scroll-bar-mode -1)
(tool-bar-mode -1)
(unless (window-system) (menu-bar-mode -1))

(global-set-key (kbd "s-j") 'windmove-down)
(global-set-key (kbd "s-k") 'windmove-up)
(global-set-key (kbd "s-l") 'windmove-right)
(global-set-key (kbd "s-h") 'windmove-left)

(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key (kbd "C-x a r") 'align-regexp)
(global-set-key (kbd "C-z") nil)

(global-set-key (kbd "C-k") 'kill-line)

(global-set-key (kbd "C-\\") 'comment-region)
(global-set-key (kbd "C-M-\\") 'uncomment-region)
(global-set-key (kbd "C-.") 'repeat)
(global-set-key (kbd "RET") 'newline-and-indent)

(global-set-key [f5] 'whitespace-mode)
(global-set-key [f7] 'cua-mode)

(require 'server)
(unless (server-running-p) (server-start))

(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; (blink-cursor-mode)

(setq inhibit-startup-screen t ; Do not display startup buffer.
      initial-scratch-message nil ; No message in *scratch* buffer.
      fill-column 80 ; Up to 80 charecters per line.
      org-startup-folded t
      query-replace-highlight t
      search-highlight t
      delete-selection-mode nil
      epg-gpg-program "/usr/local/bin/gpg"
      visible-bell t)

(setq backup-directory-alist (\` (("." \, (expand-file-name "~/.emacs.d/backups"))))
      backup-by-copying t
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

(set-variable 'display-time-24hr-format nil)
(set-variable 'display-time-day-and-date nil)

(defun dp/zap-to-char (arg char)
  "Kill up to and including ARGth occurrence of CHAR.
Case is ignored if `case-fold-search' is non-nil in the current buffer.
Goes backward if ARG is negative; error if CHAR not found."
  (interactive (list (prefix-numeric-value current-prefix-arg)
		     (read-char "Zap to char: " t)))
  ;; Avoid "obsolete" warnings for translation-table-for-input.
  (with-no-warnings
    (if (char-table-p translation-table-for-input)
	(setq char (or (aref translation-table-for-input char) char))))
  (kill-region (point) (progn
			 (search-forward (char-to-string char) nil nil arg)
                         (backward-char)
			 (point))))

(global-set-key (kbd "M-z") 'dp/zap-to-char)
(global-set-key (kbd "M-Z") 'zap-to-char)

(defun download-to-buffer (url buf &optional with-headers)
  "Downloads URL into buffer buf."
  (let ((data (url-retrieve-synchronously url)))
    (if data
        (with-current-buffer buf
          (unless with-headers
            (with-current-buffer data
              (goto-char (point-min))
              (kill-region (point-min) (progn (forward-paragraph 1) (point)))
              (kill-line)))
          (save-excursion
            (insert-buffer-substring data))))))

(defun download-to-current-buffer (url)
  "Downloads URL into current buffer."
  (interactive "sURL: ")
  (download-to-buffer url (current-buffer) nil))

(defun delete-this-buffer-and-file ()
  "Removes file connected to current buffer and kills buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (when (yes-or-no-p "Are you sure you want to remove this file? ")
        (delete-file filename)
        (kill-buffer buffer)
        (message "File '%s' successfully removed" filename)))))

(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "FNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (progn
          (rename-file name new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil))))))

(defun set-cursor-according-to-mode ()
  (cond (buffer-read-only
         (setq cursor-type 'box))
        (overwrite-mode
         (setq cursor-type 'hbar))
        (t
         (setq cursor-type 'bar))))

(add-hook 'post-command-hook 'set-cursor-according-to-mode t)
;; (defun set-cursor-according-to-mode () (setq cursor-type 'box))
;; (remove-hook 'post-command-hook 'set-cursor-according-to-mode)

(defun my-terminal-visible-bell ()
   "A friendlier visual bell effect."
   (invert-face 'mode-line)
   (run-with-timer 0.1 nil 'invert-face 'mode-line))

 (setq visible-bell nil
       ring-bell-function 'my-terminal-visible-bell)

(require 'cl)
(require 'package)

(setq package-archives
      '(;("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa" . "http://melpa.org/packages/")
        ("org" . "http://orgmode.org/elpa/")
        ;; ("marmalade" . "http://marmalade-repo.org/packages/")
        ))

(setq package-enable-at-startup nil)
(package-initialize)

(defun toggle-theme (name)
  (if (custom-theme-enabled-p name)
      (disable-theme name)
    (load-theme name t)))

(defun switch-to-theme (name)
  (dolist (theme custom-enabled-themes)
    (disable-theme theme))
  (load-theme name t))

(set-variable 'monokai-high-contrast-mode-line t)
(set-variable 'solarized-use-less-bold t)
(set-variable 'solarized-use-more-italic nil)
(set-variable 'solarized-high-contrast-mode-line t)
(set-variable 'solarized-use-variable-pitch t)

(when (window-system) (switch-to-theme 'solarized-light))

;; (defvar *my-packages*
;;   '(angular-snippets
;;     auto-complete
;;     cider
;;     clojure-mode
;;     clojure-test-mode
;;     color-theme-sanityinc-tomorrow
;;     connection
;;     css-eldoc
;;     cyberpunk-theme
;;     dash
;;     dictionary
;;     diminish
;;     dired-details
;;     drag-stuff
;;     dropdown-list
;;     editorconfig
;;     emmet-mode
;;     ensime
;;     epl
;;     eproject
;;     exec-path-from-shell
;;     expand-region
;;     f
;;     flycheck
;;     flycheck-color-mode-line
;;     git-commit-mode
;;     git-rebase-mode
;;     god-mode
;;     haskell-mode
;;     heroku-theme
;;     highlight-indentation
;;     ido-ubiquitous
;;     inf-mongo
;;     js2-mode
;;     json-mode
;;     json-reformat
;;     json-snatcher
;;     link
;;     log4e
;;     magit
;;     magit-gitflow
;;     magit-svn
;;     mongo
;;     monokai-theme
;;     multiple-cursors
;;     nodejs-repl
;;     org-plus-contrib
;;     php-mode
;;     pkg-info
;;     popup
;;     rainbow-mode
;;     s
;;     scala-mode
;;     scss-mode
;;     simple-httpd
;;     skewer-mode
;;     smex
;;     solarized-theme
;;     sql-indent
;;     sr-speedbar
;;     visual-regexp
;;     w3m
;;     web-mode
;;     whitespace-cleanup-mode
;;     wrap-region
;;     yasnippet
;;     yaxception
;;     zenburn-theme))

(require 'exec-path-from-shell)
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;; (require 'highlight-indentation)

(require 'hl-line)
(defun turn-on-hl-line-mode () (hl-line-mode t))
(defun turn-off-hl-line-mode () (hl-line-mode -1))
;; (turn-on-hl-line-mode)
;; (turn-off-hl-line-mode)
(add-hook 'prog-mode-hook 'turn-on-hl-line-mode)
(add-hook 'html-mode-hook 'turn-on-hl-line-mode)

(defun turn-on-show-trailing-whitespace ()
  (set-variable 'show-trailing-whitespace t))

(add-hook 'prog-mode-hook 'turn-on-show-trailing-whitespace)
(add-hook 'html-mode-hook 'turn-on-show-trailing-whitespace)
(add-hook 'css-mode-hook 'turn-on-show-trailing-whitespace)
(add-hook 'scss-mode-hook 'turn-on-show-trailing-whitespace)

(require 'column-enforce-mode)
(set-face-attribute 'column-enforce-face nil :underline nil)
(global-column-enforce-mode 1)

(electric-pair-mode 1)

(require 'sr-speedbar)
(global-set-key [f8] 'sr-speedbar-toggle)

(require 'editorconfig)

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

(require 'multiple-cursors)
(global-set-key (kbd "C-s-.") 'mc/mark-next-like-this)
(global-set-key (kbd "C-s-,") 'mc/mark-previous-like-this)
(global-set-key (kbd "C->") 'mc/skip-to-next-like-this)
(global-set-key (kbd "C-<") 'mc/skip-to-previous-like-this)

(require 'wgrep)

;; Lets not ask for newline at the end of file.
(setq require-final-newline t)
(setq mode-require-final-newline t)

;; Change selected text with new input.
(delete-selection-mode t)

;; Show time and load.
;; (display-time-mode -1)

;; Show end of file.
(setq-default indicate-empty-lines t)
(setq-default indicate-buffer-boundaries 'left)

;; By default use spaces instead of tabs
(setq-default indent-tabs-mode nil)

(require 'whitespace-cleanup-mode)
(global-whitespace-cleanup-mode 1)

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

(require 'macrostep)
(define-key emacs-lisp-mode-map (kbd "C-c e") 'macrostep-expand)

(require 'saveplace)
(setq-default save-place t)
(setq save-place-file "~/.emacs.d/data/places")

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

(require 'drag-stuff)
(drag-stuff-global-mode t)

(require 'smex)
(smex-initialize)

(require 'wrap-region)
(add-hook 'prog-mode-hook 'turn-on-wrap-region-mode)

(require 'visual-regexp)
(define-key global-map (kbd "C-c r") 'vr/replace)
(define-key global-map (kbd "C-c q") 'vr/query-replace)
;; if you use multiple-cursors, this is for you:
(define-key global-map (kbd "C-c m") 'vr/mc-mark)

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
;; (global-set-key "\C-cm" 'dictionary-match-words)

;; (require 'highlight-focus)

(defface dimmed-face
  '((((class color) (background dark))
     :foreground "#303030")
    (((class color) (background light))
     :foreground "#ddd")
    (t :foreground "#ddd"))
  "Dimmed default face."
  :group 'basic-faces)

(defun dim-paren-braces-colons ()
  "Dim parentheses, braces, and colons."
  (font-lock-add-keywords nil
   '(("[();]" . 'dimmed-face))))

;; (remove-hook 'js2-mode-hook 'dim-paren-braces-colons)

(defun narrow-to-region-indirect (start end)
  "Restrict editing in this buffer to the current region, indirectly."
  (interactive "r")
  (deactivate-mark)
  (let ((buf (clone-indirect-buffer nil nil)))
    (with-current-buffer buf
      (narrow-to-region start end))
    (switch-to-buffer buf)))

(global-set-key (kbd "C-x n i") 'narrow-to-region-indirect)

;; Open files and goto lines like we see from g++ etc. i.e. file:line#
;; (to-do "make `find-file-line-number' work for emacsclient as well")
;; (to-do "make `find-file-line-number' check if the file exists")
(defadvice find-file (around find-file-line-number
                             (filename &optional wildcards)
                             activate)
  "Turn files like file.cpp:14 into file.cpp and going to the 14-th line."
  (save-match-data
    (let* ((matched (string-match "^\\(.*\\):\\([0-9]+\\):?$" filename))
           (line-number (and matched
                             (match-string 2 filename)
                             (string-to-number (match-string 2 filename))))
           (filename (if matched (match-string 1 filename) filename)))
      ad-do-it
      (when line-number
        ;; goto-line is for interactive use
        (goto-char (point-min))
        (forward-line (1- line-number))))))



;;; Ido

(require 'ido)
(ido-mode t)
(ido-everywhere t)

(eval-after-load "ido"
  '(progn
     (icomplete-mode)
     (require 'ido-ubiquitous)
     (ido-ubiquitous-mode 1)
     ;; (require 'ido-vertical-mode)
     ;; (ido-vertical-mode 1)
     ))

(setq ido-enable-flex-matching t
      ido-create-new-buffer 'always
      ido-use-filename-at-point 'guess
      ido-default-file-method 'selected-window
      ido-default-buffer-method 'selected-window)



;;; Org mode
(require 'org)
(setq org-startup-indented t)
(setq org-hide-leading-stars t)

(setq org-todo-keywords
      '((sequence "TODO(t)" "FEEDBACK(f@)" "RECURRING(r)" "APPROVE(a)" "POSTPONED(p)"
		  "|"
		  "DONE(d!/!)" "CANCELLED(c@/!)")))
(setq org-todo-keyword-faces
      '(("CANCELLED" :foreground "dark gray" :weight bold)
        ("POSTPONED" :foreground "sky blue" :weight bold)
	("FEEDBACK" :foreground "dark orange" :weight bold)
	("APPROVE" :foreground "cornflower blue" :weight bold)))

(setq org-use-fast-todo-selection t)
(setq org-treat-S-cursor-todo-selection-as-state-change nil)

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
;; (global-set-key "\C-cb" 'org-iswitchb)

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
(require 'magit-gitflow)

(set-variable 'magit-emacsclient-executable
              "/usr/local/Cellar/emacs/24.3/bin/emacsclient")

(add-hook 'git-commit-mode-hook 'turn-on-flyspell)

(add-hook 'magit-mode-hook 'turn-on-magit-gitflow)

(global-set-key [f12] 'magit-status)
(global-set-key "\C-xg" 'magit-status)

;; full screen magit-status

;; (defadvice magit-status (around magit-fullscreen activate)
;;   (window-configuration-to-register :magit-fullscreen)
;;   ad-do-it
;;   (delete-other-windows))

;; (ad-unadvise 'magit-status)

;; (defun magit-quit-session ()
;;   "Restores the previous window configuration and kills the magit buffer"
;;   (interactive)
;;   (kill-buffer)
;;   (jump-to-register :magit-fullscreen))
;; (define-key magit-status-mode-map (kbd "q") 'magit-quit-session)

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

(require 'git-gutter+)
(require 'git-gutter-fringe+)

(setq git-gutter-fr+-side 'right-fringe)
(global-git-gutter+-mode 1)

(define-fringe-bitmap 'git-gutter-fr+-added
  (fringe-helper-convert
   "..X.."
   "..X.."
   "XXXXX"
   "..X.."
   "..X..")
  6 8 'center)

(define-fringe-bitmap 'git-gutter-fr+-deleted
  (fringe-helper-convert
   "....."
   "....."
   "XXXXX"
   "....."
   ".....")
  6 8 'center)

(define-fringe-bitmap 'git-gutter-fr+-modified
  (fringe-helper-convert
   "....."
   ".XXX."
   ".XXX."
   ".XXX."
   ".....")
  6 8 'center)



;;; Flycheck

(require 'flycheck)
(add-hook 'php-mode-hook 'flycheck-mode)



;;; CSS

(autoload 'rainbow-mode "rainbow-mode" nil t)

(autoload 'css-mode "css-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.css$" . css-mode))
(add-hook 'css-mode-hook '(lambda () (rainbow-mode 1)))

(setq css-indent-offset 2)

;; (autoload 'less-css-mode "less-css-mode" nil t)
;; (add-to-list 'auto-mode-alist '("\\.less$" . less-css-mode))
;; (add-hook 'less-css-mode-hook '(lambda () (rainbow-mode 1)))

;; (setq less-css-compile-at-save t)
;; (setq less-css-lessc-options '("-O2"))

(add-hook 'scss-mode 'flycheck-mode)

(flycheck-define-checker dp/sass
  ""
  :command ("sass"
            "--cache-location" (eval (flycheck-sass-scss-cache-location))
            "--compass"
            "-c" source-original)
  :error-patterns
  ((error line-start "Syntax error on line " line ": " (message))
   (warning line-start "WARNING on line " line " of " (file-name)
            ":" (optional "\r") "\n" (message) line-end)
   (error line-start
          (or "Syntax error: " "Error: ")
          (message (one-or-more not-newline)
                   (zero-or-more "\n"
                                 (one-or-more " ")
                                 (one-or-more not-newline)))
          (optional "\r") "\n        on line " line " of " (file-name)
          line-end))
  :modes (sass-mode scss-mode))

;; (flycheck-define-checker scss-lint
;;   ""
;;   :command ("scss-lint" source-original)
;;   :error-patterns ((error line-start (file-name) ":" line " [E] "
;;                           (message) line-end)
;;                    (warning line-start (file-name) ":" line " [W] " (one-or-more word) ":"
;;                             (message) line-end))
;;   :modes (scss-mode sass-mode))

(add-hook 'scss-mode 'turn-on-css-eldoc)



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

(eval-after-load "js2-mode"
  '(progn
     (define-key js2-mode-map (kbd "M-<up>") 'js2r-move-line-up)
     (define-key js2-mode-map (kbd "M-<down>") 'js2r-move-line-down)
     (require 'js2-refactor)
     (set-variable 'js2r-use-strict t)
     (js2r-add-keybindings-with-prefix "C-c C-m")))

;; (defun dp/js2-mode-set-pretty-symbols ()
;;   (push '("function" . ?ƒ) prettify-symbols-alist))
;; (remove-hook 'js2-mode-hook 'dp/js2-mode-set-pretty-symbols)

(set-variable 'js-indent-level 2)
(set-variable 'js2-basic-offset 2)

(defun dp/setup-js2-mode ()
  ;; (highlight-indentation-current-column-mode -1)
  (require 'cap-words)
  (capitalized-words-mode 1)
  (setq indent-tabs-mode nil)
  (drag-stuff-mode 0)
  (local-set-key (kbd "C-c C-l") 'dp/comment-logging-lines))

(add-hook 'js2-mode-hook 'dp/setup-js2-mode)

(defalias 'javascript-mode 'js2-mode)
(setq-default js2-include-jslint-globals t)

;;; Utils

(defun dp/comment-lines-with (search replace &optional arg)
  (save-excursion
    (destructuring-bind (beg end) (if (use-region-p)
                                      (list (region-beginning)
                                            (region-end))
                                    (list (point-min)
                                          (point-max)))
      (if arg
          (replace-regexp (concat "^\\(\\s-+\\)// *" search)
                          (concat "\\1" replace)
                          nil beg end)
        (replace-regexp (concat "^\\(\\s-+\\)" search)
                        (concat "\\1// " replace)
                        nil beg end)))))

(defun dp/comment-logging-lines (&optional arg)
  (interactive "P")
  (dp/comment-lines-with "\\(console.log\\|log.\\(info\\|debug\\|warn\\|error\\)\\)"
                         "\\2" arg))

(defun dp/camelcase-name-split (s)
  (split-string
   (let ((case-fold-search nil))
     (downcase
      (replace-regexp-in-string "\\([a-z]\\)\\([A-Z]\\)" "\\1 \\2" s)))
   "[^A-Za-z0-9]+"))

(defun dp/camelcase-to-constant (s)
  (mapconcat 'upcase (dp/camelcase-name-split s) "_"))

(defun dp/constant-to-camelcase (s &optional capitalize-first)
  (let ((parts (split-string s "_")))
    (if capitalize-first
        (mapconcat 'capitalize parts nil)
      (concat
       (downcase (car parts))
       (mapconcat 'capitalize (cdr parts) nil)))))

;; (dp/constant-to-camelcase "PAY_NOW")

(defun dp/with-region-or-thing-at-point (fn &rest additional-arguments)
  (save-excursion
    (destructuring-bind (beg . end)
        (if (use-region-p)
            (cons (region-beginning) (region-end))
          (bounds-of-thing-at-point 'symbol))
      (let ((name (buffer-substring beg end)))
        (delete-region beg end)
        (goto-char beg)
        (insert (apply fn name additional-arguments))))))

(defun dp/convert-camelcase-to-constant ()
  (interactive)
  (dp/with-region-or-thing-at-point 'dp/camelcase-to-constant))

(defun dp/convert-constant-to-camelcase (&optional arg)
  (interactive "P")
  (dp/with-region-or-thing-at-point 'dp/constant-to-camelcase arg))

(global-set-key (kbd "C-c M-c") 'dp/convert-camelcase-to-constant)
(global-set-key (kbd "C-c M-k") 'dp/convert-constant-to-camelcase)



;;; Eproject

(require 'eproject)
(require 'eproject-tasks)
(require 'eproject-extras)
(require 'helm-eproject)
(eproject-helm-configure)

(defmacro .emacs-curry (function &rest args)
  `(lambda () (interactive)
     (,function ,@args)))

;; eproject global bindings
(defvar eproject-prefix "C-x p")

(defun eproject-set-key (key command &optional local)
  (let ((set-key (if local #'local-set-key #'global-set-key)))
    (funcall set-key (kbd (format "%s %s" eproject-prefix key)) command)))

(eproject-set-key "t" 'eproject-tasks-run)
(eproject-set-key "s" 'eproject-open-shell)

(defun eproject-open-shell ()
  (interactive)
  (let ((default-directory (eproject-root))
        (name (eproject-name)))
    (let ((buf (get-buffer-create (concat "*" name " shell*"))))
      (shell buf))))

(defun eproject-set-local-keys ()
  (mapc (lambda (action)
          ;; use pcase instead of destructuring-bind when there will be other
          ;; types of actions, not only :task
          (destructuring-bind (k _ task) action
            (eproject-set-key k (lexical-let ((task task)) ; because no closures
                                  #'(lambda nil
                                      (interactive)
                                      (dp/eproject-run-task task)))
                              t)))
        (eproject-attribute :keys)))

(defun eproject-jshint ()
  (when (eq major-mode 'js2-mode)
    (flycheck-mode 1)
    (set-variable 'flycheck-checker 'javascript-jshint)
    (set-variable 'flycheck-javascript-jshint-executable
                  (if (eproject-attribute :jshint)
                      (concat (eproject-root) (eproject-attribute :jshint)))
                  "jshint")
    (set-variable 'flycheck-jshintrc (dp/search-up-for ".jshintrc"))
    (when flycheck-jshintrc
      (let* ((json (json-read-file flycheck-jshintrc))
             (globals (mapcar 'car (cdr (assoc 'globals json)))))
        (set-variable 'js2-additional-externs
                      (nconc (mapcar 'symbol-name globals)
                             'js2-additional-externs))))))

(add-to-list 'generic-project-file-visit-hook 'eproject-set-local-keys)
(add-to-list 'generic-project-file-visit-hook 'eproject-jshint)

(defun songkick-server-task-unit ()
  (compile "../node_modules/.bin/grunt test" t))

(defun songkick-server-task-serve ()
  (dp/eproject-shell-command "../node_modules/.bin/grunt &"))

(defun songkick-server-task-mongod ()
  (dp/eproject-shell-command "mongod --dbpath=/usr/local/var/mongodb &"))

(defun songkick-client-task-unit ()
  (compile "../node_modules/.bin/grunt karma:unit" t))

(defun songkick-client-task-serve ()
  (dp/eproject-shell-command "../node_modules/.bin/grunt serve --force &"))



;; A small minor mode to use a big fringe
(defvar bzg-big-fringe-mode nil)
(define-minor-mode bzg-big-fringe-mode
  "Minor mode to use big fringe in the current buffer."
  :init-value nil
  :global t
  :variable bzg-big-fringe-mode
  :group 'editing-basics
  (if (not bzg-big-fringe-mode)
      (set-fringe-style nil)
    (set-fringe-mode
     (/ (- (frame-pixel-width)
           (* 100 (frame-char-width)))
        2))))

;; Now activate this global minor mode
;; (bzg-big-fringe-mode -1)

(defvar-local hidden-mode-line-mode nil)

(define-minor-mode hidden-mode-line-mode
  "Minor mode to hide the mode-line in the current buffer."
  :init-value nil
  :global t
  :variable hidden-mode-line-mode
  :group 'editing-basics
  (if hidden-mode-line-mode
      (setq hide-mode-line mode-line-format
            mode-line-format nil)
    (setq mode-line-format hide-mode-line
          hide-mode-line nil))
  (force-mode-line-update)
  ;; Apparently force-mode-line-update is not always enough to
  ;; redisplay the mode-line
  (redraw-display)
  (when (and (called-interactively-p 'interactive)
             hidden-mode-line-mode)
    (run-with-idle-timer
     0 nil 'message
     (concat "Hidden Mode Line Mode enabled.  "
             "Use M-x hidden-mode-line-mode to make the mode-line appear."))))

;; If you want to hide the mode-line in every buffer by default
;; (add-hook 'after-change-major-mode-hook 'hidden-mode-line-mode)



;;; Drupal and PHP

(autoload 'php-mode "php-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.\\(php[s345]?\\|inc\\|phtml\\)" . php-mode))

;; (require 'php-extras)

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

(load "haskell-mode-autoloads")

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

(defun dp/setup-haskell-mode ()
  (capitalized-words-mode)
  (local-set-key (kbd "C-c m") 'haskell-align-imports)
  (local-set-key (kbd "C-c s") 'haskell-sort-imports))

;; (setq haskell-mode-hook nil)
(add-hook 'haskell-mode-hook 'haskell-indentation-mode)
(add-hook 'haskell-mode-hook 'inf-haskell-mode)
(add-hook 'haskell-mode-hook 'dp/setup-haskell-mode)

;; (setq inferior-haskell-mode-hook nil)
;; (add-hook 'inferior-haskell-mode-hook
;; 	  (lambda ()
;; 	    (setq compilation-first-column 1
;; 		  compilation-error-regexp-alist inferior-haskell-error-regexp-alist)
;; 	    (setf (nth 1 compilation-error-regexp-alist)
;; 		  `("^\\(.+?\\):\\([0-9]+\\):\\(\\([0-9]+\\):\\)?\\( \\|\n *\\)\\(Warning\\)?"
;; 		    1 2 4 ,@(if (fboundp 'compilation-fake-loc)
;; 				'((6) nil))))))

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



;;; Scala

(require 'ensime)
(require 'scala-mode2)
(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)



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

(add-hook 'sql-mode-hook
          (lambda nil
            (sql-highlight-ansi-keywords)
            (sql-highlight-mysql-keywords)))

;; (autoload 'plsql-mode "plsql.el" nil t)
;; (setq auto-mode-alist
;;       (append '(("\\.pk[sb]$" . plsql-mode)
;; 		("\\.plsql" . plsql-mode)
;; 		("\\.prc$" . plsql-mode))
;; 	      auto-mode-alist))

;; (eval-after-load "plsql" '(progn (unless (featurep 'sqlplus) (require 'sqlplus))))

;; (autoload 'sqlplus-mode "sqlplus.el" nil t)
;; (add-to-list 'auto-mode-alist '("\\.sqp$" . init-sqlplus))
;; (defun init-sqlplus ()
;;   "load plsql-mode before sqlplus"
;;   (unless (featurep 'plsql)
;;     (require 'plsql)
;;     (plsql-mode))
;;   (sqlplus-mode)
;;   (setq sqlplus-html-output-sql t)
;;   (setq sqlplus-html-output-encoding 'utf-8)
;;   (sql-highlight-ansi-keywords)
;;   (sql-highlight-oracle-keywords))



;;; HTML

(defun dp/setup-sgml-mode ()
  (define-key sgml-mode-map (kbd "C-c =") 'mc/mark-sgml-tag-pair)
  (define-key sgml-mode-map (kbd "C-}") 'sgml-skip-tag-forward)
  (define-key sgml-mode-map (kbd "C-{") 'sgml-skip-tag-backward))

(eval-after-load 'sgml-mode
  '(dp/setup-sgml-mode))

(require 'web-mode)
;; (add-hook 'web-mode-hook 'whitespace-turn-off)
;; ; (remove-hook 'web-mode-hook 'highlight-indentation-mode)
;; (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.vm?\\'" . web-mode))

;; (setq web-mode-disable-auto-pairing nil)
;; (setq web-mode-enable-current-element-highlight t)



;;; Yasnippet

(require 'dropdown-list)

(require 'yasnippet)
(yas-global-mode 1)
(setq yas-prompt-functions '(yas-dropdown-prompt
                             yas-completing-prompt
                             yas-ido-prompt))


(define-key yas-minor-mode-map (kbd "TAB") 'yas-expand)



;;; Ocaml

(setq auto-mode-alist (cons '("\\.ml[iylp]?" . tuareg-mode) auto-mode-alist))
(autoload 'tuareg-mode "tuareg" "Major mode for editing Caml code" t)
(autoload 'camldebug "camldebug" "Run the Caml debugger" t)



;;; Shell mode, Terminal mode

(defun dp/turn-off-show-trailing-whitespace ()
  (set-variable 'show-trailing-whitespace nil))

(add-hook 'term-mode 'dp/turn-off-show-trailing-whitespace)

(defun dp/last-dirs (limit path)
  (mapconcat 'identity
             (last (delete "" (split-string path "/"))
                   limit)
             "/"))

(defun add-mode-line-dirtrack ()
  (add-to-list
   'mode-line-buffer-identification 
   '(:eval `(:propertize
             ,(let ((path-with-tilde (replace-regexp-in-string
                                      (regexp-quote (substring (getenv "HOME") 1)) "~"
                                      default-directory)))
                (concat " " (dp/last-dirs 2 path-with-tilde) " "))
             face dired-directory))))

;; (add-hook 'shell-mode-hook 'add-mode-line-dirtrack)



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



(require 'json)

(defun dp/eproject-run-task (name)
  (eproject-tasks-run
   (car (member-if (lambda (task) (equal name (car task)))
                   (eproject-tasks-get)))))

(defun dp/search-up-for (file-name)
  "Search up file directory tree for a first occurance of file."
  (let ((dir (eproject--scan-parents-for
              (file-name-directory (buffer-file-name))
              (lambda (dir)
                (file-exists-p (concat (file-name-as-directory dir) file-name))))))
    (if dir
        (concat (file-name-as-directory dir) file-name)
      nil)))

(defun dp/eproject-shell-command (command)
  (let* ((name (format "*%s <%s>*" (eproject-name) command))
         (process (get-buffer-process name)))
    (if process
        (progn
          (message (format "Command '%s' is already running." command))
          (switch-to-buffer name))
      (shell-command command name))))

(defvar hotels-statics-be-loud nil)

(defun hotels-statics-e2e-tests ()
  (let ((command (if hotels-statics-be-loud
                     (concat "(grunt e2e:normal && say 'great success' "
                             "|| say \"piece of shit that doesn't work\") &")
                   "grunt e2e:normal &")))
    (dp/eproject-shell-command command)))

;; (defun dp/fix-escape-sequences (ignore)
;;   (let ((start-marker (if (and (markerp comint-last-output-start)
;; 			       (eq (marker-buffer comint-last-output-start)
;; 				   (current-buffer))
;; 			       (marker-position comint-last-output-start))
;; 			  comint-last-output-start
;; 			(point-min-marker)))
;; 	(end-marker (process-mark (get-buffer-process (current-buffer)))))
;;     (save-excursion
;;       (goto-char start-marker)
;;       (while (search-forward "[1A[2K" end-marker t)
;;         (previous-line)
;;         (kill-whole-line)
;;         (kill-forward-chars 8)
;;         ))))
;; (replace-regexp "[1A" "")) ;; nil compilation-filter-start (point-max)))
;; (remove-hook 'comint-output-filter-functions 'dp/fix-escape-sequences)



(dolist (mode '(whitespace-cleanup-mode
                drag-stuff-mode
                wrap-region-mode
                yas-minor-mode
                auto-complete-mode
                column-enforce-mode
                ;; capitalized-words-mode
                magit-auto-revert-mode))
  (diminish mode))



;;; init.el ends here
