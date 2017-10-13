;;; init.el

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(add-to-list 'load-path "~/.emacs.d/lisp")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")

(require 'cl)
(require 'package)

(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa" . "http://melpa.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")))

(setq package-enable-at-startup nil)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

(use-package f :ensure t)
(use-package dash :ensure t)

(require 'subr-x)
(require 'f)
(require 'dash)
(require 'json)

(require 'color)
(require 'dp-functions)

(setenv "PAGER" "cat")
(setq comint-input-ignoredups t)
(setq comint-eol-on-send t)
(setq comint-scroll-to-bottom-on-output nil)
(setq comint-move-point-for-output nil)
(setq comint-scroll-show-maximum-output nil)
(setq comint-process-echoes t)

;; No more symlinks to indicated that file is being edited.
(setq create-lockfiles nil)

(scroll-bar-mode -1)
(tool-bar-mode -1)
(when (not (window-system)) (menu-bar-mode -1))
(blink-cursor-mode t)

;; Do not split windows
(setq split-height-threshold nil split-width-threshold nil)

(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'erase-buffer 'disabled nil)

(setq mac-option-modifier 'alt)
(setq mac-command-modifier 'meta)

(setq cua-enable-cua-keys t)
(setq cua-prefix-override-inhibit-delay nil)

(setq gc-cons-threshold (* 20 1024 1024))

;; Put system clipboard contents into kill ring before killing so that it's not
;; lost.
(setq save-interprogram-paste-before-kill t)

;; UTF-8 please
(set-language-environment "UTF-8")
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(setq inhibit-startup-screen t ; Do not display startup buffer.
      initial-scratch-message nil ; No message in *scratch* buffer.
      fill-column 80 ; Up to 80 charecters per line.
      query-replace-highlight t
      search-highlight t)

(setq-default indicate-empty-lines nil
              indicate-buffer-boundaries 'left)

(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))
      scroll-conservatively most-positive-fixnum
      scroll-preserve-screen-position t)

(setq backup-directory-alist (\` (("." \, (expand-file-name "~/.emacs.d/backups"))))
      backup-by-copying t
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

(setq display-time-24hr-format nil
      display-time-day-and-date nil)

(setq visible-bell t
      ring-bell-function nil)

(fset 'yes-or-no-p 'y-or-n-p)

(line-number-mode t)
(column-number-mode t)
(electric-pair-mode t)

(show-paren-mode t)
(setq show-paren-style 'parenthesis)

(setq require-final-newline t
      mode-require-final-newline t)

(delete-selection-mode t)

(setq-default indent-tabs-mode nil)

;; Also auto refresh dired, but be quiet about it
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

;; Show keystrokes in progress
(setq echo-keystrokes 0.1)

;; Transparently open compressed files
(auto-compression-mode t)

;; Undo/redo window configuration with C-c <left>/<right>
(winner-mode t)

;; Real emacs knights don't use shift to mark things
(setq shift-select-mode t)

;; increate text by 1
(setq text-scale-mode-step 1.1)

(defun turn-on-show-trailing-whitespace ()
  (setq show-trailing-whitespace t))

(add-hook 'prog-mode-hook #'turn-on-show-trailing-whitespace)
(add-hook 'html-mode-hook #'turn-on-show-trailing-whitespace)
(add-hook 'css-mode-hook #'turn-on-show-trailing-whitespace)

(require 'dp-window)
(require 'dp-mode-line)
(require 'dp-key-bindings)

(use-package simple
  :config
  (setq-default visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow)))

(use-package ibuffer
  :bind (("C-x C-b" . ibuffer)))

(use-package paradox
  :ensure t
  :commands (paradox-list-packages))

(use-package epg
  :defer t
  :config
  (setf epa-pinentry-mode 'loopback))

(defun set-bidi-left-to-right ()
  (setq bidi-paragraph-direction 'left-to-right))

(use-package shell
  :config
  (add-hook 'shell-mode-hook #'set-bidi-left-to-right))

(use-package multi-term
  :config
  (add-hook 'term-mode-hook #'set-bidi-left-to-right))

(use-package linum-mode
  :disabled t
  :commands linum-mode
  :init
  (remove-hook 'prog-mode-hook 'linum-mode)
  (remove-hook 'sgml-mode-hook 'linum-mode)
  :config
  (setq linum-format
        (lambda (line)
          (let* ((width (length (number-to-string
                                 (count-lines (point-min) (point-max)))))
                 (fmt (concat "%" (number-to-string (1+ width)) "d")))
            (propertize (format fmt line) 'face 'linum)))))

(use-package grep
  :defer t
  :config
  (add-to-list 'grep-find-ignored-directories "node_modules")
  (add-to-list 'grep-find-ignored-directories "bower_components"))

(use-package eshell-fringe-status
  :ensure t
  :commands eshell-fringe-status-mode)

(use-package eshell-git-prompt
  :disabled t
  :ensure t
  :commands eshell-git-prompt-use-theme)

(use-package eshell
  :defer t
  :config
  (add-hook 'eshell-mode-hook #'eshell-fringe-status-mode))

(use-package exec-path-from-shell
  :defer 2
  :ensure t
  :init (when (memq window-system '(mac ns))
          (exec-path-from-shell-initialize)))

(use-package hungry-delete
  :ensure t
  :diminish hungry-delete-mode
  :init (global-hungry-delete-mode))

(use-package hl-line
  :disabled t
  :commands hl-line-mode
  :init
  (add-hook 'prog-mode-hook 'hl-line-mode)
  (add-hook 'html-mode-hook 'hl-line-mode))

(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :init (global-undo-tree-mode t)
  :bind (:map undo-tree-map
              ("C-x u" . undo-tree-visualize)))

(use-package websocket
  :ensure t)

(use-package markdown-preview-mode
  :ensure t
  :commands (markdown-preview-mode)
  :config
  (setq markdown-preview-stylesheets
        '("http://thomasf.github.io/solarized-css/solarized-light.min.css")))

(use-package markdown-mode
  :ensure t
  :mode "\\.md'"
  :init
  (setq markdown-command "pandoc -f markdown -t html")
  :config
  (add-hook 'markdown-mode-hook #'visual-line-mode)
  (add-hook 'markdown-mode-hook #'flyspell-mode))

(use-package groovy-mode
  :mode "\\.gradle'")

(use-package editorconfig
  :ensure t)

(defun dp/ffap-local-file (name)
  (if (or (string-prefix-p "/" name)
          (string-prefix-p "./" name)
          (string-prefix-p "../" name))
      (-first #'file-exists-p
              (list (concat name ".js") (concat name "/index.js") name))))

(defun davazp/ffap-nodejs-module (name)
  (block nil
    (if-let ((local (dp/ffap-local-file name)))
        (return local))
    (unless (or (string-prefix-p "/" name)
                (string-prefix-p "./" name)
                (string-prefix-p "../" name)
                (not (or (s-match "^import.+from\s+'" (thing-at-point 'sentence))
                         (s-match "require('[^']+')" (thing-at-point 'sentence)))))
      (let ((base (locate-dominating-file default-directory "node_modules")))
        (-first #'file-exists-p
                (list (concat base "node_modules/" name ".js")
                      (concat base "node_modules/" name "/index.js")
                      (concat base "node_modules/" name)
                      (concat base name ".js")
                      (concat base name "/index.js")
                      (concat base name)))))))

(use-package ffap
  :defer t
  :config
  (setq ffap-machine-p-known 'reject)
  (add-to-list 'ffap-alist '(js-mode . davazp/ffap-nodejs-module) t)
  (add-to-list 'ffap-alist '(js2-mode . davazp/ffap-nodejs-module) t)
  (add-to-list 'ffap-alist '(rjsx-mode . davazp/ffap-nodejs-module) t )
  (add-to-list 'ffap-alist '(js2-jsx-mode . davazp/ffap-nodejs-module) t)
  (add-to-list 'ffap-alist '(typescript-mode . davazp/ffap-nodejs-module) t)
  (add-to-list 'ffap-alist '(flowtype-mode . davazp/ffap-nodejs-module) t))

(use-package ispell
  ;; :commands (ispell-word flyspell-buffer flyspell-mode)
  :config (setq ispell-local-dictionary-alist
                '((nil "[a-zA-Z]" "[^a-zA-Z]" "[']" nil ("-l" "en") nil utf-8)
                  ("english" "[a-zA-Z]" "[^a-zA-Z]" "[']" nil ("-l" "en") nil utf-8)
                  ;; ("british" "[a-zA-Z]" "[^a-zA-Z]" "[']" nil ("-l" "en") nil utf-8)
                  ("lithuanian" "[a-zA-ZąčęėįšųūžĄČĘĖĮŠŲŪŽ]" "[^a-zA-ZąčęėįšųūžĄČĘĖĮŠŲŪŽ]" "" nil ("-l" "lt") nil utf-8))
                ispell-program-name "/usr/local/bin/aspell"))

(defun dp/xterm-color-compilation-filter ()
  (let* ((inhibit-read-only t)
         (start compilation-filter-start)
         (end   (point))
         (input (delete-and-extract-region start end)))
    (goto-char (point-max))
    (insert (xterm-color-filter input))))

(use-package xterm-color
  :ensure t
  :commands (xterm-color-filter)
  :init
  (add-hook 'comint-preoutput-filter-functions #'xterm-color-filter)
  (add-hook 'compilation-filter-hook #'dp/xterm-color-compilation-filter)
  (setq comint-output-filter-functions (remove 'ansi-color-process-output comint-output-filter-functions)))

(use-package multi-term
  :ensure t
  :defer t
  :config
  (define-key term-raw-map "\C-c\C-l" 'term-line-mode)
  (define-key term-raw-map "\C-c\C-k" 'term-char-mode))

(use-package multiple-cursors
  :ensure t
  :bind (("C-M-." . mc/mark-next-like-this)
         ("C-M-," . mc/skip-to-next-like-this)))

(use-package whitespace-cleanup-mode
  :ensure t
  :diminish whitespace-cleanup-mode
  :init (global-whitespace-cleanup-mode 1))

(use-package macrostep
  :ensure t
  :commands macrostep-expand
  :init (bind-key "C-c e" 'macrostep-expand emacs-lisp-mode-map))

(use-package eldoc
  :diminish eldoc-mode
  :commands eldoc-mode
  :config (add-hook 'emacs-lisp-mode-hook 'eldoc-mode))

(use-package uniquify
  :config (setq uniquify-buffer-name-style 'forward))

(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

(use-package wrap-region
  :ensure t
  :diminish wrap-region-mode
  :init (wrap-region-global-mode)
  :config
  (progn
    (dolist (mode '(js2-mode js-mode js2-jsx-mode typescript-mode flowtype-mode))
      (wrap-region-add-wrapper "`" "`" nil mode)
      (wrap-region-add-wrapper "_" "_" nil mode))))

(use-package visual-regexp-steroids
  :ensure t
  :defer t)

(use-package visual-regexp
  :ensure t
  :bind ("C-c r" . vr/replace)
  :config (require 'visual-regexp-steroids))

(use-package company
  :ensure t
  :diminish company-mode
  :init
  (progn
    (global-company-mode t)
    (bind-keys :map company-active-map
               ("C-w" . nil)
               ([return] . nil)
               ([tab] . company-complete-selection)
               ("TAB" . company-complete-selection)
               ("RET" . nil)
               ("C-n" . company-select-next)
               ("C-p" . company-select-previous)))
  :config
  (setq company-idle-delay 0.1))

(use-package dictionary
  :ensure t
  :commands dictionary-search
  :bind ("C-c s" . dictionary-search))

(use-package flx-isearch
  :ensure t
  :bind (("C-M-s" . flx-isearch-forward)
         ("C-M-r" . flx-isearch-backward)))

(use-package ido-ubiquitous :ensure t :defer t)
(use-package ido-vertical-mode :ensure t :defer t)
(use-package flx-ido :ensure t :defer t)

(use-package ido
  :ensure t
  :init (ido-mode t)
  :config
  (defun dp/ido-keys ()
    (define-key ido-completion-map " " 'ido-restrict-to-matches))
  (setq ido-vertical-decorations
        '("\n → " "" "\n   " "\n   ···" "[" "]" " [No match]" " [Matched]"
          " [Not readable]" " [Too big]" " [Confirm]" "\n → " ""))
  (ido-everywhere t)
  (ido-ubiquitous-mode t)
  (ido-vertical-mode t)
  (flx-ido-mode t)
  (add-to-list 'ido-setup-hook 'dp/ido-keys)
  (setq ido-enable-flex-matching t
        ido-use-virtual-buffers t
        ido-vertical-define-keys 'C-n-and-C-p-only
        ido-use-faces nil
        ido-create-new-buffer 'always
        ido-use-filename-at-point 'guess
        ido-default-file-method 'selected-window
        ido-default-buffer-method 'selected-window))

(use-package smex
  :ensure t
  :bind ("M-x" . smex))

(use-package htmlize
  :ensure t
  :defer t)

(defun eval-and-replace ()
  "Replace the preceding sexp with its evaluated value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

(use-package elisp-mode
  :defer t
  :config
  (define-key emacs-lisp-mode-map "\C-c\C-e" #'eval-and-replace))

(use-package elisp-slime-nav
  :ensure t
  :commands elisp-slime-nav-mode
  :diminish elisp-slime-nav-mode
  :config
  (add-hook 'emacs-lisp-mode-hook #'elisp-slime-nav-mode))

(use-package org-bullets
  :ensure t
  :config
  (setq org-bullets-bullet-list '("·")))

(use-package ox-reveal
  :ensure t
  :defer t
  :init
  (require 'org-compat))

(use-package org
  :ensure t
  :mode ("\\.org$" . org-mode)
  :bind (("C-c C-x C-o" . org-clock-out)
         ("C-c C-x C-j" . org-clock-goto)
         ("C-c a" . org-agenda))
  :config
  (add-hook 'org-mode-hook #'org-bullets-mode)
  (add-hook 'org-mode-hook #'visual-line-mode)
  (defun dp/org-set-source-code-background (exporter)
    "Insert custom inline css to automatically set the background
of code to whatever theme I'm using's background"
    (when (and (eq exporter 'html)
               (featurep 'htmlize))
      (let* ((my-pre-bg (face-background 'default))
             (my-pre-fg (face-foreground 'default)))
        (setq
         org-html-head-extra
         (concat
          org-html-head-extra
          (format "<style type=\"text/css\">\n pre.src {background-color: %s; color: %s;}</style>\n"
                  my-pre-bg my-pre-fg))))))
  (org-clock-persistence-insinuate)
  (setq org-startup-indented t
        org-agenda-files '("~/Org/journal.org")
        org-startup-folded t
        org-ellipsis " …"
        org-hide-leading-stars t
        org-hide-emphasis-markers t
        org-todo-keywords '((sequence "TODO(t)" "FEEDBACK(f@)" "RECURRING(r)"
                                      "APPROVE(a)" "POSTPONED(p)"
                                      "|"
                                      "DONE(d!/!)" "CANCELLED(c@/!)"))
        org-todo-keyword-faces '(("CANCELLED" :foreground "dark gray" :weight bold)
                                 ("POSTPONED" :foreground "sky blue" :weight bold)
                                 ("FEEDBACK" :foreground "dark orange" :weight bold)
                                 ("APPROVE" :foreground "cornflower blue" :weight bold))
        org-use-fast-todo-selection t
        org-treat-S-cursor-todo-selection-as-state-change nil
        org-clock-persist t
        org-clock-in-resume t
        org-clock-out-remove-zero-time-clocks t
        org-clock-out-when-done t
        org-clock-persist-query-resume t
        org-clock-auto-clock-resolution 'when-no-clock-is-running
        org-clock-report-include-clocking-task t
        org-time-clocksum-format '(:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t)
        org-src-fontify-natively t
        org-src-preserve-indentation t)
  (add-hook 'org-export-before-processing-hook 'dp/org-set-source-code-background))

(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)
         ("C-x f" . dp/git-find-file))
  :config
  (bind-keys
   :map magit-mode-map
   ("M-1" . nil)
   ("M-2" . nil)
   ("M-3" . nil)
   ("M-4" . nil))
  :init
  (progn
    (setq magit-emacsclient-executable
          "/usr/local/Cellar/emacs-mac/HEAD/bin/emacsclient"
          magit-last-seen-setup-instructions "1.4.0"
          magit-highlight-whitespace t
          magit-highlight-trailing-whitespace t
          magit-highlight-indentation nil
          magit-diff-refine-hunk nil
          magit-revert-buffers t)
    (add-hook 'git-commit-mode-hook 'turn-on-flyspell)))

(defun dp/helm-git-grep-symbol-at-point ()
  (interactive)
  (helm-git-grep-1 (symbol-name (symbol-at-point))))

(use-package helm
  :ensure t
  :bind (("C-c g" . helm-git-grep)
         ("C-c C-g" . dp/helm-git-grep-symbol-at-point))
  :config (setq helm-ff-transformer-show-only-basename nil
                helm-ls-git-show-abs-or-relative 'relative
                helm-buffers-fuzzy-matching t
                helm-recentf-fuzzy-matching t
                helm-M-x-fuzzy-match t
                helm-split-window-in-side-p t))

(use-package git-gutter-fringe
  :ensure t
  :defer t)

(use-package git-gutter
  :diminish git-gutter-mode
  :bind (("C-x v s" . git-gutter:stage-hunk)
         ("C-x v r" . git-gutter:revert-hunk)
         ("C-x v p" . git-gutter:popup-hunk))
  :init
  (global-git-gutter-mode t)
  :config
  (require 'git-gutter-fringe)
  (let ((rows (-repeat 32 "XXXX....")))
    (eval
     `(progn
        (fringe-helper-define 'git-gutter-fr:added 'center ,@rows)
        (fringe-helper-define 'git-gutter-fr:deleted 'center ,@rows)
        (fringe-helper-define 'git-gutter-fr:modified 'center ,@rows)))))

(use-package flycheck
  :ensure t
  :commands (flycheck-mode flycheck-add-mode)
  :config
  (flycheck-def-config-file-var flycheck-eslintrc
      javascript-eslint ".eslintrc" :safe #'stringp)

  (defun flycheck-eslint-config-exists-p ()
    "Whether there is an eslint config for the current buffer."
    (let* ((executable (flycheck-find-checker-executable 'javascript-eslint))
           (exitcode (and executable (call-process executable nil nil nil
                                                   "--print-config" "."))))
      (or flycheck-eslintrc
          (eq exitcode 0))))

  (flycheck-define-checker javascript-eslint
    "A Javascript syntax and style checker using eslint.

See URL `https://github.com/eslint/eslint'."
    :command ("eslint" "--format=checkstyle"
              (option "-c" flycheck-eslintrc)
              (option-list "--rulesdir" flycheck-eslint-rules-directories)
              "--stdin" "--stdin-filename" source-original)
    :standard-input t
    :error-parser flycheck-parse-checkstyle
    :error-filter
    (lambda (errors)
      (seq-do (lambda (err)
                ;; Parse error ID from the error message
                (setf (flycheck-error-message err)
                      (replace-regexp-in-string
                       (rx " ("
                           (group (one-or-more (not (any ")"))))
                           ")" string-end)
                       (lambda (s)
                         (setf (flycheck-error-id err)
                               (match-string 1 s))
                         "")
                       (flycheck-error-message err))))
              (flycheck-sanitize-errors errors))
      errors)
    :enabled (lambda () (flycheck-eslint-config-exists-p))
    :modes (js-mode js-jsx-mode js2-mode js2-jsx-mode js3-mode rjsx-mode)
    :next-checkers ((warning . javascript-jscs))
    :verify
    (lambda (_)
      (let* ((default-directory
               (flycheck-compute-working-directory 'javascript-eslint))
             (have-config (flycheck-eslint-config-exists-p)))
        (list
         (flycheck-verification-result-new
          :label "config file"
          :message (if have-config "found" "missing")
          :face (if have-config 'success '(bold error)))))))

  (flycheck-define-checker javascript-flow
    "Static type checking using Flow."
    :command ("flow" "check-contents" "--json" source-original)
    :standard-input t
    :error-parser flycheck-parse-flow
    :next-checkers (javascript-eslint)
    :modes (js2-mode typescript-mode js-mode flowtype-mode))
  (add-to-list 'flycheck-checkers 'javascript-flow))

(defun assoc-set (key value list)
  (append `((,key . ,value)) (assq-delete-all key list)))

(defun assocdr (key list)
  (cdr (assoc key list)))

(defun flycheck-parse-flow (output checker buffer)
  (let ((json-array-type 'list))
    (condition-case nil
        (let ((o (json-read-from-string output)))
          (mapcar #'(lambda (message)
                      (flycheck-error-new
                       :line (assocdr 'line message)
                       :column (assocdr 'start message)
                       :level 'error
                       :message (assocdr 'descr message)
                       :filename (f-relative
                                  (assocdr 'path message)
                                  (f-dirname (file-truename
                                              (buffer-file-name))))
                       :buffer buffer
                       :checker checker))
                  (-mapcat (lambda (err)
                             (let* ((messages (assocdr 'message err))
                                    (first-message (first messages)))
                               (list (assoc-set 'descr
                                                (s-join " " (-map (lambda (message) (assocdr 'descr message)) messages))
                                                first-message))))
                           (assocdr 'errors o))))
      (error
       (message (concat "could not read output: " output))))))

(use-package css-eldoc
  :ensure t
  :commands 'turn-on-css-eldoc
  :config
  (defun css-eldoc-function ()
    (save-excursion
      (back-to-indentation)
      (gethash (symbol-name (symbol-at-point)) css-eldoc-hash-table))))

(use-package scss-mode
  :ensure t
  :mode "\\.scss$"
  :config
  (progn
    (bind-key "C-c c" css-collapse-ruleset scss-mode-map)
    (defun scss-mode-set-comment-syntax () (setq comment-start "//" comment-end ""))
    (add-to-list 'scss-mode-hook 'scss-mode-set-comment-syntax)
    (add-to-list 'scss-mode-hook 'turn-on-css-eldoc)))

(use-package sass-mode
  :ensure t
  :mode "\\.sass$"
  :config
  (progn
    (defun sass-mode-set-comment-syntax () (setq comment-start "//" comment-end ""))
    (add-to-list 'sass-mode-hook 'sass-mode-set-comment-syntax)
    (add-to-list 'sass-mode-hook 'turn-on-css-eldoc)))

(use-package rainbow-mode
  :ensure t
  :diminish rainbow-mode
  :commands rainbow-mode
  :config (setq css-indent-offset 2)
  :init (progn
          (add-hook 'sass-mode-hook 'rainbow-mode)
          (add-hook 'scss-mode-hook 'rainbow-mode)
          (add-hook 'css-mode-hook 'rainbow-mode)))

(use-package dired-details
  :ensure t
  :commands dired-details-install)

(defun dired-back-to-start-of-files ()
  (interactive)
  (backward-char (- (current-column) 2)))

(use-package dired
  :defer t
  :config
  (require 'dp-ediff)
  (bind-keys :map dired-mode-map
             ("C-a" . dired-back-to-start-of-files)
             ("e" . dp/ediff-files))
  (setq dired-listing-switches "-alhF")
  (dired-details-install)
  (setq-default dired-details-hidden-string "--- ")
  (setq dired-dwim-target t)
  (defadvice dired-create-directory
      (after revert-buffer-after-create activate)
    (revert-buffer))
  (defadvice wdired-abort-changes
      (after revert-buffer-after-abort activate)
    (revert-buffer)))

(use-package scala-mode
  :ensure t
  :mode "\\.scala'"
  :config
  (setq scala-indent:align-parameters nil)
  :init
  (add-hook 'scala-mode-hook 'ensime-scala-mode-hook))

(use-package ensime
  :ensure t
  :commands ensime
  :pin melpa-stable)

(use-package subword-mode
  :diminish subword-mode
  :commands subword-mode)

(use-package js2-refactor
  :ensure t
  :commands js2-refactor-mode
  :config
  (progn (js2r-add-keybindings-with-prefix "C-c C-m")))

(use-package json-mode
  :ensure t
  :mode "\\.\\(json\\|babelrc\\|jshintrc\\|eslintrc\\|bowerrc\\|json\\.erb\\|watchmanconfig\\)\\'"
  :config
  (add-hook 'json-mode-hook 'flycheck-mode))

(defun js-to-json (start end &optional arg)
  (interactive "r\nP")
  (let ((js-value (buffer-substring start end)))
    (kill-region start end)
    (insert (with-temp-buffer
              (insert "console.log(JSON.stringify(" js-value "))")
              (call-process-region (point-min) (point-max) "node" t t)
              (when arg (json-mode-beautify))
              (buffer-string)))))

(require 'flowtype-mode)

;; (require 'prettier-js)
;; (setq-default prettier-args '("--single-quote"
;;                               "--no-bracket-spacing"
;;                               "--jsx-bracket-same-line"
;;                               "--trailing-comma=es5"))

(use-package js2-highlight-vars
  :ensure t
  :commands js2-highlight-vars-mode
  :config
  (defun js2--do-highlight-vars-ignore-errors (js2--do-highlight-vars)
    (ignore-errors
      (funcall js2--do-highlight-vars)))
  (advice-add 'js2--do-highlight-vars :around #'js2--do-highlight-vars-ignore-errors))

(use-package js2-mode
  :ensure t
  ;; :mode "\\.js\\'"
  :commands js2-mode
  :bind (:map js2-mode-map
              ([tab] . nil)
              ("C-c C-c" . dp/eproject-run-test-file)
              ("C-w" . backward-kill-word)
              ("M-d" . kill-word)
              ("M-<up>" . js2r-move-line-up)
              ("M-<down>" . js2r-move-line-down))
  :config
  (add-hook 'js2-mode-hook #'subword-mode)
  ;; (remove-hook 'js2-mode-hook #'ac-js2-mode)
  (add-hook 'js2-mode-hook #'js2-refactor-mode)
  (add-hook 'js2-mode-hook #'flycheck-mode)
  (add-hook 'js2-mode-hook #'js2-highlight-vars-mode)
  (setq-default js2r-use-strict t)
  (setq-default js-indent-level 2
                js2-basic-offset 2
                js-switch-indent-offset 2
                js2-bounce-indent-p nil
                js2-include-jslint-globals t))

(use-package rjsx-mode
  :ensure t
  :mode "\\.jsx?\\'"
  :bind (:map rjsx-mode-map
              ([remap rjsx-delete-creates-full-tag] . hungry-delete-forward)))

(defun jsx-indent-line-align-closing-bracket ()
  "Workaround sgml-mode and align closing bracket with opening bracket"
  (save-excursion
    (beginning-of-line)
    (when (looking-at-p "^ +\/?> *$")
      (delete-char sgml-basic-offset))))

(advice-add #'js-jsx-indent-line :after #'jsx-indent-line-align-closing-bracket)

(use-package js-mode
  :config
  (flycheck-disable-checker 'javascript-jshint)
  (flycheck-add-mode 'javascript-eslint 'js-mode))

(use-package tide :ensure t :defer t)

(use-package typescript-mode
  :ensure t
  :mode "\\.tsx?$"
  :config
  (setq-default typescript-indent-level 2)
  (add-hook 'typescript-mode-hook #'subword-mode)
  (add-hook 'typescript-mode-hook #'tide-mode)
  (add-hook 'typescript-mode-hook #'tide-hl-identifier-mode)
  (add-hook 'typescript-mode-hook #'flycheck-mode))

(use-package skewer-mode
  :disabled t
  :commands (skewer-run-phantomjs run-skewer skewer-mode)
  :config
  (setq phantomjs-program-name "/usr/local/bin/phantomjs"))

(use-package hippie-exp
  :config
  (setq hippie-expand-try-functions-list
        '(try-complete-file-name-partially
          try-complete-file-name
          try-expand-dabbrev
          try-expand-dabbrev-all-buffers
          try-expand-dabbrev-from-kill
          try-expand-all-abbrevs
          try-complete-lisp-symbol-partially
          try-complete-lisp-symbol
          try-expand-list
          try-expand-line)))

(use-package elm-mode
  :ensure t
  :mode "\\.elm$"
  :config
  ;; (setq elm-compile-arguments '("--yes" "--output=elm.js"))
  (setq elm-indent-offset 4)
  (setq elm-format-on-save t)
  (setq elm-format-command "elm-format-0.18")
  (add-hook 'elm-mode-hook #'elm-oracle-setup-completion)
  (flycheck-elm-setup)
  (add-to-list 'elm-mode-hook #'flycheck-mode)
  (add-to-list 'company-backends 'company-elm))

(use-package flycheck-elm
  :ensure t
  :commands flycheck-elm-setup)

(use-package eproject
  :ensure t
  :diminish eproject-mode
  :commands define-project-type
  :init
  (require 'flycheck)
  (require 'dp-eproject)
  (define-project-type maven (generic)
    (look-for "pom.xml")
    :relevant-files ("\\.scala$" "\\.java$" "\\.xml$"))
  (eproject-set-key "t" 'eproject-tasks-run)
  (eproject-set-key "s" 'eproject-open-term)
  (add-hook 'generic-project-file-visit-hook 'eproject-set-local-keys)
  (add-hook 'generic-project-file-visit-hook 'eproject-eslint)
  (add-hook 'generic-project-file-visit-hook 'eproject-jshint)
  (add-hook 'generic-project-file-visit-hook 'eproject-flowtype)
  (add-hook 'generic-git-project-file-visit-hook 'eproject-set-git-generic-keys)
  ;;
  (remove-hook 'after-change-major-mode-hook #'eproject--after-change-major-mode-hook))

(use-package php-mode
  :ensure t
  :mode "\\.\\(php[s345]?\\|inc\\|phtml\\)"
  :config
  (bind-keys
   :map php-mode-map
   ("C-c j" . dp/php-jump-to-function-definition)
   ("C-}" . dp/php-next-function)
   ("C-{" . dp/php-previous-function))

  (require 'flycheck)
  (add-to-list 'flycheck-disabled-checkers 'php-phplint)

  (defun basic-php-settings ()
    (setq tab-width 2
          c-basic-offset 2))

  (add-to-list 'php-mode-hook #'basic-php-settings)
  (add-to-list 'php-mode-hook #'flycheck-mode-on-safe))

(use-package nxml-mode
  :defer t
  :config
  (add-to-list 'rng-schema-locating-files "~/.emacs.d/schemas.xml")
  (setq nxml-child-indent 4
        nxml-attribute-indent 4))

(use-package utop
  :ensure t
  :commands (utop utop-eval-region utop-eval-phrase utop-eval-buffer)
  :init
  (defun utop-eval-dwim ()
    (interactive)
    (if (use-region-p)
        (utop-eval-region (region-beginning) (region-end))
      (utop-eval-phrase)))
  :config
  (setq utop-command "opam config exec -- utop -emacs"
        utop-skip-after-eval-phrase t))

(add-to-list 'auto-mode-alist '("\\<jbuild\\'" . lisp-mode))
(add-to-list 'auto-mode-alist '("\\<butler\\'" . lisp-mode))

(use-package tuareg
  :ensure t
  :mode ("\\.ml\\'" . tuareg-mode)
  :bind (:map tuareg-mode-map
              ("C-c C-l" . utop-eval-buffer)
              ("C-c C-c" . utop-eval-dwim))
  :config
  (setq *opam-share*
        (substring (shell-command-to-string "opam config var share 2> /dev/null") 0 -1))
  (add-to-list 'load-path (concat *opam-share* "/emacs/site-lisp"))

  ;; ocp
  (require 'ocp-indent)
  (ocp-setup-indent)

  ;; merlin
  (autoload 'merlin-mode "merlin" nil t nil)
  (eval-after-load "merlin"
    (progn
      (define-key merlin-mode-map (kbd "M-.") #'merlin-locate)
      (define-key merlin-mode-map (kbd "M-,") #'merlin-pop-stack)))
  (add-hook 'tuareg-mode-hook #'merlin-mode t)
  (add-hook 'caml-mode-hook #'merlin-mode t)
  (setq merlin-command 'opam))

;; (quelpa '(reason-mode :repo "reasonml-editor/reason-mode" :fetcher github :stable t))

(use-package mmm-mode
  :ensure t
  :commands (mmm-mode mmm-mode-on)
  :config
  (require 'mmm-defaults)
  (require 'mmm-vars)
  (require 'mmm-erb)
  (setq mmm-global-mode nil)
  (mmm-add-mode-ext-class 'sgml-mode nil 'html-js)
  (mmm-add-mode-ext-class 'sgml-mode nil 'html-css)
  (mmm-add-mode-ext-class 'json-mode "\\.json\\.erb\\'" 'erb))

(use-package web-mode
  :ensure t
  :config
  (setq-default
   web-mode-javascript-indent-offset 2
   web-mode-code-indent-offset 2
   web-mode-css-indent-offset 2
   web-mode-markup-indent-offset 2
   web-mode-style-padding 2
   web-mode-block-padding 2
   web-mode-script-padding 2)
  (add-to-list 'auto-mode-alist '("\\.ejs\\'" . web-mode)))

(use-package hl-tags-mode
  :commands hl-tags-mode
  :config
  (set-face-attribute 'hl-tags-face nil :underline nil))

(use-package sgml-mode
  :mode "\\.html?\\'"
  :bind (:map sgml-mode-map
              ("C-c =" . mc/mark-sgml-tag-pair)
              ("C-}" . sgml-skip-tag-forward)
              ("C-{" . sgml-skip-tag-backward))
  :init
  (add-hook 'sgml-mode-hook #'hl-tags-mode)
  :config
  (defadvice sgml-delete-tag (after reident activate)
    (indent-region (point-min) (point-max))))

(use-package yasnippet
  :ensure t
  :commands (yas-minor-mode yas-global-mode)
  :init
  (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
  (setq yas-prompt-functions '(yas-ido-prompt yas-completing-prompt))
  :config
  (yas-reload-all)
  (define-key yas-minor-mode-map (kbd "C-;") 'yas-expand)
  (define-key yas-minor-mode-map [tab] nil)
  (add-hook 'html-mode-hook 'yas-minor-mode))

(use-package log-edit
  :defer t
  :config (add-hook 'log-edit-mode-hook 'flyspell-mode))

(use-package olivetti
  :disabled t
  :ensure t
  :commands olivetti-mode
  :init (setq-default olivetti-body-width 120))

(use-package vc-svn
  :config
  (defadvice vc-svn-checkin
      (before escape-filepaths
              (files comment &optional extra-args-ignored)
              activate)
    "If filename includes @, add @ at the end of it."
    (ad-set-arg 0 (mapcar (lambda (file)
                            (if (string-match "@" file)
                                (concat file "@")
                              file))
                          (ad-get-arg 0))))
  (defadvice vc-svn-register
      (before escape-filepaths
              (files comment &optional extra-args-ignored)
              activate)
    "If filename includes @, add @ at the end of it."
    (ad-set-arg 0 (mapcar (lambda (file)
                            (if (string-match "@" file)
                                (concat file "@")
                              file))
                          (ad-get-arg 0)))))

(use-package csv-mode
  :ensure t
  :mode "\\.csv\\'")

(use-package yaml-mode
  :ensure t
  :mode "\\.yml\\'")

(use-package command-log-mode
  :ensure t
  :defer t
  :config
  (setq command-log-mode-auto-show t))

(use-package evil
  :disabled t
  :ensure t
  :defer t)

(use-package inf-lisp
  :defer t
  :config
  (setq-default inferior-lisp-program "sbcl"))

;; (require 'dp-php)
;; (require 'dp-haskell)

(when (window-system)
  (when (fboundp 'mac-auto-operator-composition-mode)
    (mac-auto-operator-composition-mode t))
  (custom-set-faces
   `(default ((t (:background "black" :foreground "white"))))
   `(cursor ((t (:background "red")))))
  ;; (remove-hook 'post-self-insert-hook #'set-cursor-according-to-mode)
  ;; (setq set-cursor-according-to-mode-timer
  ;;       (run-with-idle-timer 1 t #'set-cursor-according-to-mode 'box))
  ;; (cancel-timer set-cursor-according-to-mode-timer)
  (setq default-frame-alist '((font . "Consolas-13")))
  (set-frame-font (font-spec :family "Consolas" :size 13)))

(dp/update-environment)

;;; init.el ends here
