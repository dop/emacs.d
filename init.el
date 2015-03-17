;;; init.el

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(add-to-list 'load-path "~/.emacs.d/lisp")
(add-to-list 'custom-theme-load-path "~/.emacs.d/theme")

(require 'dp-functions)

(set-global-font "DejaVu LGC Sans Mono-12")

(scroll-bar-mode -1)
(tool-bar-mode -1)
;; (menu-bar-mode 1)

(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)

(setq mac-option-modifier 'alt)
(setq mac-command-modifier 'meta)

(setq gc-cons-threshold (* 20 1024 1024))

;; UTF-8 please
(set-language-environment       "UTF-8")
(setq locale-coding-system      'utf-8)
(set-terminal-coding-system     'utf-8)
(set-keyboard-coding-system     'utf-8)
(set-selection-coding-system    'utf-8)
(prefer-coding-system           'utf-8)

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

(setq display-time-24hr-format nil
      display-time-day-and-date nil)

(setq visible-bell nil
      ring-bell-function 'dp/terminal-visible-bell)

(fset 'yes-or-no-p 'y-or-n-p)

(line-number-mode t)
(column-number-mode t)
(electric-pair-mode t)

(show-paren-mode 1)
(setq show-paren-style 'parenthesis)

(setq require-final-newline t
      mode-require-final-newline t)

(delete-selection-mode t)

(setq-default indicate-empty-lines t)
(setq-default indicate-buffer-boundaries 'left)

(setq-default indent-tabs-mode nil)

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

(add-hook 'post-command-hook 'set-cursor-according-to-mode t)
;; (defun set-cursor-according-to-mode () (setq cursor-type 'box))
;; (remove-hook 'post-command-hook 'set-cursor-according-to-mode)

(defun turn-on-show-trailing-whitespace ()
  (set-variable 'show-trailing-whitespace t))

(add-hook 'prog-mode-hook 'turn-on-show-trailing-whitespace)
(add-hook 'html-mode-hook 'turn-on-show-trailing-whitespace)
(add-hook 'css-mode-hook 'turn-on-show-trailing-whitespace)

(require 'cl)
(require 'package)

(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa" . "http://melpa.org/packages/")
        ("org" . "http://orgmode.org/elpa/")
        ;; ("marmalade" . "http://marmalade-repo.org/packages/")
        ))

(setq package-enable-at-startup nil)
(package-initialize)

(require 'use-package)
(require 'dp-key-bindings)

(use-package server
  :init (unless (server-running-p) (server-start)))

(use-package moe-theme
  :if window-system
  :init (switch-to-theme 'moe-dark)
  :config (custom-theme-set-faces
           'moe-dark
           '(git-gutter:added ((t (:inherit fringe :foreground "#d7ff5f"))))
           '(git-gutter:deleted ((t (:inherit fringe :foreground "#ffafaf"))))
           '(git-gutter:modified ((t (:inherit fringe :foreground "deep sky blue"))))
           '(fringe ((t (:background "#303030"))))))

(use-package solarized-theme
  :disabled t
  :if window-system
  :init (progn
          (setq solarized-use-less-bold t
                solarized-use-more-italic t
                solarized-high-contrast-mode-line t
                solarized-use-variable-pitch t)
          (switch-to-theme 'solarized-dark)))

(use-package leuven-theme
  ;; :disabled t
  :if window-system
  :config (custom-theme-set-faces
           'leuven
           '(mode-line ((t (:box (:line-width 1 :color "deep sky blue")
                                 :background "deep sky blue"
                                 :foreground "#ABDFFA"))))
           '(mode-line-inactive ((t (:box (:line-width 1 :color "gainsboro")
                                          :foreground "#F0F0EF"
                                          :background "gainsboro"))))
           '(vertical-border ((t (:foreground "white")))))
  :init (switch-to-theme 'leuven))

(use-package exec-path-from-shell
  :init (when (memq window-system '(mac ns))
          (exec-path-from-shell-initialize)))

(use-package hl-line
  :disabled t
  :commands hl-line-mode
  :init (progn
          (add-hook 'prog-mode-hook 'hl-line-mode)
          (add-hook 'html-mode-hook 'hl-line-mode)))

(use-package column-enforce-mode
  :diminish column-enforce-mode
  :commands column-enforce-mode
  :init (add-hook 'prog-mode-hook 'column-enforce-mode)
  :config (set-face-attribute 'column-enforce-face nil :underline nil))

(use-package editorconfig)

(use-package ispell
  ;; :commands (ispell-word flyspell-buffer flyspell-mode)
  :config (setq ispell-local-dictionary-alist
                  '((nil "[a-zA-Z]" "[^a-zA-Z]" "[']" nil ("-l" "en") nil utf-8)
                    ("english" "[a-zA-Z]" "[^a-zA-Z]" "[']" nil ("-l" "en") nil utf-8)
                    ;; ("british" "[a-zA-Z]" "[^a-zA-Z]" "[']" nil ("-l" "en") nil utf-8)
                    ("lithuanian" "[a-zA-ZąčęėįšųūžĄČĘĖĮŠŲŪŽ]" "[^a-zA-ZąčęėįšųūžĄČĘĖĮŠŲŪŽ]" "" nil ("-l" "lt") nil utf-8))
                  ispell-program-name "/usr/local/bin/aspell"))

(use-package multiple-cursors
  :bind (("C-M-." . mc/mark-next-like-this)
         ("C-M-," . mc/skip-to-next-like-this)))

(use-package whitespace-cleanup-mode
  :diminish whitespace-cleanup-mode
  :init (global-whitespace-cleanup-mode 1))

(use-package macrostep
  :commands macrostep-expand
  :init (bind-key "C-c e" 'macrostep-expand emacs-lisp-mode-map))

(use-package saveplace
  :config (progn
            (setq-default save-place t)
            (setq save-place-file "~/.emacs.d/data/places")))

(use-package uniquify
  :config (setq uniquify-buffer-name-style 'forward))

(use-package expand-region
  :bind ("C-=" . er/expand-region))

;; (require 'drag-stuff)
;; (drag-stuff-global-mode t)

(use-package wrap-region
  :diminish wrap-region-mode
  :init (wrap-region-global-mode))

(use-package visual-regexp
  :bind ("C-c r" . vr/replace))

(use-package company
  :diminish company-mode
  :init (progn
          (global-company-mode)
          (bind-keys :map company-active-map
                     ("C-w" . nil)
                     ("C-n" . company-select-next)
                     ("C-p" . company-select-previous))))

(use-package dictionary
  :bind ("C-c s" . dictionary-search))

(use-package flx-isearch
  :bind (("C-M-s" . flx-isearch-forward)
         ("C-M-r" . flx-isearch-backward)))

(defun dp/ido-keys nil
  (define-key ido-completion-map " " 'ido-restrict-to-matches))

(use-package ido
  ;; :disabled t
  :init (ido-mode t)
  :config
  (progn
    (icomplete-mode)
    (ido-everywhere t)
    (ido-ubiquitous-mode t)
    (ido-vertical-mode t)
    (flx-ido-mode t)
    (add-to-list 'ido-setup-hook 'dp/ido-keys)
    (setq ido-enable-flex-matching t
          ido-use-faces nil
          ido-create-new-buffer 'always
          ido-use-filename-at-point 'guess
          ido-default-file-method 'selected-window
          ido-default-buffer-method 'selected-window)))

(use-package smex)

;;; Org mode
(use-package org
  :mode ("\\.org$" . org-mode)
  ;; :bind (("C-c l" . org-store-link)
  ;;        ("C-c a" . org-agenda))
  :config (progn
            (add-hook 'org-mode-hook 'typo-mode)
            (org-clock-persistence-insinuate)
            (setq org-startup-indented t
                  org-hide-leading-stars t
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
                  org-src-preserve-indentation t)))

(use-package magit
  :diminish magit-auto-revert-mode
  :bind ("C-x g" . magit-status)
  :config
  (progn
    (bind-keys :map magit-mode-map
               ("C-x f" . dp/git-find-file))
    (setq magit-emacsclient-executable
          "/usr/local/Cellar/emacs-mac/HEAD/bin/emacsclient")
    (add-hook 'git-commit-mode-hook 'turn-on-flyspell)))

(use-package helm
  :disabled t
  :diminish helm-mode
  :bind (("C-x b"   . helm-mini)
         ("C-x C-b" . helm-buffers-list)
         ("C-x C-f" . helm-find-files)
         ("C-x f"   . helm-ls-git-ls)
         ("M-x"     . helm-M-x)
         ("C-c h g" . helm-git-grep))
  :config (setq helm-ff-transformer-show-only-basename nil
                helm-ls-git-show-abs-or-relative 'relative
                helm-buffers-fuzzy-matching t
                helm-recentf-fuzzy-matching t
                helm-M-x-fuzzy-match t
                helm-split-window-in-side-p t)
  :init (progn
          (helm-mode t)
          (helm-autoresize-mode t)))

(use-package git-gutter-fringe
  :bind (("C-x v s" . git-gutter:stage-hunk)
         ("C-x v r" . git-gutter:revert-hunk))
  :init (global-git-gutter-mode t)
  :config
  (progn
    (fringe-helper-define 'git-gutter-fr:added 'center
      "........"
      "...XX..."
      "...XX..."
      ".XXXXXX."
      ".XXXXXX."
      "...XX..."
      "...XX..."
      "........")
    (fringe-helper-define 'git-gutter-fr:deleted 'center
      "........"
      "........"
      "........"
      ".XXXXXX."
      ".XXXXXX."
      "........"
      "........"
      "........")
    (fringe-helper-define 'git-gutter-fr:modified 'center
      "........"
      "........"
      "..XXXX.."
      "..XXXX.."
      "..XXXX.."
      "..XXXX.."
      "........"
      "........")))

(use-package flycheck
  :commands flycheck-mode)

(use-package rainbow-mode
  :commands rainbow-mode
  :config (setq css-indent-offset 2)
  :init (progn
          (add-hook 'sass-mode-hook 'rainbow-mode)
          (add-hook 'scss-mode-hook 'rainbow-mode)
          (add-hook 'css-mode-hook 'rainbow-mode)))

(use-package sass-mode
  :mode "\\.s\\(a\\|c\\)ss$")

;;; Dired

(use-package dired-details
  :commands dired-details-install)

(defun dired-back-to-start-of-files ()
  (interactive)
  (backward-char (- (current-column) 2)))

(use-package dired
  :config (progn
            (dired-details-install)
            (setq-default dired-details-hidden-string "--- ")
            (defadvice dired-create-directory
                (after revert-buffer-after-create activate)
              (revert-buffer))
            (defadvice wdired-abort-changes
                (after revert-buffer-after-abort activate)
              (revert-buffer)))
  :init (bind-keys :map dired-mode-map
                   ("C-a" . dired-back-to-start-of-files)))

(use-package ace-window
  :bind ("C-x o" . ace-window)
  :config (progn
            (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
                  aw-background nil)
            (custom-set-faces
             '(aw-leading-char-face
               ((t (:inherit ace-jump-face-foreground :height 3.0)))))))

(use-package cap-words
  :diminish capitalized-words-mode
  :commands capitalized-words-mode)

(use-package js2-refactor
  :config (progn
            (js2r-add-keybindings-with-prefix "C-c C-m")
            (setq-default js2r-use-strict t)))

(use-package js2-mode
  :mode "\\.js$"
  :config (progn
            (setq-default
             js-indent-level 2
             js2-basic-offset 2
             js2-include-jslint-globals t)
            (bind-keys :map js2-mode-map
                       ("M-<up>" . js2r-move-line-up)
                       ("M-<down>" . js2r-move-line-down))
            (use-package js2-refactor)
            (add-hook 'js2-mode-hook 'capitalized-words-mode)))

(use-package eproject
  :commands define-project-type
  :config (progn
            (require 'dp-eproject)
            (eproject-set-key "t" 'eproject-tasks-run)
            (eproject-set-key "s" 'eproject-open-shell)
            (add-to-list 'generic-project-file-visit-hook 'eproject-set-local-keys)
            (add-to-list 'generic-project-file-visit-hook 'eproject-jshint)
            (add-to-list 'generic-git-project-file-visit-hook
                         'eproject-set-git-generic-keys)))

(use-package php-mode
  :mode "\\.\\(php[s345]?\\|inc\\|phtml\\)"
  :config (bind-keys
           :map php-mode-map
           ("C-c j" . dp/php-jump-to-function-definition)
           ("C-}" . dp/php-next-function)
           ("C-{" . dp/php-previous-function)))

(use-package nxml-mode
  :config (add-to-list 'rng-schema-locating-files "~/.emacs.d/schemas.xml"))

(use-package html-mode
  :mode "\\.html?\\'"
  :config (bind-keys
           :map sgml-mode-map
           ("C-c =" mc/mark-sgml-tag-pair)
           ("C-}" sgml-skip-tag-forward)
           ("C-{" sgml-skip-tag-backward)))

;; (use-package web-mode
;;   :mode "\\.vm?\\'")

(use-package log-edit
  :config (add-hook 'log-edit-mode-hook 'flyspell-mode))

(use-package vc-svn
  :config (progn
            (defadvice vc-svn-checkin
                (before escape-filepaths
                        (files rev comment &optional extra-args-ignored)
                        activate)
              "If filename includes @, add @ at the end of it."
              (ad-set-arg 0 (mapcar (lambda (file)
                                      (if (string-match "@" file)
                                          (concat file "@")
                                        file))
                                    (ad-get-arg 0))))
            (defadvice vc-svn-register
                (before escape-filepaths
                        (files rev comment &optional extra-args-ignored)
                        activate)
              "If filename includes @, add @ at the end of it."
              (ad-set-arg 0 (mapcar (lambda (file)
                                      (if (string-match "@" file)
                                          (concat file "@")
                                        file))
                                    (ad-get-arg 0))))))

(require 'dp-php)
(require 'dp-haskell)

;;; init.el ends here
