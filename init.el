;;; init.el

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(add-to-list 'load-path "~/.emacs.d/lisp")

(require 'dp-functions)

(set-face-attribute
 'default nil
 :family "Source Code Pro"
 :height 120
 :weight 'regular)

(set-fontset-font t '(61440 . 61980) "FontAwesome")

(disable-theme 'user)

;; (custom-theme-set-faces
;;  'user
;;  '(default ((t (:background "gray20" :foreground "snow2"))))
;;  '(diff-added ((t (:foreground "olive drab"))))
;;  '(diff-removed ((t (:foreground "tomato"))))
;;  '(git-gutter:added ((t (:foreground "yellow green"))))
;;  '(git-gutter:deleted ((t (:foreground "coral"))))
;;  '(git-gutter:modified ((t (:foreground "light sky blue"))))
;;  '(magit-item-highlight ((t (:background "gray25"))))
;;  '(fringe ((t (:background "gray20"))))
;;  '(hl-line ((t (:background "gray25"))))
;;  '(vertical-border ((t (:foreground "gray25")))))

(setq dp/custom-user-faces
      '((default . (:background "snow2" :foreground "gray20"))
        (diff-added . (:foreground "olive drab"))
        (diff-removed . (:foreground "tomato"))
        (git-gutter:added . (:foreground "yellow green"))
        (git-gutter:deleted . (:foreground "coral"))
        (git-gutter:modified . (:foreground "light sky blue"))
        (magit-item-highlight . (:background "cornsilk"))
        (vertical-border . (:foreground "dark gray"))
        (fringe . (:background "gray10"))))

;; (dp/apply-custom-user-faces)
;; (dp/reset-custom-user-faces)

(scroll-bar-mode -1)
(tool-bar-mode -1)
;; (menu-bar-mode 1)

(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)

(setq mac-option-modifier 'alt)
(setq mac-command-modifier 'meta)

(setq gc-cons-threshold (* 20 1024 1024))

;; Put system clipboard contents into kill ring before killing so that it's not
;; lost.
(setq save-interprogram-paste-before-kill t)

(setq-default left-fringe-width 11
              right-fringe-width 11)

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
(winner-mode t)

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

(add-hook 'emacs-lisp-mode-hook (lambda () (setq mode-name "elisp")))

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
(require 'dp-window)
(require 'dp-mode-line)
(require 'dp-key-bindings)

(use-package server
  :init (unless (server-running-p) (server-start)))

(use-package moe-theme
  :if window-system
  :init (switch-to-theme 'moe-dark)
  :config
  (progn
    (custom-theme-set-faces
     'moe-dark
     '(hl-line ((t (:background "#3a3a3a"))))
     '(flyspell-incorrect ((t (:foreground unspecified :background unspecified
                               :underline (:color "#ef2929" :style wave)))))
     '(magit-item-highlight ((t (:background "#444444"))))
     '(git-gutter:added ((t (:inherit fringe :foreground "#d7ff5f"))))
     '(git-gutter:deleted ((t (:inherit fringe :foreground "tomato"))))
     '(git-gutter:modified ((t (:inherit fringe :foreground "deep sky blue"))))
     '(fringe ((t (:background "#303030")))))
    (custom-theme-set-variables
     'moe-dark
     '(ansi-color-names-vector
       ["#303030" "#ef2929" "#afff00" "#fce94f" "#5fafd7"
        "#ffafd7" "#87ffff" "#C6C6C6"]))))

(use-package solarized-theme
  :disabled t
  :if window-system
  :init
  (progn
    (setq solarized-use-less-bold t
          solarized-use-more-italic nil
          solarized-high-contrast-mode-line t
          solarized-use-variable-pitch t)
    (switch-to-theme 'solarized-light)))

(use-package leuven-theme
  :disabled t
  :if window-system
  :init (switch-to-theme 'leuven)
  :config
  (progn
    (custom-theme-set-faces
     'leuven
     '(default ((t (:background "white" :foreground "#333333"))))
     '(mode-line ((t (:box (:line-width 2 :color "deep sky blue")
                           :background "deep sky blue"
                           :foreground "#ABDFFA"))))
     '(mode-line-inactive ((t (:box (:line-width 2 :color "gainsboro")
                                    :foreground "#F0F0EF"
                                    :background "gainsboro"))))
     '(vertical-border ((t (:foreground "#f0f0ef")))))
    (custom-theme-set-variables
     'leuven
     '(ansi-color-faces-vector
       [default default default italic underline success warning error])
     '(ansi-color-names-vector
       ["black" "red3" "ForestGreen" "yellow3" "blue" "magenta3" "DeepSkyBlue" "gray50"]))))

(use-package railscasts-theme
  :disabled t
  :if window-system
  :init (switch-to-theme 'railscasts)
  :config
  (custom-theme-set-faces
   'railscasts
   '(diff-added ((t (:foreground "#A5C261"))))
   '(diff-removed ((t (:foreground "LightSalmon"))))
   '(magit-item-highlight ((t (:background "#404040"))))
   '(aw-leading-char-face ((t (:foreground "red"))))
   '(mode-line-buffer-id ((t (:bold t :foreground "#232323" :background "#A5BAF1"))))
   '(mode-line-inactive ((t (:box nil :background "#404040"))))
   '(vertical-border ((t (:foreground "#404040")))))
  (custom-theme-set-variables
   'railscasts
   '(ansi-color-names-vector
     ["#232323" "LightSalmon" "#A5C261" "#FFC66D" "#3070FF"
      "#FF00FF" "#22EEEE" "#E6E1DC"])))

(use-package cyberpunk-theme
  :disabled t
  :if window-system
  :init (switch-to-theme 'cyberpunk)
  :config (custom-theme-set-faces
           'cyberpunk
           '(mode-line
             ((t (:foreground "#4c83ff"
                  :background "#333333"
                  :box (:line-width 1 :color "#333333")))))
           '(mode-line-inactive
             ((t (:foreground "#4d4d4d"
                  :background "#1a1a1a"
                  :box (:line-width 1 :color "#1a1a1a")))))))

(use-package exec-path-from-shell
  :init (when (memq window-system '(mac ns))
          (exec-path-from-shell-initialize)))

(use-package hl-line
  :commands hl-line-mode
  :init (progn
          (add-hook 'prog-mode-hook 'hl-line-mode)
          (add-hook 'html-mode-hook 'hl-line-mode)))

(use-package undo-tree
  :diminish undo-tree-mode
  :init (global-undo-tree-mode t))

(use-package column-enforce-mode
  :diminish column-enforce-mode
  :commands column-enforce-mode
  :init (add-hook 'prog-mode-hook 'column-enforce-mode))

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

(use-package rainbow-delimiters
  :config (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode))

(use-package eldoc
  :diminish eldoc-mode
  :commands eldoc-mode
  :config (add-hook 'emacs-lisp-mode-hook 'eldoc-mode))

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
          (global-company-mode t)
          (bind-keys :map company-active-map
                     ("C-w" . nil)
                     ([return] . nil)
                     ([tab] . company-complete-selection)
                     ("TAB" . company-complete-selection)
                     ("RET" . nil)
                     ("C-n" . company-select-next)
                     ("C-p" . company-select-previous)))
  :config (setq company-idle-delay 0.1))

(use-package dictionary
  :bind ("C-c s" . dictionary-search))

(use-package flx-isearch
  :bind (("C-M-s" . flx-isearch-forward)
         ("C-M-r" . flx-isearch-backward)))

(defun dp/ido-keys ()
  (define-key ido-completion-map " " 'ido-restrict-to-matches))

(use-package ido
  ;; :disabled t
  :init (ido-mode t)
  :config
  (progn
    (setq ido-vertical-decorations
          '("\n → "
            "" "\n   " "\n   …" "[" "]" " [No match]" " [Matched]"
            " [Not readable]" " [Too big]" " [Confirm]" "\n → " ""))
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
               ("C-x f" . dp/git-find-file)
               ("M-1" . nil)
               ("M-2" . nil)
               ("M-3" . nil)
               ("M-4" . nil))
    (setq magit-emacsclient-executable
          "/usr/local/Cellar/emacs-mac/HEAD/bin/emacsclient")
    (add-hook 'git-commit-mode-hook 'turn-on-flyspell)))

(use-package helm
  :diminish helm-mode
  :bind (("C-c g" . helm-git-grep)
         ("C-c o" . helm-swoop))
  :config (setq helm-ff-transformer-show-only-basename nil
                helm-ls-git-show-abs-or-relative 'relative
                helm-buffers-fuzzy-matching t
                helm-recentf-fuzzy-matching t
                helm-M-x-fuzzy-match t
                helm-split-window-in-side-p t))

(use-package git-gutter
  :diminish git-gutter-mode
  :bind (("C-x v s" . git-gutter:stage-hunk)
         ("C-x v r" . git-gutter:revert-hunk)
         ("C-x v p" . git-gutter:popup-hunk))
  :init (progn
          (require 'git-gutter-fringe)
          (global-git-gutter-mode t))
  :config
  (progn
    (fringe-helper-define 'git-gutter-fr:added 'center
      "........"
      "...X...."
      "...X...."
      ".XXXXX.."
      "...X...."
      "...X...."
      "........"
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
      "..X..X.."
      "..X..X.."
      "..XXXX.."
      "........"
      "........")))

(use-package flycheck
  :commands flycheck-mode)

(use-package css-eldoc
  :commands 'turn-on-css-eldoc
  :init
  (defun css-eldoc-function ()
    (save-excursion
      (back-to-indentation)
      (gethash (symbol-name (symbol-at-point)) css-eldoc-hash-table))))

(use-package sass-mode
  :mode "\\.s\\(a\\|c\\)ss$"
  :config
  (progn
    (defvar sass-collapse-ruleset
      [?\C-r ?\{ return ?\C-= ?\C-c ?r ?  ?+ backspace backspace ?\C-q ?\C-j ?  ?+ return ?  return])
    (bind-key "C-c c" sass-collapse-ruleset sass-mode-map)
    (defun sass-mode-set-comment-syntax ()
      (setq comment-start "//" comment-end ""))
    (add-to-list 'sass-mode-hook 'sass-mode-set-comment-syntax)
    (add-to-list 'sass-mode-hook 'turn-on-css-eldoc)))

(use-package rainbow-mode
  :diminish t
  :commands rainbow-mode
  :config (setq css-indent-offset 2)
  :init (progn
          (add-hook 'sass-mode-hook 'rainbow-mode)
          (add-hook 'scss-mode-hook 'rainbow-mode)
          (add-hook 'css-mode-hook 'rainbow-mode)))

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

(use-package cap-words
  :diminish capitalized-words-mode
  :commands capitalized-words-mode)

(use-package js2-mode
  :mode "\\.js$"
  :init (add-hook 'js2-mode-hook (lambda () (setq mode-name "js²")))
  :config
  (progn
    (require 'tern)
    (require 'js2-refactor)
    (js2r-add-keybindings-with-prefix "C-c C-m")
    (setq-default js2r-use-strict t)
    (setq-default js-indent-level 2
                  js2-basic-offset 2
                  js2-include-jslint-globals t)
    (bind-keys :map js2-mode-map
               ("C-c d" . dp/toggle-ddescribe)
               ("C-c i" . dp/toggle-iit)
               ("M-<up>" . js2r-move-line-up)
               ("M-<down>" . js2r-move-line-down))
    (wrap-region-add-wrapper "_" "_" nil 'js2-mode)
    (add-hook 'js2-mode-hook 'capitalized-words-mode)))

(use-package coffee-mode
  :mode "\\.coffee$")

(use-package eproject
  :diminish eproject-mode
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
           :map html-mode-map
           ("C-c =" . mc/mark-sgml-tag-pair)
           ("C-}" . sgml-skip-tag-forward)
           ("C-{" . sgml-skip-tag-backward)))

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
