;;; init.el

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(add-to-list 'load-path "~/.emacs.d/lisp")

(require 'color)
(require 'dp-functions)

(when (window-system)
  (set-frame-font (font-spec :family "Inconsolata LGC" :size 12 :weight 'normal)))

;; (custom-theme-set-faces
;;  'user
;;  `(cursor ((t (:background "red"))))
;;  `(vertical-border ((t (:foreground "gray30"))))
;;  `(default ((t (:foreground "white" :background "#232624"))))
;;  `(fringe ((t (:foreground "white" :background "#232624"))))
;;  `(font-lock-comment-face ((t (:foreground "gray55"))))
;;  `(font-lock-string-face ((t (:foreground "cornsilk"))))
;;  `(font-lock-doc-face ((t (:foreground "sandy brown"))))
;;  `(font-lock-function-name-face ((t (:foreground "aquamarine"))))
;;  `(font-lock-variable-name-face ((t (:foreground "white"))))
;;  `(font-lock-keyword-face ((t (:foreground "moccasin"))))
;;  `(dired-directory ((t (:foreground "moccasin")))))

(set-fontset-font t '(61440 . 61980) "FontAwesome")

(setq insert-directory-program "/usr/local/bin/gls")
(setq dired-listing-switches "-alGhF --group-directories-first")

(setq-default comint-input-ignoredups t)

;; No more symlinks to indicated that file is being edited.
(setq create-lockfiles nil)

(scroll-bar-mode -1)
(tool-bar-mode -1)
(when (not (window-system)) (menu-bar-mode -1))

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

;; increate text by 1
(setq text-scale-mode-step 1.1)

(add-hook 'post-command-hook 'set-cursor-according-to-mode t)
;; (defun set-cursor-according-to-mode () (setq cursor-type 'box))
;; (remove-hook 'post-command-hook 'set-cursor-according-to-mode)

(defun turn-on-show-trailing-whitespace ()
  (set-variable 'show-trailing-whitespace t))

(add-hook 'prog-mode-hook 'turn-on-show-trailing-whitespace)
(add-hook 'html-mode-hook 'turn-on-show-trailing-whitespace)
(add-hook 'css-mode-hook 'turn-on-show-trailing-whitespace)

(add-hook 'emacs-lisp-mode-hook (lambda () (setq mode-name "elisp")))
(add-hook 'emacs-lisp-mode-hook 'elisp-slime-nav-mode)

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

;; (use-package server
;;   :init (unless (server-running-p) (server-start)))

(use-package monokai :if window-system :init (switch-to-theme 'monokai))

(use-package darcula :disabled t :if window-system :init (switch-to-theme 'darcula))

(use-package zenburn
  :disabled t
  :if window-system
  :init
  (switch-to-theme 'zenburn)
  (custom-theme-set-faces
   'zenburn
   `(isearch ((t (:bold nil :inherit highlight))))
   `(lazy-highlight ((t (:background "#7B6000"))))
   `(highlight ((t (:background "#CB4B16"))))
   `(font-lock-comment-face ((t (:foreground "#7F9F7F" :slant italic))))
   `(font-lock-variable-name-face ((t (:foreground unspecified))))
   `(font-lock-keyword-face ((t (:foreground "#F0DFAF" :bold nil))))
   `(font-lock-builtin-face ((t (:bold nil :slant italic :inherit default))))
   `(vertical-border ((t (:foreground "#4F4F4F"))))
   `(fringe ((t (:background "#3F3F3F"))))
   `(js2-error ((t (:background "#AA0000"))))
   `(js2-warning ((t (:background ,(color-darken-name "#7b6000" 5) :underline nil))))
   `(js2-external-variable ((t (:background ,(color-darken-name "#CB4B16" 10)))))
   `(org-level-1 ((t (:foreground "#DFAF8F" :height 1.5 :overline "#4F4F4F"))))
   `(org-level-2 ((t (:foreground "#BFEBBF" :height 1.3 :overline "#4F4F4F"))))
   `(org-level-3 ((t (:foreground "#7CB8BB" :height 1.2 :overline "#4F4F4F"))))
   `(mode-line
     ((t (:inverse-video unspecified
          :overline nil
          :underline nil
          :foreground "#8FB28F"
          :background "#2B2B2B"
          :box (:line-width 3
                :color "#2B2B2B"
                :style unspecified)))))
   `(mode-line-buffer-id
     ((t (:foreground "#F0DFAF"
          :weight bold))))
   `(mode-line-inactive
     ((t (:inverse-video unspecified
          :overline nil
          :underline nil
          :foreground "#5F7F5F"
          :background "#383838"
          :box (:line-width 3
                :color "#383838"
                :style unspecified)))))))

(use-package solarized-theme
  :disabled t
  :if window-system
  :init
  (progn
    (setq solarized-use-less-bold t
          solarized-use-more-italic t
          solarized-high-contrast-mode-line t
          solarized-use-variable-pitch t)
    (switch-to-theme 'solarized-dark)
    (let* ((variant 'dark)
           (s-base03    "#002b36")
           (s-base02    "#073642")
           ;; emphasized content
           (s-base01    "#586e75")
           ;; primary content
           (s-base00    "#657b83")
           (s-base0     "#839496")
           ;; comments
           (s-base1     "#93a1a1")
           ;; background highlight light
           (s-base2     "#eee8d5")
           ;; background light
           (s-base3     "#fdf6e3")

           ;; Solarized accented colors
           (yellow    "#b58900")
           (orange    "#cb4b16")
           (red       "#dc322f")
           (magenta   "#d33682")
           (violet    "#6c71c4")
           (blue      "#268bd2")
           (cyan      "#2aa198")
           (green     "#859900")

           ;; Darker and lighter accented colors
           ;; Only use these in exceptional circumstances!
           (yellow-d  "#7B6000")
           (yellow-l  "#DEB542")
           (orange-d  "#8B2C02")
           (orange-l  "#F2804F")
           (red-d     "#990A1B")
           (red-l     "#FF6E64")
           (magenta-d "#93115C")
           (magenta-l "#F771AC")
           (violet-d  "#3F4D91")
           (violet-l  "#9EA0E5")
           (blue-d    "#00629D")
           (blue-l    "#69B7F0")
           (cyan-d    "#00736F")
           (cyan-l    "#69CABF")
           (green-d   "#546E00")
           (green-l   "#B4C342")

           ;; Solarized palette names, use these instead of -fg -bg...
           (base0 (if (eq variant 'light) s-base00 s-base0))
           (base00 (if (eq variant 'light) s-base0 s-base00))
           (base1 (if (eq variant 'light) s-base01 s-base1))
           (base01 (if (eq variant 'light) s-base1 s-base01))
           (base2 (if (eq variant 'light) s-base02 s-base2))
           (base02 (if (eq variant 'light) s-base2 s-base02))
           (base3 (if (eq variant 'light) s-base03 s-base3))
           (base03 (if (eq variant 'light) s-base3 s-base03))

           (s-line (if (eq variant 'light) "#cccec4" "#284b54"))

           ;; Light/Dark adaptive higher/lower contrast accented colors
           ;;
           ;; NOTE Only use these in exceptional cirmumstances!
           (yellow-hc (if (eq variant 'light) yellow-d yellow-l))
           (yellow-lc (if (eq variant 'light) yellow-l yellow-d))
           (orange-hc (if (eq variant 'light) orange-d orange-l))
           (orange-lc (if (eq variant 'light) orange-l orange-d))
           (red-hc (if (eq variant 'light) red-d red-l))
           (red-lc (if (eq variant 'light) red-l red-d))
           (magenta-hc (if (eq variant 'light) magenta-d magenta-l))
           (magenta-lc (if (eq variant 'light) magenta-l magenta-d))
           (violet-hc (if (eq variant 'light) violet-d violet-l))
           (violet-lc (if (eq variant 'light) violet-l violet-d))
           (blue-hc (if (eq variant 'light) blue-d blue-l))
           (blue-lc (if (eq variant 'light) blue-l blue-d))
           (cyan-hc (if (eq variant 'light) cyan-d cyan-l))
           (cyan-lc (if (eq variant 'light) cyan-l cyan-d))
           (green-hc (if (eq variant 'light) green-d green-l))
           (green-lc (if (eq variant 'light) green-l green-d))

           ;; customize based face properties
           (s-maybe-bold (if solarized-use-less-bold
                             'unspecified 'bold))
           (s-maybe-italic (if solarized-use-more-italic
                               'italic 'normal))
           (s-variable-pitch (if solarized-use-variable-pitch
                                 'variable-pitch 'default))
           (s-fringe-bg (if solarized-distinct-fringe-background
                            base02 base03))
           (s-fringe-fg base01)


           (s-header-line-fg (if solarized-high-contrast-mode-line
                                 base1 base0))
           (s-header-line-bg (if solarized-high-contrast-mode-line
                                 base02 base03))
           (s-header-line-underline (if solarized-high-contrast-mode-line
                                        nil base02))

           (s-mode-line-fg (if solarized-high-contrast-mode-line
                               base03 base0))
           (s-mode-line-bg (if solarized-high-contrast-mode-line
                               base0 base02))
           (s-mode-line-underline (if solarized-high-contrast-mode-line
                                      nil s-line))

           (s-mode-line-buffer-id-fg (if solarized-high-contrast-mode-line
                                         'unspecified base1))
           (s-mode-line-inactive-fg (if solarized-high-contrast-mode-line
                                        base0 base01))
           (s-mode-line-inactive-bg (if solarized-high-contrast-mode-line
                                        base02 base03))
           (s-mode-line-inactive-bc (if solarized-high-contrast-mode-line
                                        base02 base02)))
      (custom-theme-set-faces
       'solarized-dark
       `(font-lock-comment-face ((t (:foreground ,base01 :slant italic))))
       `(ensime-implicit-highlight ((t (:underline (:style line :color ,base01)))))
       `(mode-line
         ((t (:inverse-video unspecified
              :overline nil
              :underline nil
              :foreground ,s-mode-line-fg
              :background ,s-mode-line-bg
              :box (:line-width 2
                    :color ,s-mode-line-bg
                    :style unspecified)))))
       `(mode-line-buffer-id
         ((t (:foreground ,s-mode-line-buffer-id-fg
              :weight bold))))
       `(mode-line-inactive
         ((t (:inverse-video unspecified
              :overline nil
              :underline nil
              :foreground ,s-mode-line-inactive-fg
              :background ,s-mode-line-inactive-bg
              :box (:line-width 2
                    :color ,s-mode-line-inactive-bg
                    :style unspecified)))))))))

(use-package leuven-theme
  :disabled t
  :if window-system
  :init (switch-to-theme 'leuven)
  :config
  (progn
    (custom-theme-set-faces
     'leuven
     '(default ((t (:background "white" :foreground "#333333"))))
     '(fringe ((t (:background "white" :foreground "light grey"))))
     '(mode-line ((t (:box (:line-width 2 :color "deep sky blue")
                           :background "deep sky blue"
                           :foreground "#ABDFFA"))))
     '(mode-line-inactive ((t (:box (:line-width 2 :color "gainsboro")
                                    :foreground "#F0F0EF"
                                    :background "gainsboro"))))
     '(column-enforce-face ((t (:background "#FFDDDD"))))
     '(hl-tags-face ((t (:background nil :underline (:style line :color "grey80")))))
     '(mmm-default-submode-face ((t :background "gray95")))
     '(vertical-border ((t (:foreground "#f0f0ef")))))
    (custom-theme-set-variables
     'leuven
     '(ansi-color-faces-vector
       [default default default italic underline success warning error])
     '(ansi-color-names-vector
       ["black" "red3" "ForestGreen" "yellow3" "blue" "magenta3" "DeepSkyBlue" "gray50"]))))

(use-package cyberpunk-theme
  :disabled t
  :if window-system
  :init (switch-to-theme 'cyberpunk)
  :config
  (let ((grey-sky-blue (color-desaturate-name (color-darken-name "#4c83ff" 12) 50)))
    (custom-theme-set-faces
     'cyberpunk
     '(default ((t (:background "grey10" :foreground "white smoke"))))
     '(fringe ((t (:background "grey10" :foreground "light grey"))))
     '(vertical-border ((t (:foreground "grey20"))))
     '(hl-line ((t (:background "grey20"))))
     '(diff-refine-added ((t (:background "#005000"))))
     '(diff-refine-removed ((t (:background "#6a0000"))))
     `(mode-line
       ((t (:foreground "white"
                        :background ,grey-sky-blue
                        :box (:line-width 4 :color ,grey-sky-blue)))))
     '(mode-line-inactive
       ((t (:foreground "light grey"
                        :background "grey25"
                        :box (:line-width 4 :color "grey25")))))))
  (custom-theme-set-variables
   'cyberpunk
   '(ansi-color-names-vector
     ["#232323" "LightSalmon" "#A5C261" "#FFC66D" "#3070FF"
      "#FF00FF" "#22EEEE" "#E6E1DC"])))

(use-package exec-path-from-shell
  :init (when (memq window-system '(mac ns))
          (exec-path-from-shell-initialize)))

(use-package hl-line
  :disabled t
  :commands hl-line-mode
  :init (progn
          (set-face-attribute 'hl-line nil :inherit 'highlight
                              :background (color-lighten-name "darkseagreen2" 9))
          (add-hook 'prog-mode-hook 'hl-line-mode)
          (add-hook 'html-mode-hook 'hl-line-mode)))

(use-package undo-tree
  :diminish undo-tree-mode
  :init (global-undo-tree-mode t))

;; (use-package column-enforce-mode
;;   :diminish column-enforce-mode
;;   :commands column-enforce-mode
;;   :init (add-hook 'prog-mode-hook 'column-enforce-mode))

(use-package editorconfig)

(use-package ffap
  :config (setq ffap-machine-p-known 'reject))

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

;; (use-package rainbow-delimiters
;;   :config
;;   (remove-hook 'prog-mode-hook 'rainbow-delimiters-mode))

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
  :bind ("C-c r" . vr/replace)
  :init (require 'visual-regexp-steroids))

(defun yasnippet-or-company-complete ()
  (interactive)
  (if (featurep 'yasnippet)
      (let ((yas-fallback-behavior #'company-complete-selection))
        (yas/expand))
    (company-complete-selection)))

(use-package company
  :diminish company-mode
  :init
  (progn
    (global-company-mode t)
    (bind-keys :map company-active-map
               ("C-w" . nil)
               ([return] . nil)
               ([tab] . yasnippet-or-company-complete)
               ("TAB" . yasnippet-or-company-complete)
               ("RET" . nil)
               ("C-n" . company-select-next)
               ("C-p" . company-select-previous)))
  :config
  (setq company-idle-delay 0.1))

(use-package auto-complete
  :disabled t
  :diminish auto-complete-mode
  :init (remove-hook 'prog-mode-hook 'auto-complete-mode))

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
          '("\n → " "" "\n   " "\n   …" "[" "]" " [No match]" " [Matched]"
            " [Not readable]" " [Too big]" " [Confirm]" "\n → " ""))
    (ido-everywhere t)
    (ido-ubiquitous-mode t)
    (ido-vertical-mode t)
    (flx-ido-mode t)
    (add-to-list 'ido-setup-hook 'dp/ido-keys)
    (setq ido-enable-flex-matching t
          ido-vertical-define-keys 'C-n-and-C-p-only
          ido-use-faces nil
          ido-create-new-buffer 'always
          ido-use-filename-at-point 'guess
          ido-default-file-method 'selected-window
          ido-default-buffer-method 'selected-window)))

(use-package smex
  :bind ("M-x" . smex))

(use-package htmlize)

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

;;; Org mode
(use-package org
  :mode ("\\.org$" . org-mode)
  ;; :bind (("C-c l" . org-store-link)
  ;;        ("C-c a" . org-agenda))
  :config
  (progn
    (add-hook 'org-mode-hook 'typo-mode)
    (org-clock-persistence-insinuate)
    (setq org-startup-indented t
          org-startup-folded t
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
          org-src-preserve-indentation t)
    (add-hook 'org-export-before-processing-hook 'dp/org-set-source-code-background)))

(use-package magit
  ;; :diminish magit-auto-revert-mode
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
          magit-revert-buffers nil)
    (add-hook 'git-commit-mode-hook 'turn-on-flyspell)))

(use-package helm
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
  :diminish rainbow-mode
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
  :config
  (progn
    (bind-keys :map dired-mode-map
               ("C-a" . dired-back-to-start-of-files))
    (dired-details-install)
    (setq-default dired-details-hidden-string "--- ")
    (defadvice dired-create-directory
        (after revert-buffer-after-create activate)
      (revert-buffer))
    (defadvice wdired-abort-changes
        (after revert-buffer-after-abort activate)
      (revert-buffer))))

(use-package purescript-mode
  :mode "\\.purs$"
  :init
  (add-hook 'purescript-mode-hook 'turn-on-purescript-indentation))

(require 'eproject)

(define-project-type maven (generic)
  (look-for "pom.xml")
  :relevant-files ("\\.scala$" "\\.java$" "\\.xml$"))

(use-package scala-mode2
  :config
  (setq scala-indent:align-parameters t)
  :init
  (add-hook 'scala-mode-hook 'ensime-scala-mode-hook))

(use-package cap-words
  :diminish capitalized-words-mode
  :commands capitalized-words-mode)

(use-package js2-refactor
  :commands js2-refactor-mode
  :config
  (progn (js2r-add-keybindings-with-prefix "C-c C-m")))

(use-package json-mode
  :mode "\\.\\(json\\|babelrc\\|jshintrc\\|bowerrc\\|json\\.erb\\)\\'")

(use-package ac-js2 :commands ac-js2-mode)

(use-package js2-mode
  :mode "\\.js$"
  :init
  (progn
    (add-hook 'js2-mode-hook (lambda () (setq mode-name "js2")))
    (add-hook 'js2-mode-hook (lambda () (ac-js2-mode t))))
  :config
  (progn
    (add-hook 'js2-mode-hook 'js2-refactor-mode)
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

(use-package typescript
  :mode ("\\.ts$" . typescript-mode)
  :config (setq typescript-indent-level 2))

(use-package skewer-mode
  :commands (skewer-run-phantomjs run-skewer)
  :config
  (setq phantomjs-program-name "/usr/local/bin/phantomjs"))

(use-package elm-mode
  :mode "\\.elm$")

(use-package eproject
  :diminish eproject-mode
  :commands define-project-type
  :init
  (progn
    (require 'dp-eproject)
    (eproject-set-key "t" 'eproject-tasks-run)
    (eproject-set-key "s" 'eproject-open-term)
    (add-to-list 'generic-project-file-visit-hook 'eproject-set-local-keys)
    (add-to-list 'generic-project-file-visit-hook 'eproject-jshint)
    (add-to-list 'generic-git-project-file-visit-hook 'eproject-set-git-generic-keys)))

(use-package php-mode
  :mode "\\.\\(php[s345]?\\|inc\\|phtml\\)"
  :config
  (progn
    (bind-keys
     :map php-mode-map
     ("C-c j" . dp/php-jump-to-function-definition)
     ("C-}" . dp/php-next-function)
     ("C-{" . dp/php-previous-function))
    (add-to-list 'php-mode-hook (lambda () (setq tab-width 2 c-basic-offset 2)))))

(use-package nxml-mode
  :config (add-to-list 'rng-schema-locating-files "~/.emacs.d/schemas.xml"))

(use-package mmm-mode
  :commands (mmm-mode mmm-mode-on)
  :init
  (require 'mmm-vars)
  (require 'mmm-erb)
  (setq mmm-global-mode 'auto)
  (mmm-add-mode-ext-class 'json-mode "\\.json\\.erb\\'" 'erb)
  (require 'mmm-defaults))

(use-package hl-tags-mode
  :commands hl-tags-mode
  :config
  (set-face-attribute 'hl-tags-face nil :background nil :underline '(:style line :color "red")))

(use-package html-mode
  :mode "\\.html?\\'"
  :config
  (bind-keys
   :map html-mode-map
   ("C-c =" . mc/mark-sgml-tag-pair)
   ("C-}" . sgml-skip-tag-forward)
   ("C-{" . sgml-skip-tag-backward))
  (add-to-list 'sgml-mode-hook 'hl-tags-mode)
  (defadvice sgml-delete-tag (after reident activate)
    (indent-region (point-min) (point-max))))

(use-package yasnippet
  :init
  (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
  (setq yas-prompt-functions '(yas-ido-prompt yas-completing-prompt))
  (yas-global-mode t))

(use-package log-edit
  :config (add-hook 'log-edit-mode-hook 'flyspell-mode))

(use-package olivetti
  :commands olivetti-mode
  :init (setq-default olivetti-body-width 120))

(use-package vc-svn
  :config
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
                          (ad-get-arg 0)))))

(require 'dp-php)
(require 'dp-haskell)

;;; init.el ends here
