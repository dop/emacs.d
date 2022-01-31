;; -*- lexical-binding: t; -*-

(setenv "TERM" "dumb")
(setenv "PAGER" "cat")
(setenv "EDITOR" "emacsclient")

(load-file (setq custom-file "~/.emacs.d/custom.el"))
(add-to-list 'load-path "~/.emacs.d/lisp")

(require 'setup-key-bindings)

;; (scroll-bar-mode -1)
;; (tool-bar-mode -1)
(unless (eq window-system 'ns)
  (menu-bar-mode -1))

(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'erase-buffer 'disabled nil)

;; UTF-8 please
(set-language-environment "UTF-8")
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(line-number-mode t)
(column-number-mode t)
(electric-pair-mode t)
(delete-selection-mode t)
(auto-compression-mode t)

(defun turn-on-show-trailing-whitespace ()
  (setq show-trailing-whitespace t))

(add-hook 'prog-mode-hook #'turn-on-show-trailing-whitespace)
(add-hook 'html-mode-hook #'turn-on-show-trailing-whitespace)
(add-hook 'css-mode-hook #'turn-on-show-trailing-whitespace)

(defalias 'e 'find-file)

(require 'package)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

;; Initialize package system, but not installed packages. I usually
;; have installed more than I actually use.
(package-initialize 'no-activate)

;; Activate and require `use-package' explicitly. We'll use it to
;; activate all the rest on demand.
(package-activate 'use-package)
(require 'use-package)

(defun activate-before-use-package (name &rest args)
  "Activate package before `use-package' is run."
  (package-activate name))

(advice-add 'use-package :before #'activate-before-use-package)

(defun make-text-smaller () (text-scale-adjust -2))

(when window-system
  (add-hook 'help-mode-hook             #'make-text-smaller)
  (add-hook 'apropos-mode-hook          #'make-text-smaller)
  (add-hook 'backtrace-mode-hook        #'make-text-smaller)
  (add-hook 'imenu-list-major-mode-hook #'make-text-smaller)
  (add-hook 'messages-buffer-mode-hook  #'make-text-smaller)
  (add-hook 'eshell-mode-hook           #'make-text-smaller)
  (add-hook 'compilation-mode-hook      #'make-text-smaller))

(with-eval-after-load 'sly (add-hook 'sly-mrepl-mode-hook #'make-text-smaller))
(add-hook 'eshell-mode-hook #'toggle-truncate-lines)

(require 'setup-ibuffer)

(use-package exec-path-from-shell
  :init
  (when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize)))

(use-package hungry-delete :init (global-hungry-delete-mode t))

(use-package undo-tree
  :bind (:map undo-tree-map ("C-x u" . undo-tree-visualize))
  :init (global-undo-tree-mode t))

(use-package ispell
  :config (setq ispell-program-name (executable-find "aspell")))

(use-package markdown-mode
  :mode "\\.md\\'"
  :config
  (add-hook 'markdown-mode-hook #'visual-line-mode)
  (add-hook 'markdown-mode-hook #'flyspell-mode))

(use-package nodejs-repl)

(require 'setup-xterm-color)

(require 'setup-compile)

(use-package whitespace-cleanup-mode
  :init (global-whitespace-cleanup-mode t)
  :config (setf (cdr (assoc 'whitespace-cleanup-mode minor-mode-alist)) (list " ░")))

(require 'setup-paredit)

(use-package org
  :mode ("\\.org\\'" . org-mode)
  :config
  (require 'org-tempo)
  (dolist (hook (list #'turn-on-show-trailing-whitespace
                      #'visual-line-mode
                      #'org-indent-mode
                      #'flyspell-mode))
    (add-hook 'org-mode-hook hook))

  (org-babel-do-load-languages 'org-babel-load-languages
                               '((emacs-lisp . t) (shell . t) (lisp . t))))

(use-package git-timemachine)

(use-package json-mode
  :pin gnu
  :mode "\\.\\(json\\|babelrc\\|jshintrc\\|eslintrc\\|bowerrc\\|json\\.erb\\|watchmanconfig\\)\\'")

(use-package prettier)

(use-package flymake
  :pin gnu
  :hook ((typescript-mode . flymake-mode))
  :bind (:map flymake-mode-map ("C-x `" . flymake-goto-next-error)))

(use-package flymake-eslint)

(use-package subword
  :hook ((js-mode . subword-mode)
         (typescript-mode . subword-mode)))

(use-package editorconfig :config (editorconfig-mode t))

(use-package typescript-mode :mode "\\.ts\\'")
(require 'tsx-mode)

(use-package string-edit)
(use-package wgrep)
(use-package eglot :pin gnu)
(use-package olivetti :defer t)
(use-package csv-mode :mode "\\.csv\\'")
(use-package restclient :mode "\\.rest\\'")
(use-package protobuf-mode :mode "\\.proto\\'")

(use-package ns-auto-titlebar
  :if (eq 'ns (window-system))
  :init (ns-auto-titlebar-mode t))

(use-package sly :hook ((lisp-mode . sly-editing-mode)))

(use-package dark-mode :commands dark-mode)
(use-package neotree)

(use-package yoshi :commands yoshi-project-mode)
