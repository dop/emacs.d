;; -*- lexical-binding: t; -*-

(setenv "TERM" "dumb")
(setenv "PAGER" "cat")

(load-file (setq custom-file "~/.emacs.d/custom.el"))
(add-to-list 'load-path "~/.emacs.d/lisp")

(require 'misc)
(require 'key-bindings)

(scroll-bar-mode -1)
(tool-bar-mode -1)
(when (not (eq system-type 'darwin))
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

(use-package exec-path-from-shell
  :init
  (when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize)))

(use-package eshell
  :defer t
  :config
  (add-hook 'eshell-mode-hook #'toggle-truncate-lines))

(use-package hungry-delete
  :init (global-hungry-delete-mode t))

(use-package undo-tree
  :bind (:map undo-tree-map
              ("C-x u" . undo-tree-visualize))
  :init (global-undo-tree-mode t))

(use-package markdown-mode
  :mode "\\.md\\'"
  :config
  (add-hook 'markdown-mode-hook #'visual-line-mode)
  (add-hook 'markdown-mode-hook #'flyspell-mode))

(use-package nodejs-repl)

(defun xterm-color-compilation-filter ()
  (let* ((inhibit-read-only t)
         (start compilation-filter-start)
         (end   (point))
         (input (delete-and-extract-region start end)))
    (goto-char (point-max))
    (insert (xterm-color-filter input))))

;; Nice colors for tools that produce color escape codes.
(use-package xterm-color
  :commands xterm-color-filter
  :hook ((compilation-filter . xterm-color-compilation-filter))
  :config
  (add-hook 'comint-preoutput-filter-functions #'xterm-color-filter)
  (setq comint-output-filter-functions (remove 'ansi-color-process-output comint-output-filter-functions)))

(use-package compile
  :defer t
  :config
  (add-hook 'compilation-mode-hook #'toggle-truncate-lines))

(use-package whitespace-cleanup-mode
  :init (global-whitespace-cleanup-mode t))

(defvar whitespace-characters
  '(?\n ?\r ?\t ?\ ))

(defun whitespace-char-p (c)
  (member c whitespace-characters))

(defun paredit-kill-dwim (fn &rest args)
  (if (use-region-p)
      (call-interactively #'kill-region)
    (apply fn args)))

(defun paredit-forward-delete-whitespace (fn &rest args)
  (if (whitespace-char-p (char-after))
      (while (whitespace-char-p (char-after))
        (delete-char 1))
    (apply fn args)))

(defun paredit-backward-delete-whitespace (fn &rest args)
  (if (whitespace-char-p (char-before))
      (while (whitespace-char-p (char-before))
        (delete-char -1))
    (apply fn args)))

(use-package paredit
  :commands paredit-mode
  :hook
  ((ielm-mode . paredit-mode)
   (emacs-lisp-mode . paredit-mode)
   (lisp-mode . paredit-mode)
   (lisp-data-mode . paredit-mode)
   (eval-expression-minibuffer-setup . paredit-mode))
  :config
  (define-key paredit-mode-map (kbd "C-w") #'paredit-backward-kill-word)
  (define-key paredit-mode-map (kbd "C-c [") #'paredit-forward-slurp-sexp)
  (define-key paredit-mode-map (kbd "C-c ]") #'paredit-forward-barf-sexp)

  (advice-add 'paredit-kill :around #'paredit-kill-dwim)
  (advice-add 'paredit-forward-delete :around #'paredit-forward-delete-whitespace)
  (advice-add 'paredit-backward-delete :around #'paredit-backward-delete-whitespace))

(use-package org-bullets
  :hook ((org-mode . org-bullets-mode)))

(use-package org
  :mode ("\\.org\\'" . org-mode)
  :config
  (add-hook 'org-mode-hook #'turn-on-show-trailing-whitespace)
  (add-hook 'org-mode-hook #'visual-line-mode)
  (org-babel-do-load-languages 'org-babel-load-languages
                               '((emacs-lisp . t) (shell . t) (lisp . t))))

(use-package git-timemachine)

(use-package json-mode
  :mode "\\.\\(json\\|babelrc\\|jshintrc\\|eslintrc\\|bowerrc\\|json\\.erb\\|watchmanconfig\\)\\'")

(use-package prettier)

(use-package flymake-eslint
  :bind (:map flymake-mode-map ("C-x `" . flymake-goto-next-error))
  :hook
  ((js-mode . flymake-eslint-enable)
   (typescript-mode . flymake-eslint-enable))
  :custom
  ((flymake-eslint-defer-binary-check t)))

(use-package js
  :config (add-hook 'js-mode-hook #'subword-mode))

(use-package typescript-mode
  :mode "\\.ts\\'"
  :config
  ;; (add-hook 'typescript-mode-hook #'lsp)
  (add-hook 'typescript-mode-hook #'subword-mode)
  :init
  (define-derived-mode tsx-mode typescript-mode "tsx")
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-mode)))

(defvar compilation-typescript-error-regexp-alist
  '(typescript-nglint-warning typescript-nglint-error typescript-tslint typescript-tsc-pretty typescript-tsc))

(use-package string-edit)

(defun project-dir-locals-project (dir)
  (let ((root (locate-dominating-file dir ".dir-locals.el")))
    (and root (cons 'dir-locals root))))

(cl-defmethod project-roots ((project (head dir-locals)))
  (list (cdr project)))

(cl-defmethod project-ignores ((project (head dir-locals)) dir)
  (cons ".log/"
        (mapcar (lambda (dir) (concat dir "/"))
                vc-directory-exclusion-list)))

(advice-add 'risky-local-variable-p :override #'ignore)

(use-package project
  :config
  (add-hook 'project-find-functions #'project-dir-locals-project))

(use-package olivetti)
(use-package csv-mode :mode "\\.csv\\'")
(use-package restclient :mode "\\.rest\\'")
(use-package protobuf-mode :mode "\\.proto\\'")

(use-package ns-auto-titlebar
  :init
  (ns-auto-titlebar-mode t)
  (when (eq 'ns (window-system))
    (setq ns-use-thin-smoothing t
          ns-use-srgb-colorspace t)))

(defun echo-ps-send-result (result)
  (message "%s" (with-temp-buffer
                  (emacs-lisp-mode)
                  (loop for value in (cdr result) do (insert (format "%s" value)))
                  (font-lock-ensure)
                  (buffer-string))))

(defun ps-expand-and-send ()
  (interactive)
  (slime-eval-async (list 'swank:eval-and-grab-output
                          (format "(js-eval:js (js-eval::*js* :wait t) %s)" (slime-sexp-at-point)))
    #'echo-ps-send-result
    (slime-current-package)))

(defun override-slime-repl-bindings-with-paredit ()
  (define-key slime-repl-mode-map
    (read-kbd-macro paredit-backward-delete-key) nil))

(use-package slime
  :ensure t
  :pin melpa-stable
  :defer t
  :config
  ;; Unblocks eldoc while using slime-watch.
  (setq slime-inhibit-pipelining nil)
  (add-hook 'lisp-mode-hook #'slime-mode)
  (add-hook 'slime-repl-mode-hook #'paredit-mode)
  (add-hook 'slime-repl-mode-hook #'override-slime-repl-bindings-with-paredit)
  (slime-setup '(slime-fancy slime-indentation slime-company slime-repl-ansi-color))
  (load "~/quicklisp/dists/quicklisp/software/Parenscript-2.7.1/extras/js-expander.el")
  (load "~/quicklisp/log4slime-setup.el")
  (global-log4slime-mode t)
  (define-key slime-mode-map (kbd "C-c e") #'ps-expand-and-send))

(use-package neotree)
