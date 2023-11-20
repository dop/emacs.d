;; -*- lexical-binding: t; -*-

(setenv "TERM" "dumb")
(setenv "PAGER" "cat")
(setenv "EDITOR" "emacsclient")

(load-file (setq custom-file "~/.emacs.d/custom.el"))
(add-to-list 'load-path "~/.emacs.d/lisp")

(with-eval-after-load 'info
  (add-to-list 'Info-additional-directory-list "~/.local/share/info"))

(defalias 'yes-or-no-p 'y-or-n-p)

(require 'my-commands)
(require 'git-utils)
(require 'setup-key-bindings)

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

(defun turn-off-local-electric-pair-mode ()
  (electric-pair-local-mode -1))

(defun turn-off-local-electric-indent-mode ()
  (electric-indent-local-mode -1))

(defun turn-on-show-trailing-whitespace ()
  (setq show-trailing-whitespace t))

(defun turn-off-bidi-display-reordering ()
  (setq bidi-display-reordering nil))

(add-hook 'prog-mode-hook #'turn-on-show-trailing-whitespace)
(add-hook 'prog-mode-hook #'turn-off-bidi-display-reordering)
(add-hook 'html-mode-hook #'turn-on-show-trailing-whitespace)
(add-hook 'css-mode-hook #'turn-on-show-trailing-whitespace)

(add-to-list 'auto-mode-alist '("\\.vue\\'" . mhtml-mode))

(defun add-watchwords ()
  "Highlight FIXME, @fixme, TODO, @todo, XXX, @xxx."
  (font-lock-add-keywords
   nil '(("\\(\\B@todo\\|\\<TODO\\)\\(?:$\\|:\s\\|\s\\)" 1 'warning prepend)
         ("\\(\\B@fixme\\|\\<FIXME\\)\\(?:$\\|:\s\\|\s\\)" 1 'error prepend)
         ("\\(\\B@xxx\\|\\<XXX\\)\\(?:$\\|:\s\\|\s\\)"  1 'error prepend))))

(add-hook 'prog-mode-hook #'add-watchwords)

;; Useful in eshell.
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

(defvar *install-package-if-used* nil
  "Install package when `use-package' is called.")

(defun activate-before-use-package (name &rest args)
  "Activate package before `use-package' is run."
  (package-activate name))

(defun install-before-use-package (name &rest args)
  (when *install-package-if-used*
    (ignore-errors
      (unless (package-installed-p name)
        (package-install name)))))

(advice-add 'use-package :before #'activate-before-use-package)
(advice-add 'use-package :before #'install-before-use-package)

(add-hook 'eshell-mode-hook #'toggle-truncate-lines)
(add-hook 'compilation-mode-hook #'toggle-truncate-lines)

(require 'setup-dired)
(require 'setup-ibuffer)

(use-package sql-indent
  :hook (sql-mode . sqlind-minor-mode))

(use-package hl-line
  :hook ((package-menu-mode . hl-line-mode)
         (vc-dir-mode . hl-line-mode)
         (vc-annotate-mode . hl-line-mode)))

(use-package exec-path-from-shell
  :init
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package hungry-delete :init (global-hungry-delete-mode t))

(use-package undo-tree
  :bind (:map undo-tree-map ("C-x u" . undo-tree-visualize))
  :init (global-undo-tree-mode t))

(use-package ispell
  :config (setq ispell-program-name (executable-find "aspell")))

(use-package deadgrep
  :commands deadgrep)

(use-package edit-indirect
  :commands edit-indirect-region)

(use-package markdown-mode
  :mode "\\.md\\'"
  :config
  (add-hook 'markdown-mode-hook #'visual-line-mode)
  (add-hook 'markdown-mode-hook #'flyspell-mode))

(use-package markdown-preview-mode
  :commands markdown-preview-mode)

(use-package nodejs-repl)

(require 'setup-project)
(require 'setup-xterm-color)
(require 'setup-compile)

(with-eval-after-load 'log-edit
  (add-hook 'log-edit-mode-hook #'flyspell-mode))

(use-package whitespace-cleanup-mode
  :init (global-whitespace-cleanup-mode t)
  :config (setf (cdr (assoc 'whitespace-cleanup-mode minor-mode-alist)) (list " wsc")))

(require 'setup-paredit)
(require 'setup-org)

(use-package git-timemachine)

(use-package json-mode
  :pin gnu
  :mode "\\.\\(json\\|babelrc\\|jshintrc\\|eslintrc\\|bowerrc\\|json\\.erb\\|watchmanconfig\\)\\'")

(use-package prettier)

(require 'setup-flymake)

(use-package subword
  :hook ((js-mode . subword-mode)
         (typescript-mode . subword-mode)))

(use-package editorconfig :config (editorconfig-mode t))

(use-package js :config (add-to-list 'auto-mode-alist '("\\.mjs\\'" . js-mode)))
(use-package typescript-mode :mode "\\.tsx\\'")
(require 'tsx-mode)

(use-package string-edit-at-point)
(use-package wgrep)
(use-package external-completion :pin gnu)

(if (fboundp 'project-name)
    (warn "`project-name' is available, no need to re-define.")
  (cl-defgeneric project-name (project)
    "A human-readable name for the project.
Nominally unique, but not enforced."
    (file-name-nondirectory (directory-file-name (project-root project)))))

(use-package eglot
  :commands (eglot eglot-ensure)
  :load-path "~/.emacs.d/lisp/eglot"
  :config
  (when-let ((config (find 'typescript-mode eglot-server-programs :key (-compose #'-list #'car) :test #'find)))
    (pushnew 'tsx-mode (car config))))

(use-package olivetti :defer t)
(use-package csv-mode :mode "\\.csv\\'")
(use-package restclient :mode "\\.rest\\'")
(use-package protobuf-mode :mode "\\.proto\\'")

(use-package ns-auto-titlebar
    :if (eq 'ns (window-system))
    :init (ns-auto-titlebar-mode t))

(when (functionp 'mac-auto-operator-composition-mode)
  (mac-auto-operator-composition-mode t))

(use-package marginalia :defer t :init (marginalia-mode t))
;; (use-package orderless :pin gnu :ensure t)
;; (use-package consult :pin gnu :ensure t)
(use-package vertico :defer t :init (vertico-mode t))

(use-package paren-face :hook ((lisp-data-mode . paren-face-mode)))

(use-package sly
  :hook ((lisp-mode . sly-editing-mode))
  :config
  (defun rps-sly-eval-last-expression ()
    (interactive)
    (sly-interactive-eval (format "(rps-user:run %s)" (sly-last-expression))))

  (defun ps-last-expression ()
    (interactive)
    (let ((expr (format "(format t (ps:ps %s))" (sly-last-expression))))
      (sly-eval-async `(slynk:eval-and-grab-output ,expr)
        (lambda (result)
          (cl-destructuring-bind (output value) result
            (with-current-buffer (pop-to-buffer (get-buffer-create "*Parenscript->JavaScript*"))
              (erase-buffer)
              (insert output)
              (unless (eq major-mode 'javascript-mode)
                (javascript-mode))))))))

  (bind-key "C-c C-j" #'rps-sly-eval-last-expression sly-editing-mode-map)
  (bind-key "C-c j" #'ps-last-expression sly-editing-mode-map))

(use-package clojure-mode :hook ((clojure-mode . paredit-mode)))
(use-package cider :commands cider-jack-in :hook ((cider-repl-mode . paredit-mode)))

(use-package imenu-list :commands imenu-list)

(use-package enumerated-windows :config (enumerated-windows-mode t))

(use-package yoshi :commands yoshi-project-mode)

(use-package keyfreq :init (keyfreq-mode 1) (keyfreq-autosave-mode 1))

(use-package plantuml-mode :mode "\\.puml\\'")

(use-package vc-git
  :bind (:map vc-dir-git-mode-map
              ("z x" . vc-git-stash-delete)
              ("z s" . vc-git-stash-show)))

(use-package git-gutter
  :hook (prog-mode . git-gutter-mode))

(use-package git-gutter-fringe
  :config
  (define-fringe-bitmap 'git-gutter-fr:added [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [224] nil nil '(top repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [224] nil nil '(top repeated)))

(use-package ligature
  :hook (prog-mode . ligature-mode)
  :config
  (ligature-set-ligatures
   'prog-mode
   '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
     ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
     "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
     "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
     "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
     "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
     "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
     "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
     ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
     "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
     "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
     "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
     "\\\\" "://")))

(use-package inf-clojure
  :load-path "~/.emacs.d/lisp/inf-clojure"
  :config (setq inf-clojure-enable-eldoc nil))

(use-package clojure-mode
  :hook ((clojure-mode . inf-clojure-eldoc-setup)))

;; Work around the issue of Emacs EPG and GPG >2.0 talking past each other.
;; https://dev.gnupg.org/T6481#170760
(fset 'epg-wait-for-status 'ignore)

(use-package conf-mode
  :mode "\\.env\\(\\.local\\)?\\'")

(defvar project-name-mode-line
  '(:propertize (:eval (when-let ((project (project-current)))
                         (format "%s/" (project-name project))))))

(put 'project-name-mode-line 'risky-local-variable t)

(unless (cl-position 'project-name-mode-line mode-line-format)
  (let ((i (cl-position 'mode-line-buffer-identification mode-line-format)))
    (push 'project-name-mode-line (nthcdr i mode-line-format))))

(keymap-global-set "C-c s" #'scratch-file)

(server-start)

;; end of init.el
