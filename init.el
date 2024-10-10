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

(savehist-mode t)
(recentf-mode t)

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
         ("\\(\\B@xxx\\|\\<XXX\\)\\(?:$\\|:\s\\|\s\\)"  1 'error prepend)
         ("\\(\\B@note\\|\\<NOTE\\)\\(?:$\\|:\s\\|\s\\)"  1 'error prepend))))

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
;; (package-activate 'use-package)
(require 'use-package)

(defun activate-before-use-package (name &rest args)
  "Activate package before `use-package' is run."
  (package-activate name))

(advice-add 'use-package :before #'activate-before-use-package)

(add-hook 'eshell-mode-hook #'toggle-truncate-lines)
(add-hook 'compilation-mode-hook #'toggle-truncate-lines)

(use-package savehist
  :hook (after-init . savehist-mode))

(require 'setup-dired)
(require 'setup-ibuffer)

(use-package sql-indent
  :config (add-hook 'sql-mode-hook #'sqlind-minor-mode))

(use-package hl-line
  :hook (package-menu-mode . hl-line-mode)
  :hook (vc-dir-mode . hl-line-mode)
  :hook (vc-annotate-mode . hl-line-mode))

(use-package exec-path-from-shell
  :unless (memq system-type '(ms-dos windows-nt))
  :hook (after-init . exec-path-from-shell-initialize))

(use-package hungry-delete :init (global-hungry-delete-mode t))

(use-package macrostep
  :commands (macrostep macrostep-expand)
  :init (defalias 'macrostep 'macrostep-expand))

(use-package undo-tree
  :bind (:map undo-tree-map ("C-x u" . undo-tree-visualize))
  :hook (after-init . global-undo-tree-mode))

(use-package ispell
  :defer t
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

(use-package markdown-preview-mode :commands markdown-preview-mode)

(use-package nodejs-repl :commands nodejs-repl)

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

(use-package json-mode
  :pin gnu
  :mode "\\.\\(json\\|babelrc\\|jshintrc\\|eslintrc\\|bowerrc\\|json\\.erb\\|watchmanconfig\\)\\'")

(use-package prettier
  :commands (prettier-mode prettier-prettify prettier-prettify-region))

(require 'setup-flymake)

(use-package subword
  :hook (js-mode . subword-mode)
  :hook (typescript-ts-mode . subword-mode)
  :hook (tsx-ts-mode . subword-mode))

(use-package typescript-ts-mode
  :mode ("\\.ts\\'" . typescript-ts-mode)
  :mode ("\\.tsx\\'" . tsx-ts-mode))

(use-package editorconfig :config (editorconfig-mode t))

(use-package js
  :mode ("\\.m?js\\'" . js-st-mode)
  :mode ("tsconfig.json\\'" . js-json-mode))

(use-package string-edit-at-point :commands string-edit-at-point)
(use-package wgrep :commands wgrep-change-to-wgrep-mode)
(use-package eglot
  :commands eglot
  :config
  (defun eglot-rename (newname)
    "Rename the current symbol to NEWNAME."
    (interactive
     (list (read-from-minibuffer
            (format "Rename `%s' to: " (or (thing-at-point 'symbol t)
                                           "unknown symbol"))
            (thing-at-point 'symbol t)
            nil nil nil
            (symbol-name (symbol-at-point)))))
    (eglot--server-capable-or-lose :renameProvider)
    (eglot--apply-workspace-edit
     (jsonrpc-request (eglot--current-server-or-lose)
                      :textDocument/rename `(,@(eglot--TextDocumentPositionParams)
                                             :newName ,newname))
     current-prefix-arg)))

(use-package olivetti :commands olivetti-mode)
(use-package csv-mode :mode "\\.csv\\'")
(use-package restclient :mode "\\.rest\\'")
(use-package protobuf-mode :mode "\\.proto\\'")

(use-package ns-auto-titlebar
  :when (eq 'ns window-system)
  :hook (after-init . ns-auto-titlebar-mode))

(when (functionp 'mac-auto-operator-composition-mode)
  (mac-auto-operator-composition-mode t))

(use-package marginalia :hook (after-init . marginalia-mode))
;; (use-package orderless :pin gnu :ensure t)
;; (use-package consult :pin gnu :ensure t)
(use-package vertico :hook (after-init . vertico-mode))

(use-package paren-face
  :hook (lisp-data-mode . paren-face-mode)
  :hook (clojure-mode . paren-face-mode))

(use-package sly
  :hook (lisp-mode . sly-editing-mode)
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

(use-package clojure-mode
  :hook (clojure-mode . paredit-mode)
  ;; (add-hook 'clojure-mode #'inf-clojure-eldoc-setup)
  )

(use-package inf-clojure
  :load-path "~/.emacs.d/lisp/inf-clojure"
  :config (setq inf-clojure-enable-eldoc nil))

(use-package cider
  :commands cider-jack-in
  :hook (cider-repl-mode . paredit-mode)
  :hook (clojure-mode . cider-eldoc-setup))

(use-package imenu-list :disabled t :commands imenu-list)

(use-package enumerated-windows :hook (after-init . enumerated-windows-mode))

(use-package yoshi :commands yoshi-project-mode)

(use-package keyfreq
  :hook (after-init . keyfreq-mode)
  :hook (after-init . keyfreq-autosave-mode))

(use-package plantuml-mode
  :disabled t
  :mode "\\.puml\\'")

(use-package vc-git
  :bind (:map vc-dir-git-mode-map
              ("z c" . vc-git-stash-snapshot)
              ("z x" . vc-git-stash-delete)
              ("z s" . vc-git-stash-show)))

(use-package diff-hl
  :hook (after-init . global-diff-hl-mode))

(use-package ligature
  :disabled t
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

;; Work around the issue of Emacs EPG and GPG >2.0 talking past each other.
;; https://dev.gnupg.org/T6481#170760
(fset 'epg-wait-for-status 'ignore)

(use-package conf-mode
  :mode "\\.env\\(\\.local\\)?\\'")

(use-package treesit-auto
  :hook (after-init . global-treesit-auto-mode))

(keymap-global-set "C-c s" #'scratch-file)

(use-package elfeed :commands elfeed)

(server-start)

;; end of init.el
