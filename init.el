;; -*- lexical-binding: t; -*-

(add-hook 'after-init-hook
          (lambda ()
            (let ((diff (time-subtract (current-time) before-init-time)))
              (message "Initialized in %fs." (float-time diff))))
          100)

(setq truncate-string-ellipsis "…")

(setenv "TERM" "dumb")
(setenv "PAGER" "cat")
(setenv "EDITOR" "emacsclient")

(setq read-process-output-max (* 64 1024 1024))
(setq process-adaptive-read-buffering nil)

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
(line-number-mode t)
(column-number-mode t)
(delete-selection-mode t)
(auto-compression-mode t)

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

(defun turn-on-local-electric-pair-mode ()
  (electric-pair-local-mode t))

(defun turn-off-local-electric-pair-mode ()
  (electric-pair-local-mode -1))

(defun turn-on-local-electric-indent-mode ()
  (electric-indent-local-mode t))

(defun turn-off-local-electric-indent-mode ()
  (electric-indent-local-mode -1))

(defun turn-on-show-trailing-whitespace ()
  (setq show-trailing-whitespace t))

(defun turn-off-bidi-display-reordering ()
  (setq bidi-display-reordering nil))

(add-hook 'prog-mode-hook #'turn-on-show-trailing-whitespace)
(add-hook 'prog-mode-hook #'turn-off-bidi-display-reordering)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(add-hook 'prog-mode-hook #'turn-on-local-electric-pair-mode)
(add-hook 'prog-mode-hook #'turn-on-local-electric-indent-mode)
(add-hook 'prog-mode-hook #'completion-preview-mode)
(add-hook 'sgml-mode-hook #'turn-on-show-trailing-whitespace)

(add-to-list 'auto-mode-alist '("\\.vue\\'" . vue-mode))

(defun font-lock-add-watchwords ()
  "Highlight FIXME, @fixme, TODO, @todo, XXX, @xxx."
  (font-lock-add-keywords
   nil '(("\\(\\B@todo\\|\\<TODO\\)\\(?:$\\|:\s\\|\s\\)" 1 'warning prepend)
         ("\\(\\B@fixme\\|\\<FIXME\\)\\(?:$\\|:\s\\|\s\\)" 1 'error prepend)
         ("\\(\\B@xxx\\|\\<XXX\\)\\(?:$\\|:\s\\|\s\\)"  1 'error prepend)
         ("\\(\\B@note\\|\\<NOTE\\)\\(?:$\\|:\s\\|\s\\)"  1 'error prepend))))

(add-hook 'prog-mode-hook #'font-lock-add-watchwords)

;; Useful in eshell.
(defalias 'e 'find-file)

(require 'package)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(package-initialize)
(package-install-selected-packages)

(require 'use-package)

(defun activate-before-use-package (name &rest args)
  "Activate package before `use-package' is run."
  (package-activate name))

(advice-add 'use-package :before #'activate-before-use-package)

(add-hook 'compilation-mode-hook #'toggle-truncate-lines)

(defun set-hbar-cursor-type ()
  (setq cursor-type 'hbar))

(add-hook 'comint-mode-hook #'set-hbar-cursor-type)
(add-hook 'minibuffer-setup-hook #'set-hbar-cursor-type)

(use-package savehist
  :hook (after-init . savehist-mode))

(require 'setup-dired)
(require 'setup-ibuffer)

(require 'setup-eshell)

(with-eval-after-load 'isearch
  (keymap-set isearch-mode-map "M-s C-s" #'isearch-query-replace)
  (keymap-set isearch-mode-map "M-s C-r" #'isearch-query-replace-regexp))

(use-package sql-indent
  :config (add-hook 'sql-mode-hook #'sqlind-minor-mode))

(use-package hl-line
  :hook (package-menu-mode . hl-line-mode)
  :hook (vc-dir-mode . hl-line-mode)
  :hook (vc-annotate-mode . hl-line-mode)
  :hook (log-view-mode . hl-line-mode)
  :hook (prog-mode . hl-line-mode))

(use-package exec-path-from-shell
  :unless (memq system-type '(ms-dos windows-nt))
  :hook (after-init . exec-path-from-shell-initialize))

(use-package hungry-delete
  :hook (after-init . global-hungry-delete-mode))

(use-package macrostep
  :commands (macrostep macrostep-expand)
  :init (defalias 'macrostep 'macrostep-expand))

(use-package vundo
  :bind (:map global-map ("C-x u" . vundo)))

(use-package ispell
  :defer t
  :config (setq ispell-program-name (executable-find "aspell")))

(use-package deadgrep
  :commands deadgrep)

(use-package edit-indirect
  :commands edit-indirect-region)

(use-package markdown-mode
  :mode "\\.md\\'"
  :mode ("README\\.md\\'" . gfm-mode)
  :hook (markdown-mode . visual-line-mode)
  :hook (markdown-mode . flyspell-mode)
  :bind (:map markdown-mode-map ("C-c C-c p" . markdown-preview-mode)))

(use-package markdown-preview-mode
  :commands markdown-preview-mode
  :config (let ((stylesheet (expand-file-name "github-markdown.css" user-emacs-directory)))
            (setq markdown-preview-stylesheets
                  `(,(format "<style>%s</style>" (file-contents-string stylesheet))))))

(use-package nodejs-repl :commands nodejs-repl)

(use-package buffer-env
  :hook (hack-local-variables . buffer-env-update)
  :hook (comint-mode . buffer-env-update)
  :hook (eshell-mode . buffer-env-update)
  :hook (compilation-mode . buffer-env-update)
  :config
  (defun unset-eshell-path-env-list ()
    (setq eshell-path-env-list nil))
  (advice-add 'buffer-env-update :after #'unset-eshell-path-env-list)
  ;; setup environemnt after vc-setup-buffer call, because it calls
  ;; kill-all-local-variables.
  (defun buffer-env-update-no-file (&rest ignore) (buffer-env-update))
  (advice-add 'vc-setup-buffer :after #'buffer-env-update-no-file))

(require 'setup-project)
(require 'setup-xterm-color)
(require 'setup-compile)
(require 'setup-scratch)

(with-eval-after-load 'log-edit
  (add-hook 'log-edit-mode-hook #'flyspell-mode))

(use-package whitespace-cleanup-mode
  :hook (after-init . global-whitespace-cleanup-mode)
  :config (setf (cdr (assoc 'whitespace-cleanup-mode minor-mode-alist)) (list " wsc")))

(require 'setup-paredit)
(require 'setup-org)

(use-package json-ts-mode
  :mode "\\.\\(json\\|babelrc\\|jshintrc\\|eslintrc\\|bowerrc\\|json\\.erb\\|watchmanconfig\\)\\'")

(use-package prettier
  :disabled t
  :commands (prettier-mode prettier-prettify prettier-prettify-region))

(use-package prettier-js)

(require 'setup-flymake)

(use-package subword
  :hook (js-mode . subword-mode)
  :hook (typescript-ts-mode . subword-mode)
  :hook (tsx-ts-mode . subword-mode)
  :hook (css-base-mode . subword-mode))

(use-package typescript-ts-mode
  :mode ("\\.m?ts\\'" . typescript-ts-mode)
  :mode ("\\.m?tsx\\'" . tsx-ts-mode))

(use-package editorconfig :config (editorconfig-mode t))

(use-package js
  :mode ("\\.m?js\\'" . js-ts-mode))

(use-package string-edit-at-point :commands string-edit-at-point)
(use-package wgrep :commands wgrep-change-to-wgrep-mode)

(require 'setup-eglot)

(with-eval-after-load "minibuffer"
  (keymap-unset completion-in-region-mode-map "TAB")
  (keymap-set completion-in-region-mode-map "M-n" #'minibuffer-next-completion)
  (keymap-set completion-in-region-mode-map "M-p" #'minibuffer-previous-completion))

(use-package dumb-jump
  :commands dumb-jump-xref-activate
  :init (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

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
(use-package embark :bind ("C-c C-;" . embark-export))

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
  :hook (clojure-mode . paredit-mode))

(use-package image-mode
  :init
  (defun image-ns-copy-to-clipboard ()
    (interactive)
    (ns-do-applescript (format "set the clipboard to (read (POSIX file \"%s\") as «class PNGf»)" buffer-file-name)))
  :bind (:map image-mode-map
              ("y" . image-ns-copy-to-clipboard)
              ("c" . image-ns-copy-to-clipboard)))

(use-package cider
  :disabled t
  :commands cider-jack-in
  :hook (cider-repl-mode . paredit-mode)
  :hook (clojure-mode . cider-eldoc-setup))

(use-package imenu-list :disabled t :commands imenu-list)

(use-package enumerated-windows :hook (after-init . enumerated-windows-mode))

(use-package yoshi :commands yoshi-project-mode)

(use-package plantuml-mode
  :disabled t
  :mode "\\.puml\\'")

(defun vc-dir-up ()
  (interactive)
  (let ((buf (current-buffer)))
    (vc-dir (up-directory default-directory))
    (kill-buffer buf)))

(use-package vc-dir
  :bind (:map vc-dir-mode-map ("^" . vc-dir-up)))

(defun hack-apply-local-variables-to-current-buffer (&rest ignore)
  (hack-local-variables))

(use-package vc-git
  :bind (:map vc-dir-git-mode-map
              ("z c" . vc-git-stash)
              ("z x" . vc-git-stash-delete)
              ("z s" . vc-git-stash-snapshot))
  :config
  (advice-add 'vc-setup-buffer :after #'hack-apply-local-variables-to-current-buffer))

(use-package diff-hl
  :bind (:map diff-hl-mode-map ("C-x v k" . diff-hl-revert-hunk))
  :hook (after-init . global-diff-hl-mode))

(require 'setup-ligatures)

;; Work around the issue of Emacs EPG and GPG >2.0 talking past each other.
;; https://dev.gnupg.org/T6481#170760
(fset 'epg-wait-for-status 'ignore)

(use-package conf-mode
  :mode "\\.env\\(\\.local\\)?\\'")

(use-package treesit-auto
  :hook (after-init . global-treesit-auto-mode))

(use-package treesit-fold
  :disabled t
  :hook (prog-mode . treesit-fold-mode)
  :bind (:map treesit-fold-mode-map
              ("C-c f c" . treesit-fold-close)
              ("C-c f C" . treesit-fold-close-all)
              ("C-c f o" . treesit-fold-open)
              ("C-c f O" . treesit-fold-open-all)))

(use-package devdocs
  :bind ("C-h D" . devdocs-lookup))

(use-package elfeed :commands elfeed)

(use-package gptel
  :commands (gptel gptel-send gptel-rewrite)
  :bind (:map gptel-mode-map
              ("C-c C-k" . gptel-abort)
              ("C-c C-p" . gptel-system-prompt))
  :config
  (let* ((ollama-list
          (cdr (string-split (shell-command-to-string "ollama list") "\n" t)))
         (ollama-models
          (mapcar (lambda (line)
                    (intern (car (string-split line))))
                  ollama-list))
         (ollama-backend
          (gptel-make-ollama "ollama"
            :host "localhost:11434"
            :stream t
            :models ollama-models)))
    (setq gptel-model (car ollama-models)
          gptel-backend ollama-backend)))

(use-package devdocs
  :bind (("C-h D" . devdocs-lookup)))

(require 'setup-tempo)

(use-package winpulse
  :vc (:url "https://github.com/dop/winpulse" :rev "less-flashing")
  :config (winpulse-mode +1))

(load "~/work/config.el" 'noerror)
(load "~/work/utils.el" 'noerror)

(server-start)

;; end of init.el
