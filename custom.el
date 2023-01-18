(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(backup-by-copying t)
 '(backup-directory-alist '(("." . "~/.emacs.d/backups")))
 '(comint-eol-on-send t)
 '(comint-input-ignoredups t)
 '(comint-move-point-for-output nil)
 '(comint-process-echoes t)
 '(comint-scroll-show-maximum-output nil)
 '(comint-scroll-to-bottom-on-output nil)
 '(company-tooltip-align-annotations t)
 '(compilation-always-kill t)
 '(compilation-ask-about-save nil)
 '(compilation-message-face 'default)
 '(create-lockfiles nil)
 '(css-indent-offset 2)
 '(csv-separators '("," "\11" ";"))
 '(custom-enabled-themes nil)
 '(custom-safe-themes nil)
 '(debug-on-error nil)
 '(delete-old-versions t)
 '(dired-dwim-target t)
 '(dired-free-space nil)
 '(dired-listing-switches "-ahl")
 '(dired-subtree-use-backgrounds nil)
 '(display-battery-mode t)
 '(display-buffer-alist
   '(("\\*\\(.*eshell\\|sly-mrepl for.*\\)\\*"
      (display-buffer-reuse-window display-buffer-in-previous-window display-buffer-at-bottom)
      (window-height . 0.375))
     ("\\*Flymake diagnostics"
      (display-buffer-reuse-window display-buffer-in-previous-window display-buffer-at-bottom)
      (window-height . 0.375))
     ("\\*\\(Async Shell Command\\|Shell Command Output\\)\\*"
      (display-buffer-reuse-window display-buffer-in-previous-window display-buffer-at-bottom)
      (window-height . 0.375))
     ("\\*\\([Hh]elp\\|Backtrace\\|Warnings\\|Messages\\|info\\|Apropos\\|vc-diff\\)\\*"
      (display-buffer-reuse-window display-buffer-in-previous-window display-buffer-pop-up-window display-buffer-same-window)
      (window-height . 0.375)
      (window-width . 0.375))
     ("\\*Completions\\*"
      (display-buffer-reuse-window display-buffer-in-previous-window display-buffer-at-bottom)
      (window-height . shrink-window-if-larger-than-buffer))
     ("\\*\\(NeoTree\\|Ilist\\)\\*"
      (display-buffer-reuse-window display-buffer-in-previous-window display-buffer-in-side-window)
      (window-width . 0.2)
      (side . left))))
 '(display-time-24hr-format t)
 '(display-time-day-and-date t)
 '(display-time-default-load-average nil)
 '(display-time-mode t)
 '(editorconfig-mode-lighter " ec")
 '(ef-themes-mixed-fonts t)
 '(eglot-autoshutdown t)
 '(eglot-confirm-server-initiated-edits nil)
 '(eglot-events-buffer-size nil)
 '(eglot-override-flymake-diagnostic-functions nil)
 '(eldoc-minor-mode-string " doc")
 '(electric-pair-mode t)
 '(epa-pinentry-mode 'loopback)
 '(epg-pinentry-mode 'loopback)
 '(eshell-hist-ignoredups t)
 '(eshell-prompt-function 'my-eshell-prompt)
 '(fast-but-imprecise-scrolling t)
 '(flymake-eslint-defer-binary-check t)
 '(frame-inhibit-implied-resize t)
 '(gc-cons-threshold 268435456)
 '(ibuffer-formats
   '((mark modified read-only locked " "
           (name 32 32 :left :elide)
           " "
           (size 9 -1 :right)
           " "
           (mode 16 16 :left :elide)
           " " filename-and-process)
     (mark " "
           (name 16 -1)
           " " filename)))
 '(ibuffer-saved-filter-groups nil)
 '(indent-tabs-mode nil)
 '(indicate-buffer-boundaries 'left)
 '(indicate-empty-lines nil)
 '(inferior-lisp-program "sbcl --dynamic-space-size 2048")
 '(inhibit-compacting-font-caches t t)
 '(inhibit-startup-screen t)
 '(initial-scratch-message nil)
 '(ispell-local-dictionary-alist
   '((nil "[a-zA-Z]" "[^a-zA-Z]" "[']" nil
          ("-l" "en")
          nil utf-8)
     ("english" "[a-zA-Z]" "[^a-zA-Z]" "[']" nil
      ("-l" "en")
      nil utf-8)
     ("lithuanian" "[a-zA-ZąčęėįšųūžĄČĘĖĮŠŲŪŽ]" "[^a-zA-ZąčęėįšųūžĄČĘĖĮŠŲŪŽ]" "" nil
      ("-l" "lt")
      nil utf-8)))
 '(jit-lock-defer-time 0)
 '(js-indent-level 2)
 '(kept-new-versions 6)
 '(kept-old-versions 2)
 '(locale-coding-system 'utf-8 t)
 '(ls-lisp-dirs-first t)
 '(ls-lisp-emulation nil)
 '(ls-lisp-ignore-case t)
 '(ls-lisp-use-insert-directory-program nil)
 '(mode-line-compact t)
 '(mode-require-final-newline t)
 '(monkeytype-mode-hook '((lambda nil (electric-pair-local-mode -1))))
 '(neo-autorefresh nil)
 '(neo-smart-open t)
 '(neo-theme 'nerd)
 '(neo-vc-integration '(face char))
 '(neo-window-fixed-size nil)
 '(neo-window-width 30)
 '(next-screen-context-lines 10)
 '(ns-alternate-modifier 'super)
 '(ns-command-modifier 'meta)
 '(ns-use-srgb-colorspace t)
 '(ns-use-thin-smoothing t t)
 '(nvm-dir "/Users/donatasp/software/nvm")
 '(olivetti-body-width 80)
 '(olivetti-minimum-body-width 78)
 '(olivetti-style 'fancy)
 '(org-babel-lisp-eval-fn 'slime-eval)
 '(org-ellipsis " ↩")
 '(org-hide-emphasis-markers t)
 '(org-html-prefer-user-labels t)
 '(org-log-done 'time)
 '(org-modern-hide-stars t)
 '(org-startup-folded t)
 '(org-structure-template-alist
   '(("a" . "export ascii")
     ("c" . "center")
     ("C" . "comment")
     ("e" . "example")
     ("E" . "export")
     ("h" . "export html")
     ("l" . "export latex")
     ("q" . "quote")
     ("s" . "src")
     ("v" . "verse")
     ("n" . "note")))
 '(org-tags-column 0)
 '(org-todo-keyword-faces
   '(("TODO" org-todo)
     ("WAIT" :foreground "orange" :weight bold)
     ("DONE" org-done)
     ("CANCELLED" :foreground "grey" :weight bold)))
 '(org-todo-keywords
   '((sequence "TODO(t)" "WAIT(w@)" "|" "DONE(d!)" "CANCELLED(c@)")))
 '(package-selected-packages
   '(async php-mode org-modern dired-collapse dired-subtree string-edit-at-point keyfreq rainbow-mode sly marginalia vertico paren-face deadgrep sql-indent ox-slack solarized-theme seq edit-indirect flow-js2-mode inf-clojure browse-kill-ring ox-jira scala-mode paredit-everywhere-mode ox-gfm imenu-list eglot macrostep flymake-proselint jsonrpc flymake eldoc project json-mode editorconfig prettier paredit-menu exec-path-from-shell markdown-mode nodejs-repl typescript-mode wgrep xterm-color flymake-eslint olivetti ns-auto-titlebar cider lsp paredit-everywhere slime-company whitespace-cleanup-mode use-package undo-tree restclient protobuf-mode paredit hungry-delete git-timemachine dictionary csv-mode company))
 '(paredit-lighter " ParEd" t)
 '(prettier-inline-errors-flag t)
 '(prettier-lighter " Pr")
 '(query-replace-highlight t)
 '(quick-peek-add-spacer nil)
 '(quick-peek-position 'below)
 '(read-buffer-completion-ignore-case t)
 '(recentf-mode t)
 '(require-final-newline t)
 '(ring-bell-function nil)
 '(safe-local-variable-values
   '((diff-add-log-use-relative-names . t)
     (vc-git-annotate-switches . "-w")
     (eval org-writing-mode t)
     (eval yoshi-project-mode t)))
 '(save-interprogram-paste-before-kill t)
 '(save-place-mode t)
 '(savehist-mode t)
 '(scroll-error-top-bottom t)
 '(search-highlight t)
 '(sly-command-switch-to-existing-lisp 'always)
 '(sly-highlight-suppressed-forms t)
 '(text-scale-mode-step 1.1)
 '(transient-save-history nil)
 '(typescript-indent-level 2)
 '(undo-tree-auto-save-history nil)
 '(undo-tree-enable-undo-in-region t)
 '(undo-tree-history-directory-alist nil)
 '(undo-tree-mode-lighter " ut")
 '(vc-annotate-background nil)
 '(vc-annotate-background-mode nil)
 '(vc-annotate-very-old-color nil)
 '(vc-directory-exclusion-list
   '("SCCS" "RCS" "CVS" "MCVS" ".src" ".svn" ".git" ".hg" ".bzr" "_MTN" "_darcs" "{arch}" "node_modules" "dist" "brower_components" "coverage" "build"))
 '(version-control t)
 '(vertico-count-format '("%-6s " . "%s/%s"))
 '(vertico-mode t)
 '(visible-bell t)
 '(window-sides-slots '(1 0 2 2)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background "white" :foreground "#202020" :height 140))))
 '(cursor ((((background light)) (:background "red")) (((background dark)) (:background "green"))))
 '(eglot-highlight-symbol-face ((t (:inherit highlight))))
 '(eshell-ls-directory ((t (:inherit (bold font-lock-function-name-face)))))
 '(eshell-prompt ((((background dark)) (:inherit nil :foreground "pink" :weight bold)) (t (:inherit nil :foreground "dark red" :weight bold))))
 '(fixed-pitch ((t (:weight normal :family "monospace"))))
 '(fixed-pitch-serif ((t (:weight normal :family "monospace"))))
 '(font-lock-comment-face ((t (:inherit shadow :slant italic))))
 '(fringe ((t (:inherit default :foreground "grey50"))))
 '(mode-line ((t (:background "grey75" :foreground "black" :box (:line-width (1 . 1) :style released-button) :weight medium :height 1.0 :width normal :family "Avenir Next"))))
 '(mode-line-inactive ((t (:inherit mode-line :background "grey75" :foreground "grey50"))))
 '(org-document-title ((((background dark)) (:foreground "pale turquoise" :weight bold :height 1.5)) (t (:foreground "midnight blue" :weight bold :height 1.5))))
 '(org-ellipsis ((t (:foreground "DarkGoldenrod"))))
 '(org-tag ((t (:inverse-video t))))
 '(outline-1 ((((background dark)) (:inherit font-lock-function-name-face :overline "gray20" :height 1.2)) (t (:inherit font-lock-function-name-face :overline "azure2" :height 1.2))))
 '(outline-2 ((t (:inherit font-lock-variable-name-face :height 1.1))))
 '(variable-pitch ((t (:weight medium :family "Avenir Next")))))
