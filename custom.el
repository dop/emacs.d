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
 '(compilation-message-face 'default)
 '(create-lockfiles nil)
 '(css-indent-offset 2)
 '(csv-separators '("," "	" ";"))
 '(debug-on-error nil)
 '(delete-old-versions t)
 '(display-buffer-alist
   '(("\\*\\(.*eshell\\|sly-mrepl for.*\\)\\*"
      (display-buffer-reuse-window display-buffer-in-previous-window display-buffer-in-side-window)
      (window-height . 0.375)
      (side . bottom)
      (slot . -1))
     ("\\*Flymake diagnostics"
      (display-buffer-reuse-window display-buffer-in-previous-window display-buffer-in-side-window)
      (window-height . shrink-window-if-larger-than-buffer)
      (side . bottom)
      (slot . 1))
     ("\\*\\(Async Shell Command\\|Shell Command Output\\)\\*"
      (display-buffer-reuse-window display-buffer-in-previous-window display-buffer-in-side-window)
      (window-height . 0.375)
      (side . bottom)
      (slot . 1))
     ("\\*\\([Hh]elp\\|Backtrace\\|Warnings\\|Messages\\|info\\|Apropos\\)\\*"
      (display-buffer-reuse-window display-buffer-in-previous-window display-buffer-in-side-window)
      (window-width . 0.375)
      (side . right)
      (slot . 1))
     ("\\*\\(compilation\\|vc-\\)\\*"
      (display-buffer-reuse-window display-buffer-in-previous-window display-buffer-in-side-window)
      (window-width . 0.375)
      (side . right)
      (slot . -1))
     ("\\*Completions\\*"
      (display-buffer-reuse-window display-buffer-in-previous-window display-buffer-at-bottom)
      (window-height . shrink-window-if-larger-than-buffer))
     ("\\*\\(NeoTree\\|Ilist\\)\\*"
      (display-buffer-reuse-window display-buffer-in-previous-window display-buffer-in-side-window)
      (window-width . 0.2)
      (side . left))))
 '(editorconfig-mode-lighter " 🐭")
 '(eglot-override-flymake-diagnostic-functions nil)
 '(eldoc-minor-mode-string " 📚")
 '(electric-pair-mode t)
 '(epa-pinentry-mode 'loopback)
 '(epg-pinentry-mode 'loopback)
 '(eshell-hist-ignoredups t)
 '(fast-but-imprecise-scrolling t)
 '(flymake-eslint-defer-binary-check t)
 '(gc-cons-threshold 268435456)
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
 '(mode-require-final-newline t)
 '(neo-autorefresh nil)
 '(neo-smart-open t)
 '(neo-theme 'nerd)
 '(neo-vc-integration '(face char))
 '(neo-window-fixed-size nil)
 '(neo-window-width nil)
 '(next-screen-context-lines 10)
 '(ns-alternate-modifier 'super)
 '(ns-command-modifier 'meta)
 '(ns-use-srgb-colorspace t)
 '(ns-use-thin-smoothing t t)
 '(olivetti-body-width 78)
 '(olivetti-minimum-body-width 78)
 '(org-ellipsis "...")
 '(org-hide-emphasis-markers t)
 '(org-log-done 'time)
 '(org-startup-folded t)
 '(org-todo-keyword-faces
   '(("TODO" org-todo)
     ("WAIT" :foreground "orange" :weight bold)
     ("DONE" org-done)))
 '(org-todo-keywords '((sequence "TODO(t)" "WAIT(w@)" "DONE(d)")))
 '(package-selected-packages
   '(imenu-list eglot macrostep flymake-proselint jsonrpc flymake eldoc project json-mode editorconfig sly prettier paredit-menu exec-path-from-shell markdown-mode nodejs-repl string-edit typescript-mode wgrep xterm-color flymake-eslint olivetti ns-auto-titlebar cider quick-peek lsp paredit-everywhere slime-company whitespace-cleanup-mode use-package undo-tree restclient protobuf-mode paredit neotree hungry-delete git-timemachine dictionary csv-mode company))
 '(paredit-lighter " ParEd" t)
 '(prettier-inline-errors-flag t)
 '(prettier-lighter " Pr")
 '(query-replace-highlight t)
 '(quick-peek-add-spacer nil)
 '(quick-peek-position 'below)
 '(require-final-newline t)
 '(ring-bell-function nil)
 '(safe-local-variable-values '((eval yoshi-project-mode t)))
 '(save-interprogram-paste-before-kill t)
 '(scroll-error-top-bottom t)
 '(search-highlight t)
 '(text-scale-mode-step 1.1)
 '(transient-save-history nil)
 '(typescript-indent-level 2)
 '(undo-tree-mode-lighter " ᛦ")
 '(vc-annotate-background nil)
 '(vc-annotate-background-mode nil)
 '(vc-annotate-very-old-color nil)
 '(vc-directory-exclusion-list
   '("SCCS" "RCS" "CVS" "MCVS" ".src" ".svn" ".git" ".hg" ".bzr" "_MTN" "_darcs" "{arch}" "node_modules" "dist" "brower_components" "coverage" "build"))
 '(version-control t)
 '(visible-bell nil)
 '(window-sides-slots '(1 0 2 2)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:height 120 :family "Noto Sans Mono"))))
 '(cursor ((t (:background "red"))))
 '(eglot-mode-line ((t (:inherit font-lock-constant-face :weight normal))))
 '(fixed-pitch ((t (:inherti 'default :family "monospace"))))
 '(mode-line ((t (:inherit variable-pitch :background "grey75" :foreground "black" :box (:line-width (1 . -1) :style released-button)))))
 '(neo-file-link-face ((t (:inherit default :foreground "dark gray"))))
 '(olivetti-fringe ((t (:inherit default))))
 '(quick-peek-background-face ((t (:inherit default :extend t))))
 '(trailing-whitespace ((t (:background "pink"))))
 '(variable-pitch ((t (:family "Latin Modern Roman")))))
