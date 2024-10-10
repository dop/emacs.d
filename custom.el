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
 '(custom-enabled-themes '(dop))
 '(custom-safe-themes
   '("fcaa2fa574b2f03e92b685bfb3de1a5de1ce6f95474fbdac7371ec665d9298de" "b87509a246c36477715801609ee7cb257afa1128d6ae98c6b6310e21b15613ad"))
 '(custom-theme-directory "~/.emacs.d/themes/")
 '(debug-on-error nil)
 '(delete-by-moving-to-trash t)
 '(delete-old-versions t)
 '(diff-hl-draw-borders nil)
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
 '(display-line-numbers-grow-only t)
 '(display-time-24hr-format t)
 '(display-time-day-and-date t)
 '(display-time-default-load-average nil)
 '(display-time-mode t)
 '(editorconfig-mode-lighter " ec")
 '(ef-themes-mixed-fonts t)
 '(ef-themes-variable-pitch-ui t)
 '(eglot-autoreconnect nil)
 '(eglot-autoshutdown t)
 '(eglot-confirm-server-edits nil)
 '(eglot-confirm-server-initiated-edits nil)
 '(eglot-connect-timeout 10)
 '(eglot-events-buffer-size 0)
 '(eglot-override-flymake-diagnostic-functions nil)
 '(eglot-sync-connect nil)
 '(eldoc-minor-mode-string " doc")
 '(electric-pair-mode t)
 '(elfeed-feeds
   '("https://chrisdone.com/rss.xml" "https://planet.emacslife.com/atom.xml" "https://irreal.org/blog/?feed=rss2" "https://nullprogram.com/feed/"))
 '(epa-pinentry-mode 'loopback)
 '(epg-pinentry-mode 'loopback)
 '(eshell-hist-ignoredups t)
 '(eshell-prompt-function 'my-eshell-prompt)
 '(exec-path-from-shell-check-startup-files nil)
 '(exec-path-from-shell-variables
   '("PATH" "MANPATH" "FNM_LOGLEVEL" "FNM_COREPACK_ENABLED" "FNM_RESOLVE_ENGINES" "FNM_VERSION_FILE_STRATEGY" "FNM_MULTISHELL_PATH" "FNM_DIR" "FNM_NODE_DIST_MIRROR" "FNM_ARCH" "NODE_OPTIONS" "LATEST_JDK" "JAVA_HOME" "AUTOMATION_MASTER_KEY"))
 '(fast-but-imprecise-scrolling t)
 '(flymake-eslint-defer-binary-check t)
 '(flymake-eslint-prefer-json-diagnostics t)
 '(fold-this-overlay-text "...")
 '(frame-inhibit-implied-resize t)
 '(gc-cons-threshold 268435456)
 '(grep-find-ignored-directories
   '("SCCS" "RCS" "CVS" "MCVS" ".src" ".svn" ".git" ".hg" ".bzr" "_MTN" "_darcs" "{arch}" "node_modules" "dist" "brower_components" "coverage" "build" ".angular"))
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
 '(modus-themes-variable-pitch-ui t)
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
 '(ns-use-thin-smoothing nil t)
 '(nvm-dir "/Users/donatasp/software/nvm")
 '(olivetti-body-width 120)
 '(olivetti-minimum-body-width 78)
 '(olivetti-style 'fancy)
 '(org-babel-lisp-eval-fn 'slime-eval)
 '(org-ellipsis " …")
 '(org-hide-emphasis-markers t)
 '(org-html-prefer-user-labels t)
 '(org-log-done 'time)
 '(org-modern-hide-stars t)
 '(org-plantuml-jar-path "/opt/local/share/java/plantuml/plantuml.jar")
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
   '(orderless treesit-auto buffer-env elfeed diff-hl web-mode vue-mode magit neotree org-download graphviz-dot-mode plantuml-mode jq-mode async php-mode org-modern dired-collapse dired-subtree keyfreq rainbow-mode sly paren-face sql-indent ox-slack seq edit-indirect browse-kill-ring ox-jira scala-mode ox-gfm flymake-proselint flymake eldoc json-mode prettier paredit-menu exec-path-from-shell xterm-color flymake-eslint ns-auto-titlebar cider paredit-everywhere slime-company whitespace-cleanup-mode use-package undo-tree restclient protobuf-mode paredit hungry-delete dictionary csv-mode))
 '(paredit-lighter " ParEd" t)
 '(plantuml-default-exec-mode 'jar)
 '(plantuml-indent-level 4)
 '(plantuml-jar-path "/opt/local/share/java/plantuml/plantuml.jar")
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
   '((project-preferred-root-resolution . top)
     (olivetti-body-width . 120)
     (diff-add-log-use-relative-names . t)
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
 '(trash-directory "~/.Trash")
 '(treesit-auto-install 'prompt)
 '(treesit-font-lock-level 4)
 '(typescript-indent-level 2)
 '(undo-tree-auto-save-history nil)
 '(undo-tree-enable-undo-in-region t)
 '(undo-tree-history-directory-alist nil)
 '(undo-tree-mode-lighter " ut")
 '(vc-annotate-background nil)
 '(vc-annotate-background-mode nil)
 '(vc-annotate-very-old-color nil)
 '(vc-directory-exclusion-list
   '("SCCS" "RCS" "CVS" "MCVS" ".src" ".svn" ".git" ".hg" ".bzr" "_MTN" "_darcs" "{arch}" "node_modules" "dist" "bower_components" "coverage" "build"))
 '(vc-git-root-log-format
   '("%h %as %an: %s" "^\\(?:[*/\\| ]+ \\)?\\([0-9a-f]+\\) \\([0-9-]+\\) \\([^:]+\\): .+$"
     ((1 'log-view-message)
      (2 'change-log-date)
      (3 'change-log-name))))
 '(vc-make-backup-files t)
 '(version-control t)
 '(vertico-count-format '("%-6s " . "%s/%s"))
 '(vertico-mode t)
 '(visible-bell t)
 '(window-sides-slots '(1 0 2 2))
 '(xref-search-program 'ripgrep))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
