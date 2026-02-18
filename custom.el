;; -*- lexical-binding: t; -*-
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(async-shell-command-buffer 'confirm-kill-process)
 '(backup-by-copying t)
 '(backup-directory-alist '(("." . "~/.emacs.d/backups")))
 '(blink-cursor-blinks 0)
 '(comint-eol-on-send t)
 '(comint-input-ignoredups t)
 '(comint-move-point-for-output nil)
 '(comint-process-echoes t)
 '(comint-scroll-show-maximum-output nil)
 '(comment-multi-line t)
 '(company-tooltip-align-annotations t)
 '(compilation-always-kill t)
 '(compilation-ask-about-save nil)
 '(compilation-message-face 'default)
 '(completion-preview-minimum-symbol-length 1)
 '(completion-show-help nil)
 '(create-lockfiles nil)
 '(css-indent-offset 2)
 '(csv-separators '("," "\11" ";"))
 '(cursor-in-non-selected-windows nil)
 '(custom-enabled-themes '(dop))
 '(custom-safe-themes
   '("5ce31ca43f656a3fca504e9a84cde8f96c58d55ad7713fe525f8596d8a47363e"
     "68e9ab92a95e1ab84478c94a912766b00cf2b492b6b56db39f7e5d31c65498a2"
     "59e54b18c77609cbfa88d1a9b48cb865e175fe1425f453dabec938d179f0ac0d"
     "bbe58ef57bf5647a6c8f025f395b4440f05511c80b513e70b849c777518c81d3"
     "8927324897fb3a82651e50778ec5afc7c11ae241face4120d7ffd17d3c46f485"
     "8937605989e56ae4b31e522b248b5a2907073ee9c5f8d0bb7259412034128daf"))
 '(custom-theme-directory "~/.emacs.d/themes/")
 '(debug-on-error nil)
 '(delete-by-moving-to-trash t)
 '(delete-old-versions t)
 '(dictionary-server "dict.org")
 '(diff-hl-draw-borders t)
 '(dired-dwim-target t)
 '(dired-free-space nil)
 '(dired-listing-switches "-ahl")
 '(dired-omit-files "\\`\\(\\.\\|#\\)")
 '(dired-subtree-use-backgrounds nil)
 '(display-battery-mode t)
 '(display-buffer-alist
   '(((or "e?shell\\*" "\\*sly-mrepl for")
      (display-buffer-reuse-window display-buffer-in-previous-window
                                   display-buffer-at-bottom)
      (window-height . 0.375))
     ("\\*\\(Async Shell Command\\|Shell Command Output\\)\\*"
      (display-buffer-reuse-window display-buffer-in-previous-window
                                   display-buffer-at-bottom)
      (window-height . 0.375))
     ("\\*\\([Hh]elp\\|Backtrace\\|Warnings\\|Messages\\|info\\|Apropos\\|vc-diff\\)\\*"
      (display-buffer-reuse-window display-buffer-in-previous-window
                                   display-buffer-same-window
                                   display-buffer-pop-up-window)
      (window-height . 0.375) (window-width . 0.375)
      (window-min-width . 80))
     ("\\*Completions\\*"
      (display-buffer-reuse-window display-buffer-in-previous-window
                                   display-buffer-at-bottom)
      (window-height . shrink-window-if-larger-than-buffer))
     ("compilation\\*"
      (display-buffer-reuse-window display-buffer-in-previous-window
                                   display-buffer-at-bottom)
      (window-parameters (no-delete-other-windows)))))
 '(display-buffer-base-action
   '(display-buffer-reuse-window display-buffer-reuse-mode-window
                                 display-buffer-same-window
                                 display-buffer-in-previous-window))
 '(display-line-numbers-grow-only t)
 '(display-time-24hr-format t)
 '(display-time-day-and-date t)
 '(display-time-default-load-average 1)
 '(display-time-format "%Y-%m-%d")
 '(display-time-mode t)
 '(dumb-jump-debug nil)
 '(dumb-jump-force-searcher 'rg)
 '(dumb-jump-prefer-searcher 'git-grep-plus-ag)
 '(editorconfig-mode-lighter " ec")
 '(ef-themes-mixed-fonts t)
 '(ef-themes-variable-pitch-ui t)
 '(eglot-autoreconnect nil)
 '(eglot-autoshutdown t)
 '(eglot-code-action-indications nil)
 '(eglot-code-action-indicator "?")
 '(eglot-confirm-server-edits nil)
 '(eglot-confirm-server-initiated-edits nil)
 '(eglot-connect-timeout 10)
 '(eglot-events-buffer-size 0)
 '(eglot-override-flymake-diagnostic-functions nil)
 '(eglot-prefer-plaintext t)
 '(eglot-sync-connect nil)
 '(eldoc-echo-area-prefer-doc-buffer t)
 '(eldoc-minor-mode-string " doc")
 '(electric-pair-mode t)
 '(elfeed-feeds
   '("https://atthis.link/rss.xml" "https://howardism.org/index.xml"
     "https://chrisdone.com/rss.xml"
     "https://planet.emacslife.com/atom.xml"
     "https://irreal.org/blog/?feed=rss2"
     "https://nullprogram.com/feed/"))
 '(elfeed-search-filter "@1-months-ago +unread")
 '(elfeed-show-entry-delete 'delete-window)
 '(elfeed-show-entry-switch 'pop-to-buffer)
 '(elfeed-show-unique-buffers t)
 '(epa-pinentry-mode 'loopback)
 '(epg-pinentry-mode 'loopback)
 '(eshell-banner-message "")
 '(eshell-hist-ignoredups t)
 '(eshell-prompt-function 'my-eshell-prompt)
 '(exec-path-from-shell-check-startup-files nil)
 '(exec-path-from-shell-variables
   '("PATH" "MANPATH" "FNM_LOGLEVEL" "FNM_COREPACK_ENABLED"
     "FNM_RESOLVE_ENGINES" "FNM_VERSION_FILE_STRATEGY"
     "FNM_MULTISHELL_PATH" "FNM_DIR" "FNM_NODE_DIST_MIRROR" "FNM_ARCH"
     "NODE_OPTIONS" "LATEST_JDK" "JAVA_HOME" "AUTOMATION_MASTER_KEY"
     "NVM_DIR"))
 '(fast-but-imprecise-scrolling t)
 '(ffap-ftp-regexp nil)
 '(ffap-lax-url nil)
 '(ffap-machine-p-known 'reject)
 '(flymake-eslint-defer-binary-check t)
 '(flymake-eslint-prefer-json-diagnostics t)
 '(fold-this-overlay-text "...")
 '(frame-inhibit-implied-resize t)
 '(gc-cons-threshold 268435456)
 '(gptel-default-mode 'org-mode)
 '(gptel-include-reasoning nil)
 '(gptel-log-level 'debug)
 '(gptel-model 'gemma3n:latest)
 '(gptel-temperature 0.4)
 '(gptel-track-media nil)
 '(grep-find-ignored-directories
   '("SCCS" "RCS" "CVS" "MCVS" ".src" ".svn" ".git" ".hg" ".bzr" "_MTN"
     "_darcs" "{arch}" "node_modules" "dist" "brower_components"
     "coverage" "build" ".angular"))
 '(grep-find-ignored-files
   '(".#*" "*.o" "*~" "*.bin" "*.lbin" "*.so" "*.a" "*.ln" "*.blg"
     "*.bbl" "*.elc" "*.lof" "*.glo" "*.idx" "*.lot" "*.fmt" "*.tfm"
     "*.class" "*.fas" "*.lib" "*.mem" "*.x86f" "*.sparcf" "*.dfsl"
     "*.pfsl" "*.d64fsl" "*.p64fsl" "*.lx64fsl" "*.lx32fsl"
     "*.dx64fsl" "*.dx32fsl" "*.fx64fsl" "*.fx32fsl" "*.sx64fsl"
     "*.sx32fsl" "*.wx64fsl" "*.wx32fsl" "*.fasl" "*.ufsl" "*.fsl"
     "*.dxl" "*.lo" "*.la" "*.gmo" "*.mo" "*.toc" "*.aux" "*.cp"
     "*.fn" "*.ky" "*.pg" "*.tp" "*.vr" "*.cps" "*.fns" "*.kys"
     "*.pgs" "*.tps" "*.vrs" "*.pyc" "*.pyo" "*.js.map"))
 '(hippie-expand-try-functions-list
   '(try-complete-file-name-partially try-complete-file-name
                                      try-expand-all-abbrevs
                                      try-expand-list
                                      try-expand-dabbrev
                                      try-expand-dabbrev-all-buffers
                                      try-expand-dabbrev-from-kill
                                      try-expand-line
                                      try-complete-lisp-symbol-partially
                                      try-complete-lisp-symbol))
 '(ibuffer-formats
   '((mark modified read-only locked " " (name 32 32 :left :elide) " "
           (size 9 -1 :right) " " (mode 16 16 :left :elide) " "
           filename-and-process)
     (mark " " (name 16 -1) " " filename)))
 '(ibuffer-saved-filter-groups nil)
 '(indent-tabs-mode nil)
 '(indicate-buffer-boundaries 'left)
 '(indicate-empty-lines nil)
 '(inferior-lisp-program "sbcl --dynamic-space-size 2048")
 '(inhibit-compacting-font-caches t t)
 '(inhibit-startup-echo-area-message "donatasp")
 '(inhibit-startup-screen t)
 '(initial-scratch-message nil)
 '(ispell-local-dictionary-alist
   '((nil "[a-zA-Z]" "[^a-zA-Z]" "[']" nil ("-l" "en") nil utf-8)
     ("english" "[a-zA-Z]" "[^a-zA-Z]" "[']" nil ("-l" "en") nil utf-8)
     ("lithuanian" "[a-zA-ZąčęėįšųūžĄČĘĖĮŠŲŪŽ]"
      "[^a-zA-ZąčęėįšųūžĄČĘĖĮŠŲŪŽ]" "" nil ("-l" "lt") nil utf-8)))
 '(jit-lock-defer-time 0)
 '(js-indent-level 2)
 '(kept-new-versions 6)
 '(kept-old-versions 2)
 '(locale-coding-system 'utf-8 t)
 '(ls-lisp-dirs-first t)
 '(ls-lisp-emulation nil)
 '(ls-lisp-ignore-case t)
 '(ls-lisp-use-insert-directory-program nil)
 '(markdown-code-lang-modes
   '(("ocaml" . tuareg-mode) ("elisp" . emacs-lisp-mode)
     ("ditaa" . artist-mode) ("asymptote" . asy-mode)
     ("dot" . fundamental-mode) ("sqlite" . sql-mode)
     ("calc" . fundamental-mode) ("C" . c-mode) ("cpp" . c++-mode)
     ("C++" . c++-mode) ("screen" . shell-script-mode)
     ("shell" . sh-mode) ("bash" . sh-mode)
     ("ts" . typescript-ts-mode)))
 '(markdown-command "pandoc")
 '(mode-line-collapse-minor-modes t)
 '(mode-line-compact t)
 '(mode-line-format
   '("%e" mode-line-front-space
     (:propertize
      ("" mode-line-mule-info mode-line-client mode-line-modified
       mode-line-remote)
      display (min-width (5.0)))
     mode-line-frame-identification
     enumerated-windows-mode-line-number
     mode-line-buffer-identification "   " mode-line-position
     (vc-mode vc-mode) "  " mode-line-modes mode-line-misc-info
     mode-line-end-spaces))
 '(mode-line-position-column-line-format '(" %l:%c"))
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
 '(olivetti-body-width 80)
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
   '(("a" . "export ascii") ("c" . "center") ("C" . "comment")
     ("e" . "example") ("E" . "export") ("h" . "export html")
     ("l" . "export latex") ("q" . "quote") ("s" . "src")
     ("v" . "verse") ("n" . "note")))
 '(org-tags-column 0)
 '(org-todo-keyword-faces
   '(("TODO" org-todo) ("WAIT" :foreground "orange" :weight bold)
     ("DONE" org-done) ("CANCELLED" :foreground "grey" :weight bold)))
 '(org-todo-keywords
   '((sequence "TODO(t)" "WAIT(w@)" "|" "DONE(d!)" "CANCELLED(c@)")))
 '(package-selected-packages
   '(alabaster-themes ascii-art-to-unicode bazel browse-kill-ring
                      buffer-env csv-mode cycle-quotes dictionary
                      dired-collapse dired-subtree edit-indirect eldoc
                      embark exec-path-from-shell flymake gptel
                      gptel-agent hungry-delete jq-mode json-mode
                      ligature macrostep markdown-preview-mode neotree
                      nodejs-repl ns-auto-titlebar ox-gfm
                      paredit-everywhere php-mode prettier prettier-js
                      project protobuf-mode rainbow-mode scala-mode
                      seq sly sql-indent string-edit-at-point
                      string-inflection treesit-auto treesit-fold
                      wgrep whitespace-cleanup-mode winpulse
                      xterm-color yaml-mode))
 '(paredit-lighter " ParEd" t)
 '(plantuml-default-exec-mode 'jar)
 '(plantuml-indent-level 4)
 '(plantuml-jar-path "/opt/local/share/java/plantuml/plantuml.jar")
 '(prettier-inline-errors-flag t)
 '(prettier-js-show-errors 'buffer)
 '(prettier-lighter " Pr")
 '(project-switch-commands
   '((project-find-file "Find file" nil)
     (project-find-regexp "Find regexp" nil)
     (project-vc-dir "VC-Dir" nil) (project-eshell "Eshell" nil)
     (project-compile "Compile" nil) (project-run-command "Run" nil)
     (project-any-command "Other" nil)))
 '(query-replace-highlight t)
 '(quick-peek-add-spacer nil)
 '(quick-peek-position 'below)
 '(read-buffer-completion-ignore-case t)
 '(recentf-mode t)
 '(require-final-newline t)
 '(ring-bell-function nil)
 '(save-interprogram-paste-before-kill t)
 '(save-place-mode t)
 '(savehist-mode t)
 '(scroll-error-top-bottom t)
 '(search-highlight t)
 '(size-indication-mode nil)
 '(sly-command-switch-to-existing-lisp 'always)
 '(sly-highlight-suppressed-forms t)
 '(switch-to-buffer-in-dedicated-window 'pop)
 '(switch-to-buffer-obey-display-actions nil)
 '(tempo-interactive t)
 '(text-scale-mode-step 1.1)
 '(transient-save-history nil)
 '(trash-directory "~/.Trash")
 '(treesit-auto-install 'prompt)
 '(treesit-auto-langs
   '(awk bash bibtex blueprint c c-sharp clojure cmake commonlisp cpp css
         dart dockerfile elixir glsl go gomod heex html java
         javascript json julia kotlin lua magik make nix nu org perl
         proto python r ruby rust scala sql surface toml tsx
         typescript typst verilog vhdl vue wast wat wgsl yaml))
 '(treesit-fold-line-count-show nil)
 '(treesit-fold-summary-show nil)
 '(treesit-font-lock-level 4)
 '(typescript-indent-level 2)
 '(undo-tree-auto-save-history nil)
 '(undo-tree-enable-undo-in-region t)
 '(undo-tree-history-directory-alist nil)
 '(undo-tree-mode-lighter " ut")
 '(vc-allow-rewriting-published-history t)
 '(vc-annotate-background nil)
 '(vc-annotate-background-mode t)
 '(vc-annotate-very-old-color nil)
 '(vc-directory-exclusion-list
   '("SCCS" "RCS" "CVS" "MCVS" ".src" ".svn" ".git" ".hg" ".bzr" "_MTN"
     "_darcs" "{arch}" "node_modules" "dist" "bower_components"
     "coverage" "build"))
 '(vc-git-root-log-format
   '("%h %as %an: %s"
     "^\\(?:[*/\\| ]+ \\)?\\([0-9a-f]+\\) \\([0-9-]+\\) \\([^:]+\\): .+$"
     ((1 'log-view-message) (2 'change-log-date) (3 'change-log-name))))
 '(vc-make-backup-files t)
 '(version-control t)
 '(vertico-count-format '("%-6s " . "%s/%s"))
 '(vertico-mode t)
 '(visible-bell t)
 '(window-sides-slots '(1 0 2 2))
 '(winpulse-duration 0.5)
 '(winpulse-step-interval 0.1)
 '(xref-auto-jump-to-first-definition t)
 '(xref-auto-jump-to-first-xref t)
 '(xref-search-program 'ripgrep)
 '(xref-show-definitions-function 'xref-show-definitions-buffer))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(variable-pitch ((t (:weight normal :family "SF Pro Display")))))
