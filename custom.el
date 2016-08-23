;; Emacs customization file
;; Time-stamp: <2013-11-18 20:53:31 donatas>
;; custom.el

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["black" "red3" "ForestGreen" "yellow3" "blue" "magenta3" "DeepSkyBlue" "gray50"])
 '(backup-directory-alist (\` (("." \, (expand-file-name "~/.emacs.d/backups")))))
 '(browse-url-browser-function (quote browse-url-generic))
 '(browse-url-generic-program "open")
 '(c-block-comment-prefix "* ")
 '(compilation-message-face (quote default))
 '(cua-auto-tabify-rectangles nil)
 '(cua-enable-cua-keys nil)
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-mode nil nil (cua-base))
 '(cua-normal-cursor-color "#839496")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(custom-safe-themes
   (quote
    ("82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" default)))
 '(delete-selection-mode nil)
 '(dired-listing-switches "-alF")
 '(display-time-24hr-format t t)
 '(display-time-day-and-date t)
 '(ediff-window-setup-function (quote ediff-setup-windows-plain))
 '(electric-pair-mode t)
 '(ensime-sem-high-faces
   (quote
    ((var :foreground "#9876aa" :underline
          (:style wave :color "yellow"))
     (val :foreground "#9876aa")
     (varField :slant italic)
     (valField :foreground "#9876aa" :slant italic)
     (functionCall :foreground "#a9b7c6")
     (implicitConversion :underline
                         (:color "#808080"))
     (implicitParams :underline
                     (:color "#808080"))
     (operator :foreground "#cc7832")
     (param :foreground "#a9b7c6")
     (class :foreground "#4e807d")
     (trait :foreground "#4e807d" :slant italic)
     (object :foreground "#6897bb" :slant italic)
     (package :foreground "#cc7832")
     (deprecated :strike-through "#a9b7c6"))))
 '(eshell-directory-name "~/.emacs.d/data/eshell/")
 '(fci-rule-color "#383838")
 '(fill-column 80)
 '(frame-title-format "emacs [%b %*%+ %f]" t)
 '(geben-dbgp-default-proxy (quote ("127.0.0.1" 9001 "emacs" nil t)))
 '(global-column-enforce-mode t)
 '(haskell-indent-after-keywords
   (quote
    (("where" 2 0)
     ("of" 2)
     ("do" 2)
     ("mdo" 2)
     ("rec" 2)
     ("in" 2 0)
     ("{" 2)
     ("if" 2)
     "then" "else"
     ("let" 2))))
 '(haskell-indent-thenelse 1)
 '(helm-autoresize-mode t)
 '(highlight-changes-colors (quote ("#d33682" "#6c71c4")))
 '(highlight-indentation-offset 2)
 '(highlight-symbol-colors
   (--map
    (solarized-color-blend it "#002b36" 0.25)
    (quote
     ("#b58900" "#2aa198" "#dc322f" "#6c71c4" "#859900" "#cb4b16" "#268bd2"))))
 '(highlight-symbol-foreground-color "#93a1a1")
 '(highlight-tail-colors
   (quote
    (("#073642" . 0)
     ("#546E00" . 20)
     ("#00736F" . 30)
     ("#00629D" . 50)
     ("#7B6000" . 60)
     ("#8B2C02" . 70)
     ("#93115C" . 85)
     ("#073642" . 100))))
 '(hl-bg-colors
   (quote
    ("#7B6000" "#8B2C02" "#990A1B" "#93115C" "#3F4D91" "#00629D" "#00736F" "#546E00")))
 '(hl-fg-colors
   (quote
    ("#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36")))
 '(icon-title-format "emacs [%b]" t)
 '(inhibit-startup-screen t)
 '(initial-scratch-message nil)
 '(js-curly-indent-offset 0)
 '(js-indent-level 2)
 '(js2-basic-offset 2)
 '(js2-enter-indents-newline t)
 '(magit-diff-use-overlays nil)
 '(magit-pull-arguments nil)
 '(magit-use-overlays nil)
 '(mark-even-if-inactive t)
 '(max-lisp-eval-depth 4096)
 '(max-specpdl-size 8192)
 '(mouse-sel-retain-highlight t)
 '(nrepl-message-colors
   (quote
    ("#dc322f" "#cb4b16" "#b58900" "#546E00" "#B4C342" "#00629D" "#2aa198" "#d33682" "#6c71c4")))
 '(org-agenda-files nil)
 '(org-export-htmlize-output-type (quote css))
 '(org-startup-folded t)
 '(package-selected-packages
   (quote
    (elm-mode quelpa-use-package flycheck-elm enlive request flycheck-flow smartparens php-mode js2-highlight-vars zenburn-theme yaml-mode xterm-color wrap-region windata whitespace-cleanup-mode wgrep web-mode w3m visual-regexp-steroids visual-fill-column utop unicode-fonts undo-tree typo tuareg tss tree-mode syntax-subword sql-indent solarized-theme smex scss-mode sass-mode restclient rainbow-mode rainbow-delimiters queue psci psc-ide popwin paredit ox-reveal ox-pandoc ox-ioslide ox-impress-js org-plus-contrib olivetti ocp-indent nodejs-repl nginx-mode neotree monokai-theme monochrome-theme mongo mmm-mode markdown-mode magit macrostep litable leuven-theme js2-refactor inf-mongo ido-vertical-mode ibuffer-vc ibuffer-projectile hungry-delete htmlize hl-line+ helm-swoop helm-ls-git helm-git-grep helm-css-scss haskell-mode graphviz-dot-mode goto-chg git-gutter-fringe flycheck-ocaml flycheck-color-mode-line flx-isearch flx-ido expand-region exec-path-from-shell eproject epresent ensime elisp-slime-nav editorconfig dropdown-list drag-stuff dockerfile-mode docker-tramp docker dired-details dictionary dash-functional darcula-theme cyberpunk-theme csv-mode css-eldoc command-log-mode column-enforce-mode color-theme-sanityinc-tomorrow clojure-mode-extra-font-locking ace-jump-mode ac-js2)))
 '(pos-tip-background-color "#073642")
 '(pos-tip-foreground-color "#93a1a1")
 '(query-replace-highlight t)
 '(require-final-newline t)
 '(safe-local-variable-values
   (quote
    ((eval defalias js2-mode js2-jsx-mode)
     (sgml-basic-offset . 4)
     (js2-missing-semi-one-line-override . t)
     (js2-strict-missing-semi-warning))))
 '(scroll-bar-mode nil)
 '(scss-compile-at-save nil)
 '(search-highlight t)
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#839496" 0.2))
 '(speedbar-use-images t)
 '(sqlplus-format-output-tables-flag t)
 '(syslog-debug-face
   (quote
    ((t :background unspecified :foreground "#2aa198" :weight bold))))
 '(syslog-error-face
   (quote
    ((t :background unspecified :foreground "#dc322f" :weight bold))))
 '(syslog-hour-face (quote ((t :background unspecified :foreground "#859900"))))
 '(syslog-info-face
   (quote
    ((t :background unspecified :foreground "#268bd2" :weight bold))))
 '(syslog-ip-face (quote ((t :background unspecified :foreground "#b58900"))))
 '(syslog-su-face (quote ((t :background unspecified :foreground "#d33682"))))
 '(syslog-warn-face
   (quote
    ((t :background unspecified :foreground "#cb4b16" :weight bold))))
 '(term-default-bg-color "#002b36")
 '(term-default-fg-color "#839496")
 '(tramp-auto-save-directory "~/.emacs.d/data/tramp-auto-save")
 '(tramp-persistency-file-name "/home/donatas/.emacs.d/data/tramp")
 '(transient-mark-mode 1)
 '(url-configuration-directory "~/.emacs.d/data/url/")
 '(vc-annotate-background "#2B2B2B")
 '(vc-annotate-color-map
   (quote
    ((20 . "#BC8383")
     (40 . "#CC9393")
     (60 . "#DFAF8F")
     (80 . "#D0BF8F")
     (100 . "#E0CF9F")
     (120 . "#F0DFAF")
     (140 . "#5F7F5F")
     (160 . "#7F9F7F")
     (180 . "#8FB28F")
     (200 . "#9FC59F")
     (220 . "#AFD8AF")
     (240 . "#BFEBBF")
     (260 . "#93E0E3")
     (280 . "#6CA0A3")
     (300 . "#7CB8BB")
     (320 . "#8CD0D3")
     (340 . "#94BFF3")
     (360 . "#DC8CC3"))))
 '(vc-annotate-very-old-color "#DC8CC3")
 '(warning-suppress-types (quote ((undo discard-info))))
 '(weechat-color-list
   (quote
    (unspecified "#002b36" "#073642" "#990A1B" "#dc322f" "#546E00" "#859900" "#7B6000" "#b58900" "#00629D" "#268bd2" "#93115C" "#d33682" "#00736F" "#2aa198" "#839496" "#657b83")))
 '(xterm-color-names
   ["#073642" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#eee8d5"])
 '(xterm-color-names-bright
   ["#002b36" "#cb4b16" "#586e75" "#657b83" "#839496" "#6c71c4" "#93a1a1" "#fdf6e3"]))
