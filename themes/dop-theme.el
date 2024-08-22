(deftheme dop)

(custom-theme-set-faces
 'dop
 ;; '(default ((((background light)) (:height 110 :family "Cascadia Code" :foreground "#20202f" :weight semilight))
 ;;            (((background dark))  (:height 110 :family "Cascadia Code" :foreground "#f0f0ff" :weight semilight))))
 '(cursor ((((background light)) (:background "red"))
           (((background dark)) (:background "green"))))
 '(variable-pitch ((t (:weight medium :family "Avenir Next"))))
 '(eglot-highlight-symbol-face ((t (:inherit highlight))))
 ;; '(font-lock-comment-face ((t (:inherit shadow :slant italic))))
 ;; '(fringe ((((background light)) (:background "#efefff" :foreground "#7f7f8f"))
 ;;           (((background dark))  (:background "#2f2f3f" :foreground "#7f7f8f"))))
 ;; '(git-gutter:added ((t (:foreground "yellow green"))))
 ;; '(git-gutter:deleted ((t (:foreground "tomato"))))
 ;; '(git-gutter:modified ((t (:foreground "orchid"))))
 '(mode-line ((t (:inherit variable-pitch :background "grey75" :foreground "black" :box (:line-width (1 . 1) :style released-button) :height 1.0))))
 '(mode-line-inactive ((t (:inherit mode-line :background "grey75" :foreground "grey50"))))
 ;; '(org-document-title ((((background dark)) (:foreground "pale turquoise" :weight bold :height 1.5)) (t (:foreground "midnight blue" :weight bold :height 1.5))))
 ;; '(org-ellipsis ((t (:foreground "DarkGoldenrod"))))
 ;; '(org-tag ((t (:inverse-video t))))
 ;; '(outline-1 ((((background dark)) (:inherit font-lock-function-name-face :overline "gray20" :height 1.2)) (t (:inherit font-lock-function-name-face :overline "azure2" :height 1.2))))
 ;; '(outline-2 ((t (:inherit font-lock-variable-name-face :height 1.1))))
 '(trailing-whitespace ((((background light)) (:background "yellow")) (((background dark)) (:background "yellow4"))))

 '(vertical-border ((t (:foreground "grey")))))

(provide-theme 'dop)
