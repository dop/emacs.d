(deftheme dp-dark)

(custom-theme-set-faces
 'dp-dark
 `(default ((t (:background "gray10" :foreground "white"))))
 `(variable-pitch ((t (:family "Georgia"))))
 `(fixed-pitch ((t (:family "Courier"))))
 `(org-block ((t (:inherit fixed-pitch))))
 `(fringe ((t (:background "gray20"))))
 `(font-lock-comment-face ((t (:foreground "chocolate" :slant normal))))
 `(git-gutter:added ((t (:foreground "yellow green" :inherit nil))))
 `(git-gutter:deleted ((t (:foreground "tomato" :inherit nil))))
 `(git-gutter:modified ((t (:foreground "orchid" :inherit nil))))
 `(trailing-whitespace ((t (:background "hot pink"))))
 `(error ((t (:foreground "tomato" :underline "red"))))
 `(js2-external-variable ((t (:foreground "gray60"))))
 `(mode-line ((t (:inherit default :overline "gray60" :box (:line-width 2 :color "gray10")))))
 `(mode-line-inactive ((t (:inherit default :foreground "gray60" :overline "gray40" :box (:line-width 2 :color "gray10")))))
 `(cursor ((t (:background "red")))))

(provide-theme 'dp-dark)
