(deftheme dp-light)

(custom-theme-set-faces
 'dp-light
 `(default ((t (:background "white" :foreground "gray10"))))
 `(variable-pitch ((t (:family "Georgia"))))
 `(fixed-pitch ((t (:family "Courier"))))
 `(org-block ((t (:inherit fixed-pitch))))
 `(org-table ((t (:inherit fixed-pitch))))
 `(fringe ((t (:background "white"))))
 `(font-lock-comment-face ((t (:foreground "chocolate" :slant normal))))
 `(git-gutter:added ((t (:foreground "yellow green" :inherit nil))))
 `(git-gutter:deleted ((t (:foreground "tomato" :inherit nil))))
 `(git-gutter:modified ((t (:foreground "orchid" :inherit nil))))
 `(trailing-whitespace ((t (:background "pink"))))
 `(error ((t (:foreground "tomato" :underline "red"))))
 `(js2-external-variable ((t (:foreground "gray40"))))
 `(mode-line ((t (:inherit default :overline "gray60" :box (:line-width 2 :color "white")))))
 `(mode-line-inactive ((t (:inherit default :foreground "gray70" :overline "gray60" :box (:line-width 2 :color "white")))))
 `(cursor ((t (:background "red")))))

(provide-theme 'dp-light)
