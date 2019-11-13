(deftheme dp-dark)

(custom-theme-set-faces
 'dp-dark
 `(default ((t (:background "gray10" :foreground "white"))))
 `(font-lock-comment-face ((t (:foreground "chocolate" :slant italic))))
 `(git-gutter:added ((t (:foreground "yellow green" :inherit nil))))
 `(git-gutter:deleted ((t (:foreground "tomato" :inherit nil))))
 `(git-gutter:modified ((t (:foreground "orchid" :inherit nil))))
 `(trailing-whitespace ((t (:background "hot pink"))))
 `(error ((t (:foreground "tomato" :underline "red"))))
 `(js2-external-variable ((t (:foreground "gray60"))))
 `(cursor ((t (:background "red")))))

(provide-theme 'dp-dark)
