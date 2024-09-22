(deftheme dop)

(custom-theme-set-faces
 'dop
 '(cursor ((((background light)) (:background "red"))
           (((background dark)) (:background "green"))))
 '(eglot-highlight-symbol-face ((t (:inherit highlight))))
 '(mode-line ((t (:inherit variable-pitch :background "grey75" :foreground "black" :box (:line-width (1 . 1) :style released-button) :height 1.0))))
 '(mode-line-inactive ((t (:inherit mode-line :background "grey75" :foreground "grey50")))))

(provide-theme 'dop)
