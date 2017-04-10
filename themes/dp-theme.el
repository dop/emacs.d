(deftheme dp "My Color Theme")

(defun apply-my-theme (&optional reversed disable)
  (cl-flet ((color (value &optional reversed-value)
                   (if disable nil (if reversed (or reversed-value value) value))))
    (let ((bg              (color "white" "#202420"))
          (fg              (color "#202420" "white"))
          (white           (color "white" "black"))
          (black           (color "black" "white"))
          (gray            (color "gray" "gray40"))
          (khaki           (color "gray73" "gray30"))
          (red             (color "red"))
          (pink            (color "hot pink" "pink"))
          (blue            (color "deep sky blue"))
          (green           (color "yellow green"))
          (red             (color "tomato"))
          (lightpink       (color "misty rose" "pink"))
          (very-light-gray (color "#f5f5f5" "#202420")))
      (custom-theme-set-faces
       'dp
       `(default ((t (:foreground ,fg :background ,bg))))
       `(cursor ((t (:foreground ,black :background "red"))))
       `(fringe ((t (:background ,bg))))
       `(vertical-border ((t (:foreground ,gray))))
       `(parenthesis ((t (:foreground ,(if reversed
                                           (color-lighten-name bg 30)
                                         (color-darken-name bg 30))))))
       `(trailing-whitespace ((t (:background ,pink))))

       `(font-lock-comment-face ((t (:foreground ,khaki :slant normal))))
       `(font-lock-constant-face ((t (:foreground ,pink))))

       `(mmm-default-submode-face ((t (:background ,very-light-gray))))

       `(git-gutter:modified ((t (:foreground ,blue))))
       `(git-gutter:added ((t (:foreground ,green))))
       `(git-gutter:deleted ((t (:foreground ,red))))

       `(js2-highlight-vars-face ((t (:inherit highlight))))
       `(js2-highlight-vars-second-face ((t (:inherit highlight))))

       `(mode-line
         ((t (:overline ,gray :underline nil :foreground ,black :background ,very-light-gray
                        :box (:line-width 3 :color ,very-light-gray :style unspecified)))))
       `(mode-line-inactive
         ((t (:overline ,gray :underline nil :foreground ,gray :background ,very-light-gray
                        :box (:line-width 3 :color ,very-light-gray :style unspecified)))))
       `(hl-tags-face ((t (:inherit highlight))))
       ))))

(apply-my-theme)

(provide-theme 'dp)
