(define-minor-mode dark-mode
  "Use light text on dark background."
  :global t
  :group 'faces
  (when (eq dark-mode
            (eq 'light (frame--current-backround-mode (selected-frame))))
    (set-face-attribute 'default nil
                        :foreground (face-attribute 'default :background)
                        :background (face-attribute 'default :foreground))
    (frame-set-background-mode (selected-frame))))
