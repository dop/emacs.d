;; -*- lexical-binding: t; -*-

(require 'tempo)

(tempo-define-template
 "react-component"
 '("interface " (r "Component name: " name) "Props {}"
   n n
   "const " (s name) ": FC<" (s name) "Props> = () => {"
   n> "return null;"
   n> "};"))

(provide 'setup-tempo)
