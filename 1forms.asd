;;;; 1forms.asd

(asdf:defsystem #:1forms
  :description "Web forms generator"
  :author "Timofei Shatrov <timofei.shatrov@gmail.com>"
  :license "MIT"
  :depends-on (#:cl-markup
               #:cl-emb
               #:cl-ppcre)
  :serial t
  :components ((:file "package")
               (:file "1forms")))

