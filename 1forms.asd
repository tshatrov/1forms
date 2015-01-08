;;;; 1forms.asd

(asdf:defsystem #:1forms
  :description "Describe 1forms here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:cl-markup
               #:cl-emb
               #:cl-ppcre)
  :serial t
  :components ((:file "package")
               (:file "1forms")))

