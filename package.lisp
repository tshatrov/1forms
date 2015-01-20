;;;; package.lisp

(defpackage 1forms
  (:use :cl :cl-markup)
  (:export
   :field-error
   :field-error-field
   :field-error-message
   :render-field
   :field :field-name :field-id :field-prefix :field-label :field-str :field-validator
   :input-field :input-type :input-disabled :input-extra
   :get-field-id
   :input-attrs
   :to-lisp
   :string-field
   :form :form-data :form-errors :form-initials :form-fields
   :validate :full-validate
   :render-label
   :validate-field
   :form-boundp
   :form-to-plist
   :accept-value
   :render-form
   :def-form
   :validate-length
   :validate-regex
   :password-field
   :process-form
   :bind-form
   :*form-template-directory*
   :*default-form-template*
   :form-vars))
