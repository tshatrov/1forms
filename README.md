# 1forms

1forms is a web form generator for Common Lisp. It is inspired by Django web forms implementation. It can be used with various Common Lisp web servers and web frameworks such as [caveman2](https://github.com/fukamachi/caveman/). 1forms uses CLOS and is extensible by design.

This project is in early stages and still under development.

## Example

Example of form definition:

```cl
(def-form register-form ()
  (:login string-field
          :validator 'validate-login
          :name "login"
          :label "Username"
          )
  (:password password-field
             :validator 'validate-password
             :name "pw"
             :label "Password")
  (:fullname string-field
              :validator (lambda (str)
                           (validate-length str 3 255 t))
              :name "fullname"
              :label "Full name")
  (:email string-field
          :validator 'validate-email
          :name "email"
          :label "Email"))

(defmethod validate :after ((form register-form))
  (let ((data (form-data form)))
    (create-user
     (getf data :login)
     (getf data :password)
     :fullname (getf data :fullname "")
     :email (getf data :email ""))))
```

Example of form being used:

```cl
  (let ((form (make-instance 'register-form)))
    (bind-form form 'post-params)
    (process-form form
        ;; if form doesn't validate, display it with errors
        (some-template-rendering-function "register.tmpl"
             `(:form ,(render-form form)))
      ;; if successful
      (redirect (url-for 'login))))
```
