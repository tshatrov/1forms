;;;; 1forms.lisp

(in-package :1forms)

(defun local-path (filename)
  (asdf:system-relative-pathname :1forms filename))

(defvar *form-template-directory* (local-path "forms/"))

(defparameter *default-form-template* "form.tmpl")

(define-condition field-error (error)
  ((field :initarg :field :initform nil
          :reader field-error-field)
   (message :initarg :message
            :reader field-error-message))
  (:report (lambda (condition stream)
             (format stream "Error in field ~a: ~a"
                     (field-error-field condition)
                     (field-error-message condition)))))

(defclass field ()
  ((name :initarg :name :initform nil :reader field-name)
   (id :initarg :id :initform nil :reader field-id)
   (prefix :initarg :prefix :initform "field" :reader field-prefix)
   (label :initarg :label :initform nil :reader field-label)
   (str-value :initarg :value :initform nil :reader field-str)
   (validator :initarg :validator :initform nil :reader field-validator)
   ))

(defgeneric render-field (field &key &allow-other-keys)
  (:method (field &key) ""))

(defgeneric render-label (field)
  (:method ((field field))
    (if (field-label field)
      (markup (:label :for (get-field-id field) (field-label field)))
      "")))

(defgeneric get-field-id (field)
  (:method ((field field))
    (or (field-id field)
        (and (field-name field)
             (format nil "~a_~a" (field-prefix field) (field-name field))))))

(defgeneric validate-field (field val)
  (:method ((field field) val) val))

(defmethod validate-field :around ((field field) val)
  (if (field-validator field)
      (funcall (field-validator field) (call-next-method))
      (call-next-method)))

(defgeneric accept-value (field val)
  (:method ((field field) val)
    (if val
        (princ-to-string val)
        "")))

(defclass input-field (field)
  ((prefix :initform "input")
   (type :initarg :type :reader input-type)
   (disabled :initarg :disabled :initform nil :reader input-disabled)
   (extra :initarg :extra :initform nil :reader input-extra)
   ))

(defgeneric input-attrs (field)
  (:method-combination append)
  (:method append ((field input-field))
           (list* :id (get-field-id field)
                  :name (field-name field)
                  :type (input-type field)
                  :disabled (input-disabled field)
                  (input-extra field))))

(defun get-input-attrs (field)
  (loop for (attr value) on (input-attrs field) by #'cddr
       when value
       append (list attr value)))

(defun write-tag (tag)
  (with-output-to-string (s)
    (loop for str in (cl-markup::tag->string tag)
         do (write-string str s))))

(defmethod render-field (field &key value)
  (write-tag `(:input ,@(get-input-attrs field) ,@(when value `(:value ,value)))))

(defgeneric to-lisp (field)
  (:documentation "returns lisp value for field or raises field-error for invalid values"))

(defclass string-field (input-field)
  ((type :initform "text")))

(defclass password-field (string-field)
  ((type :initform "password")))

(defmethod to-lisp ((field string-field))
  (field-str field))

(defclass form ()
  ((fields :initarg :fields :initform nil :reader form-fields)
   (initials :initarg :init :initform nil :reader form-initials)
   (errors :initform nil :reader form-errors)
   (data :initform nil :accessor form-data)
   (boundp :initform nil :accessor form-boundp)
   ))

(defgeneric validate (form)
  (:documentation "validate form or raise field-error")
  (:method ((form form))
    nil))

(defun full-validate (form)
  "Returns nil when no errors."
  ;; validate fields
  (loop for (keyword field) on (form-fields form) by #'cddr
       for field-validator = (field-validator field)
       for (value error-message) = (handler-case (list (validate-field field (to-lisp field)))
                                     (field-error (e)
                                       (with-slots (message) e
                                         (list nil message))))
       if error-message
       append (list keyword error-message) into errors
       else
       append (list keyword value) into data
       finally
       (with-slots ((form-errors errors) (form-data data)) form
         (setf form-errors errors
               form-data data)))
  (when (form-errors form)
    (return-from full-validate (form-errors form)))
  ;;validate the whole form
  (handler-case
      (validate form)
    (field-error (e)
      (with-slots (field message) e
        (with-slots (errors) form
          (push message errors)
          (push field errors)))))
  (form-errors form))

(defmacro process-form (form on-error &body on-success)
  `(cond ((full-validate ,form) ,on-error)
         (t ,@on-success)))

(defmacro def-form (class-name superclasses
                    &body field-defs)
  `(progn
     (defclass ,class-name ,(or superclasses '(form))
       ())
     
     (defmethod initialize-instance :after ((form ,class-name) &key)
        ,@(loop
            for field-def in field-defs
            for (kw class . options) = field-def
            for initial = (getf options :initial :none)
            unless (eql initial :none)
              append (list kw initial) into initials
            append (list kw `(make-instance ',class ,@options)) into fields
            finally
              (return
                `((setf (slot-value form 'initials) (append (slot-value form 'initials)
                                                            (list ,@initials)))
                  (setf (slot-value form 'fields) (list ,@fields))))))))
              
(defun form-to-plist (form &aux all-fields (err-map (make-hash-table)))
  (loop for (field err) on (form-errors form) by #'cddr
       do (push (list :error err) (gethash field err-map)))
  (with-slots (initials boundp) form
    (list :fields
          (loop for (kw field) on (form-fields form) by #'cddr
               for fieldinfo  = (list :label (render-label field)
                                      :field 
                                      (render-field field
                                                    :value (if (form-boundp form)
                                                               (field-str field)
                                                               (let ((val (getf initials kw :none)))
                                                                 (unless (eql val :none)
                                                                   (accept-value field val)))))
                                      :errors (nreverse (gethash kw err-map)))
             append (list kw fieldinfo)
             do (push fieldinfo all-fields))
          :allfields (nreverse all-fields)
          :errors (nreverse (gethash nil err-map))
          )))

(defun bind-form (form param-getter)
  "param-getter should return alist ((\"param_name\" . \"param_value\"))"
  (let ((params (funcall param-getter)))
    (loop for (kw field) on (form-fields form) by #'cddr
         for name = (field-name field)
         for param = (and name (assoc name params :test #'equalp))
         when param
         do (setf (slot-value field 'str-value) (cdr param))
         )
    (setf (slot-value form 'boundp) t)))

(defun render-form (form &key (template *default-form-template*) env)
  (let* ((form-path (merge-pathnames template *form-template-directory*))
         (emb:*escape-type* :raw)
         (emb:*case-sensitivity* nil))
    (emb:execute-emb
     form-path
     :env (append (form-to-plist form) env))))

;; validator utils

(defun validate-length (val start &optional end allow-empty)
  (let ((len (length val)))
    (when (and start (< len start) (or (not allow-empty) (> len 0)))
      (error 'field-error :message (format nil "Must be at least ~a characters long" start)))
    (when (and end (> len end))
      (error 'field-error :message (format nil "Must be no longer than ~a characters" start)))
    )
  val)

(defun validate-regex (val regex &optional (fail-message "Incorrect format") free)
  (unless (ppcre:scan (if free regex (format nil "^~a$" regex)) val)
    (error 'field-error :message fail-message))
  val)
