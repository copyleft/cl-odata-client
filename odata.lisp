;;;; odata.lisp

(in-package #:odata)

(push '("application" . "json") drakma:*text-content-types*)
(setf json:*lisp-identifier-name-to-json*
      'lisp-to-camel-case)
(setf json:*json-identifier-name-to-lisp*
      'camel-case-to-lisp)

(defvar *schemas* nil)

(defun register-schema (schema)
  (push schema *schemas*))

(defun find-type (type)
  (let* ((ns (getf type :namespace))
         (schema (or
                  (find-if (lambda (sch) (string= (odata/metamodel::namespace sch)
                                             ns))
                           *schemas*)
                  (error "Namespace not found: ~a" ns))))
    (or
     (find-if (lambda (el) (string= (odata/metamodel::name el)
                               (getf type :nominal)))
              (odata/metamodel::elements schema))
     (error "Type not found: ~a" type))))

(defvar *odata-base*)

(defun odata-get (url)
  (access (json:decode-json-from-string
           (drakma:http-request url))
          :value))

(defun odata-get* (uri &optional (base *odata-base*))
  (odata-get (puri:merge-uris uri base)))

(defun call-with-odata-base (base func)
  (let ((*odata-base* base))
    (funcall func)))

(defmacro with-odata-base (base &body body)
  `(call-with-odata-base ,base (lambda () ,@body)))

(defun odata-get-entities (url type)
  (let ((data (odata-get* url)))
    (loop for entity-data in data
         collect (unserialize entity-data type))))

(defun child-node (name node)
  (find-if (lambda (nd)
             (string= (dom:node-name nd) name))
           (dom:child-nodes node)))

(defun camel-case-to-lisp (string)
  (string-upcase (cl-change-case:param-case string)))

(defun lisp-to-camel-case (string)
  (cl-change-case:camel-case string))

(defun generate-odata-enum (node)
  `(defenum:defenum ,(intern (camel-case-to-lisp (odata/metamodel::name node)))
       ,(loop
           for (name . value) in (odata/metamodel::members node)
           collect (list (intern (concatenate 'string (camel-case-to-lisp (odata/metamodel::name node))
                                              "/"
                                              (string-upcase name)))
                         value))))

(defun %def-enums (metadata)
  (loop for schema in (odata/metamodel::schemas (odata/metamodel::data-services metadata))
     appending
       (loop for enum in (odata/metamodel::enums schema)
          collect (generate-odata-enum enum))))

(defmacro def-enums (metadata)
  `(progn ,@(%def-enums metadata)))

(defun %def-entities (metadata prefix)
  (loop for schema in (odata/metamodel::schemas (odata/metamodel::data-services metadata))
     appending
       (loop for entity in (odata/metamodel::entity-types schema)
          collect (generate-odata-entity entity prefix)
          collect `(defmethod entity-name ((entity-type (eql ',(entity-class-name entity prefix))))
                     ,(odata/metamodel::name entity))
       collect (generate-odata-entity-serializer entity prefix)
       collect (generate-odata-entity-unserializer entity prefix))))

(defun entity-class-name (node prefix)
  (intern (concatenate 'string prefix
                       (camel-case-to-lisp
                        (odata/metamodel::name node)))))

(defun generate-odata-entity (node &optional (prefix ""))
  `(defclass ,(entity-class-name node prefix)
       (,@(when (odata/metamodel::base-type node)
              (list (intern (camel-case-to-lisp (odata/metamodel::base-type node))))))
       ,(loop
           for property in (odata/metamodel::structural-properties node)
           collect `(,(intern (camel-case-to-lisp (odata/metamodel::name property)))
                      :initarg ,(intern (camel-case-to-lisp
                                         (odata/metamodel::name property)) :keyword)
                      :accessor ,(intern
                                  (camel-case-to-lisp
                                   (concatenate 'string
                                                (odata/metamodel::name node)
                                                "."
                                                (odata/metamodel::name property))))
                      ,@(unless (not (odata/metamodel::is-nullable property))
                          (list :initform nil))
                    
                      ))))

(defun generate-odata-entity-serializer (node prefix)
  `(defmethod odata::serialize ((node ,(entity-class-name node prefix)) stream)
     (let ((json:*json-output* stream))
       (json:with-object ()
         ,@(loop
              for property in (odata/metamodel::structural-properties node)
              collect `(json:encode-object-member
                        ,(intern (camel-case-to-lisp (odata/metamodel::name property)) :keyword)
                        (serialize-value (slot-value node ',(intern (camel-case-to-lisp (odata/metamodel::name property))))
                                         ',(odata/metamodel::property-type property))))))))

(defun generate-odata-entity-unserializer (node prefix)
  `(defmethod odata::unserialize (data (type (eql ',(entity-class-name node prefix))))
     (let ((entity (make-instance ',(entity-class-name node prefix))))
     ,@(loop
          for property in (odata/metamodel::structural-properties node)
          collect `(setf (slot-value entity ',(intern (camel-case-to-lisp (odata/metamodel::name property))))
                         (unserialize-value
                          (access:access data ,(intern (camel-case-to-lisp (odata/metamodel::name property)) :keyword))
                          ',(odata/metamodel::property-type property))))
     entity)))

(defmacro def-entities (metadata &optional (prefix ""))
  `(progn ,@(%def-entities metadata prefix)))

(defgeneric entity-name (class))

(defgeneric serialize (object stream))
(defgeneric unserialize (data type))
(defgeneric serialize-value (value type))
(defgeneric unserialize-value (value type))

(defmethod serialize-value (value type)
  (if (null value)
      nil
      (error "Don't know how to serialize value: ~a (~a)" value type)))

(defmethod serialize-value (value (type (eql '(:primitive "Edm.String"))))
  (unless (null value)
    (princ-to-string value)))

(defmethod serialize-value (value (type (eql '(:primitive "Edm.Boolean"))))
  (if value t nil))

(defmethod unserialize-value (value type)
  value)
