;;;; odata.lisp

(in-package #:odata)

(push '("application" . "json") drakma:*text-content-types*)
(setf json:*lisp-identifier-name-to-json*
      'lisp-to-camel-case)
(setf json:*json-identifier-name-to-lisp*
      'camel-case-to-lisp)

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

(defmacro def-packages (metadata)
  (loop for schema in (odata/metamodel::schemas (odata/metamodel::data-services metadata))
     appending
       `(defpackage ,(intern (odata/metamodel::namespace schema))
          (:export ,@(loop for elem in (odata/metamodel::elements schema)
                        collect (intern (camel-case-to-lisp (odata/metamodel::name elem)) :keyword))))))
  
(defun %def-entities (metadata)
  (loop for schema in (odata/metamodel::schemas (odata/metamodel::data-services metadata))
     appending
       (loop for entity in (odata/metamodel::entity-types schema)
           collect (generate-odata-entity entity)
           collect `(defmethod entity-name ((entity-type (eql ',(entity-class-name entity))))
                      ,(odata/metamodel::name entity))
           collect (generate-odata-entity-serializer entity)
           collect (generate-odata-entity-unserializer entity))))

(defun entity-class-name (node)
  (intern (camel-case-to-lisp
           (odata/metamodel::name node))
          (intern (odata/metamodel::namespace node))))

(defun generate-odata-entity (node)
  `(defclass ,(entity-class-name node)
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

(defun generate-odata-entity-serializer (node)
  `(defmethod odata::serialize ((node ,(entity-class-name node)) stream)
     (let ((json:*json-output* stream))
       (json:with-object ()
         ,@(loop
              for property in (odata/metamodel::structural-properties node)
              collect `(json:as-object-member
                           (,(intern (camel-case-to-lisp (odata/metamodel::name property)) :keyword))
                         (serialize-value (slot-value node ',(intern (camel-case-to-lisp (odata/metamodel::name property))))
                                          ',(odata/metamodel::property-type property))))))))

(defun generate-odata-entity-unserializer (node)
  `(defmethod odata::unserialize (data (type (eql ',(entity-class-name node))))
     (let ((entity (make-instance ',(entity-class-name node))))
       ,@(loop
            for property in (odata/metamodel::structural-properties node)
            collect `(setf (slot-value entity ',(intern (camel-case-to-lisp (odata/metamodel::name property))))
                           (unserialize-value
                            (access:access data ,(intern (camel-case-to-lisp (odata/metamodel::name property)) :keyword))
                            ',(odata/metamodel::property-type property))))
       entity)))

(defmacro def-entities (metadata)
  `(progn ,@(%def-entities metadata)))

(defgeneric entity-name (class))

(defgeneric serialize (object stream))
(defgeneric unserialize (data type))
(defgeneric serialize-primitive-value (value type))

(defun serialize-value (value type)
  (if (null value)
      nil
      (case (first type)
        (:primitive (serialize-primitive-value value (intern (second type) :keyword)))
        (:collection (serialize-collection-value (second type)))
        (:nominal (serialize-nominal-value value type)))))

(defmethod serialize-primitive-value (value (type (eql :|Edm.String|)))
  (json:encode-json value))

(defmethod serialize-primitive-value (value (type (eql :|Edm.Boolean|)))
  (if value
      (write-string "true" json:*json-output*)
      (write-string "false" json:*json-output*)))

(defmethod serialize-primitive-value (value type)
  (json:encode-json value))

(defun serialize-collection-value (collection col-type)
  (json:with-array ()
    (loop for elem in collection
       do (serialize-value elem col-type))))

(defun serialize-nominal-value (value nominal-type)
  ;;(let ((type (find-type nominal-type)))
  (serialize value json:*json-output*))

(defun unserialize-value (value type)
  (if (null value)
      nil
      (case (first type)
        (:primitive (unserialize-primitive-value value (intern (second type) :keyword)))
        (:collection (unserialize-collection-value value (second type)))
        (:nominal (unserialize-nominal-value value value type)))))

(defun unserialize-primitive-value (value type)
  value)

(defun unserialize-collection-value (value col-type)
  (loop for elem in value
     collect (unserialize elem col-type)))

(defun unserialize-nominal-value (value nominal-type)
  (let ((type (find-type nominal-type)))
    (let ((object (make-instance (entity-class-name type))))
      (loop for property in (odata/metamodel::properties type)
         do
           (setf (slot-value object (intern (camel-case-to-lisp (odata/metamodel::name property))))
                 (unserialize (access:access value (intern (camel-case-to-lisp (odata/metamodel::name property)) :keyword))
                              (odata/metamodel::property-type property)))))))
