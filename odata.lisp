;;;; odata.lisp

(in-package #:odata)

(defparameter +trip-pin+ "https://services.odata.org/V4/TripPinServiceRW")

(push '("application" . "json") drakma:*text-content-types*)

(drakma:http-request +trip-pin+)

(defparameter +trip-pin-modify+
  (multiple-value-bind (response status headers modify-url)
      (drakma:http-request +trip-pin+)
    modify-url))

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

(with-odata-base +trip-pin-modify+
  (odata-get* "#Person"))


(defun child-node (name node)
  (find-if (lambda (nd)
             (string= (dom:node-name nd) name))
           (dom:child-nodes node)))

(defun generate-odata-enum (node)
  `(defenum:defenum ,(intern (json:camel-case-to-lisp (dom:get-attribute node "Name")))
       ,(loop
           for child across (dom:child-nodes node)
           when (string= (dom:node-name child) "Member")
           collect (list (intern (json:camel-case-to-lisp
                                  (concatenate 'string (dom:get-attribute node "Name")
                                               "/"
                                               (dom:get-attribute child "Name"))))
                         (parse-integer (dom:get-attribute child "Value"))))))

(defun %def-enums (metadata)
  (let* ((edmx (dom:document-element metadata))
         (data-services (child-node "edmx:DataServices" edmx))
         (schema (child-node "Schema" data-services))
         (enums (remove-if-not (lambda (node) (string= (dom:node-name node) "EnumType"))
                               (dom:child-nodes schema))))
    (loop for enum across enums
       collect (generate-odata-enum enum))))

(defmacro def-enums (metadata)
  `(progn ,@(%def-enums metadata)))

(defun %def-entities (metadata prefix)
  (let* ((edmx (dom:document-element metadata))
         (data-services (child-node "edmx:DataServices" edmx))
         (schema (child-node "Schema" data-services))
         (entities (remove-if-not (lambda (node) (string= (dom:node-name node) "EntityType"))
                               (dom:child-nodes schema))))
    (loop for entity across entities
       collect (generate-odata-entity entity prefix)
         collect (generate-odata-entity-serializer entity prefix))))

(defun entity-class-name (node prefix)
  (intern (concatenate 'string prefix
                       (json:camel-case-to-lisp
                        (dom:get-attribute node "Name")))))

(defun generate-odata-entity (node &optional (prefix ""))
  `(defclass ,(entity-class-name node prefix)
       (,(intern (json:camel-case-to-lisp (dom:get-attribute node "BaseType"))))
       ,(loop
           for child across (dom:child-nodes node)
           when (string= (dom:node-name child) "Property")
           collect `(,(intern (json:camel-case-to-lisp
                             (dom:get-attribute child "Name")))
                    :initarg ,(intern (json:camel-case-to-lisp
                                      (dom:get-attribute child "Name")) :keyword)
                    :accessor ,(intern
                               (json:camel-case-to-lisp
                                (concatenate 'string
                                             (dom:get-attribute node "Name")
                                             "."
                                             (dom:get-attribute child "Name"))))
                      ,@(unless (equalp (dom:get-attribute child "Nullable") "false")
                          (list :initform nil))
                    
                    ))))

(defun generate-odata-entity-serializer (node prefix)
  `(defmethod odata::serialize ((node ,(entity-class-name node prefix)) stream)
     (let ((json:*json-output* stream))
       (json:with-object ()
         ,@(loop
              for child across (dom:child-nodes node)
              when (string= (dom:node-name child) "Property")
              collect `(json:encode-object-member
                        ,(intern (json:camel-case-to-lisp (dom:get-attribute child "Name")) :keyword)
                        (serialize-value (slot-value node ',(intern (json:camel-case-to-lisp (dom:get-attribute child "Name"))))
                                         ',(intern (dom:get-attribute child "Type") :keyword))))))))

(defmacro def-entities (metadata &optional (prefix ""))
  `(progn ,@(%def-entities metadata prefix)))

(defgeneric serialize (object stream))
(defgeneric unserialize (type string))

(defgeneric serialize-value (value type))
(defmethod serialize-value (value type)
  (if (null value)
      nil
      (error "Don't know how to serialize value: ~a (~a)" value type)))
(defmethod serialize-value (value (type (eql :|Edm.String|)))
  (unless (null value)
    (princ-to-string value)))
(defmethod serialize-value (value (type (eql :|Edm.Boolean|)))
  (if value t nil))
