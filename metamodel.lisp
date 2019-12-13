;; http://docs.oasis-open.org/odata/odata-csdl-xml/v4.01/odata-csdl-xml-v4.01.html#_Toc26368783

(defpackage :odata/metamodel
  (:use :cl :odata :anaphora))

(in-package :odata/metamodel)

(defclass edmx-document ()
  ((data-services :initarg :data-services :accessor data-services)))

(defclass data-services ()
  ((schemas :initarg :schemas :accessor schemas)))

(defclass schema ()
  ((namespace :initarg :namespace :accessor namespace)
   (alias :initarg :alias :accessor alias)
   (elements :initarg :elements :accessor elements)))

(defclass element ()
  ((namespace :initarg :namespace :accessor namespace)))

(defclass entity-type (element)
  ((name :initarg :name :accessor name)
   (base-type :initarg :base-type :accessor base-type :initform nil)
   (key :initarg :key :accessor key)
   (properties :initarg :properties :accessor properties)
   (abstract :initarg :abstract :accessor is-abstract :initform nil)))

(defclass property ()
  ((name :initarg :name :accessor name)
   (type :initarg :type :accessor property-type)
   (nullable :initarg :nullable :accessor is-nullable :initform t)
   (default-value :initarg :default-value :accessor default-value :initform nil)))

(defclass navigation-property (property)
  ())

(defclass annotation (element)
  ())

(defclass complex-type (element)
  ((name :initarg :name :accessor name)
   (properties :initarg :properties :accessor properties)))

(defclass action (element)
  ((name :initarg :name :accessor name)
   (parameters :initarg :parameters :accessor parameters)
   (return-type :initarg :return-type :accessor return-type)))

(defclass enum-type (element)
  ((name :initarg :name :accessor name)
   (members :initarg :members :accessor members)))

(defclass function* (element)
  ((name :initarg :name :accessor name)
   (parameters :initarg :parameters :accessor parameters)
   (return-type :initarg :return-type :accessor return-type)))

(defclass type* (element)
  ())

(defun parse-metamodel (xml-document)
  (let ((edmx (dom:document-element xml-document)))
    (make-instance 'edmx-document
                   :data-services (parse-data-services (aref (dom:child-nodes edmx) 0)))))

(defun parse-data-services (node)
  (make-instance 'data-services :schemas
                 (loop for schema across (dom:child-nodes node)
                 collect (parse-schema schema))))

(defun parse-schema (node)
  (make-instance 'schema
                 :namespace (dom:get-attribute node "Namespace")
                 :elements (parse-elements node)))

(defun parse-elements (schema)
  (loop for element across (dom:child-nodes schema)
       collect (parse-element element)))

(defun parse-element (node)
  (parse-element-type node (intern (dom:tag-name node) :keyword)))

(defgeneric parse-element-type (node type))

(defmethod parse-element-type (node type)
  node)

(defmethod parse-element-type (node (type (eql :|Function|)))
  (make-instance 'function*
                 :name (dom:get-attribute node "Name")
                 :parameters (loop for child across (dom:child-nodes node)
                                when (string= (dom:tag-name child) "Parameter")
                                collect (parse-parameter child))
                 :return-type (awhen (find-if (lambda (child) (string= (dom:tag-name child) "ReturnType"))
                                              (dom:child-nodes node))
                                (parse-return-type it))))

(defmethod parse-element-type (node (type (eql :|Action|)))
  (make-instance 'action
                 :name (dom:get-attribute node "Name")
                 :parameters (loop for child across (dom:child-nodes node)
                                when (string= (dom:tag-name child) "Parameter")
                                collect (parse-parameter child))
                 :return-type (awhen (find-if (lambda (child) (string= (dom:tag-name child) "ReturnType"))
                                              (dom:child-nodes node))
                                (parse-return-type it))))

(defmethod parse-element-type (node (type (eql :|EnumType|)))
  (make-instance 'enum-type
                 :name (dom:get-attribute node "Name")
                 :members (loop for child across (dom:child-nodes node)
                                when (string= (dom:tag-name child) "Member")
                             collect (cons (dom:get-attribute child "Name")
                                           (parse-integer (dom:get-attribute child "Value"))))))

(defmethod parse-element-type (node (type (eql :|EntityType|)))
  (make-instance 'entity-type
                 :name (dom:get-attribute node "Name")
                 :base-type (and (dom:has-attribute node "BaseType")
                                 (dom:get-attribute node "BaseType"))    
                 :key (let ((key (child-node "Key" node)))
                        (when key
                          (dom:get-attribute (child-node "PropertyRef" key) "Name")))
                 :properties
                 (loop for child across (dom:child-nodes node)
                    when (string= (dom:tag-name child) "Property")        
                    collect (parse-property child 'property)
                    when (string= (dom:tag-name child) "NavigationProperty")
                    collect (parse-property child 'navigation-property))))

(defun emptyp (sequence)
  (zerop (length sequence)))

(defun child-node (name node)
  (find-if (lambda (nd)
             (string= (dom:node-name nd) name))
           (dom:child-nodes node)))

(defun camel-case-to-lisp (string)
  (string-upcase (cl-change-case:param-case string)))

(defun lisp-to-camel-case (string)
  (cl-change-case:camel-case string))

(defun parse-property (node class)
  (make-instance class
                 :name (dom:get-attribute node "Name")
                 :type (parse-type (dom:get-attribute node "Type"))
                 :nullable (or (not (dom:has-attribute node "Nullable"))
                               (string= (dom:get-attribute node "Nullable") "true"))
                 :default-value (and (dom:has-attribute node "DefaultValue")
                                     (dom:get-attribute node "DefaultValue"))))

(defun parse-return-type (node)
  (dom:get-attribute node "Type"))

(defun parse-parameter (node)
  (cons (dom:get-attribute node "Name")
        (dom:get-attribute node "Type")))

(defun parse-type (node)
  (cond
    ((eql (search "Edm." node) 0)
     ;; Primitive type
     (list :primitive node))
    ((eql (search "Collection" node) 0)
     ;; A collection
      (ppcre:register-groups-bind (col-type)
          ("Collection\\((.*)\\)" node)
        (list :collection (parse-type col-type))))
    (t
     ;; Nominal type
     (ppcre:register-groups-bind (ns type) ("(.*)\\.(.*)" node)
       (list :nominal type :namespace ns)))))

(defun element-types (schema type)
  "Return the elements of SCHEMA of type TYPE.
Examples:
(element-types *schema* 'entity-type)
(element-types *schema* 'action)"
  (remove-if-not (lambda (e) (typep e type))
                 (elements schema)))
