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
  ((namespace :initarg :namespace :accessor namespace)
   (name :initarg :name :accessor name)))

(defclass entity-type (element)
  ((base-type :initarg :base-type :accessor base-type :initform nil)
   (key :initarg :key :accessor key)
   (properties :initarg :properties :accessor properties)
   (abstract :initarg :abstract :accessor is-abstract :initform nil)))

(defclass property ()
  ((name :initarg :name :accessor name)
   (type :initarg :type :accessor property-type)
   (nullable :initarg :nullable :accessor is-nullable :initform t)
   (default-value :initarg :default-value :accessor default-value :initform nil)))

(defclass navigation-property (property)
  ((contains-target :initarg :contains-target
                   :accessor contains-target
                   :initform nil)))

(defclass structural-property (property)
  ())

(defclass navigation-property-binding (element)
  ((target :initarg :target :accessor target)
   (path :initarg :path :accessor path)))

(defclass annotation (element)
  ())

(defclass complex-type (element)
  ((properties :initarg :properties :accessor properties)))

(defclass action (element)
  ((parameters :initarg :parameters :accessor parameters)
   (return-type :initarg :return-type :accessor return-type)))

(defclass enum-type (element)
  ((members :initarg :members :accessor members)))

(defclass function* (element)
  ((parameters :initarg :parameters :accessor parameters)
   (return-type :initarg :return-type :accessor return-type)))

(defclass type* (element)
  ())

(defclass entity-container (element)
  ((elements :initarg :elements :accessor elements)))

(defclass entity-set (element)
  ((entity-type :initarg :entity-type :accessor entity-type)
   (elements :initarg :elements :accessor elements)))

(defclass annotations (element)
  ())

(defclass singleton (element)
  ())

(defclass function-import (element)
  ((function :initarg :function :accessor function*)
   (entity-set :initarg :entity-set :accessor entity-set)
   (include-in-service-document :initarg :include-in-service-document
                                :accessor include-in-service-document)))

(defclass action-import (element)
  ((action :initarg :action :accessor action)
   (entity-set :initarg :entity-set :accessor entity-set)))

(defun parse-metamodel (source)
  (let ((xml-document (if (dom:document-p source)
                          source
                          (cxml:parse source (cxml-dom:make-dom-builder)))))
    (let ((edmx (dom:document-element xml-document)))
      (make-instance 'edmx-document
                     :data-services (parse-data-services (aref (dom:child-nodes edmx) 0))))))

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
       collect (parse-element element (dom:get-attribute schema "Namespace"))))

(defun parse-element (node namespace)
  (let ((el (parse-element-type node (intern (dom:tag-name node) :keyword))))
    (setf (namespace el) namespace)
    el))

(defgeneric parse-element-type (node type))

(defmethod parse-element-type (node type)
  node)

(defmethod parse-element-type (node (type (eql :|EntityContainer|)))
  (make-instance 'entity-container
                 :name (dom:get-attribute node "Name")
                 :elements (parse-elements node)))

(defmethod parse-element-type (node (type (eql :|NavigationPropertyBinding|)))
  (make-instance 'navigation-property-binding
                 :target (dom:get-attribute node "Target")
                 :path (dom:get-attribute node "Path")))

(defmethod parse-element-type (node (type (eql :|Annotations|)))
  ;; TODO
  (make-instance 'annotations
                 :name (dom:get-attribute node "Name")))

(defmethod parse-element-type (node (type (eql :|Annotation|)))
  ;; TODO
  (make-instance 'annotation
                 :name (dom:get-attribute node "Name")))

(defmethod parse-element-type (node (type (eql :|Singleton|)))
  (make-instance 'singleton
                 :name (dom:get-attribute node "Name")))

(defmethod parse-element-type (node (type (eql :|FunctionImport|)))
  (make-instance 'function-import
                 :name (dom:get-attribute node "Name")
                 :function (dom:get-attribute node "Function")
                 :entity-set (dom:get-attribute node "EntitySet")
                 :include-in-service-document (dom:get-attribute node "IncludeInServiceDocument")))

(defmethod parse-element-type (node (type (eql :|ActionImport|)))
  (make-instance 'action-import
                 :name (dom:get-attribute node "Name")
                 :action (dom:get-attribute node "Action")
                 :entity-set (dom:get-attribute node "EntitySet")))

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
                    collect (parse-property child 'structural-property)
                    when (string= (dom:tag-name child) "NavigationProperty")
                    collect (parse-property child 'navigation-property))))

(defmethod parse-element-type (node (type (eql :|ComplexType|)))
  (make-instance 'entity-type
                 :name (dom:get-attribute node "Name")
                 :properties
                 (loop for child across (dom:child-nodes node)
                    when (string= (dom:tag-name child) "Property")        
                    collect (parse-property child 'structural-property)
                    when (string= (dom:tag-name child) "NavigationProperty")
                    collect (parse-property child 'navigation-property))))

(defmethod parse-element-type (node (type (eql :|EntitySet|)))
  (make-instance 'entity-set
                 :name (dom:get-attribute node "Name")
                 :entity-type (ppcre:register-groups-bind (ns type) ("(.*)\\.(.*)" (dom:get-attribute node "EntityType"))
                                (list :type type :namespace ns))
                 :elements (parse-elements node)))

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

(defmethod parse-property (node class)
  (make-instance class
                 :name (dom:get-attribute node "Name")
                 :type (parse-type (dom:get-attribute node "Type"))
                 :nullable (or (not (dom:has-attribute node "Nullable"))
                               (string= (dom:get-attribute node "Nullable") "true"))
                 :default-value (and (dom:has-attribute node "DefaultValue")
                                     (dom:get-attribute node "DefaultValue"))))

(defmethod parse-property (node (class (eql 'navigation-property)))
  (let ((property (call-next-method)))
    (setf (contains-target property)
          (and (dom:has-attribute node "ContainsTarget")
               (string= (dom:get-attribute node "ContainsTarget") "true")))
    property))

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

(defun enums (schema)
  (element-types schema 'enum-type))

(defun entity-types (schema)
  (element-types schema 'entity-type))

(defun navigation-properties (entity-type)
  (remove-if-not (lambda (p) (typep p 'navigation-property))
                 (properties entity-type)))

(defun structural-properties (entity-type)
  (remove-if-not (lambda (p) (typep p 'structural-property))
                 (properties entity-type)))

(defun find-element (schema name)
  (find-if (lambda (el) (string= (name el) name))
           (elements schema)))

(defun enum-value (enum value)
  (cdr (assoc value (members enum) :test 'string=)))

(defun entity-container (metadata)
  (let ((schemas (schemas (data-services metadata))))
    (loop
       for schema in schemas
       for ec = (find-if (lambda (el) (typep el 'entity-container))
                         (elements schema))
       when ec
       return ec)))

(defun entity-sets (entity-container)
  (remove-if-not (lambda (el) (typep el 'entity-set)) (elements entity-container)))
