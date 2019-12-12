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

(defun parse-return-type (node)
  (dom:get-attribute node "Type"))

(defun parse-parameter (node)
  (cons (dom:get-attribute node "Name")
        (dom:get-attribute node "Type")))
