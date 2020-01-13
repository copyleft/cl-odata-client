(defpackage :odata/lang
  (:use :cl :odata :access)
  (:export :singleton
           :fetch
           :property
           :collection
           :$filter
           :$expand
           :id
           :$skip
           :$top))

(in-package :odata/lang)

(defun singleton (url name)
  (quri:uri (format nil "~a~a" url (if (stringp name) name
                             (lisp-to-camel-case (string name))))))

(defun fetch (url &optional type)
  (case type
    (:collection (access (fetch url) :value))
    (:value (access (fetch url) :value))
    (t (odata::odata-get url))))

(defun property (url name)
  (quri:uri (format nil "~a/~a" url (if (stringp name) name
                              (lisp-to-camel-case (string name))))))

(defun collection (url name)
  (quri:uri (format nil "~a/~a" url (if (stringp name) name
                                        (lisp-to-camel-case (string name))))))

(defun id (url id)
  (quri:uri (format nil "~a('~a')" url id)))

(defun parameter (url param value)
  (push (cons param value)
        (quri:uri-query-params url))
  url)

(defun $filter (url exp)
  (parameter url "$filter" (odata::compile-$filter exp)))

(defun $expand (url exp)
  (parameter url "$expand" (odata::compile-$expand exp)))

(defun $top (url top)
  (check-type top integer)
  (parameter url "$top" top))

(defun $skip (url skip)
  (check-type skip integer)
  (parameter url "$skip" skip))
