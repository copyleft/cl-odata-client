(defpackage :odata/lang
  (:use :cl :odata :access)
  (:export :singleton
           :get*
           :property
           :collection
           :$filter
           :$expand
           :get-collection))

(in-package :odata/lang)

(defun singleton (url name)
  (quri:uri (format nil "~a~a" url (if (stringp name) name
                             (lisp-to-camel-case (string name))))))

(defun get* (url)
  (odata::odata-get url))

(defun property (url name)
  (quri:uri (format nil "~a/~a" url (if (stringp name) name
                              (lisp-to-camel-case (string name))))))

(defun collection (url name)
  (quri:uri (format nil "~a/~a" url (if (stringp name) name
                              (lisp-to-camel-case (string name))))))

(defun get-collection (url)
  (access (get* url) :value))

(defun parameter (url param value)
  (push (cons param value)
        (quri:uri-query-params url))
  url)

(defun $filter (url exp)
  (parameter url "$filter" (odata::compile-$filter exp)))

(defun $expand (url exp)
  (parameter url "$expand" (odata::compile-$expand exp)))

(defun top (url top)
  (check-type top integer)
  (parameter url "top" top))

(defun skip (url skip)
  (check-type skip integer)
  (parameter url "skip" skip))
