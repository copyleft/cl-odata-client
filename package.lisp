;;;; package.lisp

(defpackage #:odata
  (:use #:cl #:cl-arrows #:access)
  (:export :singleton
           :get*
           :property
           :collection))
