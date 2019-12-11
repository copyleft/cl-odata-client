(defpackage msgraph
  (:use :cl))

(in-package :msgraph)

(defparameter +msgraph-metadata+
  (cxml:parse (asdf:system-relative-pathname :odata "msgraph.xml")
              (cxml-dom:make-dom-builder)))

(odata::def-enums #.+msgraph-metadata+)

(defclass microsoft.graph.entity ()
  ())

(odata::def-entities #.+msgraph-metadata+ "MSG/")
