(defpackage msgraph
  (:use :cl))

(in-package :msgraph)

(defparameter +msgraph-metadata+
  (odata/metamodel::parse-metamodel
   (probe-file (asdf:system-relative-pathname :odata "msgraph.xml"))))

(odata::def-enums #.+msgraph-metadata+)

(defclass microsoft.graph.entity ()
  ())

(defclass microsoft.graph.outlook-item ()
  ())

(odata::def-entities #.+msgraph-metadata+ "ENTITY/")
