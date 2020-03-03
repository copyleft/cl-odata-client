(in-package :msgraph)

(defparameter +msgraph-metadata+
  (odata/metamodel::parse-metamodel
   (probe-file (asdf:system-relative-pathname :msgraph "msgraph.xml"))))
