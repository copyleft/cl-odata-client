(asdf:defsystem #:odata-client-tests
  :description "Common Lisp client library for OData"
  :author "Mariano Montone <marianomontone@gmail.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :components ((:file "odata-client-tests"))
  :depends-on (:stefil :odata-client))
