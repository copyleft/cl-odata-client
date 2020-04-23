(asdf:defsystem #:odata-client
  :description "Common Lisp client library for OData"
  :author "Mariano Montone <marianomontone@gmail.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :components ((:file "package")
               (:file "odata-client")
               (:file "lang"))
  :depends-on (:cxml :drakma :cl-arrows :cl-json
                     :access :cl-change-case
                     :anaphora :quri))
