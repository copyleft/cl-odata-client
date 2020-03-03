;;;; odata.asd

(asdf:defsystem #:odata
  :description "Common Lisp client library for OData"
  :author "Mariano Montone <marianomontone@gmail.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :components ((:file "package")
               (:file "metamodel")
               (:file "odata")
               (:file "lang"))
  :depends-on (:cxml :drakma :cl-arrows :cl-json
                     :access :defenum :cl-change-case
                     :anaphora :quri))
