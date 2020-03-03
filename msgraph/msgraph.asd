(asdf:defsystem #:msgraph
  :description "Common Lisp client library for Microsoft Graph"
  :author "Mariano Montone <marianomontone@gmail.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (:odata :access)
  :components ((:file "package")
               (:file "msgraph-metadata")
               (:file "msgraph")))

(asdf:defsystem #:msgraph.contacts-app
  :description "Demo application that uses CL-ODATA with Microsoft Graph"
  :author "Mariano Montone <marianomontone@gmail.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (:msgraph :odata :access :easy-routes :hunchentoot :cl-who)
  :components ((:file "credentials")
               (:file "contacts-app")))
