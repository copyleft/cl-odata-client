(asdf:defsystem #:msgraph.demo.mail
  :description "Demo application that uses CL-ODATA with Microsoft Graph"
  :author "Mariano Montone <marianomontone@gmail.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (:msgraph :access :easy-routes :hunchentoot :cl-who :darts.lib.email-address)
  :components ((:file "credentials")
               (:file "mail-app")))
