(asdf:defsystem #:msgraph.demo.calendar
  :description "Demo application that uses CL-ODATA with Microsoft Graph"
  :author "Mariano Montone <marianomontone@gmail.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (:msgraph :odata :access :easy-routes :hunchentoot :cl-who)
  :components ((:file "credentials")
               (:file "calendar-app")))
