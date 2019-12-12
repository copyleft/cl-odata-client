;;;; odata.asd

(asdf:defsystem #:odata
  :description "Describe odata here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :components ((:file "package")
               (:file "odata"))
  :depends-on (:cxml :drakma :cl-arrows :cl-json :access :defenum :cl-change-case))
