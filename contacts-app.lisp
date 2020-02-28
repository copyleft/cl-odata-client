(require :hunchentoot)
(require :easy-routes)
(require :cl-who)
(require :odata)

(load (asdf:system-relative-pathname :odata "msgraph.lisp"))

(defpackage :contacts-app
  (:use :cl :odata/lang :easy-routes :cl-who :msgraph :cl-arrows))

(in-package :contacts-app)

(defvar *html*)

(defmacro with-html-page (&body body)
  `(who:with-html-output-to-string (*html*)
     (:html
      (:head
       (:title "Contacts"))
      (:body
       ,@body))))

(defparameter +mariano+ "77d37ed0-173e-474e-a477-371f4bbdd1a2")

(setf odata::*access-token* (msgraph::get-msgraph-api-token :tenant msgraph::+tenantid+))

(defun get-contacts (user)
  (-> msgraph::+msgraph+
    (path "users" user)
    (path "contacts")
    (fetch :collection)))

(defun get-contact (id)
  (-> msgraph::+msgraph+
    (path "users" user)
    (path "contacts" id)
    (fetch)))

(defun create-contact (user contact)
  (-> msgraph::+msgraph+
    (path "users" user)
    (path "contacts")
    (post contact)))

(defroute home ("/")
    ()
  (with-html-page
    (show-contacts-list +mariano+)))

(defroute show-contact ("/contacts/:id")
    (&path (id 'integer))
  (who:with-html-output ()
    (get-contact id)))

(defun show-contacts-list (user)
  (who:with-html-output (*html*)
    (:ul
     (loop
        for contact in (get-contacts user)
        do
          (who:htm (:li (:a :href (genurl 'show-contact (access contact :id))
                            (who:str (access contact :given-name))
                            (who:str (access contact :surname)))))))))
