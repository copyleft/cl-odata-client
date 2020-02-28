(require :hunchentoot)
(require :easy-routes)
(require :cl-who)
(require :odata)

(load (asdf:system-relative-pathname :odata "msgraph.lisp"))

(defpackage :contacts-app
  (:use :cl :odata/lang :easy-routes :cl-who :msgraph :cl-arrows :access))

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

(defun get-contacts (user)
  (-> msgraph::+msgraph+
    (path "users" user)
    (path "contacts")
    (fetch :collection)))

(defun get-contact (user id)
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
    (show-contacts-list +mariano+)
    (:a :href (genurl 'create-contact-page)
        (str "New contact"))))

(defroute show-contact ("/contacts/:id")
    (&path (id 'string))
  (let ((contact (get-contact +mariano+ id)))
    (with-html-page
      (:form
       (:label "Name") (:label (str (access contact :given-name))) (:br)
       (:label "Surname") (:label (str (access contact :surname))) (:br)
       (:label "Email") (:label (str (accesses contact :email-addresses 'first :address)))(:br)
       (:label "Phone") (:label (str (accesses contact :business-phones 'first)))
       
    ))))

(defroute create-contact-page ("/contacts/new")
    ()
  (with-html-page
    (:form :method "POST"
     (:label "Name") (:input :name "name") (:br)
     (:label "Surname") (:input :name "surname") (:br)
     (:label "Email") (:input :name "email")(:br)
     (:label "Phone") (:input :name "phone")(:br)
     (:input :type "submit" :value "Create"))))

(defroute save-contact ("/contacts/new" :method :post)
    (&post name surname email phone)
  (create-contact +mariano+
                  `((:given-name . ,name)
                    (:surname . ,surname)
                    (:email-addresses
                     ((:address . ,email)
                      (:name . ,(concatenate 'string name " " surname))))
                    (:businessPhones . (,phone))))
  (redirect (genurl 'home)))

(defun show-contacts-list (user)
  (who:with-html-output (*html*)
    (:ul
     (loop
        for contact in (get-contacts user)
        do
          (who:htm (:li (:a :href (genurl 'show-contact :id (access contact :id))
                            (who:str (access contact :given-name))
                            (who:str (access contact :surname)))))))))

(defun start-app ()
  (setf odata::*access-token* (msgraph::get-msgraph-api-token :tenant msgraph::+tenantid+))
  (hunchentoot:start (make-instance 'easy-routes-acceptor :port 9090)))
