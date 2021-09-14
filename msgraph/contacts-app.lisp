(defpackage :msgraph.demo.contacts
  (:use :cl :msgraph :odata/lang :easy-routes :cl-who :arrows :access)
  (:export :start-app :stop-app))

(in-package :msgraph.demo.contacts)

(defvar *html*)
(defparameter +appuser+ "77d37ed0-173e-474e-a477-371f4bbdd1a2")

(defmacro with-html-page (&body body)
  `(who:with-html-output-to-string (*html*)
     (:html
      (:head
       (:title "Contacts")
       (:link :rel "stylesheet" :href "https://unpkg.com/purecss@1.0.1/build/pure-min.css" :integrity "sha384-oAOxQR6DkCoMliIh8yFnu25d7Eq/PHS21PClpwjOTeU2jRSq11vu66rf90/cZr47" :crossorigin"anonymous"))
      (:body
       ,@body))))

(defun get-contacts (user)
  (-> msgraph::+msgraph+
      (collection "users")
      (id user)
      (collection "contacts")
      (fetch :collection)))

(defun get-contact (user id)
  (-> msgraph::+msgraph+
      (collection "users")
      (id user)
      (collection "contacts")
      (id id)
      (fetch)))

(defun create-contact (user contact)
  (-> msgraph::+msgraph+
      (collection "users")
      (id user)
      (collection "contacts")
      (post contact)))

(defroute home ("/" :acceptor-name msgraph-contacts)
    ()
  (with-html-page
    (show-contacts-list +appuser+)
    (:a :href (genurl 'create-contact-page :acceptor-name 'msgraph-contacts)
        (str "New contact"))))

(defroute show-contact ("/contacts/:id" :acceptor-name msgraph-contacts)
    (&path (id 'string))
  (access:with-dot ()
    (let ((contact (get-contact +appuser+ id)))
      (with-html-page
        (:form :class "pure-form pure-form-stacked"
               (:fieldset
                (:label "Name")
                (:label (str contact.given-name))
                (:label "Surname") (:label (str contact.surname))
                (:label "Email") (:label (str contact.email-addresses.first.address))
                (:label "Phone")
                (:label (str contact.business-phones.first))))

        ))))

(defroute create-contact-page ("/contacts/new" :acceptor-name msgraph-contacts)
    ()
  (with-html-page
    (:form :class "pure-form pure-form-aligned"
           :method "POST"
           (:legend "Create contact")
           (:fieldset
            (:div :class "pure-control-group"
                  (:label "Name") (:input :name "name"))
            (:div :class "pure-control-group"
                  (:label "Surname") (:input :name "surname"))
            (:div :class "pure-control-group"
                  (:label "Email") (:input :name "email"))
            (:div :class "pure-control-group"
                  (:label "Phone") (:input :name "phone")))
           (:input :type "submit" :value "Create"))))

(defroute save-contact ("/contacts/new" :method :post :acceptor-name msgraph-contacts)
    (&post name surname email phone)
  (create-contact +appuser+
                  `((:given-name . ,name)
                    (:surname . ,surname)
                    (:email-addresses
                     ((:address . ,email)
                      (:name . ,(concatenate 'string name " " surname))))
                    (:business-phones . (,phone))))
  (redirect (genurl 'home :acceptor-name 'msgraph-contacts)))

(defun show-contacts-list (user)
  (access:with-dot ()
    (who:with-html-output (*html*)
      (:ul
       (loop
         for contact in (get-contacts user)
         do
            (who:htm (:li (:a :href (genurl 'show-contact :id (access contact :id) :acceptor-name 'msgraph-contacts)
                              (who:str contact.given-name)
                              (who:str " ")
                              (who:str contact.surname)))))))))

(defparameter *acceptor* nil)

(defun start-app (&key (port 0))
  ;; When port is zero, the acceptor is bound to a random free port
  (setf *acceptor* (hunchentoot:start (make-instance 'easy-routes-acceptor
                                                     :port port
                                                     :name 'msgraph-contacts))))

(defun stop-app ()
  (hunchentoot:stop *acceptor*))
