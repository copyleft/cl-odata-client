(defpackage :msgraph.demo.contacts
  (:use :cl :msgraph :odata/lang :easy-routes :cl-who :cl-arrows :access)
  (:export :start-app))

(in-package :msgraph.demo.contacts)

(defvar *html*)
(defparameter +appuser+ "77d37ed0-173e-474e-a477-371f4bbdd1a2")

(defun @ (obj &rest keys)
    (apply #'accesses obj keys))

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

(defroute home ("/")
    ()
  (with-html-page
    (show-contacts-list +appuser+)
    (:a :href (genurl 'create-contact-page)
        (str "New contact"))))

(defroute show-contact ("/contacts/:id")
    (&path (id 'string))
  (let ((contact (get-contact +appuser+ id)))
    (with-html-page
      (:form :class "pure-form pure-form-stacked"
             (:fieldset
              (:label "Name")
              (:label (str (@ contact :given-name)))
              (:label "Surname") (:label (str (@ contact :surname)))
              (:label "Email") (:label (str (@ contact :email-addresses 'first :address)))
              (:label "Phone")
              (:label (str (@ contact :business-phones 'first)))))

       )))

(defroute create-contact-page ("/contacts/new")
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

(defroute save-contact ("/contacts/new" :method :post)
    (&post name surname email phone)
  (create-contact +appuser+
                  `((:given-name . ,name)
                    (:surname . ,surname)
                    (:email-addresses
                     ((:address . ,email)
                      (:name . ,(concatenate 'string name " " surname))))
                    (:business-phones . (,phone))))
  (redirect (genurl 'home)))

(defun show-contacts-list (user)
  (who:with-html-output (*html*)
    (:ul
     (loop
        for contact in (get-contacts user)
        do
          (who:htm (:li (:a :href (genurl 'show-contact :id (@ contact :id))
                            (who:str (@ contact :given-name))
                            (who:str " ")
                            (who:str (@ contact :surname)))))))))

(defun start-app ()
  (hunchentoot:start (make-instance 'easy-routes-acceptor :port 9090)))
