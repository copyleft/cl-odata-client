(defpackage :msgraph.demo.mail
  (:use :cl :msgraph :odata/lang :easy-routes :cl-who :cl-arrows :access)
  (:export :start-app))

(in-package :msgraph.demo.mail)

(defvar *html*)
(defparameter +appuser+ "77d37ed0-173e-474e-a477-371f4bbdd1a2")

(defun @ (obj &rest keys)
    (apply #'accesses obj keys))

(defmacro with-html-page (&body body)
  `(who:with-html-output-to-string (*html*)
     (:html
      (:head
       (:title "Messages")
       (:link :rel "stylesheet" :href "https://unpkg.com/purecss@1.0.1/build/pure-min.css" :integrity "sha384-oAOxQR6DkCoMliIh8yFnu25d7Eq/PHS21PClpwjOTeU2jRSq11vu66rf90/cZr47" :crossorigin"anonymous"))
      (:body
       ,@body))))

(defun get-messages (user)
  (-> msgraph::+msgraph+
      (collection "users")
      (id user)
      (collection "messages")
      (fetch :collection)))

(defun get-message (user id)
  (-> msgraph::+msgraph+
      (collection "users")
      (id user)
      (collection "messages")
      (id id)
      (fetch)))

(defun create-message (user message)
  (-> msgraph::+msgraph+
      (collection "users")
      (id user)
      (collection "messages")
      (post message)))

(defun send-message (user id)
  (-> msgraph::+msgraph+
      (collection "users")
      (id user)
      (collection "messages")
      (id id)
      (fcall "send")
      (post)))

(defroute home ("/")
    ()
  (with-html-page
    (show-messages +appuser+)
    (:a :href (genurl 'create-message-page)
        (str "New message"))))

(defroute show-message ("/messages/:id")
    (&path (id 'string))
  (let ((message (get-message +appuser+ id)))
    (with-html-page
      (:form :class "pure-form pure-form-stacked"
             (:fieldset
              (:label "From") (:label
                               (who:str (@ message :from :email-address :name))
                               (:a :href (format nil "mailto:~a" (@ message :from :email-address :address))
                                   (who:fmt "&lt;~a&gt;" (@ message :from :email-address :address))))
              (:label "To")
              (:label (loop for recipient in (@ message :to-recipients)
                         do
                           (who:htm
                            (who:str (@ recipient :email-address :name))
                            (:a :href (format nil "mailto:~a" (@ recipient :email-address :address))
                                (who:fmt "&lt;~a&gt;" (@ recipient :email-address :address))))))
              (:label "Subject")
              (:label (str (@ message :subject)))
              (:label "Body")
              (write-string (@ message :body :content) *html*)))
      (when (@ message :is-draft)
        (who:htm
         (:p
          (who:str "This message is a DRAFT")
          (:form :action (genurl 'send-message-action :id id)
                 :method :post
                (:input :type "submit" :value "Send")))))
      
      )))

(defroute send-message-action ("/messages/:id/send" :method :post)
    ()
  (send-message +appuser+ id)
  (redirect (genurl 'home)))

(defroute create-message-page ("/messages/new")
    ()
  (with-html-page
    (:form :class "pure-form pure-form-aligned"
           :method "POST"
           (:legend "Create message")
           (:fieldset
            (:div :class "pure-control-group"
                  (:label "From") (:input :name "from"))
            (:div :class "pure-control-group"
                  (:label "To") (:input :name "to"))
            (:div :class "pure-control-group"
                  (:label "Subject") (:input :name "subject"))
            (:div :class "pure-control-group"
                  (:label "Body") (:textarea :name "body"))
            (:input :type "submit" :value "Create")))))

(defun parse-recipient (str)
  (multiple-value-bind (address at name)
      (darts.lib.email-address:parse-rfc5322-mailbox str)
    `((:email-address . ((:address . ,(format nil "~a@~a" address at))
                         (:name . ,(or name address)))))))

(defun parse-recipients (str)
  (loop for rstr in (split-sequence:split-sequence #\, str)
     collect (parse-recipient rstr)))

(defroute save-message ("/messages/new" :method :post)
    (&post from to subject body)
  (create-message +appuser+
                  `((:subject . ,subject)
                    (:body . ((:content-type . "text")
                              (:content . ,body)))
                    (:from . ,(first (parse-recipients from)))
                    (:to-recipients . ,(parse-recipients to))))
  (redirect (genurl 'home)))

(defun show-messages (user)
  (who:with-html-output (*html*)
    (:ul
     (loop
        for message in (get-messages user)
        do
          (who:htm (:li (:a :href (genurl 'show-message :id (@ message :id))
                            (who:str (@ message :subject)))
                        (when (@ message :is-draft)
                          (who:str "(DRAFT)"))))))))

(defun start-app ()
  (hunchentoot:start (make-instance 'easy-routes-acceptor :port 9090)))
