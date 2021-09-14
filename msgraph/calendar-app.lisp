(defpackage :msgraph.demo.calendar
  (:use :cl :msgraph :odata/lang :easy-routes :cl-who :arrows :access)
  (:export :start-app))

(in-package :msgraph.demo.calendar)

(defvar *html*)
(defparameter +appuser+ "77d37ed0-173e-474e-a477-371f4bbdd1a2")

(defmacro with-html-page (&body body)
  `(who:with-html-output-to-string (*html*)
     (:html
      (:head
       (:title "Calendar")
       (:link :rel "stylesheet" :href "https://unpkg.com/purecss@1.0.1/build/pure-min.css" :integrity "sha384-oAOxQR6DkCoMliIh8yFnu25d7Eq/PHS21PClpwjOTeU2jRSq11vu66rf90/cZr47" :crossorigin"anonymous"))
      (:body
       ,@body))))

(defun get-calendars (user)
  (-> msgraph::+msgraph+
      (collection "users")
      (id user)
      (collection "calendars")
      (fetch :collection)))

(defun get-calendar (user id)
  (-> msgraph::+msgraph+
      (collection "users")
      (id user)
      (collection "calendars")
      (id id)))

(defun get-calendar-named (user name)
  (-> msgraph::+msgraph+
      (collection "users")
      (id user)
      (collection "calendars")
      ($filter `(:= :name ,name))
      (fetch :collection)
      (first)))

(defun create-calendar (user calendar)
  (-> msgraph::+msgraph+
      (collection "users")
      (id user)
      (collection "calendars")
      (post calendar)))

(defun add-event (user calendar event)
  (-> msgraph::+msgraph+
      (collection "users")
      (id user)
      (collection "calendars")
      (id (access calendar :id))
      (collection "events")
      (post event)))

(defun get-events (user calendar)
  (-> msgraph::+msgraph+
      (collection "users")
      (id user)
      (collection "calendars")
      (id (access calendar :id))
      (collection "events")
      (fetch :collection)))

(defun make-calendar (&key name)
  `((:name . ,name)))

(defun make-event (&key subject content start end location attendees)
  `((:subject . ,subject)
    (:body . ((:content-type . "HTML")
              (:content . ,content)))
    (:start . ((:date-time . ,start)
               (:time-zone . ,"Pacific Standard Time")))
    (:end . ((:date-time . ,end)
             (:time-zone . "Pacific Standard Time")))
    (:location . ((:display-name . ,location)))
    (:attendees ,(loop for attendee in attendees
                       collect `(:email-address . ((:name . ,(first attendee))
                                                   (:address . ,(second attendee))))))))

(defun add-event-example ()
  (let ((test-calendar (get-calendar-named +appuser+ "test")))
    (add-event +appuser+ test-calendar
               `((:subject . "Let's go for lunch")
                 (:body . ((:content-type . "HTML")
                           (:content . "Does late morning work for you?")))
                 (:start . ((:date-time . "2017-04-15T12:00:00")
                            (:time-zone . "Pacific Standard Time")))
                 (:end . ((:date-time . "2017-04-15T14:00:00")
                          (:time-zone . "Pacific Standard Time")))
                 (:location . ((:display-name . "Harry's bar")))
                 (:attendees ((:email-address . ((:address . "samanthab@contoso.onmicrosoft.com")
                                                 (:name . "Samantha Booth")))
                              (:type . "required")))))))

;; (get-events +appuser+ (get-calendar-named +appuser+ "test"))

;; (add-event +appuser+ (get-calendar-named +appuser+ "test")
;;            (make-event :subject "Test event"
;;                        :content "This is a test event"
;;                        :start "2018-04-15T12:00:00"
;;                        :end "2018-05-15T12:00:00"
;;                        :location "Mariano's house"
;;                        :attendees '(("Asgeir" "asgeir@copyleft.no")
;;                                     ("Mariano" "mariano@copyleft.no"))))

(defroute home ("/" :acceptor-name msgraph-calendar)
    ()
  (with-html-page
    (show-calendars-list +appuser+)
    (:a :href (genurl 'new-calendar-page :acceptor-name 'msgraph-calendar)
        (str "New calendar"))))

(defroute show-calendar ("/calendar/:id" :acceptor-name msgraph-calendar)
    ()
  (let ((calendar (get-calendar 
  (with-html-page
    (:h1 (who:fmt "Calendar ~a" id))))

(defroute new-calendar-page ("/new" :acceptor-name msgraph-calendar)
    ()
  (with-html-page
    (:form :class "pure-form pure-form-aligned"
           :method "POST"
           (:legend "Create calendar")
           (:fieldset
            (:div :class "pure-control-group"
                  (:label "Name") (:input :name "name")))            
           (:input :type "submit" :value "Create"))))

(defroute create-calendar-page ("/new" :method :post :acceptor-name msgraph-calendar)
    (&post name)
  (create-calendar +appuser+
                   `((name . ,name)))
  (redirect (genurl 'home :acceptor-name 'msgraph-calendar)))

(defun show-calendars-list (user)
  (access:with-dot ()
    (who:with-html-output (*html*)
      (:ul
       (loop
         for calendar in (get-calendars user)
         do
            (who:htm (:li (:a :href (genurl 'show-calendar :id (access calendar :id) :acceptor-name 'msgraph-calendar)
                              (who:str calendar.name)))))))))

(defparameter *acceptor* nil)

(defun start-app (&key (port 0))
  ;; When port is zero, the acceptor is bound to a random free port
  (setf *acceptor* (hunchentoot:start (make-instance 'easy-routes-acceptor
                                                     :port port
                                                     :name 'msgraph-calendar))))

(defun stop-app ()
  (hunchentoot:stop *acceptor*))
