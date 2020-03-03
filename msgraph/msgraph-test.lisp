(defpackage :msgraph-test
  (:use :odata :odata/lang :msgraph))

(in-package :msgraph-test)

(api-request "https://graph.microsoft.com/v1.0/users" *token*)
(api-request (format nil "https://graph.microsoft.com/v1.0/users/~A" +user-mariano+) *token*)
(api-request (format nil "https://graph.microsoft.com/v1.0/users/~A/contacts" +user-mariano+) *token*)
(api-request (format nil "https://graph.microsoft.com/v1.0/users/~A/calendars" +user-mariano+) *token*)

(defparameter +user-mariano+ "77d37ed0-173e-474e-a477-371f4bbdd1a2")
(api-request (format nil "https://graph.microsoft.com/v1.0/users/~a/onenote/notebooks" +user-mariano+) *token*)
(api-request (format nil "https://graph.microsoft.com/v1.0/users/~a/messages" +user-mariano+) *token*)

(-> +msgraph+
    (path "users" +user-mariano+)
    (path "contacts")(fetch :collection))

(-> +msgraph+
    (path "users" +user-mariano+)
    (path "contacts")
    (post '((:given-name . "Pavel")
            (:surname . "Bansky")
            (:email-addresses
             ((:address . "pavelb@fabrikam.onmicrosoft.com")
              (:name . "Pavel Bansky")))
            (:businessPhones . ("+1 732 555 0102")))))
