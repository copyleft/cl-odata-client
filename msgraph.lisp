(defpackage msgraph
  (:use :cl :cl-arrows :odata/lang :access))

(in-package :msgraph)

(defparameter +msgraph-metadata+
  (odata/metamodel::parse-metamodel
   (probe-file (asdf:system-relative-pathname :odata "msgraph.xml"))))

(odata::def-enums #.+msgraph-metadata+)

(defclass microsoft.graph.entity ()
  ())

(defclass microsoft.graph.outlook-item ()
  ())

(odata::def-packages #.+msgraph-metadata+)
(odata::def-entities #.+msgraph-metadata+)

(defun authorize ()
  (drakma:http-request "https://login.microsoftonline.com/common/oauth2/v2.0/authorize"
                       :parameters `(("client_id" . ,+appid+)
                                     ("response_type" . "code")
                                     ("redirect_uri" . ,+redirect-uri+)
                                     ("response_mode" . "query")
                                     ("scope" . "user.read")
                                     ("state" . "12345"))))

(defun get-msgraph-api-token (&key tenant scope)
  (json:decode-json-from-string
   (drakma:http-request
    (format nil "https://login.microsoftonline.com/~a/oauth2/v2.0/token"
            (or tenant "common"))
    :method :post
    :parameters `(("client_id" . ,+appid+)
                  ("scope" . ,(or scope "https://graph.microsoft.com/.default"))
                  ("client_secret" . ,+client-secret+)
                  ("grant_type" . "client_credentials")))))

(defun api-request (url token &rest args &key additional-headers &allow-other-keys)
  (apply #'drakma:http-request
         (princ-to-string url)
         (list* :additional-headers
                (cons (cons "Authorization"
                                 (format nil "~a ~a"
                                         (access:access token :token-type)
                                         (access:access token :access-token)))
                           additional-headers)
                args)))

(defparameter *token* (get-api-token :tenant +tenantid+))
(api-request "https://graph.microsoft.com/v1.0/users" *token*)
(api-request (format nil "https://graph.microsoft.com/v1.0/users/~A" +user-mariano+) *token*)
(api-request (format nil "https://graph.microsoft.com/v1.0/users/~A/contacts" +user-mariano+) *token*)
(api-request (format nil "https://graph.microsoft.com/v1.0/users/~A/calendars" +user-mariano+) *token*)

(defparameter +user-mariano+ "a1989572-3730-48fe-9e7a-2c821acc4127")
(defparameter +user-mariano+ "77d37ed0-173e-474e-a477-371f4bbdd1a2")
(api-request (format nil "https://graph.microsoft.com/v1.0/users/~a/onenote/notebooks" +user-mariano+) *token*)
(api-request (format nil "https://graph.microsoft.com/v1.0/users/~a/messages" +user-mariano+) *token*)

(defparameter +msgraph+ "https://graph.microsoft.com/v1.0")

(setf odata::*access-token* (get-msgraph-api-token :tenant +tenantid+))

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
