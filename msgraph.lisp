(defpackage msgraph
  (:use :cl))

(in-package :msgraph)

(defparameter +msgraph-metadata+
  (odata/metamodel::parse-metamodel
   (probe-file (asdf:system-relative-pathname :odata "msgraph.xml"))))

(odata::def-enums #.+msgraph-metadata+)

(defclass microsoft.graph.entity ()
  ())

(defclass microsoft.graph.outlook-item ()
  ())

(odata::def-entities #.+msgraph-metadata+)

(defun authorize ()
  (drakma:http-request "https://login.microsoftonline.com/common/oauth2/v2.0/authorize"
                       :parameters `(("client_id" . ,+appid+)
                                     ("response_type" . "code")
                                     ("redirect_uri" . ,+redirect-uri+)
                                     ("response_mode" . "query")
                                     ("scope" . "user.read")
                                     ("state" . "12345"))))

(defun get-api-token (&key tenant scope)
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
         url
         (list* :additional-headers
                (cons (cons "Authorization"
                                 (format nil "~a ~a"
                                         (access:access token :token-type)
                                         (access:access token :access-token)))
                           additional-headers)
                args)))

(defparameter *token* (get-api-token :tenant +tenantid+))
(api-request "https://graph.microsoft.com/v1.0/users" *token*)
