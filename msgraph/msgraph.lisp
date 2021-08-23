(in-package :msgraph)

(defvar *credentials*
  "The credentials for authenticating agains msgraph.
A property list with :appid, :tenantid, :appname and :client-secret.")

(defparameter +msgraph+ "https://graph.microsoft.com/v1.0"
  "The msgraph api url.")

;; Special fetch/post for Microsoft API

(defparameter *ms-token* nil
  "The current obtained msgraph api token.")

(defparameter *msgraph-http-requests-retries* 2
  "How many times to retry an HTTP request to the MS graph api, when a request fails.")

(defun get-msgraph-api-token (&key tenant scope)
  "Fetch the MS graph api token using *CREDENTIALS*.
TENANT and SCOPE are optional. If not given, 'common' tenant and default scope are used."
  (json:decode-json-from-string
   (drakma:http-request
    (format nil "https://login.microsoftonline.com/~a/oauth2/v2.0/token"
            (or tenant (getf *credentials* :tenantid) "common"))
    :method :post
    :parameters `(("client_id" . ,(getf *credentials* :appid))
                  ("scope" . ,(or scope "https://graph.microsoft.com/.default"))
                  ("client_secret" . ,(getf *credentials* :client-secret))
                  ("grant_type" . "client_credentials")))))

(defun api-request (url token &rest args &key additional-headers &allow-other-keys)
  "Make a request to api URL using TOKEN."
  (apply #'drakma:http-request
         (princ-to-string url)
         (list* :additional-headers
                (cons (cons "Authorization"
                            (format nil "~a ~a"
                                    (access:access token :token-type)
                                    (access:access token :access-token)))
                      additional-headers)
                args)))

(defun get-msgraph-token ()
  "Get token for msgraph api using *CREDENTIALS*."
  (setf *ms-token* (get-msgraph-api-token :tenant (getf *credentials* :tenantid))))

;; OData wrappers for Microsoft API

(defun call-with-ms-token (func &key (retries (1- *msgraph-http-requests-retries*)))
  "Call function FUNC using a token obtained via GET-MSGRAPH-TOKEN."
  (when (null *ms-token*)
    (setf *ms-token* (get-msgraph-token)))
  (handler-case
      (funcall func *ms-token*)
    (odata-client::odata-request-error (e)
      (if (equalp (odata-client::http-status e) 401)
          ;; Invalid token? Fetch another one
          (if (zerop retries)
              (error e)
              (progn
                (setf *ms-token* (get-msgraph-token))
                (call-with-ms-token func :retries (1- retries))))
          (error e)))))

(defun odata-get (url &rest args &key $filter $expand)
  "Call ODATA-CLIENT:ODATA-GET with MSGraph token."
  (call-with-ms-token
   (lambda (token)
     (odata-client:odata-get
      url
      :$filter $filter
      :$expand $expand
      :authorization (format nil "~a ~a"
                             (access:access token :token-type)
                             (access:access token :access-token))))))

(defun odata-post (url data &key (json-encode t))
  "Call ODATA-CLIENT:ODATA-POST with MSGraph token."
  (call-with-ms-token
   (lambda (token)
     (odata-client:odata-post
      url
      data
      :authorization (format nil "~a ~a"
                             (access:access token :token-type)
                             (access:access token :access-token))
      :json-encode json-encode))))

;; Arrows syntax interface

(defun fetch (url &optional type)
  (odata/lang::read-odata-response (odata-get url) type))

(defun post (url &optional data)
  (odata-post url data))
