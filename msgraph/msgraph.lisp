(in-package :msgraph)

(defvar *credentials*)

(defparameter +msgraph+ "https://graph.microsoft.com/v1.0")

;; Special fetch/post for Microsoft API

(defparameter *ms-token* nil)

(defparameter *msgraph-http-requests-retries* 2
  "How many times to retry an HTTP request to the MS graph api, when a request fails.")

(defun get-msgraph-api-token (&key tenant scope)
  (json:decode-json-from-string
   (drakma:http-request
    (format nil "https://login.microsoftonline.com/~a/oauth2/v2.0/token"
            (or tenant "common"))
    :method :post
    :parameters `(("client_id" . ,(getf *credentials* :appid))
                  ("scope" . ,(or scope "https://graph.microsoft.com/.default"))
                  ("client_secret" . ,(getf *credentials* :client-secret))
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

(defun get-msgraph-token ()
  (setf *ms-token* (get-msgraph-api-token :tenant (getf *credentials* :tenantid))))

;; OData wrappers for Microsoft API

(defun call-with-ms-token (func &key (retries (1- *msgraph-http-requests-retries*)))
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
  (call-with-ms-token
   (lambda (token)
     (odata-client::odata-get
      url
      :$filter $filter
      :$expand $expand
      :authorization (format nil "~a ~a"
                             (access:access token :token-type)
                             (access:access token :access-token))))))

(defun odata-post (url data &key (json-encode t))
  (call-with-ms-token
   (lambda (token)
     (odata-client::odata-post
      url
      data
      :authorization (format nil "~a ~a"
                             (access:access *ms-token* :token-type)
                             (access:access *ms-token* :access-token))
      :json-encode t))))

(defun fetch (url &optional type)
  (odata/lang::read-odata-response (odata-get url) type))

(defun post (url &optional data)
  (odata-post url data))
