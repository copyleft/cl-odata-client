(in-package #:odata-client)

(defvar *odata-base*)
(defvar *access-token*)

(push '("application" . "json") drakma:*text-content-types*)
(setf json:*lisp-identifier-name-to-json*
      'lisp-to-camel-case)
(setf json:*json-identifier-name-to-lisp*
      'camel-case-to-lisp)

(define-condition odata-request-error (simple-error)
  ((http-status :initarg :http-status :accessor http-status)))

(defun odata-get (url &key $filter $expand authorization)
  (let ((url* (if (stringp url)
                  (quri:uri url)
                  url)))
    (when $filter
      (push (cons "$filter" $filter) (quri:uri-query-params url*)))
    (when $expand
      (push (cons "$expand" $expand) (quri:uri-query-params url*)))
    (multiple-value-bind (response status)
        (drakma:http-request (quri:render-uri url*)
                             :preserve-uri t
                             :additional-headers (when authorization
                                                   (list (cons "Authorization"
                                                               authorization))))
      (let ((json (json:decode-json-from-string response)))
        (when (>= status 400)
          (error 'odata-request-error
               :http-status status
               :format-control "OData request error (~a): ~a"
               :format-arguments (list status (accesses json :error :message))))
        json))))

(defun odata-get* (uri &rest args &key $filter $expand)
  (apply #'odata-get (quri:merge-uris uri *odata-base*)
         args))

(defun odata-post (uri data &key (json-encode t) authorization)
  (multiple-value-bind (response status)
      (drakma:http-request (quri:render-uri uri)
                           :preserve-uri t
                           :content (if json-encode
                                        (json:encode-json-to-string data)
                                        data)
                           :additional-headers (when authorization
                                                 (list (cons "Authorization"
                                                             authorization)))
                           :content-type "application/json;odata.metadata=minimal"
                           :accept "application/json"
                           :method :post)
    (if (and (null response) (< status 400)) ;; no content
        (return-from odata-post nil))
    (let ((json (json:decode-json-from-string response)))
      (when (>= status 400)
        (error 'odata-request-error
               :http-status status
               :format-control "OData request error (~a): ~a"
               :format-arguments (list status (accesses json :error :message))))
      json)))

(defun odata-patch (uri data &key (json-encode t) authorization)
  (multiple-value-bind (response status)
      (drakma:http-request (quri:render-uri uri)
                           :preserve-uri t
                           :content (if json-encode
                                        (json:encode-json-to-string data)
                                        data)
                           :additional-headers (when authorization
                                                 (list (cons "Authorization"
                                                             authorization)))
                           :content-type "application/json;odata.metadata=minimal"
                           :accept "application/json"
                           :method :patch)
    (if (and (null response) (< status 400)) ;; no content
        (return-from odata-patch nil))
    (let ((json (json:decode-json-from-string response)))
      (when (>= status 400)
        (error 'odata-request-error
               :http-status status
               :format-control "OData request error (~a): ~a"
               :format-arguments (list status (accesses json :error :message)))))))

(defun call-with-odata-base (base func)
  (let ((*odata-base* base))
    (funcall func)))

(defmacro with-odata-base (base &body body)
  `(call-with-odata-base ,base (lambda () ,@body)))

(defun child-node (name node)
  (find-if (lambda (nd)
             (string= (dom:node-name nd) name))
           (dom:child-nodes node)))

(defun camel-case-to-lisp (string)
  (string-upcase (cl-change-case:param-case string)))

(defun lisp-to-camel-case (string)
  (cl-change-case:camel-case string))

(defun compile-$filter (exp)
  (when (stringp exp)
    (return-from compile-$filter exp))
  (ecase (first exp)
    (:eq (format nil "~a eq ~a" (second exp) (format-arg (third exp))))
    (:= (format nil "~a eq ~a" (second exp) (format-arg (third exp))))
    (:contains (format nil "contains(~a, ~a)" (compile-path (second exp)) (format-arg (third exp))))))

(defun format-arg (arg)
  (cond
    ((stringp arg) (format nil "'~a'" arg))
    ((symbolp arg) (symbol-name arg))
    (t (princ-to-string arg))))

(defun compile-$expand (exp)
  (cond
    ((stringp exp) exp)
    ((eql exp :all) "*")
    ((eql exp t) "*")
    ((null exp) nil)
    (t
     (with-output-to-string (s)
       (princ (compile-$expand-path (first exp)) s)
       (loop for x in (rest exp)
          do
            (princ "," s)
            (princ (compile-path x) s))))))

(defun compile-path (path)
  (cond
    ((stringp path) path)
    (t
     (with-output-to-string (s)
       (princ (first path) s)
       (loop for x in (rest path)
          do
            (princ "/" s)
            (princ x s))))))

;; (odata::compile-$expand "asdf")
;; (odata::compile-$expand '("asdf"))
;; (odata::compile-$expand '("asdf" "foo"))
;; (odata::compile-$expand '("asdf" "foo" ("Bar" "Baz")))

(defun compile-$select (exp)
  (cond
    ((stringp exp) exp)
    ((null exp) nil)
    (t
     (with-output-to-string (s)
       (princ (first exp) s)
       (loop for x in (rest exp)
          do
            (princ "," s)
            (princ x s))))))

;; (compile-$select "foo,bar")
;; (compile-$select '("foo" "bar"))

(defun compile-$search (exp)
  exp)
