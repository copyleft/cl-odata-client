(in-package #:odata-client)

(defvar *service-root* nil
  "ODATA service base url.")

(defvar *access-token* nil
  "ODATA service api token.")

;;------ Utility functions ------------------

(defun camel-case-to-lisp (string)
  (string-upcase (cl-change-case:param-case string)))

(defun lisp-to-camel-case (string)
  (cl-change-case:camel-case string))

(defun decode-json-from-source (source)
  (let ((json:*json-identifier-name-to-lisp* 'camel-case-to-lisp))
    (json:decode-json-from-source source)))

(defun encode-json-to-string (object)
  (let ((json:*lisp-identifier-name-to-json* 'lisp-to-camel-case))
    (json:encode-json-to-string object)))

(defun decode-json-from-string (string)
  (let ((json:*json-identifier-name-to-lisp* 'camel-case-to-lisp))
    (json:decode-json-from-string string)))

(defun http-request (url &rest args)
  (let ((drakma:*text-content-types* (cons (cons "application" "json") drakma:*text-content-types*)))
    (apply #'drakma:http-request url args)))

;;--- ODATA service accessing ---------------------

(define-condition odata-request-error (simple-error)
  ((http-status :initarg :http-status :accessor http-status)))

(defun odata-get (url &key $filter $expand authorization)
  "GET request on an ODATA service at URL.
$filter is an ODATA $filter expression.
$expand is an ODATA $expand expression.
AUTHORIZATION is the authorization token.

See: http://docs.oasis-open.org/odata/odata/v4.0/errata03/os/complete/part1-protocol/odata-v4.0-errata03-os-part1-protocol-complete.html#_The_$filter_System"
  (let ((url* (if (stringp url)
                  (quri:uri url)
                  url)))
    (when $filter
      (push (cons "$filter" $filter) (quri:uri-query-params url*)))
    (when $expand
      (push (cons "$expand" $expand) (quri:uri-query-params url*)))
    (multiple-value-bind (response status)
        (http-request (quri:render-uri url*)
                      :preserve-uri t
                      :additional-headers (when authorization
                                            (list (cons "Authorization"
                                                        authorization)))
                      :want-stream t)
      (let ((json (decode-json-from-source response)))
        (when (>= status 400)
          (error 'odata-request-error
                 :http-status status
                 :format-control "OData request error (~a): ~a"
                 :format-arguments (list status (accesses json :error :message))))
        json))))

(defun odata-get* (uri &rest args &key $filter $expand)
  "Make an ODATA-GET request using *SERVICE-ROOT* as URL base."
  (apply #'odata-get (quri:merge-uris uri *service-root*)
         args))

(defun odata-post (uri data &key (json-encode t) authorization)
  "Make a POST request to ODATA service at URI.
DATA is the data to be posted. It is encoded using ENCODE-JSON-TO-STRING."
  (multiple-value-bind (response status)
      (http-request (quri:render-uri uri)
                    :preserve-uri t
                    :content (if json-encode
                                 (encode-json-to-string data)
                                 data)
                    :additional-headers (when authorization
                                          (list (cons "Authorization"
                                                      authorization)))
                    :content-type "application/json;odata.metadata=minimal"
                    :accept "application/json"
                    :method :post)
    (if (and (null response) (< status 400)) ;; no content
        (return-from odata-post nil))
    (let ((json (decode-json-from-string response)))
      (when (>= status 400)
        (error 'odata-request-error
               :http-status status
               :format-control "OData request error (~a): ~a"
               :format-arguments (list status (accesses json :error :message))))
      json)))

(defun odata-patch (uri data &key (json-encode t) authorization)
  "Make a PATCH request to ODATA service at URI.
DATA is the data to be posted. It is encoded using ENCODE-JSON-TO-STRING."
  (multiple-value-bind (response status)
      (http-request (quri:render-uri uri)
                    :preserve-uri t
                    :content (if json-encode
                                 (encode-json-to-string data)
                                 data)
                    :additional-headers (when authorization
                                          (list (cons "Authorization"
                                                      authorization)))
                    :content-type "application/json;odata.metadata=minimal"
                    :accept "application/json"
                    :method :patch)
    (if (and (null response) (< status 400)) ;; no content
        (return-from odata-patch nil))
    (let ((json (decode-json-from-string response)))
      (when (>= status 400)
        (error 'odata-request-error
               :http-status status
               :format-control "OData request error (~a): ~a"
               :format-arguments (list status (accesses json :error :message)))))))

(defun odata-put (uri data &key (json-encode t) authorization)
  "Make a PUT (update) request to ODATA service at URI.
DATA is the data to be posted. It is encoded using ENCODE-JSON-TO-STRING."
  (multiple-value-bind (response status)
      (http-request (quri:render-uri uri)
                    :preserve-uri t
                    :content (if json-encode
                                 (encode-json-to-string data)
                                 data)
                    :additional-headers (when authorization
                                          (list (cons "Authorization"
                                                      authorization)))
                    :content-type "application/json;odata.metadata=minimal"
                    :accept "application/json"
                    :method :patch)
    (if (and (null response) (< status 400)) ;; no content
        (return-from odata-put nil))
    (let ((json (decode-json-from-string response)))
      (when (>= status 400)
        (error 'odata-request-error
               :http-status status
               :format-control "OData request error (~a): ~a"
               :format-arguments (list status (accesses json :error :message)))))))

(defun call-with-service-root (base func)
  (let ((*service-root* base))
    (funcall func)))

(defmacro with-service-root (base &body body)
  `(call-with-service-root ,base (lambda () ,@body)))

(defun child-node (name node)
  (find-if (lambda (nd)
             (string= (dom:node-name nd) name))
           (dom:child-nodes node)))

;;---- ODATA DSL --------------------------------------------

(defun compile-$filter (exp)
  "Compile ODATA $filter expression.

The $filter system query option allows clients to filter a collection of resources that are addressed by a request URL. The expression specified with $filter is evaluated for each resource in the collection, and only items where the expression evaluates to true are included in the response. Resources for which the expression evaluates to false or to null, or which reference properties that are unavailable due to permissions, are omitted from the response.

EXP can be either:
- And s-expression (a list). Should follow the filter mini-language and get compiled to a string.
- A string. In this case, the string is returned as is.

Return type: string.

Syntax:
(:= exp exp) : Equals.
(:eq exp exp): Equals.
(:> exp exp): Greater than.
(:< exp exp): Lower than.
(:contains path exp): Contains.
And more ...

See: https://www.odata.org/getting-started/basic-tutorial/#filter"
  (when (stringp exp)
    (return-from compile-$filter exp))
  (ecase (first exp)
    ((:eq :=) (format nil "~a eq ~a" (second exp) (format-arg (third exp))))
    ((:ne :/=) (format nil "~a ne ~a" (second exp) (format-arg (third exp))))
    ((:gt :>) (format nil "~a gt ~a" (second exp) (format-arg (third exp))))
    ((:lt :<) (format nil "~a lt ~a" (second exp) (format-arg (third exp))))
    ((:ge :>=) (format nil "~a ge ~a" (second exp) (format-arg (third exp))))
    ((:le :<=) (format nil "~a le ~a" (second exp) (format-arg (third exp))))
    (:and (format nil "~a and ~a"
                  (compile-$filter (second exp))
                  (compile-$filter (third exp))))
    (:or (format nil "~a or ~a"
                 (compile-$filter (second exp))
                 (compile-$filter (third exp))))
    (:not (format nil "not ~a" (compile-$filter (second exp))))
    (:contains (format nil "contains(~a, ~a)" (compile-path (second exp)) (format-arg (third exp))))))

(defun format-arg (arg)
  (cond
    ((stringp arg) (format nil "'~a'" arg))
    ((symbolp arg) (symbol-name arg))
    (t (princ-to-string arg))))

(defun compile-$expand (exp)
  "Returns and ODATA $expand expression from Lisp expression EXP.

Examples:

(compile-$expand \"asdf\") => \"asdf\"
(compile-$expand '(\"asdf\" \"foo\")) => \"asdf,foo\"
(compile-$expand '(\"asdf\" \"foo\" (\"Bar\" \"Baz\"))) => \"asdf,foo,Bar/Baz\"

See: http://docs.oasis-open.org/odata/odata/v4.01/odata-v4.01-part2-url-conventions.html#sec_SystemQueryOptionexpand
"
  (cond
    ((stringp exp) exp)
    ((eql exp :all) "*")
    ((eql exp t) "*")
    ((null exp) nil)
    (t
     (with-output-to-string (s)
       (princ (compile-path (first exp)) s)
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

(defun compile-$select (exp)
  "Return ODATA $select string parameter from EXP.

EXP can be either a string or a list of strings.
Elements of EXP are just separated by comma.

Examples:
(compile-$select \"name\") => \"foo\"
(compile-$select '(\"name\" \"surname\")) => \"name,surname\"

See: https://www.odata.org/getting-started/basic-tutorial/#select
"
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

(defun compile-$search (exp)
  exp)

;;-------- Entities ---------------------------------

(defparameter *schemas* (make-hash-table :test 'equalp)
  "Cached ODATA schemas")

(defun fetch-schema (schema-url)
  (xmls:parse
   (drakma:http-request
    (ppcre:regex-replace "serviceRoot" schema-url *service-root*)
    :want-stream t)))

(defun find-schema (schema-url)
  (or (gethash schema-url *schemas*)
      (setf (gethash schema-url *schemas*)
            (fetch-schema schema-url))))

(defclass odata-entity ()
  ((id :accessor odata-id :initarg :id)
   (context :accessor odata-context :initarg :context)
   (etag :accessor odata-etag :initarg :etag)
   (edit-link :accessor odata-edit-link :initarg :edit-link)
   (properties :accessor entity-properties :initarg :properties))
  (:documentation "An ODATA entity."))

(defclass odata-entity-set ()
  ((context :accessor odata-context :initarg :context)
   (next-link :accessor odata-next-link :initarg :next-link)
   (elements :accessor entity-set-elements :initarg :elements))
  (:documentation "And ODATA entity set."))

(defmethod print-object ((entity odata-entity) stream)
  (print-unreadable-object (entity stream :type nil :identity nil)
    (format stream "~a" (odata-id entity))))

(defmethod print-object ((entity-set odata-entity-set) stream)
  (print-unreadable-object (entity-set stream :type nil :identity nil)
    (format stream "~a" (odata-context entity-set))))

(defun make-odata-entity (data)
  "Unserialize data and create an ODATA-ENTITY object."
  (make-instance 'odata-entity
                 :id (access data :odata-id)
                 :context (access data :odata-context)
                 :etag (access data :odata-etag)
                 :edit-link (access data :odata-edit-link)
                 :properties (remove-if (lambda (cons)
                                          (eql (position "ODATA-" (symbol-name (car cons)) :test 'string=) 0))
                                        data)))

(defun get-property (entity property-name)
  (access (entity-properties entity) property-name))

(defmacro with-properties (properties entity &body body)
  (alexandria:once-only (entity)
    `(let ,(loop for property in properties
                 collect `(,property (get-property ,entity ',(alexandria:make-keyword (symbol-name property)))))
       ,@body)))

(defun make-odata-entity-set (data)
  "Unserialize a collection data and create an ODATA-ENTITY-SET object."
  (make-instance 'odata-entity-set
                 :context (access data :odata-context)
                 :next-link (access data :next-link)
                 :elements (mapcar 'make-odata-entity
                                   (access data :value))))
