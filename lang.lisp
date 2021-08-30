(defpackage :odata/lang
  (:use :cl :access)
  (:export
   :singleton
   :fetch
   :post
   :create
   :del
   :update
   :patch
   :link
   :path
   :update-link
   :property
   :collection
   :fcall
   :$filter
   :$expand
   :id
   :$skip
   :$top
   :$value
   :$orderby
   :$select
   :$search
   :$ref)
  (:documentation
   "This package exports functions that are meant to be used with arrows syntax to interact with an ODATA service."))

(in-package :odata/lang)

(defun read-odata-response (data type)
  (cond
    ((null type) data)
    ((eql type :collection) (access data :value))
    ((eql type :value) (access data :value))
    (t data)))

(defun singleton (url name)
  "Access the ODATA singleton with name NAME at URL.
See: https://www.odata.org/getting-started/advanced-tutorial/#querySingleton ."
  (quri:uri (format nil "~a~a" url
                    (if (stringp name) name
                        (odata-client::lisp-to-camel-case (string name))))))

(defun fetch (url &optional type)
  "Perform and HTTP GET to the ODATA URL."
  (let ((data (odata-client:odata-get url)))
    (read-odata-response data type)))

(defun post (url &optional data)
  "Perform an HTTP POST to the ODATA URL."
  (odata-client:odata-post url data))

(defun link (url data)
  "Add a link to a related entity.

Relationships from one entity to another are represented as navigation properties.
A successful POST request to a navigation property's references collection adds a relationship to an existing entity.

Example: add 'vincentcalabrese' to friends of 'scottketchum'

(-> +trip-pin-modify+
   (collection \"People\") (id \"scottketchum\")
   (property \"Friends\") ($ref)
   (link `((\"@odata.context\" . ,(quri:render-uri +trip-pin-modify+))
           (\"@odata.id\" . \"People('vincentcalabrese')\"))))

"
  (multiple-value-bind (response status)
      (odata-client::http-request (quri:render-uri url)
                           :method :post
                           :preserve-uri t
                           :content (odata-client::encode-json-to-string data)
                           :content-type "application/json"
                           :accept "application/json")
    (when (>= status 400)
      (error "Error ~a: ~a" status (accesses (odata-client::decode-json-from-string response) :error :message)))))

(defun update-link (url data)
  "Update an already existent link.

A successful PUT request to a single-valued navigation property’s reference resource changes the related entity.

Example: change the Airline of a Flight

(-> +trip-pin-modify+
              (collection \"People\")
              (id \"russellwhyte\")
              (path \"Trips(0)\"
                    \"PlanItems(11)\"
                    \"Microsoft.OData.SampleService.Models.TripPin.Flight\"
                    \"Airline\")
              (update-link `((\"@odata.context\" . ,(quri:render-uri +trip-pin-modify+))
           (\"@odata.id\" . \"Airlines('FM')\"))))
"
  (multiple-value-bind (response status)
      (odata-client::http-request (quri:render-uri url)
                           :method :put
                           :preserve-uri t
                           :content (odata-client::encode-json-to-string data)
                           :content-type "application/json"
                           :accept "application/json")
    (when (>= status 400)
      (error "Error ~a: ~a" status (accesses (odata-client::decode-json-from-string response) :error :message)))))

(defun create (url data)
  "Perform a resource creation request with DATA at ODATA service at URL."
  (post url data))

(defun del (url)
  "Perform a resource deletion request at ODATA service at URL."
  (odata-client::http-request (quri:render-uri url)
			     :method :delete
			     :preserve-uri t))

(defun patch (url data)
  "Perform a resource PATCH request with DATA to ODATA service at URL."
  (odata-client:odata-patch url data))

(defun update (url data)
  "Perform a resource update (PUT request) with DATA to ODATA service at URL."
  (odata-client::odata-put url data))

(defun property (url name)
  "Access the resource property with name NAME."
  (quri:uri (format nil "~a/~a" url
                    (if (stringp name) name
                        (odata-client::lisp-to-camel-case (string name))))))

(defun collection (url name)
  "Access the resource collection at NAME."
  (quri:uri (format nil "~a/~a" url (if (stringp name) name
                                        (odata-client::lisp-to-camel-case (string name))))))

(defun id (url id)
  "Get ODATA resource id."
  (quri:uri (format nil "~a('~a')" url id)))

(defun parameter (url param value)
  "Add parameter PARAM with VALUE to current request."
  (push (cons param value)
        (quri:uri-query-params url))
  url)

(defun $filter (url exp)
  "Add ODATA filter parameter to the current request.

The $filter system query option allows clients to filter a collection of resources that are addressed by a request URL. The expression specified with $filter is evaluated for each resource in the collection, and only items where the expression evaluates to true are included in the response. Resources for which the expression evaluates to false or to null, or which reference properties that are unavailable due to permissions, are omitted from the response.

See: ODATA-CLIENT::compile-$filter
See: https://www.odata.org/getting-started/basic-tutorial/#filter"
  (parameter url "$filter" (odata-client::compile-$filter exp)))

(defun $expand (url exp)
  (parameter url "$expand" (odata-client::compile-$expand exp)))

(defun $select (url exp)
  (parameter url "$select" (odata-client::compile-$select exp)))

(defun $search (url exp)
  (parameter url "$search" (odata-client::compile-$search exp)))

(defun $top (url top)
  (check-type top integer)
  (parameter url "$top" top))

(defun $skip (url skip)
  (check-type skip integer)
  (parameter url "$skip" skip))

(defun $value (url)
  (property url "$value"))

(defun $orderby (url property &optional (order :asc))
  (check-type order (member :asc :desc))
  (parameter url "$orderby" (format nil "~a ~a" property
                                    (string-downcase (princ-to-string order)))))

(defun $ref (url)
  (property url "$ref"))

(defun path (url &rest path)
  (let ((uri url))
    (loop
      for x in path
      do (setf uri (property uri x)))
    uri))

(defun fcall (url name &rest args)
  "Call the ODATA action with name NAME and arguments ARGS.

Actions are operations exposed by an OData service that MAY have side effects when invoked. Actions MAY return data but MUST NOT be further composed with additional path segments.

See: https://docs.oasis-open.org/odata/odata/v4.01/odata-v4.01-part1-protocol.html#sec_Actions"
  (property url
            (with-output-to-string (s)
              (flet ((print-arg (arg)
                       (princ (odata-client::lisp-to-camel-case (string (car arg))) s)
                       (princ "=" s)
                       (princ (cdr arg) s)))
                (princ (if (stringp name) name
                           (string-upcase (odata-client::lisp-to-camel-case (string name)) :end 1))
                       s)
                (princ "(" s)
                (when args
                  (let ((args (alexandria:plist-alist args)))
                    (print-arg (first args))
                    (loop
                      for arg in (rest args)
                      do (princ "," s)
                      do (print-arg arg))))
                (princ ")" s)))))
