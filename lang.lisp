(in-package :odata/lang)

(defgeneric odata-url (object)
  (:documentation "Get the ODATA service url of OBJECT."))

(defmethod odata-url ((uri quri:uri))
  uri)

(defmethod odata-url ((string string))
  (quri:uri string))

(defmethod odata-url ((entity odata-client::odata-entity))
  (odata-client::odata-id entity))

(defmethod odata-url ((entity-set odata-client::odata-entity-set))
  (odata-client::odata-context entity-set))

(defun read-odata-response (data type)
  "Read the ODATA response from DATA.

TYPE indicates how to do it:
- If NIL, data is left as it is, an association list (the default).
- If :COLLECTION, then the collection elements are returned in an association list.
- If :VALUE, then the property value is returned.
- If T, then ODATA-CLIENT::READ-ODATA-RESPONSE function is used.
" 
  (cond
    ((null type) data)
    ((eql type :collection) (access data :value))
    ((eql type :value) (access data :value))
    (t (odata-client::read-odata-response data))))

(defun singleton (url name)
  "Access the ODATA singleton with name NAME at URL.
See: https://www.odata.org/getting-started/advanced-tutorial/#querySingleton ."
  (quri:uri (format nil "~a~a" (odata-url url)
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
      (odata-client::http-request (quri:render-uri (odata-url url))
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
      (odata-client::http-request (quri:render-uri (odata-url url))
                                  :method :put
                                  :preserve-uri t
                                  :content (odata-client::encode-json-to-string data)
                                  :content-type "application/json"
                                  :accept "application/json")
    (when (>= status 400)
      (error "Error ~a: ~a" status (accesses (odata-client::decode-json-from-string response) :error :message)))))

(defun create (url data)
  "Perform a resource creation request with DATA at ODATA service at URL."
  (post (odata-url url) data))

(defun del (url)
  "Perform a resource deletion request at ODATA service at URL."
  (odata-client::http-request (quri:render-uri (odata-url url))
                              :method :delete
                              :preserve-uri t))

(defun patch (url data)
  "Perform a resource PATCH request with DATA to ODATA service at URL."
  (odata-client:odata-patch (odata-url url) data))

(defun update (url data)
  "Perform a resource update (PUT request) with DATA to ODATA service at URL."
  (odata-client::odata-put (odata-url url) data))

(defun property (url name)
  "Access the resource property with name NAME."
  (quri:uri (format nil "~a/~a" (odata-url url)
                    (if (stringp name) name
                        (odata-client::lisp-to-camel-case (string name))))))

(defun collection (url name)
  "Access the resource collection at NAME."
  (quri:uri (format nil "~a/~a" (odata-url url)
		    (if (stringp name) name
                        (odata-client::lisp-to-camel-case (string name))))))

(defun id (url id)
  "Get ODATA resource id."
  (quri:uri (format nil "~a('~a')" (odata-url url) id)))

(defun parameter (url param value)
  "Add parameter PARAM with VALUE to current request."
  (push (cons param value)
        (quri:uri-query-params url))
  url)

(defun $filter (url exp)
  "Add ODATA $filter parameter to URL.

The $filter system query option allows clients to filter a collection of resources that are addressed by a request URL. The expression specified with $filter is evaluated for each resource in the collection, and only items where the expression evaluates to true are included in the response. Resources for which the expression evaluates to false or to null, or which reference properties that are unavailable due to permissions, are omitted from the response.

See: ODATA-CLIENT::compile-$filter
See: https://www.odata.org/getting-started/basic-tutorial/#filter"
  (parameter url "$filter" (odata-client::compile-$filter exp)))

(defun $expand (url exp)
  "Add ODATA $expand parameter to URL.

The $expand system query option specifies the related resources to be included in line with retrieved resources.

EXP is the list of things to expand.

Examples:

'(\"asdf\" \"foo\")) => \"asdf,foo\"
'(\"asdf\" \"foo\" (\"Bar\" \"Baz\")) => \"asdf,foo,Bar/Baz\"

See: ODATA-CLIENT::COMPILE-$EXPAND .
See: https://www.odata.org/getting-started/basic-tutorial/#expand ."
  (parameter url "$expand" (odata-client::compile-$expand exp)))

(defun $select (url exp)
  "Adds ODATA $select parameter to URL.

The $select system query option allows the clients to requests a limited set of properties for each entity.

EXP can be either a string or a list of strings.
Elements of EXP are just separated by comma.

Examples:
(compile-$select \"name\") => \"foo\"
(compile-$select '(\"name\" \"surname\")) => \"name,surname\"

See: ODATA-CLIENT::COMPILE-$SELECT
See: https://www.odata.org/getting-started/basic-tutorial/#select"
  (parameter url "$select" (odata-client::compile-$select exp)))

(defun $search (url exp)
  "The $search system query option restricts the result to include only those entities matching the specified search expression.

See: https://www.odata.org/getting-started/basic-tutorial/#search"
  (parameter url "$search" (odata-client::compile-$search exp)))

(defun $top (url top)
  "The $top system query option requests the number of items in the queried collection to be included in the result.

See: https://www.odata.org/getting-started/basic-tutorial/#topskip"
  (check-type top integer)
  (parameter url "$top" top))

(defun $skip (url skip)
  "The $skip query option requests the number of items in the queried collection that are to be skipped and not included in the result.

See: https://www.odata.org/getting-started/basic-tutorial/#topskip"
  (check-type skip integer)
  (parameter url "$skip" skip))

(defun $value (url)
  "Address the raw value of a primitive property.

Example: returns the raw value of property Name of an Airport.
(-> +trip-pin-modify+
   (collection \"Airports\") (id \"KSFO\")
   (property \"Name\") ($value)

See: https://www.odata.org/getting-started/basic-tutorial/#propertyVal
"
  (property url "$value"))

(defun $orderby (url property &optional (order :asc))
  "The $orderby system query option allows clients to request resources in either ascending order using asc or descending order using desc. If asc or desc not specified, then the resources will be ordered in ascending order."
  (check-type order (member :asc :desc))
  (parameter url "$orderby" (format nil "~a ~a" property
                                    (string-downcase (princ-to-string order)))))

(defun $count (url)
  "The $count system query option allows clients to request a count of the matching resources included with the resources in the response."
  (parameter url "$count" "true"))

(defun $ref (url)
  "A successful POST request to a navigation property's references collection adds a relationship to an existing entity."
  (property url "$ref"))

(defun path (url &rest path)
  "Access entity in a PATH.

Example:

(-> +trip-pin-modify+
              (collection \"People\")
              (id \"russellwhyte\")
              (path \"Trips(0)\"
                    \"PlanItems(11)\"
                    \"Microsoft.OData.SampleService.Models.TripPin.Flight\"
                    \"Airline\"))"
  (let ((uri (odata-url url)))
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
