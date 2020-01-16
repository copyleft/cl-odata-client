;;;; odata.lisp

(in-package #:odata)

(push '("application" . "json") drakma:*text-content-types*)
(setf json:*lisp-identifier-name-to-json*
      'lisp-to-camel-case)
(setf json:*json-identifier-name-to-lisp*
      'camel-case-to-lisp)

(defvar *odata-base*)

(defun odata-get (url &key $filter $expand)
  (let ((url* (if (stringp url)
                  (quri:uri url)
                  url)))
    (when $filter
      (push (cons "$filter" $filter) (quri:uri-query-params url*)))
    (when $expand
      (push (cons "$expand" $expand) (quri:uri-query-params url*)))
    (multiple-value-bind (response status)
        (drakma:http-request (quri:render-uri url*) :preserve-uri t)
      (let ((json (json:decode-json-from-string response)))
        (when (>= status 400)
          (error "OData request error (~a): ~a" status (accesses json :error :message)))
        json))))

(defun odata-get* (uri &rest args &key $filter $expand)
  (apply #'odata-get (quri:merge-uris uri *odata-base*)
         args))

(defun odata-post (uri data &key (json-encode t))
  (multiple-value-bind (response status)
      (drakma:http-request (quri:render-uri uri)
                           :preserve-uri t
                           :content (if json-encode
                                        (json:encode-json-to-string data)
                                        data)
                           :additional-headers '(("OData-Version" . "4.0"))
                           :content-type "application/json;odata.metadata=minimal"
                           :accept "application/json"
                           :method :post)
    (if (= status 204) ;; no content
        (return-from odata-post nil))
    (let ((json (json:decode-json-from-string response)))
      (when (>= status 400)
        (error "OData request error (~a): ~a" status (accesses json :error :message)))
      json)))

(defun odata-patch (uri data &key (json-encode t))
  (multiple-value-bind (response status)
      (drakma:http-request (quri:render-uri uri)
                           :preserve-uri t
                           :content (if json-encode
                                        (json:encode-json-to-string data)
                                        data)
                           :additional-headers '(("OData-Version" . "4.0"))
                           :content-type "application/json;odata.metadata=minimal"
                           :accept "application/json"
                           :method :patch)
    (when (>= status 400)
      (error "OData request error (~a)" status))))

(defun call-with-odata-base (base func)
  (let ((*odata-base* base))
    (funcall func)))

(defmacro with-odata-base (base &body body)
  `(call-with-odata-base ,base (lambda () ,@body)))

(defun odata-get-entities (url type &rest args &key $filter $expand)
  (let ((data (apply #'odata-get* url args)))
    (loop for entity-data in (access data :value)
       collect (unserialize entity-data type))))

(defun odata-get-entity-by-id (url type id &rest args)
  (let ((entity-data (odata-get* (format nil "~a('~a')" url id))))
    (unserialize entity-data type)))

(defun child-node (name node)
  (find-if (lambda (nd)
             (string= (dom:node-name nd) name))
           (dom:child-nodes node)))

(defun camel-case-to-lisp (string)
  (string-upcase (cl-change-case:param-case string)))

(defun lisp-to-camel-case (string)
  (cl-change-case:camel-case string))

(defun generate-odata-enum (node)
  `(defenum:defenum ,(intern (camel-case-to-lisp (odata/metamodel::name node)))
       ,(loop
           for (name . value) in (odata/metamodel::members node)
           collect (list (intern (concatenate 'string (camel-case-to-lisp (odata/metamodel::name node))
                                              "/"
                                              (string-upcase name)))
                         value))))

(defun generate-odata-enum-name (node)
  (let ((enum-name (camel-case-to-lisp (odata/metamodel::name node))))
    `(progn
       ,@(loop for (name . value) in (odata/metamodel::members node)
            collect `(defparameter ,(intern (format nil "+~a/~a+"
                                                    enum-name
                                                    (string-upcase name)))
                       ',(make-symbol (format nil "~a.~a'~a'"
                                              (odata/metamodel::namespace node)
                                              (odata/metamodel::name node)
                                              name)))))))

(defun %def-enums (metadata)
  (loop for schema in (odata/metamodel::schemas (odata/metamodel::data-services metadata))
     appending
       (loop
          for enum in (odata/metamodel::enums schema)
          collect (generate-odata-enum enum)
          collect (generate-odata-enum-name enum))))

(defmacro def-enums (metadata)
  `(progn ,@(%def-enums metadata)))

(defmacro def-packages (metadata)
  (loop for schema in (odata/metamodel::schemas (odata/metamodel::data-services metadata))
     for package-name = (intern (odata/metamodel::namespace schema))
     appending
       `(progn
          (defpackage ,package-name
            (:export :metadata
                     ,@(loop for elem in (odata/metamodel::elements schema)
                          collect (intern (camel-case-to-lisp (odata/metamodel::name elem)) :keyword))))
          (setf (symbol-value (intern "METADATA" ',package-name))
                ,schema))))


(defun %def-entities (metadata)
  (loop for schema in (odata/metamodel::schemas (odata/metamodel::data-services metadata))
     appending
       (loop for entity in (append (odata/metamodel::entity-types schema)
                                   (odata/metamodel::element-types schema
                                                                   'odata/metamodel::complex-type))
          collect (generate-odata-entity entity)
          collect `(defmethod entity-name ((entity-type (eql ',(entity-class-name entity))))
                     ,(odata/metamodel::name entity))
          appending (generate-odata-entity-methods entity)
          collect (generate-odata-entity-serializer entity)
          collect (generate-odata-entity-unserializer entity))))

(defun generate-odata-entity-methods (entity)
  (loop
     for prop in (odata/metamodel::navigation-properties entity)
     collect (generate-entity-navigation-method prop entity)))

(defun generate-entity-navigation-method (navigation-property entity)
  `(defmethod ,(intern (camel-case-to-lisp
                        (concatenate 'string (odata/metamodel::name entity)
                                     "." (odata/metamodel::name navigation-property)))
                       (intern (odata/metamodel::namespace entity)))
       (entity &rest args &key $filter $expand)
     (let ((data (apply #'odata-get (quri:uri (concatenate 'string (odata-id entity) "/" ,(odata/metamodel::name navigation-property))) args)))
       (unserialize-value
        ;; When does the response contain "value" and when does it not? It seems "value" is present for collections, but for only top-level ones?
        (if (access data :value)
            (access data :value)
            data)
        ',(odata/metamodel::property-type navigation-property)))))

(defun entity-class-name (node)
  (intern (camel-case-to-lisp
           (odata/metamodel::name node))
          (intern (odata/metamodel::namespace node))))

(defun generate-odata-entity (node)
  `(defclass ,(entity-class-name node)
       (,@(when (odata/metamodel::base-type node)
            (list (intern (camel-case-to-lisp (odata/metamodel::base-type node))))))
     ((odata-id :initarg :odata-id :accessor odata-id)
      (odata-etag :initarg :odata-etag :accessor odata-etag)
      (odata-edit-link :initarg :odata-edit-link :accessor odata-edit-link)
      ,@(loop
           for property in (odata/metamodel::properties node)
           collect `(,(intern (camel-case-to-lisp (odata/metamodel::name property)))
                      :initarg ,(intern (camel-case-to-lisp
                                         (odata/metamodel::name property)) :keyword)
                      :accessor ,(intern
                                  (camel-case-to-lisp
                                   (concatenate 'string
                                                (odata/metamodel::name node)
                                                "."
                                                (odata/metamodel::name property))))
                      ,@(unless (not (odata/metamodel::is-nullable property))
                          (list :initform nil))

                      )))))

(defun generate-odata-entity-serializer (node)
  `(defmethod odata::serialize ((node ,(entity-class-name node)) stream)
     (let ((json:*json-output* stream))
       (json:with-object ()
         ,@(loop
              for property in (odata/metamodel::structural-properties node)
              collect `(json:as-object-member
                           (,(intern (camel-case-to-lisp (odata/metamodel::name property)) :keyword))
                         (serialize-value (slot-value node ',(intern (camel-case-to-lisp (odata/metamodel::name property))))
                                          ',(odata/metamodel::property-type property))))))))

(defun generate-odata-entity-unserializer (node)
  `(defmethod odata::unserialize (data (type (eql ',(entity-class-name node))))
     (let ((entity (make-instance ',(entity-class-name node))))
       (setf (odata-id entity) (access data :odata-id))
       (setf (odata-etag entity) (access data :odata-etag))
       (setf (odata-edit-link entity) (access data :odata-edit-link))
       ,@(loop
            for property in (odata/metamodel::properties node)
            collect `(setf (slot-value entity ',(intern (camel-case-to-lisp (odata/metamodel::name property))))
                           (unserialize-value
                            (access:access data ,(intern (camel-case-to-lisp (odata/metamodel::name property)) :keyword))
                            ',(odata/metamodel::property-type property))))
       entity)))

(defmacro def-entities (metadata)
  `(progn ,@(%def-entities metadata)))

(defun generate-odata-function (node)
  ;; TODO
  `(defun ,(camel-case-to-lisp (odata/metamodel::name node))
       ,(loop for parameter in (odata/metamodel::parameters node)
           collect (camel-case-to-lisp (odata/metamodel::name parameter)))
     (let ,(loop
              for parameter in (odata/metamodel::parameters node)
              for param-name = (camel-case-to-lisp (odata/metamodel::name parameter))
                ))))

(defun %def-functions (metadata)
  (loop for schema in (odata/metamodel::schemas (odata/metamodel::data-services metadata))
     appending
       (loop for function in (odata/metamodel::element-types schema 'odata/metamodel::odata-function)
          collect (generate-odata-function function))))

(defmacro def-functions (metadata)
  `(progn ,@(%def-functions metadata)))

(defgeneric entity-name (class))

(defgeneric serialize (object stream))
(defgeneric unserialize (data type))
(defgeneric serialize-primitive-value (value type))

(defun serialize-value (value type)
  (if (null value)
      nil
      (case (first type)
        (:primitive (serialize-primitive-value value (intern (second type) :keyword)))
        (:collection (serialize-collection-value value (second type)))
        (:nominal (serialize-nominal-value value type)))))

(defmethod serialize-primitive-value (value (type (eql :|Edm.String|)))
  (json:encode-json value))

(defmethod serialize-primitive-value (value (type (eql :|Edm.Boolean|)))
  (if value
      (write-string "true" json:*json-output*)
      (write-string "false" json:*json-output*)))

(defmethod serialize-primitive-value (value type)
  (json:encode-json value))

(defun serialize-collection-value (collection col-type)
  (json:with-array ()
    (loop for elem in collection
       do (serialize-value elem col-type))))

(defun serialize-nominal-value (value nominal-type)
  (let* ((package (find-package (intern (getf nominal-type :namespace))))
         (class-name (intern (camel-case-to-lisp (getf nominal-type :nominal))
                             package))
         (type (odata/metamodel::find-element
                (symbol-value (intern "METADATA" package))
                (getf nominal-type :nominal))))
    (if (typep type 'odata/metamodel::enum-type)
        (json:encode-json value)
        (serialize value json:*json-output*))))

(defun unserialize-value (value type)
  (if (null value)
      nil
      (case (first type)
        (:primitive (unserialize-primitive-value value (intern (second type) :keyword)))
        (:collection (unserialize-collection-value value (second type)))
        (:nominal (unserialize-nominal-value value type)))))

(defun unserialize-primitive-value (value type)
  value)

(defun unserialize-collection-value (value col-type)
  (loop for elem in value
     collect (unserialize-value elem col-type)))

(defun unserialize-nominal-value (value nominal-type)
  (let* ((package (find-package (intern (getf nominal-type :namespace))))
         (class-name (intern (camel-case-to-lisp (getf nominal-type :nominal))
                             package))
         (type (odata/metamodel::find-element
                (symbol-value (intern "METADATA" package))
                (getf nominal-type :nominal))))
    (if (typep type 'odata/metamodel::enum-type)
        (odata/metamodel::enum-value type value)
        (let ((object (make-instance class-name)))
          (loop for property in (odata/metamodel::properties type)
             do
               (setf (slot-value object (intern (camel-case-to-lisp (odata/metamodel::name property))))
                     (unserialize-value (access:access value (intern (camel-case-to-lisp (odata/metamodel::name property)) :keyword))
                                        (odata/metamodel::property-type property))))
          object))))

(defmacro def-service-model-functions (services metadata)
  (let ((entity-container (odata/metamodel::entity-container metadata)))
    `(progn
       ,@(loop
            for service in (access services :value)
            collect (let ((el (find-if (lambda (el)
                                         (string= (odata/metamodel::name el)
                                                  (access service :name)))
                                       (odata/metamodel::elements entity-container))))
                      (def-service service el))))))

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

(defmethod def-service (service (entity-set odata/metamodel::entity-set))
  (let ((fetch-fn-name (intern (format nil "FETCH-~a" (string-upcase (odata/metamodel::name entity-set)))))
        (fetch-fn-by-id-name (intern (format nil "FETCH-~a-BY-ID"
                                             (camel-case-to-lisp (getf (odata/metamodel::entity-type entity-set) :type))))))
    `(progn
       (defun ,fetch-fn-name (&key $filter $expand)
         (odata-get-entities
          ,(access service :url)
          ',(intern (camel-case-to-lisp (getf (odata/metamodel::entity-type entity-set) :type))
                    (intern (getf (odata/metamodel::entity-type entity-set) :namespace)))
          :$filter (when $filter (compile-$filter $filter))
          :$expand (when $expand (compile-$expand $expand))))
       (defun ,fetch-fn-by-id-name (id &rest args)
         (odata-get-entity-by-id
          ,(access service :url)
          ',(intern (camel-case-to-lisp (getf (odata/metamodel::entity-type entity-set) :type))
                    (intern (getf (odata/metamodel::entity-type entity-set) :namespace)))
          id)))))

(defmethod def-service (service (singleton odata/metamodel::singleton))
  ;; TODO
  )

(defmethod def-service (service (singleton odata/metamodel::function-import))
  ;; TODO
  )

(defmethod def-service (service (singleton odata/metamodel::action-import))
  ;; TODO
  )
