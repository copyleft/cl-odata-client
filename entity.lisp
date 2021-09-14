(in-package :odata/entity)

(defparameter *schemas* (make-hash-table :test 'equalp)
  "Cached ODATA schemas")

(defun fetch-schema (schema-url)
  "Fetch and parse and ODATA schema from SCHEMA-URL."
  (xmls:parse
   (drakma:http-request
    (ppcre:regex-replace "serviceRoot" schema-url *service-root*)
    :want-stream t)))

(defun find-schema (schema-url)
  "Return cached schema at SCHEMA-URL, or fetch and add to the cache."
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
                                          (eql (search "ODATA-" (symbol-name (car cons)) :test 'string=) 0))
                                        data)))

(defun get-property (entity property-name)
  "Get value of property PROPERTY-NAME in ENTITY."
  (access (entity-properties entity) property-name))

(defmacro with-properties (properties entity &body body)
  "Bind PROPERTIES in ENTITY.

Example:

(with-properties (user-name) user
  (print user-name))"
  
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

(defun read-odata-response (data)
  "Unserializes an ODATA request response."
  (cond
    ((access data :value) ;; Can we assume this is a collection?
     (make-odata-entity-set data))
    (t (make-odata-entity data))))
