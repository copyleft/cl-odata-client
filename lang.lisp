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
   :$ref))

(in-package :odata/lang)

(defun singleton (url name)
  (quri:uri (format nil "~a~a" url
                    (if (stringp name) name
                        (odata-client::lisp-to-camel-case (string name))))))

(defun read-odata-response (data type)
  (cond
    ((null type) data)
    ((eql type :collection) (access data :value))
    ((eql type :value) (access data :value))
    (t data)))

(defun fetch (url &optional type)
  (let ((data (odata-client:odata-get url)))
    (read-odata-response data type)))

(defun post (url &optional data)
  (odata-client:odata-post url data))

(defun link (url data)
  (multiple-value-bind (response status)
      (odata-client::http-request (quri:render-uri url)
                           :method :post
                           :preserve-uri t
                           :content (json:encode-json-to-string data)
                           :content-type "application/json"
                           :accept "application/json")
    (when (>= status 400)
      (error "Error ~a: ~a" status (accesses (odata-client::decode-json-from-string response) :error :message)))))

(defun update-link (url data)
  (multiple-value-bind (response status)
      (odata-client::http-request (quri:render-uri url)
                           :method :put
                           :preserve-uri t
                           :content (json:encode-json-to-string data)
                           :content-type "application/json"
                           :accept "application/json")
    (when (>= status 400)
      (error "Error ~a: ~a" status (accesses (odata-client::decode-json-from-string response) :error :message)))))


(defun create (url data)
  (post url data))

(defun del (url)
  (drakma:http-request (quri:render-uri url) :method :delete
                                             :preserve-uri t))

(defun patch (url data)
  (odata-client::odata-patch url data))

(defun update (url data)
  (patch url data))

(defun property (url name)
  (quri:uri (format nil "~a/~a" url
                    (if (stringp name) name
                        (odata-client::lisp-to-camel-case (string name))))))

(defun collection (url name)
  (quri:uri (format nil "~a/~a" url (if (stringp name) name
                                        (odata-client::lisp-to-camel-case (string name))))))

(defun id (url id)
  (quri:uri (format nil "~a('~a')" url id)))

(defun parameter (url param value)
  (push (cons param value)
        (quri:uri-query-params url))
  url)

(defun $filter (url exp)
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
