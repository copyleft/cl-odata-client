(defpackage :odata/lang
  (:use :cl :odata :access)
  (:export :singleton
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
  (quri:uri (format nil "~a~a" url (if (stringp name) name
                             (lisp-to-camel-case (string name))))))

(defun fetch (url &optional type)
  (case type
    (:collection (access (fetch url) :value))
    (:value (access (fetch url) :value))
    (t (odata::odata-get url))))

(defun post (url &optional data)
  (odata::odata-post url data))

(defun link (url data)
  (multiple-value-bind (response status)
      (drakma:http-request (quri:render-uri url)
                           :method :post
                           :preserve-uri t
                           :content (json:encode-json-to-string data)
                           :content-type "application/json"
                           :accept "application/json")
    (when (>= status 400)
      (error "Error ~a: ~a" status (accesses (json:decode-json-from-string response) :error :message)))))

(defun update-link (url data)
  (multiple-value-bind (response status)
      (drakma:http-request (quri:render-uri url)
                           :method :put
                           :preserve-uri t
                           :content (json:encode-json-to-string data)
                           :content-type "application/json"
                           :accept "application/json")
    (when (>= status 400)
      (error "Error ~a: ~a" status (accesses (json:decode-json-from-string response) :error :message)))))
  

(defun create (url data)
  (post url data))

(defun del (url)
  (drakma:http-request (quri:render-uri url) :method :delete
                       :preserve-uri t))

(defun patch (url data)
  (odata::odata-patch url data))

(defun update (url data)
  (patch url data))

(defun property (url name)
  (quri:uri (format nil "~a/~a" url
                    (if (stringp name) name
                        (lisp-to-camel-case (string name))))))

(defun collection (url name)
  (quri:uri (format nil "~a/~a" url (if (stringp name) name
                                        (lisp-to-camel-case (string name))))))

(defun id (url id)
  (quri:uri (format nil "~a('~a')" url id)))

(defun parameter (url param value)
  (push (cons param value)
        (quri:uri-query-params url))
  url)

(defun $filter (url exp)
  (parameter url "$filter" (odata::compile-$filter exp)))

(defun $expand (url exp)
  (parameter url "$expand" (odata::compile-$expand exp)))

(defun $select (url exp)
  (parameter url "$select" (odata::compile-$select exp)))

(defun $search (url exp)
  (parameter url "$search" (odata::compile-$search exp)))

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
                       (princ (odata::lisp-to-camel-case (string (car arg))) s)
                       (princ "=" s)
                       (princ (cdr arg) s)))
                (princ (if (stringp name) name
                           (string-upcase (odata::lisp-to-camel-case (string name)) :end 1))
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
