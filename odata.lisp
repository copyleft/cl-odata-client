;;;; odata.lisp

(in-package #:odata)

(defparameter +trip-pin+ "https://services.odata.org/V4/TripPinServiceRW")

(push '("application" . "json") drakma:*text-content-types*)

(drakma:http-request +trip-pin+)

(defparameter +trip-pin-modify+
  (multiple-value-bind (response status headers modify-url)
      (drakma:http-request +trip-pin+)
    modify-url))

(defvar *odata-base*)

(defun odata-get (url)
  (access (json:decode-json-from-string
           (drakma:http-request url))
          :value))  

(defun odata-get* (uri &optional (base *odata-base*))
  (odata-get (puri:merge-uris uri base)))

(defun call-with-odata-base (base func)
  (let ((*odata-base* base))
    (funcall func)))

(defmacro with-odata-base (base &body body)
  `(call-with-odata-base ,base (lambda () ,@body)))

(with-odata-base +trip-pin-modify+
  (odata-get* "#Person"))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter +msgraph-metadata+
    (cxml:parse (asdf:system-relative-pathname :odata "msgraph.xml")
                (cxml-dom:make-dom-builder)))

(defun child-node (name node)
  (find-if (lambda (nd)
             (string= (dom:node-name nd) name))
           (dom:child-nodes node)))

(defun generate-odata-enum (node)
  `(defenum:defenum ,(intern (json:camel-case-to-lisp (dom:get-attribute node "Name")))
       ,(loop
            for child across (dom:child-nodes node)
            when (string= (dom:node-name child) "Member")
           collect (list (intern (json:camel-case-to-lisp
                                  (concatenate 'string (dom:get-attribute node "Name")
                                               "/"
                                               (dom:get-attribute child "Name"))))
                          (parse-integer (dom:get-attribute child "Value"))))))

(let* ((edmx (dom:document-element +msgraph-metadata+))
       (data-services (child-node "edmx:DataServices" edmx))
       (schema (child-node "Schema" data-services))
       (enums (remove-if-not (lambda (node) (string= (dom:node-name node) "EnumType"))
                             (dom:child-nodes schema))))
  (loop for enum across enums
     collect (generate-odata-enum enum)))

(defmacro defenums ()
  (let* ((edmx (dom:document-element +msgraph-metadata+))
       (data-services (child-node "edmx:DataServices" edmx))
       (schema (child-node "Schema" data-services))
       (enums (remove-if-not (lambda (node) (string= (dom:node-name node) "EnumType"))
                             (dom:child-nodes schema))))
  `(progn ,@(loop for enum across enums
               collect (generate-odata-enum enum)))))

(defenums)



)
