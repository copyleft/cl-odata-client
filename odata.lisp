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
