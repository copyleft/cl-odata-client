;;;; odata.lisp

(in-package #:odata)

(defparameter +trip-pin+ "https://services.odata.org/V4/TripPinServiceRW")

(push '("application" . "json") drakma:*text-content-types*)

(drakma:http-request +trip-pin+)

(defparameter +trip-pin-modify+
  (multiple-value-bind (response status headers modify-url)
      (drakma:http-request +trip-pin+)
    modify-url))

(json:decode-json-from-string
 (drakma:http-request +trip-pin-modify+))
