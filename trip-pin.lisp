(defpackage :odata.trip-pin
  (:use :cl :odata))

(in-package :odata.trip-pin)

(defparameter +trip-pin-base+ "https://services.odata.org/V4/TripPinServiceRW")

(defparameter +trip-pin-modify+
  (multiple-value-bind (response status headers modify-url)
      (drakma:http-request +trip-pin-base+)
    modify-url))

(defparameter +trip-pin-metadata+
  (cxml:parse
   (probe-file
    (asdf:system-relative-pathname :odata "TripPin.xml"))
   (cxml-dom:make-dom-builder)))

(odata::def-enums #.+trip-pin-metadata+)
(odata::def-entities #.+trip-pin-metadata+)

(odata::with-odata-base +trip-pin-modify+
  (odata::odata-get-entities "People" 'person))
