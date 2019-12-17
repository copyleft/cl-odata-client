;; https://www.odata.org/blog/trippin-new-odata-v4-sample-service/

;; TripPin is a sample service based on OData V4. Generally speaking, TripPin provides a service that can manage people's trips. The service is designed for below purposes:

;;    Build a service that will cover as many features for OData V4 as possible.
;;    Build a more real world service, and will use it to build an end-to-end ecosystem.
;;    Make a service reference for developers to follow when they try to build an OData service.

;; https://services.odata.org/V4/TripPinServiceRW

(defpackage :odata.trip-pin
  (:use :cl :odata))

(in-package :odata.trip-pin)

(defparameter +trip-pin-base+ "https://services.odata.org/V4/TripPinServiceRW")

(defparameter +trip-pin-modify+
  (multiple-value-bind (response status headers modify-url)
      (drakma:http-request +trip-pin-base+)
    (quri:uri (princ-to-string modify-url))))

(defparameter +trip-pin-metadata+
  (odata/metamodel::parse-metamodel
   (probe-file
    (asdf:system-relative-pathname :odata "TripPin.xml"))))

(defparameter +trip-pin-service-spec+
  (json:decode-json-from-source (drakma:http-request +trip-pin-base+)))

(odata::def-packages #.+trip-pin-metadata+)

(odata::def-enums #.+trip-pin-metadata+)
(odata::def-entities #.+trip-pin-metadata+)

(odata::def-service-model-functions
    #.+trip-pin-service-spec+
    #.+trip-pin-metadata+)

(odata::with-odata-base +trip-pin-modify+
  (odata::odata-get* "People"))

(odata::with-odata-base +trip-pin-modify+
  (odata::odata-get-entities "People" '|Microsoft.OData.SampleService.Models.TripPin|:person))

(odata::with-odata-base +trip-pin-modify+
  (fetch-people)
  (fetch-airlines))

(odata::with-odata-base +trip-pin-modify+
  (fetch-airlines :$filter "contains(Location/Address, 'San Francisco'"))

(odata::with-odata-base +trip-pin-modify+
  (fetch-people :$filter "FirstName eq 'Scott'"))
