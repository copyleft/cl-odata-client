;; https://www.odata.org/blog/trippin-new-odata-v4-sample-service/

;; TripPin is a sample service based on OData V4. Generally speaking, TripPin provides a service that can manage people's trips. The service is designed for below purposes:

;;    Build a service that will cover as many features for OData V4 as possible.
;;    Build a more real world service, and will use it to build an end-to-end ecosystem.
;;    Make a service reference for developers to follow when they try to build an OData service.

;; https://services.odata.org/V4/TripPinServiceRW

(defpackage :odata.trip-pin
  (:use :cl :odata :odata/lang :cl-arrows :access))

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

(setf drakma:*header-stream* *standard-output*)

(odata::def-packages #.+trip-pin-metadata+)

(odata::def-enums #.+trip-pin-metadata+)
(odata::def-entities #.+trip-pin-metadata+)
(odata::def-functions #.+trip-pin-metadata+)

(odata::def-service-model-functions
    #.+trip-pin-service-spec+
    #.+trip-pin-metadata+)

;; See: https://www.odata.org/getting-started/basic-tutorial/

(-> +trip-pin-modify+
    (collection "People")
    (get*))

;; get by id
(-> +trip-pin-modify+
    (collection "People")
    (id "russellwhyte")
    (get*))

;; See: https://www.odata.org/getting-started/advanced-tutorial/
;; get singleton
(-> +trip-pin-modify+
    (singleton "Me")
    (get*))

;; request property
(-> +trip-pin-modify+
    (singleton "Me")
    (property "AddressInfo")
    (get*))

;; update singleton

;; get collection
(-> +trip-pin-modify+
    (collection "People")
    (get-collection))

;; filtered collection
(-> +trip-pin-modify+
    (collection "People")
    ($filter "FirstName eq 'Scott'")
    (get-collection)
    (first))

(-> +trip-pin-modify+
    (collection "People")
    ($filter '(:= "FirstName" "Scott"))
    (get-collection)
    (first))

(-> +trip-pin-modify+
    (collection "Airports")
    ($filter "contains(Location/Address, 'San Francisco')")
    (get-collection))

(-> +trip-pin-modify+
    (collection "Airports")
    ($filter '(:contains "Location/Address" "San Francisco"))
    (get-collection))

(-> +trip-pin-modify+
    (collection "Airports")
    ($filter '(:contains ("Location" "Address") "San Francisco"))
    (get-collection))

(-> +trip-pin-modify+
    (collection "Airports")
    ($skip 2)
    ($top 3)
    (get-collection))

(odata::with-odata-base +trip-pin-modify+
  (odata::odata-get* "People"))

(odata::with-odata-base +trip-pin-modify+
  (odata::odata-get-entities "People" '|Microsoft.OData.SampleService.Models.TripPin|:person))

(odata::with-odata-base +trip-pin-modify+
  (fetch-people)
  (fetch-airlines))

(odata::with-odata-base +trip-pin-modify+
  (fetch-airports :$filter "contains(Location/Address, 'San Francisco')"))

(odata::with-odata-base +trip-pin-modify+
  (fetch-people :$filter "FirstName eq 'Scott'"))

(odata::with-odata-base +trip-pin-modify+
  (fetch-people :$filter '(:eq "FirstName" "Scott")))

(odata::with-odata-base +trip-pin-modify+
  (fetch-person-by-id "russellwhyte"))

(odata::with-odata-base +trip-pin-modify+
  (fetch-people :$filter '(:eq "FirstName" "Scott")
                :$expand "Address"))

(describe '|Microsoft.OData.SampleService.Models.TripPin|:person)

(odata::with-odata-base +trip-pin-modify+
  (fetch-people :$filter '(:eq "FirstName" "Scott")
                :$expand "Friends"))

(odata::with-odata-base +trip-pin-modify+
  (fetch-people :$filter '(:eq "FirstName" "Scott")
                :$expand (list "Friends" "Trips" "Photo")))

(odata::with-odata-base +trip-pin-modify+
  (fetch-people :$filter '(:eq "FirstName" "Scott")
                :$expand "*"))

(odata::with-odata-base +trip-pin-modify+
  (fetch-people :$filter '(:eq "FirstName" "Scott")
                :$expand :all))

(defparameter *p* (first (odata::with-odata-base +trip-pin-modify+
                           (fetch-people :$filter '(:eq "FirstName" "Scott")))))

(|Microsoft.OData.SampleService.Models.TripPin|::person-trips *p*)
(|Microsoft.OData.SampleService.Models.TripPin|::person-friends *p*)
(|Microsoft.OData.SampleService.Models.TripPin|::person-photo *p*)
