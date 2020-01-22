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

;; Load service spec from url
#+nil(defparameter +trip-pin-service-spec+
       (json:decode-json-from-source (drakma:http-request +trip-pin-base+)))

;; Load service spec from file
(defparameter +trip-pin-service-spec+
  (json:decode-json-from-source (asdf:system-relative-pathname :odata "TripPin.json")))

(setf drakma:*header-stream* *standard-output*)

(odata::def-packages #.+trip-pin-metadata+)

(odata::def-enums #.+trip-pin-metadata+)
(odata::def-entities #.+trip-pin-metadata+)
;; (odata::def-functions #.+trip-pin-metadata+)

(odata::def-service-model-functions
    #.+trip-pin-service-spec+
    #.+trip-pin-metadata+)

;; See: https://www.odata.org/getting-started/basic-tutorial/

(-> +trip-pin-modify+
    (collection "People")
    (fetch :collection))

(-> +trip-pin-modify+
    (collection "People")
    (fetch '(:collection-of |Microsoft.OData.SampleService.Models.TripPin|:person)))

;; get by id
(-> +trip-pin-modify+
    (collection "People")
    (id "russellwhyte")
    (fetch))

(-> +trip-pin-modify+
    (collection "People")
    (id "russellwhyte")
    (fetch '|Microsoft.OData.SampleService.Models.TripPin|:person))

;; See: https://www.odata.org/getting-started/advanced-tutorial/
;; get singleton
(-> +trip-pin-modify+
    (singleton "Me")
    (fetch))

;; enums

(-> +trip-pin-modify+
    (collection "People")
    ($filter `(:eq "Gender" ,+person-gender/female+))
    (fetch :collection))

;; request property
(-> +trip-pin-modify+
    (singleton "Me")
    (property "AddressInfo")
    (fetch :value))

(-> +trip-pin-modify+
    (collection "Airports")
    (id "KSFO")
    (property "Name")
    (fetch :value))

(-> +trip-pin-modify+
    (collection "Airports")
    (id "KSFO")
    (property "Name")
    ($value)
    (fetch))

;; update singleton

;; get collection
(-> +trip-pin-modify+
    (collection "People")
    (fetch :collection))

;; filtered collection
(-> +trip-pin-modify+
    (collection "People")
    ($filter "FirstName eq 'Scott'")
    (fetch :collection))

(-> +trip-pin-modify+
    (collection "People")
    ($filter '(:= "FirstName" "Scott"))
    (fetch :collection))

(-> +trip-pin-modify+
    (collection "Airports")
    ($filter "contains(Location/Address, 'San Francisco')")
    (fetch :collection))

(-> +trip-pin-modify+
    (collection "Airports")
    ($filter '(:contains "Location/Address" "San Francisco"))
    (fetch :collection))

(-> +trip-pin-modify+
    (collection "Airports")
    ($filter '(:contains ("Location" "Address") "San Francisco"))
    (fetch :collection))

(-> +trip-pin-modify+
    (collection "Airports")
    ($skip 2)
    ($top 3)
    (fetch :collection))

;; expand
(-> +trip-pin-modify+
    (collection "People")
    ($filter '(:= "FirstName" "Scott"))
    ($expand '("Photo" "Trips"))
    (fetch :collection)
    (first))

(-> +trip-pin-modify+
    (collection "People")
    ($filter '(:= "FirstName" "Scott"))
    ($expand :all)
    (fetch :collection)
    (first))

;; orderby
(-> +trip-pin-modify+
    (collection "People")
    (id "scottketchum")
    (property "Trips")
    ($orderby "EndsAt" :desc)
    (fetch :collection))

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
