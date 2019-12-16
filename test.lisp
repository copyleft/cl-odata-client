(defpackage :odata/test
  (:use :cl :fiveam))

(in-package :odata/test)

(odata::with-odata-base odata.trip-pin::+trip-pin-modify+
  (odata::odata-get-entities "People" '|Microsoft.OData.SampleService.Models.TripPin|:person))

(let ((json:*json-output* *standard-output*))
  (json:with-array ()
    (loop for person in (odata::with-odata-base odata.trip-pin::+trip-pin-modify+
                          (odata::odata-get-entities "People" '|Microsoft.OData.SampleService.Models.TripPin|:person))
       do (json:as-array-member ()
            (odata::serialize person json:*json-output*)))))
