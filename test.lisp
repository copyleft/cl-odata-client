(defpackage :odata/test
  (:use :cl :fiveam :odata.trip-pin))

(in-package :odata.trip-pin)

(odata::with-odata-base odata.trip-pin::+trip-pin-modify+
  (odata::odata-get-entities "People" '|Microsoft.OData.SampleService.Models.TripPin|:person))

(let ((json:*json-output* *standard-output*))
  (json:with-array ()
    (loop for person in (odata::with-odata-base odata.trip-pin::+trip-pin-modify+
                          (odata::odata-get-entities "People" '|Microsoft.OData.SampleService.Models.TripPin|:person))
       do (json:as-array-member ()
            (odata::serialize person json:*json-output*)))))

(quri:render-uri 
           (quri:make-uri :scheme :http
                          :host "localhost"
                          :path "/lala"
                          :query (quri:url-encode "3 gt 53")))
