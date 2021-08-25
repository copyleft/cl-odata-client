(defpackage :odata-client-tests
  (:use :cl :stefil :odata-client :odata/lang :arrows :access)
  (:export :odata-client-tests))

(in-package :odata-client-tests)

(defparameter +trip-pin-base+ "https://services.odata.org/V4/TripPinServiceRW")

(defparameter +trip-pin-modify+
  (multiple-value-bind (response status headers modify-url)
      (drakma:http-request +trip-pin-base+)
    (quri:uri (princ-to-string modify-url))))


(stefil:in-root-suite)
(stefil:defsuite* odata-client-tests)

(stefil:in-suite odata-client-tests)

(deftest fetch-collection-test ()
  (let ((results
	  (-> +trip-pin-modify+
	      (collection "People")
	      (fetch :collection))))
    (is (listp results))
    (mapcar (lambda (item)
	      (is (access item :user-name)))
    results)))
