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

(deftest fetch-entity-test ()
  (let ((entity
	  (-> +trip-pin-modify+
	      (collection "People")
	      (id "russellwhyte")
	      (fetch))))
    (is (not (null entity)))
    (is (access entity :user-name))
    (is (access entity :first-name))
    (is (access entity :emails))))

(deftest fetch-property-test ()
  (let ((name (-> +trip-pin-modify+
		  (collection "Airports")
		  (id "KSFO")
		  (property "Name")
		  (fetch :value))))
    (is (stringp name))
    (is (string= name "San Francisco International Airport"))))

(deftest $filter-test ()
  (let ((filtered
	  (-> +trip-pin-modify+
	      (collection "People")
	      ($filter "FirstName eq 'Scott'")
	      (fetch :collection))))
    (mapcar (lambda (user)
	      (is (string= (access user :first-name) "Scott")))
	    filtered)))

(deftest compiled-$filter-test ()
  (let ((filtered
	  (-> +trip-pin-modify+
	      (collection "People")
	      ($filter '(:= "FirstName" "Scott"))
	      (fetch :collection))))
    (mapcar (lambda (user)
	      (is (string= (access user :first-name) "Scott")))
	    filtered)))

(deftest complex-filter-test ()
  (let ((filtered
	  (-> +trip-pin-modify+
	      (collection "Airports")
	      ($filter "contains(Location/Address, 'San Francisco')")
	      (fetch :collection))))
    (mapcar (lambda (airport)
	      (is (search "San Francisco" (accesses airport :location :address)
			:test 'string=)))
	    filtered))

  (let ((filtered
	  (-> +trip-pin-modify+
	      (collection "Airports")
	      ($filter '(:contains ("Location" "Address") "San Francisco"))
	      (fetch :collection))))
    (mapcar (lambda (airport)
	      (is (search "San Francisco" (accesses airport :location :address)
			:test 'string=)))
	    filtered))
  )

(deftest order-by-test ()
  (let ((trips
	  (-> +trip-pin-modify+
	      (collection "People")
	      (id "scottketchum")
	      (property "Trips")
	      ($orderby "EndsAt" :desc)
	      (fetch :collection))))
    (is (listp trips))

    ;; Check that the list is ordered
    (labels ((zip-lists (l1 l2)
	       (if (or (null l1)
		       (null l2))
		   (return-from zip-lists nil)
		   (cons (cons l1 l2)
			 (zip-lists (rest l1) (rest l2))))))
      (mapcar (lambda (elem)
		(is (equalp (car elem) (cdr elem))))
	      (zip-lists
	       (mapcar (lambda (x)
			 (alexandria:assoc-value x :trip-id))
		       trips)
	       (mapcar (lambda (x)
			 (alexandria:assoc-value x :trip-id))
		       (sort trips 'local-time:timestamp> :key
			     (lambda (trip)
			       (local-time:parse-timestring (access trip :ends-at)))))))))

  (let ((trips
	  (-> +trip-pin-modify+
	      (collection "People")
	      (id "scottketchum")
	      (property "Trips")
	      ($orderby "EndsAt" :asc)
	      (fetch :collection))))
    (is (listp trips))

    ;; Check that the list is ordered
    (labels ((zip-lists (l1 l2)
	       (if (or (null l1)
		       (null l2))
		   (return-from zip-lists nil)
		   (cons (cons l1 l2)
			 (zip-lists (rest l1) (rest l2))))))
      (mapcar (lambda (elem)
		(is (equalp (car elem) (cdr elem))))
	      (zip-lists
	       (mapcar (lambda (x)
			 (alexandria:assoc-value x :trip-id))
		       trips)
	       (mapcar (lambda (x)
			 (alexandria:assoc-value x :trip-id))
		       (sort trips 'local-time:timestamp< :key
			     (lambda (trip)
			       (local-time:parse-timestring (access trip :ends-at)))))))))

  )

(deftest $filter-syntax-test ()
  (is (string= (odata-client::compile-$filter '(:= "foo" "bar"))
	       "foo eq 'bar'"))
  (is (string= (odata-client::compile-$filter '(:> "foo" "bar"))
	       "foo gt 'bar'"))
  (is (string= (odata-client::compile-$filter '(:and (:= "foo" "bar") (:> "foo" "bar")))
	       "foo eq 'bar' and foo gt 'bar'"))
  (is (string= (odata-client::compile-$filter '(:or (:= "foo" "bar") (:> "foo" "bar")))
	       "foo eq 'bar' or foo gt 'bar'"))
  (is (string= (odata-client::compile-$filter '(:contains ("Location" "Address") "San Francisco" ))
	       "contains(Location/Address, 'San Francisco')"))
  (is (string= (odata-client::compile-$filter '(:not (:contains ("Location" "Address") "San Francisco")))
	       "not contains(Location/Address, 'San Francisco')"
	       )))

(deftest $compile-syntax-test ()
  (is (string= (odata-client::compile-$expand "asdf")
	       "asdf"))
  (is (string= (odata-client::compile-$expand '("asdf"))
	       "asdf"))
  (is (string= (odata-client::compile-$expand '("asdf" "foo"))
	       "asdf,foo"))
  (is (string= (odata-client::compile-$expand '("asdf" "foo" ("Bar" "Baz")))
	       "asdf,foo,Bar/Baz")))

(deftest $select-syntax-test ()
  (is (string= (odata-client::compile-$select "foo,bar")
	       "foo,bar"))
  (is (odata-client::compile-$select '("foo" "bar"))
      "foo,bar"))
