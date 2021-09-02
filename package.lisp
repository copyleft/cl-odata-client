(defpackage :odata-client
  (:use :cl :arrows :access)
  (:export
   :*service-root*
   :*access-token*
   :odata-request-error
   :odata-get
   :odata-get*
   :odata-post
   :odata-patch
   :odata-put
   :with-service-root)
  (:documentation "Provides core functions for interacting with an ODATA service."))

(defpackage :odata/lang
  (:use :cl :access)
  (:export
   :singleton
   :fetch
   :post
   :create
   :del
   :update
   :patch
   :link
   :path
   :update-link
   :property
   :collection
   :fcall
   :$filter
   :$expand
   :$count
   :id
   :$skip
   :$top
   :$value
   :$orderby
   :$select
   :$search
   :$ref)
  (:documentation
   "This package exports functions that are meant to be used with arrows syntax to interact with an ODATA service."))

(defpackage :odata-client-user 
  (:use :cl :odata-client :odata/lang :arrows))
