(defpackage #:odata-client
  (:use #:cl #:arrows #:access)
  (:export
   :*odata-base*
   :*access-token*
   :odata-request-error
   :odata-get
   :odata-get*
   :odata-post
   :odata-patch
   :odata-put
   :with-odata-base)
  (:documentation "Provides core functions for interacting with an ODATA service."))
