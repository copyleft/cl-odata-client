(defpackage #:odata-client
  (:use #:cl #:cl-arrows #:access)
  (:export
   :*odata-base*
   :*access-token*
   :odata-request-error
   :odata-get
   :odata-get*
   :odata-post
   :odata-patch
   :with-odata-base
           
           ))
