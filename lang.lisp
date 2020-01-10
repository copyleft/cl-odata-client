(in-package :odata)

(defun singleton (url name)
  (format nil "~a~a" url (if (stringp name) name
                             (lisp-to-camel-case (string name)))))

(defun get* (url)
  (odata-get url))

(defun property (url name)
  (format nil "~a/~a" url (if (stringp name) name
                              (lisp-to-camel-case (string name)))))

(defun collection (url name)
  (format nil "~a/~a" url (if (stringp name) name
                              (lisp-to-camel-case (string name)))))

(defun get-collection (url)
  (access (get* url) :value))
