(in-package #:cl-user)

(defpackage #:lispbuilder-net-examples
  (:use #:cl #:cffi #:net-cffi)
  (:nicknames #:net-examples)
  (:documentation "Examples for `lispbuilder-net'.")
  (:export
   #:webserver
   #:wget))
