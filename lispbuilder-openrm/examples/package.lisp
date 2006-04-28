;;; -*- lisp -*-

(in-package #:cl-user)

(defpackage #:lispbuilder-openrm-examples
  (:use #:cl #:cffi)
  (:nicknames #:rm-examples)
  (:documentation "Examples for `lispbuilder-openrm'.")
  (:export #:jballs
	   #:spotlight
	   #:rm-example-1
	   #:rm-example-2))
