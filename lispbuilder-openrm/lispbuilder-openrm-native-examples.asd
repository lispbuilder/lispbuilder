;;; -*- lisp -*-

(defpackage #:lispbuilder-openrm-native-examples-system
  (:use #:cl #:asdf))
(in-package #:lispbuilder-openrm-native-examples-system)

(defsystem lispbuilder-openrm-native-examples
  :description "Examples for the lispbuilder-openrm-native package."
  :version "0.1"
  :depends-on (cffi lispbuilder-openrm-native)
  :components
  ((:module "examples"
	    :components
	    ((:file "package")
	     (:file "globals" :depends-on ("package"))
	     (:file "jballs" :depends-on ("package"))
	     (:file "spotlight" :depends-on ("package"))
	     (:file "clrball" :depends-on ("package"))
	     (:file "cones" :depends-on ("package"))))))
