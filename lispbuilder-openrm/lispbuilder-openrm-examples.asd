;;; -*- lisp -*-

(defpackage #:lispbuilder-openrm-examples-system
  (:use #:cl #:asdf))
(in-package #:lispbuilder-openrm-examples-system)

(defsystem lispbuilder-openrm-examples
  :description "Examples for the lispbuilder-openrm package."
  :version "0.1"
  :depends-on (cffi lispbuilder-sdl lispbuilder-openrm)
  :components
  ((:module "examples"
	    :components
	    ((:file "package")
	     (:file "rm_example_1" :depends-on ("package"))
	     (:file "jballs" :depends-on ("package"))
	     (:file "spotlight" :depends-on ("package"))))))
