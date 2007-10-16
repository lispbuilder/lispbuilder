;;; -*- lisp -*-

(defpackage #:lispbuilder-cal3d-examples-system
  (:use #:cl #:asdf))
(in-package #:lispbuilder-cal3d-examples-system)

(defsystem lispbuilder-cal3d-examples
  :description "Examples for the lispbuilder-cal3d package."
  :version "0.1"
  :depends-on (cffi lispbuilder-sdl lispbuilder-cal3d lispbuilder-openrm)
  :components
  ((:module "examples"
	    :components
	    ((:file "package")
	     (:file "cal3d_demo" :depends-on ("package"))
	     ))))
