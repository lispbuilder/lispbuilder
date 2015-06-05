;;; -*- lisp -*-

(defpackage #:lispbuilder-sdl-cl-vectors-examples-system
  (:use #:cl #:asdf))
(in-package #:lispbuilder-sdl-cl-vectors-examples-system)

(defsystem lispbuilder-sdl-cl-vectors-examples
    :description "Examples for the LISPBUILDER-SDL-CL-VECTORS package."
    :version "0.0.1"
    :author "Lispbuilder Mailing List <lispbuilder@googlegroups.com>"
    :maintainer "Lispbuilder Mailing List <lispbuilder@googlegroups.com>"
    :licence "MIT"
    :depends-on (lispbuilder-sdl-cl-vectors)
    :components
    ((:module "examples"
	      :components
	      ((:file "package")
	       (:file "cl-vectors" :depends-on ("package"))))))
