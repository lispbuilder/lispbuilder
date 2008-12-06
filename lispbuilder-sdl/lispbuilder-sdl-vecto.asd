;;; -*- lisp -*-

(defpackage #:lispbuilder-sdl-vecto-system
  (:use #:cl #:asdf))
(in-package #:lispbuilder-sdl-vecto-system)

(defsystem lispbuilder-sdl-vecto
  :description "VECTO v1.0.2 glue for LISPBUILDER-SDL"
  :version "0.0.1"
  :depends-on (lispbuilder-sdl-cl-vectors vecto lispbuilder-sdl)
  :components
  ((:module "glue-vecto"
	    :components
	    ((:file "package" :depends-on ("glue-vecto"))
	     (:file "glue-vecto")))))
