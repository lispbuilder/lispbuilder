;;; -*- lisp -*-

(defpackage #:lispbuilder-sdl-cl-vectors-system
  (:use #:cl #:asdf))
(in-package #:lispbuilder-sdl-cl-vectors-system)

(defsystem lispbuilder-sdl-cl-vectors
    :description "CL-VECTORS v0.1.3 glue for LISPBUILDER-SDL"
    :version "0.0.1"
    :depends-on (cl-paths-ttf cl-aa-misc cl-vectors zpb-ttf lispbuilder-sdl)
    :components
    ((:module "glue-cl-vectors"
	      :components
	      ((:file "package" :depends-on ("glue-cl-vectors"))
	       (:file "glue-cl-vectors")))))
