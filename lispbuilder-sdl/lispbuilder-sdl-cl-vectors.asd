;;; -*- lisp -*-

(defpackage #:lispbuilder-sdl-cl-vectors-system
  (:use #:cl #:asdf))
(in-package #:lispbuilder-sdl-cl-vectors-system)

(defsystem lispbuilder-sdl-cl-vectors
    :description "CL-VECTORS v0.1.3 glue for LISPBUILDER-SDL"
    :version "0.0.1"
    :depends-on (cffi lispbuilder-sdl cl-aa-misc cl-vectors cl-paths-ttf)
    :components
    ((:module "cl-vectors"
	      :components
	      ((:file "package" :depends-on ("cl-vectors"))
	       (:file "cl-vectors")))))
