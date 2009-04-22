;;; -*- lisp -*-

(defpackage #:lispbuilder-openrm-sdl-examples-system
  (:use #:cl #:asdf))
(in-package #:lispbuilder-openrm-sdl-examples-system)

(defsystem lispbuilder-openrm-sdl-examples
  :description "Examples for the lispbuilder-openrm-sdl package."
  :version "0.1"
  :depends-on (cffi lispbuilder-openrm-sdl)
  :components
  ((:module "examples"
	    :components
	    ((:file "package")
	     (:file "globals" :depends-on ("package"))
             (:file "gl-info-sdl" :depends-on ("package"))
             (:file "image-sdl" :depends-on ("package"))
             (:file "sprite-test-sdl" :depends-on ("package"))
	     (:file "jballs-sdl" :depends-on ("package"))
             (:file "clrball-sdl" :depends-on ("package"))
	     (:file "cones-sdl" :depends-on ("package"))
             (:file "spotlight-sdl" :depends-on ("package"))
             (:file "primitives-sdl" :depends-on ("package"))
             ;; (:file "particles" :depends-on ("package"))
             ))))
