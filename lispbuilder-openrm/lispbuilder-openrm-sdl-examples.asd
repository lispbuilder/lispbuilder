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
             (:file "utils" :depends-on ("package"))
             (:file "gl-info-sdl" :depends-on ("globals" "utils"))
             (:file "image-sdl" :depends-on ("globals" "utils"))
             (:file "sprite-test-sdl" :depends-on ("globals" "utils"))
	     (:file "jballs-sdl" :depends-on ("globals" "utils"))
             (:file "clrball-sdl" :depends-on ("globals" "utils"))
	     (:file "cones-sdl" :depends-on ("globals" "utils"))
             (:file "spotlight-sdl" :depends-on ("globals" "utils"))
             (:file "primitives-sdl" :depends-on ("globals" "utils"))
             ;; (:file "particles" :depends-on ("package"))
             ))))
