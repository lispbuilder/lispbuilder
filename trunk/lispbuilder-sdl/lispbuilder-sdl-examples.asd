;;; -*- lisp -*-

(defpackage lispbuilder-sdl-examples-system
  (:use #:cl #:asdf))
(in-package #:lispbuilder-sdl-examples-system)

(defsystem lispbuilder-sdl-examples
    :description "Examples for the lispbuilder-sdl package."
    :version "0.1"
    :depends-on (cffi lispbuilder-sdl)
    :components
    ((:module "examples"
	      :components
	      ((:file "package")
	       (:file "sdl_bmp_example")
	       (:file "sdl_pixels")
	       (:file "sdl_recursive_rects")
	       (:file "sdl_drawing")
	       (:file "sdl_random_rects")
	       (:static-file "lisp.bmp")
	       (:static-file "sdl.bmp")))))
