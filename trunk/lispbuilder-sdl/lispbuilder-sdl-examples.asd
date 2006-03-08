;;; -*- lisp -*-

(defpackage :lispbuilder-sdl-examples-system
  (:use :common-lisp :asdf :cffi))
(in-package :lispbuilder-sdl-examples-system)

(defsystem lispbuilder-sdl-examples
    :description "Examples for the lispbuilder-sdl package."
    :depends-on (cffi lispbuilder-sdl)
    :components
    ((:module "examples"
	      :components
	      ((:file "package")
;	       (:file "sdl_bmp_example")
	       (:file "sdl_pixels")
	       (:file "sdl_recursive_rects")
	       (:file "sdl_drawing")
	       (:file "sdl_random_rects")
	       (:static-file "sdl1.png")
	       (:static-file "lisp.bmp")
	       (:static-file "sdl.bmp")))))
