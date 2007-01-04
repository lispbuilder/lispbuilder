;;; -*- lisp -*-

(defpackage #:lispbuilder-sdl-examples-system
  (:use #:cl #:asdf))
(in-package #:lispbuilder-sdl-examples-system)

(defsystem lispbuilder-sdl-examples
    :description "Examples for the lispbuilder-sdl package."
    :version "0.8.0"
    :depends-on (cffi lispbuilder-sdl)
    :components
    ((:module "examples"
	      :components
	      ((:file "package")
	       (:file "globals" :depends-on ("package"))
	       (:file "sdl-simple-font-demo" :depends-on ("package"))
	       (:file "sdl_bmp_example" :depends-on ("package"))
	       (:file "sdl_pixels" :depends-on ("package"))
	       (:file "sdl_recursive_rects" :depends-on ("package"))
	       (:file "sdl_drawing" :depends-on ("package"))
	       (:file "sdl_random_rects" :depends-on ("package"))
	       (:file "mandelbrot" :depends-on ("package"))
	       (:file "line-drawing" :depends-on ("package"))
	       (:file "vertices" :depends-on ("package"))
	       (:file "bezier" :depends-on ("package"))

	       (:file "setup-and-draw" :depends-on ("package"))
	       (:file "width-height" :depends-on ("package"))
	       (:file "objects" :depends-on ("package"))
	       (:file "points-and-lines" :depends-on ("package"))
	       (:file "distance-2D" :depends-on ("package"))
;	       (:file "metaballs" :depends-on ("package"))

	       (:static-file "lisp.bmp")
	       (:static-file "sdl.bmp")
 	       (:module "squashed"
 			:components
			((:file "squashed")
			 (:static-file "blood.bmp")
			 (:static-file "bug.bmp")
			 (:static-file "racket.bmp")
			 (:static-file "squash.bmp"))))
	      :serial t ;; Make sure we load "examples" before "squashed"
	      )))
