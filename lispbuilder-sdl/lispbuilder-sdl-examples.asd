;;; -*- lisp -*-

(defpackage #:lispbuilder-sdl-examples-system
  (:use #:cl #:asdf))
(in-package #:lispbuilder-sdl-examples-system)

(defsystem lispbuilder-sdl-examples
    :description "Examples for the lispbuilder-sdl package."
    :version "0.8.1"
    :depends-on (cffi lispbuilder-sdl)
    :components
    ((:module "examples"
	      :components
	      ((:file "package")
	       (:file "globals" :depends-on ("package"))
	       (:file "simple-font-demo" :depends-on ("package"))
	       (:file "bmp-sample" :depends-on ("package"))
	       (:file "flood-fill" :depends-on ("package"))
	       (:file "pixels" :depends-on ("package"))
	       (:file "recursive-rects" :depends-on ("package"))
	       (:file "mouse-painter" :depends-on ("package"))
	       (:file "random-rects" :depends-on ("package"))
	       (:file "mandelbrot" :depends-on ("package"))
	       (:file "line-drawing" :depends-on ("package"))
	       (:file "vertices" :depends-on ("package"))
	       (:file "bezier" :depends-on ("package"))
	       (:file "setup-and-draw" :depends-on ("package"))
	       (:file "width-height" :depends-on ("package"))
	       (:file "objects" :depends-on ("package"))
	       (:file "points-and-lines" :depends-on ("package"))
	       (:file "distance-2D" :depends-on ("package"))
	       (:file "metaballs" :depends-on ("package"))
	       (:file "inbuilt-fonts" :depends-on ("package"))
	       (:file "circle" :depends-on ("package"))
	       (:file "stroke" :depends-on ("package"))
	       (:file "mouse-2d" :depends-on ("package"))

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
