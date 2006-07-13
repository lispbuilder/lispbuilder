;;; -*- lisp -*-

(defpackage #:lispbuilder-sdl-examples-system
  (:use #:cl #:asdf))
(in-package #:lispbuilder-sdl-examples-system)

(defsystem lispbuilder-sdl-examples
    :description "Examples for the lispbuilder-sdl package."
    :version "0.7.1"
    :depends-on (cffi lispbuilder-sdl)
    :components
    ((:module "examples"
	      :components
	      ((:file "package")
	       (:file "sdl_simplefont" :depends-on ("package"))
	       (:file "sdl_simplefontdemo" :depends-on ("package"))
	       (:file "sdl_bmp_example" :depends-on ("package"))
	       (:file "sdl_pixels" :depends-on ("package"))
	       (:file "sdl_recursive_rects" :depends-on ("package"))
	       (:file "sdl_drawing" :depends-on ("package"))
	       (:file "sdl_random_rects" :depends-on ("package"))
	       (:file "mandelbrot" :depends-on ("package"))
	       (:file "line-drawing" :depends-on ("package"))
	       (:static-file "font.bmp")
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
