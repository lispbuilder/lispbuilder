;;; -*- lisp -*-

(in-package #:cl-user)

(defpackage #:lispbuilder-sdl-examples
  (:use #:cl #:cffi)
  (:nicknames #:sdl-examples)
  (:documentation "Examples for `lispbuilder-sdl'.")
  (:export #:bmp-sample #:pixels #:recursive-rects #:mouse-painter #:random-rects1
	   #:random-rects2 #:random-rects3 #:simple-font-demo #:squashed #:mandelbrot #:line-drawing

	   #:distance-2d #:width-height #:setup-and-draw #:objects #:points-and-lines #:bezier #:vertices
	   #:metaballs))

