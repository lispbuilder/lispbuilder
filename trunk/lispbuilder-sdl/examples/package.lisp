;;; -*- lisp -*-

(in-package #:cl-user)

(defpackage #:lispbuilder-sdl-examples
  (:use #:cl #:cffi)
  (:nicknames #:sdl-examples)
  (:documentation "Examples for `lispbuilder-sdl'.")
  (:export #:bmp_sample #:pixels #:recursive-rects #:mouse-painter #:random-rects1
	   #:random-rects2 #:random-rects3))

