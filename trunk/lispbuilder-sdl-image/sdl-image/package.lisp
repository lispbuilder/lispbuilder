;;; -*- lisp -*-

(in-package #:cl-user)

(defpackage #:lispbuilder-sdl
  (:use #:cl #:cffi)
  (:nicknames #:sdl)
  (:documentation "The methods defined here extend any methods already defined in `lispbuilder-sdl'.")
  (:export
   ;; sdl-image-util.lisp
   #:image-p
   #:image-type-of
   #:load-image
   #:load-and-convert-image))

(defpackage #:lispbuilder-sdl-image
  (:use #:cl #:cffi)
  (:nicknames #:sdl-image)
  (:documentation "The methods defined here extend any methods already defined in `lispbuilder-sdl'.")
  (:import-from #:lispbuilder-sdl
		lispbuilder-sdl:image-p
		lispbuilder-sdl:image-type-of
		lispbuilder-sdl:load-image
		lispbuilder-sdl:load-and-convert-image)
  (:export
   #:image-p
   #:image-type-of
   #:load-image
   #:load-and-convert-image))
