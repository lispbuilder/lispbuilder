;;; -*- lisp -*-

(in-package #:cl-user)

(defpackage #:lispbuilder-sdl-image
  (:use #:cl #:cffi)
  (:nicknames #:sdl-image)
  (:documentation "The main package of `lispbuilder-sdl-image'.")
  (:export

   ;; util-sdl_image.lisp
   #:create-image-from-RWops
   #:image-type
   #:is-image
   #:is-image-from-RWops
   #:load-image))
