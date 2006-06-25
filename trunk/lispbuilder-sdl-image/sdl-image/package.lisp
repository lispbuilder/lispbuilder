;;; -*- lisp -*-

(in-package #:cl-user)

(defpackage #:lispbuilder-sdl-image
  (:use #:cl #:cffi)
  (:nicknames #:sdl-image)
  (:documentation "The main package of `lispbuilder-sdl-image'.")
  (:export

   ;; util-sdl_image.lisp
   #:image-type
   #:is-image
   #:load-image
   
   ;; sdl_image.lisp
   #:Linked-Version
   #:VERSION
   ))
