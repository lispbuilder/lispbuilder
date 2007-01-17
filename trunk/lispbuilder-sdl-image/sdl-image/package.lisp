;;; -*- lisp -*-

(in-package #:cl-user)

(defpackage #:lispbuilder-sdl-image
  (:use #:cl #:cffi)
  (:nicknames #:sdl-image)
  (:documentation "The main package of `lispbuilder-sdl-image'.")
  (:export

   ;; sdl-image-util.lisp
   #:create-image-from-RWops
   #:rwops-p
   #:rwops-type-of
   #:image-p
   #:image-type-of
   #:load-image))
