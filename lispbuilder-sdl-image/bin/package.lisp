;;;; lispbuilder-sdl-image-binaries

(in-package #:cl-user)

(defpackage #:lispbuilder-sdl-image-binaries
  (:use #:cl)
  (:nicknames #:sdl-image-bin)
  (:documentation "The main package of `lispbuilder-sdl-image'.")
  
  (:export

   ;; globals.lisp
   #:*dll-path*))