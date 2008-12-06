;;;; lispbuilder-sdl-binaries

(in-package #:cl-user)

(defpackage #:lispbuilder-sdl-binaries
  (:use #:cl)
  (:nicknames #:sdl-bin)
  (:documentation "The main package of `lispbuilder-sdl'.")
  
  (:export

   ;; globals.lisp
   #:*dll-path*))
