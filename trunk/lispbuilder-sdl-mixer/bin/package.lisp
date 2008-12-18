;;;; lispbuilder-sdl-mixer-binaries

(in-package #:cl-user)

(defpackage #:lispbuilder-sdl-mixer-binaries
  (:use #:cl)
  (:nicknames #:sdl-mixer-bin)
  (:documentation "The main package of `lispbuilder-sdl-mixer'.")
  
  (:export

   ;; globals.lisp
   #:*dll-path*))
