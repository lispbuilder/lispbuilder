;;;; lispbuilder-sdl-ttf-binaries

(in-package #:cl-user)

(defpackage #:lispbuilder-sdl-ttf-binaries
  (:use #:cl)
  (:nicknames #:sdl-ttf-bin)
  (:documentation "The main package of `lispbuilder-sdl-ttf'.")
  
  (:export

   ;; globals.lisp
   #:*dll-path*))