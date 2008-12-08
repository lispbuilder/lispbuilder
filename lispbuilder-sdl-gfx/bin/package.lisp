;;;; lispbuilder-sdl-gfx-binaries

(in-package #:cl-user)

(defpackage #:lispbuilder-sdl-gfx-binaries
  (:use #:cl)
  (:nicknames #:sdl-gfx-bin)
  (:documentation "The main package of `lispbuilder-sdl-gfx'.")
  
  (:export

   ;; globals.lisp
   #:*dll-path*))