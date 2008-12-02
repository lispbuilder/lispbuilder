;;; -*- lisp -*-

(in-package #:cl-user)

(defpackage #:lispbuilder-sdl-ttf
  (:use #:cl #:cffi)
  (:nicknames #:sdl-ttf)
  (:documentation "The main package of `lispbuilder-sdl-ttf'.")
  (:export
   
   ;; globals.lisp
   #:*generation*
     
   ;; sdl-util-ttf.lisp
   #:is-init
   #:init-ttf
   #:quit-ttf
   #:close-font
   #:open-font))
