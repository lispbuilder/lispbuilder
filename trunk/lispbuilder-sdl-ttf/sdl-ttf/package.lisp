;;; -*- lisp -*-

(in-package #:cl-user)

(defpackage #:lispbuilder-sdl-ttf
  (:use #:cl #:cffi)
  (:nicknames #:sdl-ttf)
  (:documentation "The main package of `lispbuilder-sdl-ttf'.")
  (:export
   
   ;; globals.lisp
   #:*generation*
   
   ;; font.lisp
   #:ttf-font

   ;; sdl-util-ttf.lisp
   #:is-init
   #:with-open-font
   #:init-ttf
   #:quit-ttf
   #:initialise-font
   #:initialise-default-font
   #:close-font
   #:open-font
   ))
