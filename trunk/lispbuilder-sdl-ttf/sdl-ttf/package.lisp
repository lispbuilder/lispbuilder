;;; -*- lisp -*-

(in-package #:cl-user)

(defpackage #:lispbuilder-sdl-ttf
  (:use #:cl #:cffi)
  (:nicknames #:sdl-ttf)
  (:documentation "The main package of `lispbuilder-sdl-ttf'.")
  (:import-from #:lispbuilder-sdl-ttf-cffi
   lispbuilder-sdl-cffi::load-library)
  (:export

   ;; globals.lisp
   #:*generation*
     
   ;; sdl-util-ttf.lisp
   #:ttf-init-p
   #:init-ttf
   #:quit-ttf

   #:load-library))
