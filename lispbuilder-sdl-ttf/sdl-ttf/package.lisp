;;; -*- lisp -*-

(in-package #:cl-user)

(defpackage #:lispbuilder-sdl-ttf
  (:use #:cl #:cffi)
  (:nicknames #:sdl-ttf)
  (:documentation "The main package of `lispbuilder-gfx-ttf'.")
  (:export #:with-init
	   #:open-font
	   #:close-font
	   #:make-text-surface))
