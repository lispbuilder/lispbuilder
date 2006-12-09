;;; -*- lisp -*-

(in-package #:cl-user)

(defpackage #:lispbuilder-sdl-wrapper
  (:use #:cl #:cffi)
  (:nicknames #:sdl-wrapper #:swl)
  (:documentation "The basic wrapper package of `lispbuilder-sdl'."))