;;; -*- lisp -*-

(in-package #:cl-user)

(defpackage #:lispbuilder-sdl-cffi
  (:use #:cl #:cffi)
  (:nicknames #:sdl-cffi #:scl)
  (:documentation "The basic wrapper package of `lispbuilder-sdl'."))