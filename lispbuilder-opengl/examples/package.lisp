;;; -*- lisp -*-

(in-package #:cl-user)

(defpackage #:lispbuilder-opengl-examples
  (:use #:cl #:cffi)
  (:nicknames #:gl-examples)
  (:documentation "Examples for `lispbuilder-opengl'.")
  (:export #:opengl-gears))
