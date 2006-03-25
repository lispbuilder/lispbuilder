;;; -*- lisp -*-

(in-package #:cl-user)

(defpackage #:lispbuilder-opengl-examples
  (:nicknames :gl-examples)
  (:use #:cl #:cffi)
  (:documentation "Examples for `lispbuilder-opengl'.")
  (:export #:opengl-gears))
