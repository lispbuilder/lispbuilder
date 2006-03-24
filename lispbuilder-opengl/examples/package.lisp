;;; -*- lisp -*-

(in-package :common-lisp-user)

(defpackage :lispbuilder-opengl-examples
  (:nicknames :gl-examples)
  (:use :common-lisp :cffi :lispbuilder-opengl)
  (:documentation "Examples for `lispbuilder-opengl'.")
  (:export :opengl-gears))
