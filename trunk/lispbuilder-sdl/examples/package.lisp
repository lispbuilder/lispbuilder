;;; -*- lisp -*-

(in-package :common-lisp-user)

(defpackage :lispbuilder-sdl-examples
  (:nicknames :sdl-examples)
  (:use :common-lisp :cffi :lispbuilder-sdl)
  (:documentation "Examples for `lispbuilder-sdl'.")
  (:export :bmp_sample)
  (:export :pixels)
  (:export :recursive-rects)
  (:export :mouse-painter)
  (:export :random-rects1)
  (:export :random-rects2)
  (:export :random-rects3))

