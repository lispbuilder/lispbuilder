;;; -*- lisp -*-

(in-package #:cl-user)

(defpackage #:lispbuilder-sdl-mixer-examples
  (:use #:cl #:cffi)
  (:nicknames #:sdl-mixer-examples)
  (:documentation "Examples for `lispbuilder-sdl-mixer'.")
  (:export
   ;; mixer.lisp
   #:mixer
))