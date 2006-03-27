;;; -*- lisp -*-

(in-package #:lispbuilder-opengl)

(cffi:define-foreign-library GL
  (:darwin (:framework "OpenGL"))
  (:windows "OPENGL32.dll")
  (:unix (:or "libGL")))

(cffi:use-foreign-library GL)

