;;; -*- lisp -*-

(in-package #:lispbuilder-opengl)

(cffi:define-foreign-library GL
   (:darwin (:framework "OpenGL"))
   (:windows "OPENGL32.dll")
   (:unix (:or "libGL" "libGL.so.2" "libGL.so.1")))

(cffi:use-foreign-library GL)

