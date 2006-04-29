;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-

(in-package #:cl-user)

(defpackage #:lispbuilder-sdl-gfx-examples
  (:use #:cl #:cffi)
  (:nicknames #:sdl-gfx-examples)
  (:export #:inbuilt-font :export #:random-circles))