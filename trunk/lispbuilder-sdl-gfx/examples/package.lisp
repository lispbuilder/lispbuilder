;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-

(in-package #:cl-user)

(defpackage #:lispbuilder-sdl-gfx-examples
  (:nicknames :sdl-gfx-examples)
  (:use #:cl #:cffi)
  (:export #:inbuilt-font :export #:random-circles))