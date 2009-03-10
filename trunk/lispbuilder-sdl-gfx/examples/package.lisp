;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-

(in-package #:cl-user)

(defpackage #:lispbuilder-sdl-gfx-examples
  (:use #:cl #:cffi)
  (:nicknames #:sdl-gfx-examples)
  (:export #:*bmp-path*
           #:inbuilt-font
           #:random-circles
           #:setup-and-draw
           #:width-height
           #:recursion
           #:functions
           #:objects
           #:points-and-lines
           #:shape-primitives
           #:bezier
           #:distance-2d
           #:vertices
           #:metaballs
           #:rotozoom
           #:zoom
           #:fireworks))
