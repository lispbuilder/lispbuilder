;;; -*- lisp -*-

(in-package #:cl-user)

(defpackage #:lispbuilder-sdl-examples
  (:use #:cl #:cffi)
  (:nicknames #:sdl-examples)
  (:documentation "Examples for `lispbuilder-sdl'.")
  (:export
   ;; bezier.lisp
   #:bezier

   ;; bmp-sample.lisp
   #:bmp-sample

   ;; distance-2D.lisp
   #:distance-2d

   ;; flood-fill.lisp
   #:flood-fill
   #:flood-fill-timed
   #:flood-fill-stack
   #:flood-fill-stack-timed

   ;; input-util.lisp
   #:sdl-input

   ;; line-drawing.lisp
   #:line-drawing

   ;; mandelbrot.lisp
   #:mandelbrot

   ;; metaballs.lisp
   #:metaballs

   ;; mouse-painter.lisp
   #:mouse-painter

   ;; objects.lisp
   #:objects

   ;; pixels.lisp
   #:pixels-1
   #:pixels-2
   #:pixels-3
   #:pixels-4

   ;; points-and-lines.lisp
   #:points-and-lines

   ;; random-rects.lisp
   #:random-rects
   #:random-box-1
   #:random-box-2
   #:random-box-3
   #:random-box-4

   ;; recursive-rects.lisp
   #:recursive-rects

   ;; setup-and-draw.lisp
   #:setup-and-draw

   ;; simple-font-demo.lisp
   #:simple-font-demo-1
   #:simple-font-demo-2

   ;; inbuilt-font.lisp
   #:inbuilt-fonts

   ;; vertices.lisp
   #:vertices

   ;; width-height.lisp
   #:width-height

   ;; circle.lisp
   #:circle-1
   #:circle-2
   #:circle-3
   #:circle-4
   #:circle-5
   
   ;; stroke.lisp
   #:stroke

   ;; mouse-2d.lisp
   #:mouse-2d
   #:mouse-surface-2d
   
   ;; squashed.lisp
   #:squashed

   ;; joystick.lisp
   #:joystick

   ;; particles.lisp
   #:particles

   ;; fireworks.lisp
   #:fireworks

   ;; raw-audio-test
   #:raw-audio-test

   ;; functional-geometry
   #:fishes

   ;; audio-mixer
   #:mixer-test

   ;; The following are only defined in the
   ;; LISPBUILDER-SDL-CL-VECTORS-EXAMPLES package.
   ;; cl-vectors.lisp
   #:cl-vectors-1
   #:cl-vectors-2
   #:cl-vectors-3
   #:cl-vectors-4

   ;; The following are only defined in the
   ;; LISPBUILDER-SDL-VECTO-EXAMPLES package.
   ;; vecto.lisp
   #:vecto-test
   #:radiant-lambda
   #:feedlike-icon
   #:star-clipping
   #:roots
   ))
