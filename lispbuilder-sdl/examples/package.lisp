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
   #:simple-font-demo

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

   ;; stroke.lisp
   #:stroke
   
   ;; squashed.lisp
   #:squashed))
