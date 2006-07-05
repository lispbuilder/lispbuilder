;;;; Demonstration/Test of using SDL (Simple Media Layer) library
;;;; using CFFI for foreign function interfacing...
;;;; (C)2006 Frank Busse
;;;; see COPYING for license

;;;; From "http://www.frank-buss.de/lisp/canvas.html"

(in-package #:sdl-examples) 

(defvar *x0* 0.2)
(defvar *y0* 0.5)
(defvar *x1* 0.4)
(defvar *y1* 0.7)
(defvar *width* 300)
(defvar *height* 300)

(defun mandelbrot (&optional (width *width*) (height *height*) (x0 *x0*) (y0 *y0*) (x1 *x1*) (y1 *y1*))
  (sdl:with-init ()
    (sdl:with-display (width height :title-caption "Mandelbrot" :icon-caption "Mandelbrot")
      (loop for y from 0 below height do
	    (loop for x from 0 below width do
		  (loop with a = (complex (float (+ (* (/ (- x1 x0) width) x) x0))
					  (float (+ (* (/ (- y1 y0) height) y) y0)))
			for z = a then (+ (* z z) a)
			while (< (abs z) 2)
			for c from 60 above 0
			finally (sdl:draw-pixel :position (sdl:point x y)
						:color (sdl:color (mod (* 13 c) 256)
								  (mod (* 7 c) 256)
								  (mod (* 2 c) 256))
						:update-p t))))
      (sdl:with-events
	(:quit t)
	(:videoexpose (sdl:update-screen))))))
