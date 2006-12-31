;; SDL (Simple Media Layer) library using CFFI for foreign function interfacing...
;; (C)2006 Justin Heyes-Jones <justinhj@gmail.com> and Luke Crook <luke@balooga.com>
;; Thanks to Frank Buss and Surendra Singh
;; see COPYING for license
;; This file contains some useful functions for using SDL from Common lisp
;; using sdl.lisp (the CFFI wrapper)

(in-package #:lispbuilder-sdl)

(defun points-in-range (p1 p2 distance)
  "Returns true T, if the distance between the points p1 POINT and p2 POINT is <= DISTANCE"
  (<= distance (distance-to-point p1 p2)))

(defun distance-to-point (p1 p2)
  "Returns the distance between the points p1 POINT and p2 POINT."
  (sqrt (+ (expt (- (x p1) (x p2)) 2)
	   (expt (- (y p1) (y p2)) 2))))

;; (defun random-point (max-x max-y)
;;   (sdl:point (random max-x) (random max-y)))

;; (defun moveby-rectangle (&key (rectangle *default-rectangle*) (position *default-position*))
;;   (setf (rect-x rectangle) (+ (rect-x rectangle) (pos-x position))
;; 	(rect-y rectangle) (+ (rect-y rectangle) (pos-y position)))
;;   rectangle)

;; (defun moveto-rectangle (&key (rectangle *default-rectangle*) (position *default-position*))
;;   (setf (rect-x rectangle) (pos-x position)
;; 	(rect-y rectangle) (pos-y position))
;;   rectangle)
