;; SDL (Simple Media Layer) library using CFFI for foreign function interfacing...
;; (C)2006 Justin Heyes-Jones <justinhj@gmail.com> and Luke Crook <luke@balooga.com>
;; Thanks to Frank Buss and Surendra Singh
;; see COPYING for license
;; This file contains some useful functions for using SDL from Common lisp
;; using sdl.lisp (the CFFI wrapper)

(in-package #:lispbuilder-sdl)


(defun random+1 (rnd)
  (+ 1 (random rnd)))

(defun to-radian (degree)
  "converts degrees to radians."
  (* degree (/ PI 180)))

(defun to-degree (radian)
  "converts radians to degrees."
  (/ radian (/ PI 180)))

(defun distance (x1 y1 x2 y2)
  (sqrt (+ (expt (- x1 x2) 2) 
	   (expt (- y1 y2) 2))))

;; (defun points-in-range (p1 p2 distance)
;;   "return true, if the distance between p1 and p2 is not more than 'distance'"
;;   (<= (+ (expt (- (sdl:point-x p1) (sdl:point-x p2)) 2)
;;          (expt (- (sdl:point-y p1) (sdl:point-y p2)) 2))
;;       (expt distance 2)))

;; (defun random-rect (bound-w bound-h)
;;   (let* ((x (random bound-w))
;; 	 (y (random bound-h))
;; 	 (w (random+1 (- bound-w x)))
;; 	 (h (random+1 (- bound-h y))))
;;     (rectangle x y w h)))

;; (defun random-color (r g b &optional alpha)
;;   (if alpha ;; alpha is either t, or a number then create r/g/b/a
;;       (color (random r) (random g) (random b) (if (numberp alpha)
;; 						  alpha
;; 						  (random 255)))
;;       (color (random r) (random g) (random b)))) ; Or not, and create an r/g/b color

;; (defun random-point (max-x max-y)
;;   (sdl:point (random max-x) (random max-y)))
