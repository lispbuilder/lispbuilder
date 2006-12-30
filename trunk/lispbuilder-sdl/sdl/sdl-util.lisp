;; SDL (Simple Media Layer) library using CFFI for foreign function interfacing...
;; (C)2006 Justin Heyes-Jones <justinhj@gmail.com> and Luke Crook <luke@balooga.com>
;; Thanks to Frank Buss and Surendra Singh
;; see COPYING for license
;; This file contains some useful functions for using SDL from Common lisp
;; using sdl.lisp (the CFFI wrapper)

(in-package #:lispbuilder-sdl)

(defun points-in-range (p1 p2 distance)
  "return true, if the distance between p1 and p2 is not more than 'distance'"
  (<= (+ (expt (- (x p1) (x p2)) 2)
         (expt (- (y p1) (y p2)) 2))
      (expt distance 2)))

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

;; (defun point-x (&optional (point *default-position*))
;;   (cffi:foreign-slot-value point 'SDL_Rect 'x))
;; (defun (setf point-x) (x-val &optional (point *default-position*))
;;   (setf (cffi:foreign-slot-value point 'SDL_Rect 'x) (to-int x-val)))

;; (defun point-y (&optional (point *default-position*))
;;   (cffi:foreign-slot-value point 'SDL_Rect 'y))
;; (defun (setf point-y) (y-val &optional (point *default-position*))
;;   (setf (cffi:foreign-slot-value point 'SDL_Rect 'y) (to-int y-val)))

;; (defun pos-x (&optional (position *default-position*))
;;   (cffi:foreign-slot-value position 'SDL_Rect 'x))
;; (defun (setf pos-x) (x-val &optional (position *default-position*))
;;   (setf (cffi:foreign-slot-value position 'SDL_Rect 'x) (to-int x-val)))

;; (defun pos-y (&optional (position *default-position*))
;;   (cffi:foreign-slot-value position 'SDL_Rect 'y))
;; (defun (setf pos-y) (y-val &optional (position *default-position*))
;;   (setf (cffi:foreign-slot-value position 'SDL_Rect 'y) (to-int y-val)))





