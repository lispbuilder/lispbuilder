; SDL (Simple Media Layer) library using CFFI for foreign function interfacing...
;; (C)2006 Justin Heyes-Jones <justinhj@gmail.com> and Luke Crook <luke@balooga.com>
;; Thanks to Frank Buss and Surendra Singh
;; see COPYING for license
;; This file contains some useful functions for using SDL from Common lisp
;; using sdl.lisp (the CFFI wrapper)

(in-package #:lispbuilder-sdl)


;;; Color

(defmacro with-color ((color) &body body)
  `(symbol-macrolet ((r (color-r ,color))
		     (g (color-g ,color))
		     (b (color-b ,color))
		     (a (color-a ,color)))
     ,@body))

(defun color-r (color)
  (svref color 0))
(defun (setf color-r) (r-val color)
  (setf (svref color 0) (to-int r-val)))

(defun color-g (color)
  (svref color 1))
(defun (setf color-g) (g-val color)
  (setf (svref color 1) (to-int g-val)))

(defun color-b (color)
  (svref color 2))
(defun (setf color-b) (b-val color)
  (setf (svref color 2) (to-int b-val)))

(defun color-a (color)
  (if (> (length color) 3)
	 (svref color 3)
	 (error "Color is RGB not RGBA.")))
(defun (setf color-a) (a-val color)
  (setf (svref color 3) (to-int a-val)))

(defun color-rgb (r g b)
  "Returns a new color from the red R, green G, and blue B INTEGER values."
  (vector (to-int r) (to-int g) (to-int b)))

(defun color-rgba (r g b a)
  "Returns a new color from the red R, green G,  blue B and alpha A INTEGER values. "
  (vector (to-int r) (to-int g) (to-int b) (to-int a)))

(defun color (r g b &optional a)
  (if a
      (color-rgba r g b a)
      (color-rgb r g b)))
