; SDL (Simple Media Layer) library using CFFI for foreign function interfacing...
;; (C)2006 Justin Heyes-Jones <justinhj@gmail.com> and Luke Crook <luke@balooga.com>
;; Thanks to Frank Buss and Surendra Singh
;; see COPYING for license
;; This file contains some useful functions for using SDL from Common lisp
;; using sdl.lisp (the CFFI wrapper)

(in-package #:lispbuilder-sdl)

(defmacro with-point ((var &optional point)
		      &body body)
  `(symbol-macrolet ((x (x ,var))
		     (y (y ,var)))
     (let* ((,@(if point
		   `(,var ,point)
		   `(,var ,var)))
	    (*default-point* ,var))
       ,@body)))

(defun point (&key (x 0) (y 0) (src nil))
  (cond
    (src
     (copy-seq src))
    (t
     (vector x y))))

(defmethod x ((point vector))
  (elt point 0))
(defmethod (setf x) (x-val (point vector))
  (setf (elt point 0) x-val))

(defmethod y ((point vector))
  (elt point 1))
(defmethod (setf y) (y-val (point vector))
  (setf (elt point 1) y-val))

(defmethod point-from ((surface surface))
  (point :x (x surface) :y (y surface)))
(defmethod (setf pos) (pos-val (surface sdl-surface))
  (setf (x surface) (x pos-val)
	(y surface) (y pos-val)))
