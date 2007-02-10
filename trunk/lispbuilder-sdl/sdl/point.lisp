; SDL (Simple Media Layer) library using CFFI for foreign function interfacing...
;; (C)2006 Justin Heyes-Jones <justinhj@gmail.com> and Luke Crook <luke@balooga.com>
;; Thanks to Frank Buss and Surendra Singh
;; see COPYING for license
;; This file contains some useful functions for using SDL from Common lisp
;; using sdl.lisp (the CFFI wrapper)

(in-package #:lispbuilder-sdl)

(defmacro with-point ((var &optional point)
		      &body body)
  `(let* ((,@(if point
		 `(,var ,point)
		 `(,var ,var)))
	  (*default-point* ,var))
     (symbol-macrolet ((x (x ,var))
		       (y (y ,var)))
       ,@body)))

(deftype point ()
  `vector)

(defun point (&key (x 0) (y 0))
  (multiple-value-bind (int-x int-y)
      (cast-all-to-int x y)
    (vector int-x int-y)))

(defun copy-point (point)
  (copy-seq point))

(defmethod x ((point vector))
  (elt point 0))
(defmethod (setf x) (x-val (point vector))
  (setf (elt point 0) x-val))

(defmethod y ((point vector))
  (elt point 1))
(defmethod (setf y) (y-val (point vector))
  (setf (elt point 1) y-val))

(defmethod point-* ((point vector))
  (values (x point) (y point)))

(defmethod get-point ((point vector))
  point)

(defmethod set-point ((dst vector) (src vector))
  (set-point-* dst :x (x src) :y (y src))
  dst)

(defmethod set-point-* ((point vector) &key x y)
  (when x (setf (x point) x))
  (when y (setf (y point) y))
  point)

(defmethod position-* ((point vector))
  (values (x point) (y point)))

(defmethod set-position ((dst vector) (src vector))
  (set-point-* dst :x (x src) :y (y src))
  dst)

(defmethod set-position-* ((point vector) &key x y)
  (when x (setf (x point) x))
  (when y (setf (y point) y))
  point)
