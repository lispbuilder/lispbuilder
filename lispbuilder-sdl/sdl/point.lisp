;; SDL (Simple Media Layer) library using CFFI for foreign function interfacing...
;; (C)2006 Justin Heyes-Jones <justinhj@gmail.com> and Luke Crook <luke@balooga.com>
;; Thanks to Frank Buss and Surendra Singh
;; see COPYING for license
;; This file contains some useful functions for using SDL from Common lisp
;; using sdl.lisp (the CFFI wrapper)

(in-package #:lispbuilder-sdl)

(defmacro with-point ((var &optional point)
		      &body body)
  "A convenience macro that binds `\*DEFAULT-POINT\*` to `VAR` within the scope of `WITH-POINT`. `VAR` must be of type `POINT`.
If `POINT` is not `NIL`, then `VAR is set to `POINT`."
  `(let* ((,@(if point
		 `(,var ,point)
		 `(,var ,var)))
	  (*default-point* ,var))
     (symbol-macrolet ((,(intern (string-upcase (format nil "~A.x" var))) (x ,var))
		       (,(intern (string-upcase (format nil "~A.y" var))) (y ,var)))
       ,@body)))

(deftype point ()
  `vector)

(defun point (&key (x 0) (y 0))
  "Creates a new `POINT` set to the specified horizontal `X` and vertical `Y` coordinate."
  (multiple-value-bind (int-x int-y)
      (cast-all-to-int x y)
    (vector int-x int-y)))

(defun copy-point (point)
  "Returns a copy of the point `POINT`."
  (copy-seq point))

(defmethod x ((point vector))
  "Returns the `X` coordindate of the point `POINT` as an `INTEGER`."
  (elt point 0))
(defmethod (setf x) (x-val (point vector))
  "Sets the `X` coordindate of the point `POINT`."
  (setf (elt point 0) x-val))

(defmethod y ((point vector))
  "Returns the `Y` coordindate of the point `POINT` as an `INTEGER`."
  (elt point 1))
(defmethod (setf y) (y-val (point vector))
  "Sets the `X` coordindate of the point `POINT`."
  (setf (elt point 1) y-val))

(defmethod point-* ((point vector))
  "Returns the `X` and `Y` coordinates of the point `POINT` as a spread."
  (values (x point) (y point)))

(defmethod get-point ((point vector))
  "Returns the point `POINT`."
  point)

(defmethod set-point ((dst vector) (src vector))
  "Sets the `X` and `Y` coordinates of the destination point `DST` to the coordinates in 
the source point `SRC`. Returns the destination point `DST`."
  (set-point-* dst :x (x src) :y (y src))
  dst)

(defmethod set-point-* ((point vector) &key x y)
  "Sets the `X` and `Y` coordinates of the point `POINT` to `X` and `Y`."
  (when x (setf (x point) x))
  (when y (setf (y point) y))
  point)

(defmethod position-* ((point vector))
  "See [POINT-*](#point-*)."
  (values (x point) (y point)))

(defmethod set-position ((dst vector) (src vector))
  "See [SET-POINT](#set-point)."
  (set-point-* dst :x (x src) :y (y src))
  dst)

(defmethod set-position-* ((point vector) &key x y)
  "See [SET-POINT-*](#set-point-*)."
  (when x (setf (x point) x))
  (when y (setf (y point) y))
  point)
