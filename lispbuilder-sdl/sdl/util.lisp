;; SDL (Simple Media Layer) library using CFFI for foreign function interfacing...
;; (C)2006 Justin Heyes-Jones <justinhj@gmail.com> and Luke Crook <luke@balooga.com>
;; Thanks to Frank Buss and Surendra Singh
;; see COPYING for license
;; This file contains some useful functions for using SDL from Common lisp
;; using sdl.lisp (the CFFI wrapper)

(in-package #:lispbuilder-sdl)


(defun random+1 (rnd)
  "Returns a random number in the range 0 > num <= rnd."
  (+ 1 (random rnd)))

(defun to-radian (degree)
  "Converts degrees to radians."
  (* degree (/ PI 180)))

(defun to-degree (radian)
  "Converts radians to degrees."
  (/ radian (/ PI 180)))

(defun create-list-if-not (var)
  "If `VAR` is not already a list, then returns `\(LIST VAR\)`."
  (if (listp var)
      var
      (list var)))

;; From _3b in #lisp
(defmacro cast (type value)
  "Coerces the value `VALUE` to the type `TYPE`."
  `(coerce ,value ',type))

;; From _3b in #lisp
(defmacro cast-to-int (value)
  "Casts the value `VALUE` to a `FIXNUM`."
  `(the fixnum (floor (+ ,value 0.5))))

(defmacro cast-all-to-int (&rest values)
  "Casts the values in `REST` to `FIXNUM`s."
  `(values
     ,@(mapcar #'(lambda (value)
		   `(the fixnum (floor (+ ,value 0.5))))
	       values)))

(defmacro all-integers? (&rest values)
  "Returns `T` if all values are `INTEGERS`."
  `(and
    ,@(mapcar #'(lambda (value)
		  `(integerp ,value))
	      values)))

(defmacro check-types (type &rest rest)
  "Performs `CHECK-TYPE` on items in rest."
  `(progn
     ,@(mapcar #'(lambda (var)
		   `(check-type ,var ,type))
	       rest)))

(defun create-path (filename &optional path)
  "Creates a new path from `FILENAME` and `PATH`."
  (if path
      (namestring (merge-pathnames filename path))
      filename))
