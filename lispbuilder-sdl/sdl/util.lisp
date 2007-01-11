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

(defun create-list-if-not (var)
  (if (listp var)
      var
      (list var)))

;; From _3b in #lisp
(defmacro cast (type value)
  `(coerce ,value ',type))

;; From _3b in #lisp
(defmacro cast-to-int (value)
  `(the fixnum (floor (+ ,value 0.5))))

(defmacro cast-all-to-int (&rest values)
  `(values
     ,@(mapcar #'(lambda (value)
		   `(the fixnum (floor (+ ,value 0.5))))
	       values)))

(defmacro all-integers? (&rest values)
  `(and
    ,@(mapcar #'(lambda (value)
		  `(integerp ,value))
	      values)))