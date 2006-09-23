;; SDL (Simple Media Layer) library using CFFI for foreign function interfacing...
;; (C)2006 Justin Heyes-Jones <justinhj@gmail.com> and Luke Crook <luke@balooga.com>
;; Thanks to Frank Buss and Surendra Singh
;; see COPYING for license
;; This file contains some useful functions for using SDL from Common lisp
;; using sdl.lisp (the CFFI wrapper)

(in-package #:lispbuilder-sdl)

;; (defmacro check-bounds (min below &rest vars)
;;   (let (result)
;;     (loop for var in vars do
;; 	  (push `(when (< ,var ,min) (setf ,var ,min)) result)
;; 	  (push `(when (>= ,var ,below) (setf ,var (1- ,below))) result))
;;     (push 'progn result)
;;     result))

(defmacro check-bounds (min below &rest vars)
  (let (result)
    (loop for var in vars do
	 (push `(setf ,var (clamp ,var ,min ,below)) result))
    (push 'progn result)
    result))

;; cl-sdl "util.lisp"
(declaim (inline clamp))
(defun clamp (v l u)
  (min (max v l) u))

(defun clamp-to-sbyte (v)
  (min (max v -127) 127))

(defun clamp-to-ubyte (v)
  (min (max v 0) 255))

(defun clamp-to-sshort (v)
  (min (max v -32767) 32767))

(defun clamp-to-ushort (v)
  (min (max v 0) 65535))

;; cl-sdl "util.lisp"
(defun delta-clamp (v d l u)
  (let ((sum (+ v d)))
    (cond ((< sum l)
           (- d (- sum l)))
          ((> sum u)
           (- d (- sum u)))
          (t d))))

(defun is-valid-ptr (pointer)
  "IS-VALID-PTR <CFFI pointer>
  Will return T if 'pointer' is a valid <CFFI pointer> and is non-null."
  (and (cffi:pointerp pointer) (not (cffi:null-pointer-p pointer))))

(defun random+1 (rnd)
  (+ 1 (random rnd)))

(defun to-radian (degree)
  "converts degrees to radians."
  (* degree (/ PI 180)))

(defun to-degree (radian)
  "converts radians to degrees."
  (/ radian (/ PI 180)))

(defun set-flags (&rest keyword-args)
  (if (listp (first keyword-args))
      (let ((keywords 
	     (mapcar #'(lambda (x)
			 (eval x))
		     (first keyword-args))))
	(apply #'logior keywords))
      (apply #'logior keyword-args)))
