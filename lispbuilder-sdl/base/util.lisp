;; SDL (Simple Media Layer) library using CFFI for foreign function interfacing...
;; (C)2006 Justin Heyes-Jones <justinhj@gmail.com> and Luke Crook <luke@balooga.com>
;; Thanks to Frank Buss and Surendra Singh
;; see COPYING for license
;; This file contains some useful functions for using SDL from Common lisp
;; using sdl.lisp (the CFFI wrapper)

(in-package #:lispbuilder-sdl-base)

(defmacro check-bounds (min below &rest vars)
  "Clamps the values in VARS to MIN <= VARS <= BELOW. Returns as a list the values in VARS."
  (let (result)
    (loop for var in vars do
	 (push `(setf ,var (clamp ,var ,min ,below)) result))
    (push 'progn result)
    result))

;; cl-sdl "util.lisp"
(declaim (inline clamp))
(defun clamp (v l u)
  "Returns V clamped to L <= V <= U."
  (min (max v l) u))

(declaim (inline clamp-to-sbyte))
(defun clamp-to-sbyte (v)
  (min (max v -127) 127))

(declaim (inline clamp-to-ubyte))
(defun clamp-to-ubyte (v)
  (min (max v 0) 255))

(declaim (inline clamp-to-sshort))
(defun clamp-to-sshort (v)
  (min (max v -32767) 32767))

(declaim (inline clamp-to-ushort))
(defun clamp-to-ushort (v)
  (min (max v 0) 65535))

;; cl-sdl "util.lisp"
(declaim (inline delta-clamp))
(defun delta-clamp (v d l u)
  (let ((sum (+ v d)))
    (cond ((< sum l)
           (- d (- sum l)))
          ((> sum u)
           (- d (- sum u)))
          (t d))))

(declaim (inline is-valid-ptr))
(defun is-valid-ptr (pointer)
  "Returns T if pointer is not NULL and is a valid CFFI pointer to a foreign object."
  (and (cffi:pointerp pointer)
       (not (cffi:null-pointer-p pointer))))

(declaim (inline to-int))
(defun to-int (num)
  "Returns num as an INTEGER."
  (truncate (+ 0.5 num)))

(defun vec-to-int (vec)
  "vec-to-int will create a new VECTOR of the same length as VEC, but the contents are converted to integers.
   Returns VEC if the contents are not of type float."
  (if (vectorp vec)
      (let ((require-conversion nil)
	    (length (length vec)))
	(block convert
	  (dotimes (i length)
	    (when (floatp (svref vec i))
	      (setf require-conversion t)
	      (return-from convert))))
	(if require-conversion
	    (let ((new-vec (make-array (length vec) :initial-element 0)))
	      (dotimes (i length)
		(setf (svref new-vec i) (to-int (svref vec i))))
	      new-vec)
	    vec))
      nil))

(defun 1/0->t/nil (val)
  (if (integerp val)
      (if (= val 0)
	  nil
	  t)
      val))

