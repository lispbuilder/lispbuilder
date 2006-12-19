; SDL (Simple Media Layer) library using CFFI for foreign function interfacing...
;; (C)2006 Justin Heyes-Jones <justinhj@gmail.com> and Luke Crook <luke@balooga.com>
;; Thanks to Frank Buss and Surendra Singh
;; see COPYING for license
;; This file contains some useful functions for using SDL from Common lisp
;; using sdl.lisp (the CFFI wrapper)

(in-package #:lispbuilder-sdl-base)


;;; Rectangle

(defmacro with-rectangle ((var &optional rectangle (free-p t)) &body body)
  (if (or rectangle (atom var))
      `(symbol-macrolet ((,(intern (string-upcase (format nil "~A.x" var))) (rect-x ,var))
			 (,(intern (string-upcase (format nil "~A.y" var))) (rect-y ,var))
			 (,(intern (string-upcase (format nil "~A.w" var))) (rect-w ,var))
			 (,(intern (string-upcase (format nil "~A.h" var))) (rect-h ,var))
			 (x (rect-x ,var))
			 (y (rect-y ,var))
			 (w (rect-w ,var))
			 (h (rect-h ,var)))
	 ,(if rectangle
	      `(let ((,var ,rectangle))
		 ,@body
		 (when ,free-p
		   (cffi:foreign-free ,var)))
	      `(cffi:with-foreign-object (,var 'sdl-cffi::SDL-Rect)
		 ,@body)))
      (error "VAR must be a symbol or variable, not a function.")))
  
(defun rect-x (rect)
  (cffi:foreign-slot-value rect 'sdl-cffi::sdl-rect 'sdl-cffi::x))
(defun (setf rect-x) (x-val rect)
  (setf (cffi:foreign-slot-value rect 'sdl-cffi::sdl-rect 'sdl-cffi::x) (to-int x-val)))

(defun rect-y (rect)
  (cffi:foreign-slot-value rect 'sdl-cffi::sdl-rect 'sdl-cffi::y))
(defun (setf rect-y) (y-val rect)
  (setf (cffi:foreign-slot-value rect 'sdl-cffi::sdl-rect 'sdl-cffi::y) (to-int y-val)))

(defun rect-w (rect)
  (cffi:foreign-slot-value rect 'sdl-cffi::sdl-rect 'sdl-cffi::w))
(defun (setf rect-w) (w-val rect)
  (setf (cffi:foreign-slot-value rect 'sdl-cffi::sdl-rect 'sdl-cffi::w) (to-int w-val)))

(defun rect-h (rect)
  (cffi:foreign-slot-value rect 'sdl-cffi::sdl-rect 'sdl-cffi::h))
(defun (setf rect-h) (h-val rect)
  (setf (cffi:foreign-slot-value rect 'sdl-cffi::sdl-rect 'sdl-cffi::h) (to-int h-val)))

(defun rect-x2 (rect)
  (+ (rect-x rect) (rect-w rect)))
(defun (setf rect-x2) (h-val rect)
  (setf (rect-w rect) (+ (rect-x rect) h-val)))

(defun rect-y2 (rect)
  (+ (rect-y rect) (rect-h rect)))
(defun (setf rect-y2) (h-val rect)
  (setf (rect-h rect) (+ (rect-y rect) h-val)))

(defun copy-rectangle (src dst)
  "Copies the contents of the rectangle SRC to the rectangle DST. SRC and DST are both of type SDL_Rect."
  (if (and (is-valid-ptr src) (is-valid-ptr dst))
      (setf (rect-x dst) (rect-x src)
	    (rect-y dst) (rect-y src)
	    (rect-w dst) (rect-w src)
	    (rect-h dst) (rect-h src))
      (error "SRC and DST must be of type SDL_Rect."))
  dst)

(defun clone-rectangle (src)
  "Initializes and returns a new rectangle with the contents of the rectangle SRC. SRC and DST are both of type SDL_Rect."
  (if (is-valid-ptr src)
      (copy-rectangle src (cffi:foreign-alloc 'sdl-cffi::sdl-rect))
      (error "SRC and DST must be of type SDL_Rect.")))

(defun rectangle (&key (x 0) (y 0) (w 0) (h 0) src)
  "Creates a new rectangle initialized with values x, y, width W and height H, or the rectangle SRC if specified."
  (if src
      (clone-rectangle src)
      (let ((new-rect (cffi:foreign-alloc 'sdl-cffi::sdl-rect)))
	(setf (rect-x new-rect) x
	      (rect-y new-rect) y
	      (rect-w new-rect) w
	      (rect-h new-rect) h)
	new-rect)))
