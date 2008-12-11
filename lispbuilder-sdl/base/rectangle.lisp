; SDL (Simple Media Layer) library using CFFI for foreign function interfacing...
;; (C)2006 Justin Heyes-Jones <justinhj@gmail.com> and Luke Crook <luke@balooga.com>
;; Thanks to Frank Buss and Surendra Singh
;; see COPYING for license
;; This file contains some useful functions for using SDL from Common lisp
;; using sdl.lisp (the CFFI wrapper)

(in-package #:lispbuilder-sdl-base)

(declaim (inline rectangle))
(defun rectangle (&key (x 0) (y 0) (w 0) (h 0) src)
  "Creates a new rectangle initialized with values x, y, width W and height H, 
or the contents of rectangle SRC if specified."
  (if src
      (clone-rectangle src)
      (let ((new-rect (cffi:foreign-alloc 'sdl-cffi::sdl-rect)))
	(setf (rect-x new-rect) x
	      (rect-y new-rect) y
	      (rect-w new-rect) w
	      (rect-h new-rect) h)
	new-rect)))

(defmacro with-rectangle ((var &optional rectangle (free-p t)) &body body)
  (let ((mbody (gensym "mbody-")))
    `(let ((,mbody nil))
       ,(if (or rectangle (atom var))
          `(symbol-macrolet ((,(intern (string-upcase (format nil "~A.x" var))) (rect-x ,var))
                             (,(intern (string-upcase (format nil "~A.y" var))) (rect-y ,var))
                             (,(intern (string-upcase (format nil "~A.w" var))) (rect-w ,var))
                             (,(intern (string-upcase (format nil "~A.h" var))) (rect-h ,var)))
             (declare (ignorable ,(intern (string-upcase (format nil "~A.x" var)))
                                 ,(intern (string-upcase (format nil "~A.y" var)))
                                 ,(intern (string-upcase (format nil "~A.w" var)))
                                 ,(intern (string-upcase (format nil "~A.h" var)))))
             ,(if rectangle
                `(let ((,var ,rectangle))
                   (setf ,mbody (progn ,@body))
                   (when ,free-p
                     (cffi:foreign-free ,var)))
                `(cffi:with-foreign-object (,var 'sdl-cffi::SDL-Rect)
                   (setf ,mbody (progn ,@body))))
	     ,mbody)
          (error "VAR must be a symbol or variable, not a function.")))))

(defmacro with-rectangles (bindings &body body)
  (if bindings
      (return-with-rectangle bindings body)))

(defun return-with-rectangle (bindings body)
  (if bindings
      `(with-rectangle (,@(car bindings))
	 ,(return-with-rectangle (cdr bindings) body))
      `(progn ,@body)))

;(declaim (inline rectangle-from-edges-*))
(defun rectangle-from-edges-* (x1 y1 x2 y2 rectangle)
  (declare (type fixnum x1 y1 x2 y2))
  (with-rectangle (rect rectangle nil)
    (setf rect.x x1
	  rect.y y1
	  rect.w (1+ (abs (- x2 x1)))
	  rect.h (1+ (abs (- y2 y1)))))
  rectangle)

(declaim (inline rect-x))
(defun rect-x (rect)
  (cffi:foreign-slot-value rect 'sdl-cffi::sdl-rect 'sdl-cffi::x))
(defun (setf rect-x) (x-val rect)
  (declare (type fixnum x-val))
  (setf (cffi:foreign-slot-value rect 'sdl-cffi::sdl-rect 'sdl-cffi::x) x-val))

(declaim (inline rect-y))
(defun rect-y (rect)
  (cffi:foreign-slot-value rect 'sdl-cffi::sdl-rect 'sdl-cffi::y))
(defun (setf rect-y) (y-val rect)
  (declare (type fixnum y-val))
  (setf (cffi:foreign-slot-value rect 'sdl-cffi::sdl-rect 'sdl-cffi::y) y-val))

(declaim (inline rect-w))
(defun rect-w (rect)
  (cffi:foreign-slot-value rect 'sdl-cffi::sdl-rect 'sdl-cffi::w))
(defun (setf rect-w) (w-val rect)
  (declare (type fixnum w-val))
  (setf (cffi:foreign-slot-value rect 'sdl-cffi::sdl-rect 'sdl-cffi::w) w-val))

(declaim (inline rect-h))
(defun rect-h (rect)
  (cffi:foreign-slot-value rect 'sdl-cffi::sdl-rect 'sdl-cffi::h))
(defun (setf rect-h) (h-val rect)
  (declare (type fixnum h-val))
  (setf (cffi:foreign-slot-value rect 'sdl-cffi::sdl-rect 'sdl-cffi::h) h-val))

(declaim (inline rect-x2))
(defun rect-x2 (rect)
  (+ (rect-x rect) (rect-w rect)))
(defun (setf rect-x2) (h-val rect)
  (setf (rect-w rect) (+ (rect-x rect) h-val)))

(declaim (inline rect-y2))
(defun rect-y2 (rect)
  (+ (rect-y rect) (rect-h rect)))
(defun (setf rect-y2) (h-val rect)
  (setf (rect-h rect) (+ (rect-y rect) h-val)))

(declaim (inline copy-rectangle))
(defun copy-rectangle (src dst)
  "Copies the contents of the rectangle SRC to the rectangle DST. SRC and DST are both of type SDL_Rect."
  (if (and (is-valid-ptr src) (is-valid-ptr dst))
      (setf (rect-x dst) (rect-x src)
	    (rect-y dst) (rect-y src)
	    (rect-w dst) (rect-w src)
	    (rect-h dst) (rect-h src))
      (error "SRC and DST must be of type SDL_Rect."))
  dst)

(declaim (inline clone-rectangle))
(defun clone-rectangle (src)
  "Initializes and returns a new rectangle with the contents of the rectangle SRC. SRC and DST are both of type SDL_Rect."
  (if (is-valid-ptr src)
      (copy-rectangle src (cffi:foreign-alloc 'sdl-cffi::sdl-rect))
      (error "SRC and DST must be of type SDL_Rect.")))
