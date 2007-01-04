;; SDL (Simple Media Layer) library using CFFI for foreign function interfacing...
;; (C)2006 Justin Heyes-Jones <justinhj@gmail.com> and Luke Crook <luke@balooga.com>
;; Thanks to Frank Buss and Surendra Singh
;; see COPYING for license
;; This file contains some useful functions for using SDL from Common lisp
;; using sdl.lisp (the CFFI wrapper)

(in-package #:lispbuilder-sdl)


(defclass sdl-color () ())

(defclass color (sdl-color)
  ((color-vector :reader fp :initform (vector 0 0 0) :initarg :color)))

(defclass color-a (color)
  ((color-vector :reader fp :initform (vector 0 0 0 0) :initarg :color)))

(defclass color-struct (sdl-color)
  ((foreign-pointer-to-color :reader fp :initform nil :initarg :color)))

(defun color (&key (r 0) (g 0) (b 0) (a nil))
  "Returns a new color from the red R, green G, and blue B INTEGER values."
  (unless r
    (setf r 0))
  (unless g
    (setf g 0))
  (unless b
    (setf b 0))
  (if a
      (make-instance 'color-a :color (vector r g b a))
      (make-instance 'color :color (vector r g b))))


(defmacro with-color ((var &optional color (free-p t))
		      &body body)
  `(symbol-macrolet ((r (r ,var))
		     (g (g ,var))
		     (b (b ,var))
		     (a (a ,var)))
     (let* ((,@(if color
		   `(,var ,color)
		   `(,var ,var)))
	    (*default-color* ,var))
       ,@body
       (if ,free-p
	   (free-color ,var)))))

(defmethod r ((color color))
  (svref (fp color) 0))
(defmethod (setf r) (r-val (color color))
  (setf (svref (fp color) 0) r-val))

(defmethod g ((color color))
  (svref (fp color) 1))
(defmethod (setf g) (g-val (color color))
  (setf (svref (fp color) 1) g-val))

(defmethod b ((color color))
  (svref (fp color) 2))
(defmethod (setf b) (b-val (color color))
  (setf (svref (fp color) 2) b-val))

(defmethod a ((color color-a))
  (svref (fp color) 3))
(defmethod (setf a) (a-val (color color-a))
  (setf (svref (fp color) 3) a-val))

(defun set-color (color &key r g b a)
  (when r (setf (r color) r))
  (when g (setf (g color) g))
  (when b (setf (b color) b))
  (when (typep color 'color-a)
    (when a (setf (a color) a))))

(defmethod map-color ((color color) &optional (surface *default-surface*))
  (sdl-cffi::sdl-map-rgb (sdl-base::pixel-format (fp surface))
			 (r color) (g color) (b color)))

(defmethod map-color ((color color-a) &optional (surface *default-surface*))
  (sdl-cffi::sdl-map-rgba (sdl-base::pixel-format (fp surface))
			  (r color) (g color) (b color) (a color)))

(defmethod free-color ((color sdl-color)) nil)

(defmethod free-color ((color color-struct))
  (cffi:foreign-free (fp color))
  (cffi:cancel-finalization color))
