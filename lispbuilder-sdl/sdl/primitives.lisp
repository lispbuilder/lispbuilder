;; SDL (Simple Media Layer) library using CFFI for foreign function interfacing...
;; (C)2006 Justin Heyes-Jones <justinhj@gmail.com> and Luke Crook <luke@balooga.com>
;; Thanks to Frank Buss and Surendra Singh
;; see COPYING for license
;; This file contains some useful functions for using SDL from Common lisp
;; using sdl.lisp (the CFFI wrapper)

(in-package #:lispbuilder-sdl)

;; (defclass surface ()
;;   ((pointer-to-surface :accessors surface :initform nil :initarg :surface)))

;; (defclass rectangle ()
;;   ((pointer-to-rectangle :accessors rectangle :intiform nil :initarg :rectangle)))

;; (defclass color ()
;;   ((pointer-to-color :accessors color :initiform nil :initarg :color)))

;; (defclass color-a ()
;;   ((pointer-to-color-a :accessors color :initiform nil :initarg :color)))

;; (defclass color-a ()
;;   ((pointer-to-color-a :accessors color :initiform nil :initarg :color)))



(defmacro with-position ((position) &body body)
  `(symbol-macrolet ((x (pos-x ,position))
		     (y (pos-y ,position)))
     (let ((*default-position* ,position))
       ,@body)))

(defmacro with-rectangle ((rectangle) &body body)
  `(symbol-macrolet ((x (rect-x ,rectangle))
		     (y (rect-y ,rectangle))
		     (w (rect-w ,rectangle))
		     (h (rect-h ,rectangle)))
     (let ((*default-rectangle* ,rectangle))
       ,@body)))

(defmacro with-color ((color) &body body)
  `(symbol-macrolet ((r (color-r ,color))
		     (g (color-g ,color))
		     (b (color-b ,color))
		     (a (color-a ,color)))
     (let ((*default-color* ,color))
       ,@body)))

(defun color (r g b &optional a)
  (if a
      (vector (to-int r) (to-int g) (to-int b) (to-int a))
      (vector (to-int r) (to-int g) (to-int b))))

(defun color-r (&optional (color *default-color*))
  (svref color 0))
(defun (setf color-r) (r-val color)
  (setf (svref color 0) (to-int r-val)))

(defun color-g (&optional (color *default-color*))
  (svref color 1))
(defun (setf color-g) (g-val color)
  (setf (svref color 1) (to-int g-val)))

(defun color-b (&optional (color *default-color*))
  (svref color 2))
(defun (setf color-b) (b-val color)
  (setf (svref color 2) (to-int b-val)))

(defun color-a (&optional (color *default-color*))
  (if (> (length color) 3)
	 (svref color 3)))
(defun (setf color-a) (a-val color)
  (setf (svref color 3) (to-int a-val)))

(defun copy-rectangle (&optional (rectangle *default-rectangle*))
  (copy-seq rectangle))

(defun moveby-rectangle (&key (rectangle *default-rectangle*) (position *default-position*))
  (setf (rect-x rectangle) (+ (rect-x rectangle) (pos-x position))
	(rect-y rectangle) (+ (rect-y rectangle) (pos-y position)))
  rectangle)

(defun moveto-rectangle (&key (rectangle *default-rectangle*) (position *default-position*))
  (setf (rect-x rectangle) (pos-x position)
	(rect-y rectangle) (pos-y position))
  rectangle)

(defun point-x (&optional (point sdl:*default-position*))
  (svref point 0))
(defun (setf point-x) (x-val &optional (point sdl:*default-position*))
  (setf (svref point 0) (to-int x-val)))

(defun point-y (&optional (point sdl:*default-position*))
  (svref point 1))
(defun (setf point-y) (y-val &optional (point sdl:*default-position*))
  (setf (svref point 1) (to-int y-val)))

(defun point (x y)
  (vector (to-int x) (to-int y)))

(defun pos-x (&optional (position *default-position*))
  (svref position 0))
(defun (setf pos-x) (x-val position)
  (setf (svref position 0) (to-int x-val)))

(defun pos-y (&optional (position *default-position*))
  (svref position 1))
(defun (setf pos-y) (y-val position)
  (setf (svref position 1) (to-int y-val)))

(defun rectangle (x y w h)
  "Creates a new rectangle."
  (vector (to-int x) (to-int y) (to-int w) (to-int h)))

(defun rect-x (&optional (rect *default-rectangle*))
  (svref rect 0))
(defun (setf rect-x) (x-val rect)
  (setf (svref rect 0) (to-int x-val)))

(defun rect-y (&optional (rect *default-rectangle*))
  (svref rect 1))
(defun (setf rect-y) (y-val rect)
  (setf (svref rect 1) (to-int y-val)))

(defun rect-w (&optional (rect *default-rectangle*))
  (svref rect 2))
(defun (setf rect-w) (w-val rect)
  (setf (svref rect 2) (to-int w-val)))

(defun rect-h (&optional (rect *default-rectangle*))
  (svref rect 3))
(defun (setf rect-h) (h-val rect)
  (setf (svref rect 3) (to-int h-val)))

(defun rect-x2 (&optional (rect *default-rectangle*))
  (+ (rect-x rect) (rect-w rect)))
(defun (setf rect-x2) (h-val rect)
  (setf (rect-w rect) (+ (rect-x rect) h-val)))

(defun rect-y2 (&optional (rect *default-rectangle*))
  (+ (rect-y rect) (rect-h rect)))
(defun (setf rect-y2) (h-val rect)
  (setf (rect-h rect) (+ (rect-y rect) h-val)))
