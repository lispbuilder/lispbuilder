;; SDL (Simple Media Layer) library using CFFI for foreign function interfacing...
;; (C)2006 Justin Heyes-Jones <justinhj@gmail.com> and Luke Crook <luke@balooga.com>
;; Thanks to Frank Buss and Surendra Singh
;; see COPYING for license
;; This file contains some useful functions for using SDL from Common lisp
;; using sdl.lisp (the CFFI wrapper)

(in-package #:lispbuilder-sdl)


(defclass rectangle ()
  ((foreign-pointer-to-rectangle :reader fp :initform nil :initarg :rectangle)))

(defun rectangle (&key (x 0) (y 0) (w 0) (h 0)
		  (fp nil) (null nil))
  (cond
    (null
     (make-instance 'rectangle :rectangle (cffi:null-pointer)))
    ((sdl-base::is-valid-ptr fp)
     (make-instance 'rectangle :rectangle fp))
    (t
     (make-instance 'rectangle :rectangle (sdl-base::rectangle :x x :y y :w w :h h)))))

(defmacro with-rectangle ((var &optional rectangle (free-p t))
			  &body body)
  `(symbol-macrolet ((,(intern (string-upcase (format nil "~A.x" var))) (x ,var))
		     (,(intern (string-upcase (format nil "~A.y" var))) (y ,var))
		     (,(intern (string-upcase (format nil "~A.width" var))) (width ,var))
		     (,(intern (string-upcase (format nil "~A.height" var))) (height ,var)))
     (let* ((,@(if rectangle
		   `(,var ,rectangle)
		   `(,var ,var)))
	    (*default-rectangle* ,var))
       ,@body
       (if ,free-p
	   (free-rectangle ,var)))))

(defmethod x ((rectangle rectangle))
  (sdl-base::rect-x (fp rectangle)))
(defmethod (setf x) (x-val (rectangle rectangle))
  (setf (sdl-base::rect-x (fp rectangle)) x-val))

(defmethod y ((rectangle rectangle))
  (sdl-base::rect-y (fp rectangle)))
(defmethod (setf y) (y-val (rectangle rectangle))
  (setf (sdl-base::rect-y (fp rectangle)) y-val))

(defmethod x2 ((rectangle rectangle))
  (+ (sdl-base::rect-x (fp rectangle))
     (sdl-base::rect-w (fp rectangle))))
(defmethod (setf x2) (h-val (rectangle rectangle))
  (setf (sdl-base::rect-w (fp rectangle)) (+ (sdl-base::rect-x (fp rectangle))
					     h-val)))

(defmethod y2 ((rectangle rectangle))
  (+ (sdl-base::rect-y (fp rectangle))
     (sdl-base::rect-h (fp rectangle))))
(defmethod (setf y2) (h-val (rectangle rectangle))
  (setf (sdl-base::rect-h (fp rectangle)) (+ (sdl-base::rect-y (fp rectangle))
					     h-val)))

(defmethod width ((rectangle rectangle))
  (sdl-base::rect-w (fp rectangle)))
(defmethod (setf width) (w-val (rectangle rectangle))
  (setf (sdl-base::rect-w (fp rectangle)) w-val))

(defmethod height ((rectangle rectangle))
  (sdl-base::rect-h (fp rectangle)))
(defmethod (setf height) (h-val (rectangle rectangle))
  (setf (sdl-base::rect-h (fp rectangle)) h-val))

(defmethod free-rectangle ((rectangle rectangle))
  (cffi:foreign-free (fp rectangle))
  #-clisp(cffi:cancel-finalization rectangle)
  )
