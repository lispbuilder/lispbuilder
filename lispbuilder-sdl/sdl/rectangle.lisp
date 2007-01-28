;;;; lispbuilder-sdl
;;;; (C)2006 Luke Crook <luke@balooga.com>

(in-package #:lispbuilder-sdl)

(defclass rectangle ()
  ((foreign-pointer-to-rectangle :accessor fp :initform nil :initarg :rectangle)
   (rectangle-color :accessor rectangle-color :initform (color :r 255 :g 255 :b 255) :initarg :color)))

(defclass null-rectangle (rectangle) ())

(defun rectangle (&key (x 0) (y 0) (w 0) (h 0)
		  (fp nil) (null nil))
  (if null
      (make-instance 'null-rectangle :rectangle (cffi:null-pointer))
      (make-instance 'rectangle :rectangle (if (sdl-base::is-valid-ptr fp)
					       (sdl-base::rectangle :src fp)
					       (sdl-base::rectangle :x x :y y :w w :h h)))))

(defmacro with-rectangle ((var &optional rectangle (free-p t))
			  &body body)
  `(let* ((,@(if rectangle
		 `(,var ,rectangle)
		 `(,var ,var)))
	  (*default-rectangle* ,var))
     (symbol-macrolet ((x (x ,var))
		       (y (y ,var))
		       (w (width ,var))
		       (h (height ,var)))
       ,@body
       (if ,free-p
	   (free-rectangle ,var)))))

(defmacro with-rectangles (bindings &body body)
  (if bindings
      (return-with-rectangle bindings body)))

(defun return-with-rectangle (bindings body)
  (if bindings
      `(with-rectangle (,@(car bindings))
	 ,(return-with-rectangle (cdr bindings) body))
      `(progn ,@body)))

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
  (setf (sdl-base::rect-w (fp rectangle)) (- h-val
					     (sdl-base::rect-x (fp rectangle)))))

(defmethod y2 ((rectangle rectangle))
  (+ (sdl-base::rect-y (fp rectangle))
     (sdl-base::rect-h (fp rectangle))))
(defmethod (setf y2) (h-val (rectangle rectangle))
  (setf (sdl-base::rect-h (fp rectangle)) (- h-val
					     (sdl-base::rect-y (fp rectangle)))))

(defmethod width ((rectangle rectangle))
  (sdl-base::rect-w (fp rectangle)))
(defmethod (setf width) (w-val (rectangle rectangle))
  (setf (sdl-base::rect-w (fp rectangle)) w-val))

(defmethod height ((rectangle rectangle))
  (sdl-base::rect-h (fp rectangle)))
(defmethod (setf height) (h-val (rectangle rectangle))
  (setf (sdl-base::rect-h (fp rectangle)) h-val))

(defmethod free-rectangle ((rectangle null-rectangle)) nil)

(defmethod free-rectangle ((rectangle rectangle))
  (cffi:foreign-free (fp rectangle))
  #-clisp(cffi:cancel-finalization rectangle)
  )

(defmethod point-* ((rectangle rectangle))
  (values (x rectangle) (y rectangle)))

(defmethod get-point ((rectangle rectangle))
  (vector (x rectangle) (y rectangle)))

(defmethod set-point ((rectangle rectangle) (position vector))
  (set-rectangle-* rectangle :x (x position) (y position))
  rectangle)

(defmethod set-point-* ((rectangle rectangle) &key x y)
  (set-rectangle-* rectangle :x x :y y)
  rectangle)

(defmethod position-* ((rectangle rectangle))
  (values (x rectangle) (y rectangle)))

(defmethod get-position ((rectangle rectangle))
  (vector (x rectangle) (y rectangle)))

(defmethod set-position ((rectangle rectangle) (position vector))
  (set-rectangle-* rectangle :x (x position) (y position))
  rectangle)

(defmethod set-position-* ((rectangle rectangle) &key x y)
  (set-rectangle-* rectangle :x x :y y)
  rectangle)

(defmethod rectangle-* ((rectangle rectangle))
  (values (x rectangle) (y rectangle) (width rectangle) (height rectangle)))

(defmethod get-rectangle ((rectangle rectangle))
  rectangle)

(defmethod set-rectangle ((dst rectangle) (src rectangle))
  (set-rectangle-* dst :x (x src) :y (y src) :w (width src) :h (height src))
  dst)

(defmethod set-rectangle-* ((rectangle rectangle) &key x y w h)
  (when x (setf (x rectangle) x))
  (when y (setf (y rectangle) y))
  (when w (setf (width rectangle) w))
  (when h (setf (height rectangle) h))
  rectangle)

(defmethod color-* ((rectangle rectangle))
  (color-* (rectangle-color rectangle)))

(defmethod set-color ((rectangle rectangle) (color sdl-color))
  (set-color-* rectangle :r (r color) :g (g color) :b (b color) :a (a color))
  rectangle)

(defmethod set-color-* ((rectangle rectangle) &key r g b a)
  (when r (setf (r (rectangle-color rectangle)) r))
  (when g (setf (g (rectangle-color rectangle)) g))
  (when b (setf (b (rectangle-color rectangle)) b))
  (when a (setf (a (rectangle-color rectangle)) a))
  rectangle)
