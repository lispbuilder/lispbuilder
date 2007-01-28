;; lispbuilder-sdl
;; (C)2006 Luke Crook <luke@balooga.com>

(in-package #:lispbuilder-sdl)

(defclass sdl-color () ())

(defclass color (sdl-color)
  ((color-vector :accessor fp :initform (vector 0 0 0) :initarg :color)))

(defclass color-a (color)
  ((color-vector :accessor fp :initform (vector 0 0 0 0) :initarg :color)))

(defclass foreign-color (sdl-color)
  ((foreign-pointer-to-color :accessor fp :initform nil :initarg :color)))

(defun color (&key (r 0) (g 0) (b 0) (a nil))
  "Returns a new color from the red R, green G, and blue B INTEGER values."
  (if a
      (make-instance 'color-a :color (vector (cast-to-int r)
					     (cast-to-int g)
					     (cast-to-int b)
					     (cast-to-int a)))
      (make-instance 'color :color (vector (cast-to-int r)
					   (cast-to-int g)
					   (cast-to-int b)))))

(defmacro with-color ((var &optional color (free-p t))
		      &body body)
  `(let* ((,@(if color
		 `(,var ,color)
		 `(,var ,var)))
	  (*default-color* ,var))
     (symbol-macrolet ((r (r ,var))
		       (g (g ,var))
		       (b (b ,var))
		       (a (a ,var)))
       ,@body
       (if ,free-p
	   (free-color ,var)))))

(defmethod r ((color color))
  (svref (fp color) 0))
(defmethod (setf r) (r-val (color color))
  (setf (svref (fp color) 0) (cast-to-int r-val)))

(defmethod g ((color color))
  (svref (fp color) 1))
(defmethod (setf g) (g-val (color color))
  (setf (svref (fp color) 1) (cast-to-int g-val)))

(defmethod b ((color color))
  (svref (fp color) 2))
(defmethod (setf b) (b-val (color color))
  (setf (svref (fp color) 2) (cast-to-int b-val)))

(defmethod a ((color color))
  nil)

(defmethod a ((color color-a))
  (svref (fp color) 3))
(defmethod (setf a) (a-val (color color-a))
  (setf (svref (fp color) 3) (cast-to-int a-val)))

(defmethod set-color ((dst sdl-color) (src sdl-color))
  (set-color-* dst :r (r src) :g (g src) :b (b src) :a (a src))
  dst)

(defmethod set-color-* ((color sdl-color) &key r g b a)
  (when r (setf (r color) r))
  (when g (setf (g color) g))
  (when b (setf (b color) b))
  (when a (setf (a color) a))
  color)

(defmethod color-* ((color color))
  (values (r color) (g color) (b color)))

(defmethod color-* ((color color-a))
  (values (r color) (g color) (b color) (a color)))

(defmethod color-* ((color foreign-color))
  nil)

(defmethod map-color ((color color) &optional (surface *default-surface*))
  (sdl-cffi::sdl-map-rgb (sdl-base::pixel-format (fp surface))
			 (r color) (g color) (b color)))

(defmethod map-color ((color color-a) &optional (surface *default-surface*))
  (sdl-cffi::sdl-map-rgba (sdl-base::pixel-format (fp surface))
			  (r color) (g color) (b color) (a color)))

(defmethod pack-color ((color color))
  (let ((col #x00000000))
    (setf col (logior (ash (r color) 24)
		      (ash (g color) 16)
		      (ash (b color) 8)
		      #xFF))
    col))

(defmethod pack-color ((color color-a))
  (let ((col #x00000000))
    (setf col (logior (ash (r color) 24)
		      (ash (g color) 16)
		      (ash (b color) 8)
		      (a color)))
    col))


(defmethod free-color ((color sdl-color)) nil)

(defmethod free-color ((color foreign-color))
  (cffi:foreign-free (fp color))
  #-clisp(cffi:cancel-finalization color)
  )

(defmacro with-foreign-color-copy ((struct color) &body body)
  "Creates a new foreign SDL_Color object STRUCT on the stack. Then copies the color components from COLOR into STRUCT.
STRUCT is free'd after BODY." 
  `(cffi:with-foreign-object (,struct 'sdl-cffi::SDL-Color)
     (cffi:with-foreign-slots ((sdl-cffi::r sdl-cffi::g sdl-cffi::b) ,struct sdl-cffi::SDL-Color)
       (setf sdl-cffi::r (r ,color)
	     sdl-cffi::g (g ,color)
	     sdl-cffi::b (b ,color)))
     ,@body))