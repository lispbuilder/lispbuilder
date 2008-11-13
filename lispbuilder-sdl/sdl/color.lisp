;; lispbuilder-sdl
;; (C)2006 Luke Crook <luke@balooga.com>

(in-package #:lispbuilder-sdl)

(defclass color ()
  ((color-vector :accessor fp :initform (vector 0 0 0) :initarg :color))
  (:documentation "A color containing `INTEGER` Red, Green and Blue components.
Free using [FREE](#free)."))

(defclass color-a (color)
  ((color-vector :accessor fp :initform (vector 0 0 0 0) :initarg :color))
  (:documentation "An color containing `INTEGER` Red, Green, Blue and Alpha components.
Free using [FREE](#free)."))

(defun color (&key (r 0) (g 0) (b 0) (a nil))
  "Returns a new `RGB` [COLOR](#color) from the specified `R`ed, `G`reen, and `B`lue components.
Returns a new `RGBA` [COLOR-A](#color-a) from the specified `R`ed, `G`reen, `B`lue, and `A`lpha components."
  (if a
      (make-instance 'color-a :color (vector (cast-to-int r)
					     (cast-to-int g)
					     (cast-to-int b)
					     (cast-to-int a)))
      (make-instance 'color :color (vector (cast-to-int r)
					   (cast-to-int g)
					   (cast-to-int b)))))

(defmacro with-color ((var &optional color (free t))
		      &body body)
  "A convience macro that binds `\*DEFAULT-COLOR\*` to `VAR` within the scope of `WITH-COLOR`. 
`VAR` is set to `COLOR` when `COLOR` is not `NIL`.`VAR` must be of type [COLOR](#color), or [COLOR-A](#color-a). 
`VAR` is freed using [FREE](#free) when `FREE` is `T`."
  `(let* ((,@(if color
		 `(,var ,color)
		 `(,var ,var)))
	  (*default-color* ,var))
     (symbol-macrolet ((,(intern (string-upcase (format nil "~A.r" var))) (r ,var))
		       (,(intern (string-upcase (format nil "~A.g" var))) (g ,var))
		       (,(intern (string-upcase (format nil "~A.b" var))) (b ,var))
		       (,(intern (string-upcase (format nil "~A.a" var))) (a ,var)))
       (declare (ignorable ,(intern (string-upcase (format nil "~A.r" var)))
                           ,(intern (string-upcase (format nil "~A.g" var)))
                           ,(intern (string-upcase (format nil "~A.b" var)))
                           ,(intern (string-upcase (format nil "~A.a" var)))))
       ,@body
       (if ,free
	   (free ,var)))))

(defmethod free ((color color))
  nil)

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

(defmethod set-color ((dst color) (src color))
  (set-color-* dst :r (r src) :g (g src) :b (b src) :a (a src))
  dst)

(defmethod set-color-* ((color color) &key r g b a)
  (when r (setf (r color) r))
  (when g (setf (g color) g))
  (when b (setf (b color) b))
  (when a (setf (a color) a))
  color)

(defmethod color-* ((color color))
  (values (r color) (g color) (b color)))

(defmethod color-* ((color color-a))
  (values (r color) (g color) (b color) (a color)))

(defmethod map-color-* ((r integer) (g integer) (b integer) a &optional (surface *default-surface*))
  (if a
      (sdl-cffi::sdl-map-rgba (sdl-base::pixel-format (fp surface))
			      r g b a)
      (sdl-cffi::sdl-map-rgb (sdl-base::pixel-format (fp surface))
			     r g b)))
  
(defmethod map-color ((color color) &optional (surface *default-surface*))
  (map-color-* (r color) (g color) (b color) nil surface))

(defmethod map-color ((color color-a) &optional (surface *default-surface*))
  (map-color-* (r color) (g color) (b color) (a color) surface))

(defmethod pack-color-* ((r integer) (g integer) (b integer) &optional (a nil))
  (let ((col #x00000000))
    (setf col (logior (ash r 24)
		      (ash g 16)
		      (ash b 8)
		      (if a
			  a
			  #xFF)))
    col))

(defmethod pack-color ((color color))
  (pack-color-* (r color) (g color) (b color)))

(defmethod pack-color ((color color-a))
  (pack-color-* (r color) (g color) (b color) (a color)))

(defmacro with-foreign-color-copy ((struct color) &body body)
  "Creates and assigns a new foreign `SDL_Color` to `STRUCT`. Then copies the color components from `COLOR` into `STRUCT`.
`STRUCT` is free'd when out of scope." 
  `(cffi:with-foreign-object (,struct 'sdl-cffi::SDL-Color)
     (cffi:with-foreign-slots ((sdl-cffi::r sdl-cffi::g sdl-cffi::b) ,struct sdl-cffi::SDL-Color)
       (setf sdl-cffi::r (r ,color)
	     sdl-cffi::g (g ,color)
	     sdl-cffi::b (b ,color)))
     ,@body))

(defmethod color= (color1 color2)
  (declare (ignore color1 color2))
  nil)

(defmethod color= ((color1 color) (color2 color))
  (and (eq (r color1) (r color2))
       (eq (g color1) (g color2))
       (eq (b color1) (b color2))))

(defmethod color= ((color1 color-a) (color2 color-a))
  (and (eq (r color1) (r color2))
       (eq (g color1) (g color2))
       (eq (b color1) (b color2))
       (eq (a color1) (a color2))))

(defmethod any-color-but-this (color)
  (color :r (if (> (r color) 254)
		0
		255)))


