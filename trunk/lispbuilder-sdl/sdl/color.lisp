;; lispbuilder-sdl
;; (C)2006 Luke Crook <luke@balooga.com>

(in-package #:lispbuilder-sdl)

(defclass sdl-color () ()
  (:documentation "Root class defining an SDL color. All colors in SDL must inherit from this class."))

(defclass color (sdl-color)
  ((color-vector :accessor fp :initform (vector 0 0 0) :initarg :color))
  (:documentation "An SDL color containing `INTEGER` Red, Green and Blue color components."))

(defclass color-a (color)
  ((color-vector :accessor fp :initform (vector 0 0 0 0) :initarg :color))
  (:documentation "An SDL color containing `INTEGER` Red, Green, Blue and Alpha color components."))

(defclass foreign-color (sdl-color)
  ((foreign-pointer-to-color :accessor fp :initform nil :initarg :color)))

(defun color (&key (r 0) (g 0) (b 0) (a nil))
  "Creates and returns a new [COLOR](#color) from the soecified red `R`, green `G`, and blue `B` color components.
When `A` is an `INTEGER`, will return a new [COLOR-A](#color-a) with the alpha transparency."
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
  "A convience macro that binds `\*DEFAULT-COLOR\*` to `VAR` within the scope of `WITH-COLOR`. 
`VAR` is set to `COLOR` when `COLOR` is not `NIL`.`VAR` must be of type `SDL-COLOR`. 
`VAR` is freed using [FREE-COLOR](#free-color) when `FREE-P` is not `NIL`."
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
  "Returns the red color component of the color `COLOR` as an `INTEGER`."
  (svref (fp color) 0))
(defmethod (setf r) (r-val (color color))
  "Sets the red `R` color component of the color `COLOR` as an `INTEGER`."
  (setf (svref (fp color) 0) (cast-to-int r-val)))

(defmethod g ((color color))
  "Returns the green color component of the color `COLOR` as an `INTEGER`."  
  (svref (fp color) 1))
(defmethod (setf g) (g-val (color color))
  "Sets the redgreen `G` color component of the color `COLOR` as an `INTEGER`."
  (setf (svref (fp color) 1) (cast-to-int g-val)))

(defmethod b ((color color))
  "Returns the blue color component of the color `COLOR` as an `INTEGER`."  
  (svref (fp color) 2))
(defmethod (setf b) (b-val (color color))
  "Sets the blue `B` color component of the color `COLOR` as an `INTEGER`."
  (setf (svref (fp color) 2) (cast-to-int b-val)))

(defmethod a ((color color))
  "Returns `NIL` as `COLOR` has no alpha component."
  nil)

(defmethod a ((color color-a))
  "Returns the alpha color component of the color `COLOR` as an `INTEGER`."
  (svref (fp color) 3))
(defmethod (setf a) (a-val (color color-a))
  "Sets the alpha `A` color component of the color `COLOR` as an `INTEGER`."
  (setf (svref (fp color) 3) (cast-to-int a-val)))

(defmethod set-color ((dst sdl-color) (src sdl-color))
  "Copies the color components from the source color `SRC` to
the destination color `DST`."
  (set-color-* dst :r (r src) :g (g src) :b (b src) :a (a src))
  dst)

(defmethod set-color-* ((color sdl-color) &key r g b a)
  "Sets the components of the color `COLOR` to 
the red `R`, green `G`, blue `BLUE` or alpha `A` color components."
  (when r (setf (r color) r))
  (when g (setf (g color) g))
  (when b (setf (b color) b))
  (when a (setf (a color) a))
  color)

(defmethod color-* ((color color))
  "Returns the read, green and blue components of the color `COLOR` as a spread."
  (values (r color) (g color) (b color)))

(defmethod color-* ((color color-a))
  "Returns the read, green, blue and alpha components of the color `COLOR` as a spread."
  (values (r color) (g color) (b color) (a color)))

(defmethod color-* ((color foreign-color))
  "Not implemented yet. Returns `NIL`"
  nil)

(defmethod map-color ((color color) &optional (surface *default-surface*))
  "Maps the RBG color `COLOR` to the pixel format of the surface `SURFACE` and returns 
the pixel value that best approximates the color value of the 
surface `SURFACE`. If the surface has a palette \(8-bit\) the index of the 
closest matching color in the palette will be returned. If the surface has an 
alpha component it will be returned as all `1` bits \(fully opaque\). If the surface 
color depth is less than 32-bpp then the unused upper bits of the return value can safely be ignored 
\(e.g., with a 16-bpp format the return value can be assigned to a Uint16, and similarly a Uint8 for an 8-bpp format\)."
  (sdl-cffi::sdl-map-rgb (sdl-base::pixel-format (fp surface))
			 (r color) (g color) (b color)))

(defmethod map-color ((color color-a) &optional (surface *default-surface*))
  "Maps the RBGA color `COLOR` to the pixel format of the surface `SURFACE` and returns 
the pixel value that best approximates the color value of the 
surface `SURFACE`. If the surface has a palette \(8-bit\) the index of the 
closest matching color in the palette will be returned. If the surface 
 has no alpha component the alpha value will be ignored \(as it will be in formats with a palette\). 
If the surface color depth is less than 32-bpp then the unused upper bits of the return value can safely be ignored 
\(e.g., with a 16-bpp format the return value can be assigned to a Uint16, and similarly a Uint8 for an 8-bpp format\)."
  (sdl-cffi::sdl-map-rgba (sdl-base::pixel-format (fp surface))
			  (r color) (g color) (b color) (a color)))

(defmethod pack-color ((color color))
  "Packs the RGB color components in color into a four byte `INTEGER`."
  (let ((col #x00000000))
    (setf col (logior (ash (r color) 24)
		      (ash (g color) 16)
		      (ash (b color) 8)
		      #xFF))
    col))

(defmethod pack-color ((color color-a))
  "Packs the RGBA color components in color into a four byte `INTEGER`."
  (let ((col #x00000000))
    (setf col (logior (ash (r color) 24)
		      (ash (g color) 16)
		      (ash (b color) 8)
		      (a color)))
    col))


(defmethod free-color ((color sdl-color)) nil)

(defmethod free-color ((color foreign-color))
  (cffi:foreign-free (fp color))
  (tg:cancel-finalization color))

(defmacro with-foreign-color-copy ((struct color) &body body)
  "Creates a new foreign SDL_Color object `STRUCT on the stack. Then copies the color components from COLOR into STRUCT.
STRUCT is free'd after BODY." 
  `(cffi:with-foreign-object (,struct 'sdl-cffi::SDL-Color)
     (cffi:with-foreign-slots ((sdl-cffi::r sdl-cffi::g sdl-cffi::b) ,struct sdl-cffi::SDL-Color)
       (setf sdl-cffi::r (r ,color)
	     sdl-cffi::g (g ,color)
	     sdl-cffi::b (b ,color)))
     ,@body))

(defmethod color= (color1 color2)
  "Returns `NIL`. This is a catch all when an `RGB` color is compared with an `RGBA` color."
  (declare (ignore color1 color2))
  nil)

(defmethod color= ((color1 color) (color2 color))
  "Returns `T` if the `RGB` colors match, returns `NIL` otherwise."  
  (and (eq (r color1) (r color2))
       (eq (g color1) (g color2))
       (eq (b color1) (b color2))))

(defmethod color= ((color1 color-a) (color2 color-a))
  "Returns `T` if the `RGBA` colors match, returns `NIL` otherwise."
  (and (eq (r color1) (r color2))
       (eq (g color1) (g color2))
       (eq (b color1) (b color2))
       (eq (a color1) (a color2))))

(defmethod any-color-but-this (color)
  "Returns a new color that is different to the color `COLOR`."
  (color :r (if (> (r color) 254)
		0
		255)))


