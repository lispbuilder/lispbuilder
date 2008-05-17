;; SDL (Simple Media Layer) library using CFFI for foreign function interfacing...
;; (C)2006 Justin Heyes-Jones <justinhj@gmail.com> and Luke Crook <luke@balooga.com>
;; Thanks to Frank Buss and Surendra Singh
;; see COPYING for license
;; This file contains some useful functions for using SDL from Common lisp
;; using sdl.lisp (the CFFI wrapper)

(in-package #:lispbuilder-sdl)

(defclass sdl-surface ()
  ((foreign-pointer-to-surface
    :reader fp
    :initform nil
    :initarg :surface)
   (foreign-pointer-to-position-rect
    :reader fp-position
    :initform (cffi:foreign-alloc 'sdl-cffi::sdl-rectangle)
    :initarg :position)
   (foreign-pointer-to-cell-rect
    :accessor fp-cell
    :initform (cffi:null-pointer)
    :initarg cell))
  (:documentation
   "A wrapper for a foreign object of type SDL_Surface. A `SDL-SURFACE` object contains:
* a foreign SDL_Surface, 
* a foreign SDL_Rect used to position the surface for blitting operations, and 
* an SDL_Rect that defines the bounds of the surface to use when blitting."))

;;; An object of type display should never be finalized by CFFI
;;; at time of garbage collection.
(defclass display-surface (sdl-surface) ()
  (:documentation
   "A subclass of `SDL-SURFACE` that holds the surface for the current *display*. 
This object will never be garbage collected."))

(defmethod initialize-instance :around ((surface sdl-surface) &key)
  (unless (initialized-sub-systems-p)
    (error "ERROR: The SDL library must be initialized prior to use."))
  (call-next-method))

;;; An object of type is finalized by CFFI
;;; at time of garbage collection.
(defclass surface (sdl-surface) ()
  (:documentation
   "A subclass of 'SDL-SURFACE' that holds a standard SDL_Surface object.
This object will be garbage collected and the surface freed when out of scope."))

(defclass rectangle-array ()
  ((foreign-pointer-to-rectangle :accessor fp :initform nil :initarg :rectangle)
   (length :reader len :initform nil :initarg :length)))

(defmethod initialize-instance :after ((surface display-surface) &key)
  (unless (is-valid-ptr (fp surface))
    (error "SURFACE must be a foreign pointer."))
  (setf (width surface) (sdl-base::surf-w (fp surface))
	(height surface) (sdl-base::surf-h (fp surface))))

(defmethod initialize-instance :after ((surface surface)
				       &key (x 0) (y 0) (bpp 32)
				       (key-color nil)
				       (alpha nil)
				       (channel-alpha nil)
				       (surface-alpha nil)
				       (type :sw) (rle-accel t)
				       (width nil) (height nil)
				       &allow-other-keys)
  (cond
    ((is-valid-ptr (fp surface))
     ;; SDL_Surface specified.
     (setf (width surface) (sdl-base::surf-w (fp surface))
	   (height surface) (sdl-base::surf-h (fp surface))
	   (x surface) x
	   (y surface) y)
     (when key-color
       (set-color-key key-color :surface surface :rle-accel rle-accel))
     (set-alpha surface-alpha :surface surface :rle-accel rle-accel))
    ((and (integerp width) (integerp height))
     (when alpha
       (setf channel-alpha alpha
	     surface-alpha alpha))
     ;; No SDL_Surface.
     ;; But WIDTH and HEIGHT are specified, so create a new surface.
     (setf (fp surface) (sdl-base::create-surface width height
						  :bpp bpp
						  :color-key key-color
						  :channel-alpha channel-alpha
						  :surface-alpha surface-alpha
						  :type type
						  :rle-accel rle-accel))
     (unless (is-valid-ptr (fp surface))
       (error "Cannot create a new surface.")))
    (t (error ":SURFACE must be a foreign pointer to an SDL_Surface, or
:WIDTH and :HEIGHT must specified."))))

(defun surface (surface-fp &optional (display nil))
  "Creates a new `SURFACE` or a new `DISPLAY-SURFACE` when `DISPLAY` is `T`. 
`SURFACE-FP` must be a pointer to a valid SDL_Surface foreign object."
  (if display
      (make-instance 'display-surface :surface surface-fp)
      (make-instance 'surface :surface surface-fp)))

(defmacro with-surface ((var &optional surface (free-p t))
			&body body)
  (let ((surface-ptr (gensym "surface-prt-"))
	(body-value (gensym "body-value-")))
    `(let* ((,@(if surface
		   `(,var ,surface)
		   `(,var ,var)))
	    (*default-surface* ,var)
	    (,body-value nil))
       (symbol-macrolet ((,(intern (string-upcase (format nil "~A.width" var))) (width ,var))
			 (,(intern (string-upcase (format nil "~A.height" var))) (height ,var))
			 (,(intern (string-upcase (format nil "~A.x" var))) (x ,var))
			 (,(intern (string-upcase (format nil "~A.y" var))) (y ,var)))
                (declare (ignorable ,(intern (string-upcase (format nil "~A.width" var)))
                                    ,(intern (string-upcase (format nil "~A.height" var)))
                                    ,(intern (string-upcase (format nil "~A.x" var)))
                                    ,(intern (string-upcase (format nil "~A.y" var)))))
	 (sdl-base::with-surface (,surface-ptr (fp ,var) nil)
	   (setf ,body-value (progn ,@body))))
       (when ,free-p
	 (free-surface ,var))
       ,body-value)))

(defmacro with-surface-slots ((var &optional surface)
			      &body body)
  `(with-surface (,var ,surface nil)
     ,@body))

(defmacro with-surfaces (bindings &rest body)
  (if bindings
      (return-with-surface bindings body)))

(defun return-with-surface (bindings body)
  (if bindings
      `(with-surface (,@(car bindings))
	 ,(return-with-surface (cdr bindings) body))
      `(progn ,@body)))

(defmacro with-locked-surface ((var &optional surface)
			       &body body)
  (let ((surface-ptr (gensym "surface-prt-"))
	(body-value (gensym "body-value-")))
    `(let* ((,@(if surface
		   `(,var ,surface)
		   `(,var ,var)))
	    (*default-surface* ,var)
	    (,body-value nil))
       (sdl-base::with-locked-surface (,surface-ptr (fp ,var))
	 (setf ,body-value (progn ,@body)))
       ,body-value)))

(defmacro with-locked-surfaces (bindings &rest body)
  (if bindings
      (return-with-locked-surfaces bindings body)))

(defun return-with-locked-surfaces (bindings body)
  (if bindings
      `(with-locked-surface (,@(car bindings))
	 ,(return-with-locked-surfaces (cdr bindings) body))
      `(progn ,@body)))


(defmethod free-surface ((surface sdl-surface)) nil)

(defmethod free-surface ((self surface))
  "Free the foreign SDL_Surface and the SDL_Rect used for position.
Also free the SDL_Rect used as the cell mask."
  (tg:cancel-finalization self)
  (let ((surface-pointer (fp self))
	(fp-position (fp-position self))
	(fp-cell (fp-cell self)))
    (when (is-valid-ptr surface-pointer)
      (setf (slot-value self 'foreign-pointer-to-surface) nil)
      (sdl-cffi::sdl-free-surface surface-pointer))
    (when (is-valid-ptr fp-position)
      (setf (slot-value self 'foreign-pointer-to-position-rect) nil)
      (cffi:foreign-free fp-position))
    (when (is-valid-ptr fp-cell)
      (setf (slot-value self 'foreign-pointer-to-cell-rect) nil)
      (cffi:foreign-free fp-cell))))

(defmethod width ((surface sdl-surface))
  "Returns the width of the surface `SURFACE` as an `INTEGER`."
  (sdl-base::surf-w (fp surface)))
(defmethod (setf width) (w-val (surface sdl-surface))
  "Sets the width of the surface `SURFACE`. Must be an `INTEGER`."
  (setf (sdl-base::rect-w (fp-position surface)) w-val))

(defmethod height ((surface sdl-surface))
  "Returns the height of the surface `SURFACE` as an `INTEGER`."
  (sdl-base::surf-h (fp surface)))
(defmethod (setf height) (h-val (surface sdl-surface))
  "Sets the height of the surface `SURFACE`. Must be an `INTEGER`."
  (setf (sdl-base::rect-h (fp-position surface)) h-val))

(defmethod x ((surface sdl-surface))
  "Returns the x position coordinate of the surface `SURFACE` as an `INTEGER`."
  (sdl-base::rect-x (fp-position surface)))
(defmethod (setf x) (x-val (surface sdl-surface))
  "Returns the `X` position coordinate of the surface `SURFACE`. Must be an `INTEGER`."
  (setf (sdl-base::rect-x (fp-position surface)) x-val))

(defmethod y ((surface sdl-surface))
  "Returns the y position coordinate of the surface `SURFACE` as an `INTEGER`."
  (sdl-base::rect-y (fp-position surface)))
(defmethod (setf y) (y-val (surface sdl-surface))
  "Returns the `Y` position coordinate of the surface `SURFACE`. Must be an `INTEGER`."  
  (setf (sdl-base::rect-y (fp-position surface)) y-val))

(defmethod point-* ((surface sdl-surface))
  "Returns the `X` and `Y` position coordinates of the surface `SURFACE` as a spread."
  (values (x surface) (y surface)))

(defmethod get-point ((surface sdl-surface))
  "Returns the `X` and `Y` position coordinates of the surface `SURFACE` as a `POINT`."
  (vector (x surface) (y surface)))

(defmethod set-point ((surface sdl-surface) (position vector))
  "Sets the `X` and `Y` position coordinates of the surface `SURFACE`. `POSITION` is a `POINT`."
  (set-point-* surface :x (x position) :y (y position))
  surface)

(defmethod set-point-* ((surface sdl-surface) &key x y)
  "Sets the `X` and `Y` position coordinates of the surface `SURFACE`. `X` and `Y` are `INTEGERS`."
  (when x (setf (x surface) x))
  (when y (setf (y surface) y))
  surface)

(defmethod position-* ((surface sdl-surface))
  "See [POSITION](#position)."
  (values (x surface) (y surface)))

(defmethod get-position ((surface sdl-surface))
  "See [GET-POINT](#get-point)."
  (point :x (x surface) :y (y surface)))

(defmethod set-position ((surface sdl-surface) (position vector))
    "See [SET-POINT](#set-point)."
  (set-position-* surface :x (x position) :y (y position))
  surface)

(defmethod set-position-* ((surface sdl-surface) &key x y)
  "See [SET-POINT-*](#set-point-*)."
  (when x (setf (x surface) x))
  (when y (setf (y surface) y))
  surface)

(defmethod set-surface ((surface sdl-surface) (position vector))
"Sets the coordinates of the surface `SURFACE` to `POSITION`, 
where position is of type `POINT`."
(set-surface-* surface :x (x position) :y (y position))
  surface)

(defmethod set-surface-* ((surface sdl-surface) &key x y)
"Sets the coordinates of the surface `SURFACE` to the spead `X` and `Y` `INTEGER` coordinates.
`X` and `Y` are `KEY`word parameters having default values of `0` if unspecified."
  (when x (setf (x surface) x))
  (when y (setf (y surface) y))
  surface)

(defmethod rectangle-* ((surface sdl-surface))
  "Returns the `X`, `Y`, `WIDTH` and `HEIGHT` values of the surface `SURFACE` as a spread. 
The `RESULT` is `\(VALUES X Y WIDTH HEIGHT\)`"
  (values (x surface) (y surface) (width surface) (height surface)))

(defmethod get-rectangle-* ((surface sdl-surface))
  "Creates and returns a `RECTANGLE` object from the `X`, `Y`, `WIDTH` and `HEIGHT` 
values in the surface `SURFACE`."
  (rectangle :x (x surface)
	     :y (y surface)
	     :w (width surface)
	     :h (height surface))
  surface)

(defun clear-color-key (&key (surface *default-surface*) (rle-accel t))
  (check-type surface sdl-surface)
  (sdl-base::clear-color-key (fp surface) rle-accel))

(defun set-color-key (color &key (surface *default-surface*) (rle-accel t))
  "Sets the color key (transparent pixel) `COLOR` in a blittable surface `SURFACE`.

##### Paremeters

* `COLOR` the color to be used as the transparent pixel of the surface when blitting, 
of type [COLOR](#color), or [COLOR-A](#color-a). When 'NIL' will disable color keying.
* `RLE-ACCEL` when `T` will use RLE information when blitting. See [RLE-CCEL](#rle-accel)."
  (check-type surface sdl-surface)
  (check-type color sdl-color)
  (sdl-base::set-color-key (fp surface) (map-color color surface ) rle-accel))
  
(defun set-alpha (alpha &key (surface *default-surface*) (rle-accel nil))
  "Adjust the alpha `ALPHA` properties of a surface `SURFACE`. Also enables or disables alpha blending. 

##### Parameters

* `ALPHA` when `NIL` will ignore all alpha information when blitting the surface. `ALPHA` when not `NIL` is an `INTEGER`
value between `0` and `255` with `0` being transparent and `255` being opaque. *Note*: The per-surface alpha value of 
128 is considered a special case and is optimised, so it's much faster than other per-surface values. 
* `RLE-ACCEL` when `T` wil use RLE information when blitting. See [RLE-ACCEL](#rle-accel).

##### Alpha effects

Alpha has the following effect on surface blitting: 

* _RGBA to RGB_ with [SDL-SRC-ALPHA](#sdl-src-alpha): 
The source is alpha-blended with the destination, using the alpha channel. [SDL-SRC-COLOR-KEY](#SDL-SRC-COLOR-KEY) 
and the per-surface alpha are ignored.
* _RGBA to RGB_ without [SDL-SRC-ALPHA](#sdl-src-alpha): 
The RGB data is copied from the source. The source alpha channel and the per-surface alpha value are ignored. 
If [SDL-SRC-COLOR-KEY](#SDL-SRC-COLOR-KEY) is set, only the pixels not matching the colorkey value are copied.
* _RGB to RGBA_ with [SDL-SRC-ALPHA](#sdl-src-alpha): 
The source is alpha-blended with the destination using the per-surface alpha value. If 
[SDL-SRC-COLOR-KEY](#SDL-SRC-COLOR-KEY) is set, only the pixels not matching the colorkey value are copied. 
The alpha channel of the copied pixels is set to opaque.
* _RGB to RGBA_ without [SDL-SRC-ALPHA](#sdl-src-alpha): 
The RGB data is copied from the source and the alpha value of the copied pixels is set to opaque. 
If [SDL-SRC-COLOR-KEY](#SDL-SRC-COLOR-KEY) is set, only the pixels not matching the colorkey value are copied.
* _RGBA to RGBA_ with [SDL-SRC-ALPHA](#sdl-src-alpha): 
The source is alpha-blended with the destination using the source alpha channel. The alpha channel in the destination 
surface is left untouched. [SDL-SRC-COLOR-KEY](#SDL-SRC-COLOR-KEY) is ignored.
* _RGBA to RGBA_ without [SDL-SRC-ALPHA](#sdl-src-alpha):	
The RGBA data is copied to the destination surface. If [SDL-SRC-COLOR-KEY](#SDL-SRC-COLOR-KEY) is set, 
only the pixels not matching the colorkey value are copied.
* _RGB to RGB_ with [SDL-SRC-ALPHA](#sdl-src-alpha): 
The source is alpha-blended with the destination using the per-surface alpha value. 
If [SDL-SRC-COLOR-KEY](#SDL-SRC-COLOR-KEY) is set, only the pixels not matching the colorkey value are copied.
* _RGB to RGB_ without [SDL-SRC-ALPHA](#sdl-src-alpha):
The RGB data is copied from the source. If [SDL-SRC-COLOR-KEY](#SDL-SRC-COLOR-KEY) is set, only the pixels not 
matching the colorkey value are copied.

*Note*: When blitting, the presence or absence of [SDL-SRC-ALPHA](#sdl-src-alpha) is relevant only on the 
source surface, not the destination. 

*Note*: Note that _RGBA to RGBA_ blits (with [SDL-SRC-ALPHA](#sdl-src-alpha) set) keep the alpha of the destination 
surface. This means that you cannot compose two arbitrary _RGBA_ surfaces this way and get the result you would 
expect from \"overlaying\" them; the destination alpha will work as a mask. 

*Note*: Also note that per-pixel and per-surface alpha cannot be combined; the per-pixel alpha is always used 
if available."
  (check-type surface sdl-surface)
  (sdl-base::set-alpha (fp surface) alpha rle-accel))

(defun get-clip-rect (&key (surface *default-surface*) (rectangle (rectangle)))
  (check-type surface sdl-surface)
  (check-type rectangle rectangle)
  (sdl-base::get-clip-rect (fp surface) (fp rectangle))
  rectangle)

(defun set-clip-rect (rectangle &key (surface *default-surface*))
  (check-type surface sdl-surface)
  (check-type rectangle rectangle)
  (sdl-base::set-clip-rect (fp surface) (fp rectangle))
  rectangle)

(defun clear-cell (&key (surface *default-surface*))
  (check-type surface sdl-surface)
  (unless (cffi:null-pointer-p (fp-cell surface))
    (cffi:foreign-free (fp-cell surface))
    (setf (fp-cell surface) (cffi:null-pointer))))

(defun set-cell (rectangle &key (surface *default-surface*))
  (check-type surface sdl-surface)
  (check-type rectangle rectangle)
  (if (cffi:null-pointer-p (fp-cell surface))
      (setf (fp-cell surface) (sdl-base::clone-rectangle (fp rectangle)))
      (sdl-base::copy-rectangle (fp rectangle) (fp-cell surface))))

(defun set-cell-* (x y w h &key (surface *default-surface*))
  (check-type surface sdl-surface)
  (when (cffi:null-pointer-p (fp-cell surface))
    (setf (fp-cell surface) (sdl-base::rectangle)))
  (setf (sdl-base::rect-x (fp-cell surface)) x
	(sdl-base::rect-y (fp-cell surface)) y
	(sdl-base::rect-w (fp-cell surface)) w
	(sdl-base::rect-h (fp-cell surface)) h))

(defun get-surface-rect (&key (surface *default-surface*) (rectangle (rectangle)))
  (check-type surface sdl-surface)
  (check-type rectangle rectangle)
  (sdl-base::get-surface-rect (fp surface) (fp rectangle))
  rectangle)

(defun convert-surface (&key (surface *default-surface*) key-color alpha surface-alpha (free-p nil))
  (check-type surface sdl-surface)
  (when key-color
    (check-type key-color sdl-color))
  (when alpha
    (setf surface-alpha alpha))
  (let ((surf (sdl-base::convert-surface-to-display-format (fp surface)
							   :key-color (when key-color (map-color key-color surface))
							   :surface-alpha surface-alpha
							   :free-p nil)))
    (unless (sdl-base::is-valid-ptr surf)
      (error "ERROR, CONVERT-SURFACE: Cannot convert the surface."))
    (when free-p
      (free-surface surface))
    (make-instance 'surface :surface surf :alpha alpha :surface-alpha surface-alpha)))

(defun copy-surface (&key (surface *default-surface*) key-color alpha channel-alpha surface-alpha (type :sw) rle-accel)
  (check-type surface sdl-surface)
  (when key-color
    (check-type key-color sdl-color))
  (when alpha
    (setf channel-alpha alpha
	  surface-alpha alpha))
  (let ((surf (sdl-base::copy-surface (fp surface)
				      :color-key (when key-color (map-color key-color surface))
				      :channel-alpha channel-alpha
				      :surface-alpha surface-alpha
				      :type type
				      :rle-accel rle-accel)))
    (unless (sdl-base::is-valid-ptr surf)
      (error "ERROR, copy-surface: Cannot copy the surface."))
    (setf surf (surface surf))
    (when key-color
      (set-color-key key-color :surface surf :rle-accel rle-accel))
    (set-alpha surface-alpha :surface surf :rle-accel rle-accel)
    surf))

(defun create-surface (width height &key
		       (surface nil) (bpp 32) key-color channel-alpha surface-alpha alpha (type :sw) (rle-accel t))
  
  (when key-color
    (check-type key-color sdl-color))
  ;; ALPHA is deprecated, and should not be used anymore.
  ;; Use SURFACE-ALPHA and CHANNEL-ALPHA instead.
  (when alpha
    (setf channel-alpha alpha
	  surface-alpha alpha))
  (let ((surf (sdl-base::create-surface width height
					:bpp bpp
					:surface (when surface (fp surface))
					:color-key key-color
					:channel-alpha channel-alpha
					:surface-alpha surface-alpha
					:type type
					:rle-accel rle-accel)))
    (unless (sdl-base::is-valid-ptr surf)
      (error "ERROR, CREATE-SURFACE: Cannot create a new surface."))
    ;; (setf surf (surface surf))
    (setf surf (make-instance 'surface :surface surf))
    (when key-color
      (set-color-key key-color :surface surf :rle-accel rle-accel))
    (set-alpha surface-alpha :surface surf :rle-accel rle-accel)
    surf))

(defun update-surface (surface &optional template)
  (check-type surface sdl-surface)
  (if template
    (progn
      (check-type template rectangle)
      (sdl-base::update-surface (fp surface) :template (fp template)))
    (sdl-base::update-surface (fp surface)))
  surface)

(defun update-surface-* (surface x y w h)
  (check-type surface sdl-surface)
  (sdl-base::update-surface (fp surface) :x x :y y :w w :h h)
  surface)

(defun blit-surface (src &optional (surface *default-surface*))
  (unless surface
    (setf surface *default-display*))
  (check-types sdl-surface src surface)
  (sdl-base::blit-surface (fp src) (fp surface) (fp-cell src) (fp-position src))
  src)

(defun draw-surface (src &key (surface *default-surface*))
  (unless surface
    (setf surface *default-display*))
  (check-types sdl-surface src surface)

  ;; (cffi:with-foreign-object (temp-dest 'sdl-cffi::sdl-rect)
;;     (setf (sdl-base:rect-x temp-dest) (x src)
;; 	  (sdl-base:rect-y temp-dest) (y src))  
;;     (sdl-base::blit-surface (fp src) (fp surface) (fp-cell src) temp-dest))
  (sdl-base::blit-surface (fp src) (fp surface) (fp-cell src) (fp-position src))
  src)

(defun draw-surface-at-* (src x y &key (surface *default-surface*))
  (set-surface-* src :x x :y y)
  (draw-surface src :surface surface))

(defun draw-surface-at (src point &key (surface *default-surface*))
  (set-surface src point)
  (draw-surface src :surface surface))

(defun fill-surface (color &key
		     (template nil)
		     (surface *default-surface*) (update-p nil) (clipping-p nil))
  "Fill the surface with the specified color COLOR.
   Use :template to specify the SDL_Rect to be used as the fill template.
   Use :update-p to call SDL_UpdateRect, using :template if provided. This allows for a 
   'dirty recs' screen update."
  (check-type color sdl-color)
  (fill-surface-* (r color) (g color) (b color) :a (a color)
		  :template template :surface surface :update-p update-p :clipping-p clipping-p)
  surface)

(defun fill-surface-* (r g b &key (a nil) (template nil)
		       (surface *default-surface*) (update-p nil) (clipping-p t))
  "Fill the surface with the specified color R G B :A A.
   Use :TEMPLATE to specify the SDL_Rectangle to be used as the fill template.
   Use :UPDATE-P to call SDL_UpdateRect, using :TEMPLATE if provided. This allows for a 
   'dirty recs' screen update."
  (unless surface
    (setf surface *default-display*))
  (check-type surface sdl-surface)
  (when template
    (check-type template rectangle))
  (sdl-base::fill-surface (fp surface)
			  (map-color-* r g b a surface)
			  :template (if template
					(fp template)
					(cffi:null-pointer))
			  :clipping-p clipping-p
			  :update-p update-p)
  surface)

(defun copy-channel-to-alpha (destination source &key (channel :r))
  (check-types sdl-surface destination source)
  (sdl-base::with-pixels ((src (fp source))
			  (dst (fp destination)))
    (loop for y from 0 below (height destination)
	 do (loop for x from 0 below (width destination)
	       do (multiple-value-bind (src-px src-r src-g src-b src-a)
		      (sdl-base::read-pixel src x y)
		    (declare (ignorable src-px src-r src-g src-b src-a))
		    (multiple-value-bind (dst-px dst-r dst-g dst-b dst-a)
			(sdl-base::read-pixel dst x y)
		      (declare (ignore dst-px dst-a))
		      (case channel
			(:r (sdl-base::write-pixel dst x y (sdl-base::map-color (fp destination) dst-r dst-g dst-b src-r)))
			(:g (sdl-base::write-pixel dst x y (sdl-base::map-color (fp destination) dst-r dst-g dst-b src-g)))
			(:b (sdl-base::write-pixel dst x y (sdl-base::map-color (fp destination) dst-r dst-g dst-b src-b)))
			(:a (sdl-base::write-pixel dst x y (sdl-base::map-color (fp destination) dst-r dst-g dst-b src-a)))
			(otherwise (error "copy-channel-to-alpha: CHANNEL must be one of :R, :G, :B or :A"))))))))
  destination)

;; (defun set-mask-alpha (surface threshold)
;;   (check-type surface sdl-surface)
;;   (let ((mask (make-array (* (width surface) (height surface))
;; 			  :element-type 'bit
;; 			  :initial-contents nil)))
;;     (sdl-base::with-pixels (px (fp surface)))
;;     ))

;; (defun get-mask-alpha (surface))

;; (defun set-mask-color-key (surface color-key)
;;   (check-type surface sdl-surface)
;;   (let ((mask (make-array (* (width surface) (height surface))
;; 			  :element-type 'bit
;; 			  :initial-contents nil))
;; 	(mask-color (map-color color-key surface)))
;;     (sdl-base::with-pixels (px (fp surface))
;;       (dotimes (y (height surface))
;; 	(dotimes (x (width surface))
;; 	  (let ((col (sdl-base::read-pixel px x y)))
;; 	    (if (eql col mask-color)
;; 		(setf (sbit mask (* x y)) t))))))
;;     (setf (slot-value surface 'color-key-mask) mask)))

;; (defun get-mask-key-color ())

;; (defun set-mask-colors (surface &rest colors)
;;   (let ((mask-colors (mapcar #'(lambda (color)
;; 				 (map-color color surface))))
;; 	(mask (make-array (* (width surface) (height surface))
;; 			  :element-type 'bit
;; 			  :initial-contents nil)))
;;     (sdl-base::with-pixels (px (fp surface))
;;       ())
;;     ))