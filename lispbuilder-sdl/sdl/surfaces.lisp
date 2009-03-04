;; SDL (Simple Media Layer) library using CFFI for foreign function interfacing...
;; (C)2006 Justin Heyes-Jones <justinhj@gmail.com> and Luke Crook <luke@balooga.com>
;; Thanks to Frank Buss and Surendra Singh
;; see COPYING for license
;; This file contains some useful functions for using SDL from Common lisp
;; using sdl.lisp (the CFFI wrapper)

(in-package #:lispbuilder-sdl)

(defclass sdl-surface (foreign-object)
  ((position-rect
    :reader position-rect
    :initform (rectangle)
    :initarg :position)
   (cell-index
    :accessor cell-index
    :initform 0
    :initarg :cell-index)
   (cells
    :accessor cells
    :initform nil
    :initarg :cells)
   (display-surface-p
    :accessor display-surface-p
    :initform nil
    :initarg :display-surface-p))
  (:documentation
   "A wrapper for the foreign SDL_Surface object."))

(defclass display-surface (sdl-surface) ()
  (:default-initargs
   :display-surface-p t
    :gc nil
    :free #'(lambda (fp) fp))
  (:documentation
   "The current display surface. Can be accessed using `SDL:*DEFAULT-DISPLAY*`."))

(defmethod free ((self display-surface))
  "Freeing the display surface is not a valid operation."
  nil)

(defmethod initialize-instance :around ((surface sdl-surface) &key)
  (unless (initialized-subsystems-p)
    (error "ERROR: The SDL library must be initialized prior to use."))
  (call-next-method))

(defclass surface (sdl-surface) ()
  (:default-initargs
   :display-surface-p nil
    :gc t
    :free (simple-free 'sdl-cffi::sdl-free-surface 'surface))
  (:documentation
   "This object is garbage collected and the `SDL_Surface` object freed when out of scope.
Free using [FREE](#free)."))

(defclass rectangle-array ()
  ((foreign-pointer-to-rectangle :accessor fp :initform nil :initarg :rectangle)
   (length :reader len :initform nil :initarg :length)))

(defmethod initialize-instance :after ((surface surface)
				       &key using-surface
				       width height x y bpp
				       enable-color-key
				       color-key
				       color-key-at
				       enable-alpha
				       alpha
				       pixel-alpha
				       (type :sw)
				       rle-accel
				       &allow-other-keys)
  ;; A surface can be created any of four ways:
  ;; 1) Using SDL_Surface in :SURFACE only. No surface parameters may be set.
  ;; 2) Creating a new SDL_Surface from the SDL_Surface when :SURFACE and :WIDTH & :HEIGHT are set. Surface parameters may be set.
  ;; 3) Using SDL_Surface when :USING-SURFACE is set. Surface parameters may be set.
  ;; 4) Creating a new SDL_Surface when :WIDTH & :HEIGHT are set. Surface parameters may be set.

  ;; Takes care of (2) and (4)
  (if (and width height)
      (setf (slot-value surface 'foreign-pointer-to-object)
	    (sdl-base::create-surface width
				      height
				      :surface (or (fp surface) using-surface)
				      :bpp bpp
				      :enable-color-key enable-color-key
				      :pixel-alpha pixel-alpha
				      :enable-alpha enable-alpha
				      :type type
				      :rle-accel rle-accel))
      ;; Takes care of (3)
      (if using-surface
	  (setf (slot-value surface 'foreign-pointer-to-object)
		using-surface)))

  (unless (cells surface)
    (setf (cells surface) (vector (rectangle)))
    (set-cell-* (x surface) (y surface) (width surface) (height surface) :surface surface))

  (when x (setf (x surface) x))
  (when y (setf (y surface) y))
  
  (when (or (and surface width height)
	    (and width height)
	    using-surface)
    (setf (color-key-enabled-p surface) enable-color-key
	  (alpha-enabled-p surface) enable-alpha)
    (when color-key
      (setf (color-key surface) color-key))
    (when color-key-at
      (setf (color-key surface) (read-pixel color-key-at :surface surface)))
    (when alpha
      (setf (alpha surface) alpha))))

(defmacro with-surface ((var &optional surface (free t))
			&body body)
  (let ((surface-ptr (gensym "surface-prt-"))
	(body-value (gensym "body-value-"))
	(free-value (gensym "free-value-")))
    `(let* ((,@(if surface
		   `(,var ,surface)
		   `(,var ,var)))
	    (*default-surface* ,var)
	    (,body-value nil)
	    (,free-value ,free))
       (declare (ignorable ,free-value))
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
       ,(if free
          `(progn
	     (when ,free-value
	       (free ,var))
	     ,body-value)
	  `(progn
	     ,body-value)))))

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


(defmethod width ((surface sdl-surface))
  "Returns the width of `SURFACE` as an `INTEGER`."
  (sdl-base::surf-w (fp surface)))
;; (defmethod (setf width) (w-val (surface sdl-surface))
;;   "Sets the width of the surface `SURFACE`. Must be an `INTEGER`."
;;   (setf (width (position-rect surface)) w-val))

(defmethod height ((surface sdl-surface))
  "Returns the height of `SURFACE` as an `INTEGER`."
  (sdl-base::surf-h (fp surface)))
;; (defmethod (setf height) (h-val (surface sdl-surface))
;;   "Sets the height of the surface `SURFACE`. Must be an `INTEGER`."
;;   (setf (height (position-rect surface)) h-val))

(defmethod x ((surface sdl-surface))
  "Returns the `X` position coordinate of `SURFACE` as an `INTEGER`."
  (x (position-rect surface)))
(defmethod (setf x) (x-val (surface sdl-surface))
  "Sets the integer `X` position coordinate of `SURFACE`."
  (setf (x (position-rect surface)) x-val))

(defmethod y ((surface sdl-surface))
  "Returns the `Y` position coordinate of `SURFACE` as an `INTEGER`."
  (y (position-rect surface)))
(defmethod (setf y) (y-val (surface sdl-surface))
  "Sets the integer `Y` position coordinate of `SURFACE`."  
  (setf (y (position-rect surface)) y-val))

(defmethod point-* ((surface sdl-surface))
  "Returns the `X` and `Y` position coordinates of `SURFACE` as a spread."
  (values (x surface) (y surface)))

(defmethod get-point ((surface sdl-surface))
  "Returns the `POINT` position coordinates of `SURFACE`."
  (vector (x surface) (y surface)))

(defmethod set-point ((surface sdl-surface) (position vector))
  "Sets the `POINT` position coordinates of `SURFACE`."
  (set-point-* surface :x (x position) :y (y position))
  surface)

(defmethod set-point-* ((surface sdl-surface) &key x y)
  "Sets the `X` and `Y` position coordinates of `SURFACE`. `X` and `Y` are integers."
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
  "See [SET-POINT](#set-point)."
  (set-surface-* surface :x (x position) :y (y position))
  surface)

(defmethod set-surface-* ((surface sdl-surface) &key x y)
  "See [SET-POINT-*](#set-point-*)."
  (when x (setf (x surface) x))
  (when y (setf (y surface) y))
  surface)

(defmethod rectangle-* ((surface sdl-surface))
  "Returns the fields `X`, `Y`, `WIDTH` and `HEIGHT` from `SURFACE` as a spread."
  (values (x surface) (y surface) (width surface) (height surface)))

(defmethod get-rectangle-* ((surface sdl-surface))
  "Returns a new `RECTANGLE` containing the `X`, `Y`, `WIDTH` and `HEIGHT` values of `SURFACE`."
  (rectangle :x (x surface)
	     :y (y surface)
	     :w (width surface)
	     :h (height surface))
  surface)

(defmethod alpha-enabled-p ((surface sdl-surface))
  (1/0->T/NIL (sdl-base::alpha-enabled-p (fp surface))))
(defmethod (setf alpha-enabled-p) (value (surface sdl-surface))
  (setf (sdl-base::alpha-enabled-p (fp surface)) value))
(defun enable-alpha (value &key (surface *default-surface*))
  "Enable surface alpha blending for when `T`. Disable surface alpha blending when `NIL`. 
A `SURFACE` need not have a pixel alpha component \(RGBA\) to use surface alpha blending."
  (setf (alpha-enabled-p surface) value))

(defmethod alpha ((surface sdl-surface))
  (sdl-base::alpha (fp surface)))
(defmethod (setf alpha) (value (surface sdl-surface))
  (setf (sdl-base::alpha (fp surface)) value))

(defmethod color-key-enabled-p ((surface sdl-surface))
  (1/0->T/NIL (sdl-base::color-key-enabled-p (fp surface))))
(defmethod (setf color-key-enabled-p) (value (surface sdl-surface))
  (setf (sdl-base::color-key-enabled-p (fp surface)) value))
(defun enable-color-key (value &key (surface *default-surface*))
  "Enables color keying when `T`. Disable color keying when `NIL`"
  (setf (color-key-enabled-p surface) value))

(defmethod color-key ((surface sdl-surface))
  (multiple-value-bind (r g b)
      (sdl-base::map-pixel (sdl-base::color-key (fp surface))
			   (fp surface))
    (color :r r :g g :b b)))
(defmethod (setf color-key) (value (surface sdl-surface))
  (setf (sdl-base::color-key (fp surface)) (map-color value surface)))

(defun clear-color-key (&key (surface *default-surface*))
  "Disables the color key."
  (check-type surface sdl-surface)
  (setf (color-key-enabled-p surface) nil))

(defmethod pixel-alpha-enabled-p ((surface sdl-surface))
  (1/0->T/NIL (sdl-base::pixel-alpha-enabled-p (fp surface))))

(defmethod rle-accel-enabled-p ((surface sdl-surface))
  (1/0->T/NIL (sdl-base::rle-accel-enabled-p (fp surface))))
(defmethod (setf rle-accel-enabled-p) (value (surface sdl-surface))
  (setf (sdl-base::rle-accel-enabled-p (fp surface)) value))
(defun enable-rle-accel (value &key (surface *default-surface*))
  "Enables RLE blit acceleration when `T`, disables RLE acceleration when `NIL`. 
RLE acceleration can substantially speed up blitting of images with large horizontal runs 
of transparent pixels (i.e., pixels that match the key color)."
  (setf (rle-accel-enabled-p surface) value))

(defmethod clip-rect ((surface sdl-surface))
  (get-clip-rect :surface surface))
(defmethod (setf clip-rect) (value (surface sdl-surface))
  (set-clip-rect value :surface surface))

(defun clear-clip-rect (&optional (surface *default-surface*))
  "Removes the clipping [RECTANGLE](#rectangle)."
  (check-type surface sdl-surface)
  (set-clip-rect NIL :surface surface)
  t)

;; (defun set-color-key (color &key (surface *default-surface*) (rle-accel t))
;;   "Set the color key \(transparent pixel\). 

;; ##### Paremeters

;; * `COLOR` the transparent pixel color, of type [COLOR](#color), or [COLOR-A](#color-a). 
;; When 'NIL' will disable color keying.
;; * `RLE-ACCEL` when `T` will use RLE information when blitting. See [RLE-CCEL](#rle-accel)."
;;   (check-type surface sdl-surface)
;;   (check-type color color)
;;   (sdl-base::set-color-key (fp surface) (map-color color surface) rle-accel))
  
;; (defun set-alpha (alpha &key (surface *default-surface*) (source-alpha nil) (rle-accel nil))
;;   "Set the transparency,  alpha blending and RLE acceleration properties of `SURFACE`.

;; ##### Parameters

;; * `ALPHA` sets the `SURFACE` transparency. Allowable values are `NIL`, or any `INTEGER` between `0` and `255` inclusive.
;; `0` is  transparent and `255` being opaque.
;; *Note*: The per-surface alpha value of 128 is considered a special case and is optimised, so it's much faster than other per-surface values. 
;; * `SOURCE-ALPHA` will enable or disable alpha blending for `SURFACE`.
;; * `RLE-ACCEL` will enable or disable RLE acceleration when blitting. See [RLE-ACCEL](#rle-accel)."
;;   (check-type surface sdl-surface)
;;   (sdl-base::set-alpha (fp surface) alpha source-alpha rle-accel))

(defun get-clip-rect (&key (surface *default-surface*) (rectangle (rectangle)))
  "Returns the clipping [RECTANGLE](#rectangle)."
  (check-type surface sdl-surface)
  (check-type rectangle rectangle)
  (sdl-base::get-clip-rect (fp surface) (fp rectangle))
  rectangle)

(defun set-clip-rect (rectangle &key (surface *default-surface*))
  "See [CLIP-RECT](#clip-rect)."
  (check-type surface sdl-surface)
  (when rectangle (check-type rectangle rectangle))
  (if rectangle
      (sdl-base::set-clip-rect (fp surface) (fp rectangle))
      (sdl-base::set-clip-rect (fp surface) (cffi:null-pointer))))

(defun get-cell (&key (surface *default-surface*) (index 0))
  "Returns a [RECTANGLE](#rectangle) describing the bounds of `CELL` at the specified `INDEX`.
*Note:* When `SURFACE` is the source of a blit, only the area within the cell rectangle is drawn."
  (check-type surface sdl-surface)
  (aref (cells surface) index))

(defun current-cell (&key (surface *default-surface*))
  "Returns the [RECTANGLE](#rectangle) describing the bounds of the current `CELL`.
*Note:* When `SURFACE` is the source of a blit, only the area within the cell rectangle is drawn."
  (get-cell :surface surface :index (cell-index surface)))

(defun clear-cell (&key (surface *default-surface*) (index nil))
  "Sets the `CELL` at `INDEX` to the bounds of `SURFACE`."
  (check-type surface sdl-surface)
  (unless index
    (setf index (cell-index surface)))
  (with-rectangle (cell (get-cell :surface surface :index index) nil)
    (setf cell.x (x surface)
	  cell.y (y surface)
	  cell.w (width surface)
	  cell.h (height surface))
    cell))

(defun set-cell (rectangle &key (surface *default-surface*) (index nil))
  "Sets the `CELL` at `INDEX` to the bounds of `RECTANGLE`.
*Note:* When `SURFACE` is the source of a blit, only the area within the cell rectangle is drawn."
  (check-type surface sdl-surface)
  (check-type rectangle rectangle)
  (unless index
    (setf index (cell-index surface)))
  (sdl-base::copy-rectangle (fp rectangle) (fp (get-cell :surface surface :index index)))
  (get-cell :surface surface :index index))

(defun set-cell-* (x y w h &key (surface *default-surface*) (index nil))
  "Sets the `CELL` at `INDEX` to a rectangle bounded by `X`, `Y`, `W` and `H`.
*Note:* When `SURFACE` is the source of a blit, only the area within the cell rectangle is drawn."
  (check-type surface sdl-surface)
  (unless index
    (setf index (cell-index surface)))
  (with-rectangle (cell (get-cell :surface surface :index index) nil)
    (setf cell.x x
	  cell.y y
	  cell.w w
	  cell.h h)
    cell))

(defun get-surface-rect (&key (surface *default-surface*) (rectangle (rectangle)))
  (check-type surface sdl-surface)
  (check-type rectangle rectangle)
  (sdl-base::get-surface-rect (fp surface) (fp rectangle))
  rectangle)

(defun create-surface (width height
		       &key (bpp 32) (type :sw) color-key color-key-at pixel-alpha alpha (rle-accel t) (x 0) (y 0))
  "Creates and returns a new `SURFACE` with the dimensions `WIDTH` and `HEIGHT`.

`:COLOR-KEY` sets the color key of the surface and enables color-keying. 

`:ALPHA` sets the surface alpha transparency and enables alpha blending. This allows a pixel color of `RGB` with the surface `A`.

`:PIXEL-ALPHA` enables a pixel alpha component \(alpha mask of 0xFF\) on the new surface. This allows a pixel color of `RGBA`.

`:RLE-ACCEL` enables RLE blit acceleration. 

`:BPP` is the pixel depth of the surface, and my be 8, 16, 24 or 32. 

`:TYPE` attempts to create the `SURFACE` in video memory when `:HW`, and in system memory when `:SW`

`:X` and `:Y` are the positions of the `SURFACE` in screen coordinates."
  (when color-key
    (check-type color-key color))
  (make-instance 'surface
		 :width width :height height :x x :y y
		 :enable-color-key (or color-key color-key-at)
		 :color-key (or color-key color-key-at)
		 :pixel-alpha pixel-alpha
		 :enable-alpha (or alpha pixel-alpha)
		 :alpha alpha
		 :type type
		 :rle-accel rle-accel
		 :bpp bpp))

(defun convert-to-display-format (&key (surface *default-surface*) enable-alpha enable-color-key pixel-alpha (free nil) (inherit t))
  "Returns a new surface matching the pixel format and color of the video frame buffer \(`*display surface*\), 
that is suitable for fast blitting. 
The new surface will inherit the pixel, alpha and color key components of the source, *unless* `:INHERIT` is `NIL`.

Use `:ENABLE-COLOR-KEY` or `:ENABLE-ALPHA` to take advantage of hardware 
colorkey or alpha blit acceleration. Enabling these flags once a surface is created will not necessarily 
utilize available harware acceleration if the surface was not initally created in video memory.

Use `:PIXEL-ALPHA` to enable the pixel alpha component \(alpha mask of 0xFF\) for the new surface.

Use `:FREE` to delete the source `SURFACE`.

Differences between [CONVERT-TO-DISPLAY-FORMAT](#convert-to-display-format), [CONVERT-SURFACE](#convert-surface),
[COPY-SURFACE](#copy-surface), and [CREATE-SURFACE](#create-surface);

* [CONVERT-TO-DISPLAY-FORMAT](#convert-to-display-format) 
** will always attempt to create a surface in video memory using hardware acceleration if key color and surface alpha are supported. 
** A pixel alpha component can be specified. 
** New surface is filled with old surface. 
 
* [CONVERT-SURFACE](#convert-surface)
** calls [CONVERT-TO-DISPLAY-FORMAT](#convert-to-display-format) if converting a surface to the display format.
** paramatized option to create new surface in video or system memory.
** Pixel format and color match that of the source surface. 
** New surface is filled with old surface. 

* [COPY-SURFACE](#copy-surface) 
** paramatized option to create new surface in video or system memory.
** copies a portion or all of the source surface to a new destination surface
** A pixel alpha component can be specified. 
** New surface can be filled with the old surface, or the color key or both."
  (check-type surface sdl-surface)
  (let ((surf (make-instance 'surface
			     :fp (sdl-base::convert-surface-to-display-format (fp surface)
									      :enable-color-key (if inherit
												    (color-key-enabled-p surface)
												    enable-color-key)
									      :enable-alpha (if inherit
												(alpha-enabled-p surface)
												(or enable-alpha pixel-alpha)) 
									      :pixel-alpha (if inherit
											       (pixel-alpha-enabled-p surface)
											       pixel-alpha)
									      :free nil))))
    (when free
      (free surface))
    surf))

(defun convert-surface (&key (surface *default-surface*) (to-surface *default-display*) enable-alpha enable-color-key (free nil) (inherit t) (type :sw))
  "Converts `:SURFACE` and returns a new surface matching the pixel format and color of `:TO-SURFACE`.
Calls [CONVERT-TO-DISPLAY-FORMAT](#convert-to-display-format) if converting to the display format.

Use `:ENABLE-COLOR-KEY` or `:ENABLE-ALPHA` to take advantage of hardware 
colorkey or alpha blit acceleration. Enabling these flags once a surface is created will not necessarily 
utilize available harware acceleration if the surface was not initally created in video memory.

Will create the new surface in system menory when `TYPE` is `:SW`. 
Will attempt to create the new surface in video menory when `TYPE` is `:HW`, 
otherwise the surface is created in system memory if the combination 
of color key and alpha do not allow hardware acceleration.

The new surface will inherit the pixel, alpha and color key components of the source, *unless* `:INHERIT` is `NIL`.

Use `:FREE` to delete the source `SURFACE`."
  (check-type surface sdl-surface)
  (check-type to-surface sdl-surface)
  (let ((surf (if (display-surface-p to-surface)
		  (convert-to-display-format :surface surface
					     :enable-color-key enable-color-key
					     :enable-alpha enable-alpha
					     :pixel-alpha (pixel-alpha-enabled-p surface)
					     :free nil
					     :inherit inherit)
		  (make-instance 'surface
				 :fp (sdl-base::convert-surface (fp surface) (fp to-surface)
								:enable-color-key (if inherit
										      (color-key-enabled-p surface)
										      enable-color-key)
								:enable-alpha (if inherit
											  (alpha-enabled-p surface)
											  enable-alpha)
								:free nil
								:type type)))))
    (when free
      (free surface))
    surf))

(defun copy-surface (&key cell cell-index (surface *default-surface*) color-key alpha pixel-alpha (rle-accel nil) (type :sw) (free nil) (inherit t)
		     (fill t) (color-key-fill t)
		     (pixel-format nil))
  "Returns a copy of `:SURFACE`. 

Use `:COLOR-KEY` or `:ALPHA` to set the key color and surface alpha transparency.
Hardware colorkey and alpha blit acceleration will be used if supported. 

Will create the new surface in system menory when `TYPE` is `:SW`. 
Will attempt to create the new surface in video menory when `TYPE` is `:HW`, 
otherwise the surface is created in system memory if the combination 
of color key and alpha do not allow hardware acceleration.

The new surface will be filled with the old surface unless `:FILL` is `NIL`.
The new surface will be filled with the color key of the old surface \(if available\) unless `:COLOR-KEY-FILL` is `NIL`.

The new surface will inherit the pixel, alpha and color key components of the source, *unless* `:INHERIT` is `NIL`.

Use `:FREE` to delete the source `SURFACE`."
  (check-type surface sdl-surface)
  (let ((new-surface nil))
    (let ((x 0) (y 0) (width (width surface)) (height (height surface))
	  (rect nil))
      (when (or cell cell-index)
	(when cell
	  (setf x (x cell)
		y (y cell)
		width (width cell)
		height (height cell))
	  (setf rect cell))
	(when cell-index
	  (setf rect (get-cell :surface surface :index cell-index))
	  (setf x (x rect)
		y (y rect)
		width (width rect)
		height (height rect))))
      (setf new-surface (make-instance 'surface
				       :using-surface (when pixel-format surface)
				       :x x :y y :width width :height height
				       :enable-color-key (if inherit
							     (color-key-enabled-p surface)
							     color-key)
				       :color-key (if (or inherit (eq t color-key))
						      (color-key surface)
						      color-key)
				       :enable-alpha (if inherit
							 (alpha-enabled-p surface)
							 (or alpha pixel-alpha))
				       :alpha (if (or inherit (eq t alpha))
							  (alpha surface)
							  alpha)
				       :pixel-alpha (if inherit
							(pixel-alpha-enabled-p surface)
							pixel-alpha)
				       :type type
				       :rle-accel rle-accel
				       :bpp (bit-depth surface)))
      (when color-key-fill
	(if inherit
	  (if (color-key-enabled-p surface)
	      (fill-surface (color-key surface) :surface new-surface))
	  (if color-key
	      (fill-surface (color-key surface) :surface new-surface))))      
      (when fill
	(if (or cell cell-index)
	  (sdl-base::blit-surface (fp surface) (fp new-surface)
				  (fp rect) (fp rect))
	  (blit-surface surface new-surface))))
    (when free
      (free surface))
    new-surface))
 
(defun update-surface (surface &optional template)
  "Updates the surface"
  (check-type surface sdl-surface)
  (if template
    (progn
      (check-type template rectangle)
      (sdl-base::update-surface (fp surface) :template (fp template)))
    (sdl-base::update-surface (fp surface)))
  surface)

(defun update-surface-* (surface x y w h)
  "See [UPDATE-SURFACE](#update-surface)."
  (check-type surface sdl-surface)
  (sdl-base::update-surface (fp surface) :x x :y y :w w :h h)
  surface)

(defun blit-surface (source &optional (surface *default-surface*))
  "Performs a fast blit of the `SOURCE` surface to the destination `SURFACE`. The area defined 
by the `SOURCE` cell is blitted to the area defined by the destination clipping rectangle. 
The blit function should not be called on a locked surface.
The results of blitting operations vary greatly depending on whether a surface ALPHA channel is set 
on the source surface or not. The priorty of how color key and alpha attributes interact with surface blitting is as follows:

* source surface with `ALPHA` + `PIXEL-ALPHA`: Blit using per-pixel alpha only
* source surface with `ALPHA` + `COLOR-KEY`: Blit using the colour key AND the per-surface alpha value
* source surface with `ALPHA`: Blit using the colour key AND the per-surface alpha value
* source surface with `COLOR-KEY`: Blit using the colour key
* source surface: Blit using opaque rectangular blit

An ALPHA channel has the following effect on blitting operations: 

* _RGBA to RGB_ with [ALPHA-ENABLED-P](#alpha-enabled-p): 
The source is alpha-blended with the destination, using the alpha channel. [COLOR-KEY-ENABLED-P](#color-key-enabled-p) 
and the per-surface alpha are ignored.
* _RGBA to RGB_ without [ALPHA-ENABLED-P](#alpha-enabled-p): 
The RGB data is copied from the source. The source alpha channel and the per-surface alpha value are ignored. 
If [COLOR-KEY-ENABLED-P](#color-key-enabled-p) is set, only the pixels not matching the colorkey value are copied.
* _RGB to RGBA_ with [ALPHA-ENABLED-P](#alpha-enabled-p): 
The source is alpha-blended with the destination using the per-surface alpha value. If 
[COLOR-KEY-ENABLED-P](#color-key-enabled-p) is set, only the pixels not matching the colorkey value are copied. 
The alpha channel of the copied pixels is set to opaque.
* _RGB to RGBA_ without [ALPHA-ENABLED-P](#alpha-enabled-p): 
The RGB data is copied from the source and the alpha value of the copied pixels is set to opaque. 
If [COLOR-KEY-ENABLED-P](#color-key-enabled-p) is set, only the pixels not matching the colorkey value are copied.
* _RGBA to RGBA_ with [ALPHA-ENABLED-P](#alpha-enabled-p): 
The source is alpha-blended with the destination using the source alpha channel. The alpha channel in the destination 
surface is left untouched. [COLOR-KEY-ENABLED-P](#color-key-enabled-p) is ignored.
* _RGBA to RGBA_ without [ALPHA-ENABLED-P](#alpha-enabled-p):	
The RGBA data is copied to the destination surface. If [COLOR-KEY-ENABLED-P](#color-key-enabled-p) is set, 
only the pixels not matching the colorkey value are copied.
* _RGB to RGB_ with [ALPHA-ENABLED-P](#alpha-enabled-p): 
The source is alpha-blended with the destination using the per-surface alpha value. 
If [COLOR-KEY-ENABLED-P](#color-key-enabled-p) is set, only the pixels not matching the colorkey value are copied.
* _RGB to RGB_ without [ALPHA-ENABLED-P](#alpha-enabled-p):
The RGB data is copied from the source. If [COLOR-KEY-ENABLED-P](#color-key-enabled-p) is set, only the pixels not 
matching the colorkey value are copied.

*Note*: _RGBA to RGBA_ blits \(with [ALPHA-ENABLED-P](#alpha-enabled-p) set\) keep the alpha of the destination 
surface. This means that you cannot compose two arbitrary _RGBA_ surfaces this way and get the result you would 
expect from \"overlaying\" them; the destination alpha will work as a mask."
  (unless surface
    (setf surface *default-display*))
  (check-types sdl-surface source surface)
  (sdl-base::blit-surface (fp source) (fp surface)
                          (fp (current-cell :surface source))
                          (fp (position-rect source)))
  source)

(defun draw-surface (src &key (surface *default-surface*))
  "See [BLIT-SURFACE](#blit-surface)"
  (blit-surface src surface)
  src)

(defun draw-surface-at-* (src x y &key (surface *default-surface*))
  "Draws the source surface to the destination surface at position X and Y.
 See [BLIT-SURFACE](#blit-surface).
*Note*: Modifies the position of `SRC` as a side-effect."
  (setf (x src) x
	(y src) y)
  (draw-surface src :surface surface))

(defun draw-surface-at (src point &key (surface *default-surface*))
    "Draws the source surface to the destination surface at position POINT.
 See [BLIT-SURFACE](#blit-surface).
*Note*: Modifies the position of `SRC` as a side-effect."
  (set-surface src point)
  (blit-surface src surface))

(defun fill-surface (color &key
		     (template nil)
		     (surface *default-surface*) (update nil) (clipping nil))
  "Fills surface with the specified `COLOR`.
   `:TEMPLATE` accepts  a `RECTANGLE` defining the fill bounds.
   `The surface is updated when `:UPDATE`"
  (check-type color color)
  (fill-surface-* (r color) (g color) (b color) :a (a color)
		  :template template :surface surface :update update :clipping clipping)
  surface)

(defun fill-surface-* (r g b &key (a nil) (template nil)
		       (surface *default-surface*) (update nil) (clipping t))
  "Fill the surface with the specified color `R` `G` `B` `:A` `A`.
 See [FILL-SURFACE](#fill-surface)."
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
			  :clipping clipping
			  :update update)
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

;; (defun get-mask-color-key ())

;; (defun set-mask-colors (surface &rest colors)
;;   (let ((mask-colors (mapcar #'(lambda (color)
;; 				 (map-color color surface))))
;; 	(mask (make-array (* (width surface) (height surface))
;; 			  :element-type 'bit
;; 			  :initial-contents nil)))
;;     (sdl-base::with-pixels (px (fp surface))
;;       ())
;;     ))