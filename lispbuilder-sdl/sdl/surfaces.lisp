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
  (unless (initialized-sub-systems-p)
    (error "ERROR: The SDL library must be initialized prior to use."))
  (call-next-method))

(defclass surface (sdl-surface) ()
  (:default-initargs
   :display-surface-p nil
    :gc t
    :free (simple-free 'sdl-cffi::sdl-free-surface 'surface))
  (:documentation
   "This object is garbage collected and the `SDL_Surface` object freed when out of scope."))

(defclass rectangle-array ()
  ((foreign-pointer-to-rectangle :accessor fp :initform nil :initarg :rectangle)
   (length :reader len :initform nil :initarg :length)))

;; (defmethod initialize-instance :after ((surface display-surface) &key)
;;   (unless (is-valid-ptr (fp surface))
;;     (error "SURFACE must be a foreign pointer."))
;;   (setf (width surface) (sdl-base::surf-w (fp surface))
;; 	(height surface) (sdl-base::surf-h (fp surface))))

(defmethod initialize-instance :after ((surface surface)
				       &key using-surface
				       width height x y bpp
				       enable-color-key
				       color-key
				       color-key-at
				       enable-surface-alpha
				       surface-alpha
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
				      :enable-surface-alpha enable-surface-alpha
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
    (setf (enable-color-key-p surface) enable-color-key
	  (enable-surface-alpha-p surface) enable-surface-alpha)
    (when color-key
      (setf (color-key-p surface) color-key))
    (when color-key-at
      (setf (color-key-p surface) (read-pixel color-key-at :surface surface)))
    (when surface-alpha
      (setf (surface-alpha-p surface) surface-alpha))))

(defmacro with-surface ((var &optional surface (free t))
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
       (when ,free
	 (free ,var))
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
  "Returns new `RECTANGLE` containing the `X`, `Y`, `WIDTH` and `HEIGHT` values of `SURFACE`."
  (rectangle :x (x surface)
	     :y (y surface)
	     :w (width surface)
	     :h (height surface))
  surface)

(defmethod enable-surface-alpha-p ((surface sdl-surface))
  "Returns `T` when alpha blending is enabled for `SURFACE`, 
and `NIL` when disabled"
  (1/0->T/NIL (sdl-base::enable-surface-alpha-p (fp surface))))
(defmethod (setf enable-surface-alpha-p) (value (surface sdl-surface))
  "Enable alpha blending for `SURFACE` when `T`. Disable alpha blending when `NIL`. 
A `SURFACE` need not have an alpha channel \(RGBA\) to use alpha blending."
  (setf (sdl-base::enable-surface-alpha-p (fp surface)) value))

(defmethod surface-alpha-p ((surface sdl-surface))
  "Returns the per-surface alpha value for `SURFACE`. 0 is transparent, and 255 is opaque."
  (sdl-base::surface-alpha-p (fp surface)))
(defmethod (setf surface-alpha-p) (value (surface sdl-surface))
  "Sets the per-surface alpha value for `SURFACE`. 0 is transparent, and 255 is opaque. 

*Note*: The per-surface alpha value of 128 is considered a special case and is optimised, so it's much faster than other per-surface values.
*Note*: A surface need not have an alpha channel to use alpha blending.
*Note*: When blitting, the presence or absence of [SDL-SRC-ALPHA](#sdl-src-alpha) is relevant only on the source surface, not the destination. 
*Note*: Per-pixel and per-surface alpha cannot be combined; the per-pixel alpha is always used if available."
  (setf (sdl-base::surface-alpha-p (fp surface)) value))

(defmethod enable-color-key-p ((surface sdl-surface))
  "Returns `T` when color keying is enabled for `SURFACE`, and `NIL` when color keying is disabled."
  (1/0->T/NIL (sdl-base::enable-color-key-p (fp surface))))
(defmethod (setf enable-color-key-p) (value (surface sdl-surface))
  "Enables color keying for `SURFACE` when `T`. Disable color keying when `NIL`"
  (setf (sdl-base::enable-color-key-p (fp surface)) value))

(defmethod color-key-p ((surface sdl-surface))
  "Returns the color key \(transparent pixel\) of `SURFACE` as [COLOR-A](#color-a)."
  (multiple-value-bind (r g b a)
      (sdl-base::map-pixel (sdl-base::color-key-p (fp surface))
			   (fp surface))
    (color :r r :g g :b b :a a)))
(defmethod (setf color-key-p) (value (surface sdl-surface))
  "Set the color key \(transparent pixel\) for `SURFACE`. 
Of type [COLOR](#color) for RGB, or [COLOR-A](#color-a) for RGBA."
  (setf (sdl-base::color-key-p (fp surface)) (map-color value surface)))

(defun clear-color-key (&key (surface *default-surface*))
  "Disables the color key for `SURFACE`."
  (check-type surface sdl-surface)
  (setf (enable-color-key-p surface) nil))

(defmethod pixel-alpha-p ((surface sdl-surface))
  "Returns `T` if `SURFACE` has a pixel alpha component \(RGBA\), or `NIL` if not\(RGB\).
*Note*: The pixel alpha component differs from the surface alpha component which is 
retrieved using [ENABLE-SURFACE-ALPHA-P](#enable-surface-alpha-p)."
  (1/0->T/NIL (sdl-base::pixel-alpha-p (fp surface))))

(defmethod rle-accel-p ((surface sdl-surface))
  "Returns `T` if RLE acceleration is enabled for `SURFACE`, and `NIL` when RLE is disabled."
  (1/0->T/NIL (sdl-base::rle-accel-p (fp surface))))
(defmethod (setf rle-accel-p) (value (surface sdl-surface))
  "Enables RLE blit acceleration when `T`, disables RLE acceleration when `NIL`. 
RLE acceleration can substantially speed up blitting of images with large horizontal runs 
of transparent pixels (i.e., pixels that match the key color)."
  (setf (sdl-base::rle-accel-p (fp surface)) value))

(defmethod clip-rect ((surface sdl-surface))
  "Returns the clipping `RECTANGLE` for `SURFACE`."
  (get-clip-rect :surface surface))
(defmethod (setf clip-rect) (value (surface sdl-surface))
  "Sets the clipping `RECTANGLE` for `SURFACE`. Removes the clipping rectangle when `NIL`.
*Note:* When `SURFACE` is the destination of a blit, only the area within the clipping rectangle is 
drawn into."
  (set-clip-rect value :surface surface))

(defun set-color-key (color &key (surface *default-surface*) (rle-accel t))
  "Set the color key \(transparent pixel\) for `SURFACE`. 

##### Paremeters

* `COLOR` the transparent pixel color, of type [COLOR](#color), or [COLOR-A](#color-a). 
When 'NIL' will disable color keying.
* `RLE-ACCEL` when `T` will use RLE information when blitting. See [RLE-CCEL](#rle-accel)."
  (check-type surface sdl-surface)
  (check-type color sdl-color)
  (sdl-base::set-color-key (fp surface) (map-color color surface) rle-accel))
  
(defun set-alpha (alpha &key (surface *default-surface*) (source-alpha nil) (rle-accel nil))
  "Set the transparency,  alpha blending and RLE acceleration properties of `SURFACE`.

##### Parameters

* `ALPHA` sets the `SURFACE` transparency. Allowable values are `NIL`, or any `INTEGER` between `0` and `255` inclusive.
`0` is  transparent and `255` being opaque.
*Note*: The per-surface alpha value of 128 is considered a special case and is optimised, so it's much faster than other per-surface values. 
* `SOURCE-ALPHA` will enable or disable alpha blending for `SURFACE`.
* `RLE-ACCEL` will enable or disable RLE acceleration when blitting. See [RLE-ACCEL](#rle-accel)."
  (check-type surface sdl-surface)
  (sdl-base::set-alpha (fp surface) alpha source-alpha rle-accel))

(defun get-clip-rect (&key (surface *default-surface*) (rectangle (rectangle)))
  "Returns the clipping `RECTANGLE` of `SURFACE` as a new `RECTANGLE`, 
or will fill the specified rectangle."
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

(defun clear-clip-rect (&optional (surface *default-surface*))
  "Removes the clipping `RECTANGLE` from `SURFACE`."
  (check-type surface sdl-surface)
  (set-clip-rect NIL :surface surface)
  t)

(defun get-cell (&key (surface *default-surface*) (index 0))
  "Returns the `CELL` for `SURFACE` at the specified `INDEX`.
*Note:* When `SURFACE` is the source of a blit, only the area within the cell rectangle is drawn."
  (check-type surface sdl-surface)
  (aref (cells surface) index))

(defun current-cell (&key (surface *default-surface*))
  "Returns the current `CELL` for `SURFACE`.
*Note:* When `SURFACE` is the source of a blit, only the area within the cell rectangle is drawn."
  (get-cell :surface surface :index (cell-index surface)))

(defun clear-cell (&key (surface *default-surface*) (index nil))
  "Sets the specified `CELL` at `INDEX` to the bounds of `SURFACE`."
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
  "Sets the specified `CELL` at `INDEX` to the bounds of `RECTANGLE`.
*Note:* When `SURFACE` is the source of a blit, only the area within the cell rectangle is drawn."
  (check-type surface sdl-surface)
  (check-type rectangle rectangle)
  (unless index
    (setf index (cell-index surface)))
  (sdl-base::copy-rectangle (fp rectangle) (fp (get-cell :surface surface :index index)))
  (get-cell :surface surface :index index))

(defun set-cell-* (x y w h &key (surface *default-surface*) (index nil))
  "Sets the specified `CELL` at `INDEX` to a rectangle bounded by `X`, `Y`, `W` and `H`.
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
		       &key (bpp 32) (type :sw) color-key color-key-at pixel-alpha surface-alpha (rle-accel t) (x 0) (y 0))
  "Creates and returns a new `SURFACE` with the dimensions `WIDTH` and `HEIGHT`.

`:COLOR-KEY` sets the color key of the surface and enables color-keying. 

`:SURFACE-ALPHA` sets the surface alpha transparency and enables alpha blending. This allows a pixel color of `RGB` with the surface `A`.

`:PIXEL-ALPHA` enables a pixel alpha component \(alpha mask of 0xFF\) on the new surface. This allows a pixel color of `RGBA`.

`:RLE-ACCEL` enables RLE blit acceleration. 

`:BPP` is the pixel depth of the surface, and my be 8, 16, 24 or 32. 

`:TYPE` attempts to create the `SURFACE` in video memory when `:HW`, and in system memory when `:SW`

`:X` and `:Y` are the positions of the `SURFACE` in screen coordinates."
  (when color-key
    (check-type color-key sdl-color))
  (make-instance 'surface
		 :width width :height height :x x :y y
		 :enable-color-key (or color-key color-key-at)
		 :color-key (or color-key color-key-at)
		 :pixel-alpha pixel-alpha
		 :enable-surface-alpha surface-alpha
		 :surface-alpha surface-alpha
		 :type type
		 :rle-accel rle-accel
		 :bpp bpp))

(defun convert-to-display-format (&key (surface *default-surface*) enable-surface-alpha enable-color-key pixel-alpha (free nil) (inherit t))
  "Returns a new surface matching the pixel format and color of the video frame buffer \(`*display surface*\), 
that is suitable for fast blitting. 
The new surface will inherit the pixel, alpha and color key components of the source, *unless* `:INHERIT` is `NIL`.

Use `:ENABLE-COLOR-KEY` or `:ENABLE-SURFACE-ALPHA` to take advantage of hardware 
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
** Calls [CONVERT-TO-DISPLAY-FORMAT](#convert-to-display-format) if converting a surface to the display format.
** paramatized option to create new surface in video or system memory.
** Pixel format and color match that of the source surface. 
** Pixel alpha cannot be specified.
** New surface is filled with old surface. 

* [COPY-SURFACE](#copy-surface) 
** Copies a portion of the source surface to a new destination surface
** paramatized option to create new surface in video or system memory.
** Pixel format and color match that of the source surface. 
** Pixel alpha cannot be specified.
** New surface is filled with old surface."
  (check-type surface sdl-surface)
  (let ((surf (make-instance 'surface
			     :fp (sdl-base::convert-surface-to-display-format (fp surface)
									      :enable-color-key (if inherit
												    (enable-color-key-p surface)
												    enable-color-key)
									      :enable-surface-alpha (if inherit
													(enable-surface-alpha-p surface)
													enable-surface-alpha) 
									      :pixel-alpha (if inherit
											       (pixel-alpha-p surface)
											       pixel-alpha)
									      :free nil))))
    (when free
      (free surface))
    surf))

(defun convert-surface (&key (surface *default-surface*) (to-surface *default-display*) enable-surface-alpha enable-color-key (free nil) (inherit t) (type :sw))
  "Converts `:SURFACE` and returns a new surface matching the pixel format and color of `:TO-SURFACE`.
Calls [CONVERT-TO-DISPLAY-FORMAT](#convert-to-display-format) if converting to the display format.

Use `:ENABLE-COLOR-KEY` or `:ENABLE-SURFACE-ALPHA` to take advantage of hardware 
colorkey or alpha blit acceleration. Enabling these flags once a surface is created will not necessarily 
utilize available harware acceleration if the surface was not initally created in video memory.

Will create the new surface in system menory when `TYPE` is `:HW`. 
Will attempt to create the new surface in video menory when `TYPE` is `:HW`, otherwise the surface is created in system memory if the combination 
of color key and alpha do not allow hardware acceleration.

The new surface will inherit the pixel, alpha and color key components of the source, *unless* `:INHERIT` is `NIL`.

Use `:FREE` to delete the source `SURFACE`."
  (check-type surface sdl-surface)
  (check-type to-surface sdl-surface)
  (let ((surf (if (display-surface-p to-surface)
		  (convert-to-display-format :surface surface
					     :enable-color-key enable-color-key
					     :enable-surface-alpha enable-surface-alpha
					     :pixel-alpha (pixel-alpha-p surface)
					     :free nil
					     :inherit inherit)
		  (make-instance 'surface
				 :fp (sdl-base::convert-surface (fp surface) (fp to-surface)
								:enable-color-key (if inherit
										      (enable-color-key-p surface)
										      enable-color-key)
								:enable-surface-alpha (if inherit
											  (enable-surface-alpha-p surface)
											  enable-surface-alpha)
								:free nil
								:type type)))))
    (when free
      (free surface))
    surf))

(defun copy-surface (&key cell cell-index (surface *default-surface*) color-key surface-alpha pixel-alpha (rle-accel nil) (type :sw) (free nil) (inherit t)
		     (fill t) (color-key-fill t)
		     (pixel-format nil))
  "Returns a copy of `SURFACE` created in video memory when `TYPE` is `:HW`, and in system memory when `TYPE` is `:SW`. 
The new surface will inherit *all* of the source `SURFACE`s pixel format, color, alpha and color key components.
The new surface is filled with the source surface color key if the color key component of the source surface is enabled.
The source surface is blitted to the new surface when `:ALL` is `T`."
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
							     (enable-color-key-p surface)
							     color-key)
				       :color-key (if (or inherit (eq t color-key))
						      (color-key-p surface)
						      color-key)
				       :enable-surface-alpha (if inherit
								 (enable-surface-alpha-p surface)
								 surface-alpha)
				       :surface-alpha (if (or inherit (eq t surface-alpha))
							  (surface-alpha-p surface)
							  surface-alpha)
				       :pixel-alpha (if inherit
							(pixel-alpha-p surface)
							pixel-alpha)
				       :type type
				       :rle-accel rle-accel
				       :bpp (bit-depth surface)))
      (when color-key-fill
	(if inherit
	  (if (enable-color-key-p surface)
	      (fill-surface (color-key-p surface) :surface new-surface))
	  (if color-key
	      (fill-surface (color-key-p surface) :surface new-surface))))      
      (when fill
	(if (or cell cell-index)
	  (sdl-base::blit-surface (fp surface) (fp new-surface)
				  (fp rect) (fp rect))
	  (blit-surface surface new-surface))))
    (when free
      (free surface))
    new-surface))
 
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
  "##### Alpha effects

Alpha has the following effect on `SURFACE` blitting: 

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

*Note*: _RGBA to RGBA_ blits (with [SDL-SRC-ALPHA](#sdl-src-alpha) set) keep the alpha of the destination 
surface. This means that you cannot compose two arbitrary _RGBA_ surfaces this way and get the result you would 
expect from \"overlaying\" them; the destination alpha will work as a mask."
  (unless surface
    (setf surface *default-display*))
  (check-types sdl-surface src surface)
  (sdl-base::blit-surface (fp src) (fp surface)
			  (fp (current-cell :surface src)) (fp (position-rect src)))
  src)

(defun draw-surface (src &key (surface *default-surface*))
  (blit-surface src surface)
  src)

(defun draw-surface-at-* (src x y &key (surface *default-surface*))
  (setf (x src) x
	(y src) y)
  (draw-surface src :surface surface))

(defun draw-surface-at (src point &key (surface *default-surface*))
  (set-surface src point)
  (blit-surface src surface))

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