;; SDL (Simple Media Layer) library using CFFI for foreign function interfacing...
;; (C)2006 Justin Heyes-Jones <justinhj@gmail.com> and Luke Crook <luke@balooga.com>
;; Thanks to Frank Buss and Surendra Singh
;; see COPYING for license
;; This file contains some useful functions for using SDL from Common lisp
;; using sdl.lisp (the CFFI wrapper)

(in-package #:lispbuilder-sdl-base)

(defmacro with-surface ((var &optional surface (free t))
			&body body)
  "Don't use this for managing the display surface."
  (let ((body-value (gensym "body-value-")))
    (if (or surface (atom var))
	`(let ((,body-value nil)
	       (,@(if surface
		      `(,var ,surface)
		      `(,var ,var))))
	   (symbol-macrolet ((w (surf-w ,var))
			     (h (surf-h ,var)))
             (declare (ignorable w h))
	     (setf ,body-value (progn ,@body)))
	   (when ,free
	     (sdl-cffi::sdl-Free-Surface ,var))
	   ,body-value)
	(error "VAR must be a symbol or variable, not a function."))))

(defmacro with-surface-slots ((var &optional surface)
			      &body body)
  "Don't use this for managing the display surface."
  `(with-surface (,var ,surface nil)
     ,@body))

;; Taken from CFFI, with-foreign-objects in types.lisp
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
  (let ((body-value (gensym "body-value-")))
    `(let ((,body-value nil))
       (with-surface (,var ,surface ,nil)
	 (unwind-protect 
	      (progn (when (must-lock? ,var)
		       (when (/= (the fixnum (sdl-cffi::sdl-Lock-Surface ,var)) 0)
			 (error "Cannot lock surface")))
		     (setf ,body-value (progn ,@body))
		     (when (must-lock? ,var)
		       (Sdl-Cffi::Sdl-Unlock-Surface ,var))))
	 ,body-value))))

(defmacro with-locked-surfaces (bindings &rest body)
  (if bindings
      (return-with-locked-surface bindings body)))

(defun return-with-locked-surface (bindings body)
  (if bindings
      `(with-locked-surface (,@(car bindings))
	 ,(return-with-locked-surface (cdr bindings) body))
      `(progn ,@body)))

(defmacro with-possible-lock-and-update ((var &key surface template)
					 &body body)
  (let ((body-value (gensym "body-value-")))
    `(let ((,body-value nil))
       (with-locked-surface (,var ,surface)
	 (setf ,body-value (progn ,@body)))
       (when ,template
	 (update-surface ,var :template ,template))
       ,body-value)))

(defun surface-flag (surface flag)
  (logand (cffi:foreign-slot-value surface 'sdl-cffi::sdl-surface 'sdl-cffi::flags)
	  flag))

(defun alpha-enabled-p (surface)
  (surface-flag surface sdl-cffi::SDL-SRC-ALPHA))
(defun alpha (surface)
  (cffi:foreign-slot-value (pixel-format surface) 'sdl-cffi::sdl-pixel-format 'sdl-cffi::alpha))
(defun color-key-enabled-p (surface)
  (surface-flag surface sdl-cffi::SDL-SRC-COLOR-KEY))
(defun color-key (surface)
  (cffi:foreign-slot-value (pixel-format surface) 'sdl-cffi::sdl-pixel-format 'sdl-cffi::colorkey))
(defun pixel-alpha-enabled-p (surface)
  (cffi:foreign-slot-value (pixel-format surface) 'sdl-cffi::sdl-pixel-format 'sdl-cffi::amask))
(defun rle-accel-enabled-p (surface)
  (surface-flag surface sdl-cffi::SDL-RLE-ACCEL))

(defun (setf alpha-enabled-p) (value surface)
  (let ((flags (logior (rle-accel-enabled-p surface)
		       (if value sdl-cffi::SDL-SRC-ALPHA 0))))
    (with-foreign-slots ((sdl-cffi::alpha)
			 (pixel-format surface)
			 sdl-cffi::SDL-Pixel-Format)
      (sdl-cffi::SDL-Set-Alpha surface flags sdl-cffi::alpha))))

(defun (setf alpha) (value surface)
  (let ((flags (logior (rle-accel-enabled-p surface)
		       (alpha-enabled-p surface))))
    (sdl-cffi::SDL-Set-Alpha surface flags (clamp (to-int value) 0 255))))

(defun (setf color-key-enabled-p) (value surface)
  (let ((flags (logior (rle-accel-enabled-p surface)
		       (if value sdl-cffi::SDL-SRC-COLOR-KEY 0))))
    (with-foreign-slots ((sdl-cffi::colorkey)
			 (pixel-format surface)
			 sdl-cffi::SDL-Pixel-Format)
      (sdl-cffi::SDL-Set-Color-Key surface flags sdl-cffi::colorkey))))

(defun (setf color-key) (value surface)
  (let ((flags (logior (rle-accel-enabled-p surface)
		       (color-key-enabled-p surface))))
    (sdl-cffi::SDL-Set-Color-Key surface flags value)))

(defun (setf rle-accel-enabled-p) (value surface)
  (cond
    ((> (alpha-enabled-p surface) 0)
     (let ((flags (logior (if value sdl-cffi::SDL-RLE-ACCEL 0)
			   (alpha-enabled-p surface))))
	(with-foreign-slots ((sdl-cffi::alpha)
			     (pixel-format surface)
			     sdl-cffi::SDL-Pixel-Format)
	  (sdl-cffi::SDL-Set-Alpha surface flags sdl-cffi::alpha))))
    ((> (color-key-enabled-p surface) 0)
     (let ((flags (logior (if value sdl-cffi::SDL-RLE-ACCEL 0)
			  (color-key-enabled-p surface))))
       (with-foreign-slots ((sdl-cffi::colorkey)
			    (pixel-format surface)
			    sdl-cffi::SDL-Pixel-Format)
	 (sdl-cffi::SDL-Set-Color-Key surface flags sdl-cffi::colorkey))))))

;; (defun set-color-key (surface color &optional (enable-color-key nil) (rle-accel nil))
;;   "Sets the key color for the given surface. The key color is made transparent."
;;   (when (is-valid-ptr surface)
;;     (let ((flags 0))
;;       (when rle-accel
;; 	(setf flags (logior sdl-cffi::SDL-RLE-ACCEL flags)))
;;       (when enable-color-key
;; 	(setf flags (logior sdl-cffi::SDL-SRC-COLOR-KEY flags)))

;;       (with-foreign-slots ((sdl-cffi::colorkey)
;; 			   (pixel-format surface)
;; 			   sdl-cffi::SDL-Pixel-Format)
;; 	;; Unless color is an INTEGER, set it to the
;; 	;; current colorkey.
;; 	(unless (integerp color)
;; 	  (setf color sdl-cffi::colorkey)))
;;       (sdl-cffi::SDL-Set-Color-Key surface flags color))
;;     surface))

;; (defun set-alpha (surface alpha &optional (source-alpha nil) (rle-accel nil))
;;   "Sets the alpha value for the given surface."
;;   (when (is-valid-ptr surface)
;;     (let ((flags 0))
;;       (when rle-accel
;; 	(setf flags (logior sdl-cffi::SDL-RLE-ACCEL flags)))
;;       (when source-alpha
;; 	(setf flags (logior sdl-cffi::SDL-SRC-ALPHA flags)))
;;       (with-foreign-slots ((sdl-cffi::alpha)
;; 			   (pixel-format surface)
;; 			   sdl-cffi::SDL-Pixel-Format)
;; 	;; Unless alpha is an INTEGER, set it to the
;; 	;; current surface alpha.
;; 	(unless (integerp alpha)
;; 	  (setf alpha sdl-cffi::alpha)))
;;       (sdl-cffi::SDL-Set-Alpha surface flags (clamp (to-int alpha) 0 255)))
;;     surface))

(defun get-surface-rect (surface rectangle)
  (setf (rect-x rectangle) 0
	(rect-y rectangle) 0
	(rect-w rectangle) (surf-w surface)
	(rect-h rectangle) (surf-h surface))
  rectangle)

(defun get-clip-rect (surface rectangle)
  (sdl-cffi::sdl-get-clip-rect surface rectangle)
  rectangle)

(defun set-clip-rect (surface rectangle)
  (sdl-cffi::sdl-set-clip-rect surface rectangle)
  rectangle)

(defun convert-surface-to-display-format (surface
					  &key enable-color-key enable-alpha pixel-alpha (free nil))
  "Returns a new surface that has been converted to the display format and optionally free the source surface.
   Returns NIL if the surface cannot be converted."
  ;; LJC: Added support for converting to an alpha surface.
  ;; LJC: Freeing surface is now optional.
  (setf (color-key-enabled-p surface) enable-color-key
	(alpha-enabled-p surface) enable-alpha)
  (let ((display-surface (if pixel-alpha
			     (sdl-cffi::SDL-Display-Format-Alpha surface)
			     (sdl-cffi::SDL-Display-Format surface))))
    (unless (is-valid-ptr display-surface)
      (error "ERROR, CONVERT-SURFACE-TO-DISPLAY-FORMAT: Cannot convert surface to display format."))
    (when free
      (sdl-cffi::sdl-Free-Surface surface))
    display-surface))

(defun convert-surface (surface to-surface
			&key enable-color-key enable-alpha (rle-accel nil) (type :sw) (free nil))
  "Returns a new surface of the specified pixel format, 
and then copies and maps the given surface to it. Returns NIL if the surface cannot be converted."
  (let ((flags nil))
    (when enable-color-key
      (push sdl-cffi::SDL-SRC-COLOR-KEY flags))
    (when enable-alpha
      (push sdl-cffi::SDL-SRC-ALPHA flags))
    (when rle-accel
      (push sdl-cffi::SDL-RLE-ACCEL flags))
    (case type
      (:sw (push sdl-cffi::SDL-SW-SURFACE flags))
      (:hw (push sdl-cffi::SDL-HW-SURFACE flags)))
    (let ((surf (sdl-cffi::SDL-convert-surface surface (pixel-format to-surface) flags)))
      (unless (is-valid-ptr surf)
	(error "ERROR, CONVERT-SURFACE Cannot convert surface to display format."))
      (when free
	(sdl-cffi::sdl-Free-Surface surface))
      surf)))

(defun create-surface (width height
		       &key (bpp 32) surface (type :sw) enable-color-key pixel-alpha enable-alpha rle-accel pixels pitch)
  "create a surface compatible with the supplied :surface, if provided."
  (let ((surf nil) (flags nil))
    (when enable-color-key
      (push sdl-cffi::SDL-SRC-COLOR-KEY flags))
    (when enable-alpha
      (push sdl-cffi::SDL-SRC-ALPHA flags))
    (when rle-accel
      (push sdl-cffi::SDL-RLE-ACCEL flags))
    (case type
      (:sw (push sdl-cffi::SDL-SW-SURFACE flags))
      (:hw (push sdl-cffi::SDL-HW-SURFACE flags)))
    (if (is-valid-ptr surface)
	(with-foreign-slots ((sdl-cffi::BitsPerPixel
			      sdl-cffi::Rmask
			      sdl-cffi::Gmask
			      sdl-cffi::Bmask
			      sdl-cffi::Amask)
			     (pixel-format surface)
			     sdl-cffi::SDL-Pixel-Format)
	  (setf surf (sdl-cffi::SDL-Create-RGB-Surface (set-flags flags)
						       width height
						       sdl-cffi::BitsPerPixel
						       sdl-cffi::Rmask sdl-cffi::Gmask sdl-cffi::Bmask sdl-cffi::Amask)))
	(let ((Rmask 0) (Gmask 0) (Bmask 0) (Amask 0))
	  ;; Set masks according to endianess of machine
	  ;; Little-endian (X86)
	  #+(or X86 PC386 little-endian)
	  (progn
	    (setf rmask #x000000ff
		  gmask #x0000ff00
		  bmask #x00ff0000)
	    (when pixel-alpha
	      (setf amask #xff000000)))
	  ;; Big-endian (Motorola)
	  #-(or X86 PC386 little-endian)
	  (progn
	    (setf rmask #xff000000
		  gmask #x00ff0000
		  bmask #x0000ff00)
	    (when pixel-alpha
	      (setf amask #x000000ff)))
	  (if (and pixels pitch)
	      ;; Pixels not yet supported.
	      nil
	      (setf surf (sdl-cffi::SDL-Create-RGB-Surface (set-flags flags)
							   width height
							   bpp Rmask Gmask Bmask Amask)))))
    surf))

;; cl-sdl "sdl-ext.lisp"
(defun must-lock? (surface)
  "Checks if a surface can be locked.
   Re-implementation of the SDL_MUSTLOCK macro.
   Returns
    T if the surface can be locked.
    NIL if the surface cannot be locked."
  (or (/= 0 (cffi:foreign-slot-value surface 'sdl-cffi::sdl-surface 'sdl-cffi::offset))
      (/= 0 (logand (cffi:foreign-slot-value surface 'sdl-cffi::sdl-surface 'sdl-cffi::flags)
		    (logior sdl-cffi::SDL-HW-SURFACE
			    sdl-cffi::SDL-ASYNC-BLIT
			    sdl-cffi::SDL-RLE-ACCEL)))))

(defun pixel-format (surface)
  "Returns the pixel format of a surface."
  (cffi:foreign-slot-value surface 'sdl-cffi::SDL-Surface 'sdl-cffi::format))

(defun surf-w (surface)
  "return the width of the SDL_surface."
  (cffi:foreign-slot-value surface 'sdl-cffi::Sdl-Surface 'sdl-cffi::w))

(defun surf-h (surface)
  "return the height of the Sdl-Surface." 
  (cffi:foreign-slot-value surface 'sdl-cffi::Sdl-Surface 'sdl-cffi::h))

(defun clip-to-surface (template surface)
  "Clips the rectangle `TEMPLATE` to the surface `SURFACE`s `X`, `Y`, `WIDTH` and `HEIGHT`.
*Note*: `TEMPLATE` is modified."
  (let* ((x (rect-x template)) (y (rect-y template))
	 (w (rect-w template)) (h (rect-h template))
	 (x2 (+ x w)) (y2 (+ y h)))
    (setf (rect-x template) (clamp x 0 (surf-w surface))
	  (rect-y template) (clamp y 0 (surf-h surface))
	  (rect-w template) (- (clamp x2 0 (surf-w surface))
			       (rect-x template))
	  (rect-h template) (- (clamp y2 0 (surf-h surface))
			       (rect-y template))))
  template)

(defun clip-to-coords (x y w h w-coord h-coord)
  "Clips the rectangle `TEMPLATE` to the `X`, `Y`, `WIDTH` and `HEIGHT` coordinates.
*Note*: `TEMPLATE` is modified."
  (let ((x2 (+ x w)) (y2 (+ y h)))
    (setf x (clamp x 0 w-coord)
	  y (clamp y 0 h-coord)
	  w (- (clamp x2 0 w-coord)
	       x)
	  h (- (clamp y2 0 h-coord)
	       y)))
  (values x y w h))

(defun update-surface (surface &key template x y w h (clipping t))
  "Updates the screen using the keyword co-ordinates in the Vector, :template.
   All co-ordinates default to 0, updating the entire screen."
  (if template
      (progn 
	(when clipping
	  (setf template (clip-to-surface template surface)))
	(sdl-cffi::SDL-Update-Rect surface (rect-x template) (rect-y template) (rect-w template) (rect-h template)))
      (if x
	  (progn
	    (when clipping
	      (multiple-value-bind (clip-x clip-y clip-w clip-h)
		  (clip-to-coords x y w h (surf-w surface) (surf-h surface))
		(sdl-cffi::SDL-Update-Rect surface clip-x clip-y clip-w clip-h))))
	  (sdl-cffi::SDL-Update-Rect surface 0 0 0 0)))
  surface)

(defun blit-surface (src dst src-rect dst-rect &key (update nil))
  "Blits the entire SRC Sdl-Surface to the DST Sdl-Surface using SDL_BlitSurface.
   use :src-rect SDL_Rect to blit only a portion of the SRC to the DST surface
   Use :dst-rect SDL_Rect to position the SRC on the DST surface."
    (sdl-cffi::sdl-upper-Blit src src-rect
			      dst dst-rect)
    (when update
      (update-surface dst :template dst-rect :clipping t))
  dst-rect)

(defun fill-surface (surface color &key (template nil) (update nil) (clipping nil))
  "fill the entire surface with the specified RGB/A color.
   Use :template to specify the SDL_Rect to be used as the fill template.
   Use :update to call SDL_UpdateRect, using :template if provided. This allows for a 
   'dirty recs' screen update.
*Note*: `TEMPLATE` is clipped to the surface `SURFACE`, when `CLIPPING` is `T`."
  (if template
      (progn (if clipping
		 (setf template (clip-to-surface template surface)))
	     (sdl-cffi::sdl-Fill-Rect surface template color))
      (sdl-cffi::sdl-Fill-Rect surface (cffi:null-pointer) color))
  (when update
    (if template
	(update-surface surface
			:template template
			:clipping nil)
	(sdl-cffi::SDL-Update-Rect surface 0 0 0 0)))
  template)

(defun map-color (surface r g b &optional a)
  (if a
      (sdl-cffi::SDL-Map-RGBA (pixel-format surface) r g b a)
      (sdl-cffi::SDL-Map-RGB (pixel-format surface) r g b)))

(defun map-pixel (pixel surface &optional a)
  (if a
      (cffi:with-foreign-objects ((r :uint8) (g :uint8) (b :uint8) (a :uint8))
	(sdl-cffi::SDL-Get-RGBA pixel (pixel-format surface) r g b a)
	(values
	 (cffi:mem-aref r :uint8)
	 (cffi:mem-aref g :uint8)
	 (cffi:mem-aref b :uint8)
	 (cffi:mem-aref a :uint8)))
      (cffi:with-foreign-objects ((r :uint8) (g :uint8) (b :uint8))
	(sdl-cffi::SDL-Get-RGB pixel (pixel-format surface) r g b)
	(values
	 (cffi:mem-aref r :uint8)
	 (cffi:mem-aref g :uint8)
	 (cffi:mem-aref b :uint8)))))

