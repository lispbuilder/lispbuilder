;; SDL (Simple Media Layer) library using CFFI for foreign function interfacing...
;; (C)2006 Justin Heyes-Jones <justinhj@gmail.com> and Luke Crook <luke@balooga.com>
;; Thanks to Frank Buss and Surendra Singh
;; see COPYING for license
;; This file contains some useful functions for using SDL from Common lisp
;; using sdl.lisp (the CFFI wrapper)

(in-package #:lispbuilder-sdl-base)

;; cl-sdl "sdl-ext.lisp"
(defmacro with-locked-surface ((var &optional surface)
			       &body body)
  (if (or surface (atom var))
      `(let (,@(when surface `(,var ,surface)))
	 (unwind-protect 
	      (progn (when (must-lock? ,var)
		       (when (/= (sdl-cffi::sdl-Lock-Surface ,var) 0)
			 (error "Cannot lock surface")))
		     ,@body)
	   (when (must-lock? ,var)
	     (Sdl-Cffi::Sdl-Unlock-Surface ,var))))
      (error "VAR must be a symbol or variable, not a function.")))

(defun return-with-surfaces (bindings body)
  (if bindings
      `(with-locked-surface (,@(car bindings))
	 ,(return-with-surfaces (cdr bindings) body))
      `(progn ,@body)))

;; Taken from CFFI, with-foreign-objects in types.lisp
(defmacro with-locked-surfaces (bindings &rest body)
  (if bindings
      (return-with-surfaces bindings body)))

(defmacro with-possible-lock-and-update ((var &key surface template)
					 &body body)
  `(progn
     (with-locked-surface (,var ,surface)
       ,@body)
     (when ,template
       (update-surface ,var :template ,template))))
  
(defmacro with-surface ((var &optional surface (free-p t))
			&body body)
  "Don't use this for managing the display surface."
  (let ((body-value (gensym "body-value-")))
    (if (or surface (atom var))
	`(let ((,body-value nil)
	       (,@(when surface `(,var ,surface))))
	   (symbol-macrolet ((,(intern (string-upcase (format nil "~A.w" var))) (surf-w ,var))
			     (,(intern (string-upcase (format nil "~A.h" var))) (surf-h ,var)))
	     (setf ,body-value (progn ,@body)))
	   (when ,free-p
	     (sdl-cffi::sdl-Free-Surface ,var))
	   ,body-value)
	(error "VAR must be a symbol or variable, not a function."))))

(defmacro with-surface-slots ((var &optional surface)
			      &body body)
  "Don't use this for managing the display surface."
  `(with-surface (,var ,surface nil)
     ,@body))

(defun return-with-surface (bindings body)
  (if bindings
      `(with-surface (,@(car bindings))
	 ,(return-with-surface (cdr bindings) body))
      `(progn ,@body)))

;; Taken from CFFI, with-foreign-objects in types.lisp
(defmacro with-surfaces (bindings &rest body)
  (if bindings
      (return-with-surface bindings body)))

(defun set-color-key (surface &optional (color nil) (rle-accel nil))
  "Sets the key color for the given surface. The key color is made transparent."
  (when (is-valid-ptr surface)
    (let ((SRC-COLOR-KEY nil))
      (if rle-accel
	  (setf rle-accel sdl-cffi::SDL-RLE-ACCEL)
	  (setf rle-accel 0))
      (if color
	  (setf SRC-COLOR-KEY sdl-cffi::SDL-SRC-COLOR-KEY)
	  (setf color 0
		SRC-COLOR-KEY 0))
      (sdl-cffi::SDL-Set-Color-Key surface (logior SRC-COLOR-KEY rle-accel) color))
    surface))

(defun clear-color-key (surface rle-accel)
  "Removes the key color from the given surface."
  (set-color-key surface nil rle-accel))

(defun set-alpha (surface alpha-value &optional (accel nil))
  "Sets the alpha value for the given surface."
  (when (is-valid-ptr surface)
    (if accel
	(setf accel sdl-cffi::SDL-RLE-ACCEL)
	(setf accel 0))
    (if (null alpha-value)
	(sdl-cffi::SDL-Set-Alpha surface accel 0)
	(sdl-cffi::SDL-Set-Alpha surface (logior sdl-cffi::SDL-SRC-ALPHA accel) (clamp (to-int alpha-value) 0 255)))
    surface))

(defun get-surface-rect (surface rectangle)
  (setf (rect-x rectangle) 0
	(rect-y rectangle) 0
	(rect-w rectangle) (surf-w surface)
	(rect-h rectangle) (surf-h surface))
  rectangle)

(defun get-clip-rect (surface rectangle)
  (sdl-cffi::sdl-get-clip-rect surface rectangle)
  rectangle)

(defun convert-surface-to-display-format (surface &key key-color alpha-value (free-p nil))
  "converts a surface to display format and free's the source surface
    :alpha t will convert the surface and add an alpha channel.
    :free nil will not free surface.
   returns NIL if the surface cannot be converted."
  ;; LJC: Added support for converting to an alpha surface.
  ;; LJC: Freeing surface is now optional.
  (when (is-valid-ptr surface)
    (if key-color
	(set-color-key surface key-color))
    (if alpha-value
	(set-alpha surface alpha-value))
    (let ((display-surface (if alpha-value
			       (sdl-cffi::SDL-Display-Format-Alpha surface)
			       (sdl-cffi::SDL-Display-Format surface))))
      (if free-p
	  (sdl-cffi::sdl-Free-Surface surface))
      (if (is-valid-ptr display-surface)
	  display-surface
	  nil))))

(defun copy-surface (surface &key key-color alpha-value (type :sw) accel)
  "create a surface compatible with the supplied surface"
  (create-surface (surf-w surface) (surf-h surface)
		  :surface surface
		  :key-color key-color
		  :alpha-value alpha-value
		  :type type
		  :accel accel))

(defun create-surface (width height &key (bpp 32) surface pixels pitch key-color alpha-value (type :sw) (accel nil))
  "create a surface compatible with the supplied :surface, if provided."
  (let ((surf nil) (flags nil))
    (if key-color
	(push sdl-cffi::SDL-SRC-COLOR-KEY flags))
    (if alpha-value
	(push sdl-cffi::SDL-SRC-ALPHA flags))
    (if accel
	(push sdl-cffi::SDL-RLE-ACCEL flags))
    (case type
      (:sw (push sdl-cffi::SDL-SW-SURFACE flags))
      (:hw (push sdl-cffi::SDL-HW-SURFACE flags)))
    (if (is-valid-ptr surface)
	(with-foreign-slots ((sdl-cffi::BitsPerPixel sdl-cffi::Rmask sdl-cffi::Gmask sdl-cffi::Bmask sdl-cffi::Amask) (pixel-format surface) sdl-cffi::SDL-Pixel-Format)
	    (setf surf (sdl-cffi::SDL-Create-RGB-Surface (set-flags flags)
					     width height sdl-cffi::BitsPerPixel sdl-cffi::Rmask sdl-cffi::Gmask sdl-cffi::Bmask sdl-cffi::Amask)))
	(let ((Rmask 0) (Gmask 0) (Bmask 0) (Amask 0))
	  ;; Set masks according to endianess of machine
	  ;; Little-endian (X86)
	  #+(or PC386 little-endian)(setf rmask #x000000ff
					  gmask #x0000ff00
					  bmask #x00ff0000
					  amask #xff000000)
	  ;; Big-endian (Motorola)
	  #-(or PC386 little-endian)(setf rmask #xff000000
					  gmask #x00ff0000
					  bmask #x0000ff00
					  amask #x000000ff)
	  (if (and pixels pitch)
	      ;; Pixels not yet supported.
	      nil
	      (setf surf (sdl-cffi::SDL-Create-RGB-Surface (set-flags flags) width height bpp Rmask Gmask Bmask Amask)))))
    (if key-color
	(set-color-key surf key-color accel))
    (if alpha-value
	(set-alpha surf alpha-value accel))
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

(defun update-surface (surface &key template number x y w h)
  "Updates the screen using the keyword co-ordinates in the Vector, :template.
   All co-ordinates default to 0, updating the entire screen."
  (if x
      (sdl-cffi::SDL-Update-Rect surface x y w h)
      (if (is-valid-ptr template)
	  (if number
	      (sdl-cffi::sdl-Update-Rects surface number template)
	      (sdl-cffi::SDL-Update-Rect surface 
					 (rect-x template)
					 (rect-y template)
					 (rect-w template)
					 (rect-h template)))
	  (sdl-cffi::SDL-Update-Rect surface 0 0 0 0)))
  surface)

(defun blit-surface (src dst src-rect dst-rect &key (update-p nil))
  "Blits the entire SRC Sdl-Surface to the DST Sdl-Surface using SDL_BlitSurface.
   use :src-rect SDL_Rect to blit only a portion of the SRC to the DST surface
   Use :dst-rect SDL_Rect to position the SRC on the DST surface."
  (sdl-cffi::sdl-Upper-Blit src src-rect dst dst-rect)
  (when update-p
    (update-surface dst :template dst-rect))
  dst-rect)

(defun fill-surface (surface color &key template (update-p nil) (clipping-p t))
  "fill the entire surface with the specified R G B A color.
   Use :template to specify the SDL_Rect to be used as the fill template.
   Use :update-p to call SDL_UpdateRect, using :template if provided. This allows for a 
   'dirty recs' screen update."
  (if (is-valid-ptr template)
      (when clipping-p
	(let* ((x (rect-x template)) (y (rect-y template))
	       (w (rect-w template)) (h (rect-h template))
	       (x2 (+ x w)) (y2 (+ y h)))
	  (setf (rect-w template) (check-bounds 0 (surf-w surface) x x2)
		(rect-h template) (check-bounds 0 (surf-h surface) y y2)))))
  (sdl-cffi::sdl-Fill-Rect surface template color)
  (when update-p
    (update-surface surface :template template))
  template)

