;; SDL (Simple Media Layer) library using CFFI for foreign function interfacing...
;; (C)2006 Justin Heyes-Jones <justinhj@gmail.com> and Luke Crook <luke@balooga.com>
;; Thanks to Frank Buss and Surendra Singh
;; see COPYING for license
;; This file contains some useful functions for using SDL from Common lisp
;; using sdl.lisp (the CFFI wrapper)

(in-package #:lispbuilder-sdl)

;; cl-sdl "sdl-ext.lisp"
(defmacro with-locked-surface ((var &optional surface) &body body)
  `(let (,@(when surface `(,var ,surface)))
     (unwind-protect 
	  (progn (when (must-lock? ,var)
		   (if (= (SDL_LockSurface ,surface) 0)
		       ,@body
		       (error "Cannot lock surface"))))
       (when (must-lock? ,var)
         (SDL_UnlockSurface ,var)))))

;; cl-sdl "cl-sdl.lisp"
(defmacro with-possible-lock-and-update ((surface &key (check-lock-p t) (update-p nil) (template nil))
					 &body body)
  (let ((locked-p (gensym "LOCKED-P"))
        (exit (gensym "EXIT"))
	(result (gensym "RESULT")))
    `(let ((,locked-p nil)
	   (,result nil))
      (block ,exit
        (when ,check-lock-p
          (when (must-lock? ,surface)
            (when (< (sdl:SDL_LockSurface ,surface)
                     0)
              (return-from ,exit (values)))
            (setf ,locked-p t)))
        (setf ,result (progn ,@body))
        (when ,locked-p
          (SDL_UnlockSurface ,surface))
        (when ,update-p
          (update-surface ,surface ,template))
	,result))))

(defmacro with-surface ((surface-ptr &optional (free-p t)) &body body)
  "Don't use this for managing the display surface."
  (let ((body-value (gensym "body-value")))
    `(symbol-macrolet ((w (surf-w ,surface-ptr))
		       (h (surf-h ,surface-ptr)))
       (let ((,body-value nil))
	 (when (is-valid-ptr ,surface-ptr)
	   (setf ,body-value (progn ,@body))
	   (when ,free-p
	     (Free-Surface ,surface-ptr)))
	 ,body-value))))

;; Taken from CFFI, with-foreign-objects in types.lisp
(defmacro with-surfaces (bindings &rest body)
  (if bindings
      (let ((body-value (gensym "body-value")))
	`(let ((,body-value nil)
	       ,@(loop for binding in bindings
		       collect `(,(first binding) ,(second binding))))
	  (when (and ,@(loop for binding in bindings
			     collect `(is-valid-ptr ,(first binding))))
	    (setf ,body-value (progn ,@body)))
	  ,body-value))))

;;;This does not work!!!!
(defmacro with-surfaces-free (bindings &rest body)
  (if bindings
      (let ((body-value (gensym "body-value")))
	`(let ((,body-value nil)
	       ,@(loop for binding in bindings
		    collect `(,(first binding) ,(second binding))))
	   (when (and ,@(loop for binding in bindings
			   collect `(is-valid-ptr ,(first binding))))
	     (setf ,body-value (progn ,@body))
	     ,@(loop for binding in bindings
		  collect `(if (is-valid-ptr ,(first binding))
			       (Free-Surface ,(first binding)))))
	   ,body-value))))

(defun clear-colorkey (surface rel-accel)
  "Removes the key color from the given surface."
  (when (is-valid-ptr surface)
    (if rel-accel
	(setf rel-accel SDL_RLEACCEL)
	(setf rel-accel 0))
    (SDL_SetColorKey surface rel-accel 0)))

(defun get-surface-rect (surface rectangle)
  (setf (rect-x rectangle) 0
	(rect-y rectangle) 0
	(rect-w rectangle) (surf-w surface)
	(rect-h rectangle) (surf-h surface)))

(defun convert-surface-to-display-format (surface &key key-color alpha-value (free-p nil))
  "converts a surface to display format and free's the source surface
    :alpha t will convert the surface and add an alpha channel.
    :free nil will not free surface.
   returns NIL if the surface cannot be converted."
  ;; LJC: Added support for converting to an alpha surface.
  ;; LJC: Freeing surface is now optional.
  (when (is-valid-ptr surface)
    (if key-color
	(set-colorkey surface key-color))
    (if alpha-value
	(set-alpha surface alpha-value))
    (let ((display-surface (if alpha-value
			       (SDL_DisplayFormatAlpha surface)
			       (SDL_DisplayFormat surface))))
      (if free-p
	  (Free-Surface surface))
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
	(push SDL_SRCCOLORKEY flags))
    (if alpha-value
	(push SDL_SRCALPHA flags))
    (if accel
	(push SDL_RLEACCEL flags))
    (case type
      (:sw (push SDL_SWSURFACE flags))
      (:hw (push SDL_HWSURFACE flags)))
    (if (is-valid-ptr surface)
	(with-foreign-slots ((BitsPerPixel Rmask Gmask Bmask Amask) (pixel-format surface) SDL_PixelFormat)
	    (setf surf (SDL_CreateRGBSurface (set-flags flags)
					     width height BitsPerPixel Rmask Gmask Bmask Amask)))
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
	      (setf surf (SDL_CreateRGBSurface (set-flags flags) width height bpp Rmask Gmask Bmask Amask)))))
    (if key-color
	(set-colorkey surf key-color :accel accel))
    (if alpha-value
	(set-alpha surf alpha-value :accel accel))
    surf))


;; cl-sdl "sdl-ext.lisp"
(defun must-lock? (surface)
  "Checks if a surface can be locked.
   Re-implementation of the SDL_MUSTLOCK macro.
   Returns
    T if the surface can be locked.
    NIL if the surface cannot be locked."
  (when (is-valid-ptr surface)
    (or (/= 0 (cffi:foreign-slot-value surface 'sdl_surface 'offset))
	(/= 0 (logand (cffi:foreign-slot-value surface 'sdl_surface 'flags)
		      (logior SDL_HWSURFACE
			      SDL_ASYNCBLIT
			      SDL_RLEACCEL))))))

(defun pixel-format (surface)
  "Returns the pixel format of a surface."
  (cffi:foreign-slot-value surface 'sdl:SDL_Surface 'sdl:format))

(defun set-alpha (surface alpha-value &key (accel nil))
  "Sets the alpha value for the given surface."
  (when (is-valid-ptr surface)
    (if accel
	(setf accel SDL_RLEACCEL)
	(setf accel 0))
    (if (null alpha-value)
	(SDL_SetAlpha surface accel 0)
	(SDL_SetAlpha surface (logior SDL_SRCALPHA accel) (clamp (to-int alpha-value) 0 255)))
    surface))

(defun set-colorkey (surface color &key (accel nil))
  "Sets the key color for the given surface. The key color is made transparent."
  (when (is-valid-ptr surface)
    (if (null color)
	(SDL_SetColorKey surface 0 0)
	(progn
	  (if accel
	      (setf accel SDL_RLEACCEL)
	      (setf accel 0))
	  (SDL_SetColorKey surface (logior SDL_SRCCOLORKEY accel) color)))
    surface))

(defun surf-w (surface)
  "return the width of the SDL_surface."
  (cffi:foreign-slot-value surface 'SDL_Surface 'w))

(defun surf-h (surface)
  "return the height of the SDL_Surface." 
  (cffi:foreign-slot-value surface 'SDL_Surface 'h))

(defun update-surface (surface &optional template number)
  "Updates the screen using the keyword co-ordinates in the Vector, :template.
   All co-ordinates default to 0, updating the entire screen."
  (if (is-valid-ptr template)
      (if number
	  (sdl_UpdateRects surface number template)
	  (SDL_UpdateRect surface 
			  (rect-x template)
			  (rect-y template)
			  (rect-w template)
			  (rect-h template)))
      (SDL_UpdateRect surface 0 0 0 0))
  surface)

(defun blit-surface (src dst src-rect dst-rect &key (update-p nil))
  "Blits the entire SRC SDL_Surface to the DST SDL_Surface using SDL_BlitSurface.
   use :src-rect SDL_Rect to blit only a portion of the SRC to the DST surface
   Use :dst-rect SDL_Rect to position the SRC on the DST surface."
  (sdl::Upper-Blit src src-rect dst dst-rect)
  (when update-p
    (update-surface dst dst-rect))
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
  (Fill-Rect surface template color)
  (when update-p
    (update-surface surface template))
  template)

