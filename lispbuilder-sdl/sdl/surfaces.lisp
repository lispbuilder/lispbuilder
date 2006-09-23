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
         (progn (when (must-lock-p ,var)
                  (lock-surface ,var))
                ,@body)
       (when (must-lock-p ,var)
         (unlock-surface ,var)))))

(defmacro with-must-lock-surface (surface &body body)
  "WITH-MUST-LOCKSURFACE sets up a surface for directly accessing the pixels using SDL_LockSurface.
   WITH-MUST-LOCKSURFACE uses SDL_MUSTLOCK to first check if the surface should be locked.
   Within WITH-MUST-LOCKSURFACE you can write to and read from surface->pixels, using the pixel format 
   stored in surface->format."
  (let ((surf (gensym)))
    `(let ((,surf ,surface))
      (block nil
	(when (sdl-must-lock ,surf)
	  (if (>= (SDL_LockSurface ,surf) 0)
	      (progn
		,@body)
	      (error "Cannot lock surface")))
	(when (sdl-must-lock ,surf)
	  (SDL_UnlockSurface ,surf))))))

;; cl-sdl "cl-sdl.lisp"
(defmacro with-possible-lock-and-update ((&key (surface *default-surface*) (check-lock-p t)
					       (update-p nil) (template nil)) &body body)
  (let ((locked-p (gensym "LOCKED-P"))
        (exit (gensym "EXIT"))
	(result (gensym "RESULT")))
    `(let ((,locked-p nil)
	   (,result nil))
      (block ,exit
        (when ,check-lock-p
          (when (must-lock-p ,surface)
            (when (< (sdl:SDL_LockSurface ,surface)
                     0)
              (return-from ,exit (values)))
            (setf ,locked-p t)))
        (setf ,result (progn ,@body))
        (when ,locked-p
          (SDL_UnlockSurface ,surface))
        (when ,update-p
          (update-surface :surface ,surface :template ,template))
	,result))))

(defmacro with-surface-lock(surface &body body)
  (let ((surf (gensym "SURF"))
	(result (gensym "RESULT")))
    `(let ((,surf ,surface)
	   (,result nil))
       (progn
	 (SDL_LockSurface ,surf)
	 (setf ,result (progn ,@body))
	 (SDL_UnlockSurface ,surf)
	 ,result))))

;; (defmacro with-surface (&optional (surface-ptr '*default-surface*) &body body)
;;   "Don't use this for managing the display surface."
;;   (let ((body-value (gensym "body-value"))
;; 	(old-default-surface (gensym "old-default-surface")))
;;     `(let* ((,body-value nil)
;; 	    (,old-default-surface *default-surface*)
;; 	    (*default-surface* ,surface-ptr))
;;       (when (is-valid-ptr ,surface-name)
;; 	(setf ,body-value (progn ,@body))
;; 	;; Here we try attempt to verify that the surface-ptr is not the actual display surface.
;; 	;; However according to the SDL documentation, we cannot be 100% sure.
;; 	;; "(SDL_GetVideoSurface) ... returns a pointer to the current display surface. If SDL is doing format
;; 	;; conversion on the display surface, this function returns the publicly visible surface,
;; 	;; not the real video surface.
;; 	(if (is-valid-ptr ,surface-name)
;; 	    (unless (cffi:pointer-eq ,surface-ptr (SDL_GetVideoSurface))
;; 	      (SDL_FreeSurface ,surface-name))))
;;       (setf *default-surface* ,old-default-surface)
;;       ,body-value)))

(defmacro with-surface ((surface-ptr &optional (free-p t)) &body body)
  "Don't use this for managing the display surface."
  (let ((body-value (gensym "body-value")))
    `(symbol-macrolet ((w (surf-w ,surface-ptr))
		       (h (surf-h ,surface-ptr)))
       (let ((*default-surface* ,surface-ptr)
	     (,body-value nil))
	 (when (is-valid-ptr *default-surface*)
	   (setf ,body-value (progn ,@body))
	   ;; Here we try attempt to verify that the surface-ptr is not the actual display surface.
	   ;; However according to the SDL documentation, we cannot be 100% sure.
	   ;; "(SDL_GetVideoSurface) ... returns a pointer to the current display surface. If SDL is doing format
	   ;; conversion on the display surface, this function returns the publicly visible surface,
	   ;; not the real video surface.
	   (if (and ,free-p (is-valid-ptr *default-surface*))
	       (unless (or (cffi:pointer-eq *default-surface* (SDL_GetVideoSurface))
			   (cffi:pointer-eq *default-surface* *default-display*))
		 (SDL_FreeSurface *default-surface*))))
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

(defmacro with-surfaces-free (bindings &rest body)
  (if bindings
      (let ((body-value (gensym "body-value")))
	`(let ((,body-value nil)
	       ,@(loop for binding in bindings
		    collect `(,(first binding) ,(second binding))))
	   (when (and ,@(loop for binding in bindings
			   collect `(is-valid-ptr ,(first binding))))
	     (setf ,body-value (progn ,@body))
	     ;; Here we try attempt to verify that the surface-ptr is not the actual display surface.
	     ;; However according to the SDL documentation, we cannot be 100% sure.
	     ;; "(SDL_GetVideoSurface) ... returns a pointer to the current display surface. If SDL is doing format
	     ;; conversion on the display surface, this function returns the publicly visible surface,
	     ;; not the real video surface.
	     ,@(loop for binding in bindings
		  collect `(if (is-valid-ptr ,(first binding))
			       (unless (or (cffi:pointer-eq ,(first binding) (SDL_GetVideoSurface))
					   (cffi:pointer-eq ,(first binding) *default-display*))
				 (SDL_FreeSurface ,(first binding))))))
	   ,body-value))))

(defun clear-colorkey (rel-accel &key (surface *default-surface*))
  "Removes the key color from the given surface."
  (when (is-valid-ptr surface)
    (if rel-accel
	(setf rel-accel SDL_RLEACCEL)
	(setf rel-accel 0))
    (SDL_SetColorKey surface rel-accel 0)))

(defun convert-surface-to-display-format (&key key-color alpha-value (free-p nil) (surface *default-surface*))
  "converts a surface to display format and free's the source surface
    :alpha t will convert the surface and add an alpha channel.
    :free nil will not free surface.
   returns NIL if the surface cannot be converted."
  ;; LJC: Added support for converting to an alpha surface.
  ;; LJC: Freeing surface is now optional.
  (when (is-valid-ptr surface)
    (if key-color
	(set-colorkey :color key-color :surface surface))
    (if alpha-value
	(set-alpha alpha-value :surface surface))
    (let ((display-surface (if alpha-value
			       (SDL_DisplayFormatAlpha surface)
			       (SDL_DisplayFormat surface))))
      (if free-p
	  (SDL_FreeSurface surface))
      (if (is-valid-ptr display-surface)
	  display-surface
	  nil))))

(defun copy-surface (&key key-color alpha-value (type :sw) accel (surface *default-surface*))
  "create a surface compatible with the supplied surface"
  (create-surface (surf-w surface) (surf-h surface)
		  :surface surface
		  :key-color key-color
		  :alpha-value alpha-value
		  :type type
		  :accel accel))

(defun create-surface(width height &key (bpp 32) surface pixels pitch key-color alpha-value (type :sw) (accel nil))
  "create a surface compatible with the supplied :surface, if provided."
  (let ((surf nil) (flags nil) (bpp 32))
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
	(with-foreign-slots ((BitsPerPixel Rmask Gmask Bmask Amask) (pixelformat surface) SDL_PixelFormat)
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
	(set-colorkey :color key-color :surface surf :accel accel))
    (if alpha-value
	(set-alpha alpha-value :surface surf :accel accel))
    surf))

(defun get-clip-rect (&optional (surface *default-surface*))
  (cffi:with-foreign-object (r 'sdl_rect)
    (getcliprect surface r)
    (rectangle (cffi:foreign-slot-value r 'sdl_rect 'x)
	       (cffi:foreign-slot-value r 'sdl_rect 'y)
	       (cffi:foreign-slot-value r 'sdl_rect 'w)
	       (cffi:foreign-slot-value r 'sdl_rect 'h))))

(defun get-surface-rect (&optional (surface *default-surface*))
  "Returns a rectangle containing the surfaces width and height. X and Y are both set to 0."
  (rectangle 0 0 (surf-w surface) (surf-h surface)))

(defun map-color (&key (color *default-color*) (surface *default-surface*))
  (let ((int-color (vec-to-int color)))
    (if (equal 3 (length int-color))
	(sdl:SDL_MapRGB (pixelformat surface)
			(color-r int-color) (color-g int-color) (color-b int-color))
	(sdl:SDL_MapRGBA (pixelformat surface)
			 (color-r int-color) (color-g int-color) (color-b int-color) (color-a int-color)))))

;; cl-sdl "sdl-ext.lisp"
(defun must-lock-p (&optional (surface *default-surface*))
  (or (/= 0 (cffi:foreign-slot-value surface 'sdl_surface 'offset))
      (/= 0 (logand (cffi:foreign-slot-value surface 'sdl_surface 'flags)
		    (logior SDL_HWSURFACE
			    SDL_ASYNCBLIT
			    SDL_RLEACCEL)))))

(defun pixelformat (&optional (surface *default-surface*))
  "Returns the pixelformat of a surface."
  (cffi:foreign-slot-value surface 'sdl:SDL_Surface 'sdl:format))

(defun sdl-must-lock (&optional (surface *default-surface*))
  "Checks if a surface can be locked.
   Re-implementation of the SDL_MUSTLOCK macro.
   Returns
    T if the surface can be locked.
    NIL if the surface cannot be locked."
  (if (> 0 (cffi:foreign-slot-value surface 'SDL_Surface 'offset))
      t
      (if (not (eql 0 (logand 
		       (cffi:foreign-slot-value surface 'SDL_Surface 'flags)
		       (logior SDL_HWSURFACE SDL_ASYNCBLIT SDL_RLEACCEL))))
	  t
	  nil)))


(defun set-alpha (alpha-value &key (accel nil) (surface *default-surface*))
  "Sets the alpha value for the given surface."
  (when (is-valid-ptr surface)
    (if accel
	(setf accel SDL_RLEACCEL)
	(setf accel 0))
    (if (null alpha-value)
	(SDL_SetAlpha surface accel 0)
	(SDL_SetAlpha surface (logior SDL_SRCALPHA accel) (clamp (to-int alpha-value) 0 255)))
    surface))

(defun set-colorkey (&key (color *default-color*) (accel nil) (surface *default-surface*))
  "Sets the key color for the given surface. The key color is made transparent."
  (when (is-valid-ptr surface)
    (if (null color)
	(SDL_SetColorKey surface 0 0)
	(progn
	  (if accel
	      (setf accel SDL_RLEACCEL)
	      (setf accel 0))
	  (SDL_SetColorKey surface (logior SDL_SRCCOLORKEY accel) (map-color :color color :surface surface))))
    surface))

(defun set-clip-rect (&key (rectangle *default-rectangle*) (surface *default-surface*))
  (setcliprect surface rectangle))

(defun surf-w (&optional (surface *default-surface*))
  "return the width of the SDL_surface."
  (cffi:foreign-slot-value surface 'SDL_Surface 'w))

(defun surf-h (&optional (surface *default-surface*))
  "return the height of the SDL_Surface." 
  (cffi:foreign-slot-value surface 'SDL_Surface 'h))

(defun update-surface (&key (template nil) (surface *default-surface*))
  "Updates the screen using the keyword co-ordinates in the Vector, :template.
   All co-ordinates default to 0, updating the entire screen."
  (if (is-valid-ptr surface)
      (let ((int-template (vec-to-int template)))
	(if template
	    (SDL_UpdateRect surface 
			    (rect-x int-template)
			    (rect-y int-template)
			    (rect-w int-template)
			    (rect-h int-template))
	    (SDL_UpdateRect surface 0 0 0 0)))
      surface))

(defun blit-surface (&key (src *default-surface*) (dst *default-display*) src-rect dst-rect
		     (position *default-position*) (free-p nil) update-p)
  "Blits the entire SRC SDL_Surface to the DST SDL_Surface using SDL_BlitSurface.
   use :src-rect SDL_Rect to blit only a portion of the SRC to the DST surface
   Use :dst-rect SDL_Rect to position the SRC on the DST surface."
  (if src-rect
      (if (= 2 (length src-rect))
	  (setf src-rect (vector (rect-x src-rect) (rect-y src-rect) (surf-w src) (surf-h src)))))
  (if dst-rect
      (if (= 2 (length dst-rect))
	  (setf dst-rect (vector (rect-x dst-rect) (rect-y dst-rect) (surf-w src) (surf-h src)))))
  (with-possible-lock-and-update (:surface dst :check-lock-p nil :update-p update-p :template dst-rect)
    (sdl::UpperBlit src src-rect dst dst-rect))
  (if free-p
      (when (is-valid-ptr src)
	(SDL_FreeSurface src)))
  dst-rect)

(defun fill-surface (&key (surface *default-surface*) (color *default-color*) (template nil) (update-p nil) (clipping-p t))
  "fill the entire surface with the specified R G B A color.
   Use :template to specify the SDL_Rect to be used as the fill template.
   Use :update-p to call SDL_UpdateRect, using :template if provided. This allows for a 
   'dirty recs' screen update."
  (when clipping-p
    (let* ((x (rect-x template)) (y (rect-y template))
	   (w (rect-w template)) (h (rect-h template))
	   (x2 (+ x w)) (y2 (+ y h)))
      (check-bounds 0 (surf-w surface) x x2)
      (check-bounds 0 (surf-h surface) y y2)
      (setf w (- x2 x)
            h (- y2 y))
      (setf template (vector x y w h))))
  (with-possible-lock-and-update (:surface surface :check-lock-p nil :update-p update-p :template template)
    (FillRect surface template (map-color :color color :surface surface)))
  template)

