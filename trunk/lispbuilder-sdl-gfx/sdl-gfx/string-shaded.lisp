
(in-package :lispbuilder-sdl-gfx)

(defun render-string-shaded (string fg-color bg-color &key (free nil) (cache nil))
  "Render string STRING with foreground and background colors FG-COLOR and BG-COLOR 
into a new SURFACE, using the Shaded mode. 
Unless :FREE T, the caller is responsible for freeing the new SURFACE.
Use :CACHE T to cache the newly created surface in the FONT object.

  * STRING is the text to render. 

  * FG-COLOR color is the foreground color used to render text, of type SDL:SDL-COLOR

  * BG-COLOR color is the background color used to render text, of type SDL:SDL-COLOR

  * FREE when T will free the old cached SURFACE in FONT.

  * CACHE when T will cache the newly created SURFACE in FONT.

  * Returns the new cached surface SDL:SDL-SURFACE.

For example:
  * (DRAW-STRING-SHADED \"Hello World!\" F-COLOR B-COLOR)"
  (when (and free (typep (sdl:cached-surface *default-font*) 'sdl:sdl-surface))
    (sdl:free-surface (sdl:cached-surface *default-font*)))
  (let ((surf (sdl:convert-surface :surface (sdl:create-surface (* (font-width *default-font*)
								   (length string))
								(font-height *default-font*))
				   :free-p t)))
    (sdl:fill-surface bg-color :surface surf)
    (draw-string-shaded-* string 0 0 fg-color bg-color
			  :surface surf)
    (when cache 
      (setf (sdl:cached-surface *default-font*) surf))
    surf))

(defun draw-character-shaded (c p1 fg-color bg-color &key (surface sdl:*default-surface*))
  "See DRAW-CHARACTER-SHADED-*.

  * P1 is the x and y position to render the text, of type SDL:POINT."  
  (draw-character-shaded-* c (sdl:x p1) (sdl:y p1) fg-color bg-color :surface surface))

(defun draw-character-shaded-* (c x y fg-color bg-color &key (surface sdl:*default-surface*))
  "Draw character C with foreground and background colors FG-COLOR and BG-COLOR 
onto surface SURFACE, using the Solid mode. 

  * C is the character to render. 

  * X/Y are the x and y position coordinates, as INTEGERS.

  * FG-COLOR color is the foreground color used to render text, of type SDL:SDL-COLOR

  * BG-COLOR color is the background color used to render text, of type SDL:SDL-COLOR

  * SURFACE is the surface to render text onto, of type SDL:SDL-SURFACE 

  * Returns the font FONT.

For example:
  * (DRAW-CHARACTER-SHADED-* \"Hello World!\" 0 0 F-COLOR B-COLOR :SURFACE A-SURFACE)"
  (unless (typep surface 'sdl:sdl-surface)
    (error ":surface must be of type SDL:SURFACE."))
  (unless (or (typep fg-color 'sdl:color) (typep fg-color 'sdl:color-a))
    (error "FG-COLOR must be of type SDL:COLOR or SDL:COLOR-A."))
  (unless (or (typep bg-color 'sdl:color) (typep bg-color 'sdl:color-a))
    (error "BG-COLOR must be of type SDL:COLOR or SDL:COLOR-A."))
  (sdl:draw-box-* x y
		  (* (font-width *default-font*)
		     (length c))
		  (font-height *default-font*)
		  :color bg-color
		  :surface surface)
  (when (typep fg-color 'sdl:color)
    (sdl-gfx-cffi::character-color (sdl:fp surface) x y c
				   (sdl:pack-color fg-color)))
  (when (typep fg-color 'sdl:color-a)
    (sdl-gfx-cffi::character-RGBA (sdl:fp surface) x y c
				  (sdl:r fg-color) (sdl:g fg-color) (sdl:b fg-color) (sdl:a fg-color))))

(defun draw-string-shaded (c p1 fg-color bg-color &key (surface sdl:*default-surface*))
  "See DRAW-STRING-SHADED-*.

  * P1 is the x and y position to render the text, of type SDL:POINT."
  (draw-string-shaded-* c (sdl:x p1) (sdl:y p1) fg-color bg-color :surface surface))

(defun draw-string-shaded-* (c x y fg-color bg-color &key (surface sdl:*default-surface*))
  "Draw text C with foreground and background colors FG-COLOR and BG-COLOR color COLOR 
onto surface SURFACE, using the SHADED mode. 

  * C is the text to render. 

  * X/Y are the x and y position coordinates, as INTEGERS.

  * FG-COLOR color is the foreground color used to render text, of type SDL:SDL-COLOR

  * BG-COLOR color is the background color used to render text, of type SDL:SDL-COLOR

  * SURFACE is the surface to render text onto, of type SDL:SDL-SURFACE 

  * Returns the font FONT.

For example:
  * (DRAW-STRING-SHADED-* \"Hello World!\" 0 0 F-COLOR B-COLOR :SURFACE A-SURFACE)"
  (unless (typep surface 'sdl:sdl-surface)
    (error ":surface must be of type SDL:SURFACE."))
  (unless (or (typep fg-color 'sdl:color) (typep fg-color 'sdl:color-a))
    (error "FG-COLOR must be of type SDL:COLOR or SDL:COLOR-A."))
  (unless (or (typep bg-color 'sdl:color) (typep bg-color 'sdl:color-a))
    (error "BG-COLOR must be of type SDL:COLOR or SDL:COLOR-A."))
  (sdl:draw-box-* x y
		  (* (font-width *default-font*)
		     (length c))
		  (font-height *default-font*)
		  :color bg-color
		  :surface surface)
  (when (typep fg-color 'sdl:color)
    (sdl-gfx-cffi::string-color (sdl:fp surface) x y c
				(sdl:pack-color fg-color)))
  (when (typep fg-color 'sdl:color-a)
    (sdl-gfx-cffi::string-RGBA (sdl:fp surface) x y c
			       (sdl:r fg-color) (sdl:g fg-color) (sdl:b fg-color) (sdl:a fg-color))))
