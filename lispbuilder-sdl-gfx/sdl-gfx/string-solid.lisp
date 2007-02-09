
(in-package :lispbuilder-sdl-gfx)


(defun render-string-solid (string &key (color sdl:*default-color*) (free nil) (cache nil))
  "Render string STRING with color COLOR into a new SURFACE, using the Solid mode. 
Unless :FREE T, the caller is responsible for freeing the new SURFACE.
Use :CACHE T to cache the newly created surface in the FONT object.

  * STRING is the text to render. 

  * COLOR color is the color used to render text, of type SDL:SDL-COLOR

  * FREE when T will free the old cached SURFACE in FONT.

  * CACHE when T will cache the newly created SURFACE in FONT.

  * Returns the new cached surface SDL:SDL-SURFACE.

For example:
  * (DRAW-STRING-SOLID \"Hello World!\" :COLOR A-COLOR)"
  (when (and free (typep (sdl:cached-surface *default-font*) 'sdl:sdl-surface))
    (sdl:free-surface (sdl:cached-surface *default-font*)))
  (let ((surf (sdl:convert-surface :surface (sdl:create-surface (* (font-width *default-font*)
								   (length string))
								(font-height *default-font*)
								:key-color sdl:*black*)
				   :free-p t)))
    (draw-string-solid-* string 0 0
			 :surface surf
			 :color color)
    (when cache
      (setf (sdl:cached-surface *default-font*) surf))
    surf))

(defun draw-character-solid (c p1 &key (surface sdl:*default-surface*) (color sdl:*default-color*))
  "See DRAW-CHARACTER-SOLID-*.

  * P1 is the x and y position to render the text, of type SDL:POINT."
  (draw-character-solid-* c (sdl:x p1) (sdl:y p1) :surface surface :color color))

(defun draw-character-solid-* (c x y &key (surface sdl:*default-surface*) (color sdl:*default-color*))
  "Draw character C with color COLOR onto surface SURFACE, using the Solid mode. 

  * C is the character to render. 

  * X/Y are the x and y position coordinates, as INTEGERS.

  * SURFACE is the surface to render text onto, of type SDL:SDL-SURFACE 

  * COLOR color is the color used to render text, of type SDL:SDL-COLOR

  * Returns the font FONT.

For example:
  * (DRAW-CHARACTER-SOLID-* \"Hello World!\" 0 0 :SURFACE A-SURFACE :COLOR A-COLOR)"
  (unless (typep surface 'sdl:sdl-surface)
    (error ":surface must be of type SDL:SURFACE."))
  (unless (or (typep color 'sdl:color) (typep color 'sdl:color-a))
    (error ":color must be of type SDL:COLOR or SDL:COLOR-A."))
  (when (typep color 'sdl:color)
    (sdl-gfx-cffi::character-color (sdl:fp surface) x y c
				   (sdl:pack-color color)))
  (when (typep color 'sdl:color-a)
    (sdl-gfx-cffi::character-RGBA (sdl:fp surface) x y c
				  (sdl:r color) (sdl:g color) (sdl:b color) (sdl:a color))))

(defun draw-string-solid (c p1 &key (surface sdl:*default-surface*) (color sdl:*default-color*))
  "See DRAW-STRING-SOLID-*.

  * P1 is the x and y position to render the text, of type SDL:POINT."
  (draw-string-solid-* c (sdl:x p1) (sdl:y p1) :surface surface :color color))

(defun draw-string-solid-* (c x y &key (surface sdl:*default-surface*) (color sdl:*default-color*))
  "Draw text C with color COLOR onto surface SURFACE, using the Solid mode. 

  * C is the text to render. 

  * X/Y are the x and y position coordinates, as INTEGERS.

  * SURFACE is the surface to render text onto, of type SDL:SDL-SURFACE 

  * COLOR color is the color used to render text, of type SDL:SDL-COLOR

  * Returns the font FONT.

For example:
  * (DRAW-STRING-SOLID-* \"Hello World!\" 0 0 :SURFACE A-SURFACE :COLOR A-COLOR)"
  (unless (typep surface 'sdl:sdl-surface)
    (error ":surface must be of type SDL:SURFACE."))
  (unless (or (typep color 'sdl:color) (typep color 'sdl:color-a))
    (error ":color must be of type SDL:COLOR or SDL:COLOR-A."))
  (when (typep color 'sdl:color)
    (sdl-gfx-cffi::string-color (sdl:fp surface) x y c
				(sdl:pack-color color)))
  (when (typep color 'sdl:color-a)
    (sdl-gfx-cffi::string-RGBA (sdl:fp surface) x y c
			       (sdl:r color) (sdl:g color) (sdl:b color) (sdl:a color))))
