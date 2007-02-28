
(in-package :lispbuilder-sdl)

(defun render-string-solid (str &key
			    (font *default-font*)
			    (color *default-color*)
			    (free nil)
			    (cache nil))
  "Render string STR using font FONT with color COLOR into a new SURFACE, using the Solid mode. 
Unless :FREE T, the caller is responsible for freeing the new SURFACE.
Use :CACHE T to cache the newly created surface in the FONT object.

  * STR is the text to render. 

  * FONT is a FONT object.  Bound to *DEFAULT-FONT* by default. 

  * COLOR color is the color used to render text, of type SDL-COLOR

  * FREE when T will free the old cached SURFACE in FONT.

  * CACHE when T will cache the newly created SURFACE in FONT.

  * Returns the new surface SDL-SURFACE.

For example:
  * (RENDER-STRING-SOLID \"Hello World!\" :COLOR A-COLOR)"
  (when free
    (free-cached-surface font))
  (let* ((key-color (any-color-but-this color))
	 (surf (convert-surface :surface (create-surface (* (char-width font)
								    (length str))
								 (char-height font))
				    :key-color key-color
				    :free-p t)))
    (draw-string-solid-* str 0 0
			 :font font
			 :surface surf
			 :color color)
    (when cache
      (setf (cached-surface font) surf))
    surf))

(defun draw-string-solid (str p1 &key
			  (justify :left)
			  (surface *default-surface*)
			  (font *default-font*)
			  (color *default-color*))
  "See DRAW-STRING-SOLID-*.

  * P1 is the x and y position to render the text, of type POINT."
  (check-type p1 point)
  (draw-string-solid-* str (x p1) (y p1)
		       :justify justify
		       :surface surface
		       :font font
		       :color color))

(defun draw-string-solid-* (str x y &key
			    (justify :left)
			    (surface *default-surface*)
			    (font *default-font*)
			    (color *default-color*))
  "Draw text STR using font FONT with color COLOR onto surface SURFACE, using the Solid mode. 

  * STR is the text to render. 

  * X/Y are the x and y position coordinates, as INTEGERS.

  * FONT is a FONT object.  Bound to *DEFAULT-FONT* by default. 

  * SURFACE is the surface to render text onto, of type SDL-SURFACE 

  * COLOR color is the color used to render text, of type SDL-COLOR

  * Returns the font FONT.

For example:
  * (DRAW-STRING-SOLID-* \"Hello World!\" 0 0 :SURFACE A-SURFACE :COLOR A-COLOR)"
  (check-type color sdl-color)
  (check-type font bitmap-font)
  (unless surface
    (setf surface *default-display*))
  (case justify
    (:left (draw-string-left-justify-* str x y color nil :surface surface :font font))
    (:right (draw-string-right-justify-* str x y color nil :surface surface :font font))
    (:center (draw-string-centered-* str x y color nil :surface surface :font font))
    (otherwise (error ":JUSTIFY must be one of :LEFT, :RIGHT or :CENTER"))))
