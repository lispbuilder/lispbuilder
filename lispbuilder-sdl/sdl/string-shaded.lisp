
(in-package :lispbuilder-sdl)

(defun render-string-shaded (str fg-color bg-color &key
			    (font *default-font*)
			    (free nil)
			    (cache nil))
  "Render string STR using font FONT with foreground and background colors FG-COLOR and BG-COLOR 
into a new SURFACE, using the Shaded mode. 
Unless :FREE T, the caller is responsible for freeing the new SURFACE.
Use :CACHE T to cache the newly created surface in the FONT object.

  * STR is the text to render. 

  * FG-COLOR color is the foreground color used to render text, of type SDL-COLOR

  * BG-COLOR color is the background color used to render text, of type SDL-COLOR

  * FONT is a FONT object.  Bound to *DEFAULT-FONT* by default. 

  * FREE when T will free the old cached SURFACE in FONT.

  * CACHE when T will cache the newly created SURFACE in FONT.

  * Returns the new surface SDL-SURFACE.

For example:
  * (RENDER-STRING-SHADED \"Hello World!\" F-COLOR B-COLOR)"
  (check-types sdl-color fg-color bg-color)
  (when free
    (free-cached-surface font))
  (let ((surf (convert-surface :surface (create-surface (* (char-width font)
								   (length str))
								(char-height font))
				   :free-p t)))
    (draw-string-shaded-* str 0 0 fg-color bg-color
			  :font font
			  :surface surf)
    (when cache
      (setf (cached-surface font) surf))
    surf))

(defun draw-string-shaded (str p1 fg-color bg-color &key
			  (justify :left)
			  (surface *default-surface*)
			  (font *default-font*))
  "See DRAW-STRING-SHADED-*.

  * P1 is the x and y position to render the text, of type POINT."
  (check-type p1 point)
  (draw-string-shaded-* str (x p1) (y p1) fg-color bg-color
			:justify justify
			:surface surface
			:font font))

(defun draw-string-shaded-* (str x y fg-color bg-color &key
			    (justify :left)
			    (surface *default-surface*)
			    (font *default-font*))
  "Draw text STR using font FONT with foreground and background colors FG-COLOR and BG-COLOR color COLOR 
onto surface SURFACE, using the SHADED mode. 

  * STR is the text to render. 

  * X/Y are the x and y position coordinates, as INTEGERS.

  * FG-COLOR color is the foreground color used to render text, of type SDL-COLOR

  * BG-COLOR color is the background color used to render text, of type SDL-COLOR

  * FONT is a FONT object.  Bound to *DEFAULT-FONT* by default. 

  * SURFACE is the surface to render text onto, of type SDL-SURFACE 

  * Returns the font FONT.

For example:
  * (DRAW-STRING-SHADED-* \"Hello World!\" 0 0 F-COLOR B-COLOR :SURFACE A-SURFACE)"
  (check-types sdl-color fg-color bg-color)
  (check-type font bitmap-font)
  (unless surface
    (setf surface *default-display*))
  (case justify
    (:left (draw-string-left-justify-* str x y fg-color bg-color :surface surface :font font))
    (:right (draw-string-right-justify-* str x y fg-color bg-color :surface surface :font font))
    (:center (draw-string-centered-* str x y fg-color bg-color :surface surface :font font))
    (otherwise (error ":JUSTIFY must be one of :LEFT, :RIGHT or :CENTER"))))
