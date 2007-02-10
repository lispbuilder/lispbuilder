;; This file contains some useful functions for using SDL_ttf from Common lisp
;; 2006 (c) Rune Nesheim, see LICENCE.
;; 2007 (c) Luke Crook

(in-package #:lispbuilder-sdl-ttf)

(defun render-string-shaded (text fg-color bg-color &key
			     (font *default-font*)
			     (free nil)
			     (cache nil))
  "Render text TEXT using font FONT with color COLOR into a new SURFACE, using the Shaded mode. 
Unless :FREE T, the caller is responsible for freeing the new SURFACE.
Use :CACHE T to cache the newly created surface in the FONT object.

  * TEXT is the text to render.

  * FG-COLOR is the foreground color of the text, of type SDL:SDL-COLOR

  * BG-COLOR is the background color of the text, of type SDL:SDL-COLOR

  * FONT is a FONT object.  Bound to *DEFAULT-FONT* by default. 

  * FREE when T will free the old cached SURFACE in FONT.

  * CACHE when T will cache the newly created SURFACE in FONT.

  * Returns the new cached surface SDL:SDL-SURFACE.

For example:
  * (DRAW-STRING-SHADED \"Hello World!\" fg-col bg-col  :FONT *DEFAULT-FONT* :COLOR A-COLOR)"
  (check-type font font "FONT must be of type FONT.")
  (check-type fg-color sdl:sdl-color "FG-COLOR must be of type SDL:SDL-COLOR.")
  (check-type bg-color sdl:sdl-color "BG-COLOR must be of type SDL:SDL-COLOR.")
  (when (and free (sdl:cached-surface font))
    (sdl:free-surface (sdl:cached-surface font))
    (setf (sdl:cached-surface font) nil))
  (let ((surf
	 (sdl:with-foreign-color-copy (fg-struct fg-color)
	   (sdl:with-foreign-color-copy (bg-struct bg-color)
	     (sdl:surface (sdl-ttf-cffi::ttf-Render-UTF8-shaded (fp-font font)
								text fg-struct bg-struct))))))
    (when cache
      (setf (sdl:cached-surface font) surf))
    surf))


(defun draw-string-shaded (text position fg-color bg-color &key
			   (font *default-font*)
			   (surface sdl:*default-surface*))
  "See DRAW-STRING-SHADED-*

  * :POSITION is the x and y position to render the text, of type SDL:POINT."
  (check-type position sdl:point "POSITION must be of type SDL:POINT")
  (draw-string-shaded-* text (sdl:x position) (sdl:y position) fg-color bg-color
			:font font
			:surface surface))

(defun draw-string-shaded-* (text x y fg-color bg-color &key
			     (font *default-font*)
			     (surface sdl:*default-surface*))
  "Draw text TEXT using font :FONT with foreground color FG-COLOR and background color BG-COLOR 
onto surface :SURFACE, using the Shaded mode. 

  * TEXT is the text to render.

  * X/Y are the x and y position coordinates, as INTEGERS.

  * FG-COLOR is the foreground color of the text, of type SDL:SDL-COLOR

  * BG-COLOR is the background color of the text, of type SDL:SDL-COLOR

  * FONT is a FONT object.  Bound to *DEFAULT-FONT* by default. 

  * SURFACE is the surface to render text onto, of type SDL:SDL-SURFACE 

  * Returns the font FONT.

For example:
  * (DRAW-STRING-SHADED-* \"Hello World!\" 0 0 fg-col bg-col :FONT *DEFAULT-FONT* :SURFACE A-SURFACE)"
  (check-type surface sdl:sdl-surface "SURFACE must be of type SDL:SDL-SURFACE.")
  (sdl:with-surface (font-surface (render-string-shaded text fg-color bg-color
							:font font
							:cache nil
							:free nil)
				  t)
    (sdl:set-surface-* font-surface :x x :y y)
    (sdl:blit-surface font-surface surface))
  font)
