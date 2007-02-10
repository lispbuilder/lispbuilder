;; This file contains some useful functions for using SDL_ttf from Common lisp
;; 2006 (c) Rune Nesheim, see LICENCE.
;; 2007 (c) Luke Crook

(in-package #:lispbuilder-sdl-ttf)

(defun render-string-solid (text &key
			    (font *default-font*)
			    (color sdl:*default-color*)
			    (free nil)
			    (cache nil))
  "Render text TEXT using font FONT with color COLOR into a new SURFACE, using the Solid mode. 
Unless :FREE T, the caller is responsible for freeing the new SURFACE.
Use :CACHE T to cache the newly created surface in the FONT object.

  * TEXT is the text to render.

  * FONT is a FONT object.  Bound to *DEFAULT-FONT* by default. 

  * COLOR color is the color used to render text, of type SDL:SDL-COLOR

  * FREE when T will free the old cached SURFACE in FONT.

  * CACHE when T will cache the newly created SURFACE in FONT.

  * Returns the new cached surface SDL:SDL-SURFACE.

For example:
  * (DRAW-STRING-SOLID \"Hello World!\" :FONT *DEFAULT-FONT* :COLOR A-COLOR)"
  (check-type font font)
  (check-type color sdl:sdl-color)
  (when free
    (sdl:free-cached-surface font))
  (let ((surf
	 (sdl:with-foreign-color-copy (col-struct color)
	   (sdl:surface (sdl-ttf-cffi::ttf-Render-UTF8-Solid (fp-font font) text col-struct)))))
    (when cache
      (setf (sdl:cached-surface font) surf))
    surf))

(defun draw-string-solid (text position &key 
			  (font *default-font*)
			  (surface sdl:*default-surface*)
			  (color sdl:*default-color*))
  "See DRAW-STRING-SOLID-*.

  * POSITION is the x and y position to render the text, of type SDL:POINT."
  (check-type position sdl:point) 
  (draw-string-solid-* text (sdl:x position) (sdl:y position)
		       :font font
		       :surface surface
		       :color color))

(defun draw-string-solid-* (text x y &key 
			    (font *default-font*)
			    (surface sdl:*default-surface*)
			    (color sdl:*default-color*))
  "Draw text TEXT using font FONT with color COLOR onto surface SURFACE, using the Solid mode. 

  * TEXT is the text to render.

  * X/Y are the x and y position coordinates, as INTEGERS.

  * FONT is a FONT object.  Bound to *DEFAULT-FONT* by default. 

  * SURFACE is the surface to render text onto, of type SDL:SDL-SURFACE 

  * COLOR color is the color used to render text, of type SDL:SDL-COLOR

  * Returns the font FONT.

For example:
  * (DRAW-STRING-SOLID-* \"Hello World!\" 0 0 :FONT *DEFAULT-FONT* :SURFACE A-SURFACE :COLOR A-COLOR)"
  (check-type surface sdl:sdl-surface)
  (sdl:with-surface (font-surface (render-string-solid text
						       :font font
						       :color color
						       :free nil
						       :cache nil)
				  t)
    (sdl:set-surface-* font-surface :x x :y y)
    (sdl:blit-surface font-surface surface))
  font)
