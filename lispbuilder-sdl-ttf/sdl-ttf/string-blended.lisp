;; This file contains some useful functions for using SDL_ttf from Common lisp
;; 2006 (c) Rune Nesheim, see LICENCE.
;; 2007 (c) Luke Crook

(in-package #:lispbuilder-sdl-ttf)


(defun render-string-blended (text &key
			      (encoding :latin1)
			      (font *default-font*)
			      (color sdl:*default-color*)
			      (free nil)
			      (cache nil))
  "Render text TEXT using font FONT with color COLOR into a new SURFACE, using the Blended mode. 
Unless :FREE T, the caller is responsible for freeing the new SURFACE.
Use :CACHE T to cache the newly created surface in the FONT object.

  * TEXT is the text to render. TEXT may be of the encoding type LATIN1, UTF8, UNICODE, GLYPH. TEXT must match :ENCODING

  * ENCODING specifies the format of the text to render and is one of: 
    * :LATIN1
    * :UTF8
    * :UNICODE
    * :GLYPH

  * FONT is a FONT object.  Bound to *DEFAULT-FONT* by default. 

  * COLOR color is the color used to render text, of type SDL:SDL-COLOR

  * FREE when T will free the old cached SURFACE in FONT.

  * CACHE when T will cache the newly created SURFACE in FONT.

  * Returns the new cached surface SDL:SDL-SURFACE.

For example:
  * (DRAW-STRING-BLENDED \"Hello World!\" :ENCODING :LATIN1 :FONT *DEFAULT-FONT* :COLOR A-COLOR)"
  (unless (typep font 'font)
    (error "ERROR: RENDER-STRING-BLENDED; FONT must be of type FONT."))
  (unless (typep color 'sdl:sdl-color)
    (error "ERROR: RENDER-STRING-BLENDED; COLOR must be of type SDL:SDL-COLOR."))
  (when (and free (sdl:cached-surface font))
    (sdl:free-surface (sdl:cached-surface font))
    (setf (sdl:cached-surface font) nil))
  (let ((surf 
	 (case encoding
	   (:latin1
	    (sdl:with-foreign-color-copy (col-struct color)
	      (sdl:surface (sdl-ttf-cffi::ttf-Render-Text-blended (fp-font font) text col-struct))))
	   (:UTF8
	    (sdl:with-foreign-color-copy (col-struct color)
	      (sdl:surface (sdl-ttf-cffi::ttf-Render-UTF8-blended (fp-font font) text col-struct))))
	   (:GLYPH
	    (sdl:with-foreign-color-copy (col-struct color)
	      (sdl:surface (sdl-ttf-cffi::ttf-Render-Glyph-blended (fp-font font) text col-struct))))
	   (:UNICODE
	    ;; TODO
	    )
	   (otherwise
	    (error "ERROR: RENDER-STRING-BLENDED; ENCODING must be one of :LATIN1, :UTF8, :GLYPH or :UNICODE.")))))
    (when cache
      (setf (sdl:cached-surface font) surf))
    surf))


(defun draw-string-blended (text position &key
			    (encoding :latin1)
			    (font *default-font*)
			    (surface sdl:*default-surface*)
			    (color sdl:*default-color*))
  "See DRAW-STRING-BLENDED-*.
  * :POSITION is the x and y position to render the text, of type SDL:POINT."
  (unless (typep position 'sdl:point)
    (error "ERROR: DRAW-STRING-BLENDED; POSITION must be of type SDL:POINT."))
  (draw-string-blended-* text (sdl:x position) (sdl:y position)
			 :encoding encoding
			 :font font
			 :surface surface
			 :color color))


(defun draw-string-blended-* (text x y &key
			      (encoding :latin1)
			      (font *default-font*)
			      (surface sdl:*default-surface*)
			      (color sdl:*default-color*))
  "Draw text TEXT using font :FONT with color :COLOR onto surface :SURFACE, using the Blended mode. 

  * TEXT is the text to render. TEXT may be of the encoding type LATIN1, UTF8, UNICODE, GLYPH. TEXT must match :ENCODING

  * X/Y are the x and y position coordinates, as INTEGERS.

  * ENCODING specifies the format of the text to render and is one of: 
    * :LATIN1
    * :UTF8
    * :UNICODE
    * :GLYPH

  * FONT is a FONT object.  Bound to *DEFAULT-FONT* by default. 

  * SURFACE is the surface to render text onto, of type SDL:SDL-SURFACE 

  * COLOR color is the color used to render text, of type SDL:SDL-COLOR

  * Returns the font FONT.

For example:
  * (DRAW-STRING-SOLID-* \"Hello World!\" 0 0 :ENCODING :LATIN1 :FONT *DEFAULT-FONT* :SURFACE A-SURFACE :COLOR A-COLOR)"
  (unless (typep surface 'sdl:sdl-surface)
    (error "ERROR: DRAW-STRING-BLENDED-*; SURFACE must be of type SDL:SDL-SURFACE."))
  (sdl:with-surface (font-surface (render-string-blended text
							 :encoding encoding
							 :font font
							 :color color
							 :cache nil
							 :free nil)
				  t)
    (sdl:set-surface-* font-surface :x x :y y)
    (sdl:blit-surface font-surface surface))
  font)

