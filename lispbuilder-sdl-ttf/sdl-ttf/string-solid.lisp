;; This file contains some useful functions for using SDL_ttf from Common lisp
;; 2006 (c) Rune Nesheim, see LICENCE.
;; 2007 (c) Luke Crook

(in-package #:lispbuilder-sdl-ttf)

(defun render-string-solid (text &key
			    (encoding :latin1)
			    (font *default-font*)
			    (color sdl:*default-color*)
			    (free nil))
  "Render text TEXT using font FONT with color COLOR into the FONT's cached surface, using the Solid mode. 
Unless :FREE T, the caller is responsible for freeing the new SURFACE.

  * TEXT is the text to render. TEXT may be of the encoding type LATIN1, UTF8, UNICODE, GLYPH. TEXT must match :ENCODING

  * ENCODING specifies the format of the text to render and is one of: 
    * :LATIN1
    * :UTF8
    * :UNICODE
    * :GLYPH

  * FONT is a FONT object.  Bound to *DEFAULT-FONT* by default. 

  * COLOR color is the color used to render text, of type SDL:SDL-COLOR

  * FREE when T will free the old cached SURFACE in FONT.

  * Returns the new cached surface SDL:SDL-SURFACE.

For example:
  * (DRAW-STRING-SOLID \"Hello World!\" :ENCODING :LATIN1 :FONT *DEFAULT-FONT* :COLOR A-COLOR)"
  (unless (typep font 'font)
    (error "ERROR: RENDER-STRING-SOLID; FONT must be of type FONT."))
  (unless (typep color 'sdl:sdl-color)
    (error "ERROR: RENDER-STRING-SOLID; COLOR must be of type SDL:SDL-COLOR."))
  (when (and free (sdl:cached-surface font))
    (sdl:free-surface (sdl:cached-surface font)))
  (case encoding
    (:latin1
     (sdl:with-foreign-color-copy (col-struct color)
       (setf (sdl:cached-surface font) (sdl:surface (sdl-ttf-cffi::ttf-Render-Text-Solid (fp-font font) text col-struct)))))
    (:UTF8
     (sdl:with-foreign-color-copy (col-struct color)
       (setf (sdl:cached-surface font) (sdl:surface (sdl-ttf-cffi::ttf-Render-UTF8-Solid (fp-font font) text col-struct)))))
    (:GLYPH
     (sdl:with-foreign-color-copy (col-struct color)
       (setf (sdl:cached-surface font) (sdl:surface (sdl-ttf-cffi::ttf-Render-Glyph-Solid (fp-font font) text col-struct)))))
    (:UNICODE
     ;; TODO
     )
    (otherwise
     (error "ERROR: RENDER-STRING-SOLID; ENCODING must be one of :LATIN1, :UTF8, :GLYPH or :UNICODE.")))
  (sdl:cached-surface font))

(defun draw-string-solid (text position &key 
			  (encoding :latin1)
			  (font *default-font*)
			  (surface sdl:*default-surface*)
			  (color sdl:*default-color*))
  "See DRAW-STRING-SOLID-*.

  * :POSITION is the x and y position to render the text, of type SDL:POINT."
  (unless (typep surface 'sdl:point)
    (error "ERROR: DRAW-STRING-SOLID; POSITION must be of type SDL:POINT."))
  (draw-string-solid-* text (sdl:x position) (sdl:y position)
		       :encoding encoding
		       :font font
		       :surface surface
		       :color color))

(defun draw-string-solid-* (text x y &key 
			    (encoding :latin1)
			    (font *default-font*)
			    (surface sdl:*default-surface*)
			    (color sdl:*default-color*))
  "Draw text TEXT using font FONT with color COLOR onto surface SURFACE, using the Solid mode. 
Caches the new SURFACE containing the TEXT in the FONT object. 
Note: Calls RENDER-STRING-SOLID with :FREE T so the old cached surface is automatically free'd. 

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
    (error "ERROR: DRAW-STRING-SOLID-*; SURFACE must be of type SDL:SDL-SURFACE."))
  (let ((font-surface (render-string-solid text :encoding encoding :font font :color color :free t)))
    (sdl:set-surface-* font-surface :x x :y y)
    (sdl:blit-surface font-surface surface)
    font))