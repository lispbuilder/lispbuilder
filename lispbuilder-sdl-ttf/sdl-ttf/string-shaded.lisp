;; This file contains some useful functions for using SDL_ttf from Common lisp
;; 2006 (c) Rune Nesheim, see LICENCE.
;; 2007 (c) Luke Crook

(in-package #:lispbuilder-sdl-ttf)

(defun render-string-shaded (text fg-color bg-color &key
			     (encoding :latin1)
			     (font *default-font*))
  "Render text TEXT using font FONT with color COLOR into the FONT's cached surface, using the Shaded mode. 

  * TEXT is the text to render. TEXT may be of the encoding type LATIN1, UTF8, UNICODE, GLYPH. TEXT must match :ENCODING

  * FG-COLOR is the foreground color of the text, of type SDL:SDL-COLOR

  * BG-COLOR is the background color of the text, of type SDL:SDL-COLOR

  * ENCODING specifies the format of the text to render and is one of: 
    * :LATIN1
    * :UTF8
    * :UNICODE
    * :GLYPH

  * FONT is a FONT object.  Bound to *DEFAULT-FONT* by default. 

  * Returns the cached SDL:SDL-SURFACE.

For example:
  * (DRAW-STRING-SHADED \"Hello World!\" fg-col bg-col :ENCODING :LATIN1 :FONT *DEFAULT-FONT* :COLOR A-COLOR)"
  (unless (typep font 'font)
    (error "ERROR: RENDER-STRING-SHADED; FONT must be of type FONT."))
  (unless (typep fg-color 'sdl:sdl-color)
    (error "ERROR: RENDER-STRING-SHADED; FG-COLOR must be of type SDL:SDL-COLOR."))
  (unless (typep bg-color 'sdl:sdl-color)
    (error "ERROR: RENDER-STRING-SHADED; BG-COLOR must be of type SDL:SDL-COLOR."))
  (when (cached-surface font)
    (sdl:free-surface (cached-surface font)))
  (case encoding
    (:latin1
     (sdl:with-foreign-color-copy (fg-struct fg-color)
       (sdl:with-foreign-color-copy (bg-struct bg-color)
	 (setf (cached-surface font) (sdl:surface (sdl-ttf-cffi::ttf-Render-Text-shaded (fp-font font)
											text fg-struct bg-struct))))))
    (:UTF8
     (sdl:with-foreign-color-copy (fg-struct fg-color)
       (sdl:with-foreign-color-copy (bg-struct bg-color)
	 (setf (cached-surface font) (sdl:surface (sdl-ttf-cffi::ttf-Render-UTF8-shaded (fp-font font)
											text fg-struct bg-struct))))))
    (:GLYPH
     (sdl:with-foreign-color-copy (fg-struct fg-color)
       (sdl:with-foreign-color-copy (bg-struct bg-color)
	 (setf (cached-surface font) (sdl:surface (sdl-ttf-cffi::ttf-Render-Glyph-shaded (fp-font font)
											 text fg-struct bg-struct))))))
    (:UNICODE
     ;; TODO
     )
    (otherwise
     (error "ERROR: RENDER-STRING-SHADED; ENCODING must be one of :LATIN1, :UTF8, :GLYPH or :UNICODE.")))
  (cached-surface font))

(defun draw-string-shaded (text position fg-color bg-color &key
			     (encoding :latin1)
			     (font *default-font*)
			     (surface sdl:*default-surface*))
  "See DRAW-STRING-SHADED-*

  * :POSITION is the x and y position to render the text, of type SDL:POINT."
  (unless (typep position 'sdl:point)
    (error "ERROR: DRAW-STRING-SHADED; POSITION must be of type SDL:POINT."))
  (draw-string-shaded-* text (sdl:x position) (sdl:y position) fg-color bg-color
			:encoding encoding
			:font font
			:surface surface))

(defun draw-string-shaded-* (text x y fg-color bg-color &key
			     (encoding :latin1)
			     (font *default-font*)
			     (surface sdl:*default-surface*))
  "Draw text TEXT using font :FONT with foreground color FG-COLOR and background color BG-COLOR 
onto surface :SURFACE, using the Shaded mode. Caches the new surface in the FONT object.

  * TEXT is the text to render. TEXT may be of the encoding type LATIN1, UTF8, UNICODE, GLYPH. TEXT must match :ENCODING

  * X/Y are the x and y position coordinates, as INTEGERS.

  * FG-COLOR is the foreground color of the text, of type SDL:SDL-COLOR

  * BG-COLOR is the background color of the text, of type SDL:SDL-COLOR

  * ENCODING specifies the format of the text to render and is one of: 
  *  :LATIN1
  *  :UTF8
  *  :UNICODE
  *  :GLYPH

  * FONT is a FONT object.  Bound to *DEFAULT-FONT* by default. 

  * SURFACE is the surface to render text onto, of type SDL:SDL-SURFACE 

For example:
  * (DRAW-STRING-SHADED-* \"Hello World!\" 0 0 fg-col bg-col :ENCODING :LATIN1 :FONT *DEFAULT-FONT* :SURFACE A-SURFACE)"
  (unless (typep surface 'sdl:sdl-surface)
    (error "ERROR: DRAW-STRING-SHADED-*; SURFACE must be of type SDL:SDL-SURFACE."))
  (let ((font-surface (render-string-shaded text fg-color bg-color :encoding encoding :font font)))
    (sdl:set-surface-* font-surface :x x :y y)
    (sdl:blit-surface font-surface surface)
    font-surface))
