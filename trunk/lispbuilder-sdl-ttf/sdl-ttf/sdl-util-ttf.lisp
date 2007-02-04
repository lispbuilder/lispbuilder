;; This file contains some useful functions for using SDL_ttf from Common lisp
;; 2006 (c) Rune Nesheim, see LICENCE.
;; 2007 (c) Luke Crook

(in-package #:lispbuilder-sdl-ttf)


;;; Add INIT-TTF to LISPBUILDER-SDL's external initialization list.
;;; Functions in this list are called within the macro SDL:WITH-INIT, and the function SDL:INIT-SDL 
(pushnew 'init-ttf sdl:*external-init-on-startup*)

;;; Add the QUIT-TTF to LISPBUILDER-SDL's external uninitialization list.
;;; Functions in this list are called when the macro SDL:WITH-INIT exits, and the function SDL:QUIT-SDL 
(pushnew 'quit-ttf sdl:*external-quit-on-exit*)

(defun is-init ()
  "Queries the initialization status of the truetype library. 
Returns T if already initialized and NIL if uninitialized."
  (sdl-ttf-cffi::ttf-was-init))

;; (defmacro with-init (() &body body)
;;   "Initialises the truetype font library. Will exit if the libary cannot be initialised. 
;; The truetype library must be initialized prior to using functions in the LISPBUILDER-SDL-TTF package.
;; The truetype library may be initialized explicitely using this WITH-INIT macro or implicitely by adding the 
;; function INIT-TTF to sdl:*external-init-on-startup*.
;; LISPBUILDER-SDL does not have to be initialized prior initialization of the library."
;;   `(unwind-protect
;; 	(when (init-ttf)
;; 	  ,@body)
;;      (quit-ttf)))

(defmacro with-open-font ((font-name size &optional font-path) &body body)
  "This is a convenience macro that will first attempt to intialize the truetype font library and if successful, 
open the font FONT-NAME and execute BODY. Will exit if the library cannot be initialized or the FONT cannot be opened. 
Binds *DEFAULT-FONT* to the FONT in FONT-NAME. It is an ERROR if *DEFAULT-FONT* is already bound to a FONT when
WITH-OPEN-FONT is called.

Although several truetype fonts may used within a single SDL application, only a single FONT may remain open 
at any one time. For this reason WITH-OPEN-FONT calls may not be nested.

  * FONT-NAME is the name of the truetype font to be opened, of type STRING

  * SIZE is the size of the font, as an INTEGER

  * FONT-PATH is an &optional path to FONT-NAME, of type STRING"
  `(progn
     (when (typep *default-font* 'font)
       (error "WITH-OPEN-FONT; *default-font* is already bound to a FONT."))
     (when (initialise-font ,font-name ,font-path ,size)
       ,@body
       (close-font :font *default-font*)
       (setf *default-font* nil))))

;;; Functions

(defun init-ttf ()
  "Initializes the font library if uninitialized and returns T, 
or else returns NIL if uninitialized."
  (if (is-init)
      t
      (sdl-ttf-cffi::ttf-init)))

(defun quit-ttf ()
  "Uninitializes the font library if initialized. Returns NIL."
  (if (is-init)
      (sdl-ttf-cffi::ttf-quit)))

(defun initialise-font (filename pathname size)
  "Creates a new FONT object loaded from FILENAME and PATHNAME, of size SIZE.
Automatically initialises the truetype font library if uninitialised at FONT load time. 
Binds *DEFAULT-FONT* to FONT. Closes any FONT already bound to *DEFAULT-FONT* when INITIALISE-FONT is called.

  * FILENAME is the file name of the FONT, of type STRING.

  * PATHNAME is the pathname of the FONT, of type STRING.

  * SIZE is the size of the font to initialise, of type INTEGER.

  * Returns a new FONT, or NIL if unsuccessful."
  (if (is-init)
      (when (typep *default-font* 'font)
	(close-font :font *default-font*))
      (init-ttf))
  (setf *default-font* (open-font filename size pathname)))

(defun initialise-default-font ()
  "Binds *DEFAULT-FONT* to the LISPBUILDER-SDL-TTF default FONT. 
Closes any previous font that is already bound to *DEFAULT-FONT*.
Automatically initialises the truetype font library if uninitialised at FONT load time. 

  * Returns a new FONT, or NIL if unsuccessful."
  (initialise-font "Vera.ttf" *default-font-path* 32))

(defun close-font (&key font *default-font*)
  "Closes the font FONT when the font library is intitialized. 
NOTE: Does not uninitialise the font library. Does not bind *DEFAULT-FONT* to NIL. 

  * Returns T if successful, or NIL if the font cannot be closed or the font library is not initialized. "
  (unless (typep font 'font)
    (error "ERROR; CLOSE-FONT: FONT must be of type FONT."))
  (if (is-init)
      (free-font font))
  (setf font nil))

;;; g

(defun get-Glyph-Metric (ch &key metric (font *default-font*))
  "Returns the glyph metrics METRIC for the character CH, or NIL upon error. 

  * CH is a UNICODE chararacter specified as an INTEGER.

  * FONT is a FONT object from which to retrieve the glyph metrics of the character CH. Bound to *DEFAULT-FONT* by default.

  * METRIC is a KEYword argument and may be one of: 
    * :MINX , for the minimum X offset
    * :MAXX , for the maximum X offset
    * :MINY , for the minimum Y offset
    * :MAXY , for the maximum Y
    * :ADVANCE , for the advance offset

  * Returns the glyph metric as an INTEGER.

For example;
  * (GET-GLYPH-METRIC UNICODE-CHAR :METRIC :MINX :FONT *DEFAULT-FONT*)"
  (unless (typep font 'font)
    (error "ERROR; GET-GLYPH-METRIC: FONT must be of type FONT."))
  (let ((p-minx (cffi:null-pointer))
	(p-miny (cffi:null-pointer))
	(p-maxx (cffi:null-pointer))
	(p-maxy (cffi:null-pointer))
	(p-advance (cffi:null-pointer))
	(val nil)
	(r-val nil))
    (cffi:with-foreign-objects ((minx :int) (miny :int) (maxx :int) (maxy :int) (advance :int))
      (case metric
	(:minx (setf p-minx minx))
	(:miny (setf p-miny miny))
	(:maxx (setf p-maxx maxx))
	(:maxy (setf p-maxy maxy))
	(:advance (setf p-advance advance)))
      (setf r-val (sdl-ttf-cffi::ttf-Glyph-Metrics font ch p-minx p-maxx p-miny p-maxy p-advance))
      (if r-val
	  (cond
	    ((sdl:is-valid-ptr p-minx)
	     (setf val (cffi:mem-aref p-minx :int)))
	    ((sdl:is-valid-ptr miny)
	     (setf val (cffi:mem-aref p-miny :int)))
	    ((sdl:is-valid-ptr maxx)
	     (setf val (cffi:mem-aref p-maxx :int)))
	    ((sdl:is-valid-ptr maxy)
	     (setf val (cffi:mem-aref p-maxy :int)))
	    ((sdl:is-valid-ptr advance)
	     (setf val (cffi:mem-aref p-advance :int))))
	  (setf val r-val)))
    val))

(defun get-Font-Size (text &key size encoding (font *default-font*))
  "Calculates and returns the resulting SIZE of the SDL:SURFACE that is required to render the 
font FONT, or NIL on error. No actual rendering is performed however correct kerning is calculated for the 
actual width. The height returned is the same as returned using GET-FONT-HEIGHT. 

  * FONT is the font from which to calculate the size of the string. Bound to *DEFAULT-FONT* by default.

  * TEXT is the LATIN1 string to size. 

  * :SIZE may be one of: 
    * :W , text width
    * :H , text height

  * :ENCODING may be one of: 
    * :LATIN1
    * :UTF8
    * :UNICODE

    * Returns the width or height of the specified SDL:SURFACE, or NIL upon error."
  (unless (typep font 'font)
    (error "ERROR; GET-FONT-SIZE: FONT must be of type FONT."))
  (let ((p-w (cffi:null-pointer))
	(p-h (cffi:null-pointer))
	(val nil)
	(r-val nil))
    (cffi:with-foreign-objects ((w :int) (h :int))
      (case size
	(:w (setf p-w w))
	(:h (setf p-h h)))
      (case encoding
	(:LATIN1 (setf r-val (sdl-ttf-cffi::ttf-Size-Text (fp-font font) text p-w p-h)))
	(:UTF8 (setf r-val (sdl-ttf-cffi::ttf-Size-UTF8 (fp-font font) text p-w p-h))))
      (if r-val
	  (cond
	    ((sdl:is-valid-ptr p-w)
	     (setf val (cffi:mem-aref p-w :int)))
	    ((sdl:is-valid-ptr p-h)
	     (setf val (cffi:mem-aref p-h :int))))
	  (setf val r-val)))
    val))

(defun get-font-style (&key (font *default-font*))
  "Returns the rendering style of font. If no style is set then :STYLE-NORMAL is returned, 
or NIL upon error.
  
  * FONT is a FONT object. Bound to *DEFAULT-FONT* by default. 

  * Retuns the font style as one or more of:
    * :STYLE-NORMAL
    * :STYLE-NORMAL
    * :STYLE-BOLD
    * :STYLE-ITALIC
    * :STYLE-UNDERLINE"
  (unless (typep font 'font)
    (error "ERROR; GET-FONT-STYLE: FONT must be of type FONT."))
  (sdl-ttf-cffi::ttf-Get-Font-Style (fp-font font)))

(defun get-font-height (&key (font *default-font*))
  "Returns the maximum pixel height of all glyphs of font FONT. 
Use this height for rendering text as close together vertically as possible, 
though adding at least one pixel height to it will space it so they can't touch. 
Remember that SDL_ttf doesn't handle multiline printing, so you are responsible 
for line spacing, see GET-FONT-LINE-SKIP as well. 

  * FONT is a FONT object. Bound to *DEFAULT-FONT* by default. 

  * Retuns the height of the font as an INTEGER."
  (unless (typep font 'font)
    (error "ERROR; GET-FONT-HEIGHT: FONT must be of type FONT."))
  (sdl-ttf-cffi::ttf-Get-Font-height (fp-font font)))

(defun get-font-ascent (&key (font *default-font*))
  "Returns the maximum pixel ascent of all glyphs of font FONT. 
This can also be interpreted as the distance from the top of the font to the baseline. 
It could be used when drawing an individual glyph relative to a top point, 
by combining it with the glyph's maxy metric to resolve the top of the rectangle used when 
blitting the glyph on the screen. 

  * FONT is a FONT object. Bound to *DEFAULT-FONT* by default.

  * Returns the ascent of the font as an INTEGER."
  (unless (typep font 'font)
    (error "ERROR; GET-FONT-ASCENT: FONT must be of type FONT."))
  (sdl-ttf-cffi::ttf-Get-Font-Ascent (fp-font font)))

(defun get-font-descent (&key (font *default-font*))
  "Returns the maximum pixel descent of all glyphs of font FONT. 
This can also be interpreted as the distance from the baseline to the bottom of the font. 
It could be used when drawing an individual glyph relative to a bottom point, 
by combining it with the glyph’s maxy metric to resolve the top of the rectangle used when 
blitting the glyph on the screen. 

  * FONT is a FONT object. Bound to *DEFAULT-FONT* by default.

  * Returns the descent of the font as an INTEGER."
  (unless (typep font 'font)
    (error "ERROR; GET-FONT-DESCENT: FONT must be of type FONT."))
  (sdl-ttf-cffi::ttf-Get-Font-Descent (fp-font font)))

(defun get-font-line-skip (&key (font *default-font*))
  "Returns the recommended pixel height of a rendered line of text of the font FONT. 
This is usually larger than the GET-FONT-HEIGHT of the font. 

  * FONT is a FONT object. Bound to *DEFAULT-FONT* by default.

  * Returns the pixel height of the font as an INTEGER."
  (unless (typep font 'font)
    (error "ERROR; GET-FONT-LINE-SKIP: FONT must be of type FONT."))
  (sdl-ttf-cffi::ttf-Get-Font-Line-Skip (fp-font font)))

(defun get-font-faces (&key (font *default-font*))
  "Returns the number of faces 'sub-fonts' available in the font FONT. 
This is a count of the number of specific fonts (based on size and style and other
 typographical features perhaps) contained in the font itself. It seems to be a useless
 fact to know, since it can’t be applied in any other SDL_ttf functions.

  * FONT is a FONT object. Bound to *DEFAULT-FONT* by default. 

  * Returns the number of faces in the FONT as an INTEGER."
  (unless (typep font 'font)
    (error "ERROR; GET-FONT-FACES: FONT must be of type FONT."))
  (sdl-ttf-cffi::ttf-Get-Font-faces (fp-font font)))

(defun is-font-face-fixed-width (&key (font *default-font*))
  "Returns T if the font face is of a fixed width, or NIL otherwise. 
Fixed width fonts are monospace, meaning every character that exists in the font is the same width. 

  * FONT is a FONT object. Bound to *DEFAULT-FONT* by default. 

  * Retuns T FONT is of fixed width, and NIL otherwise."
  (unless (typep font 'font)
    (error "ERROR; IS-FONT-FACE-FIXED-WIDTH: FONT must be of type FONT."))
  (sdl-ttf-cffi::ttf-Get-Font-face-is-fixed-width (fp-font font)))

(defun get-font-face-family-name (&key (font *default-font*))
  "Returns the current font face family name of font FONT or NIL if the information is unavailable. 

  * FONT is a FONT object. Bound to *DEFAULT-FONT* by default. 

  * Returns the name of the font face family name as a STRING, or NIL if unavailable."
  (unless (typep font 'font)
    (error "ERROR; GET-FONT-FACE-FAMILY-NAME: FONT must be of type FONT."))
  (sdl-ttf-cffi::ttf-Get-Font-face-Family-Name (fp-font font)))

(defun get-font-face-style-name (&key (font *default-font*))
  "Returns the current font face style name of font FONT, or NIL if the information is unavailable. 

  * FONT is a FONT object. Bound to *DEFAULT-FONT* by default. 

  * Returns the name of the font face style as a STRING, or NIL if unavailable."
  (unless (typep font 'font)
    (error "ERROR; GET-FONT-FACE-STYLE-NAME: FONT must be of type FONT."))
  (sdl-ttf-cffi::ttf-Get-Font-face-Style-Name (fp-font font)))


(defun open-font (filename size &optional (pathname nil))
  "Attempts to open the truetype font at FILENAME and PATHNAME. 
NOTE: Does not bind *DEFAULT-FONT* to FONT. 
Does not attempt to initialize the truetype library if uninitialised. 

  * FILENAME is the name of the truetype font to be opened, of type STRING 

  * PATHNAME is the path to the truetype font to be opened, of type STRING 

  * SIZE is the size of the font, as an INTEGER 

  * Returns new FONT object if successful, returns NIL if unsuccessful."
  (new-font (sdl-ttf-cffi::ttf-Open-Font (namestring (if pathname
								(merge-pathnames filename pathname)
								filename))
						size)))

;;; r

(defun draw-string-solid (text position &key 
			  (encoding :latin1)
;; 			  (type :solid)
;; 			  (style :normal)
			  (font *default-font*)
			  (surface sdl:*default-surface*)
			  (color sdl:*default-color*))
  "See DRAW-STRING-SOLID-*.

  * :POSITION is the x and y position to render the text, of type SDL:POINT."
  (draw-string-solid-* text (sdl:x position) (sdl:y position)
		       :encoding encoding
;; 		       :type type
;; 		       :style style
		       :font font
		       :surface surface
		       :color color))

(defun draw-string-solid-* (text x y &key 
			    (encoding :latin1)
;; 			    (type :solid)
;; 			    (style :normal)
			    (font *default-font*)
			    (surface sdl:*default-surface*)
			    (color sdl:*default-color*))
  "Render text TEXT using font FONT with color COLOR onto surface SURFACE, using the Solid mode. 
Caches the new surface in the FONT object.

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

  * Returns the cached SDL:SDL-SURFACE.

For example:
  * (DRAW-STRING-SOLID-* \"Hello World!\" 0 0 :ENCODING :LATIN1 :FONT *DEFAULT-FONT* :SURFACE A-SURFACE :COLOR A-COLOR)"
  (unless (typep font 'font)
    (error "ERROR: draw-string-solid-*; FONT must be of type FONT."))
  (unless (typep surface 'sdl:sdl-surface)
    (error "ERROR: draw-string-solid-*; SURFACE must be of type SDL:SDL-SURFACE."))
  (unless (typep color 'sdl:sdl-color)
    (error "ERROR: draw-string-solid-*; COLOR must be of type SDL:SDL-COLOR."))  
  (case encoding
    (:latin1
     (when (cached-surface font)
       (sdl:free-surface (cached-surface font)))
     (sdl:with-foreign-color-copy (col-struct color)
       (setf (cached-surface font) (sdl:surface (sdl-ttf-cffi::ttf-Render-Text-Solid (fp-font font) text col-struct))))
     (sdl:set-surface-* (cached-surface font) :x x :y y)
     (sdl:blit-surface (cached-surface font) surface))
    (:UTF8
     (when (cached-surface font)
       (sdl:free-surface (cached-surface font)))
     (sdl:with-foreign-color-copy (col-struct color)
       (setf (cached-surface font) (sdl:surface (sdl-ttf-cffi::ttf-Render-UTF8-Solid (fp-font font) text col-struct))))
     (sdl:set-surface-* (cached-surface font) :x x :y y)
     (sdl:blit-surface (cached-surface font) surface))
    (:GLYPH
     (when (cached-surface font)
       (sdl:free-surface (cached-surface font)))
     (sdl:with-foreign-color-copy (col-struct color)
       (setf (cached-surface font) (sdl:surface (sdl-ttf-cffi::ttf-Render-Glyph-Solid (fp-font font) text col-struct))))
     (sdl:set-surface-* (cached-surface font) :x x :y y)
     (sdl:blit-surface (cached-surface font) surface))
    (:UNICODE
     ;; (defun draw-text-UNICODE-solid (surface font text color position &key update-p)
     ;;   (let ((text-surf (RenderUNICODE-Solid font text color)))
     ;;     (if text-surf
     ;; 	(sdl:blit-surface text-surf surface :dst-rect position :update-p update-p))))
     nil)))

(defun draw-string-shaded (text position fg-color bg-color &key
			     (encoding :latin1)
;; 			     (type :shaded)
;; 			     (style :normal)
			     (font *default-font*)
			     (surface sdl:*default-surface*))
  "See DRAW-STRING-SHADED-*

  * :POSITION is the x and y position to render the text, of type SDL:POINT."
  (draw-string-shaded-* text (sdl:x position) (sdl:y position) fg-color bg-color
			:encoding encoding
;; 			:type type
;; 			:style style
			:font font
			:surface surface))

(defun draw-string-shaded-* (text x y fg-color bg-color &key
			     (encoding :latin1)
;; 			     (type :shaded)
;; 			     (style :normal)
			     (font *default-font*)
			     (surface sdl:*default-surface*))
  "Render text TEXT using font :FONT with foreground color FG-COLOR and background color BG-COLOR 
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
  * (DRAW-STRING-SHADED-* \"Hello World!\" 0 0 :ENCODING :LATIN1 :FONT *DEFAULT-FONT* :SURFACE A-SURFACE :COLOR A-COLOR)"
  (unless (typep font 'font)
    (error "ERROR: draw-string-shaded-*; FONT must be of type FONT."))
  (unless (typep surface 'sdl:sdl-surface)
    (error "ERROR: draw-string-shaded-*; SURFACE must be of type SDL:SDL-SURFACE."))
  (unless (typep fg-color 'sdl:sdl-color)
    (error "ERROR: draw-string-shaded-*; FG-COLOR must be of type SDL:SDL-COLOR."))
  (unless (typep bg-color 'sdl:sdl-color)
    (error "ERROR: draw-string-shaded-*; BG-COLOR must be of type SDL:SDL-COLOR."))  
  (case encoding
    (:latin1
     (when (cached-surface font)
       (sdl:free-surface (cached-surface font)))
     (sdl:with-foreign-color-copy (fg-struct fg-color)
       (sdl:with-foreign-color-copy (bg-struct bg-color)
	 (setf (cached-surface font) (sdl:surface (sdl-ttf-cffi::ttf-Render-Text-shaded (fp-font font)
											text fg-struct bg-struct)))))
     (sdl:set-surface-* (cached-surface font) :x x :y y)
     (sdl:blit-surface (cached-surface font) surface))
    (:UTF8
     (when (cached-surface font)
       (sdl:free-surface (cached-surface font)))
     (sdl:with-foreign-color-copy (fg-struct fg-color)
       (sdl:with-foreign-color-copy (bg-struct bg-color)
	 (setf (cached-surface font) (sdl:surface (sdl-ttf-cffi::ttf-Render-UTF8-shaded (fp-font font)
											text fg-struct bg-struct)))))
     (sdl:set-surface-* (cached-surface font) :x x :y y)
     (sdl:blit-surface (cached-surface font) surface))
    (:GLYPH
     (when (cached-surface font)
       (sdl:free-surface (cached-surface font)))
     (sdl:with-foreign-color-copy (fg-struct fg-color)
       (sdl:with-foreign-color-copy (bg-struct bg-color)
	 (setf (cached-surface font) (sdl:surface (sdl-ttf-cffi::ttf-Render-Glyph-shaded (fp-font font)
											 text fg-struct bg-struct)))))
     (sdl:set-surface-* (cached-surface font) :x x :y y)
     (sdl:blit-surface (cached-surface font) surface))
    (:UNICODE
     ;; (defun draw-text-UNICODE-solid (surface font text color position &key update-p)
     ;;   (let ((text-surf (RenderUNICODE-Solid font text color)))
     ;;     (if text-surf
     ;; 	(sdl:blit-surface text-surf surface :dst-rect position :free-src t :update-p update-p))))
     nil)))

(defun draw-string-blended (text position &key
			    (encoding :latin1)
;; 			    (type :blended)
;; 			    (style :normal)
			    (font *default-font*)
			    (surface sdl:*default-surface*)
			    (color sdl:*default-color*))
  "See DRAW-STRING-BLENDED-*.
:POSITION is the x and y position to render the text, of type SDL:POINT."
  (draw-string-blended-* text (sdl:x position) (sdl:y position)
			 :encoding encoding
;; 			 :type type
;; 			 :style style
			 :font font
			 :surface surface
			 :color color))

(defun draw-string-blended-* (text x y &key
			      (encoding :latin1)
;; 			      (type :blended)
;; 			      (style :normal)
			      (font *default-font*)
			      (surface sdl:*default-surface*)
			      (color sdl:*default-color*))
  "Render text TEXT using font :FONT with color :COLOR onto surface :SURFACE, using the Blended mode. 
Caches the new surface in the FONT object. 

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

  * Returns the cached SDL:SDL-SURFACE.

For example:
  * (DRAW-STRING-SOLID-* \"Hello World!\" 0 0 :ENCODING :LATIN1 :FONT *DEFAULT-FONT* :SURFACE A-SURFACE :COLOR A-COLOR)"
  (unless (typep font 'font)
    (error "ERROR: draw-string-blended-*; FONT must be of type FONT."))
  (unless (typep surface 'sdl:sdl-surface)
    (error "ERROR: draw-string-blended-*; SURFACE must be of type SDL:SDL-SURFACE."))
  (unless (typep color 'sdl:sdl-color)
    (error "ERROR: draw-string-blended-*; COLOR must be of type SDL:SDL-COLOR."))  
  (case encoding
    (:latin1
     (when (cached-surface font)
       (sdl:free-surface (cached-surface font)))
     (sdl:with-foreign-color-copy (col-struct color)
       (setf (cached-surface font) (sdl:surface (sdl-ttf-cffi::ttf-Render-Text-blended (fp-font font) text col-struct))))
     (sdl:set-surface-* (cached-surface font) :x x :y y)
     (sdl:blit-surface (cached-surface font) surface))
    (:UTF8
     (when (cached-surface font)
       (sdl:free-surface (cached-surface font)))
     (sdl:with-foreign-color-copy (col-struct color)
       (setf (cached-surface font) (sdl:surface (sdl-ttf-cffi::ttf-Render-UTF8-blended (fp-font font) text col-struct))))
     (sdl:set-surface-* (cached-surface font) :x x :y y)
     (sdl:blit-surface (cached-surface font) surface))
    (:GLYPH
     (when (cached-surface font)
       (sdl:free-surface (cached-surface font)))
     (sdl:with-foreign-color-copy (col-struct color)
       (setf (cached-surface font) (sdl:surface (sdl-ttf-cffi::ttf-Render-Glyph-blended (fp-font font) text col-struct))))
     (sdl:set-surface-* (cached-surface font) :x x :y y)
     (sdl:blit-surface (cached-surface font) surface))
    (:UNICODE
     ;; (defun draw-text-UNICODE-solid (surface font text color position &key update-p)
     ;;   (let ((text-surf (RenderUNICODE-Solid font text color)))
     ;;     (if text-surf
     ;; 	(sdl:blit-surface text-surf surface :dst-rect position  :update-p update-p))))
     nil)))

(defun draw-font (font &key (surface sdl:*default-surface*))
  "Blit the cached SURFACE in FONT to the destination surface SURFACE. 
The cached surface is created during a previous call to any of the DRAW-STRING* functions. 
Uses POSITION in the cached SURFACE to render to the X/Y coordinates on the destination SURFACE. 

This function can speed up blitting when the text remains unchanged between screen updates."
  (sdl:blit-surface (cached-surface font) surface))

(defun draw-font-at (font point &key (surface sdl:*default-surface*))
  "See DRAW-FONT. 
POINT is used to position the cached SURFACE, where POINT is of type SDL:POINT."
  (sdl:draw-surface-at (cached-surface font) point :surface surface))

(defun draw-font-at-* (font x y &key (surface sdl:*default-surface*))
    "See DRAW-FONT. 
X and Y are used to position the cached SURFACE, where X and Y are INTEGERS."
  (sdl:draw-surface-at-* (cached-surface font) x y :surface surface))

;;; s

(defun set-font-style (style &key (font *default-font*))
  "Sets the rendering style STYLE of font FONT. This will flush the internal cache of previously 
rendered glyphs, even if there is no change in style, so it may be best to check the
 current style using GET-FONT-STYLE first. 

NOTE: Combining :STYLE-UNDERLINE with anything can cause a segfault, other combinations 
may also do this. 

FONT is a FONT object. 

STYLE is a list of one or more: 
  :STYLE-NORMAL
  :STYLE-BOLD
  :STYLE-ITALIC
  :STYLE-UNDERLINE"
  (sdl-ttf-cffi::ttf-Set-Font-Style (fp-font font) style))

