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
  "Queries the initialization status of the `SDL_TTF` truetype library. 
Returns `T` if this library is already initialized and `NIL` if uninitialized."
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

(defmacro with-open-font ((font-name size) &body body)
  "This is a convenience macro that will first attempt to initialize the truetype font library and if successful, 
will then open the font in the file `FONT-NAME` prior to evaluating the forms in `BODY`. 
Will exit if the library cannot be initialized or the `FONT` cannot be opened.  Closes `FONT` after the forms in 
`BODY` have evaluated. 

Binds `FONT` to a shadowed instance of `\*DEFAULT-FONT\*` valid within the scope of `WITH-OPEN-FONT`. 
`WITH-OPEN-FONT` calls may be nested.

##### Parameters

* `FONT-NAME` is the file name of the truetype font to be opened, of type `STRING`.
* `SIZE` is the `INTEGER` point size of the font."
  (let ((font (gensym "font-")))
    `(let* ((,font (initialise-font ,font-name ,size))
	    (*default-font* ,font))
       (when ,font
	 ,@body
	 (close-font :font ,font)))))

(defmacro with-default-font ((font) &body body)
  "Binds `FONT` to `\*DEFAULT-FONT\*` within the scope of `WITH-DEFAULT-FONT`."
  `(let ((*default-font* ,font))
     ,@body))

;;; Functions

(defun init-ttf ()
  "Initializes the `SDL_TTF` font library if uninitialized. Returns `T` if the library was uninitialized and 
is successfully initialized, or else returns `NIL` if uninitialized."
  (if (is-init)
      t
      (sdl-ttf-cffi::ttf-init)))

(defun quit-ttf ()
  "Uninitializes the `SDL_TTF` font library if already initialized."
  (if (is-init)
      (sdl-ttf-cffi::ttf-quit)))

(defun initialise-font (filename size)
  "Creates a new `FONT` object of size `SIZE` loaded from the file at location `FILENAME`.
Automatically initialises the `SDL_TTF` truetype font library if the library is uninitialised when `FONT` is loaded. 

##### Parameters

* `FILENAME` is the file name of the `FONT`, of type `STRING`.
* `SIZE` is the `INTEGER` point size of the new `FONT`.

##### Returns

* Returns a new `FONT`, or `NIL` if unsuccessful."
  (unless (is-init)
    (init-ttf))
  (open-font filename size))

(defun initialise-default-font (&optional (free t) (filename (create-path "Vera.ttf")) (size 32))
  "Creates a new `FONT` object that is loaded from the file at location `FILENAME`, and binds this to the symbol 
`\*DEFAULT-FONT\*`. Although several truetype fonts may used within a single SDL application, only a single 
`FONT` may be bound to the symbol `*\DEFAULT-FONT\*` at any one time. Generates an `ERROR` if `FREE` is `NIL` 
and `\*DEFAULT-FONT\*` is already bound to a `FONT` when `INITIALISE-DEFAULT-FONT` is called.

##### Parameters

* `FREE` when set to `T` will automatically free any font that is already bound to `*\DEFAULT-FONT\*`.
* `FILENAME` is the optional file name of the font, as a STRING. Default value is `Vera.ttf`.
* `SIZE` is the `INTEGER` point size of the new `FONT`.

##### Returns

* Returns a new `FONT`, or `NIL` if unsuccessful."
  (when *default-font*
    (if free
	(close-font :font *default-font*)
	(error "INITIALISE-DEFAULT-FONT; *default-font* is already bound to a FONT.")))
  (setf *default-font* (initialise-font filename size)))

(defun close-font (&key font *default-font*)
  "Closes the font `FONT` when the `SDL_TTF` font library is intitialized. 
NOTE: `CLOSE-FONT` does not uninitialise the font library, and does not bind `\*DEFAULT-FONT\*` to `NIL`. Returns `T` 
if successful, or `NIL` if the font cannot be closed or the `SDL_TTF` font library is not initialized."
  (check-type font font)
  (if (is-init)
      (free-font font))
  (setf font nil))

;;; g

(defun get-Glyph-Metric (ch &key metric (font *default-font*))
  "Returns the glyph metrics `METRIC` for the character `CH`, or `NIL` upon error. 

##### Parameters

* `CH` is a UNICODE chararacter specified as an `INTEGER`.
* `FONT` is a `FONT` object from which to retrieve the glyph metrics of the character `CH`. Binds to `*\DEFAULT-FONT\*` 
by default.
* `METRIC` is a `KEY`word argument and may be one of; 
`:MINX`, for the minimum `X` offset.  
`:MAXX`, for the maximum `X` offset. 
`:MINY`, for the minimum `Y` offset. 
`:MAXY`, for the maximum `Y` offset. 
`:ADVANCE`, for the advance offset. 

##### Returns

* Returns the glyph metric as an `INTEGER`.

##### Example

    \(GET-GLYPH-METRIC UNICODE-CHAR :METRIC :MINX :FONT *DEFAULT-FONT*\)"
  (check-type font font)
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

(defun get-Font-Size (text &key size (font *default-font*))
  "Calculates and returns the resulting `SIZE` of the `SDL:SURFACE` that is required to render the 
text `TEXT`, or `NIL` on error. No actual rendering is performed, however correct kerning is calculated for the 
actual width. The height returned is the same as returned using [GET-FONT-HEIGHT](#get-font-height). 

##### Parameters

* `TEXT` is the string to size, of type `STRING`. 
* `SIZE` must be one of; `:W` for the text width or `:H` for the text height.
* `FONT` is the font used to calculate the size of the `TEXT`. Binds to `\*DEFAULT-FONT\*` by default.

##### Returns

* Returns the width or height of the specified `SDL:SURFACE`, or `NIL` upon error."
  (check-type font font)
  (let ((p-w (cffi:null-pointer))
	(p-h (cffi:null-pointer))
	(val nil)
	(r-val nil))
    (cffi:with-foreign-objects ((w :int) (h :int))
      (case size
	(:w (setf p-w w))
	(:h (setf p-h h)))
      (setf r-val (sdl-ttf-cffi::ttf-Size-UTF8 (fp-font font) text p-w p-h))
      (if r-val
	  (cond
	    ((sdl:is-valid-ptr p-w)
	     (setf val (cffi:mem-aref p-w :int)))
	    ((sdl:is-valid-ptr p-h)
	     (setf val (cffi:mem-aref p-h :int))))
	  (setf val r-val)))
    val))

(defun get-font-style (&key (font *default-font*))
  "Returns the rendering style of the font `FONT`. If no style is set then `:STYLE-NORMAL` is returned, 
or `NIL` upon error.
  
##### Parameters

* `FONT` is a `FONT` object. Binfs to `\*DEFAULT-FONT\*` by default. 

##### Returns

* Retuns the font style as one or more of: `:STYLE-NORMAL`, `:STYLE-BOLD`, `:STYLE-ITALIC`, `:STYLE-UNDERLINE`"
  (check-type font font)
  (sdl-ttf-cffi::ttf-Get-Font-Style (fp-font font)))

(defun get-font-height (&key (font *default-font*))
  "Returns the maximum pixel height of all glyphs of font `FONT`. 
Use this height for rendering text as close together vertically as possible, 
though adding at least one pixel height to it will space it so they can't touch. 
Remember that `SDL_TTF` doesn't handle multiline printing so you are responsible 
for line spacing, see [GET-FONT-LINE-SKIP](#get-font-line-skip) as well. 

##### Parameters

* `FONT` is a `FONT` object. Binds to `\*DEFAULT-FONT\*` by default. 

##### Returns

* Retuns the height of the `FONT` as an `INTEGER`."
  (check-type font font)
  (sdl-ttf-cffi::ttf-Get-Font-height (fp-font font)))

(defun get-font-ascent (&key (font *default-font*))
  "Returns the maximum pixel ascent of all glyphs of font `FONT`. 
This can also be interpreted as the distance from the top of the font to the baseline. 
It could be used when drawing an individual glyph relative to a top point, 
by combining it with the glyph's maxy metric to resolve the top of the rectangle used when 
blitting the glyph on the screen. 

##### Parameters

* `FONT` is a `FONT` object. Binds to `\*DEFAULT-FONT\*` by default.

##### Returns

* Returns the ascent of the `FONT` as an `INTEGER`."
  (check-type font font)
  (sdl-ttf-cffi::ttf-Get-Font-Ascent (fp-font font)))

(defun get-font-descent (&key (font *default-font*))
  "Returns the maximum pixel descent of all glyphs of font `FONT`. 
This can also be interpreted as the distance from the baseline to the bottom of the font. 
It could be used when drawing an individual glyph relative to a bottom point, 
by combining it with the glyph’s maxy metric to resolve the top of the rectangle used when 
blitting the glyph on the screen. 

##### Parameters

* `FONT` is a `FONT` object. Binds to `\*DEFAULT-FONT\*` by default.

##### Returns

* Returns the descent of the `FONT` as an `INTEGER`."
  (check-type font font)
  (sdl-ttf-cffi::ttf-Get-Font-Descent (fp-font font)))

(defun get-font-line-skip (&key (font *default-font*))
  "Returns the recommended pixel height of a rendered line of text of the font `FONT`. 
This is usually larger than the [GET-FONT-HEIGHT](#get-font-height) of the font. 

##### Parameters

* `FONT` is a `FONT` object. Binds to `\*DEFAULT-FONT\*` by default.

##### Returns

* Returns the pixel height of the `FONT` as an `INTEGER`."
  (check-type font font)
  (sdl-ttf-cffi::ttf-Get-Font-Line-Skip (fp-font font)))

(defun get-font-faces (&key (font *default-font*))
  "Returns the number of faces 'sub-fonts' available in the font `FONT`. 
This is a count of the number of specific fonts (based on size and style and other
 typographical features perhaps) contained in the font itself. It seems to be a useless
 fact to know, since it can’t be applied in any other `SDL_TTF` functions.

##### Parameters

* `FONT` is a `FONT` object. Binds to `\*DEFAULT-FONT\*` by default.

##### Returns

* Returns the number of faces in the `FONT` as an `INTEGER`."
  (check-type font font)
  (sdl-ttf-cffi::ttf-Get-Font-faces (fp-font font)))

(defun is-font-face-fixed-width (&key (font *default-font*))
  "Returns `T` if the font face is of a fixed width, or `NIL` otherwise. 
Fixed width fonts are monospace, meaning every character that exists in the font is the same width. 

##### Parameters

* `FONT` is a `FONT` object. Binds to `\*DEFAULT-FONT\*` by default.

##### Returns

* Retuns `T` if `FONT` is of fixed width, and `NIL` otherwise."
  (check-type font font)
  (sdl-ttf-cffi::ttf-Get-Font-face-is-fixed-width (fp-font font)))

(defun get-font-face-family-name (&key (font *default-font*))
  "Returns the current font face family name of font `FONT` or `NIL` if the information is unavailable. 

##### Parameters

* `FONT` is a `FONT` object. Binds to `\*DEFAULT-FONT\*` by default.

##### Returns

* Returns the name of the `FONT` face family name as a `STRING`, or `NIL` if unavailable."
  (check-type font font)
  (sdl-ttf-cffi::ttf-Get-Font-face-Family-Name (fp-font font)))

(defun get-font-face-style-name (&key (font *default-font*))
  "Returns the current font face style name of font `FONT`, or `NIL` if the information is unavailable. 


##### Parameters

* `FONT` is a `FONT` object. Binds to `\*DEFAULT-FONT\*` by default.

##### Returns

* Returns the name of the `FONT` face style as a `STRING`, or `NIL` if unavailable."
  (check-type font font)
  (sdl-ttf-cffi::ttf-Get-Font-face-Style-Name (fp-font font)))


(defun open-font (filename size)
  "Creates a new `FONT` from the file loaded at the location specified by `FILENAME`. 
NOTE: Does not bind the new `FONT` to `\*DEFAULT-FONT\*`. Does not attempt to initialize the `SDL_TTF` 
truetype library if the library is uninitialised. 

##### Parameters

* `FILENAME` is the name of the truetype font to be loaded, of type `STRING` 
* `SIZE` is the size of the `FONT`, as an `INTEGER`. 

##### Returns

* Returns new `FONT` object if successful, returns `NIL` if unsuccessful."
  (new-font (sdl-ttf-cffi::ttf-Open-Font (namestring filename) size)))

;;; s

(defun set-font-style (style &key (font *default-font*))
  "Sets the rendering style `STYLE` of font `FONT`. This will flush the internal cache of previously 
rendered glyphs, even if there is no change in style, so it may be best to check the
 current style using [GET-FONT-STYLE](#get-font-style) first. 

##### Parameters

* `FONT` is a `FONT` object. 
* `STYLE` is a list of one or more: `:STYLE-NORMAL`, `:STYLE-BOLD`, `:STYLE-ITALIC`, `:STYLE-UNDERLINE`. 
NOTE: Combining `:STYLE-UNDERLINE` with anything  can cause a segfault, other combinations may also do this."
  (check-type font font)
  (sdl-ttf-cffi::ttf-Set-Font-Style (fp-font font) style))

(defun create-path (filename)
  (namestring (merge-pathnames filename *default-font-path*)))
