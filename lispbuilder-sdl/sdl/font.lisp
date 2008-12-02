;;;; General font definition

(in-package :lispbuilder-sdl)

(defclass font ()
  ((definition
    :reader font-definition
    :initform (error "Set :DEFINITION for FONT must no be NIL.")
    :initarg :font-definition)
   (cached-surface
    :accessor cached-surface
    :initform nil))
  (:documentation
   "The generic SDL font class. All fonts in `LISPBUILDER-SDL` inherit from
this class. Free using [FREE](#free)."))

(defmethod width ((font font))
  (width (cached-surface font)))

(defmethod height ((font font))
  (height (cached-surface font)))

(defmethod x ((font font))
  (x (cached-surface font)))
(defmethod (setf x) (x-val (font font))
  (setf (x (cached-surface font)) x-val))

(defmethod y ((font font))
  (y (cached-surface font)))
(defmethod (setf y) (y-val (font font))
  (setf (y (cached-surface font)) y-val))

(defmethod draw-font (&key (font *default-font*) (surface *default-surface*))
(check-type font font)
  (blit-surface (cached-surface font) surface))

(defmethod draw-font-at (position &key (font *default-font*) (surface *default-surface*))
  (check-type font font)
  (draw-surface-at (cached-surface font) position :surface surface))

(defmethod draw-font-at-* (x y &key (font *default-font*) (surface *default-surface*))
  (check-type font font)
  (check-type (cached-surface font) sdl-surface)
  (draw-surface-at-* (cached-surface font) x y :surface surface))

(defmethod free-cached-surface ((font font))
  (when (cached-surface font)
    (free (cached-surface font))))

(defmacro with-default-font ((font) &body body)
  "Sets `\*DEFAULT-FONT\*` to `FONT` within the scope of `WITH-DEFAULT-FONT`.

##### Example

    \(WITH-DEFAULT-FONT \(new-font\)
        \(DRAW-CHARACTER-SHADED-* \"Hello World!\" 0 0 F-COLOR B-COLOR\)\)

##### Packages

* Also supported in _LISPBUILDER-SDL-GFX_ and _LISPBUILDER-SDL-TTF_"
  `(let ((*default-font* ,font))
     ,@body))

(defmacro with-font ((font font-definition) &body body)
  "Sets `\*DEFAULT-FONT\*` to a new [BITMAP-FONT](#bitmap-font) in `FONT` within the scope of `WITH-FONT`.
Frees `FONT` when `WITH-FONT` goes out of scope, and sets `\*DEFAULT-FONT\* to `NIL.

##### Example

    \(WITH-FONT \(new-font *font-8x8*\)
        \(DRAW-CHARACTER-SHADED-* \"Hello World!\" 0 0 F-COLOR B-COLOR\)\)

##### Packages

* Also supported in _LISPBUILDER-SDL-GFX_"
  `(let ((,font (initialise-font ,font-definition)))
     (with-default-font (,font)
       ,@body)
     (free ,font)
     (when (eq *default-font* ,font)
       (setf *default-font* nil))))

(defun set-font-style (style &key (font *default-font*))
  "Sets the rendering style `STYLE` of font `FONT`. This will flush the internal cache of previously 
rendered glyphs, even if there is no change in style, so it may be best to check the
 current style using [GET-FONT-STYLE](#get-font-style) first. 

##### Parameters

* `FONT` is a `FONT` object. 
* `STYLE` is a list of one or more: `:STYLE-NORMAL`, `:STYLE-BOLD`, `:STYLE-ITALIC`, `:STYLE-UNDERLINE`. 
NOTE: Combining `:STYLE-UNDERLINE` with anything  can cause a segfault, other combinations may also do this.

##### Packages

* Supported in _LISPBUILDER-SDL-TTF_"
  (_set-font-style_ font style))

(defun get-Glyph-Metric (ch &key metric (font sdl:*default-font*))
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

    \(GET-GLYPH-METRIC UNICODE-CHAR :METRIC :MINX :FONT *DEFAULT-FONT*\)

##### Packages

* Supported in _LISPBUILDER-SDL-TTF_"
  (_get-glyph-metric_ font ch metric))

(defun get-Font-Size (text &key size (font sdl:*default-font*))
  "Calculates and returns the resulting `SIZE` of the `SDL:SURFACE` that is required to render the 
text `TEXT`, or `NIL` on error. No actual rendering is performed, however correct kerning is calculated for the 
actual width. The height returned is the same as returned using [GET-FONT-HEIGHT](#get-font-height). 

##### Parameters

* `TEXT` is the string to size, of type `STRING`. 
* `SIZE` must be one of; `:W` for the text width or `:H` for the text height.
* `FONT` is the font used to calculate the size of the `TEXT`. Binds to `\*DEFAULT-FONT\*` by default.

##### Returns

* Returns the width or height of the specified `SDL:SURFACE`, or `NIL` upon error.

##### Packages

* Supported in _LISPBUILDER-SDL-TTF_"
  (_get-font-size_ font text size))

(defun get-font-style (&key (font sdl:*default-font*))
  "Returns the rendering style of the font `FONT`. If no style is set then `:STYLE-NORMAL` is returned, 
or `NIL` upon error.
  
##### Parameters

* `FONT` is a `FONT` object. Binfs to `\*DEFAULT-FONT\*` by default. 

##### Returns

* Retuns the font style as one or more of: `:STYLE-NORMAL`, `:STYLE-BOLD`, `:STYLE-ITALIC`, `:STYLE-UNDERLINE`

##### Packages

* Supported in _LISPBUILDER-SDL-TTF_"
  (_get-font-style_ font))

(defun get-font-height (&key (font sdl:*default-font*))
  "Returns the maximum pixel height of all glyphs of font `FONT`. 
Use this height for rendering text as close together vertically as possible, 
though adding at least one pixel height to it will space it so they can't touch. 
Remember that `SDL_TTF` doesn't handle multiline printing so you are responsible 
for line spacing, see [GET-FONT-LINE-SKIP](#get-font-line-skip) as well. 

##### Parameters

* `FONT` is a `FONT` object. Binds to `\*DEFAULT-FONT\*` by default. 

##### Returns

* Retuns the height of the `FONT` as an `INTEGER`.

##### Packages

* Supported in _LISPBUILDER-SDL-TTF_"
  (_get-font-height_ font))

(defun get-font-ascent (&key (font sdl:*default-font*))
  "Returns the maximum pixel ascent of all glyphs of font `FONT`. 
This can also be interpreted as the distance from the top of the font to the baseline. 
It could be used when drawing an individual glyph relative to a top point, 
by combining it with the glyph's maxy metric to resolve the top of the rectangle used when 
blitting the glyph on the screen. 

##### Parameters

* `FONT` is a `FONT` object. Binds to `\*DEFAULT-FONT\*` by default.

##### Returns

* Returns the ascent of the `FONT` as an `INTEGER`.

##### Packages

* Supported in _LISPBUILDER-SDL-TTF_"
  (_get-font-ascent_ font))

(defun get-font-descent (&key (font sdl:*default-font*))
  "Returns the maximum pixel descent of all glyphs of font `FONT`. 
This can also be interpreted as the distance from the baseline to the bottom of the font. 
It could be used when drawing an individual glyph relative to a bottom point, 
by combining it with the glyph’s maxy metric to resolve the top of the rectangle used when 
blitting the glyph on the screen. 

##### Parameters

* `FONT` is a `FONT` object. Binds to `\*DEFAULT-FONT\*` by default.

##### Returns

* Returns the descent of the `FONT` as an `INTEGER`.

##### Packages

* Supported in _LISPBUILDER-SDL-TTF_"
  (_get-font-descent_ font))

(defun get-font-line-skip (&key (font sdl:*default-font*))
  "Returns the recommended pixel height of a rendered line of text of the font `FONT`. 
This is usually larger than the [GET-FONT-HEIGHT](#get-font-height) of the font. 

##### Parameters

* `FONT` is a `FONT` object. Binds to `\*DEFAULT-FONT\*` by default.

##### Returns

* Returns the pixel height of the `FONT` as an `INTEGER`.

##### Packages

* Supported in _LISPBUILDER-SDL-TTF_"
  (_get-font-line-skip_ font))

(defun get-font-faces (&key (font sdl:*default-font*))
  "Returns the number of faces 'sub-fonts' available in the font `FONT`. 
This is a count of the number of specific fonts (based on size and style and other
 typographical features perhaps) contained in the font itself. It seems to be a useless
 fact to know, since it can’t be applied in any other `SDL_TTF` functions.

##### Parameters

* `FONT` is a `FONT` object. Binds to `\*DEFAULT-FONT\*` by default.

##### Returns

* Returns the number of faces in the `FONT` as an `INTEGER`.

##### Packages

* Supported in _LISPBUILDER-SDL-TTF_"
  (_get-font-faces_ font))

(defun is-font-face-fixed-width (&key (font sdl:*default-font*))
  "Returns `T` if the font face is of a fixed width, or `NIL` otherwise. 
Fixed width fonts are monospace, meaning every character that exists in the font is the same width. 

##### Parameters

* `FONT` is a `FONT` object. Binds to `\*DEFAULT-FONT\*` by default.

##### Returns

* Retuns `T` if `FONT` is of fixed width, and `NIL` otherwise.

##### Packages

* Supported in _LISPBUILDER-SDL-TTF_"
  (_is-font-face-fixed-width_ font))

(defun get-font-face-family-name (&key (font sdl:*default-font*))
  "Returns the current font face family name of font `FONT` or `NIL` if the information is unavailable. 

##### Parameters

* `FONT` is a `FONT` object. Binds to `\*DEFAULT-FONT\*` by default.

##### Returns

* Returns the name of the `FONT` face family name as a `STRING`, or `NIL` if unavailable.

##### Packages

* Supported in _LISPBUILDER-SDL-TTF_"
  (_get-font-face-family-name_ font))

(defun get-font-face-style-name (&key (font sdl:*default-font*))
  "Returns the current font face style name of font `FONT`, or `NIL` if the information is unavailable. 


##### Parameters

* `FONT` is a `FONT` object. Binds to `\*DEFAULT-FONT\*` by default.

##### Returns

* Returns the name of the `FONT` face style as a `STRING`, or `NIL` if unavailable.

##### Packages

* Supported in _LISPBUILDER-SDL-TTF_"
  (_get-font-face-style-name_ font))
