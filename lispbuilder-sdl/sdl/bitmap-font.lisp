;;;; simple fixed height and width font

(in-package :lispbuilder-sdl)

(defclass bitmap-font (sdl-font foreign-object)
  ((char-pitch :reader char-pitch :initform 0 :initarg :pitch)
   (char-size :reader char-size :initform 0 :initarg :size)
   (char-width :reader char-width :initform 0 :initarg :width)
   (char-height :reader char-height :initform 0 :initarg :height)
   ;; (font-data :reader font-data :initform nil :initarg :data)
   (characters :reader characters :initform (make-hash-table :test 'equal)))
  (:default-initargs
   :gc t
    :free #'cffi:foreign-free)
  (:documentation
   "The `BITMAP-FONT` object manages the resources for a bitmap font. Resources include 
the foreign array containing font data, and the most recent cached `SURFACE` created by a call 
to any of the RENDER-STRING* functions. 
Use [DRAW-FONT](#draw-font), [DRAW-FONT-AT](#draw-font-at) or [DRAW-FONT-AT-*](#draw-font-at-*) 
to draw the cached surface. Prior to the first call to a RENDER-STRING* function, 
the cached surface is `NIL`."))

(defstruct glyph
  surface
  fg-color
  bg-color)

(defgeneric font-data (bitmap-font))
(defmethod font-data ((font bitmap-font))
  (fp font))

(defun initialise-font (font-definition)
  "Creates a new `BITMAP-FONT` object from the font data in `FONT-DEFINITION`.
Returns the new bitmap font, or `NIL` if the font cannot be created. `FONT-DEFINITION` must be one of the following
built-in fonts: 
`*FONT-10X20*, *FONT-5X7*, *FONT-5X8*, *FONT-6X10*, *FONT-6X12*, *FONT-6X13*, *FONT-6X13B*, *FONT-6X13O*, *FONT-6X9*, *FONT-7X13*, *FONT-7X13B*, *FONT-7X13O*, *FONT-7X14*, *FONT-7X14B*, *FONT-8X13*, *FONT-8X13B*, *FONT-8X13O*, *FONT-8X8*, *FONT-9X15*, *FONT-9X15B*, *FONT-9X18* OR *FONT-9X18B*`."
  (check-type font-definition font-definition)
  (let ((data (cffi:foreign-alloc :unsigned-char
				  :initial-contents (loop for i in (font-definition-data font-definition)
						       collect i)))
	(pitch (truncate (/ (+ (font-definition-width font-definition) 7)
			    8))))
    (make-instance 'bitmap-font
		   :width (font-definition-width font-definition)
		   :height (font-definition-height font-definition)
		   :pitch pitch
		   :size (* pitch (font-definition-height font-definition))
		   :fp data)))

(defun initialise-default-font (&optional (font-definition *font-8x8*))
  "Calls [INITIALISE-FONT](#initialise-font) to create a new `BITMAP-FONT` from `FONT-DEFINITION`. 
`FONT-DEFINITION` is set to `\*font-8x8\*` if unspecified. Binds the symbol `\*DEFAULT-FONT\*` to the new font.
Reutns the new font, or `NIL` if the font cannot be created."
  (check-type font-definition font-definition)
  (setf *default-font* (initialise-font font-definition)))

(defun glyph (char font)
  (gethash char (characters font)))

(defun get-character (char fg-color bg-color &key
		      (font *default-font*))
  "Returns the `SURFACE` asociated with the character `CHAR`. Creates a new surface if
`FG-COLOR` or `BG-COLOR` are different."
  (check-type char character)
  (check-type fg-color sdl-color)
  (if bg-color
      (check-type bg-color sdl-color))
  (check-type font bitmap-font)
  (let ((redraw? nil)
	(glyph (glyph char font)))
    ;; Create a surface for the character, if one does not already exist.
    (unless glyph
      (let ((g (make-glyph :surface (create-surface (char-width font) (char-height font) :surface-alpha 0 :pixel-alpha t)
			   :fg-color fg-color
			   :bg-color bg-color)))
	(setf (gethash char (characters font)) g)
	(setf glyph g)
	(setf redraw? t)))

    ;; Foreground color must match or font will be redrawn.
    (unless (color= (glyph-fg-color glyph) fg-color)
      (setf (glyph-fg-color glyph) fg-color)
      (setf redraw? t))

    ;; If bg-color must match or font will be redrawn.
    (when (and bg-color (glyph-bg-color glyph))
      (unless (color= (glyph-bg-color glyph) bg-color)
	(setf (glyph-bg-color glyph) bg-color)
	(setf redraw? t)))
    (when (or (and bg-color (not (glyph-bg-color glyph)))
	      (and (not bg-color) (glyph-bg-color glyph)))
      (setf (glyph-bg-color glyph) bg-color)
      (setf redraw? t))
    
    ;; Redraw the chracter if fg- or bg-color mismatch or
    ;; a surface was created.
    (when redraw?
      (let ((fg-col (map-color (glyph-fg-color glyph) (glyph-surface glyph)))
	    (bg-col (if bg-color
			(map-color (glyph-bg-color glyph) (glyph-surface glyph))
			0)))
	(sdl-base::with-pixel (pix (fp (glyph-surface glyph)))
	  (let ((char-pos (* (char-code char)
			     (char-size font)))
		(patt 0) (mask #x00))
	      (dotimes (iy (char-height font))
		(setf mask #x00)
		(dotimes (ix (char-width font))
		  (when (eq (setf mask (ash mask -1)) 0)
		    (setf patt (cffi:mem-aref (font-data font) :unsigned-char char-pos)
			  mask #x80)
		    (incf char-pos))
		  (if (> (logand patt mask) 0)
		      (sdl-base::write-pixel pix ix iy fg-col)
		      (sdl-base::write-pixel pix ix iy bg-col))))))))
    (glyph-surface glyph)))

(defun draw-character (c p1 fg-color bg-color &key
			     (font *default-font*)
			     (surface *default-surface*))
  "See [draw-character-shaded-*](#draw-character-shaded-*).

##### Parameters

* `P1` is the `X` and `Y` coordinates to render the text onto `SURFACE`, of type POINT."  
  (check-type p1 point)
  (draw-character-* c (x p1) (y p1) fg-color bg-color
		    :font font
		    :surface surface))

(defun draw-character-* (c x y fg-color bg-color &key
			 (font *default-font*)
			 (surface *default-surface*))
  "Draw the character `C` at location `X` and `Y` using font `FONT` with foreground and background colors `FG-COLOR` and `BG-COLOR` 
onto surface `SURFACE`. 

If `BG-COLOR` is `NIL`, then the glyph is rendered using the `SOLID` mode.
If `BG-COLOR` is NOT `NIL`, then the glyph is rendered using the `SHADED` mode.

##### Parameters

* `C` is the character to render. 
* `X` and `Y` are the X and Y position coordinates, as `INTEGERS`.
* `FG-COLOR` is the foreground color used to render text, of type `SDL-COLOR`
* `BG-COLOR` is the background color used to render text, of type `SDL-COLOR`
* `FONT` is the bitmap font used to render the character. Bound to `\*DEFAULT-FONT\*` if unspecified.
* `SURFACE` is the target surface, of type `SDL-SURFACE`. Bound to `\*DEFAULT-SURFACE\*` if unspecified.

##### Returns

* Returns the surface SURFACE.

##### For example:

    \(DRAW-CHARACTER-SHADED-* \"Hello World!\" 0 0 F-COLOR B-COLOR :SURFACE A-SURFACE\)"
  (check-type c character)
  (check-type fg-color sdl-color)
  (if bg-color
      (check-type bg-color sdl-color))
  (check-type font bitmap-font)
  (check-type surface sdl-surface)
  (draw-surface-at-* (get-character c fg-color bg-color :font font)
		     x y
		     :surface surface)
  surface)

(defun draw-string-left-justify-* (str x y fg-color bg-color &key
				   (surface *default-surface*)
				   (font *default-font*))  
  "Draw the text in the string `STR` *starting* at location `X` and `Y` using font `FONT` with foreground and background colors `FG-COLOR` and `BG-COLOR` 
onto surface `SURFACE`.

##### Parameters

* `STR` is the text to render. 
* `X` and `Y` are the X and Y position coordinates, as `INTEGERS`.
* `FG-COLOR` is the foreground color used to render text, of type `SDL-COLOR`
* `BG-COLOR` is the background color used to render text, of type `SDL-COLOR`
* `FONT` is the bitmap font used to render the character. Bound to `\*DEFAULT-FONT\*` if unspecified.
* `SURFACE` is the target surface, of type `SDL-SURFACE`. Bound to `\*DEFAULT-SURFACE\*` if unspecified.

##### Returns

* Returns the surface SURFACE."
  (loop for c across str do
       (draw-character-* c x y fg-color bg-color
			 :font font
			 :surface surface)
       (incf x (char-width font)))
  surface)

(defun draw-string-right-justify-* (str x y fg-color bg-color &key
				    (surface *default-surface*)
				    (font *default-font*))
  "Draw the text in the string `STR` *ending* at location `X` and `Y` using font `FONT` with foreground and background colors `FG-COLOR` and `BG-COLOR` 
onto surface `SURFACE`.

##### Parameters

* `STR` is the text to render. 
* `X` and `Y` are the X and Y position coordinates, as `INTEGERS`.
* `FG-COLOR` is the foreground color used to render text, of type `SDL-COLOR`
* `BG-COLOR` is the background color used to render text, of type `SDL-COLOR`
* `FONT` is the bitmap font used to render the character. Bound to `\*DEFAULT-FONT\*` if unspecified.
* `SURFACE` is the target surface, of type `SDL-SURFACE`. Bound to `\*DEFAULT-SURFACE\*` if unspecified.

##### Returns

* Returns the surface SURFACE."
  (let ((right-x (- x (char-width font)))
	(rev-str (reverse str)))
    (loop for c across rev-str do
	 (draw-character-* c right-x y fg-color bg-color
			   :font font
			   :surface surface)
	 (decf right-x (char-width font))))
  surface)

(defun draw-string-centered-* (str x y fg-color bg-color &key
			       (surface *default-surface*)
			       (font *default-font*))
  "Draw the text in the string `STR` *with midpoint centered* at location `X` and `Y` using font `FONT` with foreground and background colors `FG-COLOR` and `BG-COLOR` 
onto surface `SURFACE`.

##### Parameters

* `STR` is the text to render. 
* `X` and `Y` are the X and Y position coordinates, as `INTEGERS`.
* `FG-COLOR` is the foreground color used to render text, of type `SDL-COLOR`
* `BG-COLOR` is the background color used to render text, of type `SDL-COLOR`
* `FONT` is the bitmap font used to render the character. Bound to `\*DEFAULT-FONT\*` if unspecified.
* `SURFACE` is the target surface, of type `SDL-SURFACE`. Bound to `\*DEFAULT-SURFACE\*` if unspecified.

##### Returns

* Returns the surface SURFACE."
  (let* ((width (* (length str) (char-width font)))
	 (left-x (- x (/ width 2))))
    (loop for c across str do
	 (draw-character-* c left-x y fg-color bg-color
			   :font font
			   :surface surface)
	 (incf left-x (char-width font))))
  surface)

;; (defun load-font (file-name font-width font-height &optional (path-name ""))
;;   "Load and initialise a simple font using a bmp with a strip of fixed width characters mapped by the char-map-string"
;;   (let ((font-surface (load-image bmp-file-name bmp-path-name :key-color key-color)))
;;     (if font-surface
;; 	(make-instance 'font
;; 		       :font-surface font-surface
;; 		       :font-width font-width
;; 		       :font-height font-height
;; 		       :char-map (make-char-map char-map-string)
;; 		       :key-color key-color)
;; 	(error "LOAD-FONT: Font cannot be initialised."))))

;; (defmacro with-open-font ((font-name char-width char-height key-color &optional (font-path ""))
;; 			  &body body)
;;   `(let ((*default-font* (load-font ,font-image-name ,font-path
;; 				    ,char-width ,font-height ,char-map-string ,key-color)))
;;      (if *default-font*
;; 	 (progn
;; 	   ,@body
;; 	   (free-font *default-font*)))))

(defmacro with-default-font ((font) &body body)
  "Sets `\*DEFAULT-FONT\*` to the bitmap font in `FONT` within the scope of `WITH-DEFAULT-FONT`.

##### Example

    \(WITH-DEFAULT-FONT \(new-font\)
        \(DRAW-CHARACTER-SHADED-* \"Hello World!\" 0 0 F-COLOR B-COLOR\)\)"
  `(let ((*default-font* ,font))
     ,@body))

(defmacro with-font ((font font-definition) &body body)
  "Creates a new bitmap font `BITMAP-FONT` and sets `\*DEFAULT-FONT\*` to the bitmap font in `FONT` within the scope of `WITH-FONT`.
Frees the bitmap font when `WITH-FONT` goes out of scope.

##### Example

    \(WITH-FONT \(new-font *font-8x8*\)
        \(DRAW-CHARACTER-SHADED-* \"Hello World!\" 0 0 F-COLOR B-COLOR\)\)"
  `(let ((,font (initialise-font ,font-definition)))
     (with-default-font (,font)
       ,@body)
     (free ,font)))

