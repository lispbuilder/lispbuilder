;;;; simple fixed height and width font

(in-package :lispbuilder-sdl)

(defclass bitmap-font (sdl-font)
  ((char-pitch :reader char-pitch :initform 0 :initarg :pitch)
   (char-size :reader char-size :initform 0 :initarg :size)
   (char-width :reader char-width :initform 0 :initarg :width)
   (char-height :reader char-height :initform 0 :initarg :height)
   (font-data :reader font-data :initform nil :initarg :data)
   (characters :reader characters :initform (make-hash-table :test 'equal)))
  (:documentation
   "The FONT object maintains the resources for a bitmap font. This includes the foreign array containing the FONT data, 
as well as the most recent surface SURFACE created by a call to any of the RENDER-STRING* functions. 
Use DRAW-FONT, DRAW-FONT-AT or DRAW-FONT-AT-* to draw the cached surface.
Prior to the first call to a RENDER-STRING* function, the cached surface is NIL."))

(defstruct glyph
  surface
  fg-color
  bg-color)

(defun initialise-font (font-definition)
  "Creates a new FONT object from the font data in FONT-DATA.
Binds the symbol *DEFAULT-FONT* to FONT.
  * Returns a new FONT, or NIL if unsuccessful."
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
		   :data data)))

(defun initialise-default-font (&optional (font-definition *font-8x8*))
  "Creates a new FONT object from the font data in SDL-GFX-CFFI:*FONT-DATA*.
Binds the symbol *DEFAULT-FONT* to FONT
  * Returns a new FONT, or NIL if unsuccessful."
  (check-type font-definition font-definition)
  (setf *default-font* (initialise-font font-definition)))

(defmethod free-font ((font bitmap-font))
  "Free resources associated with the font FONT."
  (tg:cancel-finalization font)
  (when (cached-surface font)
    (free-cached-surface font))
  (cffi:foreign-free (font-data font))
;;   #-clisp(maphash #'(lambda (key val)
;; 		      (declare (ignore key))
;; 		      (when val
;; 			(free-surface (glyph-surface val))))
;; 		  (characters font))
  (clrhash (characters font))
  font)

(defun glyph (char font)
  (gethash char (characters font)))

(defun get-character (char fg-color bg-color &key
		      (font *default-font*))
  (check-type char character)
  (check-type fg-color sdl-color)
  (if bg-color
      (check-type bg-color sdl-color))
  (check-type font bitmap-font)
  (let ((redraw? nil)
	(glyph (glyph char font)))
    ;; Create a surface for the character, if one does not already exist.
    (unless glyph
      (let ((g (make-glyph :surface (create-surface (char-width font) (char-height font))
			   :fg-color fg-color
			   :bg-color bg-color)))
	(setf (gethash char (characters font)) g)
	(setf glyph g)
	(setf redraw? t)))

    ;; Foreground color must match or font will be redrawn.
    (unless (color= (glyph-fg-color glyph) fg-color)
      (setf (glyph-fg-color glyph) fg-color)
      (setf redraw? t))

    ;; If bg-color must match or font will br redrawn.
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
  "See DRAW-CHARACTER-SHADED-*.

  * P1 is the x and y position to render the text, of type POINT."  
  (check-type p1 point)
  (draw-character-* c (x p1) (y p1) fg-color bg-color
		    :font font
		    :surface surface))

(defun draw-character-* (c x y fg-color bg-color &key
			 (font *default-font*)
			 (surface *default-surface*))
  "Draw character C using font FONT with foreground and background colors FG-COLOR and BG-COLOR 
onto surface SURFACE. 
If BG-COLOR is NIL, then the glyph is rendered using the SOLID mode.
If BG-COLOR is NOT NIL, then the glyph is rendered using the SHADED mode.

  * C is the character to render. 

  * X/Y are the x and y position coordinates, as INTEGERS.

  * FG-COLOR color is the foreground color used to render text, of type SDL-COLOR

  * BG-COLOR color is the background color used to render text, of type SDL-COLOR

  * FONT is a FONT object.  Bound to *DEFAULT-FONT* by default. 

  * SURFACE is the surface to render text onto, of type SDL-SURFACE 

  * Returns the surface SURFACE.

For example:
  * (DRAW-CHARACTER-SHADED-* \"Hello World!\" 0 0 F-COLOR B-COLOR :SURFACE A-SURFACE)"
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
  "draw a string starting at the x y position"
  (loop for c across str do
       (draw-character-* c x y fg-color bg-color
			 :font font
			 :surface surface)
       (incf x (char-width font)))
  surface)

(defun draw-string-right-justify-* (str x y fg-color bg-color &key
				    (surface *default-surface*)
				    (font *default-font*))
  "draw a string ending at the x y position"
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
  "draw a string centered at x y"
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
  `(let ((*default-font* ,font))
     ,@body))

(defmacro with-font ((font font-definition) &body body)
  `(let* ((,font (initialise-font ,font-definition))
	  (*default-font* ,font))
     ,@body
     (free-font ,font)))

