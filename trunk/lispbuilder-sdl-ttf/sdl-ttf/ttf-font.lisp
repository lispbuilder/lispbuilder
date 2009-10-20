
(in-package #:lispbuilder-sdl)

(export 'ttf-font :lispbuilder-sdl)

(defclass ttf-font (font foreign-object)
  ((font-style
    :accessor font-style
    :initform nil
    :initarg :style)
   (font-encoding
    :accessor font-encoding
    :initform nil
    :initarg :encoding)
   (generation
    :accessor generation
    :initform sdl-ttf:*generation*
    :initarg :generation))
  (:default-initargs
   :gc t
   :free (let ((font-generation sdl-ttf:*generation*))
           #'(lambda (fp)
               (when (and (sdl-ttf:is-init)
                          (= (sdl-ttf:is-init) font-generation))
                 (sdl-ttf-cffi::ttf-close-font fp)))))
  (:documentation
   "A `FONT` object is wrapper around a foreign `TTF_Font` object. 

The `FONT` object maintains the most recent surface `SDL:SURFACE` created by a call to any of the 
`RENDER-STRING*` functions. Use [DRAW-FONT](#draw-font), [DRAW-FONT-AT](#draw-font-at) 
or [DRAW-FONT-AT-*](#draw-font-at-*) to draw the cached surface.

Prior to the first call to a `RENDER-STRING*` function, the cached surface is `NIL`."))

(defmethod open-font ((definition ttf-font-definition))
  (let ((font (sdl-ttf-cffi::ttf-Open-Font (namestring (sdl::filename definition))
                                           (sdl::char-size definition))))
    (unless (cffi:null-pointer-p font)
      (make-instance 'ttf-font
                     :fp font
                     :font-definition definition))))

(defmethod close-font ((font ttf-font))
  "Closes the font `FONT` when the `SDL_TTF` font library is intitialized. 
NOTE: `CLOSE-FONT` does not uninitialise the font library, and does not bind `\*DEFAULT-FONT\*` to `NIL`. Returns `T` 
if successful, or `NIL` if the font cannot be closed or the `SDL_TTF` font library is not initialized."
  (sdl:free font))

(defmethod set-default-font ((font ttf-font))
  (setf sdl:*default-font* font)
  font)

(defmethod initialise-font ((self ttf-font-definition))
  (unless (sdl-ttf:is-init)
    (sdl-ttf:init-ttf))
  (open-font self))

(defmethod _get-Glyph-Metric_ ((font ttf-font) (ch character) metric)
  (let ((val nil)
	(r-val nil))
    (cffi:with-foreign-objects ((minx :int) (miny :int) (maxx :int) (maxy :int) (advance :int))
      (setf r-val (sdl-ttf-cffi::ttf-Glyph-Metrics (sdl:fp font) (char-code ch) minx maxx miny maxy advance))
      (when r-val
	(setf val
	      (case metric
		(:minx (setf val (cffi:mem-aref minx :int)))
		(:miny (setf val (cffi:mem-aref miny :int)))
		(:maxx (setf val (cffi:mem-aref maxx :int)))
		(:maxy (setf val (cffi:mem-aref maxy :int)))
		(:advance (setf val (cffi:mem-aref advance :int)))
		(t (error "ERROR: GET-GLYPH-METRIC; :METRIC must be one of :MINX, :MINY, :MAXX, :MAXY or :ADVANCE"))))))
    val))

(defmethod _get-Font-Size_ ((font ttf-font) text size)
  (let ((val nil)
        (r-val nil))
    (cffi:with-foreign-objects ((w :int) (h :int))
      (setf r-val (sdl-ttf-cffi::ttf-Size-UTF8 (sdl:fp font) text w h))
      (setf val
	    (case size
	      (:w (cffi:mem-aref w :int))
	      (:h (cffi:mem-aref h :int))
	      (t (error "ERROR: GET-FONT-SIZE; :SIZE must be :W or :H")))))
    val))

(defmethod _get-font-style_ ((font ttf-font))
  (sdl-ttf-cffi::ttf-Get-Font-Style (sdl:fp font)))

(defmethod _get-font-height_ ((font ttf-font))
  (sdl-ttf-cffi::ttf-Get-Font-height (sdl:fp font)))

(defmethod _get-font-ascent_ ((font ttf-font))
  (sdl-ttf-cffi::ttf-Get-Font-Ascent (sdl:fp font)))

(defmethod _get-font-descent_ ((font ttf-font))
  (sdl-ttf-cffi::ttf-Get-Font-Descent (sdl:fp font)))

(defmethod _get-font-line-skip_ ((font ttf-font))
  (sdl-ttf-cffi::ttf-Get-Font-Line-Skip (sdl:fp font)))

(defmethod _get-font-faces_ ((font ttf-font))
  (sdl-ttf-cffi::ttf-Get-Font-faces (sdl:fp font)))

(defmethod _font-fixed-width-p_ ((font ttf-font))
  (sdl-ttf-cffi::ttf-Get-Font-face-is-fixed-width (sdl:fp font)))

(defmethod _get-font-face-family-name_ ((font ttf-font))
  (sdl-ttf-cffi::ttf-Get-Font-face-Family-Name (sdl:fp font)))

(defmethod _get-font-face-style-name_ ((font ttf-font))
  (sdl-ttf-cffi::ttf-Get-Font-face-Style-Name (sdl:fp font)))

;;; s

(defmethod _set-font-style_ ((font ttf-font) style)
  (sdl-ttf-cffi::ttf-Set-Font-Style (sdl:fp font) style))