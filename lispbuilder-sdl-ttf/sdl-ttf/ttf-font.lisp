
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

(defmethod _get-Glyph-Metric_ ((font ttf-font) ch metric)
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

(defmethod _get-Font-Size_ ((font ttf-font) text size)
  (let ((p-w (cffi:null-pointer))
	(p-h (cffi:null-pointer))
	(val nil)
        (r-val nil))
    (cffi:with-foreign-objects ((w :int) (h :int))
      (case size
	(:w (setf p-w w))
	(:h (setf p-h h)))
      (setf r-val (sdl-ttf-cffi::ttf-Size-UTF8 (sdl:fp font) text p-w p-h))
      (if r-val
	  (cond
	    ((sdl:is-valid-ptr p-w)
	     (setf val (cffi:mem-aref p-w :int)))
	    ((sdl:is-valid-ptr p-h)
	     (setf val (cffi:mem-aref p-h :int))))
	  (setf val r-val)))
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