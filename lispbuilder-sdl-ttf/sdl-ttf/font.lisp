
(in-package #:lispbuilder-sdl-ttf)

(defclass font (sdl::sdl-font)
  ((foreign-pointer-to-font :accessor fp-font :initform nil :initarg :font)
   (font-style :accessor font-style :initform nil :initarg :style)
   (font-encoding :accessor font-encoding :initform nil :initarg :encoding)
   (generation :accessor generation :initform *generation* :initarg :generation))
  (:documentation
   "The FONT object is a CLOS wrapper around a foreign TTF_Font object. 
The FONT object maintains the most recent surface SDL:SURFACE created by a call to any of the RENDER-STRING* functions. 
Use DRAW-FONT, DRAW-FONT-AT or DRAW-FONT-AT-* to draw the cached surface.
Prior to the first call to a RENDER-STRING* function, the cached surface is NIL."))

(defun new-font (fp)
  "Creates and returns a new FONT object, returns NIL otherwise. 
FP is a pointer to a foreign TTF_Font object." 
  (when (sdl:is-valid-ptr fp)
    (make-instance 'font :font fp)))

(defmethod free-font ((font font))
  "Free's the resources used by FONT. Explicitely free's the FONT's cached surface.
Closes the SDL_Font object only when the font has been created within the current INIT-TTF/QUIT-TTF sequence. In other
words, when (EQ (GENERATION FONT) *GENERATION*). Otherwise it is assumed that the font resources have already been
freed in a previous call to QUIT-TTF."
  (if (eq (generation font) *generation*)
      (sdl-ttf-cffi::ttf-close-font (fp-font font)))
  (when (sdl:cached-surface font)
    (sdl:free-surface (sdl:cached-surface font)))
  #-clisp(cffi:cancel-finalization font)
  )