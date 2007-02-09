
(in-package :lispbuilder-sdl-gfx)

(defclass font (sdl::sdl-font)
  ((font-width :reader font-width :initform nil :initarg :font-width)
   (font-height :reader font-height :initform nil :initarg :font-height))
  (:documentation
   "The FONT object maintains the most recent surface SDL:SURFACE created by a call to any of the RENDER-STRING* functions. 
Use DRAW-FONT, DRAW-FONT-AT or DRAW-FONT-AT-* to draw the cached surface.
Prior to the first call to a RENDER-STRING* function, the cached surface is NIL."))

(defun initialise-default-font ()
  "Creates a new FONT object from the font data in *font-data*.
Binds the symbol *DEFAULT-FONT* to FONT
  * Returns a new FONT, or NIL if unsuccessful."
  (gfx-Primitives-Set-Font *font-data* 8 8)
  (setf *default-font* (make-instance 'font
				      :font-width 8
				      :font-height 8)))

