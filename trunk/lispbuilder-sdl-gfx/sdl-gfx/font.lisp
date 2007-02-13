
(in-package :lispbuilder-sdl-gfx)

(defclass font (sdl::sdl-font)
  ((font-width :reader font-width :initform nil :initarg :font-width)
   (font-height :reader font-height :initform nil :initarg :font-height))
  (:documentation
   "The FONT object maintains the most recent surface SDL:SURFACE created by a call to any of the RENDER-STRING* functions. 
Use DRAW-FONT, DRAW-FONT-AT or DRAW-FONT-AT-* to draw the cached surface.
Prior to the first call to a RENDER-STRING* function, the cached surface is NIL."))

(defun initialise-font (font-data width height)
  "Creates a new FONT object from the font data in FONT-DATA.
Binds the symbol *DEFAULT-FONT* to FONT.
  * Returns a new FONT, or NIL if unsuccessful."
  (sdl-gfx-cffi::gfx-Primitives-Set-Font font-data width height)
  (setf *default-font* (make-instance 'font
				      :font-width width
				      :font-height height)))

(defun initialise-default-font ()
  "Creates a new FONT object from the font data in SDL-GFX-CFFI:*FONT-DATA*.
Binds the symbol *DEFAULT-FONT* to FONT
  * Returns a new FONT, or NIL if unsuccessful."
  (initialise-font sdl-gfx-cffi::*font-data* 8 8))

