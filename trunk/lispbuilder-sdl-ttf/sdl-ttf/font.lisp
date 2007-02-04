
(in-package #:lispbuilder-sdl-ttf)

(defclass font ()
  ((foreign-pointer-to-font :accessor fp-font :initform nil :initarg :font)
   (font-style :accessor font-style :initform nil :initarg :style)
   (font-encoding :accessor font-encoding :initform nil :initarg :encoding)
   (cached-surface :accessor cached-surface :initform nil))
  (:documentation
   "The FONT object is a CLOS wrapper around a foreign TTF_Font object. 
The FONT object maintains the most recent surface SDL:SURFACE created by a call to any of the DRAW-STRING* functions. 
Use DRAW-FONT, DRAW-FONT-AT or DRAW-FONT-AT-* to draw the cached surface.
Prior to the first call to a DRAW-STRING* function, the cached surface is NIL."))

(defun new-font (fp)
  "Creates and returns a new FONT object, returns NIL otherwise. 
FP is a pointer to a foreign TTF_Font object." 
  (when (sdl:is-valid-ptr fp)
    (make-instance 'font :font fp)))

;; (defmethod x ((font font))
;;   "Returns the X position of the cached string surface as an INTEGER."
;;   (sdl-base::rect-x (fp-position (cached-surface font))))
;; (defmethod (setf x) (x-val (font font))
;;   "Sets the X position of the cached string surface. X is an INTEGER."
;;   (setf (sdl-base::rect-x (fp-position (cached-surface font))) x-val))

;; (defmethod y ((font font))
;;   "Returns the Y position of the cached string surface as an INTEGER."
;;   (sdl-base::rect-y (fp-position (cached-surface font))))
;; (defmethod (setf y) (y-val (font font))
;;   "Sets the Y position of the cached string surface. Y is an INTEGER."
;;   (setf (sdl-base::rect-y (fp-position (cached-surface font))) y-val))

;; (defmethod fp-position ((font font))
;;   "Returns the foreign SDL_Rect from the font's cached surface. The SDL_Rect is used to store the X/Y position of the 
;; cached surface."
;;   (fp-position (cached-surface font)))

(defun free-font (font)
  "Free's the resources used by FONT. 
Closes the SDL_Font object. Explicitely free's the FONT's cached surface."
  (sdl-ttf-cffi::ttf-close-font (fp-font font))
  (when (cached-surface font)
    (sdl:free-surface (cached-surface font)))
  #-clisp(cffi:cancel-finalization font)
  )