
(in-package :lispbuilder-sdl-gfx)

(defclass font (sdl::sdl-font sdl::foreign-object)
  ((font-width :reader font-width :initform nil :initarg :width)
   (font-height :reader font-height :initform nil :initarg :height)
   ;; (font-data :reader font-data :initform nil :initarg :data)
   (font-default :accessor default-font-p :initform nil :initarg :default-p))
  (:default-initargs
   :gc t
    :free #'cffi:foreign-free)
  (:documentation
   "A `FONT` object manages the resources for the font. These resources include 
any cached surface as well as the foreign array containing the font data.

The cached surface is created by a call to any of the RENDER-STRING* functions. 
Use [DRAW-FONT](#draw-font), [DRAW-FONT-AT](#draw-font-at) 
or [DRAW-FONT-AT-*](#draw-font-at-*) to draw the cached surface.

Prior to the first call to a RENDER-STRING* function, the cached surface is `NIL`."))

(defun set-default-font (font)
  "Sets the font `FONT` as the default font to be used for subsequent font rendering or drawing
operations. Binds the symbol `\*DEFAULT-FONT\*` to font. 
Functions that take a `FONT` argument use `\*DEFAULT-FONT\*` unless otherwise specified.
Returns a new `FONT`, or `NIL` if unsuccessful."
  (sdl-gfx-cffi::gfx-Primitives-Set-Font (sdl:fp font)
					 (font-width font)
					 (font-height font))
  (when (typep *default-font* 'font)
    (setf (default-font-p *default-font*) nil))
  (setf (default-font-p font) t
	*default-font* font)
  font)

(defun initialise-font (font-definition)
  "Creates a new `FONT` object from the font data in `FONT-DEFINITION`.
Returns a new `FONT`, or `NIL` if unsuccessful."
  (let ((data (cffi:foreign-alloc :unsigned-char
				  :initial-contents (loop for i in (sdl::font-definition-data font-definition)
						       collect i))))
    (make-instance 'font
		   :width (sdl::font-definition-width font-definition)
		   :height (sdl::font-definition-height font-definition)
		   :fp data)))
  
(defun initialise-default-font (&optional (font-definition sdl:*font-8x8*))
  "Creates a new `FONT` object from the font definition in `FONT-DEFINITION`.
Sets the font `FONT` as the default font to be used for subsequent font rendering or drawing
operations. 

Binds the symbol `\*DEFAULT-FONT\*` to font. Functions that take a `FONT` argument use `\*DEFAULT-FONT\*` 
unless otherwise specified. Returns a new `FONT`, or `NIL` if unsuccessful."
  (set-default-font (initialise-font font-definition)))

;; (defmethod free-font ((font font))
;;   "Free the resources associated with the font `FONT`.
;; This includes freeing the cached surface and the foreign array containing the font data."
;;   (tg:cancel-finalization font)
;;   (when (sdl:cached-surface font)
;;     (sdl:free-cached-surface font))
;;   (when (sdl:is-valid-ptr (font-data font))
;;     (cffi:foreign-free (font-data font)))
;;   (setf (default-font-p font) nil))

;; (with-open-file (in "../fonts/10x20.fnt" :element-type '(unsigned-byte 8))
;;   (format t "~A" (loop 
;; 		    for byte = (read-byte in nil)
;; 		    while byte collect byte)))


