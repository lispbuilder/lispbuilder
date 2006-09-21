
;; SDL library using CFFI for foreign function interfacing...
;; (C)2006 Luke Crook <luke@balooga.com>
;; see COPYING for license

(in-package #:lispbuilder-sdl)

(defctype sdl-surface :pointer)
(defctype sdl-rectangle :pointer)
(defctype sdl-string :string)
(defctype SDL-RWops :pointer)

(declaim (inline to-int))
(defun to-int (num)
  (truncate (+ 0.5 num)))

(defun vec-to-int (vec)
  "vec-to-int will create a new VECTOR of the same length as VEC, but the contents are converted to integers.
   Returns VEC if the contents are not of type float."
  (if (vectorp vec)
      (let ((require-conversion nil)
	    (length (length vec)))
	(block convert
	  (dotimes (i length)
	    (when (floatp (svref vec i))
	      (setf require-conversion t)
	      (return-from convert))))
	(if require-conversion
	    (let ((new-vec (make-array (length vec) :initial-element 0)))
	      (dotimes (i length)
		(setf (svref new-vec i) (to-int (svref vec i))))
	      new-vec)
	    vec))
      nil))

(cffi:defcfun ("SDL_UpperBlit" UpperBlit) :int
  (src sdl-surface)
  (srcrect sdl-rectangle)
  (dst sdl-surface)
  (dstrect sdl-rectangle))

(cffi:defcfun ("SDL_FillRect" FillRect) :int
  (dst sdl-surface)
  (dstrect sdl-rectangle)
  (color :unsigned-int))

(cffi:defcfun ("SDL_FreeSurface" FreeSurface) :void
  (surface sdl-surface))

(cffi:defcfun ("SDL_WM_SetCaption" WM_SetCaption) :void
  (title sdl-string)
  (icon sdl-string))

(cffi:defcfun ("SDL_RWFromFile" RWFromFile) :pointer
  (file sdl-string)
  (mode sdl-string))

(cffi:defcfun ("SDL_VideoDriverName" VideoDriverName) :pointer
  (namebuf :pointer)
  (maxlen :int))

(cffi:defcfun ("SDL_SetClipRect" SetClipRect) :void
  (surface sdl-surface)
  (rect sdl-rectangle))

(cffi:defcfun ("SDL_GetClipRect" GetClipRect) :void
  (surface sdl-surface)
  (rect :pointer))

(defmethod translate-to-foreign (value (type (eql 'sdl-surface)))
  (unless (is-valid-ptr value)
    (error "Error: sdl-surface must be a valid pointer"))
  value)

(defmethod translate-from-foreign (value (type (eql 'sdl-surface)))
  (if (is-valid-ptr value)
      value
      nil))

(defmethod translate-to-foreign (value (type (eql 'SDL-RWops)))
  (unless (is-valid-ptr value)
    (error "Error: SDL-RWops must be a valid, non-NULL pointer"))
  value)


(defmethod translate-to-foreign (value (type (eql 'sdl-rectangle)))
  (if value
      (let ((rect (cffi:foreign-alloc 'SDL_Rect))
	    (value (vec-to-int value)))
	(cffi:with-foreign-slots ((x y w h) rect SDL_rect)
	  (setf x (rect-x value)
		y (rect-y value)
		w (rect-w value)
		h (rect-h value)))
	(values rect t))
      (values (cffi:null-pointer) nil)))

(defmethod translate-to-foreign (value (type (eql 'sdl-string)))
  (if (null value)
      (setf value ""))
  (values (cffi:foreign-string-alloc value) t))

(defmethod free-translated-object (ptr (name (eql 'sdl-rectangle)) free-p)
  (if free-p
      (cffi:foreign-free ptr)))

(defmethod free-translated-object (ptr (name (eql 'sdl-string)) free-p)
  (if free-p
      (cffi:foreign-string-free ptr)))
