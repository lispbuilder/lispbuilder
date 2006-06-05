;;;; SDL_ttf2.0 CFFI lisp wrapper

(in-package #:lispbuilder-sdl-ttf)


(defmethod translate-to-foreign (value (type (eql 'ttf-font)))
  (unless (sdl:is-valid-ptr value)
    (error "Error: ttf-font nust not be a NULL pointer."))
  value)

(defmethod translate-from-foreign (value (type (eql 'ttf-font)))
  (if (sdl:is-valid-ptr value)
      value
      nil))

(defmethod translate-from-foreign (value (type (eql 'ttf-return-val-0-1)))
  (if (= value 0)
      t
      nil))

(defmethod translate-from-foreign (value (type (eql 'ttf-return-val-0+1)))
  (if (= value 0)
      nil
      t))

(defmethod translate-from-foreign (value (type (eql 'ttf-font-style)))
  (let ((font-style 0))
    (if (equal TTF_STYLE_NORMAL (logand value TTF_STYLE_NORMAL))
	(push :STYLE-NORMAL font-style))
    (if (equal TTF_STYLE_BOLD (logand value TTF_STYLE_BOLD))
	(push :STYLE-BOLD font-style))
    (if (equal TTF_STYLE_ITALIC (logand value TTF_STYLE_ITALIC))
	(push :STYLE-ITALIC font-style))    
    (if (equal TTF_STYLE_UNDERLINE (logand value TTF_STYLE_UNDERLINE))
	(push :STYLE-UNDERLINE font-style))
    font-style))

(defmethod translate-to-foreign (value (type (eql 'ttf-font-style)))
  (let ((font-style 0))
    (dolist (style value)
      (case style
	(:STYLE-NORMAL (logior TTF_STYLE_NORMAL font-style))
	(:STYLE-BOLD (logior TTF_STYLE_BOLD font-style))
	(:STYLE-ITALIC (logior TTF_STYLE_ITALIC font-style))
	(:STYLE-UNDERLINE (logior TTF_STYLE_UNDERLINE font-style))
	(otherwise nil)))
    font-style))

(defmethod translate-to-foreign (value (type (eql 'sdl-color)))
  (unless (vectorp value)
    (error "Error: sdl-ttf:sdl-color must be of type vector.")
    (unless (equal 3 (length value))
      (error "Error: sdl-ttf:sdl-color must contain (r g b) values.")))
  (let ((color (cffi:foreign-alloc 'sdl:sdl_color)))
    (cffi:with-foreign-slots ((sdl:r sdl:g sdl:b) color sdl:sdl_color)
      (setf sdl:r (elt value 0)
	    sdl:g (elt value 1)
	    sdl:b (elt value 2)))
    (values color t)))

(defmethod free-translated-object (ptr (name (eql 'sdl-color)) free-p)
  (if free-p
      (cffi:foreign-free ptr)))

(defmethod translate-to-foreign (value (type (eql 'ttf-swapped-unicode)))
  (case value
    (:bom-native UNICODE_BOM_NATIVE)
    (:bom-swapped UNICODE_BOM_SWAPPED)
    (otherwise 0)))
