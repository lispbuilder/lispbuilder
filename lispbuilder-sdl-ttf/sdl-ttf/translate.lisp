
;; SDL_ttf library using CFFI for foreign function interfacing...
;; see COPYING for license

(in-package #:lispbuilder-sdl-ttf)

(defctype sdl-color :pointer)
(defctype ttf-font :pointer)

(defcfun ("TTF_RenderText_Solid" RenderText_Solid) :pointer
  (font ttf-font)
  (text :string)
  (fg sdl-color))

(defmethod translate-to-foreign (value (type (eql 'ttf-font)))
  (unless (sdl:is-valid-ptr value)
    (error "Error: sdl-ttf:ttf-font must be a valid pointer."))
  value)

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
