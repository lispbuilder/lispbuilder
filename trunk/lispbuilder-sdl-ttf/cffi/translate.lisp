;;;; SDL_ttf CFFI lisp wrapper

(in-package #:lispbuilder-sdl-ttf-cffi)


(defun pass-font (value)
  (unless (and (cffi:pointerp value)
	       (not (cffi:null-pointer-p value)))
    (error "Error: ttf-font nust not be a NULL pointer."))
  value)

(defun return-font (value)
  (if (and (cffi:pointerp value)
	   (not (cffi:null-pointer-p value)))
      value
      nil))

(defun return-val-0-1 (value)
  (if (= value 0)
      t
      nil))

(defun return-val-0+1 (value)
  (if (= value 0)
      nil
      t))

(defun return-font-style (value)
  (let ((font-style 0))
    (if (equal TTF-STYLE-NORMAL (logand value TTF-STYLE-NORMAL))
	(push :STYLE-NORMAL font-style))
    (if (equal TTF-STYLE-BOLD (logand value TTF-STYLE-BOLD))
	(push :STYLE-BOLD font-style))
    (if (equal TTF-STYLE-ITALIC (logand value TTF-STYLE-ITALIC))
	(push :STYLE-ITALIC font-style))    
    (if (equal TTF-STYLE-UNDERLINE (logand value TTF-STYLE-UNDERLINE))
	(push :STYLE-UNDERLINE font-style))
    font-style))

(defun pass-font-style (value)
  (let ((font-style 0))
    (dolist (style value)
      (case style
	(:STYLE-NORMAL (logior TTF-STYLE-NORMAL font-style))
	(:STYLE-BOLD (logior TTF-STYLE-BOLD font-style))
	(:STYLE-ITALIC (logior TTF-STYLE-ITALIC font-style))
	(:STYLE-UNDERLINE (logior TTF-STYLE-UNDERLINE font-style))
	(otherwise nil)))
    font-style))

;; (defmethod translate-to-foreign (value (type (eql 'sdl-color)))
;;   (unless (vectorp value)
;;     (error "Error: sdl-ttf:sdl-color must be of type vector.")
;;     (unless (equal 3 (length value))
;;       (error "Error: sdl-ttf:sdl-color must contain (r g b) values.")))
;;   (let ((color (cffi:foreign-alloc 'sdl:sdl_color)))
;;     (cffi:with-foreign-slots ((sdl:r sdl:g sdl:b) color sdl:sdl_color)
;;       (setf sdl:r (elt value 0)
;; 	    sdl:g (elt value 1)
;; 	    sdl:b (elt value 2)))
;;     (values color t)))

;; (defmethod free-translated-object (ptr (name (eql 'sdl-color)) free-p)
;;   (if free-p
;;       (cffi:foreign-free ptr)))

(defun pass-swapped-unicode (value)
  (case value
    (:bom-native UNICODE-BOM-NATIVE)
    (:bom-swapped UNICODE-BOM-SWAPPED)
    (otherwise 0)))
