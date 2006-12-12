
;; SDL library using CFFI for foreign function interfacing...
;; (C)2006 Luke Crook <luke@balooga.com>
;; see COPYING for license

(in-package #:lispbuilder-sdl-cffi)

(defctype sdl-surface :pointer)
(defctype sdl-rectangle :pointer)
(defctype sdl-string :string)
;(defctype SDL-RWops :pointer)

;; (defmethod translate-to-foreign (value (type (eql 'sdl-surface)))
;;   (unless (is-valid-ptr value)
;;     (error "Error: sdl-surface must be a valid pointer"))
;;   value)

;; (defmethod translate-from-foreign (value (type (eql 'sdl-surface)))
;;   (if (is-valid-ptr value)
;;       value
;;       nil))

;; (defmethod translate-to-foreign (value (type (eql 'SDL-RWops)))
;;   (unless (is-valid-ptr value)
;;     (error "Error: SDL-RWops must be a valid, non-NULL pointer"))
;;   value)


;; (defmethod translate-to-foreign (value (type (eql 'sdl-rectangle)))
;;   (if value
;;       (let ((rect (cffi:foreign-alloc 'SDL_Rect))
;; 	    (value (vec-to-int value)))
;; 	(cffi:with-foreign-slots ((x y w h) rect SDL_rect)
;; 	  (setf x (rect-x value)
;; 		y (rect-y value)
;; 		w (rect-w value)
;; 		h (rect-h value)))
;; 	(values rect t))
;;       (values (cffi:null-pointer) nil)))

(defmethod translate-to-foreign (value (type (eql 'sdl-string)))
  (if (null value)
      (setf value ""))
  (values (cffi:foreign-string-alloc value) t))

;; (defmethod free-translated-object (ptr (name (eql 'sdl-rectangle)) free-p)
;;   (if free-p
;;       (cffi:foreign-free ptr)))

(defmethod free-translated-object (ptr (name (eql 'sdl-string)) free-p)
  (if free-p
      (cffi:foreign-string-free ptr)))
