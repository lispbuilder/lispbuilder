
(in-package :lispbuilder-sdl)

(defclass gfx-bitmap-font (sdl-bitmap-font)
  ((font-default :accessor default-font-p :initform nil :initarg :default-p))
  (:default-initargs
   :gc t
    :free #'cffi:foreign-free))

(defmethod set-default-font ((font gfx-bitmap-font))
  (sdl-gfx-cffi::gfx-Primitives-Set-Font (fp font)
					 (char-width font)
					 (char-height font))
  (when (typep *default-font* 'gfx-bitmap-font)
    (setf (default-font-p *default-font*) nil))
  (setf (default-font-p font) t
	*default-font* font)
  font)

(in-package :lispbuilder-sdl-gfx)

(defun initialise-font (font-definition)
  (make-instance 'sdl:gfx-bitmap-font :font-definition font-definition))
  
(defun initialise-default-font (&optional (font-definition sdl:*font-8x8*))
  (sdl:set-default-font (initialise-font font-definition)))

;; (with-open-file (in "../fonts/10x20.fnt" :element-type '(unsigned-byte 8))
;;   (format t "~A" (loop 
;; 		    for byte = (read-byte in nil)
;; 		    while byte collect byte)))


