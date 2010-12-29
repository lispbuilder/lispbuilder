
(in-package :lispbuilder-sdl-gfx)

(defmethod initialise-font ((self sdl::gfx-font-definition))
  (make-instance 'sdl:gfx-bitmap-font :font-definition self))
  
(defun initialise-default-font (&optional (font-definition sdl:*gfx-font-8x8*))
  (sdl:set-default-font (initialise-font font-definition)))

;; (with-open-file (in "../fonts/10x20.fnt" :element-type '(unsigned-byte 8))
;;   (format t "~A" (loop 
;; 		    for byte = (read-byte in nil)
;; 		    while byte collect byte)))


