
(in-package #:lispbuilder-sdl)


(defmethod load-image ((filename string) &key key-color alpha-value (image-type nil) (force nil) (free nil))
  (declare (ignore image-type force free))
  (let ((surf (surface (sdl-base::load-image filename))))
    (if surf
	(progn
	  (when key-color (set-color-key key-color :surface surf))
	  (when alpha-value (set-alpha alpha-value :surface surf))
	  surf)
	(error "ERROR, LOAD-IMAGE: file ~A, ~A not found" filename))))

(defun save-image (surface filename)
  "save the supplied filename, must be a bmp file"
  (unless (typep surface 'sdl-surface)
    (error "SURFACE must be a lispbuilder Surface object."))
  (let ((file (namestring filename)))
    (sdl-cffi::SDL-Save-BMP-RW (fp surface) (sdl-cffi::SDL-RW-FROM-FILE file "wb") 1)))

