
(in-package #:lispbuilder-sdl)


(defun load-image (filename path &key
		   key-color alpha-value)
  (let ((surf (surface (sdl-base::load-image filename path))))
    (if surf
	(progn
	  (when key-color (set-color-key key-color :surface surf))
	  (when alpha-value (set-alpha alpha-value :surface surf))
	  surf)
	(error "ERROR, LOAD-IMAGE: file ~A, ~A not found" filename path))))

(defun save-image (surface filename path)
  "save the supplied filename, must be a bmp file"
  (unless (typep surface 'sdl-surface)
    (error "SURFACE must be a lispbuilder Surface object."))
  (let ((file (namestring (merge-pathnames filename path))))
    (sdl-cffi::SDL-Save-BMP-RW (fp surface) (sdl-cffi::SDL-RW-FROM-FILE file "wb") 1)))

