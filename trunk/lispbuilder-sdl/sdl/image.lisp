
(in-package #:lispbuilder-sdl)

(defmethod load-image (filename
		       &key key-color alpha-value (image-type nil) (force nil) (free nil)
		       key-color-at)
  (declare (ignore image-type force free))
  (let ((surf (make-instance 'surface
			     :surface (sdl-base::load-image (namestring filename))
			     :key-color key-color
			     :alpha-value alpha-value)))
    (when key-color-at
      (set-color-key (read-pixel key-color-at :surface surf) :surface surf))
    surf))

(defun save-image (surface filename)
  "Saves the surface `SURFACE` as a BMP image to a file at location `FILENAME`."
  (check-type surface sdl-surface)
  (let ((file (namestring filename)))
    (sdl-cffi::SDL-Save-BMP-RW (fp surface) (sdl-cffi::SDL-RW-FROM-FILE file "wb") 1)))

