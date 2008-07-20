
(in-package #:lispbuilder-sdl)
                         
(defmethod load-image (filename &key key-color surface-alpha image-type force free (key-color-at nil))
  (declare (ignore image-type force free))
  (let ((surf (make-instance 'surface
			     :surface (sdl-base::load-image (namestring filename))
			     :key-color key-color
			     :surface-alpha surface-alpha)))
    (when key-color-at
      (set-color-key (read-pixel key-color-at :surface surf) :surface surf))
    surf))

(defmethod load-image ((source rwops) &key key-color surface-alpha image-type force free (key-color-at nil))
  (declare (ignore image-type force))
  (let ((surf (make-instance 'surface
			     :surface (sdl-cffi::SDL-Load-BMP-RW (fp source) 0)
			     :key-color key-color
			     :surface-alpha surface-alpha)))
    (when key-color-at
      (set-color-key (read-pixel key-color-at :surface surf) :surface surf))
    (when free
      (sdl:free-rwops source))
    surf))

(defmethod load-image ((source VECTOR) &key key-color surface-alpha image-type force free (key-color-at nil))
  (declare (ignore image-type force free))
  (let ((surf (load-image (create-RWops-from-byte-array source)
			  :key-color key-color :surface-alpha surface-alpha
			  :free t :key-color-at key-color-at)))
    surf))

(defun save-image (surface filename)
  "Saves the surface `SURFACE` as a BMP image to a file at location `FILENAME`."
  (check-type surface sdl-surface)
  (let ((file (namestring filename)))
    (sdl-cffi::SDL-Save-BMP-RW (fp surface) (sdl-cffi::SDL-RW-FROM-FILE file "wb") 1)))

