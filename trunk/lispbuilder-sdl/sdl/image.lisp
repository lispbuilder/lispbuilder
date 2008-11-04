
(in-package #:lispbuilder-sdl)
                         
(defmethod load-image ((source rwops) &key color-key alpha image-type force free-rwops (color-key-at nil))
  "Returns a new `SURFACE` from the `RWOPS` in `SOURCE`."
  (declare (ignore image-type force))
  (let ((surf (make-instance 'surface
			     :using-surface (sdl-cffi::SDL-Load-BMP-RW (fp source) 0)
			     :enable-color-key (or color-key color-key-at)
			     :color-key color-key
			     :color-key-at color-key-at
			     :enable-alpha alpha
			     :alpha alpha
			     :x 0 :y 0)))
    (when free-rwops
      (free source))
    surf))

(defmethod load-image ((source VECTOR) &key color-key alpha image-type force free-rwops (color-key-at nil))
  "Returns a new `SURFACE` from the byte array in `SOURCE`."
  (declare (ignore image-type force free-rwops))
  (load-image (create-RWops-from-byte-array source)
	      :color-key color-key
	      :color-key-at color-key-at
	      :alpha alpha
	      :free-rwops t))

(defmethod load-image ((filename string) &key color-key alpha image-type force free-rwops (color-key-at nil))
  "Returns a new `SURFACE` from the file at location `FILENAME`."
  (declare (ignore image-type force free-rwops))
  (let ((file (namestring filename)))
    (if (and (stringp file) (probe-file file))
	(load-image (create-RWops-from-file file)
		    :color-key color-key
		    :color-key-at color-key-at
		    :alpha alpha
		    :free-rwops t)
	(error "File ~A does not exist." file))))

(defun save-image (surface filename)
  "Saves the surface `SURFACE` as a BMP image to a file at location `FILENAME`."
  (check-type surface sdl-surface)
  (let ((file (namestring filename)))
    (sdl-cffi::SDL-Save-BMP-RW (fp surface) (sdl-cffi::SDL-RW-FROM-FILE file "wb") 1)))

