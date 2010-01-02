
(in-package #:lispbuilder-sdl)

                    
(defmethod load-image ((source rwops) &key color-key alpha image-type force free-rwops (color-key-at nil))
  "Returns a new `SURFACE` from the `RWOPS` in `SOURCE`."
  (declare (ignore image-type force))
  (let* ((surf-ptr (sdl-cffi::SDL-Load-BMP-RW (fp source) 0))
	 (surf (make-instance 'surface
			      :using-surface (if (cffi-sys:null-pointer-p surf-ptr)
                                               (error "ERROR: LOAD-IMAGE; cannot load file '~a'" (sdl-cffi::sdl-get-error))
                                               surf-ptr)
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

(defmethod load-image ((filename pathname) &key color-key alpha image-type force free-rwops (color-key-at nil))
  "Returns a new `SURFACE` from the file at location `PATHNAME`."
  (declare (ignore image-type force free-rwops))
  (load-image (namestring filename)
              :color-key color-key
              :alpha alpha
              :image-type image-type
              :force force
              :free-rwops t
              :color-key-at color-key-at))

(defmethod save-image ((surface sdl-surface) (filename string))
  "Saves the `SURFACE` as a `BMP` image to a file at location `FILENAME`."
  (sdl-cffi::SDL-Save-BMP-RW (fp surface) (sdl-cffi::SDL-RW-FROM-FILE filename "wb") 1))

(defmethod save-image ((surface sdl-surface) (filename pathname))
  "Saves the `SURFACE` as a `BMP` image to a file at location `FILENAME`."
  (save-image surface (namestring filename)))

(defun load-image-from-byte-sequence (array)
  (let ((mem-array (cffi:foreign-alloc :unsigned-char :initial-contents array)))
    (make-instance 'surface :fp (sdl-cffi::sdl-load-bmp-rw (sdl-cffi::sdl-rw-from-mem mem-array (length array)) 1))))
