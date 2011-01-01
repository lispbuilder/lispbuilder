
(in-package #:lispbuilder-sdl)

(setf *base-image-support* (list :BMP))
(setf *additional-image-support* (list :PNM :PPM :PGM :PBM
                                       :XPM
                                       :LBM
                                       :PCX
                                       :GIF
                                       :TGA :XV
                                       :XCF))

(defun supported-image-formats ()
  (let ((formats *base-image-support*))
    (when sdl-cffi::*image-loaded-p*
      (setf formats (append formats
                            *additional-image-support*
                            (when (cffi:foreign-symbol-pointer "IMG_isICO" :library 'sdl-cffi::sdl-image)
                              (list :ICO))
                            (when (cffi:foreign-symbol-pointer "IMG_isCUR" :library 'sdl-cffi::sdl-image)
                              (list :CUR))
                            (when (cffi:foreign-symbol-pointer "IMG_Init" :library 'sdl-cffi::sdl-image)
                              (cffi:foreign-funcall-pointer (cffi:foreign-symbol-pointer "IMG_Init" :library 'sdl-cffi::sdl-image) ()
                                                            sdl-cffi::image-init-flags (cffi:foreign-bitfield-symbol-list 'sdl-cffi::image-init-flags)
                                                            sdl-cffi::image-init-flags)))))
    formats))

(defun image-init-p (&rest flags)
  (let ((formats *base-image-support*))
    (when sdl-cffi::*image-loaded-p*
      (setf formats (append formats
                            *additional-image-support*
                            (when (cffi:foreign-symbol-pointer "IMG_isICO" :library 'sdl-cffi::sdl-image)
                              (list :ICO))
                            (when (cffi:foreign-symbol-pointer "IMG_isCUR" :library 'sdl-cffi::sdl-image)
                              (list :CUR))
                            (cffi:foreign-bitfield-symbol-list 'sdl-cffi::image-init-flags)))
      ;; Signal an error if FLAGS are not of the supported types
      (when (set-exclusive-or (intersection flags formats) flags)
        (error "ERROR: IMAGE-INIT-P does not support the ~A types." (set-exclusive-or (intersection flags formats) flags)))))
  (if sdl-cffi::*image-loaded-p*
    (progn
      (let ((fp (cffi:foreign-symbol-pointer "IMG_Init" :library 'sdl-cffi::sdl-image))
            ;; FLAGS can contain any of the supported image types, so make sure we only
            ;; logior the types supported by IMG_Init.
            ;; Example;
            ;; '(:ICO :BMP :LBM :JPG :PNG) -> '(:JPG :PNG)
            (bit-flags (cffi:foreign-bitfield-value 'sdl-cffi::image-init-flags
                                                    (intersection (cffi:foreign-bitfield-symbol-list 'sdl-cffi::image-init-flags)
                                                                  flags)))
            ;; Then, create a new list from flags, without the formats supported by IMG_Init
            ;; Example;
            ;; '(:ICO :BMP :LBM :JPG :PNG) -> '(:JPG :PNG) -> '(:ICO :BMP :LBM)
            (built-in-only (set-exclusive-or flags (intersection (cffi:foreign-bitfield-symbol-list 'sdl-cffi::image-init-flags) flags))))
        (if fp
          (let ((result (cffi:foreign-funcall-pointer fp () :int bit-flags :int)))
            (if (> result 0)
              (when (/= 0 (logand result bit-flags))
                (append built-in-only (cffi:foreign-bitfield-symbols 'sdl-cffi::image-init-flags result)))
              built-in-only))
          built-in-only)))
    flags))

(defun init-image (&rest systems)
  (apply #'image-init-p systems))

(defun quit-image ()
  (when (and sdl-cffi::*image-loaded-p* (cffi:foreign-symbol-pointer "IMG_Quit" :library 'sdl-cffi::sdl-image))
    (cffi:foreign-funcall-pointer (cffi:foreign-symbol-pointer "IMG_Quit" :library 'sdl-cffi::sdl-image)
				  () :void)))

(defun image-is-bmp (source)
  ;; start = SDL_RWtell(src);
  (let  ((is-bmp nil)
         (start (cffi:foreign-funcall-pointer (cffi:foreign-slot-value source 'sdl-cffi::sdl-rwops 'sdl-cffi::seek)
                                              () :pointer source
                                              :int 0
                                              :int sdl-cffi::rw-seek-cur
                                              :pointer)))
    ;; if ( SDL_RWread(src, magic, sizeof(magic), 1) ) {
    (cffi:with-foreign-pointer (str 2)
      (let ((result (cffi:foreign-funcall-pointer (cffi:foreign-slot-value source 'sdl-cffi::sdl-rwops 'sdl-cffi::read)
                                                  () :pointer source
                                                  :string str
                                                  :int 2
                                                  :int 1
                                                  :int)))
        (when (and (> result 0)
                   (and (> (cffi:mem-ref str :char 0) 0)
                        (< (cffi:mem-ref str :char 0) 255))
                   (and (> (cffi:mem-ref str :char 1) 0)
                        (< (cffi:mem-ref str :char 1) 255)))
          (when (string-equal (cffi:foreign-string-to-lisp str :count 2 :encoding :ascii) "BM")
            (setf is-bmp t)))))
    ;; SDL_RWseek(src, start, SEEK_SET);
    (cffi:foreign-funcall-pointer (cffi:foreign-slot-value source 'sdl-cffi::sdl-rwops 'sdl-cffi::seek)
                                  () :pointer source
                                  :pointer start
                                  :int sdl-cffi::rw-seek-set
                                  :pointer)
    is-bmp))

(defmethod image-p ((source sdl:rwops) image-type)
  (case image-type
    (:ICO (when (sdl-cffi::image-is-ICO (fp source)) image-type))
    (:CUR (when (sdl-cffi::image-is-CUR (fp source)) image-type))
    (:BMP (when (or (and sdl-cffi::*image-loaded-p* (sdl-cffi::image-is-BMP (fp source)))
                    (image-is-bmp (fp source)))
            image-type))
    (:GIF (when (sdl-cffi::image-is-GIF (fp source)) image-type))
    (:JPG (when (sdl-cffi::image-is-JPG (fp source)) image-type))
    (:LBM (when (sdl-cffi::image-is-LBM (fp source)) image-type))
    (:PCX (when (sdl-cffi::image-is-PCX (fp source)) image-type))
    (:PNG (when (sdl-cffi::image-is-PNG (fp source)) image-type))
    ((:PNM :PPM :PGM :PBM) (when (sdl-cffi::image-is-PNM (fp source)) image-type))
    (:TIF (when (sdl-cffi::image-is-TIF (fp source)) image-type))
    (:XCF (when (sdl-cffi::image-is-XCF (fp source)) image-type))
    (:XPM (when (sdl-cffi::image-is-XPM (fp source)) image-type))
    (:XV  (when (sdl-cffi::image-is-XV  (fp source)) image-type))
    (otherwise nil)))

(defmethod image-type-of ((source sdl:rwops))
  (let ((i-type nil))
    (block image-loop
      (dolist (type (supported-image-formats))
	(setf i-type (image-p source type))
	(if i-type
          (return-from image-loop))))
    i-type))

(defmethod image-p (source image-type)
  (let ((rwops (sdl:create-RWops-from-file (if (pathnamep source)
                                             (namestring source)
                                             source))))
    (when rwops
      (let ((result (image-p rwops image-type)))
	(sdl:free rwops)
	result))))

(defmethod image-type-of (source)
  (let ((rwops (sdl:create-RWops-from-file (if (pathnamep source)
                                             (namestring source)
                                             source))))
    (when rwops
      (let ((result (image-type-of rwops)))
        (sdl:free rwops)
	result))))

(defmethod image-supported-p (source)
  
  (let* ((source (if (pathnamep source)
                   (namestring source)
                   source))
         (rwops (sdl:create-RWops-from-file source)))
    (when rwops
      (let ((result (image-type-of rwops)))
        (sdl:free rwops)
        (when result
          source)))))

(defmethod %sdl-load-image% ((source rwops) &key color-key alpha image-type force free-rwops (color-key-at nil))
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

(defmethod %image-load-image% ((source sdl:rwops) &key color-key alpha (image-type nil) (force nil) (free-rwops t) (color-key-at nil))
  "Creates and returns a new surface from the image contained in the `RWOPS` structure in the source `SOURCE`."
  (let* ((image (if image-type
		    (if force
			(case image-type
			  (:ICO (sdl-cffi::image-Load-ICO-RW (sdl:fp source)))
			  (:CUR (sdl-cffi::image-Load-CUR-RW (sdl:fp source)))
			  (:BMP (sdl-cffi::image-Load-BMP-RW (sdl:fp source)))
			  (:GIF (sdl-cffi::image-Load-GIF-RW (sdl:fp source)))
			  (:JPG (sdl-cffi::image-Load-JPG-RW (sdl:fp source)))
			  (:LBM (sdl-cffi::image-Load-LBM-RW (sdl:fp source)))
			  (:PCX (sdl-cffi::image-Load-PCX-RW (sdl:fp source)))
			  (:PNG (sdl-cffi::image-Load-PNG-RW (sdl:fp source)))
			  ((:PNM :PPM :PGM :PBM) (sdl-cffi::image-Load-PNM-RW (sdl:fp source)))
			  (:TGA (sdl-cffi::image-Load-TGA-RW (sdl:fp source)))
			  (:TIF (sdl-cffi::image-Load-TIF-RW (sdl:fp source)))
			  (:XCF (sdl-cffi::image-Load-XCF-RW (sdl:fp source)))
			  (:XPM (sdl-cffi::image-Load-XPM-RW (sdl:fp source)))
			  (:XV  (sdl-cffi::image-Load-XV-RW  (sdl:fp source))))
			(sdl-cffi::image-load-typed-rw (sdl:fp source) free-rwops image-type))
		    (sdl-cffi::image-Load-RW (sdl:fp source) free-rwops)))
	 (surf (make-instance 'sdl:surface
			      :using-surface (if (cffi-sys:null-pointer-p image)
						 (error "ERROR: LOAD-IMAGE; cannot load file '~a'" (sdl-cffi::sdl-get-error))
						 image)
			      :enable-color-key (or color-key color-key-at)
			      :color-key color-key
			      :color-key-at color-key-at
			      :enable-alpha alpha
			      :alpha alpha)))
    (when free-rwops
      (setf (sdl:gc-p source) nil)
      ;;(sdl:free source)
      )
    surf))

(defmethod load-image ((source rwops) &key color-key alpha image-type force free-rwops (color-key-at nil))
  "Returns a new `SURFACE` from the `RWOPS` in `SOURCE`."
  (if sdl-cffi::*image-loaded-p*
    (%image-load-image% source :color-key color-key :alpha alpha :image-type image-type :force force :free-rwops free-rwops :color-key-at color-key-at)
    (%sdl-load-image% source :color-key color-key :alpha alpha :image-type image-type :force force :free-rwops free-rwops :color-key-at color-key-at)))

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
  (declare (ignore free-rwops))
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

(defmethod load-and-convert-image ((source string) &key color-key alpha (color-key-at nil) image-type force &allow-other-keys)
  "Loads an image from the filename `SOURCE` as per [LOAD-IMAGE-*](#load-image-*), 
 converts this image to the current display format using `CONVERT-SURFACE`. 

Parameters supported are the same as those for [LOAD-IMAGE](#load-image) and `CONVERT-IMAGE`. "
  (sdl:convert-to-display-format :surface (load-image source
						      :free-rwops t
						      :image-type image-type
						      :force force
						      :color-key color-key
						      :alpha alpha
						      :color-key-at color-key-at)
                                 :free t
                                 :inherit t))

(defmethod load-and-convert-image ((source pathname) &key color-key alpha (color-key-at nil) image-type force &allow-other-keys)
  "Loads an image from the filename `SOURCE` as per [LOAD-IMAGE-*](#load-image-*), 
 converts this image to the current display format using `CONVERT-SURFACE`. 

Parameters supported are the same as those for [LOAD-IMAGE](#load-image) and `CONVERT-IMAGE`. "
  (load-and-convert-image (namestring source)
                          :color-key color-key
                          :alpha alpha
                          :color-key-at color-key-at
                          :image-type image-type
                          :force force
                          :free-rwops t))
