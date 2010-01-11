;; This file contains some useful functions for using SDL_image from Common lisp
;; 2006 (c) Luke Crook, see LICENCE.

(in-package #:lispbuilder-sdl-image)

(defvar *base-image-format-support* (list :BMP :GIF :JPG :LBM :PCX :PNG :PNM :TGA :TIF :XCF :XPM :XV))

(defun image-library-version ()
  (sdl:library-version (sdl-image-cffi::img-linked-version)))

(defun image-glue-version ()
  (sdl:version-number sdl-image-cffi::sdl-image-major-version
                      sdl-image-cffi::sdl-image-minor-version
                      sdl-image-cffi::sdl-image-patch-level))

(defun load-library ()
  (sdl-image-cffi::load-library))

(defun image-init-p (&rest flags)
  ;; Singal an error if FLAGS are not of the supported types
  (let ((types (set-exclusive-or (intersection flags (append *base-image-format-support*
						      (cffi:foreign-bitfield-symbol-list 'sdl-image-cffi::init-flags)))
			  flags)))
    (when types
	(error "ERROR: INIT-P does not support the ~A types." types)))
  (let ((fp (cffi:foreign-symbol-pointer "IMG_Init" :library 'sdl-image-cffi::sdl-image))
	;; FLAGS can contain any of the supported image types, so make sure we only
	;; logior the types supported by IMG_Init.
	;; Example;
	;; '(:ICO :BMP :LBM :JPG :PNG) -> '(:JPG :PNG)
	(bit-flags (cffi:foreign-bitfield-value 'sdl-image-cffi::init-flags
						(intersection (cffi:foreign-bitfield-symbol-list 'sdl-image-cffi::init-flags)
							      flags))) 
	;; Then, create a new list from flags, without the formats supported by IMG_Init
	;; Example;
	;; '(:ICO :BMP :LBM :JPG :PNG) -> '(:JPG :PNG) -> '(:ICO :BMP :LBM)
	(built-in-only (set-exclusive-or flags (intersection (cffi:foreign-bitfield-symbol-list 'sdl-image-cffi::init-flags) flags))))
    (when fp
      (let ((result (cffi:foreign-funcall-pointer fp () :int bit-flags :int)))
	(if (> result 0)
	    (when (/= 0 (logand result bit-flags))
	      (append built-in-only (cffi:foreign-bitfield-symbols 'sdl-image-cffi::init-flags result)))
	    built-in-only)))))

(defun init-image (&rest systems)
  (apply #'image-init-p systems))

(defun quit-image ()
  (when (cffi:foreign-symbol-pointer "IMG_Quit" :library 'sdl-image-cffi::sdl-image)
    (cffi:foreign-funcall-pointer (cffi:foreign-symbol-pointer "IMG_Quit" :library 'sdl-image-cffi::sdl-image)
				  () :void)))

(defun img-is-ico (source)
  (when (cffi:foreign-symbol-pointer "IMG_isICO" :library 'sdl-image-cffi::sdl-image)
    (when (= 1 (cffi:foreign-funcall-pointer (cffi:foreign-symbol-pointer "IMG_isICO" :library 'sdl-image-cffi::sdl-image)
					     () sdl-cffi::SDL-RWops source :int))
      t)))

(defun img-is-cur (source)
  (when (cffi:foreign-symbol-pointer "IMG_isCUR" :library 'sdl-image-cffi::sdl-image)
    (when (= 1 (cffi:foreign-funcall-pointer (cffi:foreign-symbol-pointer "IMG_isCUR" :library 'sdl-image-cffi::sdl-image)
					   () sdl-cffi::SDL-RWops source :int))
      t)))

(defun img-load-ico-rw (source)
  (when (cffi:foreign-symbol-pointer "IMG_LoadICO_RW" :library 'sdl-image-cffi::sdl-image)
    (cffi:foreign-funcall-pointer (cffi:foreign-symbol-pointer "IMG_LoadICO_RW" :library 'sdl-image-cffi::sdl-image)
					   ()  sdl-cffi::SDL-RWops source sdl-cffi::sdl-surface)))

(defun img-load-cur-rw (source)
  (when (cffi:foreign-symbol-pointer "IMG_LoadCUR_RW" :library 'sdl-image-cffi::sdl-image)
    (cffi:foreign-funcall-pointer (cffi:foreign-symbol-pointer "IMG_LoadCUR_RW" :library 'sdl-image-cffi::sdl-image)
					   ()  sdl-cffi::SDL-RWops source sdl-cffi::sdl-surface)))

(defmethod image-p ((source sdl:rwops) image-type)
  (case image-type
    (:ICO (when (img-is-ICO (sdl:fp source)) 
            :ICO))
    (:CUR (when (img-is-CUR (sdl:fp source))
            :CUR))
    (:BMP (when (sdl-image-cffi::img-is-BMP (sdl:fp source)) 
            :BMP))
    (:GIF (when (sdl-image-cffi::img-is-GIF (sdl:fp source))
            :GIF))
    (:JPG (when (sdl-image-cffi::img-is-JPG (sdl:fp source))
            :JPG))
    (:LBM (when (sdl-image-cffi::img-is-LBM (sdl:fp source))
            :LBM))
    (:PCX (when (sdl-image-cffi::img-is-PCX (sdl:fp source))
            :PCX))
    (:PNG (when (sdl-image-cffi::img-is-PNG (sdl:fp source))
            :PNG))
    (:PNM (when (sdl-image-cffi::img-is-PNM (sdl:fp source))
            :PNM))
    (:TIF (when (sdl-image-cffi::img-is-TIF (sdl:fp source))
            :TIF))
    (:XCF (when (sdl-image-cffi::img-is-XCF (sdl:fp source))
            :XCF))
    (:XPM (when (sdl-image-cffi::img-is-XPM (sdl:fp source))
            :XPM))
    (:XV  (when (sdl-image-cffi::img-is-XV  (sdl:fp source))
            :XV))
    (otherwise nil)))

(defmethod image-p (source image-type)
  (let ((rwops (sdl:create-RWops-from-file source)))
    (when rwops
      (let ((result (image-p rwops image-type)))
	(sdl:free rwops)
	result))))

(defmethod image-type-of ((source sdl:rwops))
  (let ((i-type nil))
    (block image-loop
      (dolist (type '(:ICO :CUR :BMP :PNM :XPM :XCF :PCX :GIF :JPG :TIF :LBM :PNG :XV))
	(setf i-type (image-p source type))
	(if i-type
          (return-from image-loop))))
    i-type))


(defmethod image-type-of (source)
  (let ((rwops (sdl:create-RWops-from-file source)))
    (when rwops
      (let ((result (image-type-of rwops)))
	(sdl:free rwops)
	result))))

(defmethod load-image ((source sdl:rwops) &key color-key alpha (image-type nil) (force nil) (free-rwops t) (color-key-at nil))
  "Creates and returns a new surface from the image contained in the `RWOPS` structure in the source `SOURCE`."
  (let* ((image (if image-type
		    (if force
			(case image-type
			  (:ICO (img-Load-ICO-RW (sdl:fp source)))
			  (:CUR (img-Load-CUR-RW (sdl:fp source)))
			  (:BMP (sdl-image-cffi::img-Load-BMP-RW (sdl:fp source)))
			  (:GIF (sdl-image-cffi::img-Load-GIF-RW (sdl:fp source)))
			  (:JPG (sdl-image-cffi::img-Load-JPG-RW (sdl:fp source)))
			  (:LBM (sdl-image-cffi::img-Load-LBM-RW (sdl:fp source)))
			  (:PCX (sdl-image-cffi::img-Load-PCX-RW (sdl:fp source)))
			  (:PNG (sdl-image-cffi::img-Load-PNG-RW (sdl:fp source)))
			  (:PNM (sdl-image-cffi::img-Load-PNM-RW (sdl:fp source)))
			  (:TGA (sdl-image-cffi::img-Load-TGA-RW (sdl:fp source)))
			  (:TIF (sdl-image-cffi::img-Load-TIF-RW (sdl:fp source)))
			  (:XCF (sdl-image-cffi::img-Load-XCF-RW (sdl:fp source)))
			  (:XPM (sdl-image-cffi::img-Load-XPM-RW (sdl:fp source)))
			  (:XV  (sdl-image-cffi::img-Load-XV-RW  (sdl:fp source))))
			(sdl-image-cffi::img-load-typed-rw (sdl:fp source) free-rwops image-type))
		    (sdl-image-cffi::img-Load-RW (sdl:fp source) free-rwops)))
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

(defmethod load-image ((source VECTOR) &key color-key alpha (image-type nil) (force nil) (free-rwops t) (color-key-at nil))
  "Creates and returns a new surface from the image contained in the byte VECTOR in `SOURCE`."
  (declare (ignore free-rwops))
  (load-image (sdl::create-RWops-from-byte-array source)
	      :color-key color-key
	      :color-key-at color-key-at
	      :alpha alpha
	      :image-type image-type
	      :force force :free-rwops t))

(defmethod load-image ((source string) &key color-key alpha (image-type nil) (force nil) (free-rwops t) (color-key-at nil))
  "Creates and returns a new surface from the image in the file at the location `SOURCE`."
  (declare (ignore free-rwops))
  (let ((rwops (sdl:create-RWops-from-file source)))
    (if rwops
      (let ((surface (load-image rwops
				 :color-key color-key
				 :color-key-at color-key-at
				 :alpha alpha
				 :free-rwops t
				 :image-type image-type
				 :force force)))
	(if surface
          surface
          (error "ERROR: LOAD-IMAGE: file type ~A, not supported" source)))
      (error "ERROR: LOAD-IMAGE: file ~A, not found" source))))

(defmethod load-image ((source pathname) &key color-key alpha (image-type nil) (force nil) (free-rwops t) (color-key-at nil))
  "Returns a new `SURFACE` from the file at location `PATHNAME`."
  (declare (ignore free-rwops))
  (load-image (namestring source)
              :color-key color-key
              :alpha alpha
              :image-type image-type
              :force force
              :free-rwops t
              :color-key-at color-key-at))

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


;; (defun load-and-convert-image (source &rest named-pairs &key image-type force &allow-other-keys)
;;   "Loads an image from the filename `SOURCE` as per [LOAD-IMAGE-*](#load-image-*), 
;;  converts this image to the current display format using `CONVERT-SURFACE`. 

;; Parameters supported are the same as those for [LOAD-IMAGE](#load-image) and `CONVERT-IMAGE`. "
;;   (apply #'sdl:convert-surface
;; 	 :surface (load-image source
;; 			      :free t
;; 			      :image-type image-type
;; 			      :force force)
;; 	 :free t
;; 	 :allow-other-keys t
;; 	 named-pairs))
