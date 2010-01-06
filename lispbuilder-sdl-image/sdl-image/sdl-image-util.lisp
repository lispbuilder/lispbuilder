;; This file contains some useful functions for using SDL_image from Common lisp
;; 2006 (c) Luke Crook, see LICENCE.

(in-package #:lispbuilder-sdl-image)

(defvar *initialized* 0)

(defun image-library-version ()
  (sdl:library-version (sdl-image-cffi::img-linked-version)))

(defun image-glue-version ()
  (sdl:version-number sdl-image-cffi::sdl-image-major-version
                      sdl-image-cffi::sdl-image-minor-version
                      sdl-image-cffi::sdl-image-patch-level))

(defun initialized-p (flags)
  (let ((fp (cffi:foreign-symbol-pointer "IMG_Init" :library 'sdl-image-cffi::sdl-image)))
    (when fp
      (when (/= 0 (logand (cffi:foreign-funcall-pointer fp () :int flags :int)
			  flags))
	(setf *initialized* (logior *initialized* flags))))))

(defun init-jpg ()
  (initialized-p (cffi:foreign-enum-value 'sdl-image-cffi::img-init-flags :init-jpg)))

(defun init-png ()
  (initialized-p (cffi:foreign-enum-value 'sdl-image-cffi::img-init-flags :init-png)))

(defun init-tif ()
  (initialized-p (cffi:foreign-enum-value 'sdl-image-cffi::img-init-flags :init-tif)))

(defun jpg-init-p ()
  (initialized-p (cffi:foreign-enum-value 'sdl-image-cffi::img-init-flags :init-jpg)))

(defun png-init-p ()
  (initialized-p (cffi:foreign-enum-value 'sdl-image-cffi::img-init-flags :init-png)))

(defun tif-init-p ()
  (initialized-p (cffi:foreign-enum-value 'sdl-image-cffi::img-init-flags :init-tif)))

(defun init (&key (jpg nil) (png nil) (tif nil))
  (initialized-p (logior (if jpg (cffi:foreign-enum-value 'sdl-image-cffi::img-init-flags :init-jpg) 0)
			 (if png (cffi:foreign-enum-value 'sdl-image-cffi::img-init-flags :init-png) 0)
			 (if tif (cffi:foreign-enum-value 'sdl-image-cffi::img-init-flags :init-tif) 0))))

(defun quit ()
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
  (let ((image nil))
    (setf image (if image-type
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
                    (sdl-image-cffi::img-load-typed-rw (sdl:fp source) nil image-type))
                  (sdl-image-cffi::img-Load-RW (sdl:fp source) free-rwops)))
    (when free-rwops
      (setf (sdl:gc-p source) nil)
      ;;(sdl:free source)
      )
    (when (sdl:is-valid-ptr image)
      (make-instance 'sdl:surface
		     :using-surface image
		     :enable-color-key (or color-key color-key-at)
		     :color-key color-key
		     :color-key-at color-key-at
		     :enable-alpha alpha
		     :alpha alpha))))

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
