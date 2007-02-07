;; This file contains some useful functions for using SDL_image from Common lisp
;; 2006 (c) Luke Crook, see LICENCE.

(in-package #:lispbuilder-sdl-image)

;;; Macros

;;; Functions


(defun create-image-from-RWops (source &key image-type force free)
  "Creates a new SDL:SURFACE from the SDL:RWOPS in SOURCE. 

  * SOURCE is of type sdl:RWOPS. Throws an error if SOURCE is not of type SDL:RWOPS. 

  * :IMAGE-TYPE type can be one of :BMP, :GIF, :JPG, :LBM, :PCX, :PNG, :PNM, :TGA, :TIF, :XCF, :XPM or :XV. 

  * :FORCE T will force an image to be loaded as :IMAGE-TYPE, ignoring the 'magic number' when present. 

  * Frees the SDL:RWOPS in SOURCE when :FREE is T. 

  * Retuns a new SDL:SURFACE, or NIL if SOURCE does not contain a valid image or the image type cannot be determined. 

Unless :FORCE T, the image 'magic number' is always used to determine the image type. 
To load an image as a specific image when the 'magic number' is unavailable, specify the image type using the 
KEYword :IMAGE-TYPE. If the 'magic number' is available and does not match :IMAGE-TYPE, then :IMAGE-TYPE is ignored. 
To load an image as :IMAGE-TYPE when the 'magic number' is available \(effectively ignoring the 'magic number'), 
specify :FORCE T. It is probably best to avoid doing this unless you know what you are doing. 

All 'non-magicable' image formats, such as TGA, must be specified using :iMAGE-TYPE. For example, to load a 
TGA image use :IMAGE-TYPE :TGA 

For example; 
  * To load a BMP image using the 'magic number': (CREATE-IMAGE-FROM-RWOPS SOURCE)
  * To load a TGA image:                          (CREATE-IMAGE-FROM-RWOPS SOURCE :IMAGE-TYPE :TGA)
  * To load a BMP image as TGA:                   (CREATE-IMAGE-FROM-RWOPS SOURCE :IMAGE-TYPE :TGA :FORCE T)"
  (unless (typep source 'sdl:rwops)
    (error "ERROR: create-image-from-RWops; SOURCE must be of type SDL:RWOPS."))
  (let ((image nil))
    (setf image (if image-type
		    (if force
			(case image-type
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
		    (sdl-image-cffi::img-Load-RW (sdl:fp source) nil)))
    (if free
	(sdl:free-rwops source))
    (when (sdl-base::is-valid-ptr image)
      (sdl:surface image))))

(defun rwops-p (source image-type)
  "Returns T if the image type of the SDL:RWOPS in SOURCE is of the type IMAGE-TYPE, returns NIL otherwise. 
Attempts to detect the image type using the 'magic number' contained in the image, if one is available. 

  * SOURCE is of type SDL:RWOPS. Throws an error if source is not of type SDL:RWOPS. 

  * :IMAGE-TYPE can be one of :BMP, :GIF, :JPG, :LBM, :PCX, :PNG, :PNM, :TIF, :XCF, :XPM or :XV. 

Note: NIL is always returned for images of type TGA as a TGA image does not contain a 'magic number'.

For example; 
  * (RWOPS-P SOURCE :IMAGE-TYPE :BMP)"
  (unless (typep source 'sdl:rwops)
    (error "ERROR; RWOPS-P: SOURCE must be of type SDL:RWOPS."))
  (case image-type
    (:BMP (if (sdl-image-cffi::img-is-BMP (sdl:fp source)) 
	      :BMP))
    (:GIF (if (sdl-image-cffi::img-is-GIF (sdl:fp source))
	      :GIF))
    (:JPG (if (sdl-image-cffi::img-is-JPG (sdl:fp source))
	      :JPG))
    (:LBM (if (sdl-image-cffi::img-is-LBM (sdl:fp source))
	      :LBM))
    (:PCX (if (sdl-image-cffi::img-is-PCX (sdl:fp source))
	      :PCX))
    (:PNG (if (sdl-image-cffi::img-is-PNG (sdl:fp source))
	      :PNG))
    (:PNM (if (sdl-image-cffi::img-is-PNM (sdl:fp source))
	      :PNM))
    (:TIF (if (sdl-image-cffi::img-is-TIF (sdl:fp source))
	      :TIF))
    (:XCF (if (sdl-image-cffi::img-is-XCF (sdl:fp source))
	      :XCF))
    (:XPM (if (sdl-image-cffi::img-is-XPM (sdl:fp source))
	      :XPM))
    (:XV  (if (sdl-image-cffi::img-is-XV  (sdl:fp source))
	      :XV))
    (otherwise nil)))

(defun rwops-type-of (source)
  "Returns the image type of the SDL:RWOPS in SOURCE. 
Attempts to detect the image type using the 'magic number' contained in the image, if one is available. 

  * SOURCE is of type SDL:RWOPS. Throws an error if source is not of type SDL:RWOPS. 

  * Returns the image type of SOURCE which may be one of 
:BMP, :GIF, :JPG, :LBM, :PCX, :PNG, :PNM, :TIF, :XCF, :XPM or :XV, or NIL if the image type 
cannot be determined or if there is no magic number available. 

Note: This means that NIL is returned for images of type TGA as a TGA image does not contain a 'magic number'.

For example; 
  * (RWOPS-TYPE-OF SOURCE)"
  (unless (typep source 'sdl:rwops)
    (error "ERROR; RWOPS-TYPE-OF: SOURCE must be of type SDL:RWOPS."))
  (let ((i-type nil))
    (block image-loop
      (dolist (type '(:BMP :PNM :XPM :XCF :PCX :GIF :JPG :TIF :LBM :PNG :XV))
	(setf i-type (rwops-p source type))
	(if i-type
	    (return-from image-loop))))
    i-type))

(defun image-p (filename pathname image-type)
  "Returns T if the image at location FILENAME and PATHNAME is of the type IMAGE-TYPE, returns NIL otherwise.
Attempts to detect the image type using the 'magic number' contained in the image, if one is available. 

  * FILENAME and PATHNAME are STRINGS. 

  * :IMAGE-TYPE can be one of :BMP, :GIF, :JPG, :LBM, :PCX, :PNG, :PNM, :TIF, :XCF, :XPM or :XV. 

  * Returns T if the image is of IMAGE-TYPE 
  * Returns NIL if:
    * The file at FILENAME and PATHNAME does not exist, or
    * The IMAGE-TYPE cannot be determined if the magic number is not supported or the magic number is not found

Note: NIL is always returned for images of type TGA as a TGA image does not contain a 'magic number'.

For example; 
  * (IMAGE-P \"image.bmp\" \"c:/images/\" :IMAGE-TYPE :BMP)"
  (let ((rwops (sdl:create-RWops-from-file filename PATHNAME)))
    (when rwops
      (let ((result (rwops-p rwops image-type)))
	(sdl:free-rwops rwops)
	result))))

(defun image-type-of (filename pathname)
  "Returns the type of image at location FILENAME and PATHNAME.
Attempts to detect the image type using the 'magic number' contained in the image, if one is available. 

  * FILENAME and PATHNAME are STRINGS.

  * Returns one of :BMP, :GIF, :JPG, :LBM, :PCX, :PNG, :PNM, :TIF, :XCF, :XPM or :XV, if the image type can be determined 
  * Returns NIL if: 
    * The file at FILENAME and PATHNAME does not exist, or 
    * The image type cannot be determined if the magic number is not supported or the magic number is not found 

Note: NIL is always returned for images of type TGA as a TGA image does not contain a 'magic number'. 

For example; 
  * (IMAGE-TYPE-OF \"image.bmp\" \"c:/images/\")"
  (let ((rwops (sdl:create-RWops-from-file filename PATHNAME)))
    (when rwops
      (let ((result (rwops-type-of rwops)))
	(sdl:free-rwops rwops)
	result))))


(defun load-image (filename pathname &key image-type force)
  "Creates a new SDL:SURFACE from the image load from location FILENAME and PATHNAME. 

  * FILENAME and PATHNAME are STRINGs.

  * :IMAGE-TYPE type can be one of :BMP, :GIF, :JPG, :LBM, :PCX, :PNG, :PNM, :TGA, :TIF, :XCF, :XPM or :XV. 

  * :FORCE T will force an image to be loaded as :IMAGE-TYPE, ignoring the 'magic number' when present. 

  * Retuns a new SDL:SURFACE, or NIL if the file does not contain a valid image or the image type cannot be determined. 

Unless :FORCE T, the image 'magic number' is always used to determine the image type. 
To load an image as a specific image when the 'magic number' is unavailable, specify the image type using the 
KEYword :IMAGE-TYPE. If the 'magic number' is available and does not match :IMAGE-TYPE, then :IMAGE-TYPE is ignored. 
To load an image as :IMAGE-TYPE when the 'magic number' is available \(effectively ignoring the 'magic number'), 
specify :FORCE T. It is probably best to avoid doing this unless you know what you are doing. 

All 'non-magicable' image formats, such as TGA, must be specified using :iMAGE-TYPE. For example, to load a 
TGA image use :IMAGE-TYPE :TGA 

For example; 
  * To load a BMP image using the 'magic number': (LOAD-IMAGE \"image.bmp\" \"c:/images/\")
  * To load a TGA image:                          (LOAD-IMAGE \"image.tga\" \"c:/images/\" :IMAGE-TYPE :TGA)
  * To load a BMP image as TGA:                   (LOAD-IMAGE \"image.bmp\" \"c:/images/\" :IMAGE-TYPE :BMP :FORCE T)"
  (let ((rwops (sdl:create-RWops-from-file filename pathname)))
    (when rwops (create-image-from-RWops rwops
					 :free t
					 :image-type image-type
					 :force force))))

(defun load-and-convert-image (filename pathname &rest named-pairs &key image-type force &allow-other-keys)
  "Loads an image as per LOAD-IMAGE and converts this surface to the current display format using SDL:CONVERT-SURFACE. 
Parameters supported are the same as those for LOAD-IMAGE and SDL:CONVERT-IMAGE. "
  (let ((rwops (sdl:create-RWops-from-file filename pathname)))
    (when rwops
      (apply #'sdl:convert-surface
	     :surface (create-image-from-RWops rwops
					       :free t
					       :image-type image-type
					       :force force)
	     :free-p t
	     :allow-other-keys t
	     named-pairs))))