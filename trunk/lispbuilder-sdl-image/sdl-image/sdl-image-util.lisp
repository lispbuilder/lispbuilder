;; This file contains some useful functions for using SDL_image from Common lisp
;; 2006 (c) Luke Crook, see LICENCE.

(in-package #:lispbuilder-sdl-image)

;;; Macros

;;; Functions


(defmethod create-image-from-RWops ((source sdl:rwops) &key image-type force free)
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

(defmethod rwops-p ((source sdl:rwops) image-type)
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

(defmethod rwops-type-of ((source sdl:rwops))
  (let ((i-type nil))
    (block image-loop
      (dolist (type '(:BMP :PNM :XPM :XCF :PCX :GIF :JPG :TIF :LBM :PNG :XV))
	(setf i-type (rwops-p source type))
	(if i-type
	    (return-from image-loop))))
    i-type))

(defun image-p (filename pathname image-type)
  "Returns T if the image at location FILENAME and PATHNAME is of the type IMAGE-TYPE, returns NIL if otherwise.
Attempts to detect the image type using the 'magic number' contained in the image, if one is available. 

FILENAME and PATHNAME are STRINGS. 

:IMAGE-TYPE can be one of :BMP, :GIF, :JPG, :LBM, :PCX, :PNG, :PNM, :TIF, :XCF, :XPM or :XV. 
Note: This means that NIL is always returned for images of type TGA."
  (let ((rwops (sdl:create-RWops-from-file filename PATHNAME)))
    (when rwops
      (let ((result (rwops-p rwops image-type)))
	(sdl:free-rwops rwops)
	result))))

(defun image-type-of (filename pathname)
  "Returns the image type of the image at location FILENAME and PATHNAME.
Attempts to detect the image type using the 'magic number' contained in the image, if one is available. 

FILENAME and PATHNAME are both STRINGS.

Returns one of :BMP, :GIF, :JPG, :LBM, :PCX, :PNG, :PNM, :TIF, :XCF, :XPM or :XV, or NIL if the image type 
cannot be determined or if there is no magic number available.
Note: This means that NIL is always returned for images of type TGA."
  (let ((rwops (sdl:create-RWops-from-file filename PATHNAME)))
    (when rwops
      (let ((result (rwops-type-of rwops)))
	(sdl:free-rwops rwops)
	result))))


(defun load-image (filename pathname &key image-type force)
  "Creates a new sdl:SURFACE from the image loaded at location FILENAME and PATHNAME. 
The image 'magic number' contained in the image is used to detect the image type and automatically load the image. 

FILENAME and PATHNAME are STRINGs.

Returns a new SDL:SURFACE, or NIL if the image cannot be loaded or the image type cannot be determined.

To load an image as a specific image type, set the :IMAGE-TYPE to the desired type. 
The image type can be one of :BMP, :GIF, :JPG, :LBM, :PCX, :PNG, :PNM, :TGA, :TIF, :XCF, :XPM or :XV. 
If the 'magic number' is available and does not match :IMAGE-TYPE, then :IMAGE-TYPE is ignored.

Use :FORCE T to override the 'magic number' when attempting to load an image as a different type.

All 'non-magicable' image formats, such as TGA, must be specified using :iMAGE-TYPE or :FORCE and IMAGE-TYPE. 
For example, to load a TGA image use :IMAGE-TYPE :TGA"
  (create-image-from-RWops (sdl:create-RWops-from-file filename pathname)
			   :free t
			   :image-type image-type
			   :force force))

(defun load-and-convert-image (filename pathname &rest named-pairs &key image-type force &allow-other-keys)
  "Loads an image as per LOAD-IMAGE. Then Converts this image as per SDL:CONVERT-SURFACE.
Returns a new SDL:SURFACE, or NIL if the image cannot be loaded or the image type cannot be determined.
Parameters are per LOAD-IMAGE and SDL:CONVERT-IMAGE."
  (apply #'sdl:convert-surface
	 :surface (create-image-from-RWops (sdl:create-RWops-from-file filename pathname)
					   :free t
					   :image-type image-type
					   :force force)
	 :free-p t
	 :allow-other-keys t
	 named-pairs))
