;; This file contains some useful functions for using SDL_image from Common lisp
;; 2006 (c) Luke Crook, see LICENCE.

(in-package #:lispbuilder-sdl-image)

;;; Macros

;;; Functions


(defmethod create-image-from-RWops ((source sdl:rwops) &key image-type force free)
  "Returns a new SDL surface RESULT from the image SOURCE. The 'magic number' contained in the image SOURCE 
is used to detect the image type, if one is available. 
RESULT is a foreign type SDL:SDL_Surface, or NIL if SOURCE does not contain a valid image.
SOURCE is a foreign type SDL:SDL_RWops.
Use :IMAGE-TYPE to load an image when the 'magic number' is unavailable, where :IMAGE-TYPE is one of 
:BMP, :GIF, :JPG, :LBM, :PCX, :PNG, :PNM, :TGA, :TIF, :XCF, :XPM or :XV. Note: All 'non-magicable' image types, 
such as TGA must have the image type specified using :IMAGE-TYPE.
Use :FORCE T to override the 'magic number' when attempting to load an image as a different type.
Use :FREE T to automatically free SOURCE."
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
  "Verifies that an image SOURCE is of the image type IMAGE-TYPE, where :IMAGE-TYPE can be one of 
:BMP, :GIF, :JPG, :LBM, :PCX, :PNG, :PNM, :TIF, :XCF, :XPM or :XV. Attempts to detect the image type using the 
'magic number' contained in the image, if one is available. Note: This means that NIL is always returned for images 
of type TGA.
RESULT is IMAGE-TYPE if (eql SOURCE IMAGE-TYPE), or NIL if the image is of a different type.
SOURCE is a foreign type SDL:SDL_RWops."
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
  "Returns the image type found at the location specified by FILENAME and PATHNAME. Attempts to detect the image type 
using the 'magic number' contained in the image, if one is available. 
FILENAME is a STRING.
PATHNAME is a STRING.
RESULT is one of :BMP, :GIF, :JPG, :LBM, :PCX, :PNG, :PNM, :TIF, :XCF, :XPM or :XV, or NIL if the image type 
cannot be determined or if there is no magic number available. 
Note: This means that NIL is always returned for images of type TGA."
  (let ((i-type nil))
    (block image-loop
      (dolist (type '(:BMP :PNM :XPM :XCF :PCX :GIF :JPG :TIF :LBM :PNG :XV))
	(setf i-type (rwops-p source type))
	(if i-type
	    (return-from image-loop))))
    i-type))

(defun image-p (filename pathname image-type)
  "Verifies that the image found at FILENAME and PATHNAME is of the image type :IMAGE-TYPE, where :IMAGE-TYPE can be 
one of :BMP, :GIF, :JPG, :LBM, :PCX, :PNG, :PNM, :TIF, :XCF, :XPM or :XV. Attempts to detect the image type using the 
'magic number' contained in the image, if one is available. Note: This means that NIL is always returned for images 
of type TGA.
RESULT is IMAGE-TYPE if (eql image IMAGE-TYPE), or NIL if the image is of a different type.
FILENAME is a STRING.
PATHNAME is a STRING."
  (let ((rwops (sdl:create-RWops-from-file filename PATHNAME)))
    (when rwops
      (let ((result (rwops-p rwops image-type)))
	(sdl:free-rwops rwops)
	result))))

(defun load-image (filename pathname &key image-type force)
  "Returns a new SDL surface RESULT from the image at location FILENAME and PATHNAME. The 'magic number' contained 
in the image is used to detect the image type, if one is available. 
RESULT is a foreign type SDL:SDL_Surface, or NIL if the image cannot be loaded or the image type cannot be determined.
Note: SDL:SDL_Surface is automatically converted to the SDL screen format.
FILENAME is a STRING.
PATHNAME is a STRING.
:KEY-COLOR sets the key color SDL:COLOR of the image.
:ALPHA-VALUE sets the alpha value of the image, an INTEGER (between 0 and 255).
Use :IMAGE-TYPE to load an image when the 'magic number' is unavailable, where :IMAGE-TYPE is one of 
:BMP, :GIF, :JPG, :LBM, :PCX, :PNG, :PNM, :TGA, :TIF, :XCF, :XPM or :XV. Note: All 'non-magicable' image types, 
such as TGA must have the image type specified using :IMAGE-TYPE.
Use :FORCE T to override the 'magic number' when attempting to load an image as a different type."
  (create-image-from-RWops (sdl:create-RWops-from-file filename pathname)
			   :free t
			   :image-type image-type
			   :force force))
