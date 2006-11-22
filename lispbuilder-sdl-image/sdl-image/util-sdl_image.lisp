;; This file contains some useful functions for using SDL_image from Common lisp
;; 2006 (c) Luke Crook, see LICENCE.

(in-package #:lispbuilder-sdl-image)

;;; Macros

;;; Functions

;;; c

(defun create-image-from-RWops (source &key image-type force free)
  "Returns a new SDL surface RESULT from the image SOURCE. The 'magic number' contained in the image SOURCE 
is used to detect the image type, if one is available. 
RESULT is a foreign type SDL:SDL_Surface, or NIL if SOURCE does not contain a valid image.
SOURCE is a foreign type SDL:SDL_RWop.
Use :IMAGE-TYPE to load an image when the 'magic number' is unavailable, where :IMAGE-TYPE is one of 
:BMP, :GIF, :JPG, :LBM, :PCX, :PNG, :PNM, :TGA, :TIF, :XCF, :XPM or :XV. Note: All 'non-magicable' image types, 
such as TGA must have the image type specified using :IMAGE-TYPE.
Use :FORCE T to override the 'magic number' when attempting to load an image as a different type.
Use :FREE T to automatically free SOURCE."
  (let ((image nil))
    (if (sdl:is-valid-ptr source)
	(progn
	  (setf image (if image-type
			  (if force
			      (case image-type
				(:BMP (Load-BMP-RW source))
				(:GIF (Load-GIF-RW source))
				(:JPG (Load-JPG-RW source))
				(:LBM (Load-LBM-RW source))
				(:PCX (Load-PCX-RW source))
				(:PNG (Load-PNG-RW source))
				(:PNM (Load-PNM-RW source))
				(:TGA (Load-TGA-RW source))
				(:TIF (Load-TIF-RW source))
				(:XCF (Load-XCF-RW source))
				(:XPM (Load-XPM-RW source))
				(:XV  (Load-XV-RW  source)))
			      (load-typed-rw source nil image-type))
			  (Load-RW source nil)))
	  (if free
	      (sdl:SDL_FreeRW source)))
	nil)
    image))

;;; i

(defun is-image-from-RWops (source image-type)
 "Verifies that an image SOURCE is of the image type IMAGE-TYPE, where :IMAGE-TYPE can be one of 
:BMP, :GIF, :JPG, :LBM, :PCX, :PNG, :PNM, :TIF, :XCF, :XPM or :XV. Attempts to detect the image type using the 
'magic number' contained in the image, if one is available. Note: This means that NIL is always returned for images 
of type TGA.
RESULT is IMAGE-TYPE if (eql SOURCE IMAGE-TYPE), or NIL if the image is of a different type.
SOURCE is a foreign type SDL:SDL_RWops."
  (if (sdl:is-valid-ptr source)
      (case image-type
	(:BMP (if (isBMP source) 
		  :BMP))
	(:GIF (if (isGIF source)
		  :GIF))
	(:JPG (if (isJPG source)
		  :JPG))
	(:LBM (if (isLBM source)
		  :LBM))
	(:PCX (if (isPCX source)
		  :PCX))
	(:PNG (if (isPNG source)
		  :PNG))
	(:PNM (if (isPNM source)
		  :PNM))
	(:TIF (if (isTIF source)
		  :TIF))
	(:XCF (if (isXCF source)
		  :XCF))
	(:XPM (if (isXPM source)
		  :XPM))
	(:XV  (if (isXV  source)
		  :XV))
	(otherwise nil))
      nil))

(defun image-type (filename pathname)
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
	(let ((rwops (sdl:create-RWops-from-file filename PATHNAME)))
	  (setf i-type (is-image-from-RWops rwops type))
	  (if (sdl:is-valid-ptr rwops)
	      (sdl:SDL_FreeRW rwops))
	  (if i-type
	      (return-from image-loop)))))
    i-type))

(defun is-image (filename pathname image-type)
  "Verifies that the image found at FILENAME and PATHNAME is of the image type :IMAGE-TYPE, where :IMAGE-TYPE can be 
one of :BMP, :GIF, :JPG, :LBM, :PCX, :PNG, :PNM, :TIF, :XCF, :XPM or :XV. Attempts to detect the image type using the 
'magic number' contained in the image, if one is available. Note: This means that NIL is always returned for images 
of type TGA.
RESULT is IMAGE-TYPE if (eql image IMAGE-TYPE), or NIL if the image is of a different type.
FILENAME is a STRING.
PATHNAME is a STRING."
  (let* ((rwops (sdl:create-RWops-from-file filename PATHNAME))
	 (result (is-image-from-RWops rwops image-type)))
    (if (sdl:is-valid-ptr rwops)
	(sdl:SDL_FreeRW rwops))
    result))

;;; l

(defun load-image (filename pathname &key key-color alpha-value image-type force)
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
  (sdl:with-surface ((create-image-from-RWops (sdl:create-RWops-from-file filename pathname)
					      :free t
					      :image-type image-type
					      :force force))
    (sdl:convert-surface-to-display-format :key-color key-color :alpha-value alpha-value :free-p nil)))
