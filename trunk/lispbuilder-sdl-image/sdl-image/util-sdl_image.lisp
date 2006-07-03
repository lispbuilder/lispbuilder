;; This file contains some useful functions for using SDL_image from Common lisp
;; 2006 (c) Luke Crook, see LICENCE.

(in-package #:lispbuilder-sdl-image)

;;; Macros

;;; Functions

;;; c

(defun create-image-from-RWops (source &key free-source image-type force)
  "create-image-from-RWops source &key free-source image-type force => SDL_Surface 
   Attempts to load an image from an SDL_RWops structure or a filename, if a pathname is given. Returns a foreign pointer
   to an SDL_Surface. Will attempt to automatically detect the type of image, unless :force is T.
   :free-source - T   - Free the SDL_RWops structure in SOURCE.
                  NIL - Do not free the SDL_RWops structure in SOURCE.
   :image-type  - :TGA | :BMP | :PNM | :XPM | :XCF | :PCX | :GIF | :JPG | :TIF | :LBM | :PNG | :XV
   :force       - T   - Force the loading of a specific type of image.
                  NIL - Auto-detect image type.
   NOTE: Due to a lack of a magic number in the TGA file format, load-image cannot auto-detect a TGA image unless 
   loading from a filename.
   To load a TGA image use :image-type :TGA :force t
   
   For example:
   A) To load an image from a filename:
   (load-image \"test.bmp\")
   B) To specify that a PNM image should be loaded from a SDL_RWops structure, freeing the source:
   (load-image *SDL_RWops* :image-type :PNM :free-source t)"
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
	  (if free-source
	      (sdl:SDL_FreeRW source)))
	nil)
    image))

;;; i

(defun is-image-from-RWops (source image-type)
  (if (sdl:is-valid-ptr source)
      (case image-type
	(:BMP (isBMP source))
	(:GIF (isGIF source))
	(:JPG (isJPG source))
	(:LBM (isLBM source))
	(:PCX (isPCX source))
	(:PNG (isPNG source))
	(:PNM (isPNM source))
	(:TIF (isTIF source))
	(:XCF (isXCF source))
	(:XPM (isXPM source))
	(:XV  (isXV  source))
	(otherwise nil))
      nil))

(defun image-type (filename path)
  (let ((i-type nil))
    (block image-loop
      (dolist (type '(:BMP :PNM :XPM :XCF :PCX :GIF :JPG :TIF :LBM :PNG :XV))
	(let ((rwops (sdl:create-RWops-from-file filename path)))
	  (setf i-type (is-image-from-RWops rwops type))
	  (if (sdl:is-valid-ptr rwops)
	      (sdl:SDL_FreeRW rwops))
	  (if i-type
	      (return-from image-loop)))))
    i-type))

(defun is-image (filename path image-type)
  (let* ((rwops (sdl:create-RWops-from-file filename path))
	 (result (is-image-from-RWops rwops image-type)))
    (if (sdl:is-valid-ptr rwops)
	(sdl:SDL_FreeRW rwops))
    result))


;;; l

(defun load-image (filename path &key key-color alpha-value image-type force)
  (sdl:with-surface ((create-image-from-RWops (sdl:create-RWops-from-file filename path)
					      :free-source t
					      :image-type image-type
					      :force force))
    (sdl:convert-surface-to-display-format :key-color key-color :alpha-value alpha-value :free-surface nil)))


