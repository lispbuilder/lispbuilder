;; This file contains some useful functions for using SDL_image from Common lisp
;; 2006 (c) Luke Crook, see LICENCE.

(in-package #:lispbuilder-sdl-image)

;;; Macros

;;; Functions

;;; l

(defun load-image (source &key free-source image-type force)
  "load-image source &key free-source image-type force => SDL_Surface"
  "Attempts to load an image from an SDL_RWops structure or a filename, if a pathname is given. Returns a foreign pointer"
  "to an SDL_Surface. Will attempt to automatically detect the type of image, unless :force is T."
  ":free-source - T   - Free the SDL_RWops structure in SOURCE."
  "               NIL - Do not free the SDL_RWops structure in SOURCE."
  ":image-type  - :TGA | :BMP | :PNM | :XPM | :XCF | :PCX | :GIF | :JPG | :TIF | :LBM | :PNG | :XV"
  ":force       - T   - Force the loading of a specific type of image."
  "               NIL - Auto-detect image type."
  "NOTE: Due to a lack of a magic number in the TGA file format, load-image cannot auto-detect a TGA image unless "
  "loading from a filename."
  "To load a TGA image use :image-type :TGA :force t"
  ""
  "For example:"
  "A) To load an image from a filename:"
  "(load-image \"test.bmp\")"
  "B) To specify that a PNM image should be loaded from a SDL_RWops structure, freeing the source:"
  "(load-image *SDL_RWops* :image-type :PNM :free-source t)"
  (cond
    ((sdl:is-valid-ptr source)
     (if image-type
	 (if force
	     (case force
	       (:TGA (Load-TGA-RW source))
	       (:BMP (Load-BMP-RW source))
	       (:PNM (Load-PNM-RW source))
	       (:XPM (Load-XPM-RW source))
	       (:XCF (Load-XCF-RW source))
	       (:PCX (Load-PCX-RW source))
	       (:GIF (Load-GIF-RW source))
	       (:JPG (Load-JPG-RW source))
	       (:TIF (Load-TIF-RW source))
	       (:LBM (Load-LBM-RW source))
	       (:PNG (Load-PNG-RW source))
	       (:XV  (Load-XV-RW  source)))
	     (load-typed-rw source free-source image-type))
	 (Load-RW source free-source)))
    ((and (stringp source) (probe-file source))
     (load-img source))
    (t
     nil)))

;;; i

(defun is-image (source image-type)
  (case image-type
    (:BMP (isBMP source))
    (:PNM (isPNM source))
    (:XPM (isXPM source))
    (:XCF (isXCF source))
    (:PCX (isPCX source))
    (:GIF (isGIF source))
    (:JPG (isJPG source))
    (:TIF (isTIF source))
    (:LBM (isLBM source))
    (:PNG (isPNG source))
    (:XV  (isXV  source))
    (otherwise nil)))

(defun image-type (source)
  (let ((i-type nil))
    (block image-loop
      (dolist (type '(:BMP :PNM :XPM :XCF :PCX :GIF :JPG :TIF :LBM :PNG :XV))
	(setf i-type (is-image source type))
	(if i-type
	    (return-from image-loop))))
    i-type))

