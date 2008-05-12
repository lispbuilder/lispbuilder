;; This file contains some useful functions for using SDL_image from Common lisp
;; 2006 (c) Luke Crook, see LICENCE.

(in-package #:lispbuilder-sdl-image)

(defmethod image-p ((source sdl:rwops) image-type)
  "Returns `T` when the type of image contained in the `RWOPS` in the source `SOURCE` is of the type `IMAGE-TYPE`.

##### Parameters

* `SOURCE` is of type `RWOPS`.
* `IMAGE-TYPE` must be one of `:BMP`, `:GIF`, `:JPG`, `:LBM`, `:PCX`, `:PNG`, `:PNM`, `:TIF`, `:XCF`, `:XPM` or `:XV`."
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

(defmethod image-p (source image-type)
  "Returns `T` if the image in the file at location `SOURCE` is of type `IMAGE-TYPE`.

##### Parameters

* `SOURCE` is the filename and path of the file on the drive of type `STRING`. 
* `IMAGE-TYPE` can be one of `:BMP`, `:GIF`, `:JPG`, `:LBM`, `:PCX`, `:PNG`, `:PNM`, `:TIF`, `:XCF`, `:XPM` or `:XV`. 

##### Examples

    \(IMAGE-P \"image.bmp\" :IMAGE-TYPE :BMP\)"
  (let ((rwops (sdl:create-RWops-from-file source)))
    (when rwops
      (let ((result (image-p rwops image-type)))
	(sdl:free-rwops rwops)
	result))))

(defmethod image-type-of ((source sdl:rwops))
  "Returns the type of image contained in the `RWOPS` in the source `SOURCE`.

##### Parameters

* `SOURCE` is of type `RWOPS`.

##### Example

    \(IMAGE-TYPE-OF SOURCE\)"
  (let ((i-type nil))
    (block image-loop
      (dolist (type '(:BMP :PNM :XPM :XCF :PCX :GIF :JPG :TIF :LBM :PNG :XV))
	(setf i-type (image-p source type))
	(if i-type
	    (return-from image-loop))))
    i-type))


(defmethod image-type-of (source)
  "Returns the type of image in the file at location `SOURCE`.

##### Parameters

* `SOURCE` is the filename and path of the file on the drive of type `STRING`. 

##### Example

    \(IMAGE-TYPE-OF \"image.bmp\"\)"
  (let ((rwops (sdl:create-RWops-from-file source)))
    (when rwops
      (let ((result (image-type-of rwops)))
	(sdl:free-rwops rwops)
	result))))


(defmethod load-image ((source sdl:rwops) &key key-color alpha-value (image-type nil) (force nil) (free t) (key-color-at nil))
  "Creates and returns a new surface from the image contained in the `RWOPS` structure in the source `SOURCE`.

##### Parameters

* `SOURCE` is of type `RWOPS`.
* `IMAGE-TYPE` when specified type may be one of `NIL`, `:BMP`, `:GIF`, `:JPG`, `:LBM`, `:PCX`, `:PNG`, `:PNM`, `:TGA`, `:TIF`, `:XCF`, `:XPM` or `:XV`. 
* `FORCE` when `T` will force an image to be loaded as `IMAGE-TYPE`, ignoring any *magic number* when present in `SOURCE`. 
* `FREE` when `T` will automatically free the `RWOPS` in `SOURCE`."
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
      (let ((surface (sdl:surface image)))
	(when surface
	  (if key-color-at
	      (sdl:set-color-key (sdl:read-pixel key-color-at :surface surface) :surface surface)
	      (when key-color (sdl:set-color-key key-color :surface surface)))
	  (when alpha-value (sdl:set-alpha alpha-value :surface surface))
	  surface)))))

(defmethod load-image (source &key key-color alpha-value (image-type nil) (force nil) (free nil) (key-color-at nil))
  "Creates and returns a new surface from the image in the file at the location `SOURCE`. 

##### Parameters

* `SOURCE` is the filename and path of the file on the drive of type `STRING`. 
* `IMAGE-TYPE` when specified type may be one of `NIL`, `:BMP`, `:GIF`, `:JPG`, `:LBM`, `:PCX`, `:PNG`, `:PNM`, `:TGA`, `:TIF`, `:XCF`, `:XPM` or `:XV`. 
* `FORCE` when `T` will force an image to be loaded as `IMAGE-TYPE`, ignoring any *magic number* when present in `SOURCE`. 
* `FREE` is only used when `SOURCE` is `RWOPS`.

##### Example

* To load a `BMP` image using the *magic number*

    \(LOAD-IMAGE \"image.bmp\"\)
    
* To load a `TGA` image

    \(LOAD-IMAGE \"image.tga\" :IMAGE-TYPE :TGA\)
    
* To load a `BMP` image as `TGA`

    \(LOAD-IMAGE \"image.bmp\" :IMAGE-TYPE :BMP :FORCE T\)"
  (declare (ignore free))
  (let ((rwops (sdl:create-RWops-from-file source)))
    (when rwops
      (let ((surface (load-image rwops
				 :key-color key-color
				 :key-color-at key-color-at
				 :alpha-value alpha-value
				 :free t
				 :image-type image-type
				 :force force)))
	(if surface
	    surface
	    (error "ERROR, LOAD-IMAGE: file ~A, not found" source))))))

(defun load-and-convert-image (source &rest named-pairs &key image-type force &allow-other-keys)
  "Loads an image from the filename `SOURCE` as per [LOAD-IMAGE-*](#load-image-*), 
 converts this image to the current display format using `CONVERT-SURFACE`. 

Parameters supported are the same as those for [LOAD-IMAGE](#load-image) and `CONVERT-IMAGE`. "
  (apply #'sdl:convert-surface
	 :surface (load-image source
			      :free t
			      :image-type image-type
			      :force force)
	 :free-p t
	 :allow-other-keys t
	 named-pairs))
