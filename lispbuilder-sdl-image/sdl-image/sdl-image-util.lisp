;; This file contains some useful functions for using SDL_image from Common lisp
;; 2006 (c) Luke Crook, see LICENCE.

(in-package #:lispbuilder-sdl)

(defmethod image-p ((source rwops) image-type)
  "Returns `T` when the type of image contained in the `RWOPS` in the source `SOURCE` is of the type `IMAGE-TYPE`.

##### Parameters

* `SOURCE` is of type `RWOPS`.
* `IMAGE-TYPE` must be one of `:BMP`, `:GIF`, `:JPG`, `:LBM`, `:PCX`, `:PNG`, `:PNM`, `:TIF`, `:XCF`, `:XPM` or `:XV`."
  (case image-type
    (:BMP (if (sdl-image-cffi::img-is-BMP (fp source)) 
	      :BMP))
    (:GIF (if (sdl-image-cffi::img-is-GIF (fp source))
	      :GIF))
    (:JPG (if (sdl-image-cffi::img-is-JPG (fp source))
	      :JPG))
    (:LBM (if (sdl-image-cffi::img-is-LBM (fp source))
	      :LBM))
    (:PCX (if (sdl-image-cffi::img-is-PCX (fp source))
	      :PCX))
    (:PNG (if (sdl-image-cffi::img-is-PNG (fp source))
	      :PNG))
    (:PNM (if (sdl-image-cffi::img-is-PNM (fp source))
	      :PNM))
    (:TIF (if (sdl-image-cffi::img-is-TIF (fp source))
	      :TIF))
    (:XCF (if (sdl-image-cffi::img-is-XCF (fp source))
	      :XCF))
    (:XPM (if (sdl-image-cffi::img-is-XPM (fp source))
	      :XPM))
    (:XV  (if (sdl-image-cffi::img-is-XV  (fp source))
	      :XV))
    (otherwise nil)))

(defmethod image-p ((source string) image-type)
  "Returns `T` if the image in the file at location `SOURCE` is of type `IMAGE-TYPE`.

##### Parameters

* `SOURCE` is the filename and path of the file on the drive of type `STRING`. 
* `IMAGE-TYPE` can be one of `:BMP`, `:GIF`, `:JPG`, `:LBM`, `:PCX`, `:PNG`, `:PNM`, `:TIF`, `:XCF`, `:XPM` or `:XV`. 

##### Examples

    \(IMAGE-P \"image.bmp\" :IMAGE-TYPE :BMP\)"
  (let ((rwops (create-RWops-from-file source)))
    (when rwops
      (let ((result (image-p rwops image-type)))
	(free-rwops rwops)
	result))))

(defmethod image-type-of ((source rwops))
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


(defmethod image-type-of ((source string))
  "Returns the type of image in the file at location `SOURCE`.

##### Parameters

* `SOURCE` is the filename and path of the file on the drive of type `STRING`. 

##### Example

    \(IMAGE-TYPE-OF \"image.bmp\"\)"
  (let ((rwops (create-RWops-from-file source)))
    (when rwops
      (let ((result (image-type-of rwops)))
	(free-rwops rwops)
	result))))


(defmethod load-image ((source rwops) &key key-color alpha-value (image-type nil) (force nil) (free t) (key-color-at nil))
  "Creates and returns a new surface from the image contained in the `RWOPS` structure in the source `SOURCE`.

##### Parameters

* `SOURCE` is of type `RWOPS`.
* `IMAGE-TYPE` when specified type may be one of `NIL`, `:BMP`, `:GIF`, `:JPG`, `:LBM`, `:PCX`, `:PNG`, `:PNM`, `:TGA`, `:TIF`, `:XCF`, `:XPM` or `:XV`. 
* `FORCE` when `T` will force an image to be loaded as `IMAGE-TYPE`, ignoring any *magic number* when present in `SOURCE`. 
* `FREE` when `T` will automatically free the `RWOPS` in `SOURCE`."
  (check-type source rwops)
  (let ((image nil))
    (setf image (if image-type
		    (if force
			(case image-type
			  (:BMP (sdl-image-cffi::img-Load-BMP-RW (fp source)))
			  (:GIF (sdl-image-cffi::img-Load-GIF-RW (fp source)))
			  (:JPG (sdl-image-cffi::img-Load-JPG-RW (fp source)))
			  (:LBM (sdl-image-cffi::img-Load-LBM-RW (fp source)))
			  (:PCX (sdl-image-cffi::img-Load-PCX-RW (fp source)))
			  (:PNG (sdl-image-cffi::img-Load-PNG-RW (fp source)))
			  (:PNM (sdl-image-cffi::img-Load-PNM-RW (fp source)))
			  (:TGA (sdl-image-cffi::img-Load-TGA-RW (fp source)))
			  (:TIF (sdl-image-cffi::img-Load-TIF-RW (fp source)))
			  (:XCF (sdl-image-cffi::img-Load-XCF-RW (fp source)))
			  (:XPM (sdl-image-cffi::img-Load-XPM-RW (fp source)))
			  (:XV  (sdl-image-cffi::img-Load-XV-RW  (fp source))))
			(sdl-image-cffi::img-load-typed-rw (fp source) nil image-type))
		    (sdl-image-cffi::img-Load-RW (fp source) nil)))
    (if free
	(free-rwops source))
    (when (sdl-base::is-valid-ptr image)
      (let ((surface (surface image)))
	(when surface
	  (if key-color-at
	      (set-color-key (read-pixel key-color-at :surface surface) :surface surface)
	      (when key-color (set-color-key key-color :surface surface)))
	  (when alpha-value (set-alpha alpha-value :surface surface))
	  surface)))))


(defmethod load-image :around ((source string) &key key-color alpha-value (image-type nil) (force nil) (free nil) (key-color-at nil))
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
  (let ((rwops (create-RWops-from-file source)))
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
  (apply #'convert-surface
	 :surface (load-image source
			      :free t
			      :image-type image-type
			      :force force)
	 :free-p t
	 :allow-other-keys t
	 named-pairs))
