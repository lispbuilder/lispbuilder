;;;; SDL_ttf CFFI lisp wrapper

(in-package #:lispbuilder-sdl-image)

(defmethod translate-to-foreign (value (type (eql 'image-type)))
  (values (case value
	    (:TGA (cffi:foreign-string-alloc "TGA"))
	    (:BMP (cffi:foreign-string-alloc "BMP"))
	    (:PNM (cffi:foreign-string-alloc "PNM"))
	    (:XPM (cffi:foreign-string-alloc "XPM"))
	    (:XCF (cffi:foreign-string-alloc "XCF"))
	    (:PCX (cffi:foreign-string-alloc "PCX"))
	    (:GIF (cffi:foreign-string-alloc "GIF"))
	    (:JPG (cffi:foreign-string-alloc "JPG"))
	    (:TIF (cffi:foreign-string-alloc "TIF"))
	    (:LBM (cffi:foreign-string-alloc "LBM"))
	    (:PNG (cffi:foreign-string-alloc "PNG"))
	    (otherwise (cffi:foreign-string-alloc "")))
	  t))

(defmethod translate-to-foreign (value (type (eql 'free-src)))
  (if value
      1
      0))

(defmethod free-translated-object (ptr (name (eql 'image-type)) free-p)
  (if free-p
      (cffi:foreign-string-free ptr)))

(defmethod translate-from-foreign (value (type (eql 'image-return-val-0-1)))
  (if (= value 0)
      t
      nil))

(defmethod translate-from-foreign (value (type (eql 'image-return-val-0+1)))
  (if (= value 0)
      nil
      t))
