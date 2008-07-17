
(in-package #:lispbuilder-sdl)
                         
(defmethod load-image (filename &key key-color surface-alpha image-type force free (key-color-at nil))
  (declare (ignore image-type force free))
  (let ((surf (make-instance 'surface
			     :surface (sdl-base::load-image (namestring filename))
			     :key-color key-color
			     :surface-alpha surface-alpha)))
    (when key-color-at
      (set-color-key (read-pixel key-color-at :surface surf) :surface surf))
    surf))

;; (defmethod load-image ((source sdl:rwops) &key key-color surface-alpha image-type force (free t) (key-color-at nil))
;;   "Creates and returns a new surface from the image contained in the `RWOPS` structure in the source `SOURCE`.

;; ##### Parameters

;; * `SOURCE` is of type `RWOPS`.
;; * `FREE` when `T` will automatically free the `RWOPS` in `SOURCE`."
;;   (let ((image nil))
;;     (setf image (if image-type
;; 		    (if force
;; 			(case image-type
;; 			  (:BMP (sdl-image-cffi::img-Load-BMP-RW (sdl:fp source)))
;; 			  (:GIF (sdl-image-cffi::img-Load-GIF-RW (sdl:fp source)))
;; 			  (:JPG (sdl-image-cffi::img-Load-JPG-RW (sdl:fp source)))
;; 			  (:LBM (sdl-image-cffi::img-Load-LBM-RW (sdl:fp source)))
;; 			  (:PCX (sdl-image-cffi::img-Load-PCX-RW (sdl:fp source)))
;; 			  (:PNG (sdl-image-cffi::img-Load-PNG-RW (sdl:fp source)))
;; 			  (:PNM (sdl-image-cffi::img-Load-PNM-RW (sdl:fp source)))
;; 			  (:TGA (sdl-image-cffi::img-Load-TGA-RW (sdl:fp source)))
;; 			  (:TIF (sdl-image-cffi::img-Load-TIF-RW (sdl:fp source)))
;; 			  (:XCF (sdl-image-cffi::img-Load-XCF-RW (sdl:fp source)))
;; 			  (:XPM (sdl-image-cffi::img-Load-XPM-RW (sdl:fp source)))
;; 			  (:XV  (sdl-image-cffi::img-Load-XV-RW  (sdl:fp source))))
;; 			(sdl-image-cffi::img-load-typed-rw (sdl:fp source) nil image-type))
;; 		    (sdl-image-cffi::img-Load-RW (sdl:fp source) nil)))
;;     (when free
;;       (sdl:free-rwops source))
;;     (when (sdl-base::is-valid-ptr image)
;;       (let ((surface (make-instance 'sdl:surface :surface image :key-color key-color :surface-alpha surface-alpha)))
;; 	(when key-color-at
;; 	  (sdl:set-color-key (sdl:read-pixel key-color-at :surface surface) :surface surface))
;; 	surface))))

(defun save-image (surface filename)
  "Saves the surface `SURFACE` as a BMP image to a file at location `FILENAME`."
  (check-type surface sdl-surface)
  (let ((file (namestring filename)))
    (sdl-cffi::SDL-Save-BMP-RW (fp surface) (sdl-cffi::SDL-RW-FROM-FILE file "wb") 1)))

