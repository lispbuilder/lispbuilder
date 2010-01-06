
(in-package #:lispbuilder-sdl)

;; (defun write-pixel (x y color &key (surface *default-surface*) (raw-color nil))
;;   (funcall (pixel-writer surface) x y (if raw-color
;; 					  color
;; 					  (map-color color surface))))
  
;; (defun read-pixel (x y &key (surface *default-surface*))
;;   (funcall (pixel-reader surface) x y))

(defun bit-depth (&optional (surface *default-surface*))
  "Returns the number of bytes per pixel, or bpp, of `SURFACE`."
  (check-type surface sdl-surface)
  (with-foreign-slots ((sdl-cffi::BitsPerPixel)
		       (sdl-base:pixel-format (fp surface))
		       sdl-cffi::SDL-Pixel-Format)
    sdl-cffi::BitsPerPixel))

(defun byte-depth (&optional (surface *default-surface*))
  "Returns the number of bytes per pixel, or bpp, of `SURFACE`."
  (check-type surface sdl-surface)
  (with-foreign-slots ((sdl-cffi::BytesPerPixel)
		       (sdl-base:pixel-format (fp surface))
		       sdl-cffi::SDL-Pixel-Format)
    sdl-cffi::BytesPerPixel))

(defun r-mask (&optional (surface *default-surface*))
  "Returns the red mask of `SURFACE`."
  (check-type surface sdl-surface)
  (with-foreign-slots ((sdl-cffi::rmask)
                       (sdl-base:pixel-format (fp surface))
		       sdl-cffi::SDL-Pixel-Format)
    sdl-cffi::rmask))

(defun g-mask (&optional (surface *default-surface*))
  "Returns the green mask of `SURFACE`."
  (check-type surface sdl-surface)
  (with-foreign-slots ((sdl-cffi::gmask)
                       (sdl-base:pixel-format (fp surface))
		       sdl-cffi::SDL-Pixel-Format)
    sdl-cffi::gmask))

(defun b-mask (&optional (surface *default-surface*))
  "Returns the blue mask of `SURFACE`."
  (check-type surface sdl-surface)
  (with-foreign-slots ((sdl-cffi::bmask)
                       (sdl-base:pixel-format (fp surface))
		       sdl-cffi::SDL-Pixel-Format)
    sdl-cffi::bmask))

(defun a-mask (&optional (surface *default-surface*))
  "Returns the alpha mask of `SURFAaCE`."
  (check-type surface sdl-surface)
  (with-foreign-slots ((sdl-cffi::amask)
                       (sdl-base:pixel-format (fp surface))
		       sdl-cffi::SDL-Pixel-Format)
    sdl-cffi::amask))
