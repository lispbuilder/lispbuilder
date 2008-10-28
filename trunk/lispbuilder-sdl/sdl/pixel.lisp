
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
