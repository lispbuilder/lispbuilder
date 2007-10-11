
(in-package #:lispbuilder-sdl)

;; (defun write-pixel (x y color &key (surface *default-surface*) (raw-color nil))
;;   (funcall (pixel-writer surface) x y (if raw-color
;; 					  color
;; 					  (map-color color surface))))
  
;; (defun read-pixel (x y &key (surface *default-surface*))
;;   (funcall (pixel-reader surface) x y))

(defun bit-depth (surface)
  "Returns the bit depth (the number of bytes per pixel, or bpp) of the surface `SURFACE` as an `INTEGER`."
  (check-type surface sdl-surface)
  (foreign-slot-value (sdl-base::pixel-format (fp surface)) 'sdl-cffi::SDL-Pixel-Format 'sdl-cffi::BytesPerPixel))

