
(in-package #:lispbuilder-sdl)

(defun write-pixel (x y color &key (surface *default-surface*))
  (funcall (pixel-writer surface) x y (map-color color surface)))
  
(defun read-pixel (x y &key (surface *default-surface*))
  (funcall (pixel-reader surface) x y))

