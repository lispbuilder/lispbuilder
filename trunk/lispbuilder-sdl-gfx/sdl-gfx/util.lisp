
(in-package #:lispbuilder-sdl-gfx)


(defun return-list-for-array (points index-type)
  (case index-type
    (:x (mapcar #'(lambda (point)
		    (sdl:cast-to-int (elt point 0)))
		points))
    (:y (mapcar #'(lambda (point)
		    (sdl:cast-to-int (elt point 1)))
		points))
    (t nil)))
