;;;;; Converted from the "Distance 2D" Processing example at:
;;;;; http://www.processing.org/learning/examples/distance2d.html
;;;;; (C)2006 Luke J Crook

(in-package #:sdl-examples) 


(defun when-creating-executables ()
  ;; Make sure the libraries are loaded on startup.
  ;; Necessary when creating a stand-alone executable.
  (cffi:define-foreign-library sdl
    (:darwin (:or (:framework "SDL")
              (:default "libSDL")))
    (:windows "SDL.dll")
    (:unix (:or "libSDL-1.2.so.0.7.2"
            "libSDL-1.2.so.0"
            "libSDL-1.2.so"
            "libSDL.so"
            "libSDL")))
  (cffi:define-foreign-library sdl-glue
    (:windows "lispbuilder-sdl-glue.dll")
    (:darwin (:or (:framework "lispbuilder-sdl-glue")
              (:default "lispbuilder-sdl-glue")))
    (:unix (:or "lispbuilder-sdl-glue"
            "lispbuilder-sdl-glue.so")))
  (cffi:use-foreign-library sdl)
  (cffi:use-foreign-library sdl-glue))

(defun distance-2D ()
  (when-creating-executables)

  (let ((width 200) (height 200)
	(max-distance (sdl:distance #(0 0) #(200 200))))
    (sdl:with-init ()
      (sdl:window width height :title-caption "Distance 2D, from Processing.org")
      (setf (sdl:frame-rate) 30)
      (sdl:with-events ()
        (:quit-event () t)
        (:idle ()
         (sdl:clear-display sdl:*black*)
         ;; We create the rectangle prior to entering the loops
         ;; as a minor optimazation.
         (sdl:with-rectangle (a-rect (sdl:rectangle))
           (loop for i from 20 below width by 20
                 do (loop for j from 20 below height by 20
                          do (let ((size (* (/ (sdl:distance (sdl:mouse-position) (vector i j))
                                               max-distance)
                                            66)))
                               (sdl:draw-box (sdl:rectangle-from-midpoint-*
                                              i j
                                              size size
                                              a-rect)
                                             :color sdl:*white*)))))
         (sdl:update-display))))))
