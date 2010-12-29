;; SDL (Simple Media Layer) library using CFFI for foreign function interfacing...
;; (C)2006 Justin Heyes-Jones <justinhj@gmail.com> and Luke Crook <luke@balooga.com>
;; Thanks to Frank Buss and Surendra Singh
;; see COPYING for license
;; This file contains some useful functions for using SDL from Common lisp
;; using sdl.lisp (the CFFI wrapper)

(in-package #:lispbuilder-sdl)

(defun surface-pointer-function (surface) (fp surface))
(defun return-x-function (pos) (x pos))
(defun return-y-function (pos) (y pos))
(defun return-r-function (color) (r color))
(defun return-g-function (color) (g color))
(defun return-b-function (color) (b color))
(defun return-a-function (color) (a color))
(defun return-packed-color-function (color) (pack-color color))
(defun return-true-to-1-function (value) (if value 1 0))
(defun return-int-to-double-function (value) (coerce value 'double-float))

(cffi:defctype surface-pointer (:wrapper :pointer :to-c surface-pointer-function));;#'(lambda (surface) (fp surface))))
(cffi:defctype return-x (:wrapper :int16 :to-c return-x-function));;#'(lambda (pos) (x pos))))
(cffi:defctype return-y (:wrapper :int16 :to-c return-y-function));;#'(lambda (pos) (y pos))))
(cffi:defctype return-r (:wrapper :uint8 :to-c return-r-function));;#'(lambda (color) (r color))))
(cffi:defctype return-g (:wrapper :uint8 :to-c return-g-function));;#'(lambda (color) (g color))))
(cffi:defctype return-b (:wrapper :uint8 :to-c return-b-function));;#'(lambda (color) (b color))))
(cffi:defctype return-a (:wrapper :uint8 :to-c return-a-function));;#'(lambda (color) (a color))))
(cffi:defctype return-packed-color (:wrapper :uint32 :to-c return-packed-color-function));;#'(lambda (color) (pack-color color))))

(cffi:defctype return-true-to-1 (:wrapper :int :to-c return-true-to-1-function));;#'(lambda (value) (if value 1 0))))
(cffi:defctype return-int-to-double (:wrapper :double :to-c return-int-to-double-function));;#'(lambda (value) (coerce value 'double-float))))

(defun return-list-for-array (points index-type)
  (case index-type
    (:x (mapcar #'(lambda (point)
		    (sdl:cast-to-int (elt point 0)))
		points))
    (:y (mapcar #'(lambda (point)
		    (sdl:cast-to-int (elt point 1)))
		points))
    (t nil)))

(defconstant FPS-UPPER-LIMIT 200)

(defconstant FPS-LOWER-LIMIT 1)

(defconstant FPS-DEFAULT 30)

(defconstant M-PI 3.141592654)

(defconstant SMOOTHING-OFF 0)

(defconstant SMOOTHING-ON 1)

(cffi:defcstruct t-Color-RGBA
  (r :uint8)
  (g :uint8)
  (b :uint8)
  (a :uint8))

(cffi:defcstruct t-Color-Y
  (y :uint8))

(cffi:defctype sdl-gfx-font-data :pointer)

(cffi:defcstruct FPS-manager
  (framecount :uint32)
  (rateticks :float)
  (lastticks :uint32)
  (rate :uint32))

(defun gfx-init-frame-rate (fps-manager)
  (cffi:foreign-funcall-pointer (cffi:foreign-symbol-pointer "SDL_initFramerate" :library 'sdl-cffi::sdl-gfx)
                                () 
                                :pointer fps-manager
                                :void))

(defun gfx-set-frame-rate (fps-manager rate)
  (cffi:foreign-funcall-pointer (cffi:foreign-symbol-pointer "SDL_setFramerate" :library 'sdl-cffi::sdl-gfx)
                                () 
                                :pointer fps-manager
                                :int rate
                                :int))

(defun gfx-get-frame-rate (fps-manager)
  (cffi:foreign-funcall-pointer (cffi:foreign-symbol-pointer "SDL_getFramerate" :library 'sdl-cffi::sdl-gfx)
                                () 
                                :pointer fps-manager
                                :int))

(defun gfx-frame-rate-delay (fps-manager)
  (cffi:foreign-funcall-pointer (cffi:foreign-symbol-pointer "SDL_framerateDelay" :library 'sdl-cffi::sdl-gfx)
                                () 
                                :pointer fps-manager
                                :void))

(defun gfx-draw-aa-line-* (x0 y0 x1 y1 &key (surface sdl:*default-surface*) (color sdl:*default-color*))
  (unless surface
    (setf surface sdl:*default-display*))
  (check-type surface sdl:sdl-surface)
  (check-type color sdl:color)
  (if (sdl:a color)
    (cffi:foreign-funcall-pointer (cffi:foreign-symbol-pointer "aalineRGBA" :library 'sdl-cffi::sdl-gfx)
                                  () 
                                  surface-pointer surface
                                  :int16 x0
                                  :int16 y0
                                  :int16 x1
                                  :int16 y1
                                  return-r color
                                  return-g color
                                  return-b color
                                  return-a color
                                  :int)
      (cffi:foreign-funcall-pointer (cffi:foreign-symbol-pointer "aalineColor" :library 'sdl-cffi::sdl-gfx)
                                  () 
                                  surface-pointer surface
                                  :int16 x0
                                  :int16 y0
                                  :int16 x1
                                  :int16 y1
                                  return-packed-color color
                                  :int)))

(defun gfx-draw-line-* (x0 y0 x1 y1 &key (surface *default-surface*) (color *default-color*) (aa nil))
  (unless surface
    (setf surface *default-display*))
  (check-type surface sdl-surface)
  (check-type color color)
  (if aa
    (gfx-draw-aa-line-* x0 y0 x1 y1 :surface surface :color color)
    (if (a color)
      (cffi:foreign-funcall-pointer (cffi:foreign-symbol-pointer "lineRGBA" :library 'sdl-cffi::sdl-gfx)
                                    () 
                                    surface-pointer surface
                                    :int16 x0
                                    :int16 y0
                                    :int16 x1
                                    :int16 y1
                                    return-r color
                                    return-g color
                                    return-b color
                                    return-a color
                                    :int)
      (cffi:foreign-funcall-pointer (cffi:foreign-symbol-pointer "lineColor" :library 'sdl-cffi::sdl-gfx)
                                    () 
                                    surface-pointer surface
                                    :int16 x0
                                    :int16 y0
                                    :int16 x1
                                    :int16 y1
                                    return-packed-color color
                                    :int))))

(defun gfx-draw-pixel-* (x y &key (surface *default-surface*) (color *default-color*))
  (unless surface
    (setf surface *default-display*))
  (check-type surface sdl-surface)
  (check-type color color)
  (if (a color)
    (cffi:foreign-funcall-pointer (cffi:foreign-symbol-pointer "pixelRGBA" :library 'sdl-cffi::sdl-gfx)
                                  () 
                                  surface-pointer surface
                                  :int16 x
                                  :int16 y
                                  return-r color
                                  return-g color
                                  return-b color
                                  return-a color
                                  :int)
    (cffi:foreign-funcall-pointer (cffi:foreign-symbol-pointer "pixelColor" :library 'sdl-cffi::sdl-gfx)
                                  () 
                                  surface-pointer surface
                                  :int16 x
                                  :int16 y
                                  return-packed-color color
                                  :int)))

(defun gfx-draw-hline (x0 x1 y &key (surface sdl:*default-surface*) (color sdl:*default-color*))
  (unless surface
    (setf surface sdl:*default-display*))
  (check-type surface sdl:sdl-surface)
  (check-type color sdl:color)
  (if (sdl:a color)
    (cffi:foreign-funcall-pointer (cffi:foreign-symbol-pointer "hlineRGBA" :library 'sdl-cffi::sdl-gfx)
                                  () 
                                  surface-pointer surface
                                  :int16 x0
                                  :int16 x1
                                  :int16 y
                                  return-r color
                                  return-g color
                                  return-b color
                                  return-a color
                                  :int)
    (cffi:foreign-funcall-pointer (cffi:foreign-symbol-pointer "hlineColor" :library 'sdl-cffi::sdl-gfx)
                                  () 
                                  surface-pointer surface
                                  :int16 x0
                                  :int16 x1
                                  :int16 y
                                  return-packed-color color
                                  :int)))

(defun gfx-draw-vline (x y0 y1 &key (surface sdl:*default-surface*) (color sdl:*default-color*))
  (unless surface
    (setf surface sdl:*default-display*))
  (check-type surface sdl:sdl-surface)
  (check-type color sdl:color)
  (if (sdl:a color)
    (cffi:foreign-funcall-pointer (cffi:foreign-symbol-pointer "vlineRGBA" :library 'sdl-cffi::sdl-gfx)
                                  () 
                                  surface-pointer surface
                                  :int16 x
                                  :int16 y0
                                  :int16 y1
                                  return-r color
                                  return-g color
                                  return-b color
                                  return-a color
                                  :int)
    (cffi:foreign-funcall-pointer (cffi:foreign-symbol-pointer "vlineColor" :library 'sdl-cffi::sdl-gfx)
                                  () 
                                  surface-pointer surface
                                  :int16 x
                                  :int16 y0
                                  :int16 y1
                                  return-packed-color color
                                  :int)))

(defun gfx-draw-rectangle-edges-* (x0 y0 x1 y1 &key (surface sdl:*default-surface*) (color sdl:*default-color*))
  (unless surface
    (setf surface sdl:*default-display*))
  (check-type surface sdl:sdl-surface)
  (check-type color sdl:color)
  (if (sdl:a color)
    (cffi:foreign-funcall-pointer (cffi:foreign-symbol-pointer "rectangleRGBA" :library 'sdl-cffi::sdl-gfx)
                                  () 
                                  surface-pointer surface
                                  :int16 x0
                                  :int16 y0
                                  :int16 x1
                                  :int16 y1
                                  return-r color
                                  return-g color
                                  return-b color
                                  return-a color
                                  :int)
    (cffi:foreign-funcall-pointer (cffi:foreign-symbol-pointer "rectangleColor" :library 'sdl-cffi::sdl-gfx)
                                  () 
                                  surface-pointer surface
                                  :int16 x0
                                  :int16 y0
                                  :int16 x1
                                  :int16 y1
                                  return-packed-color color
                                  :int)))

(defun gfx-draw-box-edges-* (x0 y0 x1 y1 &key (surface sdl:*default-surface*) (color sdl:*default-color*))
  (unless surface
    (setf surface sdl:*default-display*))
  (check-type surface sdl:sdl-surface)
  (check-type color sdl:color)
  (if (sdl:a color)
    (cffi:foreign-funcall-pointer (cffi:foreign-symbol-pointer "boxRGBA" :library 'sdl-cffi::sdl-gfx)
                                  () 
                                  surface-pointer surface
                                  :int16 x0
                                  :int16 y0
                                  :int16 x1
                                  :int16 y1
                                  return-r color
                                  return-g color
                                  return-b color
                                  return-a color
                                  :int)
    (cffi:foreign-funcall-pointer (cffi:foreign-symbol-pointer "boxColor" :library 'sdl-cffi::sdl-gfx)
                                  () 
                                  surface-pointer surface
                                  :int16 x0
                                  :int16 y0
                                  :int16 x1
                                  :int16 y1
                                  return-packed-color color
                                  :int)))

(defun gfx-draw-aa-circle-* (x y r &key (surface sdl:*default-surface*) (color sdl:*default-color*))
  (unless surface
    (setf surface sdl:*default-display*))
  (check-type surface sdl:sdl-surface)
  (check-type color sdl:color)
  (if (sdl:a color)
    (cffi:foreign-funcall-pointer (cffi:foreign-symbol-pointer "aacircleRGBA" :library 'sdl-cffi::sdl-gfx)
                                      () 
                                      surface-pointer surface
                                      :int16 x
                                      :int16 y
                                      :int16 r
                                      return-r color
                                      return-g color
                                      return-b color
                                      return-a color
                                      :int)
        (cffi:foreign-funcall-pointer (cffi:foreign-symbol-pointer "aacircleColor" :library 'sdl-cffi::sdl-gfx)
                                      () 
                                      surface-pointer surface
                                      :int16 x
                                      :int16 y
                                      :int16 r
                                      return-packed-color color
                                      :int)))

(defun gfx-draw-circle-* (x y r &key (surface sdl:*default-surface*) (color sdl:*default-color*) (aa nil))
  (unless surface
    (setf surface sdl:*default-display*))
  (check-type surface sdl:sdl-surface)
  (check-type color sdl:color)
  (if aa
    (gfx-draw-aa-circle-* x y r :surface surface :color color)
    (if (sdl:a color)
      (cffi:foreign-funcall-pointer (cffi:foreign-symbol-pointer "circleRGBA" :library 'sdl-cffi::sdl-gfx)
                                    () 
                                    surface-pointer surface
                                    :int16 x
                                    :int16 y
                                    :int16 r
                                    return-r color
                                    return-g color
                                    return-b color
                                    return-a color
                                    :int)
      (cffi:foreign-funcall-pointer (cffi:foreign-symbol-pointer "circleColor" :library 'sdl-cffi::sdl-gfx)
                                    () 
                                    surface-pointer surface
                                    :int16 x
                                    :int16 y
                                    :int16 r
                                    return-packed-color color
                                    :int))))

(defun gfx-draw-filled-circle-* (x y r &key (surface sdl:*default-surface*) (color sdl:*default-color*))
  (unless surface
    (setf surface sdl:*default-display*))
  (check-type surface sdl:sdl-surface)
  (check-type color sdl:color)
  (if (sdl:a color)
    (cffi:foreign-funcall-pointer (cffi:foreign-symbol-pointer "filledCircleRGBA" :library 'sdl-cffi::sdl-gfx)
                                  () 
                                  surface-pointer surface
                                  :int16 x
                                  :int16 y
                                  :int16 r
                                  return-r color
                                  return-g color
                                  return-b color
                                  return-a color
                                  :int)
    (cffi:foreign-funcall-pointer (cffi:foreign-symbol-pointer "filledCircleColor" :library 'sdl-cffi::sdl-gfx)
                                  () 
                                  surface-pointer surface
                                  :int16 x
                                  :int16 y
                                  :int16 r
                                  return-packed-color color
                                  :int)))

(defun gfx-draw-aa-ellipse-* (x y rx ry &key (surface sdl:*default-surface*) (color sdl:*default-color*))
  (unless surface
    (setf surface sdl:*default-display*))
  (check-type surface sdl:sdl-surface)
  (check-type color sdl:color)
  (if (sdl:a color)
    (cffi:foreign-funcall-pointer (cffi:foreign-symbol-pointer "aaellipseRGBA" :library 'sdl-cffi::sdl-gfx)
                                      () 
                                      surface-pointer surface
                                      :int16 x
                                      :int16 y
                                      :int16 rx
                                      :int16 ry
                                      return-r color
                                      return-g color
                                      return-b color
                                      return-a color
                                      :int)
        (cffi:foreign-funcall-pointer (cffi:foreign-symbol-pointer "aaellipseColor" :library 'sdl-cffi::sdl-gfx)
                                      () 
                                      surface-pointer surface
                                      :int16 x
                                      :int16 y
                                      :int16 rx
                                      :int16 rx
                                      return-packed-color color
                                      :int)))

(defun gfx-draw-ellipse-* (x y rx ry &key (surface sdl:*default-surface*) (color sdl:*default-color*) (aa nil))
  (unless surface
    (setf surface sdl:*default-display*))
  (check-type surface sdl:sdl-surface)
  (check-type color sdl:color)
  (if aa
    (gfx-draw-aa-ellipse-* x y rx ry :surface surface :color color)
    (if (sdl:a color)
      (cffi:foreign-funcall-pointer (cffi:foreign-symbol-pointer "ellipseRGBA" :library 'sdl-cffi::sdl-gfx)
                                    () 
                                    surface-pointer surface
                                    :int16 x
                                    :int16 y
                                    :int16 rx
                                    :int16 ry
                                    return-r color
                                    return-g color
                                    return-b color
                                    return-a color
                                    :int)
      (cffi:foreign-funcall-pointer (cffi:foreign-symbol-pointer "ellipseColor" :library 'sdl-cffi::sdl-gfx)
                                    () 
                                    surface-pointer surface
                                    :int16 x
                                    :int16 y
                                    :int16 rx
                                    :int16 ry
                                    return-packed-color color
                                    :int))))

(defun gfx-draw-filled-ellipse-* (x y rx ry &key (surface sdl:*default-surface*) (color sdl:*default-color*))
  (unless surface
    (setf surface sdl:*default-display*))
  (check-type surface sdl:sdl-surface)
  (check-type color sdl:color)
  (if (sdl:a color)
    (cffi:foreign-funcall-pointer (cffi:foreign-symbol-pointer "filledEllipseRGBA" :library 'sdl-cffi::sdl-gfx)
                                  () 
                                  surface-pointer surface
                                  :int16 x
                                  :int16 y
                                  :int16 rx
                                  :int16 ry
                                  return-r color
                                  return-g color
                                  return-b color
                                  return-a color
                                  :int)
    (cffi:foreign-funcall-pointer (cffi:foreign-symbol-pointer "filledEllipseColor" :library 'sdl-cffi::sdl-gfx)
                                  () 
                                  surface-pointer surface
                                  :int16 x
                                  :int16 y
                                  :int16 rx
                                  :int16 ry
                                  return-packed-color color
                                  :int)))

(defun gfx-draw-pie-* (x y rad start end &key (surface sdl:*default-surface*) (color sdl:*default-color*))
  (unless surface
    (setf surface sdl:*default-display*))
  (check-type surface sdl:sdl-surface)
  (check-type color sdl:color)
  (if (sdl:a color)
    (cffi:foreign-funcall-pointer (cffi:foreign-symbol-pointer "pieRGBA" :library 'sdl-cffi::sdl-gfx)
                                  () 
                                  surface-pointer surface
                                  :int16 x
                                  :int16 y
                                  :int16 rad
                                  :int16 start
                                  :int16 end
                                  return-r color
                                  return-g color
                                  return-b color
                                  return-a color
                                  :int)
    (cffi:foreign-funcall-pointer (cffi:foreign-symbol-pointer "pieColor" :library 'sdl-cffi::sdl-gfx)
                                  () 
                                  surface-pointer surface
                                  :int16 x
                                  :int16 y
                                  :int16 rad
                                  :int16 start
                                  :int16 end
                                  return-packed-color color
                                  :int)))
(defun gfx-draw-filled-pie-* (x y rad start end &key (surface sdl:*default-surface*) (color sdl:*default-color*))
  (unless surface
    (setf surface sdl:*default-display*))
  (check-type surface sdl:sdl-surface)
  (check-type color sdl:color)
  (if (sdl:a color)
    (cffi:foreign-funcall-pointer (cffi:foreign-symbol-pointer "filledPieRGBA" :library 'sdl-cffi::sdl-gfx)
                                  () 
                                  surface-pointer surface
                                  :int16 x
                                  :int16 y
                                  :int16 rad
                                  :int16 start
                                  :int16 end
                                  return-r color
                                  return-g color
                                  return-b color
                                  return-a color
                                  :int)
    (cffi:foreign-funcall-pointer (cffi:foreign-symbol-pointer "filledPieColor" :library 'sdl-cffi::sdl-gfx)
                                  () 
                                  surface-pointer surface
                                  :int16 x
                                  :int16 y
                                  :int16 rad
                                  :int16 start
                                  :int16 end
                                  return-packed-color color
                                  :int)))

(defun gfx-draw-aa-trigon (p1 p2 p3 &key (surface sdl:*default-surface*) (color sdl:*default-color*))
  (sdl:check-types sdl:point p1 p2 p3)
  (unless surface
    (setf surface sdl:*default-display*))
  (check-type surface sdl:sdl-surface)
  (check-type color sdl:color)
  (if (sdl:a color)
    (cffi:foreign-funcall-pointer (cffi:foreign-symbol-pointer "aatrigonRGBA" :library 'sdl-cffi::sdl-gfx)
                                  () 
                                  surface-pointer surface
                                  return-x p1
                                  return-y p1
                                  return-x p2
                                  return-y p2
                                  return-x p3
                                  return-y p3
                                  return-r color
                                  return-g color
                                  return-b color
                                  return-a color
                                  :int)
    (cffi:foreign-funcall-pointer (cffi:foreign-symbol-pointer "aatrigonColor" :library 'sdl-cffi::sdl-gfx)
                                  () 
                                  surface-pointer surface
                                  return-x p1
                                  return-y p1
                                  return-x p2
                                  return-y p2
                                  return-x p3
                                  return-y p3
                                  return-packed-color color
                                  :int)))

(defun gfx-draw-trigon (p1 p2 p3 &key (surface sdl:*default-surface*) (color sdl:*default-color*) (aa nil))
  (sdl:check-types sdl:point p1 p2 p3)
  (unless surface
    (setf surface sdl:*default-display*))
  (check-type surface sdl:sdl-surface)
  (check-type color sdl:color)
  (if aa
    (gfx-draw-aa-trigon p1 p2 p3 :surface surface :color color)
    (if (sdl:a color)
      (cffi:foreign-funcall-pointer (cffi:foreign-symbol-pointer "trigonRGBA" :library 'sdl-cffi::sdl-gfx)
                                    () 
                                    surface-pointer surface
                                    return-x p1
                                    return-y p1
                                    return-x p2
                                    return-y p2
                                    return-x p3
                                    return-y p3
                                    return-r color
                                    return-g color
                                    return-b color
                                    return-a color
                                    :int)
      (cffi:foreign-funcall-pointer (cffi:foreign-symbol-pointer "trigonColor" :library 'sdl-cffi::sdl-gfx)
                                    () 
                                    surface-pointer surface
                                    return-x p1
                                    return-y p1
                                    return-x p2
                                    return-y p2
                                    return-x p3
                                    return-y p3
                                    return-packed-color color
                                    :int))))

(defun gfx-draw-filled-trigon (p1 p2 p3 &key (surface sdl:*default-surface*) (color sdl:*default-color*))
  (sdl:check-types sdl:point p1 p2 p3)
  (unless surface
    (setf surface sdl:*default-display*))
  (check-type surface sdl:sdl-surface)
  (check-type color sdl:color)
  (if (sdl:a color)
    (cffi:foreign-funcall-pointer (cffi:foreign-symbol-pointer "filledTrigonRGBA" :library 'sdl-cffi::sdl-gfx)
                                      () 
                                      surface-pointer surface
                                      return-x p1
                                      return-y p1
                                      return-x p2
                                      return-y p2
                                      return-x p3
                                      return-y p3
                                      return-r color
                                      return-g color
                                      return-b color
                                      return-a color
                                      :int)
        (cffi:foreign-funcall-pointer (cffi:foreign-symbol-pointer "filledTrigonColor" :library 'sdl-cffi::sdl-gfx)
                                      () 
                                      surface-pointer surface
                                      return-x p1
                                      return-y p1
                                      return-x p2
                                      return-y p2
                                      return-x p3
                                      return-y p3
                                      return-packed-color color
                                      :int)))

(defun gfx-draw-aa-polygon (vertices &key (surface sdl:*default-surface*) (color sdl:*default-color*))
  (check-type vertices (and list (not null)) "Vertices must be a LIST of SDL:POINTs")
  (unless surface
    (setf surface sdl:*default-display*))
  (check-type surface sdl:sdl-surface)
  (check-type color sdl:color)
  (let ((x-array (cffi:foreign-alloc :short :initial-contents (return-list-for-array vertices :x)))
	(y-array (cffi:foreign-alloc :short :initial-contents (return-list-for-array vertices :y)))
	(poly-surface nil))
    (if (sdl:a color)
      (setf poly-surface (cffi:foreign-funcall-pointer (cffi:foreign-symbol-pointer "aapolygonRGBA" :library 'sdl-cffi::sdl-gfx)
                                      () 
                                      surface-pointer surface
                                      :pointer x-array
                                      :pointer y-array
                                      :uint32  (length vertices)
                                      return-r color
                                      return-g color
                                      return-b color
                                      return-a color
                                      :int))
      (setf poly-surface (cffi:foreign-funcall-pointer (cffi:foreign-symbol-pointer "aapolygonColor" :library 'sdl-cffi::sdl-gfx)
                                      () 
                                      surface-pointer surface
                                      :pointer x-array
                                      :pointer y-array
                                      :uint32 (length vertices)
                                      return-packed-color color
                                      :int)))
    (cffi:foreign-free x-array)
    (cffi:foreign-free y-array)
    poly-surface))

(defun gfx-draw-polygon (vertices &key (surface sdl:*default-surface*) (color sdl:*default-color*) (aa nil))
  (check-type vertices (and list (not null)) "Vertices must be a LIST of SDL:POINTs")
  (unless surface
    (setf surface sdl:*default-display*))
  (check-type surface sdl:sdl-surface)
  (check-type color sdl:color)
  (if aa
    (gfx-draw-aa-polygon vertices :surface surface :color color)
    (let ((x-array (cffi:foreign-alloc :short :initial-contents (return-list-for-array vertices :x)))
          (y-array (cffi:foreign-alloc :short :initial-contents (return-list-for-array vertices :y)))
          (poly-surface nil))
      (if (sdl:a color)
        (setf poly-surface (cffi:foreign-funcall-pointer (cffi:foreign-symbol-pointer "polygonRGBA" :library 'sdl-cffi::sdl-gfx)
                                                         () 
                                                         surface-pointer surface
                                                         :pointer x-array
                                                         :pointer y-array
                                                         :uint32 (length vertices)
                                                         return-r color
                                                         return-g color
                                                         return-b color
                                                         return-a color
                                                         :int))
        (setf poly-surface (cffi:foreign-funcall-pointer (cffi:foreign-symbol-pointer "polygonColor" :library 'sdl-cffi::sdl-gfx)
                                                         () 
                                                         surface-pointer surface
                                                         :pointer x-array
                                                         :pointer y-array
                                                         :uint32 (length vertices)
                                                         return-packed-color color
                                                         :int)))
      (cffi:foreign-free x-array)
      (cffi:foreign-free y-array)
      poly-surface)))

(defun gfx-draw-filled-polygon (vertices &key (surface sdl:*default-surface*) (color sdl:*default-color*))
  (check-type vertices (and list (not null)) "Vertices must be a LIST of SDL:POINTs")
  (unless surface
    (setf surface sdl:*default-display*))
  (check-type surface sdl:sdl-surface)
  (check-type color sdl:color)
  (let ((x-array (cffi:foreign-alloc :short :initial-contents (return-list-for-array vertices :x)))
	(y-array (cffi:foreign-alloc :short :initial-contents (return-list-for-array vertices :y)))
	(poly-surface nil))
    (if (sdl:a color)
      (setf poly-surface (cffi:foreign-funcall-pointer (cffi:foreign-symbol-pointer "filledPolygonRGBA" :library 'sdl-cffi::sdl-gfx)
                                    () 
                                    surface-pointer surface
                                    :pointer x-array
                                    :pointer y-array
                                    :uint32 (length vertices)
                                    return-r color
                                    return-g color
                                    return-b color
                                    return-a color
                                    :int))
      (setf poly-surface (cffi:foreign-funcall-pointer (cffi:foreign-symbol-pointer "filledPolygonColor" :library 'sdl-cffi::sdl-gfx)
                                    () 
                                    surface-pointer surface
                                    :pointer x-array
                                    :pointer y-array
                                    :uint32 (length vertices)
                                    return-packed-color color
                                    :int)))
    (cffi:foreign-free x-array)
    (cffi:foreign-free y-array)
    poly-surface))

(defun gfx-draw-bezier (vertices &key (surface sdl:*default-surface*) (color sdl:*default-color*) (segments 20) (style nil))
  (declare (ignorable style))
  (check-type vertices (and list (not null)) "Vertices must be a LIST of SDL:POINTs")
  (unless surface
    (setf surface sdl:*default-display*))
  (check-type surface sdl:sdl-surface)
  (check-type color sdl:color)

  (let ((x-array (cffi:foreign-alloc :short :initial-contents (return-list-for-array vertices :x)))
	(y-array (cffi:foreign-alloc :short :initial-contents (return-list-for-array vertices :y)))
	(bezier-surface nil)
        (length (length vertices)))
    (if (sdl:a color)
      (setf bezier-surface (cffi:foreign-funcall-pointer (cffi:foreign-symbol-pointer "bezierRGBA" :library 'sdl-cffi::sdl-gfx)
                                                         () 
                                                         surface-pointer surface
                                                         :pointer x-array
                                                         :pointer y-array
                                                         :int length
                                                         :int segments
                                                         return-r color
                                                         return-g color
                                                         return-b color
                                                         return-a color
                                                         :int))
      (setf bezier-surface (cffi:foreign-funcall-pointer (cffi:foreign-symbol-pointer "bezierColor" :library 'sdl-cffi::sdl-gfx)
                                                         () 
                                                         surface-pointer surface
                                                         :pointer x-array
                                                         :pointer y-array
                                                         :int length
                                                         :int segments
                                                         return-packed-color color
                                                         :int)))
    (cffi:foreign-free x-array)
    (cffi:foreign-free y-array)
    bezier-surface))

(defun gfx-draw-arc-* (x y rad start end &key (surface sdl:*default-surface*) (color sdl:*default-color*))
  (unless surface
    (setf surface sdl:*default-display*))
  (check-type surface sdl:sdl-surface)
  (check-type color sdl:color)
  (if (sdl:a color)
    (cffi:foreign-funcall-pointer (cffi:foreign-symbol-pointer "arcRGBA" :library 'sdl-cffi::sdl-gfx)
                                      () 
                                      surface-pointer surface
                                      :int16 x
                                      :int16 y
                                      :int16 rad
                                      :int16 start
                                      :int16 end
                                      return-r color
                                      return-g color
                                      return-b color
                                      return-a color
                                      :int)
        (cffi:foreign-funcall-pointer (cffi:foreign-symbol-pointer "arcColor" :library 'sdl-cffi::sdl-gfx)
                                      () 
                                      surface-pointer surface
                                      :int16 x
                                      :int16 y
                                      :int16 rad
                                      :int16 start
                                      :int16 end
                                      return-packed-color color
                                      :int)))

(defun gfx-roto-zoom-surface (angle zoom smooth &key (surface sdl:*default-surface*))
  (check-type surface sdl:surface)
  (make-instance 'sdl:surface :fp (cffi:foreign-funcall-pointer (cffi:foreign-symbol-pointer "rotozoomSurface" :library 'sdl-cffi::sdl-gfx)
                                                                () 
                                                                surface-pointer surface
                                                                return-int-to-double angle
                                                                return-int-to-double zoom
                                                                return-true-to-1 smooth
                                                                :pointer)))

(defun gfx-roto-zoom-xy (angle zoomx zoomy smooth &key (surface sdl:*default-surface*))
  (check-type surface sdl:surface)
  (make-instance 'sdl:surface :fp (cffi:foreign-funcall-pointer (cffi:foreign-symbol-pointer "rotozoomSurfaceXY" :library 'sdl-cffi::sdl-gfx)
                                                                () 
                                                                surface-pointer surface
                                                                return-int-to-double angle
                                                                return-int-to-double zoomx
                                                                return-int-to-double zoomy
                                                                return-true-to-1 smooth
                                                                :pointer)))

(defun gfx-roto-zoom-size (width height angle zoom)
  (cffi:with-foreign-objects ((dstwidth :int) (dstheight :int))
    (cffi:foreign-funcall-pointer (cffi:foreign-symbol-pointer "rotozoomSurfaceSize" :library 'sdl-cffi::sdl-gfx)
                                  ()
                                  :int width
                                  :int height
                                  return-int-to-double angle
                                  return-int-to-double zoom
                                  :pointer dstwidth
                                  :pointer dstheight
                                  :void)
    (vector (cffi:mem-ref dstwidth :int) (cffi:mem-ref dstheight :int))))

(defun gfx-roto-zoom-size-xy (width height angle zoomx zoomy)
  (cffi:with-foreign-objects ((dstwidth :int) (dstheight :int))
    (cffi:foreign-funcall-pointer (cffi:foreign-symbol-pointer "rotozoomSurfaceSizeXY" :library 'sdl-cffi::sdl-gfx)
                                  ()
                                  :int width
                                  :int height
                                  return-int-to-double angle
                                  return-int-to-double zoomx
                                  return-int-to-double zoomy
                                  :pointer dstwidth
                                  :pointer dstheight
                                  :void)
    (vector (cffi:mem-ref dstwidth :int) (cffi:mem-ref dstheight :int))))

(defun gfx-zoom-surface (zoomx zoomy &key (surface sdl:*default-surface*) (smooth nil) (free nil))
  (declare (ignorable free))
  (check-type surface sdl:surface)
  (make-instance 'sdl:surface :fp (cffi:foreign-funcall-pointer (cffi:foreign-symbol-pointer "zoomSurface" :library 'sdl-cffi::sdl-gfx)
                                                                () 
                                                                surface-pointer surface
                                                                return-int-to-double zoomx
                                                                return-int-to-double zoomy
                                                                return-true-to-1 smooth
                                                                :pointer)))

(defun gfx-zoom-surface-size (width height zoomx zoomy)
  (cffi:with-foreign-objects ((dstwidth :int) (dstheight :int))
    (cffi:foreign-funcall-pointer (cffi:foreign-symbol-pointer "zoomSurfaceSize" :library 'sdl-cffi::sdl-gfx)
                                  ()
                                  :int width
                                  :int height
                                  return-int-to-double zoomx
                                  return-int-to-double zoomy
                                  :pointer dstwidth
                                  :pointer dstheight
                                  :void)
    (vector (cffi:mem-ref dstwidth :int) (cffi:mem-ref dstheight :int))))

(defun gfx-Primitives-Set-Font (fontdata cw ch)
  (cffi:foreign-funcall-pointer (cffi:foreign-symbol-pointer "gfxPrimitivesSetFont" :library 'sdl-cffi::sdl-gfx)
                                ()
                                :pointer fontdata
                                :int cw
                                :int ch
                                :void))

(defun gfx-character-color (x y c &key (surface sdl:*default-surface*) (color sdl:*default-color*))
  (if (sdl:a color)
    (cffi:foreign-funcall-pointer (cffi:foreign-symbol-pointer "characterRGBA" :library 'sdl-cffi::sdl-gfx)
                                ()
                                surface-pointer surface
                                :int16 x
                                :int16 y
                                :char c
                                return-r color
                                return-g color
                                return-b color
                                return-a color
                                :int)
    (cffi:foreign-funcall-pointer (cffi:foreign-symbol-pointer "characterColor" :library 'sdl-cffi::sdl-gfx)
                                ()
                                surface-pointer surface
                                :int16 x
                                :int16 y
                                :char c
                                return-packed-color color
                                :int)))

(defun gfx-string-color (x y c &key (surface sdl:*default-surface*) (color sdl:*default-color*))
  (if (sdl:a color)
    (cffi:foreign-funcall-pointer (cffi:foreign-symbol-pointer "stringRGBA" :library 'sdl-cffi::sdl-gfx)
                                ()
                                surface-pointer surface
                                :int16 x
                                :int16 y
                                :string c
                                return-r color
                                return-g color
                                return-b color
                                return-a color
                                :int)
    (cffi:foreign-funcall-pointer (cffi:foreign-symbol-pointer "stringColor" :library 'sdl-cffi::sdl-gfx)
                                ()
                                surface-pointer surface
                                :int16 x
                                :int16 y
                                :string c
                                return-packed-color color
                                :int)))
