;; SDL_gfx v2.0.13 library. Uses CFFI for foreign function interfacing...
;; (C)2006 Luke Crook <luke@balooga.com>, Justin Heyes-Jones <justinhj@gmail.com>
;; Thanks to Frank Buss and Surendra Singh
;; see COPYING for license
;; This file contains some useful functions for using SDL_gfx from Common lisp
;; using sdl_gfx.lisp (the CFFI wrapper)

(in-package #:lispbuilder-sdl-gfx)

;;; Helper Functions

(defun return-list-for-array (points index-type)
  (case index-type
    (:x (mapcar #'(lambda (point)
		    (elt point 0))
		points))
    (:y (mapcar #'(lambda (point)
		    (elt point 1))
		points))
    (t nil)))


;;; d

(defun draw-pixel (&key (position sdl:*default-position*) (surface sdl:*default-surface*) (color sdl:*default-color*))
  (if (= 3 (length color))
      (pixelcolor surface (sdl:point-x position) (sdl:point-y position)
		  (map-color color))
      (pixelrgba surface (sdl:point-x position) (sdl:point-y position)
		 (sdl:color-r color) (sdl:color-g color) (sdl:color-b color) (sdl:color-a color))))

(defun draw-hline (x1 x2 y &key (surface sdl:*default-surface*) (color sdl:*default-color*))
  (if (= 3 (length color))
      (hlinecolor surface x1 x2 y
		  (map-color color))
      (hlineRGBA surface x1 x2 y
		 (sdl:color-r color) (sdl:color-g color) (sdl:color-b color) (sdl:color-a color))))

(defun draw-vline (x y1 y2 &key (surface sdl:*default-surface*) (color sdl:*default-color*))
  (if (= 3 (length color))
      (vlinecolor surface x y1 y2
		  (map-color color))
      (vlineRGBA surface x y1 y2
		 (sdl:color-r color) (sdl:color-g color) (sdl:color-b color) (sdl:color-a color))))

(defun draw-rectangle (rect &key (surface sdl:*default-surface*) (color sdl:*default-color*))
  (if (= 3 (length color))
      (rectanglecolor surface (sdl:rect-x rect) (sdl:rect-y rect) (sdl:rect-x2 rect) (sdl:rect-y2 rect)
		      (map-color color))
      (rectangleRGBA surface (sdl:rect-x rect) (sdl:rect-y rect) (sdl:rect-x2 rect) (sdl:rect-y2 rect)
		     (sdl:color-r color) (sdl:color-g color) (sdl:color-b color) (sdl:color-a color))))

(defun draw-box (rect &key (surface sdl:*default-surface*) (color sdl:*default-color*))
  (if (= 3 (length color))
      (boxcolor surface (sdl:rect-x rect) (sdl:rect-y rect) (sdl:rect-x2 rect) (sdl:rect-y2 rect)
		(map-color color))
      (boxRGBA surface (sdl:rect-x rect) (sdl:rect-y rect) (sdl:rect-x2 rect) (sdl:rect-y2 rect)
	       (sdl:color-r color) (sdl:color-g color) (sdl:color-b color) (sdl:color-a color))))

(defun draw-line (point1 point2 &key (surface sdl:*default-surface*) (color sdl:*default-color*))
  (if (= 3 (length color))
      (linecolor surface (sdl:point-x point1) (sdl:point-y point1) (sdl:point-x point2) (sdl:point-y point2)
		 (map-color color))
      (lineRGBA surface (sdl:point-x point1) (sdl:point-y point1) (sdl:point-x point2) (sdl:point-y point2)
		(sdl:color-r color) (sdl:color-g color) (sdl:color-b color) (sdl:color-a color))))

(defun draw-aaline (point1 point2 &key (surface sdl:*default-surface*) (color sdl:*default-color*))
  (if (= 3 (length color))
      (aalinecolor surface (sdl:point-x point1) (sdl:point-y point1) (sdl:point-x point2) (sdl:point-y point2)
		   (map-color color))
      (aalineRGBA surface (sdl:point-x point1) (sdl:point-y point1) (sdl:point-x point2) (sdl:point-y point2)
		  (sdl:color-r color) (sdl:color-g color) (sdl:color-b color) (sdl:color-a color))))

(defun draw-circle (r color
		    &key (position sdl:*default-position*) (surface sdl:*default-surface*) (color sdl:*default-color*))
  (if (= 3 (length color))
      (circlecolor surface (sdl:point-x position) (sdl:point-y position) r
		   (map-color color))
      (circleRGBA surface (sdl:point-x position) (sdl:point-y position) r
		  (sdl:color-r color) (sdl:color-g color) (sdl:color-b color) (sdl:color-a color))))

(defun draw-aacircle (r color
		      &key (position sdl:*default-position*) (surface sdl:*default-surface*) (color sdl:*default-color*))
  (if (= 3 (length color))
      (aacirclecolor surface (sdl:point-x position) (sdl:point-y position) r
		     (map-color color))
      (aacircleRGBA surface (sdl:point-x position) (sdl:point-y position) r
		    (sdl:color-r color) (sdl:color-g color) (sdl:color-b color) (sdl:color-a color))))

(defun draw-filledcircle (r
			  &key (position sdl:*default-position*) (surface sdl:*default-surface*) (color sdl:*default-color*))
  (if (= 3 (length color))
      (filledcirclecolor surface (sdl:point-x position) (sdl:point-y position) r
		     (map-color color))
      (filledcircleRGBA surface (sdl:point-x position) (sdl:point-y position) r
		    (sdl:color-r color) (sdl:color-g color) (sdl:color-b color) (sdl:color-a color))))

(defun draw-ellipse (rx ry
		     &key (position sdl:*default-position*) (surface sdl:*default-surface*) (color sdl:*default-color*))
  (if (= 3 (length color))
      (ellipsecolor surface (sdl:point-x position) (sdl:point-y position) rx ry
		    (map-color color))
      (ellipseRGBA surface (sdl:point-x position) (sdl:point-y position) rx ry
		   (sdl:color-r color) (sdl:color-g color) (sdl:color-b color) (sdl:color-a color))))

(defun draw-aaellipse (rx ry
		       &key (position sdl:*default-position*) (surface sdl:*default-surface*) (color sdl:*default-color*))
  (if (= 3 (length color))
      (aaellipsecolor surface (sdl:point-x position) (sdl:point-y position) rx ry
		      (map-color color))
      (aaellipseRGBA surface (sdl:point-x position) (sdl:point-y position) rx ry
		     (sdl:color-r color) (sdl:color-g color) (sdl:color-b color) (sdl:color-a color))))

(defun draw-filledellipse (rx ry
			   &key (position sdl:*default-position*) (surface sdl:*default-surface*) (color sdl:*default-color*))
  (if (= 3 (length color))
      (filledellipsecolor surface (sdl:point-x position) (sdl:point-y position) rx ry
			  (map-color color))
      (filledellipseRGBA surface (sdl:point-x position) (sdl:point-y position) rx ry
			 (sdl:color-r color) (sdl:color-g color) (sdl:color-b color) (sdl:color-a color))))

(defun draw-pie (rad start end
		 &key (position sdl:*default-position*) (surface sdl:*default-surface*) (color sdl:*default-color*))
  (if (= 3 (length color))
      (piecolor surface (sdl:point-x position) (sdl:point-y position) rad start end
		(map-color color))
      (pieRGBA surface (sdl:point-x position) (sdl:point-y position) rad start end
	       (sdl:color-r color) (sdl:color-g color) (sdl:color-b color) (sdl:color-a color))))

(defun draw-filledpie (rad start end
		       &key (position sdl:*default-position*) (surface sdl:*default-surface*) (color sdl:*default-color*))
  (if (= 3 (length color))
      (filledpiecolor surface (sdl:point-x position) (sdl:point-y position) rad start end
		      (map-color color))
      (filledpieRGBA surface (sdl:point-x position) (sdl:point-y position) rad start end
		     (sdl:color-r color) (sdl:color-g color) (sdl:color-b color) (sdl:color-a color))))

(defun draw-trigon (point1 point2 point3 &key (surface sdl:*default-surface*) (color sdl:*default-color*))
  (if (= 3 (length color))
      (trigoncolor surface (sdl:point-x point1) (sdl:point-y point1)
		   (sdl:point-x point2) (sdl:point-y point2)
		   (sdl:point-x point3) (sdl:point-y point3)
		   (map-color color))
      (trigonRGBA surface (sdl:point-x point1) (sdl:point-y point1)
		   (sdl:point-x point2) (sdl:point-y point2)
		   (sdl:point-x point3) (sdl:point-y point3)
		  (sdl:color-r color) (sdl:color-g color) (sdl:color-b color) (sdl:color-a color))))

(defun draw-aatrigon (point1 point2 point3 &key (surface sdl:*default-surface*) (color sdl:*default-color*))
  (if (= 3 (length color))
      (aatrigoncolor surface (sdl:point-x point1) (sdl:point-y point1)
		     (sdl:point-x point2) (sdl:point-y point2)
		     (sdl:point-x point3) (sdl:point-y point3)
		     (map-color color))
      (aatrigonRGBA surface (sdl:point-x point1) (sdl:point-y point1)
		    (sdl:point-x point2) (sdl:point-y point2)
		    (sdl:point-x point3) (sdl:point-y point3)
		    (sdl:color-r color) (sdl:color-g color) (sdl:color-b color) (sdl:color-a color))))

(defun draw-filledtrigon (point1 point2 point3 &key (surface sdl:*default-surface*) (color sdl:*default-color*))
  (if (= 3 (length color))
      (filledtrigoncolor surface (sdl:point-x point1) (sdl:point-y point1)
			 (sdl:point-x point2) (sdl:point-y point2)
			 (sdl:point-x point3) (sdl:point-y point3)
			 (map-color color))
      (filledtrigonRGBA surface (sdl:point-x point1) (sdl:point-y point1)
			(sdl:point-x point2) (sdl:point-y point2)
			(sdl:point-x point3) (sdl:point-y point3)
			(sdl:color-r color) (sdl:color-g color) (sdl:color-b color) (sdl:color-a color))))

(defun draw-polygon (points &key (surface sdl:*default-surface*) (color sdl:*default-color*))
  (unless (listp points)
    (error "draw-polygon: ~A must be a list of (x y) points." points))
  (let ((x-array (cffi:foreign-alloc :unsigned-short :initial-contents (return-list-for-array points :x)))
	(y-array (cffi:foreign-alloc :unsigned-short :initial-contents (return-list-for-array points :y)))
	(poly-surface nil))
    (if (= 3 (length color))
	(setf poly-surface (polygoncolor surface x-array y-array (length points)
					 (map-color color)))
	(setf poly-surface (polygonRGBA surface x-array y-array (length points)
					(sdl:color-r color) (sdl:color-g color) (sdl:color-b color) (sdl:color-a color))))
    (cffi:foreign-free x-array)
    (cffi:foreign-free y-array)
    poly-surface))

(defun draw-aapolygon (points &key (surface sdl:*default-surface*) (color sdl:*default-color*))
  (unless (listp points)
    (error "draw-aapolygon: ~A must be a list of (x y) points." points))
  (let ((x-array (cffi:foreign-alloc :unsigned-short :initial-contents (return-list-for-array points :x)))
	(y-array (cffi:foreign-alloc :unsigned-short :initial-contents (return-list-for-array points :y)))
	(poly-surface nil))
    (if (= 3 (length color))
	(setf poly-surface (aapolygoncolor surface x-array y-array (length points)
					   (map-color color)))
	(setf poly-surface (aapolygonRGBA surface x-array y-array (length points)
					  (sdl:color-r color) (sdl:color-g color) (sdl:color-b color)
					  (sdl:color-a color))))
    (cffi:foreign-free x-array)
    (cffi:foreign-free y-array)
    poly-surface))

(defun draw-filledpolygon (points &key (surface sdl:*default-surface*) (color sdl:*default-color*))
  (unless (listp points)
    (error "draw-filledpolygon: ~A must be a list of (x y) points." points))
  (let ((x-array (cffi:foreign-alloc :unsigned-short :initial-contents (return-list-for-array points :x)))
	(y-array (cffi:foreign-alloc :unsigned-short :initial-contents (return-list-for-array points :y)))
	(poly-surface nil))
    (if (= 3 (length color))
	(setf poly-surface (filledpolygoncolor surface x-array y-array (length points)
					       (map-color color)))
	(setf poly-surface (filledpolygonRGBA surface x-array y-array (length points)
					      (sdl:color-r color) (sdl:color-g color) (sdl:color-b color)
					      (sdl:color-a color))))
    (cffi:foreign-free x-array)
    (cffi:foreign-free y-array)
    poly-surface))

(defun draw-bezier (points s &key (surface sdl:*default-surface*) (color sdl:*default-color*))
  (unless (listp points)
    (error "draw-bezier: ~A must be a list of (x y) points." points))
  (let ((x-array (cffi:foreign-alloc :unsigned-short :initial-contents (return-list-for-array points :x)))
	(y-array (cffi:foreign-alloc :unsigned-short :initial-contents (return-list-for-array points :y)))
	(bezier-surface nil))
    (if (= 3 (length color))
	(setf bezier-surface (beziercolor surface x-array y-array (length points) s (map-color color)))
	(setf bezier-surface (bezierRGBA surface x-array y-array (length points) s
					 (sdl:color-r color) (sdl:color-g color) (sdl:color-b color)
					 (sdl:color-a color))))
    (cffi:foreign-free x-array)
    (cffi:foreign-free y-array)
    bezier-surface))

(defun draw-character (c
		       &key (position sdl:*default-position*) (surface sdl:*default-surface*) (color sdl:*default-color*))
  (if (= 3 (length color))
      (charactercolor surface (sdl:point-x position) (sdl:point-y position) c
		      (map-color color))
      (characterRGBA surface (sdl:point-x position) (sdl:point-y position) c
		     (sdl:color-r color) (sdl:color-g color) (sdl:color-b color) (sdl:color-a color))))

(defun draw-string (c
		    &key (position sdl:*default-position*) (surface sdl:*default-surface*) (color sdl:*default-color*))
  (if (= 3 (length color))
      (stringcolor surface (sdl:point-x position) (sdl:point-y position) c
		   (map-color color))
      (stringRGBA surface (sdl:point-x position) (sdl:point-y position) c
		  (sdl:color-r color) (sdl:color-g color) (sdl:color-b color) (sdl:color-a color))))

;;; m

(defun map-color (color)
  (let ((col #x00000000))
    (setf col (logior (ash (sdl:color-r color) 24)
		      (ash (sdl:color-g color) 16)
		      (ash (sdl:color-b color) 8)
		      (if (= 4 (length color))
			  (sdl:color-a color)
			  #xFF)))
    col))

;;; r

(defun rotozoom (angle zoom smooth &key (surface sdl:*default-surface*))
  (rotozoomSurface surface angle zoom smooth))

(defun rotozoom-xy (angle zoomx zoomy smooth &key (surface sdl:*default-surface*))
  (rotozoomSurfacexy surface angle zoomx zoomy smooth))

(defun rotozoom-size (width height angle zoom)
  (cffi:with-foreign-objects ((dstwidth :int) (dstheight :int))
    (rotozoomSurfaceSize width height angle zoom dstwidth dstheight)
    (vector (cffi:mem-ref dstwidth :int) (cffi:mem-ref dstheight :int))))

(defun rotozoom-size-xy (width height angle zoomx zoomy)
  (cffi:with-foreign-objects ((dstwidth :int) (dstheight :int))
    (rotozoomSurfaceSizeXY width height angle zoomx zoomy dstwidth dstheight)
    (vector (cffi:mem-ref dstwidth :int) (cffi:mem-ref dstheight :int))))

;;; z

(defun zoom (zoomx zoomy smooth &key (surface sdl:*default-surface*))
  (zoomSurface surface zoomx zoomy smooth))

(defun zoom-size (width height zoomx zoomy)
  (cffi:with-foreign-objects ((dstwidth :int) (dstheight :int))
    (zoomSurfaceSize width height zoomx zoomy dstwidth dstheight)
    (vector (cffi:mem-ref dstwidth :int) (cffi:mem-ref dstheight :int))))

