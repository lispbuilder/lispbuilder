;; SDL_gfx v2.0.13 library. Uses CFFI for foreign function interfacing...
;; (C)2006 Luke Crook <luke@balooga.com>, Justin Heyes-Jones <justinhj@gmail.com>
;; Thanks to Frank Buss and Surendra Singh
;; see COPYING for license
;; This file contains some useful functions for using SDL_gfx from Common lisp
;; using sdl_gfx.lisp (the CFFI wrapper)

(in-package #:lispbuilder-sdl-gfx)

;; Coefficients for Matrix M
(defvar *M11*	 0.0)	
(defvar *M12*	 1.0)
(defvar *M13*	 0.0)
(defvar *M14*	 0.0)
(defvar *M21*	-0.5)
(defvar *M22*	 0.0)
(defvar *M23*	 0.5)
(defvar *M24*	 0.0)
(defvar *M31*	 1.0)
(defvar *M32*	-2.5)
(defvar *M33*	 2.0)
(defvar *M34*	-0.5)
(defvar *M41*	-0.5)
(defvar *M42*	 1.5)
(defvar *M43*	-1.5)
(defvar *M44*	 0.5)


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


;;; Macros

;;; w

(defmacro with-bezier ((shape-type &optional (segments 10)) &body body)
  (let ((point-list (gensym "point-list-")))
    `(let ((,point-list nil))
       (labels ((add-vertex (point)
		  (setf ,point-list (append ,point-list (list point)))))
	 ,@body)
       (draw-bezier ,point-list ,segments))))

(defmacro with-curve ((shape-type &optional (segments 10)) &body body)
  (let ((point-list (gensym "point-list-")))
    `(let ((,point-list nil))
       (labels ((add-vertex (point)
		  (setf ,point-list (append ,point-list (list point)))))
	 ,@body)
       (draw-curve ,point-list ,shape-type ,segments))))

(defmacro with-shape ((shape-type) &body body)
  (let ((point-list (gensym "point-list-")))
    `(let ((,point-list nil))
       (labels ((add-vertex (point)
		  (setf ,point-list (append ,point-list (list point)))))
	 ,@body)
       (draw-shape ,point-list ,shape-type))))



;;; Functions

;;; c

(defun calculate-curve (p1 p2 p3 p4 segments)
  (let ((step-size 0)
	(points nil))
    (when (or (null segments) (= segments 0))
      (setf segments (distance (sdl:point-x p2) (sdl:point-y p2)
			       (sdl:point-x p3) (sdl:point-y p3))))
    (setf step-size (coerce (/ 1 segments) 'float))
    (setf points (loop for i from 0.0 below 1.0 by step-size
	  collecting (sdl:point (catmull-rom-spline i (sdl:point-x p1) (sdl:point-x p2)
						    (sdl:point-x p3) (sdl:point-x p4))
				(catmull-rom-spline i (sdl:point-y p1) (sdl:point-y p2)
						    (sdl:point-y p3) (sdl:point-y p4)))))
    ; NOTE: There must be a more efficient way to add the first and last points to the point list.
    (push p2 points)
    (nconc points (list p3))))

(defun catmull-rom-spline (val v0 v1 v2 v3)
  (let ((c1 0) (c2 0) (c3 0) (c4 0))
    (setf c1                 (* *M12* v1)
	  c2 (+ (* *M21* v0)              (* *M23* v2))
	  c3 (+ (* *M31* v0) (* *M32* v1) (* *M33* v2) (* *M34* v3))
	  c4 (+ (* *M41* v0) (* *M42* v1) (* *M43* v2) (* *M44* v3)))
    (+ c1 (* val (+ c2 (* val (+ c3 (* c4 val))))))))


;;; d

(defun distance (x1 y1 x2 y2)
  (sqrt (+ (expt (- x1 x2) 2) 
	   (expt (- y1 y2) 2))))

(defun draw-curve (points type segments)
  (do* ((p1 points (cdr p1))
	(p2 (cdr p1) (cdr p1))
	(p3 (cdr p2) (cdr p2))
	(p4 (cdr p3) (cdr p3)))
       ((or (null p4) (null p3) (null p2) (null p1)))
    (draw-shape (calculate-curve (first p1) (first p2) (first p3) (first p4) segments) type)))

(defun draw-shape (points type)
  (case type
    (:line-strip
     (do* ((p1 points (cdr p1))
	   (p2 (cdr p1) (cdr p1)))
	  ((or (null p2)
	       (null p1)))
       (sdl-gfx::draw-line (first p1) (first p2))))
    (:lines
     (do* ((p1 points (if (cdr p1)
			  (cddr p1)
			  nil))
	   (p2 (cdr p1) (cdr p1)))
	  ((or (null p2)
	       (null p1)))
       (sdl-gfx::draw-line (first p1) (first p2))))
    (:points
     (loop for point in points
	do (sdl-gfx::draw-pixel :position point)))))

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
