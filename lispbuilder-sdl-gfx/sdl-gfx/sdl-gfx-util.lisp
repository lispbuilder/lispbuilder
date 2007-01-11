;; SDL_gfx v2.0.13 library. Uses CFFI for foreign function interfacing...
;; (C)2006 Luke Crook <luke@balooga.com>, Justin Heyes-Jones <justinhj@gmail.com>
;; Thanks to Frank Buss and Surendra Singh
;; see COPYING for license
;; This file contains some useful functions for using SDL_gfx from Common lisp
;; using sdl_gfx.lisp (the CFFI wrapper)

(in-package #:lispbuilder-sdl-gfx)


;;; Macros

;;; w

(defmacro with-bezier ((&optional (segments 10)) &body body)
  (let ((point-list (gensym "point-list-")))
    `(let ((,point-list nil))
       (labels ((add-vertex (point)
		  (setf ,point-list (append ,point-list (list point))))
		(add-vertex-* (x y)
		  (add-vertex (sdl:point :x x :y y))))
	 ,@body)
       (draw-bezier ,point-list ,segments))))

(defmacro with-curve ((shape-type &optional (segments 10)) &body body)
  (let ((point-list (gensym "point-list-")))
    `(let ((,point-list nil))
       (labels ((add-vertex (point)
		  (setf ,point-list (append ,point-list (list point))))
		(add-vertex-* (x y)
		  (add-vertex (sdl:point :x x :y y))))
	 ,@body)
       (draw-curve ,point-list ,shape-type ,segments))))

(defmacro with-shape ((shape-type) &body body)
  (let ((point-list (gensym "point-list-")))
    `(let ((,point-list nil))
       (labels ((add-vertex (point)
		  (setf ,point-list (append ,point-list (list point))))
		(add-vertex-* (x y)
		  (add-vertex (sdl:point :x x :y y))))
	 ,@body)
       (draw-shape ,point-list ,shape-type))))

;;; Functions

;;; d

(defun draw-curve (points type segments)
  (do* ((p1 points (cdr p1))
	(p2 (cdr p1) (cdr p1))
	(p3 (cdr p2) (cdr p2))
	(p4 (cdr p3) (cdr p3)))
       ((or (null p4) (null p3) (null p2) (null p1)))
    (draw-shape (sdl:calculate-curve (first p1) (first p2) (first p3) (first p4) segments) type)))

(defun draw-shape (points type)
  (case type
    (:line-strip
     (do* ((p1 points (cdr p1))
	   (p2 (cdr p1) (cdr p1)))
	  ((or (null p2)
	       (null p1)))
       (draw-line (first p1) (first p2))))
    (:lines
     (do* ((p1 points (if (cdr p1)
			  (cddr p1)
			  nil))
	   (p2 (cdr p1) (cdr p1)))
	  ((or (null p2)
	       (null p1)))
       (draw-line (first p1) (first p2))))
    (:points
     (loop for point in points
	do (draw-pixel point)))))

(defun draw-pixel (position &key (surface sdl:*default-surface*) (color sdl:*default-color*))
  (if (typep color 'sdl:color)
      (sdl-gfx-cffi::pixel-color (sdl:fp surface) (sdl:x position) (sdl:y position)
				 (map-color color))
      (sdl-gfx-cffi::pixel-rgba (sdl:fp surface) (sdl:x position) (sdl:y position)
				(sdl:r color) (sdl:g color) (sdl:b color) (sdl:a color))))

(defun draw-pixel-* (x y &key (surface sdl:*default-surface*) (color sdl:*default-color*))
  (if (typep color 'sdl:color)
      (sdl-gfx-cffi::pixel-color (sdl:fp surface) x y
				 (map-color color))
      (sdl-gfx-cffi::pixel-rgba (sdl:fp surface) x y
				(sdl:r color) (sdl:g color) (sdl:b color) (sdl:a color))))

(defun draw-hline (x1 x2 y &key (surface sdl:*default-surface*) (color sdl:*default-color*))
  (if (typep color 'sdl:color)
      (sdl-gfx-cffi::hline-color (sdl:fp surface) x1 x2 y
				 (map-color color))
      (sdl-gfx-cffi::hline-RGBA (sdl:fp surface) x1 x2 y
				(sdl:r color) (sdl:g color) (sdl:b color) (sdl:a color))))

(defun draw-vline (x y1 y2 &key (surface sdl:*default-surface*) (color sdl:*default-color*))
  (if (typep color 'sdl:color)
      (sdl-gfx-cffi::vline-color (sdl:fp surface) x y1 y2
				 (map-color color))
      (sdl-gfx-cffi::vline-RGBA (sdl:fp surface) x y1 y2
				(sdl:r color) (sdl:g color) (sdl:b color) (sdl:a color))))

(defun draw-rectangle (rect &key (surface sdl:*default-surface*) (color sdl:*default-color*))
  (if (typep color 'sdl:color)
      (sdl-gfx-cffi::rectangle-color (sdl:fp surface) (sdl:x rect) (sdl:y rect) (sdl:x2 rect) (sdl:y2 rect)
				     (map-color color))
      (sdl-gfx-cffi::rectangle-RGBA (sdl:fp surface) (sdl:x rect) (sdl:y rect) (sdl:x2 rect) (sdl:y2 rect)
				    (sdl:r color) (sdl:g color) (sdl:b color) (sdl:a color))))

(defun draw-rectangle-* (x y w h &key (surface sdl:*default-surface*) (color sdl:*default-color*))
  (if (typep color 'sdl:color)
      (sdl-gfx-cffi::rectangle-color (sdl:fp surface) x y (+ x w) (+ y h)
				     (map-color color))
      (sdl-gfx-cffi::rectangle-RGBA (sdl:fp surface) x y (+ x w) (+ y h)
				    (sdl:r color) (sdl:g color) (sdl:b color) (sdl:a color))))

(defun draw-rectangle-edges-* (x1 y1 x2 y2 &key (surface sdl:*default-surface*) (color sdl:*default-color*))
  (if (typep color 'sdl:color)
      (sdl-gfx-cffi::rectangle-color (sdl:fp surface) x1 y1 x2 y2
				     (map-color color))
      (sdl-gfx-cffi::rectangle-RGBA (sdl:fp surface) x1 y1 x2 y2
				    (sdl:r color) (sdl:g color) (sdl:b color) (sdl:a color))))

(defun draw-box (rect &key (surface sdl:*default-surface*) (color sdl:*default-color*))
  (if (typep color 'sdl:color)
      (sdl-gfx-cffi::box-color (sdl:fp surface) (sdl:x rect) (sdl:y rect) (sdl:x2 rect) (sdl:y2 rect)
			       (map-color color))
      (sdl-gfx-cffi::box-RGBA (sdl:fp surface) (sdl:x rect) (sdl:y rect) (sdl:x2 rect) (sdl:y2 rect)
			      (sdl:r color) (sdl:g color) (sdl:b color) (sdl:a color))))

(defun draw-box-* (x y w h &key (surface sdl:*default-surface*) (color sdl:*default-color*))
  (if (typep color 'sdl:color)
      (sdl-gfx-cffi::box-color (sdl:fp surface) x y (+ x w) (+ y h)
			       (map-color color))
      (sdl-gfx-cffi::box-RGBA (sdl:fp surface) x y (+ x w) (+ y h)
			      (sdl:r color) (sdl:g color) (sdl:b color) (sdl:a color))))

(defun draw-box-edges-* (x1 y1 x2 y2 &key (surface sdl:*default-surface*) (color sdl:*default-color*))
  (if (typep color 'sdl:color)
      (sdl-gfx-cffi::box-color (sdl:fp surface) x1 y1 x2 y2
			       (map-color color))
      (sdl-gfx-cffi::box-RGBA (sdl:fp surface) x1 y1 x2 y2
			      (sdl:r color) (sdl:g color) (sdl:b color) (sdl:a color))))

(defun draw-line (point1 point2 &key (surface sdl:*default-surface*) (color sdl:*default-color*))
  (if (typep color 'sdl:color)
      (sdl-gfx-cffi::line-color (sdl:fp surface) (sdl:x point1) (sdl:y point1) (sdl:x point2) (sdl:y point2)
				(map-color color))
      (sdl-gfx-cffi::line-RGBA (sdl:fp surface) (sdl:x point1) (sdl:y point1) (sdl:x point2) (sdl:y point2)
			       (sdl:r color) (sdl:g color) (sdl:b color) (sdl:a color))))

(defun draw-line-* (x1 y1 x2 y2 &key (surface sdl:*default-surface*) (color sdl:*default-color*))
  (if (typep color 'sdl:color)
      (sdl-gfx-cffi::line-color (sdl:fp surface) x1 y1 x2 y2
				(map-color color))
      (sdl-gfx-cffi::line-RGBA (sdl:fp surface) x1 y1 x2 y2
			       (sdl:r color) (sdl:g color) (sdl:b color) (sdl:a color))))

(defun draw-aa-line (point1 point2 &key (surface sdl:*default-surface*) (color sdl:*default-color*))
  (if (typep color 'sdl:color)
      (sdl-gfx-cffi::aa-line-color (sdl:fp surface) (sdl:x point1) (sdl:y point1) (sdl:x point2) (sdl:y point2)
				   (map-color color))
      (sdl-gfx-cffi::aa-line-RGBA (sdl:fp surface) (sdl:x point1) (sdl:y point1) (sdl:x point2) (sdl:y point2)
				  (sdl:r color) (sdl:g color) (sdl:b color) (sdl:a color))))

(defun draw-aa-line-* (x1 y1 x2 y2 &key (surface sdl:*default-surface*) (color sdl:*default-color*))
  (if (typep color 'sdl:color)
      (sdl-gfx-cffi::aa-line-color (sdl:fp surface) x1 y1 x2 y2
				   (map-color color))
      (sdl-gfx-cffi::aa-line-RGBA (sdl:fp surface) x1 y1 x2 y2
				  (sdl:r color) (sdl:g color) (sdl:b color) (sdl:a color))))

(defun draw-circle (p1 r &key (surface sdl:*default-surface*) (color sdl:*default-color*))
  (if (typep color 'sdl:color)
      (sdl-gfx-cffi::circle-color (sdl:fp surface) (sdl:x p1) (sdl:y p1) r
				  (map-color color))
      (sdl-gfx-cffi::circle-RGBA (sdl:fp surface) (sdl:x p1) (sdl:y p1) r
				 (sdl:r color) (sdl:g color) (sdl:b color) (sdl:a color))))

(defun draw-circle-* (x y r &key (surface sdl:*default-surface*) (color sdl:*default-color*))
  (if (typep color 'sdl:color)
      (sdl-gfx-cffi::circle-color (sdl:fp surface) x y r
				  (map-color color))
      (sdl-gfx-cffi::circle-RGBA (sdl:fp surface) x y r
				 (sdl:r color) (sdl:g color) (sdl:b color) (sdl:a color))))

(defun draw-aa-circle (p1 r &key (surface sdl:*default-surface*) (color sdl:*default-color*))
  (if (typep color 'sdl:color)
      (sdl-gfx-cffi::aa-circle-color (sdl:fp surface) (sdl:x p1) (sdl:y p1) r
				     (map-color color))
      (sdl-gfx-cffi::aa-circle-RGBA (sdl:fp surface) (sdl:x p1) (sdl:y p1) r
				    (sdl:r color) (sdl:g color) (sdl:b color) (sdl:a color))))

(defun draw-aa-circle-* (x y r &key (surface sdl:*default-surface*) (color sdl:*default-color*))
  (if (typep color 'sdl:color)
      (sdl-gfx-cffi::aa-circle-color (sdl:fp surface) x y r
				     (map-color color))
      (sdl-gfx-cffi::aa-circle-RGBA (sdl:fp surface) x y r
				    (sdl:r color) (sdl:g color) (sdl:b color) (sdl:a color))))

(defun draw-filled-circle (p1 r &key (surface sdl:*default-surface*) (color sdl:*default-color*))
  (if (typep color 'sdl:color)
      (sdl-gfx-cffi::filled-circle-color (sdl:fp surface) (sdl:x p1) (sdl:y p1) r
					 (map-color color))
      (sdl-gfx-cffi::filled-circle-RGBA (sdl:fp surface) (sdl:x p1) (sdl:y p1) r
					(sdl:r color) (sdl:g color) (sdl:b color) (sdl:a color))))

(defun draw-filled-circle-* (x y r &key (surface sdl:*default-surface*) (color sdl:*default-color*))
  (if (typep color 'sdl:color)
      (sdl-gfx-cffi::filled-circle-color (sdl:fp surface) x y r
					 (map-color color))
      (sdl-gfx-cffi::filled-circle-RGBA (sdl:fp surface) x y r
					(sdl:r color) (sdl:g color) (sdl:b color) (sdl:a color))))

(defun draw-ellipse (p1 rx ry &key (surface sdl:*default-surface*) (color sdl:*default-color*))
  (if (typep color 'sdl:color)
      (sdl-gfx-cffi::ellipse-color (sdl:fp surface) (sdl:x p1) (sdl:y p1) rx ry
				   (map-color color))
      (sdl-gfx-cffi::ellipse-RGBA (sdl:fp surface) (sdl:x p1) (sdl:y p1) rx ry
				  (sdl:r color) (sdl:g color) (sdl:b color) (sdl:a color))))

(defun draw-ellipse-* (x y rx ry &key (surface sdl:*default-surface*) (color sdl:*default-color*))
  (if (typep color 'sdl:color)
      (sdl-gfx-cffi::ellipse-color (sdl:fp surface) x y rx ry
				   (map-color color))
      (sdl-gfx-cffi::ellipse-RGBA (sdl:fp surface) x y rx ry
				  (sdl:r color) (sdl:g color) (sdl:b color) (sdl:a color))))

(defun draw-aa-ellipse (p1 rx ry &key (surface sdl:*default-surface*) (color sdl:*default-color*))
  (if (typep color 'sdl:color)
      (sdl-gfx-cffi::aa-ellipse-color (sdl:fp surface) (sdl:x p1) (sdl:y p1) rx ry
				      (map-color color))
      (sdl-gfx-cffi::aa-ellipse-RGBA (sdl:fp surface) (sdl:x p1) (sdl:y p1) rx ry
				     (sdl:r color) (sdl:g color) (sdl:b color) (sdl:a color))))

(defun draw-aa-ellipse-* (x y rx ry &key (surface sdl:*default-surface*) (color sdl:*default-color*))
  (if (typep color 'sdl:color)
      (sdl-gfx-cffi::aa-ellipse-color (sdl:fp surface) x y rx ry
				      (map-color color))
      (sdl-gfx-cffi::aa-ellipse-RGBA (sdl:fp surface) x y rx ry
				     (sdl:r color) (sdl:g color) (sdl:b color) (sdl:a color))))

(defun draw-filled-ellipse (p1 rx ry &key (surface sdl:*default-surface*) (color sdl:*default-color*))
  (draw-filled-ellipse-* (sdl:x p1) (sdl:y p1) rx ry :surface surface :color color))

(defun draw-filled-ellipse-* (x y rx ry &key (surface sdl:*default-surface*) (color sdl:*default-color*))
  (unless (sdl::all-integers? x y rx ry)
    (error "ERROR, draw-filled-ellipse-*: Parameters must be of type :short."))
  (if (typep color 'sdl:color)
      (sdl-gfx-cffi::filled-ellipse-color (sdl:fp surface) x y rx ry
					  (map-color color))
      (sdl-gfx-cffi::filled-ellipse-RGBA (sdl:fp surface) x y rx ry
					 (sdl:r color) (sdl:g color) (sdl:b color) (sdl:a color))))

(defun draw-pie (p1 rad start end &key (surface sdl:*default-surface*) (color sdl:*default-color*))
  (if (typep color 'sdl:color)
      (sdl-gfx-cffi::pie-color (sdl:fp surface) (sdl:x p1) (sdl:y p1) rad start end
			       (map-color color))
      (sdl-gfx-cffi::pie-RGBA (sdl:fp surface) (sdl:x p1) (sdl:y p1) rad start end
			      (sdl:r color) (sdl:g color) (sdl:b color) (sdl:a color))))

(defun draw-pie-* (x y rad start end &key (surface sdl:*default-surface*) (color sdl:*default-color*))
  (if (typep color 'sdl:color)
      (sdl-gfx-cffi::pie-color (sdl:fp surface) x y rad start end
			       (map-color color))
      (sdl-gfx-cffi::pie-RGBA (sdl:fp surface) x y rad start end
			      (sdl:r color) (sdl:g color) (sdl:b color) (sdl:a color))))

(defun draw-filled-pie (p1 rad start end &key (surface sdl:*default-surface*) (color sdl:*default-color*))
  (if (typep color 'sdl:color)
      (sdl-gfx-cffi::filled-pie-color (sdl:fp surface) (sdl:x p1) (sdl:y p1) rad start end
				      (map-color color))
      (sdl-gfx-cffi::filled-pie-RGBA (sdl:fp surface) (sdl:x p1) (sdl:y p1) rad start end
				     (sdl:r color) (sdl:g color) (sdl:b color) (sdl:a color))))

(defun draw-filled-pie-* (x y rad start end &key (surface sdl:*default-surface*) (color sdl:*default-color*))
  (if (typep color 'sdl:color)
      (sdl-gfx-cffi::filled-pie-color (sdl:fp surface) x y rad start end
				      (map-color color))
      (sdl-gfx-cffi::filled-pie-RGBA (sdl:fp surface) x y rad start end
				     (sdl:r color) (sdl:g color) (sdl:b color) (sdl:a color))))

(defun draw-trigon (point1 point2 point3 &key (surface sdl:*default-surface*) (color sdl:*default-color*))
  (if (typep color 'sdl:color)
      (sdl-gfx-cffi::trigon-color (sdl:fp surface) (sdl:x point1) (sdl:y point1)
				  (sdl:x point2) (sdl:y point2)
				  (sdl:x point3) (sdl:y point3)
				  (map-color color))
      (sdl-gfx-cffi::trigon-RGBA (sdl:fp surface) (sdl:x point1) (sdl:y point1)
				 (sdl:x point2) (sdl:y point2)
				 (sdl:x point3) (sdl:y point3)
				 (sdl:r color) (sdl:g color) (sdl:b color) (sdl:a color))))

(defun draw-aa-trigon (point1 point2 point3 &key (surface sdl:*default-surface*) (color sdl:*default-color*))
  (if (typep color 'sdl:color)
      (sdl-gfx-cffi::aa-trigon-color (sdl:fp surface) (sdl:x point1) (sdl:y point1)
				     (sdl:x point2) (sdl:y point2)
				     (sdl:x point3) (sdl:y point3)
				     (map-color color))
      (sdl-gfx-cffi::aa-trigon-RGBA (sdl:fp surface) (sdl:x point1) (sdl:y point1)
				    (sdl:x point2) (sdl:y point2)
				    (sdl:x point3) (sdl:y point3)
				    (sdl:r color) (sdl:g color) (sdl:b color) (sdl:a color))))

(defun draw-filled-trigon (point1 point2 point3 &key (surface sdl:*default-surface*) (color sdl:*default-color*))
  (if (typep color 'sdl:color)
      (sdl-gfx-cffi::filled-trigon-color (sdl:fp surface) (sdl:x point1) (sdl:y point1)
					 (sdl:x point2) (sdl:y point2)
					 (sdl:x point3) (sdl:y point3)
					 (map-color color))
      (sdl-gfx-cffi::filled-trigon-RGBA (sdl:fp surface) (sdl:x point1) (sdl:y point1)
					(sdl:x point2) (sdl:y point2)
					(sdl:x point3) (sdl:y point3)
					(sdl:r color) (sdl:g color) (sdl:b color) (sdl:a color))))

(defun draw-polygon (points &key (surface sdl:*default-surface*) (color sdl:*default-color*))
  (unless (listp points)
    (error "draw-polygon: ~A must be a list of (x y) points." points))
  (let ((x-array (cffi:foreign-alloc :short :initial-contents (return-list-for-array points :x)))
	(y-array (cffi:foreign-alloc :short :initial-contents (return-list-for-array points :y)))
	(poly-surface nil))
    (if (typep color 'sdl:color)
	(setf poly-surface (sdl-gfx-cffi::polygon-color (sdl:fp surface) x-array y-array (length points)
							(map-color color)))
	(setf poly-surface (sdl-gfx-cffi::polygon-RGBA (sdl:fp surface) x-array y-array (length points)
						       (sdl:r color) (sdl:g color) (sdl:b color) (sdl:a color))))
    (cffi:foreign-free x-array)
    (cffi:foreign-free y-array)
    poly-surface))

(defun draw-aa-polygon (points &key (surface sdl:*default-surface*) (color sdl:*default-color*))
  (unless (listp points)
    (error "draw-aapolygon: ~A must be a list of (x y) points." points))
  (let ((x-array (cffi:foreign-alloc :short :initial-contents (return-list-for-array points :x)))
	(y-array (cffi:foreign-alloc :short :initial-contents (return-list-for-array points :y)))
	(poly-surface nil))
    (if (typep color 'sdl:color)
	(setf poly-surface (sdl-gfx-cffi::aa-polygon-color (sdl:fp surface) x-array y-array (length points)
							   (map-color color)))
	(setf poly-surface (sdl-gfx-cffi::aa-polygon-RGBA (sdl:fp surface) x-array y-array (length points)
							  (sdl:r color) (sdl:g color) (sdl:b color)
							  (sdl:a color))))
    (cffi:foreign-free x-array)
    (cffi:foreign-free y-array)
    poly-surface))

(defun draw-filled-polygon (points &key (surface sdl:*default-surface*) (color sdl:*default-color*))
  (unless (listp points)
    (error "draw-filledpolygon: ~A must be a list of (x y) points." points))
  (let ((x-array (cffi:foreign-alloc :short :initial-contents (return-list-for-array points :x)))
	(y-array (cffi:foreign-alloc :short :initial-contents (return-list-for-array points :y)))
	(poly-surface nil))
    (if (typep color 'sdl:color)
	(setf poly-surface (sdl-gfx-cffi::filled-polygon-color (sdl:fp surface) x-array y-array (length points)
							       (map-color color)))
	(setf poly-surface (sdl-gfx-cffi::filled-polygon-RGBA (sdl:fp surface) x-array y-array (length points)
							      (sdl:r color) (sdl:g color) (sdl:b color)
							      (sdl:a color))))
    (cffi:foreign-free x-array)
    (cffi:foreign-free y-array)
    poly-surface))

(defun draw-bezier (points steps &key (surface sdl:*default-surface*) (color sdl:*default-color*))
  (unless (listp points)
    (error "draw-bezier: ~A must be a list of (x y) points." points))
  (let ((x-array (cffi:foreign-alloc :short :initial-contents (return-list-for-array points :x)))
	(y-array (cffi:foreign-alloc :short :initial-contents (return-list-for-array points :y)))
	(bezier-surface nil))
    (if (typep color 'sdl:color)
	(setf bezier-surface (sdl-gfx-cffi::bezier-color (sdl:fp surface) x-array y-array (length points) steps (map-color color)))
	(setf bezier-surface (sdl-gfx-cffi::bezier-RGBA (sdl:fp surface) x-array y-array (length points) steps
							(sdl:r color) (sdl:g color) (sdl:b color) (sdl:a color))))
    (cffi:foreign-free x-array)
    (cffi:foreign-free y-array)
    bezier-surface))

(defun draw-character (c p1 &key (surface sdl:*default-surface*) (color sdl:*default-color*))
  (if (typep color 'sdl:color)
      (sdl-gfx-cffi::character-color (sdl:fp surface) (sdl:x p1) (sdl:y p1) c
				     (map-color color))
      (sdl-gfx-cffi::character-RGBA (sdl:fp surface) (sdl:x p1) (sdl:y p1) c
				    (sdl:r color) (sdl:g color) (sdl:b color) (sdl:a color))))

(defun draw-character-* (c x y &key (surface sdl:*default-surface*) (color sdl:*default-color*))
  (if (typep color 'sdl:color)
      (sdl-gfx-cffi::character-color (sdl:fp surface) x y c
				     (map-color color))
      (sdl-gfx-cffi::character-RGBA (sdl:fp surface) x y c
				    (sdl:r color) (sdl:g color) (sdl:b color) (sdl:a color))))

(defun draw-string (c p1 &key (surface sdl:*default-surface*) (color sdl:*default-color*))
  (if (typep color 'sdl:color)
      (sdl-gfx-cffi::string-color (sdl:fp surface) (sdl:x p1) (sdl:y p1) c
				  (map-color color))
      (sdl-gfx-cffi::string-RGBA (sdl:fp surface) (sdl:x p1) (sdl:y p1) c
				 (sdl:r color) (sdl:g color) (sdl:b color) (sdl:a color))))

(defun draw-string-* (c x y &key (surface sdl:*default-surface*) (color sdl:*default-color*))
  (if (typep color 'sdl:color)
      (sdl-gfx-cffi::string-color (sdl:fp surface) x y c
				  (map-color color))
      (sdl-gfx-cffi::string-RGBA (sdl:fp surface) x y c
				 (sdl:r color) (sdl:g color) (sdl:b color) (sdl:a color))))

;;; m

(defmethod map-color ((color sdl:color))
  (let ((col #x00000000))
    (setf col (logior (ash (sdl:r color) 24)
		      (ash (sdl:g color) 16)
		      (ash (sdl:b color) 8)
		      #xFF))
    col))

(defmethod map-color ((color sdl:color-a))
  (let ((col #x00000000))
    (setf col (logior (ash (sdl:r color) 24)
		      (ash (sdl:g color) 16)
		      (ash (sdl:b color) 8)
		      (sdl:a color)))
    col))

;;; r

(defun roto-zoom-surfaze (angle zoom smooth &key (surface sdl:*default-surface*))
  (sdl-gfx-cffi::rotozoomSurface (sdl:fp surface) angle zoom smooth))

(defun roto-zoom-xy (angle zoomx zoomy smooth &key (surface sdl:*default-surface*))
  (sdl-gfx-cffi::rotozoomSurfacexy (sdl:fp surface) angle zoomx zoomy smooth))

(defun roto-zoom-size (width height angle zoom)
  (cffi:with-foreign-objects ((dstwidth :int) (dstheight :int))
    (sdl-gfx-cffi::rotozoomSurfaceSize width height angle zoom dstwidth dstheight)
    (vector (cffi:mem-ref dstwidth :int) (cffi:mem-ref dstheight :int))))

(defun roto-zoom-size-xy (width height angle zoomx zoomy)
  (cffi:with-foreign-objects ((dstwidth :int) (dstheight :int))
    (sdl-gfx-cffi::rotozoomSurfaceSizeXY width height angle zoomx zoomy dstwidth dstheight)
    (vector (cffi:mem-ref dstwidth :int) (cffi:mem-ref dstheight :int))))

;;; z

(defun zoom-surface (zoomx zoomy smooth &key (surface sdl:*default-surface*))
  (sdl-gfx-cffi::zoomSurface (sdl:fp surface) zoomx zoomy smooth))

(defun zoom-surface-size (width height zoomx zoomy)
  (cffi:with-foreign-objects ((dstwidth :int) (dstheight :int))
    (sdl-gfx-cffi::zoomSurfaceSize width height zoomx zoomy dstwidth dstheight)
    (vector (cffi:mem-ref dstwidth :int) (cffi:mem-ref dstheight :int))))

