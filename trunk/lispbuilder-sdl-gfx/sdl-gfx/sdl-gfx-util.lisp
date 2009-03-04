;; SDL_gfx v2.0.13 library. Uses CFFI for foreign function interfacing...
;; (C)2006 Luke Crook <luke@balooga.com>, Justin Heyes-Jones <justinhj@gmail.com>
;; Thanks to Frank Buss and Surendra Singh
;; see COPYING for license
;; This file contains some useful functions for using SDL_gfx from Common lisp
;; using sdl_gfx.lisp (the CFFI wrapper)

(in-package #:lispbuilder-sdl-gfx)


;;; Macros

;;; w

(defmacro with-bezier ((&optional (style nil) (segments 10)) &body body)
  (let ((point-list (gensym "point-list-")))
    `(let ((,point-list nil))
       (labels ((add-vertex (point)
		  (setf ,point-list (append ,point-list (list point))))
		(add-vertex-* (x y)
		  (add-vertex (sdl:point :x x :y y))))
	 ,@body)
       (draw-bezier ,point-list :segments ,segments :style ,style))))

(defmacro with-curve ((&optional (style :solid) (segments 10)) &body body)
  (let ((point-list (gensym "point-list-")))
    `(let ((,point-list nil))
       (labels ((add-vertex (point)
		  (setf ,point-list (append ,point-list (list point))))
		(add-vertex-* (x y)
		  (add-vertex (sdl:point :x x :y y))))
	 ,@body)
       (draw-curve ,point-list :style ,style :segments ,segments))))

(defmacro with-shape ((&optional (style :SOLID)) &body body)
  (let ((point-list (gensym "point-list-")))
    `(let ((,point-list nil))
       (labels ((add-vertex (point)
		  (setf ,point-list (append ,point-list (list point))))
		(add-vertex-* (x y)
		  (add-vertex (sdl:point :x x :y y))))
	 ,@body)
       (draw-shape ,point-list :style ,style))))

;;; Functions

;;; d

(defun draw-curve (vertices &key (surface sdl:*default-surface*) (color sdl:*default-color*)
		   (style :SOLID) (segments 20))
  (do* ((p1 vertices (cdr p1))
	(p2 (cdr p1) (cdr p1))
	(p3 (cdr p2) (cdr p2))
	(p4 (cdr p3) (cdr p3)))
       ((or (null p4) (null p3) (null p2) (null p1)))
    (draw-shape (sdl::generate-curve (first p1) (first p2) (first p3) (first p4) segments)
		:style style :surface surface :color color)))

(defun draw-shape (vertices &key (surface sdl:*default-surface*) (color sdl:*default-color*) (style :solid))
  (case style
    (:solid
     (do* ((p1 vertices (cdr p1))
	   (p2 (cdr p1) (cdr p1)))
	  ((or (null p2)
	       (null p1)))
       (draw-line (first p1) (first p2) :surface surface :color color)))
    (:dash
     (do* ((p1 vertices (if (cdr p1)
			  (cddr p1)
			  nil))
	   (p2 (cdr p1) (cdr p1)))
	  ((or (null p2)
	       (null p1)))
       (draw-line (first p1) (first p2) :surface surface :color color)))
    (:points
     (loop for point in vertices
	do (draw-pixel point :surface surface :color color)))))

(defun draw-pixel (position &key (surface sdl:*default-surface*) (color sdl:*default-color*))
  (check-type position sdl:point)
  (draw-pixel-* (sdl:x position) (sdl:y position) :surface surface :color color))

(defun draw-pixel-* (x y &key (surface sdl:*default-surface*) (color sdl:*default-color*))
  (unless surface
    (setf surface sdl:*default-display*))
  (check-type surface sdl:sdl-surface)
  (check-type color sdl:color)
  (if (sdl:a color)
      (sdl-gfx-cffi::pixel-rgba (sdl:fp surface) x y
			      (sdl:r color) (sdl:g color) (sdl:b color) (sdl:a color))
      (sdl-gfx-cffi::pixel-color (sdl:fp surface) x y
			       (sdl:pack-color color))))

(defun draw-hline (x0 x1 y &key (surface sdl:*default-surface*) (color sdl:*default-color*))
  (unless surface
    (setf surface sdl:*default-display*))
  (check-type surface sdl:sdl-surface)
  (check-type color sdl:color)
  (if (sdl:a color)
    (sdl-gfx-cffi::hline-RGBA (sdl:fp surface) x0 x1 y
			      (sdl:r color) (sdl:g color) (sdl:b color) (sdl:a color))
    (sdl-gfx-cffi::hline-color (sdl:fp surface) x0 x1 y
			       (sdl:pack-color color))))

(defun draw-vline (x y0 y1 &key (surface sdl:*default-surface*) (color sdl:*default-color*))
  (unless surface
    (setf surface sdl:*default-display*))
  (check-type surface sdl:sdl-surface)
  (check-type color sdl:color)
  (if (sdl:a color)
      (sdl-gfx-cffi::vline-RGBA (sdl:fp surface) x y0 y1
			      (sdl:r color) (sdl:g color) (sdl:b color) (sdl:a color))
      (sdl-gfx-cffi::vline-color (sdl:fp surface) x y0 y1
			       (sdl:pack-color color))))

(defun draw-rectangle (rect &key (surface sdl:*default-surface*) (color sdl:*default-color*))
  (check-type rect sdl:rectangle)
  (draw-rectangle-* (sdl:x rect) (sdl:y rect) (sdl:width rect) (sdl:height rect) :surface surface :color color))

(defun draw-rectangle-* (x y w h &key (surface sdl:*default-surface*) (color sdl:*default-color*))
  (draw-rectangle-edges-* x y (+ x w) (+ y h) :surface surface :color color))

(defun draw-rectangle-edges-* (x0 y0 x1 y1 &key (surface sdl:*default-surface*) (color sdl:*default-color*))
  (unless surface
    (setf surface sdl:*default-display*))
  (check-type surface sdl:sdl-surface)
  (check-type color sdl:color)
  (if (sdl:a color)
      (sdl-gfx-cffi::rectangle-RGBA (sdl:fp surface) x0 y0 x1 y1
				  (sdl:r color) (sdl:g color) (sdl:b color) (sdl:a color))
      (sdl-gfx-cffi::rectangle-color (sdl:fp surface) x0 y0 x1 y1
				   (sdl:pack-color color))))

(defun draw-box (rect &key (surface sdl:*default-surface*) (color sdl:*default-color*))
  (check-type rect sdl:rectangle)
  (draw-box-* (sdl:x rect) (sdl:y rect) (sdl:width rect) (sdl:height rect) :surface surface :color color))

(defun draw-box-* (x y w h &key (surface sdl:*default-surface*) (color sdl:*default-color*))
  (draw-box-edges-* x y (+ x w) (+ y h) :surface surface :color color))

(defun draw-box-edges-* (x0 y0 x1 y1 &key (surface sdl:*default-surface*) (color sdl:*default-color*))
  (unless surface
    (setf surface sdl:*default-display*))
  (check-type surface sdl:sdl-surface)
  (check-type color sdl:color)
  (if (sdl:a color)
      (sdl-gfx-cffi::box-RGBA (sdl:fp surface) x0 y0 x1 y1
			    (sdl:r color) (sdl:g color) (sdl:b color) (sdl:a color))
      (sdl-gfx-cffi::box-color (sdl:fp surface) x0 y0 x1 y1
			     (sdl:pack-color color))))

(defun draw-line (p1 p2 &key (surface sdl:*default-surface*) (color sdl:*default-color*) (aa nil))
  (sdl:check-types sdl:point p1 p2)
  (draw-line-* (sdl:x p1) (sdl:y p1) (sdl:x p2) (sdl:y p2) :surface surface :color color :aa aa))

(defun draw-line-* (x0 y0 x1 y1 &key (surface sdl:*default-surface*) (color sdl:*default-color*) (aa nil))
  (unless surface
    (setf surface sdl:*default-display*))
  (check-type surface sdl:sdl-surface)
  (check-type color sdl:color)
  (if aa
      (draw-aa-line-* x0 y0 x1 y1 :surface surface :color color)
      (if (sdl:a color)
	  (sdl-gfx-cffi::line-RGBA (sdl:fp surface) x0 y0 x1 y1
				   (sdl:r color) (sdl:g color) (sdl:b color) (sdl:a color))
	  (sdl-gfx-cffi::line-color (sdl:fp surface) x0 y0 x1 y1
				    (sdl:pack-color color)))))

(defun draw-aa-line (p1 p2 &key (surface sdl:*default-surface*) (color sdl:*default-color*))
  (sdl:check-types sdl:point p1 p2)
  (draw-aa-line-* (sdl:x p1) (sdl:y p1) (sdl:x p2) (sdl:y p2) :surface surface :color color))

(defun draw-aa-line-* (x0 y0 x1 y1 &key (surface sdl:*default-surface*) (color sdl:*default-color*))
  (unless surface
    (setf surface sdl:*default-display*))
  (check-type surface sdl:sdl-surface)
  (check-type color sdl:color)
  (if (sdl:a color)
      (sdl-gfx-cffi::aa-line-RGBA (sdl:fp surface) x0 y0 x1 y1
				(sdl:r color) (sdl:g color) (sdl:b color) (sdl:a color))
      (sdl-gfx-cffi::aa-line-color (sdl:fp surface) x0 y0 x1 y1
				 (sdl:pack-color color))))

(defun draw-circle (p1 r &key (surface sdl:*default-surface*) (color sdl:*default-color*) (aa nil))
  (check-type p1 sdl:point)
  (draw-circle-* (sdl:x p1) (sdl:y p1) r :surface surface :color color :aa aa))

(defun draw-circle-* (x y r &key (surface sdl:*default-surface*) (color sdl:*default-color*) (aa nil))
  (unless surface
    (setf surface sdl:*default-display*))
  (check-type surface sdl:sdl-surface)
  (check-type color sdl:color)
  (if aa
      (draw-aa-circle-* x y r :surface surface :color color)
      (if (sdl:a color)
	  (sdl-gfx-cffi::circle-RGBA (sdl:fp surface) x y r
				     (sdl:r color) (sdl:g color) (sdl:b color) (sdl:a color))
	  (sdl-gfx-cffi::circle-color (sdl:fp surface) x y r
				      (sdl:pack-color color)))))

(defun draw-aa-circle (p1 r &key (surface sdl:*default-surface*) (color sdl:*default-color*))
  (check-type p1 sdl:point)
  (draw-aa-circle-* (sdl:x p1) (sdl:y p1) r :surface surface :color color))

(defun draw-aa-circle-* (x y r &key (surface sdl:*default-surface*) (color sdl:*default-color*))
  (unless surface
    (setf surface sdl:*default-display*))
  (check-type surface sdl:sdl-surface)
  (check-type color sdl:color)
  (if (sdl:a color)
      (sdl-gfx-cffi::aa-circle-RGBA (sdl:fp surface) x y r
				  (sdl:r color) (sdl:g color) (sdl:b color) (sdl:a color))
      (sdl-gfx-cffi::aa-circle-color (sdl:fp surface) x y r
				   (sdl:pack-color color))))

(defun draw-filled-circle (p1 r &key (surface sdl:*default-surface*) (color sdl:*default-color*))
  (check-type p1 sdl:point)
  (draw-filled-circle-* (sdl:x p1) (sdl:y p1) r :surface surface :color color))

(defun draw-filled-circle-* (x y r &key (surface sdl:*default-surface*) (color sdl:*default-color*))
  (unless surface
    (setf surface sdl:*default-display*))
  (check-type surface sdl:sdl-surface)
  (check-type color sdl:color)
  (if (sdl:a color)
      (sdl-gfx-cffi::filled-circle-RGBA (sdl:fp surface) x y r
				      (sdl:r color) (sdl:g color) (sdl:b color) (sdl:a color))
      (sdl-gfx-cffi::filled-circle-color (sdl:fp surface) x y r
				       (sdl:pack-color color))))

(defun draw-ellipse (p1 rx ry &key (surface sdl:*default-surface*) (color sdl:*default-color*) (aa nil))
  (check-type p1 sdl:point)
  (draw-ellipse-* (sdl:x p1) (sdl:y p1) rx ry :surface surface :color color :aa aa))

(defun draw-ellipse-* (x y rx ry &key (surface sdl:*default-surface*) (color sdl:*default-color*) (aa nil))
  (unless surface
    (setf surface sdl:*default-display*))
  (check-type surface sdl:sdl-surface)
  (check-type color sdl:color)
  (if aa
      (draw-aa-ellipse-* x y rx ry :surface surface :color color)
      (if (sdl:a color)
	  (sdl-gfx-cffi::ellipse-RGBA (sdl:fp surface) x y rx ry
				      (sdl:r color) (sdl:g color) (sdl:b color) (sdl:a color))
	  (sdl-gfx-cffi::ellipse-color (sdl:fp surface) x y rx ry
				       (sdl:pack-color color)))))

(defun draw-aa-ellipse (p1 rx ry &key (surface sdl:*default-surface*) (color sdl:*default-color*))
  (check-type p1 sdl:point)
  (draw-aa-ellipse-* (sdl:x p1) (sdl:y p1) rx ry :surface surface :color color))

(defun draw-aa-ellipse-* (x y rx ry &key (surface sdl:*default-surface*) (color sdl:*default-color*))
  (unless surface
    (setf surface sdl:*default-display*))
  (check-type surface sdl:sdl-surface)
  (check-type color sdl:color)
  (if (sdl:a color)
      (sdl-gfx-cffi::aa-ellipse-RGBA (sdl:fp surface) x y rx ry
				   (sdl:r color) (sdl:g color) (sdl:b color) (sdl:a color))
      (sdl-gfx-cffi::aa-ellipse-color (sdl:fp surface) x y rx ry
				    (sdl:pack-color color))))

(defun draw-filled-ellipse (p1 rx ry &key (surface sdl:*default-surface*) (color sdl:*default-color*))
  (check-type p1 sdl:point)
  (draw-filled-ellipse-* (sdl:x p1) (sdl:y p1) rx ry :surface surface :color color))

(defun draw-filled-ellipse-* (x y rx ry &key (surface sdl:*default-surface*) (color sdl:*default-color*))
  (unless surface
    (setf surface sdl:*default-display*))
  (check-type surface sdl:sdl-surface)
  (check-type color sdl:color)
  (if (sdl:a color)
      (sdl-gfx-cffi::filled-ellipse-RGBA (sdl:fp surface) x y rx ry
					 (sdl:r color) (sdl:g color) (sdl:b color) (sdl:a color))
      (sdl-gfx-cffi::filled-ellipse-color (sdl:fp surface) x y rx ry
					  (sdl:pack-color color))))

(defun draw-pie (p1 rad start end &key (surface sdl:*default-surface*) (color sdl:*default-color*))
  (check-type p1 sdl:point)
  (draw-pie-* (sdl:x p1) (sdl:y p1) rad start end :surface surface :color color))

(defun draw-pie-* (x y rad start end &key (surface sdl:*default-surface*) (color sdl:*default-color*))
  (unless surface
    (setf surface sdl:*default-display*))
  (check-type surface sdl:sdl-surface)
  (check-type color sdl:color)
  (if (sdl:a color)
      (sdl-gfx-cffi::pie-RGBA (sdl:fp surface) x y rad start end
			    (sdl:r color) (sdl:g color) (sdl:b color) (sdl:a color))
      (sdl-gfx-cffi::pie-color (sdl:fp surface) x y rad start end
			     (sdl:pack-color color))))

(defun draw-filled-pie (p1 rad start end &key (surface sdl:*default-surface*) (color sdl:*default-color*))
  (check-type p1 sdl:point)
  (draw-filled-pie-* (sdl:x p1) (sdl:y p1) rad start end :surface surface :color color))

(defun draw-filled-pie-* (x y rad start end &key (surface sdl:*default-surface*) (color sdl:*default-color*))
  (unless surface
    (setf surface sdl:*default-display*))
  (check-type surface sdl:sdl-surface)
  (check-type color sdl:color)
  (if (sdl:a color)
      (sdl-gfx-cffi::filled-pie-RGBA (sdl:fp surface) x y rad start end
				   (sdl:r color) (sdl:g color) (sdl:b color) (sdl:a color))
      (sdl-gfx-cffi::filled-pie-color (sdl:fp surface) x y rad start end
				    (sdl:pack-color color))))

(defun draw-trigon (p1 p2 p3 &key (surface sdl:*default-surface*) (color sdl:*default-color*) (aa nil))
  (sdl:check-types sdl:point p1 p2 p3)
  (unless surface
    (setf surface sdl:*default-display*))
  (check-type surface sdl:sdl-surface)
  (check-type color sdl:color)
  (if aa
      (draw-aa-trigon p1 p2 p3 :surface surface :color color)
      (if (sdl:a color)
	  (sdl-gfx-cffi::trigon-RGBA (sdl:fp surface) (sdl:x p1) (sdl:y p1)
				     (sdl:x p2) (sdl:y p2)
				     (sdl:x p3) (sdl:y p3)
				     (sdl:r color) (sdl:g color) (sdl:b color) (sdl:a color))
	  (sdl-gfx-cffi::trigon-color (sdl:fp surface) (sdl:x p1) (sdl:y p1)
				      (sdl:x p2) (sdl:y p2)
				      (sdl:x p3) (sdl:y p3)
				      (sdl:pack-color color)))))

(defun draw-aa-trigon (p1 p2 p3 &key (surface sdl:*default-surface*) (color sdl:*default-color*))
  (sdl:check-types sdl:point p1 p2 p3)
  (unless surface
    (setf surface sdl:*default-display*))
  (check-type surface sdl:sdl-surface)
  (check-type color sdl:color)
  (if (sdl:a color)
      (sdl-gfx-cffi::aa-trigon-RGBA (sdl:fp surface) (sdl:x p1) (sdl:y p1)
				  (sdl:x p2) (sdl:y p2)
				  (sdl:x p3) (sdl:y p3)
				  (sdl:r color) (sdl:g color) (sdl:b color) (sdl:a color))
      (sdl-gfx-cffi::aa-trigon-color (sdl:fp surface) (sdl:x p1) (sdl:y p1)
				   (sdl:x p2) (sdl:y p2)
				   (sdl:x p3) (sdl:y p3)
				   (sdl:pack-color color))))

(defun draw-filled-trigon (p1 p2 p3 &key (surface sdl:*default-surface*) (color sdl:*default-color*))
  (sdl:check-types sdl:point p1 p2 p3)
  (unless surface
    (setf surface sdl:*default-display*))
  (check-type surface sdl:sdl-surface)
  (check-type color sdl:color)
  (if (sdl:a color)
      (sdl-gfx-cffi::filled-trigon-RGBA (sdl:fp surface) (sdl:x p1) (sdl:y p1)
				      (sdl:x p2) (sdl:y p2)
				      (sdl:x p3) (sdl:y p3)
				      (sdl:r color) (sdl:g color) (sdl:b color) (sdl:a color))
      (sdl-gfx-cffi::filled-trigon-color (sdl:fp surface) (sdl:x p1) (sdl:y p1)
				       (sdl:x p2) (sdl:y p2)
				       (sdl:x p3) (sdl:y p3)
				       (sdl:pack-color color))))

(defun draw-polygon (vertices &key (surface sdl:*default-surface*) (color sdl:*default-color*) (aa nil))
  (check-type vertices (and list (not null)) "Vertices must be a LIST of SDL:POINTs")
  (unless surface
    (setf surface sdl:*default-display*))
  (check-type surface sdl:sdl-surface)
  (check-type color sdl:color)
  (if aa
      (draw-aa-polygon vertices :surface surface :color color)
      (let ((x-array (cffi:foreign-alloc :short :initial-contents (return-list-for-array vertices :x)))
	    (y-array (cffi:foreign-alloc :short :initial-contents (return-list-for-array vertices :y)))
	    (poly-surface nil))
	(if (sdl:a color)
	    (setf poly-surface (sdl-gfx-cffi::polygon-RGBA (sdl:fp surface) x-array y-array (length vertices)
							   (sdl:r color) (sdl:g color) (sdl:b color) (sdl:a color)))
	    (setf poly-surface (sdl-gfx-cffi::polygon-color (sdl:fp surface) x-array y-array (length vertices)
							    (sdl:pack-color color))))
	(cffi:foreign-free x-array)
	(cffi:foreign-free y-array)
	poly-surface)))

(defun draw-aa-polygon (vertices &key (surface sdl:*default-surface*) (color sdl:*default-color*))
  (check-type vertices (and list (not null)) "Vertices must be a LIST of SDL:POINTs")
  (unless surface
    (setf surface sdl:*default-display*))
  (check-type surface sdl:sdl-surface)
  (check-type color sdl:color)
  (let ((x-array (cffi:foreign-alloc :short :initial-contents (return-list-for-array vertices :x)))
	(y-array (cffi:foreign-alloc :short :initial-contents (return-list-for-array vertices :y)))
	(poly-surface nil))
    (if (sdl:a color)
	(setf poly-surface (sdl-gfx-cffi::aa-polygon-RGBA (sdl:fp surface) x-array y-array (length vertices)
							(sdl:r color) (sdl:g color) (sdl:b color)
							(sdl:a color)))
	(setf poly-surface (sdl-gfx-cffi::aa-polygon-color (sdl:fp surface) x-array y-array (length vertices)
							 (sdl:pack-color color))))
    (cffi:foreign-free x-array)
    (cffi:foreign-free y-array)
    poly-surface))

(defun draw-filled-polygon (vertices &key (surface sdl:*default-surface*) (color sdl:*default-color*))
  (check-type vertices (and list (not null)) "Vertices must be a LIST of SDL:POINTs")
  (unless surface
    (setf surface sdl:*default-display*))
  (check-type surface sdl:sdl-surface)
  (check-type color sdl:color)
  (let ((x-array (cffi:foreign-alloc :short :initial-contents (return-list-for-array vertices :x)))
	(y-array (cffi:foreign-alloc :short :initial-contents (return-list-for-array vertices :y)))
	(poly-surface nil))
    (if (sdl:a color)
	(setf poly-surface (sdl-gfx-cffi::filled-polygon-RGBA (sdl:fp surface) x-array y-array (length vertices)
							      (sdl:r color) (sdl:g color) (sdl:b color)
							      (sdl:a color)))
	(setf poly-surface (sdl-gfx-cffi::filled-polygon-color (sdl:fp surface) x-array y-array (length vertices)
							     (sdl:pack-color color))))
    (cffi:foreign-free x-array)
    (cffi:foreign-free y-array)
    poly-surface))

(defun draw-bezier (vertices &key (surface sdl:*default-surface*) (color sdl:*default-color*) (segments 20) (style nil))
  (declare (ignorable style))
  (check-type vertices (and list (not null)) "Vertices must be a LIST of SDL:POINTs")
  (unless surface
    (setf surface sdl:*default-display*))
  (check-type surface sdl:sdl-surface)
  (check-type color sdl:color)
  (let ((x-array (cffi:foreign-alloc :short :initial-contents (return-list-for-array vertices :x)))
	(y-array (cffi:foreign-alloc :short :initial-contents (return-list-for-array vertices :y)))
	(bezier-surface nil))
    (if (sdl:a color)
	(setf bezier-surface (sdl-gfx-cffi::bezier-RGBA (sdl:fp surface) x-array y-array (length vertices) segments
						      (sdl:r color) (sdl:g color) (sdl:b color) (sdl:a color)))
	(setf bezier-surface (sdl-gfx-cffi::bezier-color (sdl:fp surface) x-array y-array (length vertices) segments (sdl:pack-color color))))
    (cffi:foreign-free x-array)
    (cffi:foreign-free y-array)
    bezier-surface))

;;; r

(defun rotate-surface (degrees &key (surface sdl:*default-surface*) (free nil) (zoom 1) (smooth nil))
  (declare (ignorable free))
  (roto-zoom-surface degrees zoom smooth :surface surface))

(defun rotate-surface-xy (degrees &key (surface sdl:*default-surface*) (free nil) (zoomx 1) (zoomy 1) (smooth nil))
  (declare (ignorable free))
  (roto-zoom-xy degrees zoomx zoomy smooth :surface surface))

(defun roto-zoom-surface (angle zoom smooth &key (surface sdl:*default-surface*))
  (check-type surface sdl:surface)
  (make-instance 'sdl:surface :fp (sdl-gfx-cffi::rotozoomSurface (sdl:fp surface) angle zoom smooth)))

(defun roto-zoom-xy (angle zoomx zoomy smooth &key (surface sdl:*default-surface*))
  (check-type surface sdl:surface)
  (make-instance 'sdl:surface :fp (sdl-gfx-cffi::rotozoomSurfacexy (sdl:fp surface) angle zoomx zoomy smooth)))

(defun roto-zoom-size (width height angle zoom)
  (cffi:with-foreign-objects ((dstwidth :int) (dstheight :int))
    (sdl-gfx-cffi::rotozoomSurfaceSize width height angle zoom dstwidth dstheight)
    (vector (cffi:mem-ref dstwidth :int) (cffi:mem-ref dstheight :int))))

(defun roto-zoom-size-xy (width height angle zoomx zoomy)
  (cffi:with-foreign-objects ((dstwidth :int) (dstheight :int))
    (sdl-gfx-cffi::rotozoomSurfaceSizeXY width height angle zoomx zoomy dstwidth dstheight)
    (vector (cffi:mem-ref dstwidth :int) (cffi:mem-ref dstheight :int))))

;;; z

(defun zoom-surface (zoomx zoomy &key (surface sdl:*default-surface*) (smooth nil) (free nil))
  (declare (ignorable free))
  (check-type surface sdl:surface)
  (make-instance 'sdl:surface :fp (sdl-gfx-cffi::zoomSurface (sdl:fp surface) zoomx zoomy smooth)))

(defun zoom-surface-size (width height zoomx zoomy)
  (cffi:with-foreign-objects ((dstwidth :int) (dstheight :int))
    (sdl-gfx-cffi::zoomSurfaceSize width height zoomx zoomy dstwidth dstheight)
    (vector (cffi:mem-ref dstwidth :int) (cffi:mem-ref dstheight :int))))

;;; s

;; SDL_gfx 2.0.16
;; (defun shrink-surface (factor-x factor-y &key (surface sdl:*default-surface*))
;;   "Returns a new 32bit or 8bit SDl:SURFACE from the SDL:SURFACE :SURFACE.
;;     FACTOR-X and FACTOR-Y are the shrinking ratios \(i.e. 2=1/2 the size,
;;     3=1/3 the size, etc.\) The destination surface is antialiased by averaging
;;     the source box RGBA or Y information. If the surface is not 8bit
;;     or 32bit RGBA/ABGR it will be converted into a 32bit RGBA format on the fly."
;;   (check-type surface sdl:surface)
;;   (sdl:surface (sdl-gfx-cffi::shrinkSurface (sdl:fp surface) factor-x factor-y)))

