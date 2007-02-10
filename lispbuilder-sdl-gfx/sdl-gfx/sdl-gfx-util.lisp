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
  "See DRAW-PIXEL-*.

  * POSITION is the X/Y coordinate of the pixel, of type SDL:POINT."
  (check-type position sdl:point)
  (draw-pixel-* (sdl:x position) (sdl:y position) :surface surface :color color))

(defun draw-pixel-* (x y &key (surface sdl:*default-surface*) (color sdl:*default-color*))
  "Draw the color COLOR to the surface SURFACE at the specified X and Y coordiates. 

  * X Y specify the coordinates of the pixel, and are of type INTEGER.

  * SURFACE is the target surface, of type SDL:SDL-SURFACE. Binds to SDL:*DEFAULT-SURFACE* by default.

  * COLOR is the new color of the pixel, of type SDL:COLOR or SDL:COLOR-A. Binds to SDL:*DEFAULT-COLOR* by default."
  (check-type surface sdl:sdl-surface)
  (check-type color sdl:sdl-color)
  (when (typep color 'sdl:color)
    (sdl-gfx-cffi::pixel-color (sdl:fp surface) x y
			       (sdl:pack-color color)))
  (when (typep color 'sdl:color-a)
    (sdl-gfx-cffi::pixel-rgba (sdl:fp surface) x y
			      (sdl:r color) (sdl:g color) (sdl:b color) (sdl:a color))))

(defun draw-hline (x1 x2 y &key (surface sdl:*default-surface*) (color sdl:*default-color*))
  "Draws a horizontal line of color COLOR from X1 to X2 through Y onto the surface SURFACE. 

  * X1 and X2 are the horizontal start and end points of the line, of type INTEGER.

  * X is the vertical INTEGER coordinate that the horizontal line must intersect.  

  * SURFACE is the target surface, of type SDL:SDL-SURFACE. Binds to SDL:*DEFAULT-SURFACE* by default.

  * COLOR is the line color, of type SDL:COLOR or SDL:COLOR-A. Binds to SDL:*DEFAULT-COLOR* by default."
  (check-type surface sdl:sdl-surface)
  (check-type color sdl:sdl-color)
  (when (typep color 'sdl:color)
    (sdl-gfx-cffi::hline-color (sdl:fp surface) x1 x2 y
			       (sdl:pack-color color)))
  (when (typep color 'sdl:color-a)
    (sdl-gfx-cffi::hline-RGBA (sdl:fp surface) x1 x2 y
			      (sdl:r color) (sdl:g color) (sdl:b color) (sdl:a color))))

(defun draw-vline (x y1 y2 &key (surface sdl:*default-surface*) (color sdl:*default-color*))
  "Draws a vertical line of color COLOR from Y1 to Y2 through X onto the surface SURFACE. 

  * X is the horizontal INTEGER coordinate that the vertical line must intersect.  

  * Y1 and Y2 are the vertical start and end points of the line, of type INTEGER.

  * SURFACE is the target surface, of type SDL:SDL-SURFACE. Binds to SDL:*DEFAULT-SURFACE* by default.

  * COLOR is the line color, of type SDL:COLOR or SDL:COLOR-A. Binds to SDL:*DEFAULT-COLOR* by default."
  (check-type surface sdl:sdl-surface)
  (check-type color sdl:sdl-color)
  (when (typep color 'sdl:color)
    (sdl-gfx-cffi::vline-color (sdl:fp surface) x y1 y2
			       (sdl:pack-color color)))
  (when (typep color 'sdl:color-a)
    (sdl-gfx-cffi::vline-RGBA (sdl:fp surface) x y1 y2
			      (sdl:r color) (sdl:g color) (sdl:b color) (sdl:a color))))

(defun draw-rectangle (rect &key (surface sdl:*default-surface*) (color sdl:*default-color*))
  "See DRAW-RECTANGLE-*.

  * RECT is the rectangle to draw, of type SDL:RECTANGLE."
  (check-type rect sdl:rectangle)
  (draw-rectangle-* (sdl:x rect) (sdl:y rect) (sdl:width rect) (sdl:height rect) :surface surface :color color))

(defun draw-rectangle-* (x y w h &key (surface sdl:*default-surface*) (color sdl:*default-color*))
  "Draws a rectangle of color COLOR to the surface SURFACE.

  * X and Y are the INTEGER coordinates of the top-left corner of the rectangle.

  * W and H are the width and height of the rectangle, of type INTEGER.

  * SURFACE is the target surface, of type SDL:SDL-SURFACE. Binds to SDL:*DEFAULT-SURFACE* by default.

  * COLOR is the line color, of type SDL:COLOR or SDL:COLOR-A. Binds to SDL:*DEFAULT-COLOR* by default."
  (draw-rectangle-edges-* x y (+ x w) (+ y h) :surface surface :color color))

(defun draw-rectangle-edges-* (x1 y1 x2 y2 &key (surface sdl:*default-surface*) (color sdl:*default-color*))
  "Draws a rectangle of color COLOR to the surface SURFACE.

  * X1 and Y1 are the INTEGER coordinates of the top-left corner of the rectangle.

  * X2 and Y2 are the INTEGER coordinates of the bottom-right corner of the rectangle.

  * SURFACE is the target surface, of type SDL:SDL-SURFACE. Binds to SDL:*DEFAULT-SURFACE* by default.

  * COLOR is the line color, of type SDL:COLOR or SDL:COLOR-A. Binds to SDL:*DEFAULT-COLOR* by default."
  (check-type surface sdl:sdl-surface)
  (check-type color sdl:sdl-color)
  (when (typep color 'sdl:color)
    (sdl-gfx-cffi::rectangle-color (sdl:fp surface) x1 y1 x2 y2
				   (sdl:pack-color color)))
  (when (typep color 'sdl:color-a)
    (sdl-gfx-cffi::rectangle-RGBA (sdl:fp surface) x1 y1 x2 y2
				  (sdl:r color) (sdl:g color) (sdl:b color) (sdl:a color))))

(defun draw-box (rect &key (surface sdl:*default-surface*) (color sdl:*default-color*))
  "See DRAW-BOX-*.

  * RECT is filled rectangle to draw, of type SDL:RECTANGLE."
  (check-type rect sdl:rectangle)
  (draw-box-* (sdl:x rect) (sdl:y rect) (sdl:width rect) (sdl:height rect) :surface surface :color color))

(defun draw-box-* (x y w h &key (surface sdl:*default-surface*) (color sdl:*default-color*))
  "Draws a filled rectangle of color COLOR to surface SURFACE.

  * X and Y are the INTEGER coordinates of the top-left corner of the rectangle.

  * W and H are the width and height of the rectangle, of type INTEGER.

  * SURFACE is the target surface, of type SDL:SDL-SURFACE. Binds to SDL:*DEFAULT-SURFACE* by default.

  * COLOR is the line color, of type SDL:COLOR or SDL:COLOR-A. Binds to SDL:*DEFAULT-COLOR* by default."
  (draw-box-edges-* x y (+ x w) (+ y h) :surface surface :color color))

(defun draw-box-edges-* (x1 y1 x2 y2 &key (surface sdl:*default-surface*) (color sdl:*default-color*))
  "Draws a filled rectangle of color COLOR to the surface SURFACE.

  * X1 and Y1 are the INTEGER coordinates of the top-left corner of the rectangle.

  * X2 and Y2 are the INTEGER coordinates of the bottom-right corner of the rectangle.

  * SURFACE is the target surface, of type SDL:SDL-SURFACE. Binds to SDL:*DEFAULT-SURFACE* by default.

  * COLOR is the fill color, of type SDL:COLOR or SDL:COLOR-A. Binds to SDL:*DEFAULT-COLOR* by default."
  (check-type surface sdl:sdl-surface)
  (check-type color sdl:sdl-color)
  (when (typep color 'sdl:color)
    (sdl-gfx-cffi::box-color (sdl:fp surface) x1 y1 x2 y2
			     (sdl:pack-color color)))
  (when (typep color 'sdl:color-a)
    (sdl-gfx-cffi::box-RGBA (sdl:fp surface) x1 y1 x2 y2
			    (sdl:r color) (sdl:g color) (sdl:b color) (sdl:a color))))

(defun draw-line (point1 point2 &key (surface sdl:*default-surface*) (color sdl:*default-color*))
  "See DRAW-LINE-*

  * POINT1 and POINT2 are the start and end x/y co-ordinates of the line, of type SDL:POINT."
  (sdl:check-types sdl:point point1 point2)
  (draw-line-* (sdl:x point1) (sdl:y point1) (sdl:x point2) (sdl:y point2) :surface surface :color color))

(defun draw-line-* (x1 y1 x2 y2 &key (surface sdl:*default-surface*) (color sdl:*default-color*))
  "Draws a line of color COLOR to the surface SURFACE.

  * X1 Y1 are the start X/Y coordinates of the line, of type INTEGER.

  * X2 Y2 are the end X/Y coordinates of the line, of type INTEGER.

  * SURFACE is the target surface, of type SDL:SDL-SURFACE. Binds to SDL:*DEFAULT-SURFACE* by default.

  * COLOR is the fill color, of type SDL:COLOR or SDL:COLOR-A. Binds to SDL:*DEFAULT-COLOR* by default."
  (check-type surface sdl:sdl-surface)
  (check-type color sdl:sdl-color)
  (when (typep color 'sdl:color)
    (sdl-gfx-cffi::line-color (sdl:fp surface) x1 y1 x2 y2
			      (sdl:pack-color color)))
  (when (typep color 'sdl:color-a)
    (sdl-gfx-cffi::line-RGBA (sdl:fp surface) x1 y1 x2 y2
			     (sdl:r color) (sdl:g color) (sdl:b color) (sdl:a color))))

(defun draw-aa-line (point1 point2 &key (surface sdl:*default-surface*) (color sdl:*default-color*))
  "See DRAW-AA-LINE-*

  * POINT1 and POINT2 are the start and end x/y co-ordinates of the line, of type SDL:POINT."
  (sdl:check-types sdl:point point1 point2)
  (draw-aa-line-* (sdl:x point1) (sdl:y point1) (sdl:x point2) (sdl:y point2) :surface surface :color color))

(defun draw-aa-line-* (x1 y1 x2 y2 &key (surface sdl:*default-surface*) (color sdl:*default-color*))
  "Draws an antialiased line of color COLOR to the surface SURFACE.

  * X1 Y1 are the start X/Y coordinates of the line, of type INTEGER.

  * X2 Y2 are the end X/Y coordinates of the line, of type INTEGER.

  * SURFACE is the target surface, of type SDL:SDL-SURFACE. Binds to SDL:*DEFAULT-SURFACE* by default.

  * COLOR is the fill color, of type SDL:COLOR or SDL:COLOR-A. Binds to SDL:*DEFAULT-COLOR* by default."
  (check-type surface sdl:sdl-surface)
  (check-type color sdl:sdl-color)
  (when (typep color 'sdl:color)
    (sdl-gfx-cffi::aa-line-color (sdl:fp surface) x1 y1 x2 y2
				 (sdl:pack-color color)))
  (when (typep color 'sdl:color-a)
    (sdl-gfx-cffi::aa-line-RGBA (sdl:fp surface) x1 y1 x2 y2
				(sdl:r color) (sdl:g color) (sdl:b color) (sdl:a color))))

(defun draw-circle (p1 r &key (surface sdl:*default-surface*) (color sdl:*default-color*))
  "See DRAW-CIRCLE-*

  * P1 is the X/Y coordinate of the center of the circle, of type SDL:POINT."
  (check-type p1 sdl:point)
  (draw-circle-* (sdl:x p1) (sdl:y p1) r :surface surface :color color))

(defun draw-circle-* (x y r &key (surface sdl:*default-surface*) (color sdl:*default-color*))
  "Draws a circle of color COLOR to the surface SURFACE.
Note that this is not a 'filled circle'. Only the circle circumference is drawn.

  * X and Y specify the center coordinate of the circle, of type INTEGER.

  * R is the circle radius, of type INTEGER.

  * SURFACE is the target surface, of type SDL:SDL-SURFACE. Binds to SDL:*DEFAULT-SURFACE* by default.

  * COLOR is the circumference color, of type SDL:COLOR or SDL:COLOR-A. Binds to SDL:*DEFAULT-COLOR* by default."
  (check-type surface sdl:sdl-surface)
  (check-type color sdl:sdl-color)
  (when (typep color 'sdl:color)
    (sdl-gfx-cffi::circle-color (sdl:fp surface) x y r
				(sdl:pack-color color)))
  (when (typep color 'sdl:color-a)
    (sdl-gfx-cffi::circle-RGBA (sdl:fp surface) x y r
			       (sdl:r color) (sdl:g color) (sdl:b color) (sdl:a color))))

(defun draw-aa-circle (p1 r &key (surface sdl:*default-surface*) (color sdl:*default-color*))
  "See DRAW-AA-CIRCLE-*

  * P1 is the X/Y coordinate of the center of the antialiased circle, of type SDL:POINT."
  (check-type p1 sdl:point)
  (draw-aa-circle-* (sdl:x p1) (sdl:y p1) r :surface surface :color color))

(defun draw-aa-circle-* (x y r &key (surface sdl:*default-surface*) (color sdl:*default-color*))
  "Draws an antialiased circle of color COLOR to the surface SURFACE.
Note that this is not a 'filled circle'. Only the circle circumference is drawn.

  * X and Y specify the center coordinate of the circle, of type INTEGER.

  * R is the circle radius, of type INTEGER.

  * SURFACE is the target surface, of type SDL:SDL-SURFACE. Binds to SDL:*DEFAULT-SURFACE* by default.

  * COLOR is the circumference color, of type SDL:COLOR or SDL:COLOR-A. Binds to SDL:*DEFAULT-COLOR* by default."
  (check-type surface sdl:sdl-surface)
  (check-type color sdl:sdl-color)
  (when (typep color 'sdl:color)
    (sdl-gfx-cffi::aa-circle-color (sdl:fp surface) x y r
				   (sdl:pack-color color)))
  (when (typep color 'sdl:color-a)
    (sdl-gfx-cffi::aa-circle-RGBA (sdl:fp surface) x y r
				  (sdl:r color) (sdl:g color) (sdl:b color) (sdl:a color))))

(defun draw-filled-circle (p1 r &key (surface sdl:*default-surface*) (color sdl:*default-color*))
  "See DRAW-FILLED-CIRCLE-*

  * P1 is the X/Y coordinate of the center of the filled circle, of type SDL:POINT."
  (check-type p1 sdl:point)
  (draw-filled-circle-* (sdl:x p1) (sdl:y p1) r :surface surface :color color))

(defun draw-filled-circle-* (x y r &key (surface sdl:*default-surface*) (color sdl:*default-color*))
  "Draws an filled circle of color COLOR to the surface SURFACE.

  * X and Y specify the center coordinate of the circle, of type INTEGER.

  * R is the circle radius, of type INTEGER.

  * SURFACE is the target surface, of type SDL:SDL-SURFACE. Binds to SDL:*DEFAULT-SURFACE* by default.

  * COLOR is the fill color, of type SDL:COLOR or SDL:COLOR-A. Binds to SDL:*DEFAULT-COLOR* by default."
  (check-type surface sdl:sdl-surface)
  (check-type color sdl:sdl-color)
  (when (typep color 'sdl:color)
    (sdl-gfx-cffi::filled-circle-color (sdl:fp surface) x y r
				       (sdl:pack-color color)))
  (when (typep color 'sdl:color-a)
    (sdl-gfx-cffi::filled-circle-RGBA (sdl:fp surface) x y r
				      (sdl:r color) (sdl:g color) (sdl:b color) (sdl:a color))))

(defun draw-ellipse (p1 rx ry &key (surface sdl:*default-surface*) (color sdl:*default-color*))
  "See DRAW-ELLIPSE-*

  * P1 is the X/Y coordinate of the center of the ellipse, of type SDL:POINT."
  (check-type p1 sdl:point)
  (draw-ellipse-* (sdl:x p1) (sdl:y p1) rx ry :surface surface :color color))

(defun draw-ellipse-* (x y rx ry &key (surface sdl:*default-surface*) (color sdl:*default-color*))
  "Draws an ellipse of color COLOR to the surface SURFACE.
Note that this is not a 'filled ellipse'. Only the ellipse circumference is drawn.

  * X and Y specify the center coordinate of the ellipse, of type INTEGER.

  * RX and RY is the ellipse radius, of type INTEGER.

  * SURFACE is the target surface, of type SDL:SDL-SURFACE. Binds to SDL:*DEFAULT-SURFACE* by default.

  * COLOR is the circumference color, of type SDL:COLOR or SDL:COLOR-A. Binds to SDL:*DEFAULT-COLOR* by default."
  (check-type surface sdl:sdl-surface)
  (check-type color sdl:sdl-color)
  (when (typep color 'sdl:color)
    (sdl-gfx-cffi::ellipse-color (sdl:fp surface) x y rx ry
				 (sdl:pack-color color)))
  (when (typep color 'sdl:color-a)
    (sdl-gfx-cffi::ellipse-RGBA (sdl:fp surface) x y rx ry
				(sdl:r color) (sdl:g color) (sdl:b color) (sdl:a color))))

(defun draw-aa-ellipse (p1 rx ry &key (surface sdl:*default-surface*) (color sdl:*default-color*))
  "See DRAW-AA-ELLIPSE-*

  * P1 is the X/Y coordinate of the center of the antialiased ellipse, of type SDL:POINT."
  (check-type p1 sdl:point)
  (draw-aa-ellipse-* (sdl:x p1) (sdl:y p1) rx ry :surface surface :color color))

(defun draw-aa-ellipse-* (x y rx ry &key (surface sdl:*default-surface*) (color sdl:*default-color*))
  "Draws an antialiased ellipse of color COLOR to the surface SURFACE.
Note that this is not a 'filled ellipse'. Only the ellipse circumference is drawn.

  * X and Y specify the center coordinate of the ellipse, of type INTEGER.

  * RX and RY is the ellipse radius, of type INTEGER.

  * SURFACE is the target surface, of type SDL:SDL-SURFACE. Binds to SDL:*DEFAULT-SURFACE* by default.

  * COLOR is the circumference color, of type SDL:COLOR or SDL:COLOR-A. Binds to SDL:*DEFAULT-COLOR* by default."
  (check-type surface sdl:sdl-surface)
  (check-type color sdl:sdl-color)
  (when (typep color 'sdl:color)
    (sdl-gfx-cffi::aa-ellipse-color (sdl:fp surface) x y rx ry
				    (sdl:pack-color color)))
  (when (typep color 'sdl:color-a)
    (sdl-gfx-cffi::aa-ellipse-RGBA (sdl:fp surface) x y rx ry
				   (sdl:r color) (sdl:g color) (sdl:b color) (sdl:a color))))

(defun draw-filled-ellipse (p1 rx ry &key (surface sdl:*default-surface*) (color sdl:*default-color*))
  "See DRAW-FILLED-ELLIPSE-*

  * P1 is the X/Y coordinate of the center of the filled ellipse, of type SDL:POINT."
  (check-type p1 sdl:point)
  (draw-filled-ellipse-* (sdl:x p1) (sdl:y p1) rx ry :surface surface :color color))

(defun draw-filled-ellipse-* (x y rx ry &key (surface sdl:*default-surface*) (color sdl:*default-color*))
  "Draws a filled ellipse of color COLOR to the surface SURFACE.

  * X and Y specify the center coordinate of the ellipse, of type INTEGER.

  * RX and RY is the ellipse radius, of type INTEGER.

  * SURFACE is the target surface, of type SDL:SDL-SURFACE. Binds to SDL:*DEFAULT-SURFACE* by default.

  * COLOR is the fill color, of type SDL:COLOR or SDL:COLOR-A. Binds to SDL:*DEFAULT-COLOR* by default."
  (check-type surface sdl:sdl-surface)
  (check-type color sdl:sdl-color)
  (when (typep color 'sdl:color)
    (sdl-gfx-cffi::filled-ellipse-color (sdl:fp surface) x y rx ry
					(sdl:pack-color color)))
  (when (typep color 'sdl:color-a)
    (sdl-gfx-cffi::filled-ellipse-RGBA (sdl:fp surface) x y rx ry
				       (sdl:r color) (sdl:g color) (sdl:b color) (sdl:a color))))

(defun draw-pie (p1 rad start end &key (surface sdl:*default-surface*) (color sdl:*default-color*))
  "See DRAW-PIE-*

  * P1 is the X/Y coordinate of the center of the pie, of type SDL:POINT."
  (check-type p1 sdl:point)
  (draw-pie-* (sdl:x p1) (sdl:y p1) rad start end :surface surface :color color))

(defun draw-pie-* (x y rad start end &key (surface sdl:*default-surface*) (color sdl:*default-color*))
  "Draws a pie of color COLOR to the surface SURFACE.
Note that this is not a 'filled pie'. Only the pie circumference is drawn.

  * X and Y specify the center coordinate of the pie, of type INTEGER.

  * RAD is the pie radius, of type INTEGER.

  * START is the pie start, of type INTEGER.

  * END is the pie end, of type INTEGER.

  * SURFACE is the target surface, of type SDL:SDL-SURFACE. Binds to SDL:*DEFAULT-SURFACE* by default.

  * COLOR is the circumference color, of type SDL:COLOR or SDL:COLOR-A. Binds to SDL:*DEFAULT-COLOR* by default."
  (check-type surface sdl:sdl-surface)
  (check-type color sdl:sdl-color)
  (when (typep color 'sdl:color)
    (sdl-gfx-cffi::pie-color (sdl:fp surface) x y rad start end
			     (sdl:pack-color color)))
  (when (typep color 'sdl:color-a)
    (sdl-gfx-cffi::pie-RGBA (sdl:fp surface) x y rad start end
			    (sdl:r color) (sdl:g color) (sdl:b color) (sdl:a color))))

(defun draw-filled-pie (p1 rad start end &key (surface sdl:*default-surface*) (color sdl:*default-color*))
  "See DRAW-PIE-*

  * P1 is the X/Y coordinate of the center of the filled pie, of type SDL:POINT."
  (check-type p1 sdl:point)
  (draw-filled-pie-* (sdl:x p1) (sdl:y p1) rad start end :surface surface :color color))

(defun draw-filled-pie-* (x y rad start end &key (surface sdl:*default-surface*) (color sdl:*default-color*))
    "Draws a filled pie of color COLOR to the surface SURFACE.

  * X and Y specify the center coordinate of the pie, of type INTEGER.

  * RAD is the pie radius, of type INTEGER.

  * START is the pie start, of type INTEGER.

  * END is the pie end, of type INTEGER.

  * SURFACE is the target surface, of type SDL:SDL-SURFACE. Binds to SDL:*DEFAULT-SURFACE* by default.

  * COLOR is the fill color, of type SDL:COLOR or SDL:COLOR-A. Binds to SDL:*DEFAULT-COLOR* by default."
  (check-type surface sdl:sdl-surface)
  (check-type color sdl:sdl-color)
  (when (typep color 'sdl:color)
    (sdl-gfx-cffi::filled-pie-color (sdl:fp surface) x y rad start end
				    (sdl:pack-color color)))
  (when (typep color 'sdl:color-a)
    (sdl-gfx-cffi::filled-pie-RGBA (sdl:fp surface) x y rad start end
				   (sdl:r color) (sdl:g color) (sdl:b color) (sdl:a color))))

(defun draw-trigon (point1 point2 point3 &key (surface sdl:*default-surface*) (color sdl:*default-color*))
  "Draw a trigon, of color COLOR to surface SURFACE.
Note: The trigon is not filled, only the edges are drawn.

  * POINT1, POINT2 and POINT3 specify the vertices of the trigon, of type SDL:POINT.

  * SURFACE is the target surface, of type SDL:SDL-SURFACE. Binds to SDL:*DEFAULT-SURFACE* by default.

  * COLOR is the circumference color, of type SDL:COLOR or SDL:COLOR-A. Binds to SDL:*DEFAULT-COLOR* by default."
  (sdl:check-types sdl:point point1 point2 point3)
  (check-type surface sdl:sdl-surface)
  (check-type color sdl:sdl-color)
  (when (typep color 'sdl:color)
    (sdl-gfx-cffi::trigon-color (sdl:fp surface) (sdl:x point1) (sdl:y point1)
				(sdl:x point2) (sdl:y point2)
				(sdl:x point3) (sdl:y point3)
				(sdl:pack-color color)))
  (when (typep color 'sdl:color-a)
    (sdl-gfx-cffi::trigon-RGBA (sdl:fp surface) (sdl:x point1) (sdl:y point1)
			       (sdl:x point2) (sdl:y point2)
			       (sdl:x point3) (sdl:y point3)
			       (sdl:r color) (sdl:g color) (sdl:b color) (sdl:a color))))

(defun draw-aa-trigon (point1 point2 point3 &key (surface sdl:*default-surface*) (color sdl:*default-color*))
  "Draw an antialiased trigon, of color COLOR to surface SURFACE.
Note: The trigon is not filled, only the edges are drawn.

  * POINT1, POINT2 and POINT3 specify the vertices of the trigon, of type SDL:POINT.

  * SURFACE is the target surface, of type SDL:SDL-SURFACE. Binds to SDL:*DEFAULT-SURFACE* by default.

  * COLOR is the circumference color, of type SDL:COLOR or SDL:COLOR-A. Binds to SDL:*DEFAULT-COLOR* by default."
  (sdl:check-types sdl:point point1 point2 point3)
  (check-type surface sdl:sdl-surface)
  (check-type color sdl:sdl-color)
  (when (typep color 'sdl:color)
    (sdl-gfx-cffi::aa-trigon-color (sdl:fp surface) (sdl:x point1) (sdl:y point1)
				   (sdl:x point2) (sdl:y point2)
				   (sdl:x point3) (sdl:y point3)
				   (sdl:pack-color color)))
  (when (typep color 'sdl:color-a)
    (sdl-gfx-cffi::aa-trigon-RGBA (sdl:fp surface) (sdl:x point1) (sdl:y point1)
				  (sdl:x point2) (sdl:y point2)
				  (sdl:x point3) (sdl:y point3)
				  (sdl:r color) (sdl:g color) (sdl:b color) (sdl:a color))))

(defun draw-filled-trigon (point1 point2 point3 &key (surface sdl:*default-surface*) (color sdl:*default-color*))
  "Draw a filled trigon, of color COLOR to surface SURFACE.

  * POINT1, POINT2 and POINT3 specify the vertices of the trigon, of type SDL:POINT.

  * SURFACE is the target surface, of type SDL:SDL-SURFACE. Binds to SDL:*DEFAULT-SURFACE* by default.

  * COLOR is the fill color, of type SDL:COLOR or SDL:COLOR-A. Binds to SDL:*DEFAULT-COLOR* by default."
  (sdl:check-types sdl:point point1 point2 point3)
  (check-type surface sdl:sdl-surface)
  (check-type color sdl:sdl-color)
  (when (typep color 'sdl:color)
    (sdl-gfx-cffi::filled-trigon-color (sdl:fp surface) (sdl:x point1) (sdl:y point1)
				       (sdl:x point2) (sdl:y point2)
				       (sdl:x point3) (sdl:y point3)
				       (sdl:pack-color color)))
  (when (typep color 'sdl:color-a)
    (sdl-gfx-cffi::filled-trigon-RGBA (sdl:fp surface) (sdl:x point1) (sdl:y point1)
				      (sdl:x point2) (sdl:y point2)
				      (sdl:x point3) (sdl:y point3)
				      (sdl:r color) (sdl:g color) (sdl:b color) (sdl:a color))))

(defun draw-polygon (points &key (surface sdl:*default-surface*) (color sdl:*default-color*))
  "Draw a polygon, of color COLOR to surface SURFACE.
Note: The polygon is not filled, only the edges are drawn.

  * POINT1, POINT2 and POINT3 THAT specify the vertices of the polygon, of type SDL:POINT.
of the polygon, of type SDL:POINT.specify the vertices of the polygon, of type SDL:POINT.

  * SURFACE is the target surface, of type SDL:SDL-SURFACE. Binds to SDL:*DEFAULT-SURFACE* by default.

  * COLOR is the circumference color, of type SDL:COLOR or SDL:COLOR-A. Binds to SDL:*DEFAULT-COLOR* by default."
  (check-type points (and list (not null)) "POINTs must be a LIST of SDL:POINTs")
  (check-type surface sdl:sdl-surface)
  (check-type color sdl:sdl-color)
  (let ((x-array (cffi:foreign-alloc :short :initial-contents (return-list-for-array points :x)))
	(y-array (cffi:foreign-alloc :short :initial-contents (return-list-for-array points :y)))
	(poly-surface nil))
    (when (typep color 'sdl:color)
      (setf poly-surface (sdl-gfx-cffi::polygon-color (sdl:fp surface) x-array y-array (length points)
						      (sdl:pack-color color))))
    (when (typep color 'sdl:color-a)
      (setf poly-surface (sdl-gfx-cffi::polygon-RGBA (sdl:fp surface) x-array y-array (length points)
						     (sdl:r color) (sdl:g color) (sdl:b color) (sdl:a color))))
    (cffi:foreign-free x-array)
    (cffi:foreign-free y-array)
    poly-surface))

(defun draw-aa-polygon (points &key (surface sdl:*default-surface*) (color sdl:*default-color*))
  (check-type points (and list (not null)) "POINTs must be a LIST of SDL:POINTs")
  (check-type surface sdl:sdl-surface)
  (check-type color sdl:sdl-color)
  (let ((x-array (cffi:foreign-alloc :short :initial-contents (return-list-for-array points :x)))
	(y-array (cffi:foreign-alloc :short :initial-contents (return-list-for-array points :y)))
	(poly-surface nil))
    (when (typep color 'sdl:color)
      (setf poly-surface (sdl-gfx-cffi::aa-polygon-color (sdl:fp surface) x-array y-array (length points)
							 (sdl:pack-color color))))
    (when (typep color 'sdl:color-a)
      (setf poly-surface (sdl-gfx-cffi::aa-polygon-RGBA (sdl:fp surface) x-array y-array (length points)
							(sdl:r color) (sdl:g color) (sdl:b color)
							(sdl:a color))))
    (cffi:foreign-free x-array)
    (cffi:foreign-free y-array)
    poly-surface))

(defun draw-filled-polygon (points &key (surface sdl:*default-surface*) (color sdl:*default-color*))
  (check-type points (and list (not null)) "POINTs must be a LIST of SDL:POINTs")
  (check-type surface sdl:sdl-surface)
  (check-type color sdl:sdl-color)
  (let ((x-array (cffi:foreign-alloc :short :initial-contents (return-list-for-array points :x)))
	(y-array (cffi:foreign-alloc :short :initial-contents (return-list-for-array points :y)))
	(poly-surface nil))
    (when (typep color 'sdl:color)
      (setf poly-surface (sdl-gfx-cffi::filled-polygon-color (sdl:fp surface) x-array y-array (length points)
							     (sdl:pack-color color))))
    (when (typep color 'sdl:color-a)
      (setf poly-surface (sdl-gfx-cffi::filled-polygon-RGBA (sdl:fp surface) x-array y-array (length points)
							      (sdl:r color) (sdl:g color) (sdl:b color)
							      (sdl:a color))))
    (cffi:foreign-free x-array)
    (cffi:foreign-free y-array)
    poly-surface))

(defun draw-bezier (points steps &key (surface sdl:*default-surface*) (color sdl:*default-color*))
  (check-type points (and list (not null)) "POINTs must be a LIST of SDL:POINTs")
  (check-type surface sdl:sdl-surface)
  (check-type color sdl:sdl-color)
  (let ((x-array (cffi:foreign-alloc :short :initial-contents (return-list-for-array points :x)))
	(y-array (cffi:foreign-alloc :short :initial-contents (return-list-for-array points :y)))
	(bezier-surface nil))
    (when (typep color 'sdl:color)
      (setf bezier-surface (sdl-gfx-cffi::bezier-color (sdl:fp surface) x-array y-array (length points) steps (sdl:pack-color color))))
    (when (typep color 'sdl:color-a)
      (setf bezier-surface (sdl-gfx-cffi::bezier-RGBA (sdl:fp surface) x-array y-array (length points) steps
						      (sdl:r color) (sdl:g color) (sdl:b color) (sdl:a color))))
    (cffi:foreign-free x-array)
    (cffi:foreign-free y-array)
    bezier-surface))

;;; r

(defun roto-zoom-surfaze (angle zoom smooth &key (surface sdl:*default-surface*))
  (check-type surface sdl:sdl-surface)
  (sdl-gfx-cffi::rotozoomSurface (sdl:fp surface) angle zoom smooth))

(defun roto-zoom-xy (angle zoomx zoomy smooth &key (surface sdl:*default-surface*))
  (check-type surface sdl:sdl-surface)
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
  (check-type surface sdl:sdl-surface)
  (sdl-gfx-cffi::zoomSurface (sdl:fp surface) zoomx zoomy smooth))

(defun zoom-surface-size (width height zoomx zoomy)
  (cffi:with-foreign-objects ((dstwidth :int) (dstheight :int))
    (sdl-gfx-cffi::zoomSurfaceSize width height zoomx zoomy dstwidth dstheight)
    (vector (cffi:mem-ref dstwidth :int) (cffi:mem-ref dstheight :int))))

