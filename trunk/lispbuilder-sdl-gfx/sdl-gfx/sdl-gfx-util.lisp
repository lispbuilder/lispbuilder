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
  "Draw a bezier curve of color `\*DEFAULT-COLOR\*` to the surface `\*DEFAULT-SURFACE\*`.
The shape of the Bezier curve is defined by control points. 
A control point is a vertex containing an X and Y coordinate pair.

##### Local Methods

A vertex may be added using:
* `ADD-VERTEX` which accepts an `SDL:POINT`, or 
* `ADD-VERTEX-*` which is the x/y spread version

`ADD-VERTEX` and `ADD-VERTEX-*` are valid only within the scop of `WITH-BEZIER`.

##### Parameters

* `SEGMENTS` is the number of segments used to draw the Bezier curve.  
Default is 10 segments if unspecified. The greater the number of segments, 
the smoother the curve.

##### Example

    \(SDL:WITH-COLOR \(COL \(SDL:COLOR\)\)
       \(WITH-BEZIER \(30\)
         \(ADD-VERTEX-* 60  40\)
         \(ADD-VERTEX-* 160 10\)
         \(ADD-VERTEX-* 170 150\)
         \(ADD-VERTEX-* 60  150\)\)\)"
  (let ((point-list (gensym "point-list-")))
    `(let ((,point-list nil))
       (labels ((add-vertex (point)
		  (setf ,point-list (append ,point-list (list point))))
		(add-vertex-* (x y)
		  (add-vertex (sdl:point :x x :y y))))
	 ,@body)
       (draw-bezier ,point-list ,segments))))

(defmacro with-curve ((shape-type &optional (segments 10)) &body body)
  "Draw a Cattmul-Rom spline of color `\*DEFAULT-COLOR\*` to the surface `\*DEFAULT-SURFACE\*`.
The shape of the curve is defined by waypoints. 
A waypoint is a vertex containing an X and Y coordinate pair.

##### Local Methods

A vertex may be added using:
* `ADD-VERTEX` which accepts an `SDL:POINT`, or 
* `ADD-VERTEX-*` which is the x/y spread version

`ADD-VERTEX` and `ADD-VERTEX-*` are valid only within the scope of `WITH-CURVE`.

##### Parameters

* `SHAPE-TYPE` describes the line style used to draw the curve and may be one of 
`:LINE-STRIP`, `:LINES`, or `:POINTS`. Use `:LINE-STRIP` to draw a single continuous line through the specified waypoints. Use `:LINES` to draw a line between alternate waypoint pairs. Use `:POINTS` to draw a single pixel at each waypoint.
* `SEGMENTS` is the number of segments used to draw the Catmull-Rom spline.  
Default is 10 segments if unspecified. The greater the number of segments, 
the smoother the spline.

##### Example

    \(SDL:WITH-COLOR \(COL \(SDL:COLOR\)\)
       \(WITH-CURVE \(:LINE-STRIP 30\)
         \(ADD-VERTEX-* 60  40\)
         \(ADD-VERTEX-* 160 10\)
         \(ADD-VERTEX-* 170 150\)
         \(ADD-VERTEX-* 60  150\)\)\)"
  (let ((point-list (gensym "point-list-")))
    `(let ((,point-list nil))
       (labels ((add-vertex (point)
		  (setf ,point-list (append ,point-list (list point))))
		(add-vertex-* (x y)
		  (add-vertex (sdl:point :x x :y y))))
	 ,@body)
       (draw-curve ,point-list ,shape-type ,segments))))

(defmacro with-shape ((shape-type) &body body)
  "Draw a polygon of color `\*DEFAULT-COLOR\*` to the surface `\*DEFAULT-SURFACE\*`.

##### Local Methods

A vertex may be added using:
* `ADD-VERTEX` which accepts an `SDL:POINT`, or 
* `ADD-VERTEX-*` which is the x/y spread version

ADD-VERTEX and ADD-VERTEX-* are valid only within the scop of WITH-SHAPE.

##### Parameters

* `SHAPE-TYPE` describes the line style used to draw the shape and may be one of 
`:LINE-STRIP`, `:LINES`, or `:POINTS`. Use `:LINE-STRIP` to draw a single continuous line through the specified waypoints. Use `:LINES` to draw a line between alternate waypoint pairs. Use `:POINTS` to draw a single pixel at each waypoint.

##### Example

    \(SDL:WITH-COLOR \(COL \(SDL:COLOR\)\)
       \(WITH-SHAPE \(:POINTS\)
         \(ADD-VERTEX-* 60  40\)
         \(ADD-VERTEX-* 160 10\)
         \(ADD-VERTEX-* 170 150\)
         \(ADD-VERTEX-* 60  150\)\)\)"
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

(defun draw-curve (points type segments &key (surface sdl:*default-surface*) (color sdl:*default-color*))
  "Draw a Cattmul-Rom spline using color `COLOR` to the surface `SURFACE`. The shape of the curve is defined by waypoints. 
A waypoint is a vertex containing an X and Y coordinate pair.

##### Parameters

* `POINTS` is a list of waypoints or vetices for the spline, of type `SDL:POINT`
* `TYPE` describes the line style used to draw the curve and may be one of 
`:LINE-STRIP`, `:LINES`, or `:POINTS`. Use `:LINE-STRIP` to draw a single continuous line through the specified waypoints. Use `:LINES` to draw a line between alternate waypoint pairs. Use `:POINTS` to draw a single pixel at each waypoint.
* `SEGMENTS` is the number of segments used to draw the Catmull-Rom spline.  
Default is 10 segments if unspecified. The greater the number of segments, 
the smoother the spline.
* `SURFACE` is the target surface, of type `SDL:SDL-SURFACE`. 
* `COLOR` is the line color, of type `SDL:COLOR` or `SDL:COLOR-A`. Bound to `SDL:\*DEFAULT-COLOR\*` if unspecified.

##### Example

    \(DRAW-CURVE \(LIST \(SDL:POINT :X 60  :Y 40\)
	    	  \(SDL:POINT :X 160 :Y 10\)
		  \(SDL:POINT :X 170 :Y 150\)
		  \(SDL:POINT :X 60  :Y 150\)\)
	    :LINE-STRIP
	    10\)"
  (do* ((p1 points (cdr p1))
	(p2 (cdr p1) (cdr p1))
	(p3 (cdr p2) (cdr p2))
	(p4 (cdr p3) (cdr p3)))
       ((or (null p4) (null p3) (null p2) (null p1)))
    (draw-shape (sdl:calculate-curve (first p1) (first p2) (first p3) (first p4) segments)
		type :surface surface :color color)))

(defun draw-shape (points type &key (surface sdl:*default-surface*) (color sdl:*default-color*))
  "Draw a polygon of color `COLOR` to the surface `SURFACE` using the vertices in `POINTS`.

##### Parameters

* `POINTS` is a list of vertices, of type `SDL:POINT`
* `TYPE` describes the line style used to draw the polygon and may be one of 
`:LINE-STRIP`, `:LINES`, or `:POINTS`. Use `:LINE-STRIP` to draw a single continuous line through the specified waypoints. Use `:LINES` to draw a line between alternate waypoint pairs. Use `:POINTS` to draw a single pixel at each waypoint.
* `SURFACE` is the target surface, of type `SDL:SDL-SURFACE`. Bound to `SDL:\*DEFAULT-SURFACE\*` if unspecified.
* `COLOR` is the line color, of type `SDL:COLOR` or `SDL:COLOR-A`. Bound to `SDL:\*DEFAULT-COLOR\*` if unspecified.

##### Example

    \(DRAW-SHAPE \(LIST \(SDL:POINT :X 60  :Y 40\)
		    \(SDL:POINT :X 160 :Y 10\)
		    \(SDL:POINT :X 170 :Y 150\)
   		    \(SDL:POINT :X 60  :Y 150\)\)
	    :LINE-STRIP\)"
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
	do (draw-pixel point :surface surface :color color)))))

(defun draw-pixel (position &key (surface sdl:*default-surface*) (color sdl:*default-color*))
  "See [DRAW-PIXEL-*](#draw-pixel-*).

##### Parameters

* `POSITION` is the X/Y coordinate of the pixel, of type `SDL:POINT`."
  (check-type position sdl:point)
  (draw-pixel-* (sdl:x position) (sdl:y position) :surface surface :color color))

(defun draw-pixel-* (x y &key (surface sdl:*default-surface*) (color sdl:*default-color*))
  "Draw a single pixel of color `COLOR` to the surface `SURFACE` at the specified `X` and `Y` coordiates. 

##### Parameters

* `X` and `Y` specify the coordinates of the pixel, and are of type `INTEGER`.
* `SURFACE` is the target surface, of type `SDL:SDL-SURFACE`. Bound to `SDL:\*DEFAULT-SURFACE\*` if unspecified.
* `COLOR` is the pixel color, of type `SDL:COLOR` or `SDL:COLOR-A`. Bound to `SDL:\*DEFAULT-COLOR\*` if unspecified."
  (unless surface
    (setf surface sdl:*default-display*))
  (check-type surface sdl:sdl-surface)
  (check-type color sdl:sdl-color)
  (when (typep color 'sdl:color)
    (sdl-gfx-cffi::pixel-color (sdl:fp surface) x y
			       (sdl:pack-color color)))
  (when (typep color 'sdl:color-a)
    (sdl-gfx-cffi::pixel-rgba (sdl:fp surface) x y
			      (sdl:r color) (sdl:g color) (sdl:b color) (sdl:a color))))

(defun draw-hline (x0 x1 y &key (surface sdl:*default-surface*) (color sdl:*default-color*))
  "Draw a horizontal line of color `COLOR` from `X0` to `X1` through `Y` onto the surface `SURFACE`. 

##### Parameters

* `X0` and `X1` are the horizontal start and end points of the line, of type `INTEGER`.
* `Y` is the vertical `INTEGER` coordinate that the horizontal line must intersect.  
* `SURFACE` is the target surface, of type `SDL:SDL-SURFACE`. Bound to `SDL:\*DEFAULT-SURFACE\*` if unspecified.
* `COLOR` is the line color, of type `SDL:COLOR` or `SDL:COLOR-A`. Bound to `SDL:\*DEFAULT-COLOR\*` if unspecified."
  (unless surface
    (setf surface sdl:*default-display*))
  (check-type surface sdl:sdl-surface)
  (check-type color sdl:sdl-color)
  (when (typep color 'sdl:color)
    (sdl-gfx-cffi::hline-color (sdl:fp surface) x0 x1 y
			       (sdl:pack-color color)))
  (when (typep color 'sdl:color-a)
    (sdl-gfx-cffi::hline-RGBA (sdl:fp surface) x0 x1 y
			      (sdl:r color) (sdl:g color) (sdl:b color) (sdl:a color))))

(defun draw-vline (x y0 y1 &key (surface sdl:*default-surface*) (color sdl:*default-color*))
  "Draw a vertical line of color `COLOR` from `Y0` to `Y1` through `X` onto the surface `SURFACE`. 

##### Parameters

* `X` is the horizontal `INTEGER` coordinate that the vertical line must intersect.  
* `Y0` and `Y1` are the vertical start and end points of the line, of type `INTEGER`.
* `SURFACE` is the target surface, of type `SDL:SDL-SURFACE`. Bound to `SDL:\*DEFAULT-SURFACE\*` if unspecified.
* `COLOR` is the line color, of type `SDL:COLOR` or `SDL:COLOR-A`. Bound to `SDL:\*DEFAULT-COLOR\*` if unspecified."
  (unless surface
    (setf surface sdl:*default-display*))
  (check-type surface sdl:sdl-surface)
  (check-type color sdl:sdl-color)
  (when (typep color 'sdl:color)
    (sdl-gfx-cffi::vline-color (sdl:fp surface) x y0 y1
			       (sdl:pack-color color)))
  (when (typep color 'sdl:color-a)
    (sdl-gfx-cffi::vline-RGBA (sdl:fp surface) x y0 y1
			      (sdl:r color) (sdl:g color) (sdl:b color) (sdl:a color))))

(defun draw-rectangle (rect &key (surface sdl:*default-surface*) (color sdl:*default-color*))
  "See [DRAW-RECTANGLE-*](#draw-rectangle-*).

##### Parameters

* `RECT` is the rectangle to draw, of type `SDL:RECTANGLE`."
  (check-type rect sdl:rectangle)
  (draw-rectangle-* (sdl:x rect) (sdl:y rect) (sdl:width rect) (sdl:height rect) :surface surface :color color))

(defun draw-rectangle-* (x y w h &key (surface sdl:*default-surface*) (color sdl:*default-color*))
  "Draw a rectangle outline of color `COLOR` to the surface `SURFACE`.

##### Parameters

* `X` and `Y` are the `INTEGER` coordinates of the top-left corner of the rectangle.
* `W` and `H` are the width and height of the rectangle, of type `INTEGER`.
* `SURFACE` is the target surface, of type `SDL:SDL-SURFACE`. Bound to `SDL:\*DEFAULT-SURFACE\*` if unspecified.
* `COLOR` is the line color, of type `SDL:COLOR` or `SDL:COLOR-A`. Bound to `SDL:\*DEFAULT-COLOR\*` if unspecified."
  (draw-rectangle-edges-* x y (+ x w) (+ y h) :surface surface :color color))

(defun draw-rectangle-edges-* (x0 y0 x1 y1 &key (surface sdl:*default-surface*) (color sdl:*default-color*))
  "Draw a rectangle outline of color `COLOR` to the surface `SURFACE`.

##### Parameters

* `X0` and `Y0` are the `INTEGER` coordinates of the top-left corner of the rectangle.
* `X1` and `Y1` are the `INTEGER` coordinates of the bottom-right corner of the rectangle.
* `SURFACE` is the target surface, of type `SDL:SDL-SURFACE`. Bound to `SDL:\*DEFAULT-SURFACE\*` if unspecified.
* `COLOR` is the line color, of type `SDL:COLOR` or `SDL:COLOR-A`. Bound to `SDL:\*DEFAULT-COLOR\*` if unspecified."
  (unless surface
    (setf surface sdl:*default-display*))
  (check-type surface sdl:sdl-surface)
  (check-type color sdl:sdl-color)
  (when (typep color 'sdl:color)
    (sdl-gfx-cffi::rectangle-color (sdl:fp surface) x0 y0 x1 y1
				   (sdl:pack-color color)))
  (when (typep color 'sdl:color-a)
    (sdl-gfx-cffi::rectangle-RGBA (sdl:fp surface) x0 y0 x1 y1
				  (sdl:r color) (sdl:g color) (sdl:b color) (sdl:a color))))

(defun draw-box (rect &key (surface sdl:*default-surface*) (color sdl:*default-color*))
  "See [DRAW-BOX-*](#draw-box-*).

##### Parameters
* `RECT` is the rectangle to fill, of type `SDL:RECTANGLE`."
  (check-type rect sdl:rectangle)
  (draw-box-* (sdl:x rect) (sdl:y rect) (sdl:width rect) (sdl:height rect) :surface surface :color color))

(defun draw-box-* (x y w h &key (surface sdl:*default-surface*) (color sdl:*default-color*))
  "Draws a filled rectangle of color `COLOR` to surface `SURFACE`.

##### Parameters

* `X` and `Y` are the `INTEGER` coordinates of the top-left corner of the rectangle.
* `W` and `H` are the width and height of the rectangle, of type `INTEGER`.
* `SURFACE` is the target surface, of type `SDL:SDL-SURFACE`. Bound to `SDL:\*DEFAULT-SURFACE\*` if unspecified.
* `COLOR` is the fill color, of type `SDL:COLOR` or `SDL:COLOR-A`. Bound to `SDL:\*DEFAULT-COLOR\*` if unspecified."
  (draw-box-edges-* x y (+ x w) (+ y h) :surface surface :color color))

(defun draw-box-edges-* (x0 y0 x1 y1 &key (surface sdl:*default-surface*) (color sdl:*default-color*))
  "Draws a filled rectangle of color `COLOR` to the surface `SURFACE`.

##### Parameters

* `X0` and `Y0` are the `INTEGER` coordinates of the top-left corner of the rectangle.
* `X1` and `Y1` are the `INTEGER` coordinates of the bottom-right corner of the rectangle.
* `SURFACE` is the target surface, of type `SDL:SDL-SURFACE`. Bound to `SDL:\*DEFAULT-SURFACE\*` if unspecified.
* `COLOR` is the fill color, of type `SDL:COLOR` or `SDL:COLOR-A`. Bound to `SDL:\*DEFAULT-COLOR\*` if unspecified."
  (unless surface
    (setf surface sdl:*default-display*))
  (check-type surface sdl:sdl-surface)
  (check-type color sdl:sdl-color)
  (when (typep color 'sdl:color)
    (sdl-gfx-cffi::box-color (sdl:fp surface) x0 y0 x1 y1
			     (sdl:pack-color color)))
  (when (typep color 'sdl:color-a)
    (sdl-gfx-cffi::box-RGBA (sdl:fp surface) x0 y0 x1 y1
			    (sdl:r color) (sdl:g color) (sdl:b color) (sdl:a color))))

(defun draw-line (p1 p2 &key (surface sdl:*default-surface*) (color sdl:*default-color*))
  "See [DRAW-LINE-*](#draw-line-*).

##### Parameters

* `P1` and `P2` are the start and end x/y co-ordinates of the line, of type `SDL:POINT`."
  (sdl:check-types sdl:point p1 p2)
  (draw-line-* (sdl:x p1) (sdl:y p1) (sdl:x p2) (sdl:y p2) :surface surface :color color))

(defun draw-line-* (x0 y0 x1 y1 &key (surface sdl:*default-surface*) (color sdl:*default-color*))
  "Draws a line of color `COLOR` to the surface `SURFACE`.

##### Parameters

* `X0` `Y0` are the start X/Y coordinates of the line, of type `INTEGER`.
* `X1` `Y1` are the end X/Y coordinates of the line, of type `INTEGER`.
* `SURFACE` is the target surface, of type `SDL:SDL-SURFACE`. Bound to `SDL:\*DEFAULT-SURFACE\*` if unspecified.
* `COLOR` is the line color, of type `SDL:COLOR` or `SDL:COLOR-A`. Bound to `SDL:\*DEFAULT-COLOR\*` if unspecified."
  (unless surface
    (setf surface sdl:*default-display*))
  (check-type surface sdl:sdl-surface)
  (check-type color sdl:sdl-color)
  (when (typep color 'sdl:color)
    (sdl-gfx-cffi::line-color (sdl:fp surface) x0 y0 x1 y1
			      (sdl:pack-color color)))
  (when (typep color 'sdl:color-a)
    (sdl-gfx-cffi::line-RGBA (sdl:fp surface) x0 y0 x1 y1
			     (sdl:r color) (sdl:g color) (sdl:b color) (sdl:a color))))

(defun draw-aa-line (p1 p2 &key (surface sdl:*default-surface*) (color sdl:*default-color*))
  "See [DRAW-AA-LINE-*](#draw-aa-line-*)-*

##### Parameters

* `P1` and `P2` are the start and end x/y co-ordinates of the line, of type `SDL:POINT`."
  (sdl:check-types sdl:point p1 p2)
  (draw-aa-line-* (sdl:x p1) (sdl:y p1) (sdl:x p2) (sdl:y p2) :surface surface :color color))

(defun draw-aa-line-* (x0 y0 x1 y1 &key (surface sdl:*default-surface*) (color sdl:*default-color*))
  "Draws an antialiased line of color `COLOR` to the surface `SURFACE`.

##### Parameters

* `X0` `Y0` are the start X/Y coordinates of the line, of type `INTEGER`.
* `X1` `Y1` are the end X/Y coordinates of the line, of type `INTEGER`.
* `SURFACE` is the target surface, of type `SDL:SDL-SURFACE`. Bound to `SDL:\*DEFAULT-SURFACE\*` if unspecified.
* `COLOR` is the line color, of type `SDL:COLOR` or `SDL:COLOR-A`. Bound to `SDL:\*DEFAULT-COLOR\*` if unspecified."
  (unless surface
    (setf surface sdl:*default-display*))
  (check-type surface sdl:sdl-surface)
  (check-type color sdl:sdl-color)
  (when (typep color 'sdl:color)
    (sdl-gfx-cffi::aa-line-color (sdl:fp surface) x0 y0 x1 y1
				 (sdl:pack-color color)))
  (when (typep color 'sdl:color-a)
    (sdl-gfx-cffi::aa-line-RGBA (sdl:fp surface) x0 y0 x1 y1
				(sdl:r color) (sdl:g color) (sdl:b color) (sdl:a color))))

(defun draw-circle (p1 r &key (surface sdl:*default-surface*) (color sdl:*default-color*))
  "See [DRAW-CIRCLE-*](#draw-circle-*).

##### Parameters

* `P1` is the X/Y coordinate of the center of the circle, of type `SDL:POINT`."
  (check-type p1 sdl:point)
  (draw-circle-* (sdl:x p1) (sdl:y p1) r :surface surface :color color))

(defun draw-circle-* (x y r &key (surface sdl:*default-surface*) (color sdl:*default-color*))
  "Draws a circle circumference of color `COLOR` to the surface `SURFACE`.
Use [DRAW-FILLED-CIRCLE-*](#draw-filled-circle-*) to draw a filled circle.

##### Parameters

* `X` and `Y` specify the center coordinate of the circle, of type `INTEGER`.
* `R` is the circle radius, of type `INTEGER`.
* `SURFACE` is the target surface, of type `SDL:SDL-SURFACE`. Bound to `SDL:\*DEFAULT-SURFACE\*` if unspecified.
* `COLOR` is the circumference color, of type `SDL:COLOR` or `SDL:COLOR-A`. Bound to `SDL:\*DEFAULT-COLOR\*` if unspecified."
  (unless surface
    (setf surface sdl:*default-display*))
  (check-type surface sdl:sdl-surface)
  (check-type color sdl:sdl-color)
  (when (typep color 'sdl:color)
    (sdl-gfx-cffi::circle-color (sdl:fp surface) x y r
				(sdl:pack-color color)))
  (when (typep color 'sdl:color-a)
    (sdl-gfx-cffi::circle-RGBA (sdl:fp surface) x y r
			       (sdl:r color) (sdl:g color) (sdl:b color) (sdl:a color))))

(defun draw-aa-circle (p1 r &key (surface sdl:*default-surface*) (color sdl:*default-color*))
  "See [DRAW-AA-CIRCLE-*](#draw-aa-circle-*).

##### Parameters

* `P1` is the X/Y coordinate of the center of the antialiased circle, of type `SDL:POINT`."
  (check-type p1 sdl:point)
  (draw-aa-circle-* (sdl:x p1) (sdl:y p1) r :surface surface :color color))

(defun draw-aa-circle-* (x y r &key (surface sdl:*default-surface*) (color sdl:*default-color*))
  "Draws the circumference of a circle of color COLOR to the surface SURFACE using anti-aliasing.
Use [DRAW-FILLED-CIRCLE-*](#draw-filled-circle-*) to draw a filled circle.

##### Parameters

* `X` and `Y` specify the center coordinate of the circle, of type `INTEGER`.
* `R` is the circle radius, of type `INTEGER`.
* `SURFACE` is the target surface, of type `SDL:SDL-SURFACE`. Bound to `SDL:\*DEFAULT-SURFACE\*` if unspecified.
* `COLOR` is the circumference color, of type `SDL:COLOR` or `SDL:COLOR-A`. Bound to `SDL:\*DEFAULT-COLOR\*` if unspecified."
  (unless surface
    (setf surface sdl:*default-display*))
  (check-type surface sdl:sdl-surface)
  (check-type color sdl:sdl-color)
  (when (typep color 'sdl:color)
    (sdl-gfx-cffi::aa-circle-color (sdl:fp surface) x y r
				   (sdl:pack-color color)))
  (when (typep color 'sdl:color-a)
    (sdl-gfx-cffi::aa-circle-RGBA (sdl:fp surface) x y r
				  (sdl:r color) (sdl:g color) (sdl:b color) (sdl:a color))))

(defun draw-filled-circle (p1 r &key (surface sdl:*default-surface*) (color sdl:*default-color*))
  "See [DRAW-FILLED-CIRCLE-*](#draw-filled-circle-*).

##### Parameters

* `P1` is the X/Y coordinate of the center of the filled circle, of type `SDL:POINT`."
  (check-type p1 sdl:point)
  (draw-filled-circle-* (sdl:x p1) (sdl:y p1) r :surface surface :color color))

(defun draw-filled-circle-* (x y r &key (surface sdl:*default-surface*) (color sdl:*default-color*))
  "Draws a filled circle of color `COLOR` to the surface `SURFACE`.

##### Parameters

* `X` and `Y` specify the center coordinate of the circle, of type `INTEGER`.
* `R` is the circle radius, of type `INTEGER`.
* `SURFACE` is the target surface, of type `SDL:SDL-SURFACE`. Bound to `SDL:\*DEFAULT-SURFACE\*` if unspecified.
* `COLOR` is the fill color, of type `SDL:COLOR` or `SDL:COLOR-A`. Bound to `SDL:\*DEFAULT-COLOR\*` if unspecified."
  (unless surface
    (setf surface sdl:*default-display*))
  (check-type surface sdl:sdl-surface)
  (check-type color sdl:sdl-color)
  (when (typep color 'sdl:color)
    (sdl-gfx-cffi::filled-circle-color (sdl:fp surface) x y r
				       (sdl:pack-color color)))
  (when (typep color 'sdl:color-a)
    (sdl-gfx-cffi::filled-circle-RGBA (sdl:fp surface) x y r
				      (sdl:r color) (sdl:g color) (sdl:b color) (sdl:a color))))

(defun draw-ellipse (p1 rx ry &key (surface sdl:*default-surface*) (color sdl:*default-color*))
  "See [DRAW-ELLIPSE-*](#draw-ellipse-*).

##### Parameters

* `P1` is the X/Y coordinate of the center of the ellipse, of type `SDL:POINT`."
  (check-type p1 sdl:point)
  (draw-ellipse-* (sdl:x p1) (sdl:y p1) rx ry :surface surface :color color))

(defun draw-ellipse-* (x y rx ry &key (surface sdl:*default-surface*) (color sdl:*default-color*))
  "Draws an ellipse circumference of color `COLOR` to the surface `SURFACE`.
Use [DRAW-FILLED-ELLIPSE-*](#draw-filled-ellipse-*) to draw a filled ellipse.

##### Parameters

* `X` and `Y` specify the center coordinate of the ellipse, of type `INTEGER`.
* `RX` and `RY` specify the ellipse radius, of type `INTEGER`.
* `SURFACE` is the target surface, of type `SDL:SDL-SURFACE`. Bound to `SDL:\*DEFAULT-SURFACE\*` if unspecified.
* `COLOR` is the circumference color, of type `SDL:COLOR` or `SDL:COLOR-A`. Bound to `SDL:\*DEFAULT-COLOR\*` if unspecified."
  (unless surface
    (setf surface sdl:*default-display*))
  (check-type surface sdl:sdl-surface)
  (check-type color sdl:sdl-color)
  (when (typep color 'sdl:color)
    (sdl-gfx-cffi::ellipse-color (sdl:fp surface) x y rx ry
				 (sdl:pack-color color)))
  (when (typep color 'sdl:color-a)
    (sdl-gfx-cffi::ellipse-RGBA (sdl:fp surface) x y rx ry
				(sdl:r color) (sdl:g color) (sdl:b color) (sdl:a color))))

(defun draw-aa-ellipse (p1 rx ry &key (surface sdl:*default-surface*) (color sdl:*default-color*))
  "See [DRAW-AA-ELLIPSE-*](#draw-aa-ellipse-*).

##### Parameters

* `P1` is the X/Y coordinate of the center of the ellipse, of type `SDL:POINT`."
  (check-type p1 sdl:point)
  (draw-aa-ellipse-* (sdl:x p1) (sdl:y p1) rx ry :surface surface :color color))

(defun draw-aa-ellipse-* (x y rx ry &key (surface sdl:*default-surface*) (color sdl:*default-color*))
  "Draws the circumference of an ellipse of color COLOR to the surface SURFACE using anti-aliasing.
Use [DRAW-FILLED-ELLIPSE-*](#draw-filled-ellipse-*) to draw a filled ellipse.

##### Parameters

* `X` and `Y` specify the center coordinate of the ellipse, of type `INTEGER`.
* `RX` and `RY` specify the ellipse radius, of type `INTEGER`.
* `SURFACE` is the target surface, of type `SDL:SDL-SURFACE`. Bound to `SDL:\*DEFAULT-SURFACE\*` if unspecified.
* `COLOR` is the circumference color, of type `SDL:COLOR` or `SDL:COLOR-A`. Bound to `SDL:\*DEFAULT-COLOR\*` if unspecified."
  (unless surface
    (setf surface sdl:*default-display*))
  (check-type surface sdl:sdl-surface)
  (check-type color sdl:sdl-color)
  (when (typep color 'sdl:color)
    (sdl-gfx-cffi::aa-ellipse-color (sdl:fp surface) x y rx ry
				    (sdl:pack-color color)))
  (when (typep color 'sdl:color-a)
    (sdl-gfx-cffi::aa-ellipse-RGBA (sdl:fp surface) x y rx ry
				   (sdl:r color) (sdl:g color) (sdl:b color) (sdl:a color))))

(defun draw-filled-ellipse (p1 rx ry &key (surface sdl:*default-surface*) (color sdl:*default-color*))
  "See [DRAW-FILLED-ELLIPSE-*](#draw-filled-ellipse-*).

##### Parameters

* `P1` is the X/Y coordinate of the center of the filled ellipse, of type `SDL:POINT`."
  (check-type p1 sdl:point)
  (draw-filled-ellipse-* (sdl:x p1) (sdl:y p1) rx ry :surface surface :color color))

(defun draw-filled-ellipse-* (x y rx ry &key (surface sdl:*default-surface*) (color sdl:*default-color*))
  "Draws a filled ellipse of color `COLOR` to the surface `SURFACE`.

##### Parameters

* `X` and `Y` specify the center coordinate of the ellipse, of type `INTEGER`.
* `RX` and `RY` specify the ellipse radius, of type `INTEGER`.
* `SURFACE` is the target surface, of type `SDL:SDL-SURFACE`. Bound to `SDL:\*DEFAULT-SURFACE\*` if unspecified.
* `COLOR` is the fill color, of type `SDL:COLOR` or `SDL:COLOR-A`. Bound to `SDL:\*DEFAULT-COLOR\*` if unspecified."
  (unless surface
    (setf surface sdl:*default-display*))
  (check-type surface sdl:sdl-surface)
  (check-type color sdl:sdl-color)
  (when (typep color 'sdl:color)
    (sdl-gfx-cffi::filled-ellipse-color (sdl:fp surface) x y rx ry
					(sdl:pack-color color)))
  (when (typep color 'sdl:color-a)
    (sdl-gfx-cffi::filled-ellipse-RGBA (sdl:fp surface) x y rx ry
				       (sdl:r color) (sdl:g color) (sdl:b color) (sdl:a color))))

(defun draw-pie (p1 rad start end &key (surface sdl:*default-surface*) (color sdl:*default-color*))
  "See [DRAW-PIE-*](#draw-pie-*).

##### Parameters

* `P1` is the X/Y coordinate of the center of the pie, of type `SDL:POINT`."
  (check-type p1 sdl:point)
  (draw-pie-* (sdl:x p1) (sdl:y p1) rad start end :surface surface :color color))

(defun draw-pie-* (x y rad start end &key (surface sdl:*default-surface*) (color sdl:*default-color*))
  "Draws a pie of color `COLOR` to the surface `SURFACE`.
Use [DRAW-FILLED-PIE-*](#draw-filled-pie-*) to draw a filled pie.

##### Parameters

* `X` and `Y` specify the center coordinate of the pie, of type `INTEGER`.
* `RAD` is the pie radius, of type `INTEGER`.
* `START` is the pie start, of type `INTEGER`.
* `END` is the pie end, of type `INTEGER`.
* `SURFACE` is the target surface, of type `SDL:SDL-SURFACE`. Bound to `SDL:\*DEFAULT-SURFACE\*` if unspecified.
* `COLOR` is the circumference color, of type `SDL:COLOR` or `SDL:COLOR-A`. Bound to `SDL:\*DEFAULT-COLOR\*` if unspecified."
  (unless surface
    (setf surface sdl:*default-display*))
  (check-type surface sdl:sdl-surface)
  (check-type color sdl:sdl-color)
  (when (typep color 'sdl:color)
    (sdl-gfx-cffi::pie-color (sdl:fp surface) x y rad start end
			     (sdl:pack-color color)))
  (when (typep color 'sdl:color-a)
    (sdl-gfx-cffi::pie-RGBA (sdl:fp surface) x y rad start end
			    (sdl:r color) (sdl:g color) (sdl:b color) (sdl:a color))))

(defun draw-filled-pie (p1 rad start end &key (surface sdl:*default-surface*) (color sdl:*default-color*))
  "See [DRAW-FILLED-PIE-*](#draw-filled-pie-*).

##### Parameters

* `P1` is the X/Y coordinate of the center of the filled pie, of type `SDL:POINT`."
  (check-type p1 sdl:point)
  (draw-filled-pie-* (sdl:x p1) (sdl:y p1) rad start end :surface surface :color color))

(defun draw-filled-pie-* (x y rad start end &key (surface sdl:*default-surface*) (color sdl:*default-color*))
    "Draws a filled pie of color `COLOR` to the surface `SURFACE`.

##### Parameters

* `X` and `Y` specify the center coordinate of the pie, of type `INTEGER`.
* `RAD` is the pie radius, of type `INTEGER`.
* `START` is the pie start, of type `INTEGER`.
* `END` is the pie end, of type `INTEGER`.
* `SURFACE` is the target surface, of type `SDL:SDL-SURFACE`. Bound to `SDL:\*DEFAULT-SURFACE\*` if unspecified.
* `COLOR` is the fill color, of type `SDL:COLOR` or `SDL:COLOR-A`. Bound to `SDL:\*DEFAULT-COLOR\*` if unspecified."
  (unless surface
    (setf surface sdl:*default-display*))
  (check-type surface sdl:sdl-surface)
  (check-type color sdl:sdl-color)
  (when (typep color 'sdl:color)
    (sdl-gfx-cffi::filled-pie-color (sdl:fp surface) x y rad start end
				    (sdl:pack-color color)))
  (when (typep color 'sdl:color-a)
    (sdl-gfx-cffi::filled-pie-RGBA (sdl:fp surface) x y rad start end
				   (sdl:r color) (sdl:g color) (sdl:b color) (sdl:a color))))

(defun draw-trigon (p1 p2 p3 &key (surface sdl:*default-surface*) (color sdl:*default-color*))
  "Draw the outline of a trigon or triangle, of color `COLOR` to surface `SURFACE`.
Use [DRAW-FILLED-TRIGON-*](#draw-filled-trigon-*) to draw a filled trigon.

##### Parameters

* `P1`, `P2` and `P3` specify the vertices of the trigon, of type `SDL:POINT`.
* `SURFACE` is the target surface, of type `SDL:SDL-SURFACE`. Bound to `SDL:\*DEFAULT-SURFACE\*` if unspecified.
* `COLOR` is the circumference color, of type `SDL:COLOR` or `SDL:COLOR-A`. Bound to `SDL:\*DEFAULT-COLOR\*` if unspecified."
  (sdl:check-types sdl:point p1 p2 p3)
  (unless surface
    (setf surface sdl:*default-display*))
  (check-type surface sdl:sdl-surface)
  (check-type color sdl:sdl-color)
  (when (typep color 'sdl:color)
    (sdl-gfx-cffi::trigon-color (sdl:fp surface) (sdl:x p1) (sdl:y p1)
				(sdl:x p2) (sdl:y p2)
				(sdl:x p3) (sdl:y p3)
				(sdl:pack-color color)))
  (when (typep color 'sdl:color-a)
    (sdl-gfx-cffi::trigon-RGBA (sdl:fp surface) (sdl:x p1) (sdl:y p1)
			       (sdl:x p2) (sdl:y p2)
			       (sdl:x p3) (sdl:y p3)
			       (sdl:r color) (sdl:g color) (sdl:b color) (sdl:a color))))

(defun draw-aa-trigon (p1 p2 p3 &key (surface sdl:*default-surface*) (color sdl:*default-color*))
  "Draw the outline of a trigon or triangle, of color `COLOR` to surface `SURFACE` using anti-aliasing.
Use [DRAW-FILLED-TRIGON-*](#draw-filled-trigon-*) to draw a filled trigon.

##### Parameters

* `P1`, `P2` and `P3` specify the vertices of the trigon, of type `SDL:POINT`.
* `SURFACE` is the target surface, of type `SDL:SDL-SURFACE`. Bound to `SDL:\*DEFAULT-SURFACE\*` if unspecified.
* `COLOR` is the circumference color, of type `SDL:COLOR` or `SDL:COLOR-A`. Bound to `SDL:\*DEFAULT-COLOR\*` if unspecified."
  (sdl:check-types sdl:point p1 p2 p3)
  (unless surface
    (setf surface sdl:*default-display*))
  (check-type surface sdl:sdl-surface)
  (check-type color sdl:sdl-color)
  (when (typep color 'sdl:color)
    (sdl-gfx-cffi::aa-trigon-color (sdl:fp surface) (sdl:x p1) (sdl:y p1)
				   (sdl:x p2) (sdl:y p2)
				   (sdl:x p3) (sdl:y p3)
				   (sdl:pack-color color)))
  (when (typep color 'sdl:color-a)
    (sdl-gfx-cffi::aa-trigon-RGBA (sdl:fp surface) (sdl:x p1) (sdl:y p1)
				  (sdl:x p2) (sdl:y p2)
				  (sdl:x p3) (sdl:y p3)
				  (sdl:r color) (sdl:g color) (sdl:b color) (sdl:a color))))

(defun draw-filled-trigon (p1 p2 p3 &key (surface sdl:*default-surface*) (color sdl:*default-color*))
  "Draw a filled trigon, of color `COLOR` to surface `SURFACE`.

##### Parameters

* `P1`, `P2` and `P3` specify the vertices of the trigon, of type `SDL:POINT`.
* `SURFACE` is the target surface, of type `SDL:SDL-SURFACE`. Bound to `SDL:\*DEFAULT-SURFACE\*` if unspecified.
* `COLOR` is the fill color, of type `SDL:COLOR` or `SDL:COLOR-A`. Bound to `SDL:\*DEFAULT-COLOR\*` if unspecified."
  (sdl:check-types sdl:point p1 p2 p3)
  (unless surface
    (setf surface sdl:*default-display*))
  (check-type surface sdl:sdl-surface)
  (check-type color sdl:sdl-color)
  (when (typep color 'sdl:color)
    (sdl-gfx-cffi::filled-trigon-color (sdl:fp surface) (sdl:x p1) (sdl:y p1)
				       (sdl:x p2) (sdl:y p2)
				       (sdl:x p3) (sdl:y p3)
				       (sdl:pack-color color)))
  (when (typep color 'sdl:color-a)
    (sdl-gfx-cffi::filled-trigon-RGBA (sdl:fp surface) (sdl:x p1) (sdl:y p1)
				      (sdl:x p2) (sdl:y p2)
				      (sdl:x p3) (sdl:y p3)
				      (sdl:r color) (sdl:g color) (sdl:b color) (sdl:a color))))

(defun draw-polygon (vertices &key (surface sdl:*default-surface*) (color sdl:*default-color*))
  "Draw the circumference of a polygon of color `COLOR` to surface SURFACE using the vertices in `VERTICES`.
Use [DRAW-FILLED-POLYGON-*](#draw-filled-polygon-*) to draw a filled polygon.

##### Parameters

* `VERTICES` is the list of vertices for the polygon. `VERTICES` is a list of `SDL:POINT`s.
* `SURFACE` is the target surface, of type `SDL:SDL-SURFACE`. Bound to `SDL:\*DEFAULT-SURFACE\*` if unspecified.
* `COLOR` is the circumference color, of type `SDL:COLOR` or `SDL:COLOR-A`. Bound to `SDL:\*DEFAULT-COLOR\*` if unspecified."
  (check-type vertices (and list (not null)) "Vertices must be a LIST of SDL:POINTs")
  (unless surface
    (setf surface sdl:*default-display*))
  (check-type surface sdl:sdl-surface)
  (check-type color sdl:sdl-color)
  (let ((x-array (cffi:foreign-alloc :short :initial-contents (return-list-for-array vertices :x)))
	(y-array (cffi:foreign-alloc :short :initial-contents (return-list-for-array vertices :y)))
	(poly-surface nil))
    (when (typep color 'sdl:color)
      (setf poly-surface (sdl-gfx-cffi::polygon-color (sdl:fp surface) x-array y-array (length vertices)
						      (sdl:pack-color color))))
    (when (typep color 'sdl:color-a)
      (setf poly-surface (sdl-gfx-cffi::polygon-RGBA (sdl:fp surface) x-array y-array (length vertices)
						     (sdl:r color) (sdl:g color) (sdl:b color) (sdl:a color))))
    (cffi:foreign-free x-array)
    (cffi:foreign-free y-array)
    poly-surface))

(defun draw-aa-polygon (vertices &key (surface sdl:*default-surface*) (color sdl:*default-color*))
  "Draw the circumference of a polygon of color `COLOR` to surface SURFACE using the vertices in `VERTICES`.
The polygon is anti-aliased. Use [DRAW-FILLED-POLYGON-*](#draw-filled-polygon-*) to draw a filled polygon.

##### Parameters

* `VERTICES` is the list of vertices of type `SDL:POINT`.
* `SURFACE` is the target surface, of type `SDL:SDL-SURFACE`. Bound to `SDL:\*DEFAULT-SURFACE\*` if unspecified.
* `COLOR` is the circumference color, of type `SDL:COLOR` or `SDL:COLOR-A`. Bound to `SDL:\*DEFAULT-COLOR\*` if unspecified."
  (check-type vertices (and list (not null)) "Vertices must be a LIST of SDL:POINTs")
  (unless surface
    (setf surface sdl:*default-display*))
  (check-type surface sdl:sdl-surface)
  (check-type color sdl:sdl-color)
  (let ((x-array (cffi:foreign-alloc :short :initial-contents (return-list-for-array vertices :x)))
	(y-array (cffi:foreign-alloc :short :initial-contents (return-list-for-array vertices :y)))
	(poly-surface nil))
    (when (typep color 'sdl:color)
      (setf poly-surface (sdl-gfx-cffi::aa-polygon-color (sdl:fp surface) x-array y-array (length vertices)
							 (sdl:pack-color color))))
    (when (typep color 'sdl:color-a)
      (setf poly-surface (sdl-gfx-cffi::aa-polygon-RGBA (sdl:fp surface) x-array y-array (length vertices)
							(sdl:r color) (sdl:g color) (sdl:b color)
							(sdl:a color))))
    (cffi:foreign-free x-array)
    (cffi:foreign-free y-array)
    poly-surface))

(defun draw-filled-polygon (vertices &key (surface sdl:*default-surface*) (color sdl:*default-color*))
  "Draw a filled polygon, of color `COLOR` to surface `SURFACE`.

##### Parameters

* `VERTICES` is the list of vertices of type `SDL:POINT`.
* `SURFACE` is the target surface, of type `SDL:SDL-SURFACE`. Bound to `SDL:\*DEFAULT-SURFACE\*` if unspecified.
* `COLOR` is the fill color, of type `SDL:COLOR` or `SDL:COLOR-A`. Bound to `SDL:\*DEFAULT-COLOR\*` if unspecified."
  (check-type vertices (and list (not null)) "Vertices must be a LIST of SDL:POINTs")
  (unless surface
    (setf surface sdl:*default-display*))
  (check-type surface sdl:sdl-surface)
  (check-type color sdl:sdl-color)
  (let ((x-array (cffi:foreign-alloc :short :initial-contents (return-list-for-array vertices :x)))
	(y-array (cffi:foreign-alloc :short :initial-contents (return-list-for-array vertices :y)))
	(poly-surface nil))
    (when (typep color 'sdl:color)
      (setf poly-surface (sdl-gfx-cffi::filled-polygon-color (sdl:fp surface) x-array y-array (length vertices)
							     (sdl:pack-color color))))
    (when (typep color 'sdl:color-a)
      (setf poly-surface (sdl-gfx-cffi::filled-polygon-RGBA (sdl:fp surface) x-array y-array (length vertices)
							      (sdl:r color) (sdl:g color) (sdl:b color)
							      (sdl:a color))))
    (cffi:foreign-free x-array)
    (cffi:foreign-free y-array)
    poly-surface))

(defun draw-bezier (vertices steps &key (surface sdl:*default-surface*) (color sdl:*default-color*))
  "Draw a bezier curve of color `COLOR` to the surface `SURFACE`. The shape of the Bezier curve is defined by several control points. 
A control point is a vertex containing an X and Y coordinate pair.

##### Parameters

* `VERTICES` is the list of control points of type `SDL:POINT`.
* `STEPS` is the number of segments used to draw the Bezier curve. The greater the number of segments, the smoother the Bezier curve.
* `SURFACE` is the target surface, of type `SDL:SDL-SURFACE`. Bound to `SDL:\*DEFAULT-SURFACE\*` if unspecified.
* `COLOR` is the line color, of type `SDL:COLOR` or `SDL:COLOR-A`. Bound to `SDL:\*DEFAULT-COLOR\*` if unspecified.

##### Example

    \(DRAW-BEZIER \(LIST \(SDL:POINT :X 60  :Y 40\)
                         \(SDL:POINT :X 160 :Y 10\)
                         \(SDL:POINT :X 170 :Y 150\)
                         \(SDL:POINT :X 60 :Y 150\)\)
                   10\)"
  (check-type vertices (and list (not null)) "Vertices must be a LIST of SDL:POINTs")
  (unless surface
    (setf surface sdl:*default-display*))
  (check-type surface sdl:sdl-surface)
  (check-type color sdl:sdl-color)
  (let ((x-array (cffi:foreign-alloc :short :initial-contents (return-list-for-array vertices :x)))
	(y-array (cffi:foreign-alloc :short :initial-contents (return-list-for-array vertices :y)))
	(bezier-surface nil))
    (when (typep color 'sdl:color)
      (setf bezier-surface (sdl-gfx-cffi::bezier-color (sdl:fp surface) x-array y-array (length vertices) steps (sdl:pack-color color))))
    (when (typep color 'sdl:color-a)
      (setf bezier-surface (sdl-gfx-cffi::bezier-RGBA (sdl:fp surface) x-array y-array (length vertices) steps
						      (sdl:r color) (sdl:g color) (sdl:b color) (sdl:a color))))
    (cffi:foreign-free x-array)
    (cffi:foreign-free y-array)
    bezier-surface))

;;; r

(defun roto-zoom-surfaze (angle zoom smooth &key (surface sdl:*default-surface*))
  (check-type surface sdl:surface)
  (sdl-gfx-cffi::rotozoomSurface (sdl:fp surface) angle zoom smooth))

(defun roto-zoom-xy (angle zoomx zoomy smooth &key (surface sdl:*default-surface*))
  (check-type surface sdl:surface)
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
  (check-type surface sdl:surface)
  (sdl-gfx-cffi::zoomSurface (sdl:fp surface) zoomx zoomy smooth))

(defun zoom-surface-size (width height zoomx zoomy)
  (cffi:with-foreign-objects ((dstwidth :int) (dstheight :int))
    (sdl-gfx-cffi::zoomSurfaceSize width height zoomx zoomy dstwidth dstheight)
    (vector (cffi:mem-ref dstwidth :int) (cffi:mem-ref dstheight :int))))

