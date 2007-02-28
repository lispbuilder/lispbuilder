;; SDL (Simple Media Layer) library using CFFI for foreign function interfacing...
;; (C)2006 Justin Heyes-Jones <justinhj@gmail.com> and Luke Crook <luke@balooga.com>
;; Thanks to Frank Buss and Surendra Singh
;; see COPYING for license
;; This file contains some useful functions for using SDL from Common lisp
;; using sdl.lisp (the CFFI wrapper)

(in-package #:lispbuilder-sdl)

;; Coefficients for Matrix M
;; For catmull-rom-spline
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

;; (defun bounds-collision? (bounds1 bounds2)
;;   (let ((collision? nil))
;;     (destructuring-bind (s1-x1 s1-y1 s1-x2 s1-y2)
;; 	(coerce bounds1 'list)
;;       (destructuring-bind (s2-x1 s2-y1 s2-x2 s2-y2)
;; 	  (coerce bounds2 'list)
;; 	(if (and (> s1-x2 s2-x1)
;; 		 (> s1-y2 s2-y1)
;; 		 (< s1-y1 s2-y2)
;; 		 (< s1-x1 s2-x2))
;; 	    (setf collision? t))))
;;     collision?))



(defun random-rectangle (bound-w bound-h &optional (rectangle (rectangle)))
  "Returns a rectangle of random x, y width and height coordinates within the specified
bounds of width BOUND-W and height BOUND-H.

If the &optional parameter RECTANGLE is unset then a new RECTANGLE object is created and returned.
If the &optional parameter RECTANGLE is set then the coordinates if this rectangle are modified and 
this RECTANGLE object is returned."
  (check-type rectangle rectangle)
  (let* ((x (random bound-w))
	 (y (random bound-h))
	 (w (random+1 (- bound-w x)))
	 (h (random+1 (- bound-h y))))
    (set-rectangle-* rectangle :x x :y y :w w :h h))
  rectangle)

;; (defun rectangle-from-wh (width height &key (position *default-position*))
;;   (rectangle :x (x position)
;; 	     :y (y position)
;; 	     :w (+ (x position) width)
;; 	     :h (+ (y position) height)))

(defun rectangle-from-edges (p1 p2 &optional (rectangle (rectangle)))
  "See RECTANGLE-FROM-EDGES-*.
P1 and P2 are POINTS that specify the bounds of the RECTANGLE. 
P1 specifies the top left coordinate.
P2 specifies the lower right coordinate."
  (rectangle-from-edges-* (x p1) (y p1) (x p2) (y p2) rectangle))

(defun rectangle-from-edges-* (x1 y1 x2 y2 &optional (rectangle (rectangle)))
  "Returns a rectangle using the bounds specified by the INTEGERS X1, X2, Y1 Y2.
X1, Y1 specifies the top left coordinate. 
X2, Y2 specifies the top left coordinate. 

The coordinates of the rectangle are X = X1, Y = Y1, WIDTH = \(- X2 X1\), HEIGHT = \(- Y2 Y1\) 

If the &optional parameter RECTANGLE is unset then a new RECTANGLE object is created and returned. 
If the &optional parameter RECTANGLE is set then the coordinates if this rectangle are modified and 
this RECTANGLE object is returned.

RESULT is an object of type RECTANGLE."
  (check-type rectangle rectangle)
  (set-rectangle-* rectangle
		   :x x1
		   :y y1
		   :w (1+ (abs (- x2 x1)))
		   :h (1+ (abs (- y2 y1)))))

(defun rectangle-from-midpoint-* (x y w h &optional (rectangle (rectangle)))
  "Returns a rectangle of width W and height H with the rectangle mid-point at X and Y. 

If the &optional parameter RECTANGLE is unset then a new RECTANGLE object is created and returned. 
If the &optional parameter RECTANGLE is set then the coordinates if this rectangle are modified and 
this RECTANGLE object is returned.

RESULT is an object of type RECTANGLE."
  (check-type rectangle rectangle)
  (set-rectangle-* rectangle
		   :x (- x (/ w 2))
		   :y (- y (/ h 2))
		   :w w
		   :h h))

(defun genbez (x0 y0 x1 y1 x2 y2 x3 y3 &key (segments 20))
  (let ((gx0 x0) (gy0 y0)
	(gx1 x1) (gy1 y1)
	(gx3 x3) (gy3 y3)
	(point-list nil)
	(du (/ 1.0 segments)))
    (let ((cx (* (- gx1 gx0) 3))
	  (cy (* (- gy1 gy0) 3))
	  (px (* (- x2 gx1) 3))
	  (py (* (- y2 gy1) 3)))
      (let ((bx (- px cx))
	    (by (- py cy))
	    (ax (- gx3 px gx0))
	    (ay (- gy3 py gy0)))
	(push (point :x gx0 :y gy0) point-list)
	(loop for n from 0 below (1- segments)
	   do (let* ((u (* n du))
		     (u^2 (* u u))
		     (u^3 (expt u 3)))
		(push (point :x (+ (* ax u^3)
				(* bx u^2)
				(* cx u)
				gx0)
			     :y (+ (* ay u^3)
				(* by u^2)
				(* cy u)
				gy0))
		      point-list)))
	(push (point :x gx3
		     :y gy3)
	      point-list)))))

(defmacro with-bezier ((shape-type &optional (segments 20)) &body body)
  "Draw a bezier curve to the *DEFAULT-SURFACE* using *DEFAULT-COLOR*.
A vertex specifies a control point for the Bezier curve. A vertex may be added
using ADD-VERTEX which accepts an POINT, or the x/y spread version 
ADD-VERTEX-*.

ADD-VERTEX and ADD-VERTEX-* are valid only within the scop of WITH-BEZIER.

  * WITH-CURVE accepts a SHAPE-TYPE where SHAPE-TYPE may be one of 
:LINE-STRIP, :LINES, or :POINTS.
    
    * When SHAPE-TYPE is :LINE-STRIP, a single continuous line is drawn through the
specified waypoints.

    * When SHAPE-TYPE is :LINES, a line is drawn to alternate waypoint pairs.

    * When SHAPE-TYPE is :POINTS, a single point is drawn at each waypoint.

The number of segments used to draw the Bezier curve defaults to 10.
However this number may be increased by specifying SEGMENTS. 
The greater the number of segments, the smoother the Bezier curve.

Example:
\(WITH-SURFACE \(DSP *DEFAULT-DISPLAY*\)
  \(WITH-COLOR \(COL \(COLOR\)\)
    \(WITH-BEZIER \(:lise-strip 30\)
      \(ADD-VERTEX-* 60  40\)
      \(ADD-VERTEX-* 160 10\)
      \(ADD-VERTEX-* 170 150\)
      \(ADD-VERTEX-* 60  150\)\)\)\)"
  (let ((point-list (gensym "point-list-")))
    `(let ((,point-list nil))
       (labels ((add-vertex (point)
		  (setf ,point-list (append ,point-list (list point))))
		(add-vertex-* (x y)
		  (add-vertex (point :x x :y y))))
	 ,@body)
       (draw-bezier ,point-list ,shape-type :segments ,segments))))

(defmacro with-curve ((shape-type &optional (segments 10)) &body body)
  "Draw a Cattmul-Rom spline to the *DEFAULT-SURFACE* using *DEFAULT-COLOR*.
A vertex specifies a waypoint for the spline. A vertex may be added
using ADD-VERTEX which accepts an POINT, or the x/y spread version 
ADD-VERTEX-*.

ADD-VERTEX and ADD-VERTEX-* are valid only within the scop of WITH-CURVE.

  * WITH-CURVE accepts a SHAPE-TYPE where SHAPE-TYPE may be one of 
:LINE-STRIP, :LINES, or :POINTS.
    
    * When SHAPE-TYPE is :LINE-STRIP, a single continuous line is drawn through the
specified waypoints.

    * When SHAPE-TYPE is :LINES, a line is drawn to alternate waypoint pairs.

    * When SHAPE-TYPE is :POINTS, a single point is drawn at each waypoint.

The number of segments used to draw the Catmull-Rom spline defaults to 10.
However this number may be increased by specifying SEGMENTS. 
The greater the number of segments, the smoother the spline.

Example:
\(WITH-SURFACE \(DSP *DEFAULT-DISPLAY*\)
  \(WITH-COLOR \(COL \(COLOR\)\)
    \(WITH-CURVE \(:LINE-STRIP 30\)
      \(ADD-VERTEX-* 60  40\)
      \(ADD-VERTEX-* 160 10\)
      \(ADD-VERTEX-* 170 150\)
      \(ADD-VERTEX-* 60  150\)\)\)\)"
  (let ((point-list (gensym "point-list-")))
    `(let ((,point-list nil))
       (labels ((add-vertex (point)
		  (setf ,point-list (append ,point-list (list point))))
		(add-vertex-* (x y)
		  (add-vertex (point :x x :y y))))
	 ,@body)
       (draw-curve ,point-list ,shape-type :segments ,segments))))

(defmacro with-shape ((shape-type) &body body)
  "Draw a polygon to the *DEFAULT-SURFACE* using *DEFAULT-COLOR*.
Addional vertices are added using ADD-VERTEX which accepts an POINT, 
or the x/y spread version ADD-VERTEX-*.

ADD-VERTEX and ADD-VERTEX-* are valid only within the scop of WITH-SHAPE.

  * WITH-SHAPE accepts a SHAPE-TYPE where SHAPE-TYPE may be one of 
:LINE-STRIP, :LINES, or :POINTS.
    
    * When SHAPE-TYPE is :LINE-STRIP, a single continuous line is drawn through the
specified vertices.

    * When SHAPE-TYPE is :LINES, a line is drawn to alternate vertex pairs.

    * When SHAPE-TYPE is :POINTS, a single point is drawn at each vertex.

Example:
\(WITH-SURFACE \(DSP *DEFAULT-DISPLAY*\)
  \(WITH-COLOR \(COL \(COLOR\)\)
    \(WITH-SHAPE \(:POINTS\)
      \(ADD-VERTEX-* 60  40\)
      \(ADD-VERTEX-* 160 10\)
      \(ADD-VERTEX-* 170 150\)
      \(ADD-VERTEX-* 60  150\)\)\)\)"
  (let ((point-list (gensym "point-list-")))
    `(let ((,point-list nil))
       (labels ((add-vertex (point)
		  (setf ,point-list (append ,point-list (list point))))
		(add-vertex-* (x y)
		  (add-vertex (point :x x :y y))))
	 ,@body)
       (draw-shape ,point-list ,shape-type))))

(defun calculate-curve (p1 p2 p3 p4 segments)
  (let ((step-size 0)
	(points nil))
    (when (or (null segments) (= segments 0))
      (setf segments (distance p2 p3)))
    (setf step-size (coerce (/ 1 segments) 'float))
    (setf points (loop for i from 0.0 below 1.0 by step-size
		    collecting (point :x (catmull-rom-spline i (x p1) (x p2)
							     (x p3) (x p4))
				      :y (catmull-rom-spline i (y p1) (y p2)
							     (y p3) (y p4)))))
    ;; NOTE: There must be a more efficient way to add the first and last points to the point list.
    (push p2 points)
    (nconc points (list p3))))

(defun catmull-rom-spline (val v0 v1 v2 v3)
  (let ((c1 0) (c2 0) (c3 0) (c4 0))
    (setf c1                 (* *M12* v1)
	  c2 (+ (* *M21* v0)              (* *M23* v2))
	  c3 (+ (* *M31* v0) (* *M32* v1) (* *M33* v2) (* *M34* v3))
	  c4 (+ (* *M41* v0) (* *M42* v1) (* *M43* v2) (* *M44* v3)))
    (+ c1 (* val (+ c2 (* val (+ c3 (* c4 val))))))))

(defun draw-bezier (points type
		    &key (clipping-p t) (surface *default-surface*) (color *default-color*) (segments 20))
  (do* ((p1 points (cdr p1))
	(p2 (cdr p1) (cdr p1))
	(p3 (cdr p2) (cdr p2))
	(p4 (cdr p3) (cdr p3)))
       ((or (null p4) (null p3) (null p2) (null p1)))
    (draw-shape (genbez (x (first p1)) (y (first p1))
			(x (first p2)) (y (first p2))
			(x (first p3)) (y (first p3))
			(x (first p4)) (y (first p4))
			:segments segments)
		type :clipping-p clipping-p :surface surface :color color)))
  
(defun draw-curve (points type &key (clipping-p t) (surface *default-surface*) (color *default-color*)
		   (segments 10))
  "Draw a Cattmul-Rom spline to the surface SURFACE using color COLOR.

  * POINTS is a list of vertices or waypoints for the spline. A vertex is of type POINT

  * TYPE is the shape type where TYPE may be one of 
:LINE-STRIP, :LINES, or :POINTS.
    
    * When TYPE is :LINE-STRIP, a single continuous line is drawn through the
specified waypoints.

    * When TYPE is :LINES, a line is drawn to alternate waypoint pairs.

    * When TYPE is :POINTS, a single point is drawn at each waypoint.

  * SEGMENTS is the number of segments used to draw the Catmull-Rom spline.
The greater the number of segments, the smoother the spline.

Example:
\(DRAW-CURVE \(LIST \(POINT :X 60  :Y 40\)
		  \(POINT :X 160 :Y 10\)
		  \(POINT :X 170 :Y 150\)
		  \(POINT :X 60  :Y 150\)\)
	    :LINE-STRIP
	    :segments 10\)"
  (do* ((p1 points (cdr p1))
	(p2 (cdr p1) (cdr p1))
	(p3 (cdr p2) (cdr p2))
	(p4 (cdr p3) (cdr p3)))
       ((or (null p4) (null p3) (null p2) (null p1)))
    (draw-shape (calculate-curve (first p1) (first p2) (first p3) (first p4) segments) type
		:clipping-p clipping-p :surface surface :color color)))

(defun draw-shape (points type &key (clipping-p t) (surface *default-surface*) (color *default-color*))
  "Draw a polygon using the vertices in POINTS to the surface SURFACE using color COLOR.

  * POINTS is a list of vertices. A vertex is of type POINT

  * TYPE is the shape type where TYPE may be one of 
:LINE-STRIP, :LINES, or :POINTS.
    
    * When TYPE is :LINE-STRIP, a single continuous line is drawn through the vertices in POINTS

    * When TYPE is :LINES, a line is drawn to alternate vertex pairs.

    * When TYPE is :POINTS, a single point is drawn at each vertex.

Example:
\(DRAW-SHAPE \(LIST \(POINT :X 60  :Y 40\)
		    \(POINT :X 160 :Y 10\)
		    \(POINT :X 170 :Y 150\)
   		    \(POINT :X 60  :Y 150\)\)
	    :LINE-STRIP\)"
  (unless surface
    (setf surface *default-display*))
  (check-type surface sdl-surface)
  (check-type color sdl-color)
  (case type
    (:line-strip
     (do* ((p1 points (cdr p1))
	   (p2 (cdr p1) (cdr p1)))
	  ((or (null p2)
	       (null p1)))
       (draw-line (first p1) (first p2)
		  :clipping-p clipping-p
		  :surface surface :color color)))
    (:lines
     (do* ((p1 points (if (cdr p1)
			  (cddr p1)
			  nil))
	   (p2 (cdr p1) (cdr p1)))
	  ((or (null p2)
	       (null p1)))
       (draw-line (first p1) (first p2)
		  :clipping-p clipping-p
		  :surface surface :color color)))
    (:points
     (loop for point in points
	do (draw-point point
		       :clipping-p clipping-p
		       :surface surface
		       :color color)))))

(defun draw-line-* (x0 y0 x1 y1 &key (surface *default-surface*) (color *default-color*) (clipping-p t))
  "Draws a line of color COLOR to the surface SURFACE.

  * X1 Y1 are the start X/Y coordinates of the line, of type INTEGER.

  * X2 Y2 are the end X/Y coordinates of the line, of type INTEGER.

  * SURFACE is the target surface, of type SDL-SURFACE. Binds to *DEFAULT-SURFACE* by default.

  * COLOR is the fill color, of type COLOR or COLOR-A. Binds to *DEFAULT-COLOR* by default."
  (unless surface
    (setf surface *default-display*))
  (check-type surface sdl-surface)
  (check-type color sdl-color)
  (let ((x0 (sdl-base::to-int x0))
	(y0 (sdl-base::to-int y0))
	(x1 (sdl-base::to-int x1))
	(y1 (sdl-base::to-int y1)))
    (declare (type fixnum x0 y0 x1 y1))
    (cond
      ((eq x0 x1)
       ;; Optimization. If (eq x0 x1) then draw using vline.
       (draw-vline x0 y0 y1 :surface surface :color color :clipping-p nil))
      ((eq y0 y1)
       ;; Optimization. If (eq y0 y1) then draw using hline.
       (draw-hline x0 x1 y0 :surface surface :color color :clipping-p nil))
      (t
       (when clipping-p
	 ;; simple clipping, should be improved with Cohen-Sutherland line clipping
	 (sdl-base::check-bounds 0 (- (width surface) 1) x0 x1)
	 (sdl-base::check-bounds 0 (- (height surface) 1) y0 y1))
       
       ;; draw line with Bresenham algorithm
       (let ((x 0) (y 0) (e 0) (dx 0) (dy 0)
	     (color (map-color color surface)))
	 (declare (type fixnum x y dx dy)
		  (type (unsigned-byte 32) color))
	 (when (> x0 x1)
	   (rotatef x0 x1)
	   (rotatef y0 y1))
	 (setf e 0)
	 (setf x x0)
	 (setf y y0)
	 (setf dx (- x1 x0))
	 (setf dy (- y1 y0))

	 (sdl-base::with-pixel (pix (fp surface))
	   (if (>= dy 0)
	       (if (>= dx dy)
		   (loop for x from x0 to x1 do
			(sdl-base::write-pixel pix x y color)
			(if (< (* 2 (+ e dy)) dx)
			    (incf e dy)
			    (progn
			      (incf y)
			      (incf e (- dy dx)))))
		   (loop for y from y0 to y1 do
			(sdl-base::write-pixel pix x y color)
			(if (< (* 2 (+ e dx)) dy)
			    (incf e dx)
			    (progn
			      (incf x)
			      (incf e (- dx dy))))))
	       (if (>= dx (- dy))
		   (loop for x from x0 to x1 do
			(sdl-base::write-pixel pix x y color)
			(if (> (* 2 (+ e dy)) (- dx))
			    (incf e dy)
			    (progn
			      (decf y)
			      (incf e (+ dy dx)))))
		   (progn
		     (rotatef x0 x1)
		     (rotatef y0 y1)
		     (setf x x0)
		     (setf dx (- x1 x0))
		     (setf dy (- y1 y0))
		     (loop for y from y0 to y1 do
			  (sdl-base::write-pixel pix x y color)
			  (if (> (* 2 (+ e dx)) (- dy))
			      (incf e dx)
			      (progn
				(decf x)
				(incf e (+ dx dy))))))))))))))

(defun draw-line (p1 p2 &key (surface *default-surface*) (color *default-color*) (clipping-p t))
  "See DRAW-LINE-*

  * POINT1 and POINT2 are the start and end x/y co-ordinates of the line, of type POINT."
    (check-types point p1 p2)
    (draw-line-* (x p1) (y p1)
		 (x p2) (y p2)
		 :clipping-p clipping-p :color color :surface surface))


(defun draw-vline (x y0 y1 &key (surface *default-surface*) (color *default-color*) (clipping-p nil))
  "Draws a vertical line of color COLOR from Y1 to Y2 through X onto the surface SURFACE. 

  * X is the horizontal INTEGER coordinate that the vertical line must intersect.  

  * Y1 and Y2 are the vertical start and end points of the line, of type INTEGER.

  * SURFACE is the target surface, of type SDL-SURFACE. Binds to *DEFAULT-SURFACE* by default.

  * COLOR is the line color, of type COLOR or COLOR-A. Binds to *DEFAULT-COLOR* by default."
  (unless surface
    (setf surface *default-display*))
  (check-type surface sdl-surface)
  (check-type color sdl-color)
  (when (> y0 y1)
    (rotatef y0 y1))
  (with-rectangle (template (rectangle-from-edges-* x y0 x y1))
    (fill-surface color
		  :surface surface
		  :template template
		  :clipping-p clipping-p)))
  
(defun draw-hline (x0 x1 y &key (surface *default-surface*) (color *default-color*) (clipping-p nil))
  "Draws a horizontal line of color COLOR from X1 to X2 through Y onto the surface SURFACE. 

  * X1 and X2 are the horizontal start and end points of the line, of type INTEGER.

  * X is the vertical INTEGER coordinate that the horizontal line must intersect.  

  * SURFACE is the target surface, of type SDL-SURFACE. Binds to *DEFAULT-SURFACE* by default.

  * COLOR is the line color, of type COLOR or COLOR-A. Binds to *DEFAULT-COLOR* by default."
  (unless surface
    (setf surface *default-display*))
  (check-type surface sdl-surface)
  (check-type color sdl-color)
  (when (> x0 x1)
    (rotatef x0 x1))
  (with-rectangle (template (rectangle-from-edges-* x0 y x1 y))
    (fill-surface color
		  :surface surface
		  :template template
		  :clipping-p clipping-p)))
  
(defun draw-box (rectangle &key (clipping-p nil) (surface *default-surface*) (color *default-color*) (stroke-color nil) (alpha nil))
  "See DRAW-BOX-*.

  * RECT is filled rectangle to draw, of type RECTANGLE."
  (unless surface
    (setf surface *default-display*))
  (check-type surface sdl-surface)
  (check-type color sdl-color)
  (when stroke-color
    (check-type stroke-color sdl-color))
  (check-type rectangle rectangle)
    (let* ((width  (width rectangle))
	   (height (height rectangle))
	   (x (x rectangle))
	   (y (y rectangle))
	   (surf (if alpha (create-surface width height :alpha-value alpha) surface)))
      (fill-surface color :surface surf :template (if alpha nil rectangle) :clipping-p clipping-p)
      (when stroke-color
	(draw-rectangle-* (if alpha 0 x) (if alpha 0 y) width height
			  :surface surf :clipping-p clipping-p :color stroke-color :alpha nil))
      (when alpha
	(draw-surface-at-* surf x y :surface surface)
	(free-surface surf)))
    rectangle)

(defun draw-box-* (x y w h &key (clipping-p nil) (surface *default-surface*) (color *default-color*) (stroke-color nil) (alpha nil))
  "Draws a filled rectangle of color COLOR to surface SURFACE.

  * X and Y are the INTEGER coordinates of the top-left corner of the rectangle.

  * W and H are the width and height of the rectangle, of type INTEGER.

  * SURFACE is the target surface, of type SDL-SURFACE. Binds to *DEFAULT-SURFACE* by default.

  * COLOR is the line color, of type COLOR or COLOR-A. Binds to *DEFAULT-COLOR* by default.

  * STROKE-COLOR when not NIL will draw a 1 pixel line of color COLOR around the perimiter of the box.

  * ALPHA when between 0 and 255 is used as the alpha transparency value when blitting the box onto SURFACE.
Note that drawing a box with alpha transparency requires that a new surface with alpha transparency be created, 
the box is then draw to this surface and this surface is then blitted to SURFACE."
  (with-rectangle (template (rectangle :x x :y y :w w :h h))
    (draw-box template :clipping-p clipping-p :surface surface :color color
	      :stroke-color stroke-color :alpha alpha)))

(defun draw-box-edges-* (x1 y1 x2 y2 &key (clipping-p nil) (surface *default-surface*) (color *default-color*) (stroke-color nil) (alpha nil))
  "Draws a filled rectangle of color COLOR to the surface SURFACE.

  * X1 and Y1 are the INTEGER coordinates of the top-left corner of the rectangle.

  * X2 and Y2 are the INTEGER coordinates of the bottom-right corner of the rectangle.

  * SURFACE is the target surface, of type SDL-SURFACE. Binds to *DEFAULT-SURFACE* by default.

  * COLOR is the fill color, of type COLOR or COLOR-A. Binds to *DEFAULT-COLOR* by default.

  * STROKE-COLOR when not NIL will draw a 1 pixel line of color COLOR around the perimiter of the box.

  * ALPHA when between 0 and 255 is used as the alpha transparency value when blitting the box onto SURFACE.
Note that drawing a box with alpha transparency requires that a new surface with alpha transparency be created, 
the box is then draw to this surface and this surface is then blitted to SURFACE."
  (with-rectangle (template (rectangle-from-edges-* x1 y1 x2 y2))
    (draw-box template :clipping-p clipping-p :surface surface :color color
	      :stroke-color stroke-color :alpha alpha)))

(defun draw-rectangle (rectangle &key (clipping-p nil) (surface *default-surface*) (color *default-color*) (alpha nil))
  "See DRAW-RECTANGLE-*.

  * RECT is the rectangle to draw, of type RECTANGLE."
  (check-type rectangle rectangle)
  (draw-rectangle-* (x rectangle) (y rectangle)
		    (width rectangle) (height rectangle)
		    :clipping-p clipping-p :surface surface :color color :alpha alpha)
  surface)

(defun draw-rectangle-* (x y w h &key (clipping-p nil) (surface *default-surface*) (color *default-color*) (alpha nil))
  "Draws a rectangle of color COLOR to the surface SURFACE.

  * X and Y are the INTEGER coordinates of the top-left corner of the rectangle.

  * W and H are the width and height of the rectangle, of type INTEGER.

  * SURFACE is the target surface, of type SDL-SURFACE. Binds to *DEFAULT-SURFACE* by default.

  * COLOR is the line color, of type COLOR or COLOR-A. Binds to *DEFAULT-COLOR* by default.

  * ALPHA when between 0 and 255 is used as the alpha transparency value when blitting the rectangle onto SURFACE.
Note that drawing a rectangle with alpha transparency requires that a new surface with alpha transparency be created, 
the rectangle is then draw to this surface and this surface is then blitted to SURFACE."
  (unless surface
    (setf surface *default-display*))
  (check-type surface sdl-surface)
  (check-type color sdl-color)
  (let ((x+width  (1- (+ x w)))
	(y+height (1- (+ y h))))
    (let ((surf (if alpha (create-surface w h :alpha-value alpha) surface))
	  (x (if alpha 0 x))
	  (y (if alpha 0 y)))
      (draw-hline x x+width y :surface surf :color color :clipping-p clipping-p)
      (draw-hline x x+width y+height :surface surf :color color :clipping-p clipping-p)
      (draw-vline x y y+height :surface surf :color color :clipping-p clipping-p)
      (draw-vline x+width y y+height :surface surf :color color :clipping-p clipping-p)
      (when alpha
	(draw-surface-at-* surf x y :surface surface)
	(free-surface surf))))
  surface)

;; (defun draw-rectangle-points (p1 p2 &key (clipping-p t) (surface *default-surface*) (color *default-color*))
;;   "Given a surface pointer draw a rectangle with the specified x,y, width, height and color"
;;   (draw-rectangle-xy (x p1) (y p1) (x p2) (y p2)
;; 		     :clipping-p clipping-p :surface surface :color color))


(defun draw-rectangle-edges-* (x1 y1 x2 y2
			       &key (clipping-p nil) (surface *default-surface*) (color *default-color*) (alpha nil))
  "Draws a rectangle of color COLOR to the surface SURFACE.

  * X1 and Y1 are the INTEGER coordinates of the top-left corner of the rectangle.

  * X2 and Y2 are the INTEGER coordinates of the bottom-right corner of the rectangle.

  * SURFACE is the target surface, of type SDL-SURFACE. Binds to *DEFAULT-SURFACE* by default.

  * COLOR is the line color, of type COLOR or COLOR-A. Binds to *DEFAULT-COLOR* by default.

  * ALPHA when between 0 and 255 is used as the alpha transparency value when blitting the rectangle onto SURFACE.
Note that drawing a rectangle with alpha transparency requires that a new surface with alpha transparency be created, 
the rectangle is then draw to this surface and this surface is then blitted to SURFACE."
  (check-type surface sdl-surface)
  (check-type color sdl-color)
  (with-rectangle (template (rectangle-from-edges-* x1 y1 x2 y2))
    (draw-rectangle template :surface surface :clipping-p clipping-p :color color :alpha alpha))
  surface)

(defun draw-point (point &key (clipping-p t) (surface *default-surface*) (color *default-color*))
  "See DRAW-POINT-*.

  * POSITION is the X/Y coordinate of the pixel, of type POINT."
  (check-type point point)
  (draw-point-* (x point) (y point) :clipping-p clipping-p :surface surface :color color))

(defun draw-point-* (x y &key (clipping-p t) (surface *default-surface*) (color *default-color*))
  "Draw the color COLOR to the surface SURFACE at the specified X and Y coordiates. 

  * X Y specify the coordinates of the pixel, and are of type INTEGER.

  * SURFACE is the target surface, of type SDL-SURFACE. Binds to *DEFAULT-SURFACE* by default.

  * COLOR is the new color of the pixel, of type COLOR or COLOR-A. Binds to *DEFAULT-COLOR* by default."
  (unless surface
    (setf surface *default-display*))
  (check-type surface sdl-surface)
  (check-type color sdl-color)
  (when clipping-p
    (sdl-base::check-bounds 0 (width surface) x)
    (sdl-base::check-bounds 0 (height surface) y))
  (sdl-base::with-pixel (pix (fp surface))
    (sdl-base::write-pixel pix x y (map-color color surface)))
  surface)


(defun read-point (point &key (clipping-p t) (surface *default-surface*))
  (let ((x (x point)) (y (y point)))
    (when clipping-p
      (sdl-base::check-bounds 0 (width surface) x)
      (sdl-base::check-bounds 0 (height surface) y))
    (sdl-base::with-pixel (surf (fp surface))
      (sdl-base::read-pixel surf x y))))

(defun draw-filled-circle (p1 radius &key (surface *default-surface*) (color *default-color*) (stroke-color nil) (alpha nil))
  "See DRAW-FILLED-CIRCLE-*

  * P1 is the X/Y coordinate of the center of the filled circle, of type POINT."
  (check-type p1 point)
  (draw-filled-circle-* (x p1) (y p1) radius
			:surface surface :color color :stroke-color stroke-color :alpha alpha))

(defun draw-filled-circle-* (x0 y0 radius &key (surface *default-surface*) (color *default-color*) (stroke-color nil) (alpha nil))
  "Draws an filled circle of color COLOR to the surface SURFACE.

  * X and Y specify the center coordinate of the circle, of type INTEGER.

  * R is the circle radius, of type INTEGER.

  * SURFACE is the target surface, of type SDL-SURFACE. Binds to *DEFAULT-SURFACE* by default.

  * COLOR is the fill color, of type COLOR or COLOR-A. Binds to *DEFAULT-COLOR* by default.

  * STROKE-COLOR when not NIL will draw a 1 pixel line of color COLOR around the circumference of the circle.

  * ALPHA when between 0 and 255 is used as the alpha transparency value when blitting the circle onto SURFACE.
Note that drawing a circle with alpha transparency requires that a new surface with alpha transparency be created, 
the circle is then draw to this surface and this surface is then blitted to SURFACE."
  (unless surface
    (setf surface *default-display*))
  (check-type surface sdl-surface)
  (check-type color sdl-color)
  (if stroke-color
      (check-type stroke-color sdl-color))

  (let ((surf (if alpha (create-surface (1+ (* radius 2)) (1+ (* radius 2)) :alpha-value alpha) surface)))
    (let ((x0 (if alpha radius x0))
	  (y0 (if alpha radius y0)))
      
      (let ((f (- 1 radius))
	    (ddf-x 0)
	    (ddf-y (* -2 radius)))
	(draw-vline x0 (+ y0 radius) (- y0 radius) :color color :surface surf :clipping-p nil)
	(draw-hline (+ x0 radius) (- x0 radius) y0 :color color :surface surf :clipping-p nil)
	(do ((x 0)
	     (y radius))
	    ((<= y x))
	  (when (>= f 0)
	    (decf y)
	    (incf ddf-y 2)
	    (incf f ddf-y))
	  (incf x)
	  (incf ddf-x 2)
	  (incf f (1+ ddf-x))
	  (draw-hline (+ x0 x) (- x0 x) (+ y0 y) :color color :surface surf :clipping-p nil)
	  (draw-hline (+ x0 x) (- x0 x) (- y0 y) :color color :surface surf :clipping-p nil)
	  (draw-hline (+ x0 y) (- x0 y) (+ y0 x) :color color :surface surf :clipping-p nil)
	  (draw-hline (+ x0 y) (- x0 y) (- y0 x) :color color :surface surf :clipping-p nil))

	;; Draw the circle outline when a color is specified.
	(when stroke-color
	  (draw-circle-* x0 y0 radius :surface surf :color stroke-color))))

    (when alpha
      (draw-surface-at-* surf (- x0 radius) (- y0 radius) :surface surface)
      (free-surface surf)))
  surface)

(defun draw-circle (p1 radius &key
		    (surface *default-surface*)
		    (color *default-color*)
		    (alpha nil))
  "See DRAW-CIRCLE-*

  * P1 is the X/Y coordinate of the center of the circle, of type POINT."
  (check-type p1 point)
  (draw-circle-* (x p1) (y p1) radius
		 :surface surface :color color :alpha alpha))

(defun draw-circle-* (x0 y0 radius &key
		      (surface *default-surface*)
		      (color *default-color*)
		      (alpha nil))
  "Draws a circle of color COLOR to the surface SURFACE.
Note that this is not a 'filled circle'. Only the circle circumference is drawn.

  * X and Y specify the center coordinate of the circle, of type INTEGER.

  * R is the circle radius, of type INTEGER.

  * SURFACE is the target surface, of type SDL-SURFACE. Binds to *DEFAULT-SURFACE* by default.

  * COLOR is the circumference color, of type COLOR or COLOR-A. Binds to *DEFAULT-COLOR* by default.

  * ALPHA when between 0 and 255 is used as the alpha transparency value when blitting the circle onto SURFACE.
Note that drawing a circle with alpha transparency requires that a new surface with alpha transparency be created, 
the circle is then draw to this surface and this surface is then blitted to SURFACE."
  (unless surface
    (setf surface *default-display*))
  (check-type surface sdl-surface)
  (check-type color sdl-color)
  (let ((f (- 1 radius))
	(ddf-x 0)
	(ddf-y (* -2 radius)))
    (labels ((in-bounds (x y w h)
	       (if (and (>= x 0) (< x w)
			(>= y 0) (< y h))
		   t
		   nil)))
      
      (let* ((width (if alpha (1+ (* radius 2)) (width surface)))
	     (height (if alpha (1+ (* radius 2)) (height surface)))
	     (surf (if alpha (create-surface width height :alpha-value alpha) surface))
	     (col (map-color color surf)))
	(let ((x0 (if alpha radius x0))
	      (y0 (if alpha radius y0)))
	  
	  (let ((x-pos 0) (y-pos 0))
	    (sdl-base::with-pixel (pix (fp surf))
	      (setf x-pos x0
		    y-pos (+ y0 radius))
	      (when (in-bounds x-pos y-pos width height)
		(sdl-base::write-pixel pix x-pos y-pos col))
	      (setf x-pos x0
		    y-pos (- y0 radius))
	      (when (in-bounds x-pos y-pos width height)
		(sdl-base::write-pixel pix x-pos y-pos col))
	      (setf x-pos (+ x0 radius)
		    y-pos y0)
	      (when (in-bounds x-pos y-pos width height)
		(sdl-base::write-pixel pix x-pos y-pos col))
	      (setf x-pos (- x0 radius)
		    y-pos y0)
	      (when (in-bounds x-pos y-pos width height)
		(sdl-base::write-pixel pix x-pos y-pos col))
	      (do ((x 0)
		   (y radius))
		  ((<= y x))
		(when (>= f 0)
		  (decf y)
		  (incf ddf-y 2)
		  (incf f ddf-y))

		(incf x)
		(incf ddf-x 2)
		(incf f (1+ ddf-x))
	    
		(setf x-pos (+ x0 x)
		      y-pos (+ y0 y))
		(when (in-bounds x-pos y-pos width height)
		  (sdl-base::write-pixel pix x-pos y-pos col)) ;     setPixel(x0 + x, y0 + y);
		(setf x-pos (- x0 x)
		      y-pos (+ y0 y))
		(when (in-bounds x-pos y-pos width height)
		  (sdl-base::write-pixel pix x-pos y-pos col)) ;     setPixel(x0 - x, y0 + y);
		(setf x-pos (+ x0 x)
		      y-pos (- y0 y))
		(when (in-bounds x-pos y-pos width height)
		  (sdl-base::write-pixel pix x-pos y-pos col)) ;     setPixel(x0 + x, y0 - y);
		(setf x-pos (- x0 x)
		      y-pos (- y0 y))
		(when (in-bounds x-pos y-pos width height)
		  (sdl-base::write-pixel pix x-pos y-pos col)) ;     setPixel(x0 - x, y0 - y);
		(setf x-pos (+ x0 y)
		      y-pos (+ y0 x))
		(when (in-bounds x-pos y-pos width height)
		  (sdl-base::write-pixel pix x-pos y-pos col)) ;     setPixel(x0 + y, y0 + x);
		(setf x-pos (- x0 y)
		      y-pos (+ y0 x))
		(when (in-bounds x-pos y-pos width height)
		  (sdl-base::write-pixel pix x-pos y-pos col)) ;     setPixel(x0 - y, y0 + x);
		(setf x-pos (+ x0 y)
		      y-pos (- y0 x))
		(when (in-bounds x-pos y-pos width height)
		  (sdl-base::write-pixel pix x-pos y-pos col)) ;     setPixel(x0 + y, y0 - x);
		(setf x-pos (- x0 y)
		      y-pos (- y0 x))
		(when (in-bounds x-pos y-pos width height)
		  (sdl-base::write-pixel pix x-pos y-pos col)) ;     setPixel(x0 - y, y0 - x);
		))))
      
	(when alpha
	  (draw-surface-at-* surf (- x0 radius) (- y0 radius) :surface surface)
	  (free-surface surf)))))
    surface)

(defun draw-trigon (p1 p2 p3 &key (surface *default-surface*) (color *default-color*) (clipping-p t))
    "Draw a trigon, of color COLOR to surface SURFACE.
Note: The trigon is not filled, only the edges are drawn.

  * P1, P2 and P3 specify the vertices of the trigon, of type POINT.

  * SURFACE is the target surface, of type SDL-SURFACE. Binds to *DEFAULT-SURFACE* by default.

  * COLOR is the circumference color, of type COLOR or COLOR-A. Binds to *DEFAULT-COLOR* by default."
  (check-types point p1 p2 p3)
   (unless surface
    (setf surface *default-display*))
 (check-type color sdl-color)
 (draw-line p1 p2 :surface surface :color color :clipping-p clipping-p)
 (draw-line p2 p3 :surface surface :color color :clipping-p clipping-p)
 (draw-line p3 p1 :surface surface :color color :clipping-p clipping-p))

(defun draw-polygon (vertices &key (surface *default-surface*) (color *default-color*) (clipping-p t))
  "Draw a polygon, of color COLOR to surface SURFACE.
Note: The polygon is not filled, only the edges are drawn.

  * VECTICES is the list of vertices for the polygon. POINTS is a list of POINTs.

  * SURFACE is the target surface, of type SDL-SURFACE. Binds to *DEFAULT-SURFACE* by default.

  * COLOR is the circumference color, of type COLOR or COLOR-A. Binds to *DEFAULT-COLOR* by default."
  (check-type vertices (and list (not null)) "POINTs must be a LIST of POINTs")
  (unless surface
    (setf surface *default-display*))
  (check-type color sdl-color)
  (draw-shape vertices :line-strip :clipping-p clipping-p :surface surface :color color))
