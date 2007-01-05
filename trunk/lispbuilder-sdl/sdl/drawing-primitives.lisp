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



(defun random-rectangle (bound-w bound-h &optional (rectangle (sdl::rectangle)))
  (let* ((x (random bound-w))
	 (y (random bound-h))
	 (w (random+1 (- bound-w x)))
	 (h (random+1 (- bound-h y))))
    (set-rectangle rectangle :x x :y y :w w :h h))
  rectangle)

;; (defun rectangle-from-wh (width height &key (position sdl:*default-position*))
;;   (rectangle :x (x position)
;; 	     :y (y position)
;; 	     :w (+ (x position) width)
;; 	     :h (+ (y position) height)))

(defun rectangle-from-xy (x1 y1 x2 y2)
  (rectangle :x x1
	     :y y1
	     :w (1+ (abs (- x2 x1)))
	     :h (1+ (abs (- y2 y1)))))

(defun rectangle-from-points (p1 p2)
  (rectangle-from-xy (x p1) (y p1) (x p2) (y p2)))

(Defun rectangle-from-midpoint (x y w h)
  (rectangle :x (- x (/ w 2))
	     :y (- y (/ h 2))
	     :w w
	     :h h))

(defun rectangle-from-surface (&key (surface *default-surface*))
  (rectangle :x (x surface) :y (y surface) :w (width surface) :h (height surface)))

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
  (let ((point-list (gensym "point-list-")))
    `(let ((,point-list nil))
       (labels ((add-vertex (point)
		  (setf ,point-list (append ,point-list (list point)))))
	 ,@body)
       (draw-bezier ,point-list ,shape-type :segments ,segments))))

(defmacro with-curve ((shape-type &optional (segments 10)) &body body)
  (let ((point-list (gensym "point-list-")))
    `(let ((,point-list nil))
       (labels ((add-vertex (point)
		  (setf ,point-list (append ,point-list (list point)))))
	 ,@body)
       (draw-curve ,point-list ,shape-type :segments ,segments))))

(defmacro with-shape ((shape-type) &body body)
  (let ((point-list (gensym "point-list-")))
    `(let ((,point-list nil))
       (labels ((add-vertex (point)
		  (setf ,point-list (append ,point-list (list point)))))
	 ,@body)
       (draw-shape ,point-list ,shape-type))))

(defun calculate-curve (p1 p2 p3 p4 segments)
  (let ((step-size 0)
	(points nil))
    (when (or (null segments) (= segments 0))
      (setf segments (distance (x p2) (y p2)
			       (x p3) (y p3))))
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
  (do* ((p1 points (cdr p1))
	(p2 (cdr p1) (cdr p1))
	(p3 (cdr p2) (cdr p2))
	(p4 (cdr p3) (cdr p3)))
       ((or (null p4) (null p3) (null p2) (null p1)))
    (draw-shape (calculate-curve (first p1) (first p2) (first p3) (first p4) segments) type
		:clipping-p clipping-p :surface surface :color color)))

(defun draw-shape (points type &key (clipping-p t) (surface *default-surface*) (color *default-color*))
  (case type
    (:line-strip
     (do* ((p1 points (cdr p1))
	   (p2 (cdr p1) (cdr p1)))
	  ((or (null p2)
	       (null p1)))
       (sdl:draw-line (first p1) (first p2) :clipping-p clipping-p
				  :surface surface :color color)))
    (:lines
     (do* ((p1 points (if (cdr p1)
			  (cddr p1)
			  nil))
	   (p2 (cdr p1) (cdr p1)))
	  ((or (null p2)
	       (null p1)))
       (sdl:draw-line (first p1) (first p2) :clipping-p clipping-p
				  :surface surface :color color)))
    (:points
     (loop for point in points
	do (draw-point point
		       :clipping-p clipping-p
		       :surface surface
		       :color color)))))

(defun draw-line-xy (x0 y0 x1 y1 &key (surface *default-surface*) (color *default-color*) (clipping-p t))
  (let ((x0 (sdl-base::to-int x0))
	(y0 (sdl-base::to-int y0))
	(x1 (sdl-base::to-int x1))
	(y1 (sdl-base::to-int y1)))
    (declare (type fixnum x0 y0 x1 y1))

    (when clipping-p
      ;; simple clipping, should be improved with Cohen-Sutherland line clipping
      (sdl-base::check-bounds 0 (- (width surface) 1) x0 x1)
      (sdl-base::check-bounds 0 (- (height surface) 1) y0 y1))

    ;; draw line with Bresenham algorithm
    (let ((x 0) (y 0) (e 0) (dx 0) (dy 0)
	  (color (map-color color surface)))
      (declare (type fixnum x y dx dy color))
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
			     (incf e (+ dx dy))))))))))))

(defun draw-line (p1 p2 &key (surface *default-surface*) (color *default-color*) (clipping-p t))
  (draw-line-xy (x p1) (y p1)
		(x p2) (y p2)
		:clipping-p clipping-p :color color :surface surface))

(defun draw-vline-points (p1 p2 &key (surface *default-surface*) (color *default-color*) (clipping-p t))
  (draw-box-xy (x p1) (y p1) (x p1) (y p2)
	       :clipping-p clipping-p :surface surface :color color))

(defun draw-hline-points (p1 p2 &key (surface *default-surface*) (color *default-color*) (clipping-p t))
  (draw-box-xy (x p1) (y p1) (x p2) (y p1)
	       :clipping-p clipping-p :surface surface :color color))

(defun draw-vline-xy (x y0 y1 &key (surface *default-surface*) (color *default-color*) (clipping-p t))
  (draw-box-xy x y0 x y1 :clipping-p clipping-p :surface surface :color color))

(defun draw-hline-xy (x0 x1 y &key (surface *default-surface*) (color *default-color*) (clipping-p t))
  (draw-box-xy x0 y x1 y :clipping-p clipping-p :surface surface :color color))

(defun draw-box (rectangle &key (clipping-p t)
		 (surface *default-surface*) (color *default-color*))
  "Given a surface pointer draw a rectangle with the specified x,y, width, height and color"
  (fill-surface color :surface surface :template rectangle :clipping-p clipping-p)
  rectangle)

(defun draw-box-points (p1 p2 &key (clipping-p t) (surface *default-surface*) (color *default-color*))
  "Given a surface pointer draw a rectangle with the specified corner co-ordinates and color"
  (fill-surface color
		:surface surface
		:template (rectangle-from-points p1 p2)
		:clipping-p clipping-p))

(defun draw-box-xy (x1 y1 x2 y2 &key (clipping-p t) (surface *default-surface*) (color *default-color*))
  "Given a surface pointer draw a rectangle with the specified corner co-ordinates and color"
  (fill-surface color
		:surface surface
		:template (rectangle-from-xy x1 y1 x2 y2)
		:clipping-p clipping-p))

(defun draw-rectangle (rectangle &key (clipping-p t)
		       (surface *default-surface*) (color *default-color*))
  "Given a surface pointer draw a rectangle with the specified x,y, width, height and color"
  (with-rectangle (rectangle nil nil)
    (let ((x+width (+ x w))
	  (y+height (+ y h)))
      (draw-line-xy x y x+width y :surface surface :color color :clipping-p clipping-p)
      (draw-line-xy x+width y x+width y+height :surface surface :color color :clipping-p clipping-p)
      (draw-line-xy x+width y+height x y+height :surface surface :color color :clipping-p clipping-p)
      (draw-line-xy x y+height x y :surface surface :color color :clipping-p clipping-p)))
  surface)

(defun draw-rectangle-points (p1 p2 &key (clipping-p t) (surface *default-surface*) (color *default-color*))
  "Given a surface pointer draw a rectangle with the specified x,y, width, height and color"
  (draw-rectangle-xy (x p1) (y p1) (x p2) (y p2)
		     :clipping-p clipping-p :surface surface :color color))


(defun draw-rectangle-xy (x1 y1 x2 y2
				  &key (clipping-p t) (surface *default-surface*) (color *default-color*))
  "Given a surface pointer draw a rectangle with the specified x,y, width, height and color"
  (draw-line-xy x1 y1 x2 y1 :surface surface :color color :clipping-p clipping-p)
  (draw-line-xy x2 y1 x2 y2 :surface surface :color color :clipping-p clipping-p)
  (draw-line-xy x2 y2 x1 y2 :surface surface :color color :clipping-p clipping-p)
  (draw-line-xy x1 y2 x1 y1 :surface surface :color color :clipping-p clipping-p)
  surface)

(defun draw-point (point &key (clipping-p t) (surface *default-surface*) (color *default-color*))
  (let ((x (x point)) (y (y point)))
    (when clipping-p
      (sdl-base::check-bounds 0 (width surface) x)
      (sdl-base::check-bounds 0 (height surface) y))
    (sdl-base::with-pixel (surf (fp surface))
      (sdl-base::write-pixel surf x y (map-color color surface)))))

(defun read-point (point &key (clipping-p t) (surface *default-surface*))
  (let ((x (x point)) (y (y point)))
    (when clipping-p
      (sdl-base::check-bounds 0 (width surface) x)
      (sdl-base::check-bounds 0 (height surface) y))
    (sdl-base::with-pixel (surf (fp surface))
      (sdl-base::read-pixel surf x y))))
