;; SDL (Simple Media Layer) library using CFFI for foreign function interfacing...
;; (C)2006 Justin Heyes-Jones <justinhj@gmail.com> and Luke Crook <luke@balooga.com>
;; Thanks to Frank Buss and Surendra Singh
;; see COPYING for license
;; This file contains some useful functions for using SDL from Common lisp
;; using sdl.lisp (the CFFI wrapper)

(in-package #:lispbuilder-sdl)

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

(defun bounds-from-wh (width height &key (point sdl:*default-position*))
  (rectangle (point-x point) (point-y point) (+ (point-x point) width) (+ (point-y point) height)))

(defun bounds-from-surface (&key (surface *default-surface*) (point *default-position*))
  (bounds-from-wh (surf-w surface) (surf-h surface) :point point))

(defun bounds-collision? (bounds1 bounds2)
  (let ((collision? nil))
    (destructuring-bind (s1-x1 s1-y1 s1-x2 s1-y2)
	(coerce bounds1 'list)
      (destructuring-bind (s2-x1 s2-y1 s2-x2 s2-y2)
	  (coerce bounds2 'list)
	(if (and (> s1-x2 s2-x1)
		 (> s1-y2 s2-y1)
		 (< s1-y1 s2-y2)
		 (< s1-x1 s2-x2))
	    (setf collision? t))))
    collision?))

(defun rect-from-wh (width height &key (point sdl:*default-position*))
  (rectangle (point-x point) (point-y point) width height))

(defun rect-from-pp (p1 p2)
  (rect-from-xy (pos-x p1) (pos-y p1) (pos-x p2) (pos-y p2)))

(defun rect-from-xy (x1 y1 x2 y2)
  (rectangle x1 y1 (1+ (abs (- x1 x2))) (1+ (abs (- y1 y2)))))

(Defun rect-from-midpoint (x y w h)
  (sdl:rectangle (- x (/ w 2))
		 (- y (/ h 2))
		 w h))


;; (defmacro genbez (x0 y0 x1 y1 x2 y2 x3 y3)
;;   (let ((gx0 (gensym "gx0-")) (gx1 (gensym "gx1-")) (gy0 (gensym "gy0-"))
;; 	(gy1 (gensym "gy1")) (gx3 (gensym "gx3")) (gy3 (gensym "gy3-"))
;; 	(point-list (gensym "point-list-")))
;;     `(let ((,gx0 ,x0) (,gy0 ,y0)
;; 	   (,gx1 ,x1) (,gy1 ,y1)
;; 	   (,gx3 ,x3) (,gy3 ,y3)
;; 	   (,point-list nil))
;;        (let ((cx (* (- ,gx1 ,gx0) 3))
;; 	     (cy (* (- ,gy1 ,gy0) 3))
;; 	     (px (* (- ,x2 ,gx1) 3))
;; 	     (py (* (- ,y2 ,gy1) 3)))
;; 	 (let ((bx (- px cx))
;; 	       (by (- py cy))
;; 	       (ax (- ,gx3 px ,gx0))
;; 	       (ay (- ,gy3 py ,gy0)))
;; 	   (push (point ,gx0 ,gy0) ,point-list)
;; 	   ,@(map1-n #'(lambda (n)
;; 			 (let* ((u (* n *du*))
;; 				(u^2 (* u u))
;; 				(u^3 (expt u 3)))
;; 			   `(push (point (+ (* ax ,u^3)
;; 					    (* bx ,u^2)
;; 					    (* cx ,u)
;; 					    ,gx0)
;; 					 (+ (* ay ,u^3)
;; 					    (* by ,u^2)
;; 					    (* cy ,u)
;; 					    ,gy0))
;; 				  ,point-list)))
;; 		     (1- *segs*))
;; 	   (push (point ,gx3
;; 			,gy3)
;; 		 ,point-list)))
;;        (reverse ,point-list))))

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
	(push (point gx0 gy0) point-list)
	(loop for n from 0 below (1- segments)
	   do (let* ((u (* n du))
		     (u^2 (* u u))
		     (u^3 (expt u 3)))
		(push (point (+ (* ax u^3)
				(* bx u^2)
				(* cx u)
				gx0)
			     (+ (* ay u^3)
				(* by u^2)
				(* cy u)
				gy0))
		      point-list)))
	(push (point gx3
		     gy3)
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

(defun draw-bezier (points type
		    &key update-p (clipping-p t) (surface *default-surface*) (color *default-color*) (segments 20))
  (do* ((p1 points (cdr p1))
	(p2 (cdr p1) (cdr p1))
	(p3 (cdr p2) (cdr p2))
	(p4 (cdr p3) (cdr p3)))
       ((or (null p4) (null p3) (null p2) (null p1)))
    (draw-shape (genbez (pos-x (first p1)) (pos-y (first p1))
			(pos-x (first p2)) (pos-y (first p2))
			(pos-x (first p3)) (pos-y (first p3))
			(pos-x (first p4)) (pos-y (first p4))
			:segments segments)
		type :update-p update-p :clipping-p clipping-p :surface surface :color color)))
  
(defun draw-curve (points type &key update-p (clipping-p t) (surface *default-surface*) (color *default-color*)
		   (segments 10))
  (do* ((p1 points (cdr p1))
	(p2 (cdr p1) (cdr p1))
	(p3 (cdr p2) (cdr p2))
	(p4 (cdr p3) (cdr p3)))
       ((or (null p4) (null p3) (null p2) (null p1)))
    (draw-shape (calculate-curve (first p1) (first p2) (first p3) (first p4) segments) type
		:update-p update-p :clipping-p clipping-p :surface surface :color color)))

(defun draw-shape (points type &key update-p (clipping-p t) (surface *default-surface*) (color *default-color*))
  (case type
    (:line-strip
     (do* ((p1 points (cdr p1))
	   (p2 (cdr p1) (cdr p1)))
	  ((or (null p2)
	       (null p1)))
       (sdl:draw-line (first p1) (first p2) :update-p update-p :clipping-p clipping-p
				  :surface surface :color color)))
    (:lines
     (do* ((p1 points (if (cdr p1)
			  (cddr p1)
			  nil))
	   (p2 (cdr p1) (cdr p1)))
	  ((or (null p2)
	       (null p1)))
       (sdl:draw-line (first p1) (first p2) :update-p update-p :clipping-p clipping-p
				  :surface surface :color color)))
    (:points
     (loop for point in points
	do (sdl:draw-point :position point :update-p update-p :clipping-p clipping-p
				  :surface surface :color color)))))

(defun draw-image (&key
		   (surface sdl:*default-surface*)
		   (position sdl:*default-position*)
		   (screen sdl:*default-display*))
  (let ((w (sdl:surf-w surface))
        (h (sdl:surf-h surface)))
    (sdl:blit-surface :src surface
		      :dst screen
		      :src-rect (sdl:rectangle 0 0 w h)
		      :dst-rect (sdl:point (sdl:point-x position) (sdl:point-y position)))))

(defun draw-line-xy (x0 y0 x1 y1 &key (surface *default-surface*) (color *default-color*) update-p (clipping-p t))
  (let ((x0 (to-int x0))
	(y0 (to-int y0))
	(x1 (to-int x1))
	(y1 (to-int y1)))
    (declare (type fixnum x0 y0 x1 y1))

    (when clipping-p
      ;; simple clipping, should be improved with Cohen-Sutherland line clipping
      (sdl:check-bounds 0 (- (sdl:surf-w surface) 1) x0 x1)
      (sdl:check-bounds 0 (- (sdl:surf-h surface) 1) y0 y1))

    ;; draw line with Bresenham algorithm
    (let ((x 0) (y 0) (e 0) (dx 0) (dy 0)
	  (color (map-color :color color :surface surface)))
      (declare (type fixnum x y dx dy color))
      (when (> x0 x1)
	(rotatef x0 x1)
	(rotatef y0 y1))
      (setf e 0)
      (setf x x0)
      (setf y y0)
      (setf dx (- x1 x0))
      (setf dy (- y1 y0))

      (with-possible-lock-and-update (:surface surface :check-lock-p t :update-p update-p)
	(if (>= dy 0)
	    (if (>= dx dy)
		(loop for x from x0 to x1 do
		     (sdl:draw-pixel x y :surface surface :color color)
		     (if (< (* 2 (+ e dy)) dx)
			 (incf e dy)
			 (progn
			   (incf y)
			   (incf e (- dy dx)))))
		(loop for y from y0 to y1 do
		     (sdl:draw-pixel x y :surface surface :color color)
		     (if (< (* 2 (+ e dx)) dy)
			 (incf e dx)
			 (progn
			   (incf x)
			   (incf e (- dx dy))))))
	    (if (>= dx (- dy))
		(loop for x from x0 to x1 do
		     (sdl:draw-pixel x y :surface surface :color color)
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
		       (sdl:draw-pixel x y :surface surface :color color)
		       (if (> (* 2 (+ e dx)) (- dy))
			   (incf e dx)
			   (progn
			     (decf x)
			     (incf e (+ dx dy))))))))))))

(defun draw-line (p1 p2 &key (surface *default-surface*) (color *default-color*) update-p (clipping-p t))
  (draw-line-xy (pos-x p1) (pos-y p1)
		(pos-x p2) (pos-y p2)
		:update-p update-p :clipping-p clipping-p :color color :surface surface))

(defun draw-vline (p1 p2 &key (surface *default-surface*) (color *default-color*) update-p (clipping-p t))
  (draw-box-xy (pos-x p1) (pos-y p1) (pos-x p1) (pos-y p2)
	       :update-p update-p :clipping-p clipping-p :surface surface :color color))

(defun draw-hline (p1 p2 &key (surface *default-surface*) (color *default-color*) update-p (clipping-p t))
  (draw-box-xy (pos-x p1) (pos-y p1) (pos-x p2) (pos-y p1)
	       :update-p update-p :clipping-p clipping-p :surface surface :color color))

(defun draw-vline-xy (x y0 y1 &key (surface *default-surface*) (color *default-color*) update-p (clipping-p t))
  (draw-box-xy x y0 x y1 :update-p update-p :clipping-p clipping-p :surface surface :color color))

(defun draw-hline-xy (x0 x1 y &key (surface *default-surface*) (color *default-color*) update-p (clipping-p t))
  (draw-box-xy x0 y x1 y :update-p update-p :clipping-p clipping-p :surface surface :color color))

(defun draw-box (&key update-p (clipping-p t)
		 (rectangle *default-rectangle*) (surface *default-surface*) (color *default-color*))
  "Given a surface pointer draw a rectangle with the specified x,y, width, height and color"
  (fill-surface :surface surface :color color :template rectangle :update-p update-p :clipping-p clipping-p)
  rectangle)

(defun draw-box-pp (p1 p2 &key update-p (clipping-p t) (surface *default-surface*) (color *default-color*))
  "Given a surface pointer draw a rectangle with the specified corner co-ordinates and color"
  (fill-surface :surface surface :color color
		:template (rect-from-pp p1 p2)
		:update-p update-p
		:clipping-p clipping-p))

(defun draw-box-xy (x1 y1 x2 y2 &key update-p (clipping-p t) (surface *default-surface*) (color *default-color*))
  "Given a surface pointer draw a rectangle with the specified corner co-ordinates and color"
  (fill-surface :surface surface :color color
		:template (rect-from-xy x1 y1 x2 y2)
		:update-p update-p
		:clipping-p clipping-p))

(defun draw-rectangle (&key update-p (clipping-p t)
		       (rectangle *default-rectangle*) (surface *default-surface*) (color *default-color*))
  "Given a surface pointer draw a rectangle with the specified x,y, width, height and color"
  (with-rectangle (rectangle)
    (let ((x+width (+ x w))
	  (y+height (+ y h)))
      (sdl:draw-line-xy x y x+width y :surface surface :color color :update-p update-p :clipping-p clipping-p)
      (sdl:draw-line-xy x+width y x+width y+height :surface surface :color color :update-p update-p :clipping-p clipping-p)
      (sdl:draw-line-xy x+width y+height x y+height :surface surface :color color :update-p update-p :clipping-p clipping-p)
      (sdl:draw-line-xy x y+height x y :surface surface :color color :update-p update-p :clipping-p clipping-p)))
  surface)

(defun draw-rectangle-pp (p1 p2 &key update-p (clipping-p t) (surface *default-surface*) (color *default-color*))
  "Given a surface pointer draw a rectangle with the specified x,y, width, height and color"
  (draw-rectangle-xy (pos-x p1) (pos-y p1) (pos-x p2) (pos-y p2)
		     :update-p update-p :clipping-p clipping-p :surface surface :color color))


(defun draw-rectangle-xy (x1 y1 x2 y2
				  &key update-p (clipping-p t) (surface *default-surface*) (color *default-color*))
  "Given a surface pointer draw a rectangle with the specified x,y, width, height and color"
  (sdl:draw-line-xy x1 y1 x2 y1 :surface surface :color color :update-p update-p :clipping-p clipping-p)
  (sdl:draw-line-xy x2 y1 x2 y2 :surface surface :color color :update-p update-p :clipping-p clipping-p)
  (sdl:draw-line-xy x2 y2 x1 y2 :surface surface :color color :update-p update-p :clipping-p clipping-p)
  (sdl:draw-line-xy x1 y2 x1 y1 :surface surface :color color :update-p update-p :clipping-p clipping-p)
  surface)

(defun draw-point (&key (position sdl:*default-position*) (check-lock-p t) (update-p nil) (clipping-p t)
		   (surface *default-surface*) (color *default-color*))
  (let ((x (pos-x position)) (y (pos-y position)))
    (when clipping-p
      (check-bounds 0 (surf-w surface) x)
      (check-bounds 0 (surf-h surface) y))
    (with-possible-lock-and-update (:surface surface :check-lock-p check-lock-p :update-p update-p
					     :template (vector x y 1 1))
      (sdl:draw-pixel x y :surface surface :color (map-color :color color :surface surface)))))

(defun draw-pixel (x y &key (surface *default-surface*) color)
  "Set the pixel at (x, y) to the given value 
   NOTE: The surface must be locked before calling this.
   Also NOTE: Have not tested 1,2,3 bpp surfaces, only 4 bpp"
  (let* ((format (foreign-slot-value surface 'SDL_Surface 'format))
	 (bpp (foreign-slot-value format 'SDL_PixelFormat 'BytesPerPixel))
	 (offset (+ (* y (foreign-slot-value surface 'SDL_Surface 'Pitch))
		    (* x bpp)))
	 (pixel-address (foreign-slot-value surface 'SDL_Surface 'Pixels)))
    (cond
      ((= bpp 1) 
       (setf (mem-aref pixel-address :unsigned-char offset) color))
      ((= bpp 2) 
       (setf (mem-aref pixel-address :unsigned-short (/ offset 2)) color))
      ((= bpp 3) 
       (if (eq SDL_BYTEORDER SDL_BIG_ENDIAN)
	   (progn
	     (setf (mem-aref pixel-address :char offset) (logand (ash color -16) #xff))
	     (setf (mem-aref pixel-address :char (1+ offset)) (logand (ash color -8) #xff))
	     (setf (mem-aref pixel-address :char (+ 2 offset)) (logand color #xff)))
	   (progn
	     (setf (mem-aref pixel-address :char offset) (logand color #xff))
	     (setf (mem-aref pixel-address :char (1+ offset)) (logand (ash color -8) #xff))
	     (setf (mem-aref pixel-address :char (+ 2 offset)) (logand (ash color -16) #xff)))))
      ((= bpp 4) 
       (setf (mem-aref pixel-address :unsigned-int (/ offset 4)) color)))))

#|
Reference source
void putpixel(SDL_Surface *surface, int x, int y, Uint32 pixel)
{
    int bpp = surface->format->BytesPerPixel;
    /* Here p is the address to the pixel we want to set */
    Uint8 *p = (Uint8 *)surface->pixels + y * surface->pitch + x * bpp;

    switch(bpp) {
    case 1:
        *p = pixel;
        break;

    case 2:
        *(Uint16 *)p = pixel;
        break;

    case 3:
        if(SDL_BYTEORDER == SDL_BIG_ENDIAN) {
            p[0] = (pixel >> 16) & 0xff;
            p[1] = (pixel >> 8) & 0xff;
            p[2] = pixel & 0xff;
        } else {
            p[0] = pixel & 0xff;
            p[1] = (pixel >> 8) & 0xff;
            p[2] = (pixel >> 16) & 0xff;
        }
        break;

    case 4:
        *(Uint32 *)p = pixel;
        break;
    }
}
|#


(defun get-pixel (&key (position sdl:*default-position*) (check-lock-p t) (surface *default-surface*))
  "Get the pixel at (x, y) as a Uint32 color value
   NOTE: The surface must be locked before calling this.
   Also NOTE: Have not tested 1,2,3 bpp surfaces, only 4 bpp"
  (with-possible-lock-and-update (:surface surface :check-lock-p check-lock-p :update-p nil
					  :template (rect-from-wh 1 1 :point position))
    (let* ((bpp (foreign-slot-value (pixelformat surface) 'SDL_PixelFormat 'BytesPerPixel))
	   (offset (+ (* (point-y position) (foreign-slot-value surface 'SDL_Surface 'Pitch))
		      (* (point-x position) bpp)))
	   (pixel-address (foreign-slot-value surface 'SDL_Surface 'Pixels)))
      (cffi:with-foreign-objects ((r :unsigned-char) (g :unsigned-char) (b :unsigned-char) (a :unsigned-char))
	(SDL_GetRGBA (cond
		       ((= bpp 1) 
			(mem-aref pixel-address :unsigned-char offset))
		       ((= bpp 2) 
			(mem-aref pixel-address :unsigned-short (/ offset 2)))
		       ((= bpp 3) 
					;	 (if (eq SDL_BYTEORDER SDL_BIG_ENDIAN) ; TODO
			(error "3 byte per pixel surfaces not supported yet"))
		       ((= bpp 4) 
			(mem-aref pixel-address :unsigned-int (/ offset 4))))
		     (pixelformat surface)
		     r g b a)
	(color (mem-aref r :unsigned-char)
	       (mem-aref g :unsigned-char)
	       (mem-aref b :unsigned-char)
	       (mem-aref a :unsigned-char))))))


#|

/*
 * Return the pixel value at (x, y)
 * NOTE: The surface must be locked before calling this!
 */
Uint32 getpixel(SDL_Surface *surface, int x, int y)
{
    int bpp = surface->format->BytesPerPixel;
    /* Here p is the address to the pixel we want to retrieve */
    Uint8 *p = (Uint8 *)surface->pixels + y * surface->pitch + x * bpp;

    switch(bpp) {
    case 1:
        return *p;

    case 2:
        return *(Uint16 *)p;

    case 3:
        if(SDL_BYTEORDER == SDL_BIG_ENDIAN)
            return p[0] << 16 | p[1] << 8 | p[2];
        else
            return p[0] | p[1] << 8 | p[2] << 16;

    case 4:
        return *(Uint32 *)p;

    default:
        return 0;       /* shouldn't happen, but avoids warnings */
    }
}

|#

(defun load-bmp (filename)
  "load in the supplied filename, must be a bmp file"
  (if (and (stringp filename) (probe-file filename)) ; LJC: Make sure filename is a string and the filename exists.
      (SDL_LoadBMP_RW (RWFromFile filename "rb") 1)
      nil))

(defun load-image (filename path &key key-color alpha-value)
  (sdl:with-surface ((sdl:load-bmp (namestring (merge-pathnames filename path))))
    (sdl:convert-surface-to-display-format :key-color key-color :alpha-value alpha-value :free-p nil)))
