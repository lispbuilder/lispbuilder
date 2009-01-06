;; SDL (Simple Media Layer) library using CFFI for foreign function interfacing...
;; (C)2006 Justin Heyes-Jones <justinhj@gmail.com> and Luke Crook <luke@balooga.com>
;; Thanks to Frank Buss and Surendra Singh
;; see COPYING for license
;; This file contains some useful functions for using SDL from Common lisp
;; using sdl.lisp (the CFFI wrapper)

(in-package #:lispbuilder-sdl)

(defun within-range (p1 p2 distance)
  "Returns true `T`, if the distance between the `POINT`s `P1` `P2` is <= the distance `DISTANCE`."
  (>= distance (distance p1 p2)))

(defun within-range-* (x1 y1 x2 y2 distance)
  "Returns true `T`, if the distance between the coordinates `X1`, `Y1` and `X2`, `Y2` is <= the distance `DISTANCE`."
  (>= distance (distance-* x1 y1 x2 y2)))

(defun distance (p1 p2)
  "Returns the distance between the `POINT`s `P1` and `P2`."
  (distance-* (x p1) (y p1) (x p2) (y p2)))

(defun distance-* (x1 y1 x2 y2)
  "Returns the distance between the coordinates `X1`, `Y1` and `X2`, `Y2`."
  (sqrt (+ (expt (- x1 x2) 2) 
	   (expt (- y1 y2) 2))))

(defun zoom-surface (zoomx zoomy &key (surface *default-surface*) (free nil) (smooth nil))
  "Returns a new [SURFACE](#surface) scaled to `ZOOMX` and `ZOOMY`.

##### Parameters

* `:ZOOMX` and `ZOOMY` are the scaling factors. 
A negative scaling factor will flip the corresponding axis. 
_Note_: Flipping is only supported with anti-aliasing turned off.
* `:SURFACE` is the surface to rotate [SURFACE](#surface).
* `:FREE` when `T` will free `SURFACE`.
* `:SMOOTH` when `T` will anti-aliase the new surface.

##### Packages

* Supported in _LISPBUILDER-SDL-GFX_
* _LISPBUILDER-SDL-GFX_ ignores `:FREE`."
  (declare (ignore zoomx zoomy surface free smooth))
  nil)

;;; Anthony Fairchild.
;;; http://article.gmane.org/gmane.lisp.cl-lispbuilder.general/559
(defun rotate-surface (degrees &key (surface *default-surface*) (free nil) (zoom 1) (smooth nil))
  "Returns a new [SURFACE](#surface) rotated to `DEGREES`.

##### Parameters

* `DEGREES` is the rotation in degrees. 
* `:SURFACE` is the surface to rotate [SURFACE](#surface).
* `:FREE` when `T` will free `SURFACE`.
* `:ZOOM` is the scaling factor.
* `:SMOOTH` when `T` will anti-aliase the new surface.

##### Packages

* Also supported in _LISPBUILDER-SDL-GFX_
* _LISPBUILDER-SDL_ supports rotations of only `0`, `90`, `180`, or `270` degrees. 
_LISPBUILDER-SDL-GFX_ supports any rotation.
* _LISPBUILDER-SDL_ ignores `:SMOOTH`. _LISPBUILDER-SDL-GFX_ supports `:SMOOTH`.
* _LISPBUILDER-SDL_ ignores `:ZOOM`. _LISPBUILDER-SDL-GFX_ supports `:ZOOM`.
* _LISPBUILDER-SDL-GFX_ ignores `:FREE`."
  (declare (ignore zoom smooth)
           (type fixnum degrees)
 	   (optimize (speed 3)(safety 0)))
  (unless (member degrees '(0 90 180 270))
    (error "ERROR, ROTATE-SURFACE: degrees ~A is not one of 0, 90, 180 or 270" degrees))
  (if (= 0 degrees)
      ;; in the case of 0 degrees, just return the surface
      (let ((new-surf (copy-surface :surface surface)))
	(when free
	  (free surface))
	new-surf)
      ;; else do rotation
      (let* ((even (evenp (/ degrees 90)))
	     (w (width surface))
	     (h (height surface))
	     (new-w (if even w h))
	     (new-h (if even h w)))
	(declare (type fixnum w h new-w new-h))
	(with-surfaces ((src surface free)
			(dst (make-instance 'surface
					    :using-surface surface
					    :width new-w :height new-h
					    :bpp (bit-depth surface)
					    :enable-alpha (alpha-enabled-p surface)
					    :enable-color-key (color-key-enabled-p surface)
					    :alpha (when (alpha-enabled-p surface) (alpha surface))
					    :color-key (when (color-key-enabled-p surface) (color-key surface))
					    :pixel-alpha (pixel-alpha-enabled-p surface)) nil))
	  (let ((new-x (case degrees
			 (90  #'(lambda (x y)
				  (declare (ignore x)(type fixnum x y))
				  (the fixnum (+ (the fixnum (1- new-w)) (the fixnum (- 0 y))))))
			 (180 #'(lambda (x y)
				  (declare (ignore y)(type fixnum x y))
				  (the fixnum (+ (the fixnum (1- new-w)) (the fixnum (- 0 x))))))
			 (270 #'(lambda (x y)
				  (declare (ignore x)(type fixnum x y))
				  y))
			 (otherwise #'(lambda (x y)
					(declare (ignore y)(type fixnum x y))
					x))))
		(new-y (case degrees
			 (90  #'(lambda (x y)
				  (declare (ignore y)(type fixnum x y))
				  x))
			 (180 #'(lambda (x y)
				  (declare (ignore x)(type fixnum x y))
				  (the fixnum (+ (the fixnum (1- new-h)) (the fixnum(- 0 y))))))
			 (270 #'(lambda (x y)
				  (declare (ignore y)(type fixnum x y))
				  (the fixnum (+ (the fixnum (1- new-h)) (the fixnum (- 0 x))))))
			 (otherwise  #'(lambda (x y)
					 (declare (ignore x)(type fixnum x y))
					 y)))))
 	    (declare (type fixnum w h))
	    (sdl-base::with-pixels ((src (fp src))
				    (dst (fp dst)))
	      (loop :for x :from 0 :to (1- w)
		 :do (loop :for y :from 0 :to (1- h)
			:do (sdl-base::write-pixel dst
						   (funcall new-x x y)
						   (funcall new-y x y)
						   (sdl-base::read-pixel src x y))))))
	  dst))))

(defun rotate-surface-xy (degrees &key (surface *default-surface*) (free nil) (zoomx 1) (zoomy 1) (smooth nil))
  "Returns a new [SURFACE](#surface) rotated to `DEGREES`.

##### Parameters

* `DEGREES` is the rotation in degrees. 
* `:SURFACE` is the surface to rotate [SURFACE](#surface).
* `:FREE` when `T` will free `SURFACE`.
* `:ZOOMX` and `ZOOMY` are the the scaling factors.
A negative scaling factor will flip the corresponding axis. 
_Note_: Flipping is only supported with anti-aliasing turned off.
* `:SMOOTH` when `T` will anti-aliase the new surface.

##### Packages

* Supported in _LISPBUILDER-SDL-GFX_
* _LISPBUILDER-SDL-GFX_ ignores `:FREE`."
  (declare (ignore degrees surface free zoomx zoomy smooth))
  nil)


;;; Anthony Fairchild.
;;; http://article.gmane.org/gmane.lisp.cl-lispbuilder.general/587
;;; Converted from: http://student.kuleuven.be/~m0216922/CG/floodfill.html

;; (eval-when (:compile-toplevel :load-toplevel :execute)
;;   (asdf:operate 'asdf:load-op :lispbuilder-sdl))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A naive recursive flood fill algorithm
;;
;; This function will most certainly blow the stack.
;;
;; (defun flood-fill-recursive (x y &key (surface *default-surface*) (color *default-color*))
;;   "This function should not be used but it is
;; interesting to look at how simple a recursive flood fill can be."
;;   (sdl-base::with-pixel (pixels (fp surface))
;;     (let ((width (width surface))
;;           (height (height surface))
;;           (replacement-color (map-color color surface))
;;           (target-color  (sdl-base::read-pixel pixels x y)))
;;       (assert (/= target-color replacement-color))
;;       (labels ((fill (x y)
;;                  (when (and (>= x 0)(>= y 0)(< x width)(< y height)
;;                             (/= target-color
;;                                 (sdl-base::read-pixel pixels x y)))
;;                    (return-from fill))
;;                  (sdl-base::write-pixel pixels x y replacement-color)
;;                  (fill (1- x) y)
;;                  (fill (1+ x) y)
;;                  (fill x (1+ y))
;;                  (fill x (1- y))))
;;         (fill x y)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A consy, scanline, stack based flood fill
;;
;; Code stolen from:
;; http://student.kuleuven.be/~m0216922/CG/floodfill.html
(defun flood-fill (point &key (surface *default-surface*) (color *default-color*))
  "See [FLOOD-FILL-*](#flood-fill-*).

##### Parameters

* POINT is the position to state the fill, of type `POINT`."
  (flood-fill-* (x point) (y point) :surface surface :color color))

(defun flood-fill-* (x y &key (surface *default-surface*) (color *default-color*))
  "Performs a flood fill of surface `SURFACE` with color `COLOR`. The fill starts at the 
position specified by the `X` and `Y` coordinates. Uses a stack based flood fill that does a 
lot of consing because it uses PUSH/POP as the stack.  This function is fast.

##### Parameters

* `X` and `Y` are `INTEGER` coordinates.
* `SURFACE` is the target surface, of type `SDL:SDL-SURFACE`. Bound to `SDL:\*DEFAULT-SURFACE\*` if unspecified.
* `COLOR` is the fill color, of type `SDL:COLOR` or `SDL:COLOR-A`. Bound to `SDL:\*DEFAULT-COLOR\*` if unspecified."
  (sdl-base::with-pixel (pixels (fp surface))
    (let* ((stack nil)
           (w (width surface))
           (h (height surface))
           (new-color (map-color color surface))
           (old-color  (sdl-base::read-pixel pixels x y)))
      (when (/= old-color new-color)
        (let ((y1)
              (span-left)
              (span-right))
          (push x stack)(push y stack)
          (loop
             :while stack
             :do (let ((y (pop stack))
                       (x (pop stack)))
                   (setf y1 y)
                   (loop
                      :while (and (>= y1 0) (= (sdl-base::read-pixel pixels x y1)
                                               old-color))
                      :do (decf y1))
                   (incf y1)
                   (setf span-left nil)
                   (setf span-right nil)
                   (loop
                      :while (and (< y1 h) (= (sdl-base::read-pixel pixels x y1)
                                              old-color))
                      :do (progn (sdl-base::write-pixel pixels x y1 new-color)
                                 (if (and (not span-left)
                                          (> x 0)
                                          (= (sdl-base::read-pixel pixels (- x 1) y1)
                                             old-color))
                                     (progn (push (- x 1) stack)
                                            (push y1 stack)                                            
                                            (setf span-left T))
                                     (if (and span-left
                                              (> x 0)
                                              (/= (sdl-base::read-pixel pixels (- x 1) y1)
                                                  old-color))
                                         (setf span-left nil)))
                                 (if (and (not span-right)
                                          (< x (1- w))
                                          (= (sdl-base::read-pixel pixels (+ x 1) y1)
                                             old-color))
                                     (progn (push (+ x 1) stack)
                                            (push y1 stack)
                                            (setf span-right T))
                                     (when (and span-right
                                                (< x (1- w))
                                                (/= (sdl-base::read-pixel pixels (+ x 1) y1)
                                                    old-color))
                                       (setf span-right nil)))
                                 (incf y1))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A cons-less, scanline, stack based flood fill
;;
;; This algorithm implements its own stack for reducing
;; consing and better efficiency.
;;
;; Code stolen from:
;; http://student.kuleuven.be/~m0216922/CG/floodfill.html

(declaim (fixnum *ff-stack-size*))
(defvar *ff-stack-size* 16777215)

;; This variable is used for efficient storage of (x,y) coordinates in
;; the stack.  See FF-PUSH and FF-POP code.
(declaim (fixnum *ff-max-height*))
(defvar *ff-max-height* 1600)

;; We don't preallocate the stack because it increases the size of the
;; initial Lisp image
(defparameter *ff-stack* nil)

(declaim (fixnum *ff-stack-pointer*))
(defparameter *ff-stack-pointer* -1)

(defun ff-empty-stack()
  "Intializes the stack. Allocates it if necessary."
  (unless *ff-stack*
    (setf *ff-stack* (make-array *ff-stack-size* :element-type 'fixnum)))
  (setf *ff-stack-pointer* -1))

(defun ff-empty-p()
  "Is the stack empty?"
  (< *ff-stack-pointer* 0))

(defun ff-push(x y)
  (declare (type fixnum x y)
           (optimize (speed 3)(safety 0)))
  (when (< (1- *ff-stack-pointer*))
    (incf *ff-stack-pointer*)
    (setf (svref *ff-stack* *ff-stack-pointer*)
          (the fixnum (+ (the fixnum (* x *ff-max-height*)) y)))))

(defun ff-pop()
  (declare (optimize (speed 3)(safety 0)))
  (when (>= *ff-stack-pointer* 0)
    (let ((x (truncate (the fixnum (/ (the fixnum (svref *ff-stack* *ff-stack-pointer*)) *ff-max-height*))))
          (y (mod (the fixnum (svref *ff-stack* *ff-stack-pointer*)) *ff-max-height*)))
      (declare (type fixnum x y))
      (decf *ff-stack-pointer*)
      (values x y))))

(defun flood-fill-stack (point &key (surface *default-surface*) (color *default-color*))
  "See [FLOOD-FILL-STACK-*](#flood-fill-stack-*).

##### Parameters

* POINT is the position to state the fill, of type `POINT`."
  (flood-fill-stack-* (x point) (y point) :surface surface :color color))

(defun flood-fill-stack-* (x y &key (surface *default-surface*) (color *default-color*))
  "See [FLOOD-FILL-*](#flood-fill-*).

`FLOOD-FILL-STACK-*` is maintains an internal array-based stack.

*Note*: More of an experiment to see if an array would be faster than a bunch of consing.
 The timing of both functions indicates they run at the same speed.
With compiler declarations it may have better results.  Another
disadvantage to this is it preallocates the stack, chewing up quite a
bit of ram."
  (sdl-base::with-pixel (pixels (fp surface))
    (let* ((w (width surface))
           (h (height surface))
           (new-color (map-color color surface))
           (old-color  (sdl-base::read-pixel pixels x y)))
      (when (/= old-color new-color)
        (ff-empty-stack)
        (let ((y1)
              (span-left)
              (span-right))
          (unless (ff-push x y) (return-from flood-fill-stack-* nil))
          (loop
             :while (not (ff-empty-p))
             :do (multiple-value-bind (x y)(ff-pop)
                   (setf y1 y)
                   (loop
                      :while (and (>= y1 0) (= (sdl-base::read-pixel pixels x y1)
                                               old-color))
                      :do (decf y1))
                   (incf y1)
                   (setf span-left nil)
                   (setf span-right nil)
                   (loop
                      :while (and (< y1 h) (= (sdl-base::read-pixel pixels x y1)
                                              old-color))
                      :do (progn (sdl-base::write-pixel pixels x y1 new-color)
                                 (if (and (not span-left)
                                          (> x 0)
                                          (= (sdl-base::read-pixel pixels (- x 1) y1)
                                             old-color))
                                     (progn (unless (ff-push (- x 1) y1)
                                              (return-from flood-fill-stack-* nil))
                                            (setf span-left T))
                                     (if (and span-left
                                              (> x 0)
                                              (/= (sdl-base::read-pixel pixels (- x 1) y1)
                                                  old-color))
                                         (setf span-left nil)))
                                 (if (and (not span-right)
                                          (< x (1- w))
                                          (= (sdl-base::read-pixel pixels (+ x 1) y1)
                                             old-color))
                                     (progn (unless (ff-push (+ x 1) y1)
                                              (return-from flood-fill-stack-* nil))
                                            (setf span-right T))
                                     (when (and span-right
                                                (< x (1- w))
                                                (/= (sdl-base::read-pixel pixels (+ x 1) y1)
                                                    old-color))
                                       (setf span-right nil)))
                                 (incf y1))))))))))


;; (defun random-point (max-x max-y)
;;   (point (random max-x) (random max-y)))

;; (defun moveby-rectangle (&key (rectangle *default-rectangle*) (position *default-position*))
;;   (setf (rect-x rectangle) (+ (rect-x rectangle) (pos-x position))
;; 	(rect-y rectangle) (+ (rect-y rectangle) (pos-y position)))
;;   rectangle)

;; (defun moveto-rectangle (&key (rectangle *default-rectangle*) (position *default-position*))
;;   (setf (rect-x rectangle) (pos-x position)
;; 	(rect-y rectangle) (pos-y position))
;;   rectangle)


(defun print-surface-info (name surface)
  (format t "~A.WIDTH: ~A, ~A.HEIGHT: ~A~%" name (sdl::width surface) name (sdl::height surface))
  (format t "~A.COLOR-KEY-ENABLED-P: ~A~%" name (sdl::color-key-enabled-p surface))
  (format t "~A.COLOR-KEY: ~A:~A, ~A, ~A, ~A~%" name (sdl::color-key surface)
	  (sdl:r (sdl::color-key surface))
	  (sdl:g (sdl::color-key surface))
	  (sdl:b (sdl::color-key surface))
	  (sdl:a (sdl::color-key surface)))
  (format t "~A.ALPHA-ENABLED-P: ~A~%" name (sdl::alpha-enabled-p surface))
  (format t "~A.ALPHA: ~A~%" name (sdl::alpha surface))
  (format t "~A.PIXEL-ALPHA-ENABLED-P: ~A~%" name (sdl::pixel-alpha-enabled-p surface))
  (format t "~A.RLE-ACCEL-ENABLED-P: ~A~%" name (sdl::rle-accel-enabled-p surface))
  (format t "~A.BIT-DEPTH: ~A~%" name (sdl::bit-depth surface))
  (format t "~%"))