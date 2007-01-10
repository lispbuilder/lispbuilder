;; SDL (Simple Media Layer) library using CFFI for foreign function interfacing...
;; (C)2006 Justin Heyes-Jones <justinhj@gmail.com> and Luke Crook <luke@balooga.com>
;; Thanks to Frank Buss and Surendra Singh
;; see COPYING for license
;; This file contains some useful functions for using SDL from Common lisp
;; using sdl.lisp (the CFFI wrapper)

(in-package #:lispbuilder-sdl)


(defun within-range (p1 p2 distance)
  "Returns true T, if the distance between the points p1 POINT and p2 POINT is <= DISTANCE"
  (>= distance (distance p1 p2)))

(defun within-range-* (x1 y1 x2 y2 distance)
  (>= distance (distance-* x1 y1 x2 y2)))

(defun distance (p1 p2)
  "Returns the distance between the points p1 POINT and p2 POINT."
  (distance-* (x p1) (y p1) (x p2) (y p2)))

(defun distance-* (x1 y1 x2 y2)
  (sqrt (+ (expt (- x1 x2) 2) 
	   (expt (- y1 y2) 2))))

;;; Anthony Fairchild.
;;; http://article.gmane.org/gmane.lisp.cl-lispbuilder.general/559
(defun rotate-surface (degrees &key
		       (surface *default-surface*) (free-p nil))
  "Returns a new Surface rotated 0, 90, 180, or 270 degrees.
When :free-p is T, the source surface SURFACE is freed."
  (declare (type fixnum degrees)
           (optimize (speed 3)(safety 0)))
  (unless (member degrees '(0 90 180 270))
    (error "ERROR, ROTATE-SURFACE: degrees ~A is not one of 0, 90, 180 or 270" degrees))
  (if (= 0 degrees)
      ;; in the case of 0 degrees, just return the surface
      (let ((new-surf (surface (fp surface))))
	(when free-p
	  (free-surface surface))
	new-surf)
      ;; else do rotation
      (let* ((even (evenp (/ degrees 90)))
	     (w (width surface))
	     (h (height surface))
	     (new-w (if even w h))
	     (new-h (if even h w)))
	(with-surfaces ((src surface free-p)
			(dst (create-surface new-w new-h :surface surface) nil))
	  (let ((new-x (case degrees
			 (90  #'(lambda (x y)
				  (declare (ignore x)(type fixnum x y))
				  (+ (1- new-w) (- 0 y))))
			 (180 #'(lambda (x y)
				  (declare (ignore y)(type fixnum x y))
				  (+ (1- new-w) (- 0 x))))
			 (270 #'(lambda (x y)
				  (declare (ignore x)(type fixnum x y))
				  y))))
		(new-y (case degrees
			 (90  #'(lambda (x y)
				  (declare (ignore y)(type fixnum x y))
				  x))
			 (180 #'(lambda (x y)
				  (declare (ignore x)(type fixnum x y))
				  (+ (1- new-h) (- 0 y))))
			 (270 #'(lambda (x y)
				  (declare (ignore y)(type fixnum x y))
				  (+ (1- new-h) (- 0 x)))))))
	    (sdl-base::with-pixels ((src (fp src))
				    (dst (fp dst)))
	      (loop :for x :from 0 :to (1- w)
		 :do (loop :for y :from 0 :to (1- h)
			:do (sdl-base::write-pixel dst
						   (funcall new-x x y)
						   (funcall new-y x y)
						   (sdl-base::read-pixel src x y))))))
	  dst))))

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
;;           (replacement-color (sdl:map-color color surface))
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
  (flood-fill-* (x point) (y point) :surface surface :color color))

(defun flood-fill-* (x y &key (surface *default-surface*) (color *default-color*))
  "A stack based flood fill that does a lot of consing
because it uses PUSH/POP as the stack.  This function is fast."
  (sdl-base::with-pixel (pixels (fp surface))
    (let* ((stack nil)
           (w (width surface))
           (h (height surface))
           (new-color (sdl:map-color color surface))
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

(defparameter *ff-stack-size* 16777215)

;; This variable is used for efficient storage of (x,y) coordinates in
;; the stack.  See FF-PUSH and FF-POP code.
(defparameter *ff-max-height* 1600)

;; We don't preallocate the stack because it increases the size of the
;; initial Lisp image
(defparameter *ff-stack* nil)

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
    (setf (aref *ff-stack* *ff-stack-pointer*)
          (+ (* x *ff-max-height*) y))))

(defun ff-pop()
  (when (>= *ff-stack-pointer* 0)
    (let ((x (truncate (/ (aref *ff-stack* *ff-stack-pointer*) *ff-max-height*)))
          (y (mod (aref *ff-stack* *ff-stack-pointer*) *ff-max-height*)))
      (decf *ff-stack-pointer*)
      (values x y))))

(defun flood-fill-stack (point &key (surface *default-surface*) (color *default-color*))
  (flood-fill-stack-* (x point) (y point) :surface surface :color color))

(defun flood-fill-stack-* (x y &key (surface *default-surface*) (color *default-color*))
  "This function is the same as the one above
but has its own custom array-based stack.  It was more of an
experiment to see if an array would be faster than a bunch of consing.
 The timing of both functions indicates they run at the same speed.
With compiler declarations it may have better results.  Another
disadvantage to this is it preallocates the stack, chewing up quite a
bit of ram."
  (sdl-base::with-pixel (pixels (fp surface))
    (let* ((w (width surface))
           (h (height surface))
           (new-color (sdl:map-color color surface))
           (old-color  (sdl-base::read-pixel pixels x y)))
      (when (/= old-color new-color)
        (ff-empty-stack)
        (let ((y1)
              (span-left)
              (span-right))
          (when (not (ff-push x y)) (return-from flood-fill-stack-* nil))
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
                                     (progn (when (not (ff-push (- x 1) y1))
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
                                     (progn (when (not (ff-push (+ x 1) y1))
                                              (return-from flood-fill-stack-* nil))
                                            (setf span-right T))
                                     (when (and span-right
                                                (< x (1- w))
                                                (/= (sdl-base::read-pixel pixels (+ x 1) y1)
                                                    old-color))
                                       (setf span-right nil)))
                                 (incf y1))))))))))


;; (defun random-point (max-x max-y)
;;   (sdl:point (random max-x) (random max-y)))

;; (defun moveby-rectangle (&key (rectangle *default-rectangle*) (position *default-position*))
;;   (setf (rect-x rectangle) (+ (rect-x rectangle) (pos-x position))
;; 	(rect-y rectangle) (+ (rect-y rectangle) (pos-y position)))
;;   rectangle)

;; (defun moveto-rectangle (&key (rectangle *default-rectangle*) (position *default-position*))
;;   (setf (rect-x rectangle) (pos-x position)
;; 	(rect-y rectangle) (pos-y position))
;;   rectangle)
