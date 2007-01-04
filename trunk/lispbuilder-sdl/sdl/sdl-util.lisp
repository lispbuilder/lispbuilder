;; SDL (Simple Media Layer) library using CFFI for foreign function interfacing...
;; (C)2006 Justin Heyes-Jones <justinhj@gmail.com> and Luke Crook <luke@balooga.com>
;; Thanks to Frank Buss and Surendra Singh
;; see COPYING for license
;; This file contains some useful functions for using SDL from Common lisp
;; using sdl.lisp (the CFFI wrapper)

(in-package #:lispbuilder-sdl)

(defun points-in-range (p1 p2 distance)
  "Returns true T, if the distance between the points p1 POINT and p2 POINT is <= DISTANCE"
  (>= distance (distance-to-point p1 p2)))

(defun distance-to-point (p1 p2)
  "Returns the distance between the points p1 POINT and p2 POINT."
  (sqrt (+ (expt (- (x p1) (x p2)) 2)
	   (expt (- (y p1) (y p2)) 2))))

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
	    (sdl-base::with-pixels ((src-pix (fp src))
				    (dst-pix (fp dst)))
	      (loop :for x :from 0 :to (1- w)
		 :do (loop :for y :from 0 :to (1- h)
			:do (dst-pix.write-pixel (funcall new-x x y)
						 (funcall new-y x y)
						 (src-pix.read-pixel x y))))))
	  dst))))

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
