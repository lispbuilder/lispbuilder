
;; SDL (Simple Media Layer) library using CFFI for foreign function interfacing...
;; (C)2006 Luke Crook <luke@balooga.com>
;; This file contains some useful functions for using Zach Beane's VECTO with lispbuilder-SDL
;; http://www.xach.com/lisp/vecto/

(in-package #:lispbuilder-sdl)

(defun vecto->surface (dst-surface &key (x 0) (y 0) (vecto-image vecto::*graphics-state*) (pixel-alpha t) (alpha 255))
  (check-type dst-surface sdl-surface)
  (unless (and (<= (vecto::width vecto-image)
		   (width dst-surface))
	       (<= (vecto::height vecto-image)
		   (height dst-surface)))
    (error "VECTO->SURFACE: SRC-IMAGE larger than dst-surface"))
  (sdl:with-surface (temp-surf (sdl:create-surface (vecto::width vecto-image)
						   (vecto::height vecto-image)
						   :pixel-alpha pixel-alpha
						   :alpha alpha))
    (let ((fp-to-surf (fp temp-surf)))
      (sdl-base::with-pixel (px fp-to-surf)
	(let ((image-data (vecto::image-data vecto-image)))
	  (let ((ix 0) (iy 0)
		(w (vecto::width vecto-image)))
	    (do ((h 0 (+ h 4))
		 (i 1 (+ i 4))
		 (j 2 (+ j 4))
		 (k 3 (+ k 4)))
		((<= (length image-data) k))
	      (sdl-base::write-pixel px ix iy (sdl-base::map-color fp-to-surf
								   (aref image-data h)
								   (aref image-data i)
								   (aref image-data j)
								   (aref image-data k)))
	      (incf ix)
	      (when (>= ix w)
		(setf ix 0
		      iy (1+ iy))))))))
    (sdl:draw-surface-at-* temp-surf x y :surface dst-surface))
  dst-surface)

(defun surface->vecto (src-surface dst-image)
  (declare (ignore src-surface dst-image)))

(defun surface-from-vecto (&key (vecto-image vecto::*graphics-state*) (pixel-alpha t) (alpha 255))
  (sdl:with-surface (surf (sdl:create-surface (vecto::width vecto-image)
                                              (vecto::height vecto-image)
                                              :pixel-alpha pixel-alpha
                                              :alpha alpha)
                          nil)
    (let ((fp-to-surf (fp surf)))
      (sdl-base::with-pixel (px fp-to-surf)
	(let ((image-data (vecto::image-data vecto-image)))
	  (let ((ix 0) (iy 0)
		(w (vecto::width vecto-image)))
	    (do ((h 0 (+ h 4))
		 (i 1 (+ i 4))
		 (j 2 (+ j 4))
		 (k 3 (+ k 4)))
		((<= (length image-data) k))
	      (sdl-base::write-pixel px ix iy (sdl-base::map-color fp-to-surf
								   (aref image-data h)
								   (aref image-data i)
								   (aref image-data j)
								   (aref image-data k)))
	      (incf ix)
	      (when (>= ix w)
		(setf ix 0
		      iy (1+ iy))))))))
    surf))

(defmacro with-canvas ((&rest vecto-args) &body body)
  (let ((surface (gensym "surface-")))
    `(let ((,surface nil))
       (vecto:with-canvas (,@vecto-args)
         ,@body
         (setf ,surface (surface-from-vecto)))
       ,surface)))
