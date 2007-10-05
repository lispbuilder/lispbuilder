
;; SDL (Simple Media Layer) library using CFFI for foreign function interfacing...
;; (C)2006 Luke Crook <luke@balooga.com>
;; Thanks to Frank Buss and Surendra Singh
;; see COPYING for license
;; This file contains some useful functions for using SDL from Common lisp

(in-package #:lispbuilder-sdl)

(defun vecto->surface (src-image dst-surface x y)
  (check-type dst-surface sdl-surface)
  (unless (and (<= (vecto::width src-image)
		   (width dst-surface))
	       (<= (vecto::height src-image)
		   (height dst-surface)))
    (error "VECTO->SURFACE: SRC-IMAGE larger than dst-surface"))
  (sdl:with-surface (temp-surf (sdl:create-surface (vecto::width src-image)
						   (vecto::height src-image)))
    (let ((fp-to-surf (fp temp-surf)))
      (sdl-base::with-pixel (px fp-to-surf)
	(let ((image-data (vecto::image-data src-image)))
	  (let ((ix 0) (iy 0)
		(w (vecto::width src-image)))
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