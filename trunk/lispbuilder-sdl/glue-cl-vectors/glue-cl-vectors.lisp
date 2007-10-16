
;; SDL (Simple Media Layer) library using CFFI for foreign function interfacing...
;; (C)2006 Luke Crook <luke@balooga.com>
;; Thanks to Frank Buss and Surendra Singh
;; see COPYING for license
;; This file contains some useful functions for using SDL from Common lisp
;; using sdl.lisp (the CFFI wrapper)

(in-package #:lispbuilder-sdl)

(defun image-put-pixel (image &optional (color #(0 0 0)) (opacity 1.0) (alpha-function :normalized))
  (check-type image sdl-surface)
  (let ((width (width image))
        (height (height image)))
    (case alpha-function
      (:normalized
       (setf alpha-function #'aa-misc::alpha/normalized))
      (:even-odd
       (setf alpha-function #'aa-misc::alpha/even-odd)))
    (if (/= opacity 1.0)
        (lambda (x y alpha)
	  (declare (optimize speed (safety 0) (debug 0)))
          (when (and (<= 0 x (1- width))
                     (<= 0 y (1- height)))
	    (setf alpha (funcall alpha-function alpha))
	    (sdl-base::with-pixel (px (fp image))
	      (multiple-value-bind (rgba r g b a)
		  (sdl-base::read-pixel px x y)
		(declare (ignore rgba a))
		(sdl-base::write-pixel px x y (sdl:map-color-* (aa-misc::blend-value r
										     (aref color 0)
										     (floor (* opacity alpha)))
							       (aa-misc::blend-value g
										     (aref color 1)
										     (floor (* opacity alpha)))
							       (aa-misc::blend-value b
										     (aref color 2)
										     (floor (* opacity alpha)))
							       nil
							       image))))))
        (lambda (x y alpha)
          (declare (optimize speed (safety 0) (debug 0)))
          (when (and (<= 0 x (1- width))
                     (<= 0 y (1- height)))
	    (setf alpha (funcall alpha-function alpha))
	    (sdl-base::with-pixel (px (fp image))
	      (multiple-value-bind (rgba r g b a)
		  (sdl-base::read-pixel px x y)
		(declare (ignore rgba a))
		(sdl-base::write-pixel px x y (sdl:map-color-* (aa-misc::blend-value r (aref color 0) alpha)
							       (aa-misc::blend-value g (aref color 1) alpha)
							       (aa-misc::blend-value b (aref color 2) alpha)
							       nil
							       image)))))))))

(defun image-put-span (image &optional (color #(0 0 0)) (opacity 1.0) (alpha-function :normalized))
  (check-type image sdl-surface)
  (let ((width (width image))
        (height (height image)))
    (case alpha-function
      (:normalized
       (setf alpha-function #'aa-misc::alpha/normalized))
      (:even-odd
       (setf alpha-function #'aa-misc::alpha/even-odd)))
    (if (/= opacity 1.0)
        (lambda (x1 x2 y alpha)
          (declare (optimize speed (safety 0) (debug 0)))
          (when (and (< x1 width)
                     (> x2 0)
                     (<= 0 y (1- height)))
            (setf alpha (funcall alpha-function alpha))
            (loop for x from (max 0 x1) below (min x2 width)
	       do (sdl-base::with-pixel (px (fp image))
		    (multiple-value-bind (rgba r g b a)
			(sdl-base::read-pixel px x y)
		      (declare (ignore rgba a))
		      (sdl-base::write-pixel px x y (sdl:map-color-* (aa-misc::blend-value r
											   (aref color 0)
											   (floor (* opacity alpha)))
								     (aa-misc::blend-value g
											   (aref color 1)
											   (floor (* opacity alpha)))
								     (aa-misc::blend-value b
											   (aref color 2)
											   (floor (* opacity alpha)))
								     nil
								     image)))))))
        (lambda (x1 x2 y alpha)
          (declare (optimize speed (safety 0) (debug 0)))
          (when (and (< x1 width)
                     (> x2 0)
                     (<= 0 y (1- height)))
            (setf alpha (funcall alpha-function alpha))
            (loop for x from (max 0 x1) below (min x2 width)
	       do (sdl-base::with-pixel (px (fp image))
		    (multiple-value-bind (rgba r g b a)
			(sdl-base::read-pixel px x y)
		      (declare (ignore rgba a))
		      (sdl-base::write-pixel px x y (sdl:map-color-* (aa-misc::blend-value r (aref color 0) alpha)
								     (aa-misc::blend-value g (aref color 1) alpha)
								     (aa-misc::blend-value b (aref color 2) alpha)
								     nil
								     image))))))))))