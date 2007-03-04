;;;; Demonstration/Test of using SDL (Simple Media Layer) library
;;;; using CFFI for foreign function interfacing...
;;;; (C)2006 Justin Heyes-Jones
;;;; see COPYING for license

(in-package #:sdl-image-examples) 


(defun create-path (filename)
  (namestring (merge-pathnames filename *bmp-path*)))
  
(defun image-example ()
  (sdl:with-init ()
    (sdl:window 640 480
		:title-caption "Example loading images of various types" :icon-caption "Lispbuilder-sdl-image Example")
    (setf (sdl:frame-rate) 5)
    (sdl:with-surfaces ((alien-bmp (sdl-image:load-and-convert-image (create-path "lisp.bmp")
								     :key-color (sdl:color :r 253 :g 59 :b 251)) t)
			(alien-gif (sdl-image:load-and-convert-image (create-path "lisp.gif")) t)
			;; Uncomment alien-jpg if the necessary jpeg libraries are in the search path.
;; 			(alien-jpg (sdl-image:load-and-convert-image "lisp.jpg" *bmp-path*
;; 								     :key-color (sdl:color :r 253 :g 59 :b 251)) t)
			(alien-lbm (sdl-image:load-and-convert-image (create-path "lisp.lbm")) t)
			(alien-pcx (sdl-image:load-and-convert-image (create-path "lisp.pcx")
								     :key-color (sdl:color :r 253 :g 59 :b 251)) t)
			;; pnm (See pbm, ppm, pgm below)
			(alien-pbm (sdl-image:load-and-convert-image (create-path "lisp.pbm")) t)
			(alien-ppm (sdl-image:load-and-convert-image (create-path "lisp.ppm")
								     :key-color (sdl:color :r 253 :g 59 :b 251)) t)
			(alien-pgm (sdl-image:load-and-convert-image (create-path "lisp.pgm")) t)
			(alien-tga (sdl-image:load-and-convert-image (create-path "lisp.tga")
								     :image-type :TGA ; TGA must be 'forced'
								     :force t
								     :key-color (sdl:color :r 253 :g 59 :b 251)) t))
      
      (let ((image-width (sdl:width alien-bmp)) (image-height (sdl:height alien-bmp))
	    (image-gap 10) (x 0) (y 0))
	(dolist (image (list alien-bmp alien-gif #| alien-jpg |# alien-lbm alien-pcx
			     alien-pbm alien-pgm alien-ppm alien-tga))
	  (when (equal 0 (mod x 4))
	    (incf y)
	    (setf x 0))
	  (sdl:draw-surface-at-* image (+ image-gap (* x image-width)) (+ image-gap (* y image-height))
				 :surface sdl:*default-display*)
	  (incf x))
	(sdl:with-events ()
	  (:quit-event () t)
	  (:key-down-event (:key key)
			   (if (sdl:key= key :SDL-KEY-ESCAPE)
			       (sdl:push-quit-event)))
	  (:idle () (sdl:update-display)))))))
