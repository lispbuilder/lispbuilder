;;;; Demonstration/Test of using SDL (Simple Media Layer) library
;;;; using CFFI for foreign function interfacing...
;;;; (C)2006 Justin Heyes-Jones
;;;; see COPYING for license

(in-package #:sdl-image-examples) 

(defun image-example ()
  (sdl:with-init ()
    (sdl:window 640 480)
    (setf (sdl:frame-rate) 5)
    (sdl:with-surfaces ((alien-bmp (sdl-image:load-and-convert-image "lisp.bmp" *bmp-path*
								     :key-color (sdl:color :r 253 :g 59 :b 251)) t)
			(alien-gif (sdl-image:load-and-convert-image "lisp.gif" *bmp-path*) t)
			;; Uncomment alien-jpg if the necessary jpeg libraries are in the search path.
;; 			(alien-jpg (sdl-image:load-and-convert-image "lisp.jpg" *bmp-path*
;; 								     :key-color (sdl:color :r 253 :g 59 :b 251)) t)
			(alien-lbm (sdl-image:load-and-convert-image "lisp.lbm" *bmp-path*) t)
			(alien-pcx (sdl-image:load-and-convert-image "lisp.pcx" *bmp-path*
								     :key-color (sdl:color :r 253 :g 59 :b 251)) t)
			;; pnm (See pbm, ppm, pgm below)
			(alien-pbm (sdl-image:load-and-convert-image "lisp.pbm" *bmp-path*) t)
			(alien-ppm (sdl-image:load-and-convert-image "lisp.ppm" *bmp-path*
								     :key-color (sdl:color :r 253 :g 59 :b 251)) t)
			(alien-pgm (sdl-image:load-and-convert-image "lisp.pgm" *bmp-path*) t)
			(alien-tga (sdl-image:load-and-convert-image "lisp.tga" *bmp-path*
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
	  (sdl:set-position-* image :x (+ image-gap (* x image-width)) :y (+ image-gap (* y image-height)))
	  (sdl:draw-surface image
			    :surface sdl:*default-display*)
	  (incf x))
	(sdl:with-events ()
	  (:quit-event () t)
	  (:key-down-event (:key key)
			   (if (sdl:key= key :SDL-KEY-ESCAPE)
			       (sdl:push-quit-event)))
	  (:idle () (sdl:update-display)))))))
