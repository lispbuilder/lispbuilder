;;;; Demonstration/Test of using SDL (Simple Media Layer) library
;;;; using CFFI for foreign function interfacing...
;;;; (C)2006 Justin Heyes-Jones
;;;; see COPYING for license

(in-package #:sdl-image-examples) 

(defun image-example ()
  (sdl:with-init ()
    (sdl:window 640 480)
    (setf (sdl-base::frame-rate) 5)
    (sdl:with-surfaces ((alien-bmp (sdl:convert-surface :surface (sdl-image:load-image "lisp.bmp" *bmp-path*)
							:key-color (sdl:color :r 253 :g 59 :b 251)
							:free-p t) t)
			(alien-gif (sdl:convert-surface :surface (sdl-image:load-image "lisp.gif" *bmp-path*)
							:free-p t) t)
			(alien-jpg (sdl:convert-surface :surface (sdl-image:load-image "lisp.jpg" *bmp-path*)
							:key-color (sdl:color :r 253 :g 59 :b 251)
							:free-p t) t)
			(alien-lbm (sdl:convert-surface :surface (sdl-image:load-image "lisp.lbm" *bmp-path*)
							:free-p t) t)
			(alien-pcx (sdl:convert-surface :surface (sdl-image:load-image "lisp.pcx" *bmp-path*)
							:key-color (sdl:color :r 253 :g 59 :b 251)
							:free-p t) t)
			;; pnm (See pbm, ppm, pgm below)
			(alien-pbm (sdl:convert-surface :surface (sdl-image:load-image "lisp.pbm" *bmp-path*)
							:free-p t) t)
			(alien-ppm (sdl:convert-surface :surface (sdl-image:load-image "lisp.ppm" *bmp-path*)
							:key-color (sdl:color :r 253 :g 59 :b 251)
							:free-p t) t)
			(alien-pgm (sdl:convert-surface :surface (sdl-image:load-image "lisp.pgm" *bmp-path*)
							:free-p t) t)
			(alien-tga (sdl:convert-surface :surface (sdl-image:load-image "lisp.tga" *bmp-path*
										       :image-type :TGA	; TGA must be 'forced'
										       :force t)
							:key-color (sdl:color :r 253 :g 59 :b 251)
							:free-p t) t))
      (let ((image-width (sdl:width alien-bmp)) (image-height (sdl:height alien-bmp))
	    (image-gap 10) (x 0) (y 0))
	(dolist (image (list alien-bmp alien-gif alien-jpg alien-lbm alien-pcx
			     alien-pbm alien-pgm alien-ppm alien-tga))
	  (when (equal 0 (mod x 4))
	    (incf y)
	    (setf x 0))
	  (sdl:set-xy image (+ image-gap (* x image-width)) (+ image-gap (* y image-height)))
	  (sdl:draw-surface image
			    :surface sdl:*default-display*)
	  (incf x))
	(sdl:with-events ()
			 (:quit-event () t)
			 (:key-down-event (:key key)
					  (if (sdl-base::key= key :SDL-KEY-ESCAPE)
					      (sdl-base::push-quit-event)))
			 (:idle () (sdl:update-display)))))))
