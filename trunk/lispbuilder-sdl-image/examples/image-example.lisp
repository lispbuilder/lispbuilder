;;;; Demonstration/Test of using SDL (Simple Media Layer) library
;;;; using CFFI for foreign function interfacing...
;;;; (C)2006 Justin Heyes-Jones
;;;; see COPYING for license

(in-package #:sdl-image-examples) 

(defvar *bmp-path* (or *load-truename* *default-pathname-defaults*))

(defun image-example ()
  (sdl:with-init ()
    (sdl:with-display (640 480)
      (sdl::with-surfaces ((alien-bmp (sdl-image:load-image "lisp.bmp" *bmp-path* :key-color #(253 59 251)))
			   (alien-gif (sdl-image:load-image "lisp.gif" *bmp-path*))
			   (alien-jpg (sdl-image:load-image "lisp.jpg" *bmp-path* :key-color #(253 59 251)))
			   (alien-lbm (sdl-image:load-image "lisp.lbm" *bmp-path*))
			   (alien-pcx (sdl-image:load-image "lisp.pcx" *bmp-path* :key-color #(253 59 251)))
;;  		           (alien-png (sdl-image:load-image "lisp__.png" *bmp-path* )) ;; Won't work. Missing png lib?
			   ;; pnm (See pbm, ppm, pgm below)
			   (alien-pbm (sdl-image:load-image "lisp.pbm" *bmp-path*))
			   (alien-ppm (sdl-image:load-image "lisp.ppm" *bmp-path* :key-color #(253 59 251)))
			   (alien-pgm (sdl-image:load-image "lisp.pgm" *bmp-path*))
 			   (alien-tga (sdl-image:load-image "lisp.tga" *bmp-path* :key-color #(253 59 251)
							    :image-type :TGA :force t))	;; TGA must be 'forced'
;; 			   (alien-tif (sdl-image:load-image "lisp_.tif" *bmp-path*)) ;; Won't work. Irfanview?
			   ;; xcf
			   ;; xpm
			   ;; xv
			   )
	(let ((image-width (sdl:surf-w alien-bmp)) (image-height (sdl:surf-h alien-bmp))
	      (image-gap 10) (x 0) (y 0))
	  (dolist (image (list alien-bmp alien-gif alien-jpg alien-lbm alien-pcx
;;  			       alien-png
			       alien-pbm alien-pgm alien-ppm alien-tga
;; 			       alien-tif
			       ))
	    (when (equal 0 (mod x 4))
	      (incf y)
	      (setf x 0))
	    (sdl:draw-image (sdl:point (+ image-gap (* x image-width)) (+ image-gap (* y image-height)))
			    image :screen sdl:*default-display*)
	    (incf x)))
	(sdl:with-events
	  (:quit t)
	  (:keydown (state scancode key mod unicode)
		    (if (sdl:is-key key :SDLK_ESCAPE)
			(sdl:push-quitevent)))
	  (:idle
	   (sdl:update-screen)))))))

