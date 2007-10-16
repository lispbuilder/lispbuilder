;;;; Demonstration/Test of using SDL (Simple Media Layer) library
;;;; using CFFI for foreign function interfacing...
;;;; (C)2006 Justin Heyes-Jones
;;;; see COPYING for license

(in-package #:sdl-image-examples) 

(defvar *yellow* (sdl:color :r 255 :g 255 :b 0))

(defun image-example ()
  (sdl:with-init ()
    (sdl:window 540 250 :title-caption "Loading images of various formats." :icon-caption "IMAGE-EXAMPLE")
    (setf (sdl:frame-rate) 5)
    (sdl:initialise-default-font)
    (let ((images (list (sdl:draw-string-solid-* "BMP" 0 0 :color *yellow*
						 :surface (sdl:load-image (sdl:create-path "lisp.bmp" *bmp-path*)
									  :key-color (sdl:color :r 253 :g 59 :b 251)))
			(sdl:draw-string-solid-* "GIF" 0 0 :color *yellow*
						 :surface (sdl:load-image (sdl:create-path "lisp.gif" *bmp-path*)))
			(sdl:draw-string-solid-* "LBM" 0 0 :color *yellow*
						 :surface (sdl:load-image (sdl:create-path "lisp.lbm" *bmp-path*)
									  :key-color (sdl:color :r 253 :g 59 :b 251)))
			(sdl:draw-string-solid-* "PCX" 0 0 :color *yellow*
						 :surface (sdl:load-image (sdl:create-path "lisp.pcx" *bmp-path*)
									  :key-color (sdl:color :r 253 :g 59 :b 251)))
			(sdl:draw-string-solid-* "PBM" 0 0 :color *yellow*
						 :surface (sdl:load-image (sdl:create-path "lisp.pbm" *bmp-path*)))
			(sdl:draw-string-solid-* "PPM" 0 0 :color *yellow*
						 :surface (sdl:load-image (sdl:create-path "lisp.ppm" *bmp-path*)
									  :key-color (sdl:color :r 253 :g 59 :b 251)))
			(sdl:draw-string-solid-* "PGM" 0 0 :color *yellow*
						 :surface (sdl:load-image (sdl:create-path "lisp.pgm" *bmp-path*)))
			(sdl:draw-string-solid-* "TGA" 0 0 :color *yellow*
						 :surface (sdl:load-image (sdl:create-path "lisp.tga" *bmp-path*)
									  :image-type :TGA ; TGA must be specified
									  :key-color (sdl:color :r 253 :g 59 :b 251))))))
      (loop
	 for image in images
	 for i from 0
	 for (y x) = (multiple-value-list (floor i 4))
	 for position = (sdl:point :x (+ 10 (* x 128))
				   :y (+ 10 (* y 111)))
	 do (sdl:draw-surface-at image position))
      
      (sdl:with-events ()
	(:quit-event () t)
	(:key-down-event (:key key)
			 (if (sdl:key= key :SDL-KEY-ESCAPE)
			     (sdl:push-quit-event)))
	(:video-expose-event () (sdl:update-display))))))

