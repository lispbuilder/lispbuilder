;;;; Demonstration/Test of using SDL (Simple Media Layer) library
;;;; using CFFI for foreign function interfacing...
;;;; (C)2006 Justin Heyes-Jones, Luke J Crook.
;;;; see COPYING for license

(in-package #:sdl-image-examples) 

(defun image-example ()
  (sdl:with-init ()
    (sdl:window 540 250 :title-caption "Loading images of various formats." :icon-caption "IMAGE-EXAMPLE")
    (setf (sdl:frame-rate) 5)
    (sdl:initialise-default-font)
    (let ((images (list (sdl:draw-string-solid-* "BMP" 0 0 :color sdl:*yellow*
						 :surface (sdl-image:load-and-convert-image (sdl:create-path "lisp.bmp" *bmp-path*)
											    :color-key-at #(0 0)))
			(sdl:draw-string-solid-* "GIF" 0 0 :color sdl:*yellow*
						 :surface (sdl-image:load-image (sdl:create-path "lisp.gif" *bmp-path*)
										:color-key-at #(0 0)))
			(sdl:draw-string-solid-* "LBM" 0 0 :color sdl:*yellow*
						 :surface (sdl-image:load-image (sdl:create-path "lisp.lbm" *bmp-path*)
										:color-key-at #(0 0)))
			(sdl:draw-string-solid-* "PCX" 0 0 :color sdl:*yellow*
						 :surface (sdl-image:load-image (sdl:create-path "lisp.pcx" *bmp-path*)
										:color-key-at #(0 0)))
			(sdl:draw-string-solid-* "PBM" 0 0 :color sdl:*yellow*
						 :surface (sdl-image:load-image (sdl:create-path "lisp.pbm" *bmp-path*)))
			(sdl:draw-string-solid-* "PPM" 0 0 :color sdl:*yellow*
						 :surface (sdl-image:load-image (sdl:create-path "lisp.ppm" *bmp-path*)
										:color-key-at #(0 0)))
			(sdl:draw-string-solid-* "PGM" 0 0 :color sdl:*yellow*
						 :surface (sdl-image:load-image (sdl:create-path "lisp.pgm" *bmp-path*)
										:color-key-at #(0 0)))
			(sdl:draw-string-solid-* "TGA" 0 0 :color sdl:*yellow*
						 :surface (sdl-image:load-image (sdl:create-path "lisp.tga" *bmp-path*)
										:image-type :TGA ; TGA must be specified
										:color-key-at #(0 0))))))
      (loop
	 for image in images
	 for i from 0
	 for (y x) = (multiple-value-list (floor i 4))
	 for position = (sdl:point :x (+ 10 (* x 128))
				   :y (+ 10 (* y 111)))
	 do (sdl:draw-surface-at image position)))
      
      (sdl:with-events ()
	(:quit-event () t)
	(:key-down-event (:key key)
			 (if (sdl:key= key :SDL-KEY-ESCAPE)
			     (sdl:push-quit-event)))
	(:video-expose-event () (sdl:update-display)))))

