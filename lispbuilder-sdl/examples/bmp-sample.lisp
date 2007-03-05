;;;; Demonstration/Test of using SDL (Simple Media Layer) library
;;;; using CFFI for foreign function interfacing...
;;;; (C)2006 Justin Heyes-Jones
;;;; see COPYING for license

;;;; To run this sample
;;;; (load "sdl_bmp_sample.lisp")
;;;; (sdl:bmp_sample) 

(in-package #:sdl-examples) 

(defun bmp-sample ()
  (sdl:with-init ()
    (sdl:window 320 240 :title-caption "simple bmp example" :icon-caption "simple bmp example")
    (setf (sdl:frame-rate) 5)
    
    (let* ((img-1 (sdl:convert-surface :surface (sdl:load-image (sdl:create-path "sdl.bmp" *bmp-path*))
				       :free-p t))
	   (img-2 (sdl:convert-surface :surface (sdl:load-image (sdl:create-path "lisp.bmp" *bmp-path*))
				       :key-color (sdl:color :r 253 :g 59 :b 251)
				       :free-p t))
	   (img-3 (sdl:convert-surface :surface (sdl:rotate-surface 90 :surface img-2)
				       :key-color (sdl:color :r 253 :g 59 :b 251)
				       :free-p t))
	   (img-4 (sdl:convert-surface :surface (sdl:load-image (sdl:create-path "sdl.bmp" *bmp-path*))
				       :free-p t)))

      ;; Set position using 'set-position'
      (sdl:set-position-* img-1 :x 10 :y 10)
      (sdl:draw-surface img-1)
      
      ;; Set position using a 'point'
      (sdl:draw-surface-at img-2 (sdl:point :x 190 :y 10))
      
      ;; Set position using x/y spread coords
      (sdl:draw-surface-at-* img-3 80 100)

      ;; Specify a cell to clip the surface as if rendering from a sprite sheet
      (sdl:set-cell-* 10 10 80 80 :surface img-4)
      (sdl:draw-surface-at-* img-4 200 150))
    
    (sdl:with-events ()
      (:quit-event () t)
      (:video-expose-event () (sdl:update-display)))))
