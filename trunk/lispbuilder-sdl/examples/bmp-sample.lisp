;;;; Demonstration/Test of using SDL (Simple Media Layer) library
;;;; using CFFI for foreign function interfacing...
;;;; (C)2006 Justin Heyes-Jones, Luke John Crook
;;;; see COPYING for license


(in-package #:sdl-examples) 

(defun bmp-sample ()
  (sdl:with-init ()
    (sdl:window 320 320 :title-caption "Simple BMP example" :icon-caption "Simple BMP example")
    (sdl:fill-surface sdl:*white* :surface sdl:*default-display*)  
    
    (let* ((img-1 (sdl:load-image (sdl:create-path "lisp.bmp" sdl:*default-asset-path*)
				  :color-key-at #(0 0)))
	   (img-2 (sdl:copy-surface :surface img-1))
	   (img-3 (sdl:copy-surface :cell (sdl:rectangle :x 10 :y 10 :w 100 :h 100) :surface img-1))
	   (img-4 (sdl:rotate-surface 90 :surface img-1)))
           
      (sdl:draw-surface-at img-1 #(10 10))
      (sdl:draw-surface-at img-2 #(150 10))
      (sdl:draw-surface-at img-3 #(10 150))
      (sdl:draw-surface-at img-4 #(150 150)))
    
    (sdl:update-display)
    (sdl:with-events ()
      (:quit-event () t)
      (:video-expose-event () (sdl:update-display)))))