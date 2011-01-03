;;;; Demonstration/Test of using SDL (Simple Media Layer) library
;;;; using CFFI for foreign function interfacing...
;;;; (C)2006 Justin Heyes-Jones, Luke J Crook.
;;;; see COPYING for license

(in-package #:sdl-image-examples) 

(defun image-example ()
  (sdl:load-library)
  (sdl:with-init ()
    (sdl:window 540 350 :title-caption "Loading images of various formats." :icon-caption "IMAGE-EXAMPLE")
    (setf (sdl:frame-rate) 30)
    (sdl:initialise-default-font)
    ;; SDL_Image 1.2.8 and later allows the jpg, png and tif
    ;; libraries to be preloaded to speed subsequent
    ;; loading of these images.
    (sdl-image:init-image :jpg :png :tif)
    (let ((images (list
                   (sdl:draw-string-solid-* "BMP" 0 0 :color sdl:*yellow*
                                            :surface (sdl-image:load-image (merge-pathnames "lisp.bmp" *bmp-path*)
                                                                           :color-key-at #(0 0)))
                   (sdl:draw-string-solid-* "GIF" 0 0 :color sdl:*yellow*
                                            :surface (sdl-image:load-image (merge-pathnames "lisp.gif" *bmp-path*)
                                                                           :color-key-at #(0 0)))
                   (sdl:draw-string-solid-* "LBM" 0 0 :color sdl:*yellow*
                                            :surface (sdl-image:load-image (merge-pathnames "lisp.lbm" *bmp-path*)
                                                                           :color-key-at #(0 0)))
                   (sdl:draw-string-solid-* "PCX" 0 0 :color sdl:*yellow*
                                            :surface (sdl-image:load-image (merge-pathnames "lisp.pcx" *bmp-path*)
                                                                           :color-key-at #(0 0)))
                   (sdl:draw-string-solid-* "PBM" 0 0 :color sdl:*yellow*
                                            :surface (sdl-image:load-image (merge-pathnames "lisp.pbm" *bmp-path*)))
                   (sdl:draw-string-solid-* "PPM" 0 0 :color sdl:*yellow*
                                            :surface (sdl-image:load-image (merge-pathnames "lisp.ppm" *bmp-path*)
                                                                           :color-key-at #(0 0)))
                   (sdl:draw-string-solid-* "PGM" 0 0 :color sdl:*yellow*
                                            :surface (sdl-image:load-image (merge-pathnames "lisp.pgm" *bmp-path*)
                                                                           :color-key-at #(0 0)))
                   (sdl:draw-string-solid-* "TGA" 0 0 :color sdl:*yellow*
                                            :surface (sdl-image:load-image (merge-pathnames "lisp.tga" *bmp-path*)
                                                                           :image-type :TGA ; TGA must be specified
									   :force t ; And loaded by force
                                                                           :color-key-at #(0 0)))
                   ;; SDL_Image 1.2.8 and later allow testing
                   ;; for png/jpg/tif support prior to loading the image.
                   (when (sdl-image:image-init-p :png)
                     (sdl:draw-string-solid-* "PNG" 0 0 :color sdl:*yellow*
                                              :surface (sdl-image:load-image (merge-pathnames "lisp.png" *bmp-path*)
                                                                             :color-key-at #(0 0))))
                   (when (sdl-image:image-init-p :tif)
                     (sdl:draw-string-solid-* "TIF" 0 0 :color sdl:*yellow*
                                              :surface (sdl-image:load-image (merge-pathnames "lisp.tif" *bmp-path*)
                                                                             :color-key-at #(0 0))))
                   (when (sdl-image:image-init-p :jpg)
                     (sdl:draw-string-solid-* "JPG" 0 0 :color sdl:*yellow*
                                              :surface (sdl-image:load-image (merge-pathnames "lisp.jpg" *bmp-path*)
                                                                             :color-key-at #(0 0))))
		   )))
      (loop for image in (remove nil images)
            for i from 0
            for (y x) = (multiple-value-list (floor i 4))
            for position = (sdl:point :x (+ 10 (* x 128))
                                      :y (+ 10 (* y 111)))
            do (sdl:draw-surface-at image position)))

    (sdl:update-display)
      
    (sdl:with-events ()
      (:quit-event ()
       ;; SDL_Image 1.2.8 and later requires a corresponding
       ;; sdl-image:quit *if* sdl-image:init is used.
       (sdl-image:quit-image)
       t)
      (:key-down-event (:key key)
       (if (sdl:key= key :SDL-KEY-ESCAPE)
         (sdl:push-quit-event)))
      (:video-expose-event () (sdl:update-display)))))
