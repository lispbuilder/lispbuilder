;;;; Demonstration/Test of using SDL (Simple Media Layer) library
;;;; using CFFI for foreign function interfacing...
;;;; (C)2006 Justin Heyes-Jones, Luke J Crook.
;;;; see COPYING for license

(in-package #:sdl-examples) 

(defun image-example ()
  (sdl:with-init ()
    (sdl:window 540 350 :title-caption "Loading images of various formats." :icon-caption "IMAGE-EXAMPLE")
    (setf (sdl:frame-rate) 30)
    (sdl:initialise-default-font)
    ;; SDL_Image 1.2.8 and later allows the jpg, png and tif
    ;; libraries to be preloaded to speed subsequent
    ;; loading of these images.
    (sdl:init-image :jpg :png :tif)
    (let* ((images (loop for format in (loop for format in (sdl:supported-image-formats) collecting (symbol-name format))
                         collecting (let ((supported? (sdl:image-supported-p (merge-pathnames (format nil "lisp.~A" format) sdl:*default-asset-path*))))
                                      (when supported?
                                        (sdl:draw-string-solid-* format 0 0 :color sdl:*yellow*
                                                                 :surface (sdl:load-image supported? :color-key-at #(0 0))))))))
      ;;(sdl:draw-string-solid-* "TGA" 0 0 :color sdl:*yellow*
      ;;                           :surface (sdl:load-image (merge-pathnames "lisp.tga" *bmp-path*)
      ;;                                                    :image-type :TGA ; TGA must be specified
      ;;                                                    :color-key-at #(0 0)))
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
       ;; sdl:quit-image *if* sdl:init-image is used.
       (sdl:quit-image)
       t)
      (:key-down-event ()
       (when (sdl:key-pressed-p :SDL-KEY-ESCAPE)
         (sdl:push-quit-event)))
      (:video-expose-event () (sdl:update-display)))))
