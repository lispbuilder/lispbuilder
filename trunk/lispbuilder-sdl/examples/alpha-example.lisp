;;;; Demonstration/Test of using SDL (Simple Media Layer) library
;;;; using CFFI for foreign function interfacing...
;;;; (C)2006 Justin Heyes-Jones, Luke John Crook
;;;; see COPYING for license

(in-package #:sdl-examples) 

(defun alpha-example ()
  (sdl:with-init ()
    (sdl:window 320 180 :title-caption "Simple BMP alpha example" :icon-caption "Simple BMP alpha example")
    
    (sdl:enable-key-repeat nil nil)
    (setf (sdl:frame-rate) 30)
    (sdl:initialise-default-font sdl:*font-5x7*)
    
    (let* ((image-1 (sdl:load-image (sdl:create-path "lisp.bmp" sdl:*default-asset-path*) :alpha 0))
           (image-2 (sdl:load-image (sdl:create-path "lisp.bmp" sdl:*default-asset-path*) :alpha 255))
           (alpha-1 (sdl:alpha image-1))
           (alpha-2 (sdl:alpha image-2))
           (alpha-1-enabled (sdl:alpha-enabled-p image-1))
           (alpha-2-enabled (sdl:alpha-enabled-p image-2)))
      
      (sdl:with-events ()
        (:quit-event () t)
        
        (:key-down-event ()
         (when (sdl:key-pressed-p :sdl-key-escape)
           (sdl:push-quit-event))
         (when (sdl:key-held-p :sdl-key-q)
           (if (> (incf alpha-1 5) 255) (setf alpha-1 255)))
         (when (sdl:key-held-p :sdl-key-w)
           (if (> (incf alpha-2 5) 255) (setf alpha-2 255)))
         (when (sdl:key-held-p :sdl-key-a)
           (if (< (decf alpha-1 5) 0) (setf alpha-1 0)))
         (when (sdl:key-held-p :sdl-key-s)
           (if (< (decf alpha-2 5) 0) (setf alpha-2 0)))
         (when (sdl:key-held-p :sdl-key-z)
           (setf alpha-1-enabled (if alpha-1-enabled nil t)))
         (when (sdl:key-held-p :sdl-key-x)
           (setf alpha-2-enabled (if alpha-2-enabled nil t)))

         (setf (sdl:alpha-enabled-p image-1) alpha-1-enabled)
         (setf (sdl:alpha-enabled-p image-2) alpha-2-enabled)

         (setf (sdl:alpha image-1) alpha-1)
         (setf (sdl:alpha image-2) alpha-2))
        
        (:video-expose-event () (sdl:update-display))
        
        (:idle ()
         (sdl:clear-display sdl:*white*)
         (sdl:draw-surface-at image-1 #(10 10))
         (sdl:draw-surface-at image-2 #(150 10))

         (sdl:with-color (sdl:*black* nil nil)
           (sdl:with-surface (sdl:*default-display* nil nil)
             (sdl:draw-string-solid "'Z' - Toggle alpha" #(10 130))
             (sdl:draw-string-solid "'X' - Toggle alpha" #(150 130))

             (sdl:draw-string-solid "'Q'/'A' - Incf/Decf alpha" #(10 140))
             (sdl:draw-string-solid "'W'/'S' - Incf/Decf alpha" #(150 140))

             (sdl:draw-string-solid "-------------------------" #(10 150))
             (sdl:draw-string-solid "-------------------------" #(150 150))
         
             (sdl:draw-string-solid (format nil "alpha-enabled-p: ~A" (sdl:alpha-enabled-p image-1)) #(10 160))
             (sdl:draw-string-solid (format nil "alpha-enabled-p: ~A" (sdl:alpha-enabled-p image-2)) #(150 160))

             (sdl:draw-string-solid (format nil "alpha: ~A" (sdl:alpha image-1)) #(10 170))
             (sdl:draw-string-solid (format nil "alpha: ~A" (sdl:alpha image-2)) #(150 170))))
         
         (sdl:update-display))))))



