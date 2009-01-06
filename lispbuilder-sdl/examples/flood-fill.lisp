;;; Anthony Fairchild.
;;; http://article.gmane.org/gmane.lisp.cl-lispbuilder.general/587

(in-package #:sdl-examples)

(defun flood-fill ()
  (let ((width 200) (height 200)
	(100-frames-p (every-n-frames 100)))
    (sdl:with-init ()
      (sdl:window width height :title-caption "Flood Fill")
      (setf (sdl:frame-rate) 0)
      (sdl:initialise-default-font sdl:*font-5x7*)
      (draw-fps "Calculating FPS....." 10 180 sdl:*default-font* sdl:*default-surface* t)
      (sdl:with-events ()
	(:quit-event () t)
	(:video-expose-event () (sdl:update-display))
	(:idle ()
         (sdl:clear-display sdl:*black*)
         ;; Draw a triangle
         (sdl:with-surface (disp sdl:*default-display*)
           (sdl:with-color (white (sdl:color :r 255 :g 255 :b 255))
             (sdl:with-shape (:solid)
               (sdl:add-vertex-* 100 50)
               (sdl:add-vertex-* 25  150)
               (sdl:add-vertex-* 175 150)
               (sdl:add-vertex-* 100 50))))
         (sdl:with-color (random-color (sdl:color :r (random 256) :g (random 256) :b (random 256)))
           (sdl:flood-fill-* 100 100 :surface sdl:*default-display*))
         (draw-fps (format nil "FPS : ~2$" (sdl:average-fps))
                   10 180 sdl:*default-font* sdl:*default-surface*
                   (funcall 100-frames-p))
         (sdl:update-display))))))

(defun flood-fill-timed ()
  (let ((width 200) (height 200))
    (sdl:with-init ()
      (sdl:window width height :title-caption "Flood Fill")
      (setf (sdl:frame-rate) 5)
      (sdl:clear-display (sdl::color :r 0 :g 0 :b 0))
      ;; Draw a triangle
      (sdl:with-surface (disp sdl:*default-display*)
	(sdl:with-color (white (sdl:color :r 255 :g 255 :b 255))
	  (sdl:with-shape (:solid)
	    (sdl:add-vertex-* 100 50)
	    (sdl:add-vertex-* 25  150)
	    (sdl:add-vertex-* 175 150)
	    (sdl:add-vertex-* 100 50))))
      (let ((iterations 10))
	;; Time flood-fill-stack
	(time (loop
               :for x :from 1 :to iterations
               :do (sdl:with-color (random-color (sdl:color :r (random 256) :g (random 256) :b (random 256)))
                     (sdl:flood-fill-* 100 100 :surface sdl:*default-display*)))))
      (sdl:update-display)
      (sdl:with-events ()
	(:quit-event () t)
	(:video-expose-event () (sdl:update-display))))))

(defun flood-fill-stack ()
  (let ((width 200) (height 200)
	(100-frames-p (every-n-frames 100)))
    (sdl:with-init ()
      (sdl:window width height :title-caption "Flood Fill")
      (setf (sdl:frame-rate) 15)

      (sdl:clear-display sdl:*black*)
      (sdl:initialise-default-font sdl:*font-5x7*)
      (draw-fps "Calculating FPS....." 10 180 sdl:*default-font* sdl:*default-surface* t)

      (sdl:with-events ()
	(:quit-event () t)
	(:video-expose-event () (sdl:update-display))
	(:idle ()
	       (sdl:clear-display sdl:*black*)
	       ;; Draw a triangle
	       (sdl:with-surface (disp sdl:*default-display*)
		 (sdl:with-color (white (sdl:color :r 255 :g 255 :b 255))
		   (sdl:with-shape (:solid)
		     (sdl:add-vertex (sdl:point :x 100 :y 50))
		     (sdl:add-vertex (sdl:point :x 25 :y 150))
		     (sdl:add-vertex (sdl:point :x 175 :y 150))
		     (sdl:add-vertex (sdl:point :x 100 :y 50)))))
	       (sdl:with-color (random-color (sdl:color :r (random 256) :g (random 256) :b (random 256)))
		 (sdl:flood-fill-stack-* 100 100 :surface sdl:*default-display*))
	       (draw-fps (format nil "FPS : ~2$" (sdl:average-fps))
			 10 180 sdl:*default-font* sdl:*default-surface*
			 (funcall 100-frames-p))
	       (sdl:update-display))))))

(defun flood-fill-stack-timed ()
  (let ((width 200) (height 200))
    (sdl:with-init ()
      (sdl:window width height :title-caption "Flood Fill")
      (setf (sdl:frame-rate) 5)
      (sdl:clear-display (sdl::color :r 0 :g 0 :b 0))
      ;; Draw a triangle
      (sdl:with-surface (disp sdl:*default-display*)
	(sdl:with-color (white (sdl:color :r 255 :g 255 :b 255))
	  (sdl:with-shape (:solid)
	    (sdl:add-vertex (sdl:point :x 100 :y 50))
	    (sdl:add-vertex (sdl:point :x 25 :y 150))
	    (sdl:add-vertex (sdl:point :x 175 :y 150))
	    (sdl:add-vertex (sdl:point :x 100 :y 50)))))
      (let ((iterations 10))
	;; Time flood-fill-stack
	(time (loop
		 :for x :from 1 :to iterations
		 :do (sdl:with-color (random-color (sdl:color :r (random 256) :g (random 256) :b (random 256)))
		       (sdl:flood-fill-stack-* 100 100 :surface sdl:*default-display*)))))
      (sdl:update-display)
      (sdl:with-events ()
	(:quit-event () t)
	(:video-expose-event () (sdl:update-display))))))

