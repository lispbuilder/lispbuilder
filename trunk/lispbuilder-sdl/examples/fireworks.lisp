;;;;; From Schaf5 at http://schaf5.wordpress.com/2007/12/31/gesegnetes-neues-jahr/

(in-package #:sdl-examples)

(defparameter *width* 640)
(defparameter *height* 480)

(defun fireworks ()
  (sdl:WITH-INIT (sdl:SDL-INIT-VIDEO)
    (sdl:WINDOW *width* *height*
		:flags (logior sdl:sdl-sw-surface))
    (setf (sdl:frame-rate) 50)
    (let ((world (make-world)))
      (sdl:WITH-EVENTS ()
	(:QUIT-EVENT () T)
	(:KEY-DOWN-EVENT
	 (:KEY key)
	 (WHEN (sdl:KEY= key :SDL-KEY-ESCAPE)
	   (sdl:PUSH-QUIT-EVENT)))
	(:IDLE
	 ()
	 (dim-screen)
	 (setf world (funcall world))
	 (sdl:UPDATE-DISPLAY))))))

(defun dim-screen ()
  (sdl:draw-box (sdl:rectangle :w *width* :h *height*)
		:color (sdl:color :r 0 :g 0 :b 0 :a 10)
		:alpha 0))

(defun random-pos ()
  (vector (random *width*)
	  (random *height*)))

(defun random-dir (&key (length (1+ (random 20))))
  (vector (- (random length) (floor length 2))
	  (- (random length) (floor length 2))))

(defun make-world (&key (objects nil))
  (lambda ()
    (let ((updated-objects
	   (loop for obj in objects
	      for new-obj = (funcall obj)
	      when new-obj collect new-obj))
	  (new-objects
	   (when (zerop (random 8))
	     (loop with pos = (random-pos)
		with rf = (+ 3 (random 7))
		with bf = (+ 3 (random 7))
		with gf = (+ 3 (random 7))
		for i below (random 70)
		collect (make-circle :pos pos
				     :red-fade rf
				     :blue-fade bf
				     :green-fade gf)))))
      (make-world :objects (append updated-objects new-objects)))))

(defun make-circle (&key
		    (pos (random-pos))
		    (dir (random-dir))
		    (radius (1+ (random 3)))
		    (red (random 256))
		    (green (random 256))
		    (blue (random 256))
		    red-fade
		    green-fade
		    blue-fade)
  (lambda ()
    (unless (= 0 red green blue)
      (sdl:draw-circle pos (floor radius) :color (sdl:color :r red
							    :g green
							    :b blue))
      (make-circle :pos (map 'vector #'+ pos dir)
		   :dir dir
		   :radius (* radius 1.05)
		   :red (max (- red red-fade) 0)
		   :green (max (- green green-fade) 0)
		   :blue (max (- blue blue-fade) 0)
		   :red-fade red-fade
		   :green-fade green-fade
		   :blue-fade blue-fade))))