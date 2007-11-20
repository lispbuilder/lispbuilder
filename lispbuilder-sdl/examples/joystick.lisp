
(in-package #:sdl-examples)

(defvar *joystick* nil)
(defvar *num-joysticks* 0)
(defvar *current-joystick-index* 0)
(defvar *status* nil)

(defstruct joystick
  fp
  index
  axis-motion
  button-down
  button-up
  hat-motion
  ball-motion
  status)

(defun incf-line (&optional (lines 1) (font sdl:*default-font*))
  (* lines (+ 2 (sdl:char-height font))))

(defun new-axis-motion-event (which axis value)
  (lambda (x y &optional (surface sdl:*default-display*))
    (sdl:draw-string-solid-* (format nil "Axis-Motion: Which: ~A, Axis: ~A, Value: ~A" which axis value)
			     x y
			     :color sdl:*white*
			     :surface surface)))

(defun new-button-down-event (which button state)
  (lambda (x y &optional (surface sdl:*default-display*))
    (sdl:draw-string-solid-* (format nil "Button-Down: Which: ~A, Button: ~A, State: ~A" which button state)
			     x y
			     :color sdl:*white*
			     :surface surface)))

(defun new-button-up-event (which button state)
  (lambda (x y &optional (surface sdl:*default-display*))
    (sdl:draw-string-solid-* (format nil "Button-Up: Which: ~A, Button: ~A, State: ~A" which button state)
			     x y
			     :color sdl:*white*
			     :surface surface)))

(defun new-hat-motion-event (which axis value)
  (lambda (x y &optional (surface sdl:*default-display*))
    (sdl:draw-string-solid-* (format nil "Hat-Motion: Which: ~A, Axis: ~A, Value: ~A" which axis value)
			     x y
			     :color sdl:*white*
			     :surface surface)))

(defun new-ball-motion-event (which ball x-rel y-rel)
  (lambda (x y &optional (surface sdl:*default-display*))
    (sdl:draw-string-solid-* (format nil "Ball-Motion: Which: ~A, Ball: ~A, X-rel: ~A, Y-rel: ~A" which ball x-rel y-rel)
			     x y
			     :color sdl:*white*
			     :surface surface)))

(defun new-joystick (index)
  (let ((fp (sdl-cffi::sdl-joystick-open index)))
    (if (sdl:is-valid-ptr fp)
	(make-joystick :fp fp
		       :index index
		       :axis-motion (new-axis-motion-event nil nil nil)
		       :button-down (new-button-down-event nil nil nil)
		       :button-up (new-button-up-event nil nil nil)
		       :hat-motion (new-hat-motion-event nil nil nil)
		       :ball-motion (new-ball-motion-event nil nil nil nil))
	nil)))

(defun close-joystick (joystick)
  (when (> (sdl-cffi::sdl-joystick-opened (joystick-index joystick)) 0)
    (sdl-cffi::sdl-joystick-close (joystick-fp joystick))))

(defun print-status (&optional (surface sdl:*default-display*))
  (let ((x 300) (y 30))
    (sdl:draw-string-solid-* "No Joystick Found / Joystick Open Error"
			     x y
			     :color sdl:*white*
			     :surface surface
			     :justify :center)))

(defun print-joystick (joystick &optional (surface sdl:*default-display*))
  (let ((x 20) (y 30))
    (sdl:draw-string-solid-* (format nil "Index: ~d \(~d of ~d\)" (joystick-index joystick) (1+ (joystick-index joystick)) *num-joysticks*)
			     x y
			     :color sdl:*white*
			     :surface surface)
    (sdl:draw-string-solid-* (format nil "Name: ~A" (sdl-cffi::sdl-joystick-name (joystick-index joystick)))
			     x (incf y (incf-line))
			     :color sdl:*white*
			     :surface surface)
    (sdl:draw-string-solid-* (format nil"Axes: ~d" (sdl-cffi::sdl-joystick-num-axes (joystick-fp joystick)))
			     x (incf y (incf-line))
			     :color sdl:*white*
			     :surface surface)
    (sdl:draw-string-solid-* (format nil"Buttons: ~d" (sdl-cffi::sdl-joystick-num-buttons (joystick-fp joystick)))
			     x (incf y (incf-line))
			     :color sdl:*white*
			     :surface surface)
    (sdl:draw-string-solid-* (format nil"Balls: ~d" (sdl-cffi::sdl-joystick-num-balls (joystick-fp joystick)))
			     x (incf y (incf-line))
			     :color sdl:*white*
			     :surface surface)
    (funcall (joystick-axis-motion joystick) x (incf y (incf-line)))
    (funcall (joystick-button-up joystick) x (incf y (incf-line)))
    (funcall (joystick-button-down joystick) x (incf y (incf-line)))
    (funcall (joystick-hat-motion joystick) x (incf y (incf-line)))
    (funcall (joystick-ball-motion joystick) x (incf y (incf-line)))))

(defun joystick ()
  (sdl:with-init (sdl:SDL-INIT-EVERYTHING)
    (sdl:window 600 240 :title-caption "Joystick" :icon-caption "Joystick")
    (setf (sdl:frame-rate) 30)
    (sdl:initialise-default-font)

    (sdl-cffi::sdl-joystick-event-state sdl-cffi::sdl-enable)
    
    (setf *num-joysticks* (sdl:num-joysticks)
	  *current-joystick-index* 0)

    (when (> *num-joysticks* 0)
      (setf *joystick* (new-joystick *current-joystick-index*)))
    
    (sdl:with-events ()
      (:quit-event ()
		   (when *joystick*
		     (close-joystick *joystick*))
		   t)
      (:key-down-event (:key key)
		       (cond
			 ((sdl:key= key :sdl-key-escape)
			  (sdl:push-quit-event))
			 ((sdl:key= key :sdl-key-space)
			  (when *joystick*
			    (close-joystick *joystick*)
			    (incf *current-joystick-index*)
			    (when (>= *current-joystick-index* *num-joysticks*)
			      (setf *current-joystick-index* 0))
			    (setf *joystick* (new-joystick *current-joystick-index*))))))
      (:joy-axis-motion-event (:WHICH WHICH :AXIS AXIS :VALUE VALUE)
			      (setf (joystick-axis-motion *joystick*) (new-axis-motion-event which axis value)))
      (:joy-button-down-event (:WHICH WHICH :BUTTON BUTTON :STATE STATE)
			      (setf (joystick-button-down *joystick*) (new-button-down-event which button state)))
      (:joy-button-up-event (:WHICH WHICH :BUTTON BUTTON :STATE STATE)
			    (setf (joystick-button-up *joystick*) (new-button-up-event which button state)))
      (:joy-hat-motion-event (:WHICH WHICH :AXIS AXIS :VALUE VALUE)
			     (setf (joystick-hat-motion *joystick*) (new-hat-motion-event which axis value)))
      (:joy-ball-motion-event (:WHICH WHICH :BALL BALL :X-REL X-REL :Y-REL Y-REL)
			      (setf (joystick-ball-motion *joystick*) (new-ball-motion-event which ball x-rel y-rel)))
      (:video-expose-event ()
			   (sdl:update-display))
      (:idle ()
	     (sdl:clear-display sdl:*black*)
	     (sdl:draw-string-solid-* (format nil "Joystick Test")
				      300 5
				      :color sdl:*white*
				      :justify :center)
	     (sdl:draw-string-solid-* (format nil "SPACE to test next joystick. ESC to exit.")
				      300 15
				      :color sdl:*white*
				      :justify :center)	     
	     (if *joystick*
		 (print-joystick *joystick*)
		 (print-status))
	     (sdl:update-display)))))
