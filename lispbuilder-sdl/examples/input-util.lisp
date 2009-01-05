;;;; Demonstration/Test of using SDL (Simple Media Layer) library
;;;; using CFFI for foreign function interfacing...
;;;; (C)2008 Justin Heyes-Jones, Luke Crook
;;;; see COPYING for license

(in-package #:sdl-examples) 
  
(defparameter *window-x* 400)
(defparameter *window-y* 400)

(defun get-key-status-as-string(key)
  (if (sdl:key-pressed-p key)
      "Pressed"
      (if (sdl:key-released-p key)
	  "Released"
	  (if (sdl:key-held-p key)
	      "Held"
	      "Not Held"))))
  
(defun sdl-input()
  (sdl:with-init ()
    (let ((font (sdl:initialise-default-font sdl:*font-10x20*)))
      (sdl:initialise-input-util)
      (sdl:window *window-x* *window-y*)
      (setf (sdl:frame-rate) 30)
      (sdl:with-events ()
	(:quit-event () t)
	
	(:key-down-event (:key key)
			 (sdl:handle-key-down key)
			 (if (sdl:key= key :SDL-KEY-ESCAPE)
			     (sdl:push-quit-event)))
	(:key-up-event (:key key)
		       (sdl:handle-key-up key))
	
	(:idle ()
	       ;; simple text print out for press and releases
	       (if (sdl:key-pressed-p :SDL-KEY-A)
		   (format t "A pressed~%"))
		   
	       (if (sdl:key-released-p :SDL-KEY-A)
		   (format t "A released~%"))

	       ;; update input system
	       (sdl:update-input-util (sdl:frame-time))

       	       ;; fill the background
	       (sdl:clear-display (sdl:color :r #x00 :g #x00 :b #x00))

	       ;; draw A key status 
	       (sdl:draw-string-solid-* (format nil "A key state : ~a" (get-key-status-as-string :SDL-KEY-A))
					20 40 
					:color sdl:*white* :font font :justify :left)

	       (sdl:draw-string-solid-* (format nil "A key time : ~06,2f" (sdl:key-time-in-current-state :SDL-KEY-A))
					20 60
					:color sdl:*white* :font font :justify :left)

	       (sdl:draw-string-solid-* (format nil "A key previous time : ~06,2f" (sdl:key-time-in-previous-state :SDL-KEY-A))
					20 80
					:color sdl:*white* :font font :justify :left)

	       (sdl:update-display)))
      (sdl:quit-input-util))))




