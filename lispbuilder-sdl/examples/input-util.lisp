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
      (if (sdl:key-held-p key)
	  "Held"
	  "Not Held")))
  
(defun sdl-input()
  (sdl:with-init ()
    (let ((font (sdl:initialise-default-font sdl:*font-10x20*)))
      (sdl:initialise-input-util)
      (sdl:window *window-x* *window-y*)
      (setf (sdl:frame-rate) 0)
      (sdl:with-events ()
	(:quit-event () t)
	
	(:key-down-event (:key key)
			 (sdl:handle-key-down key)
			 (if (sdl:key= key :SDL-KEY-ESCAPE)
			     (sdl:push-quit-event)))
	(:key-up-event (:key key)
		       (sdl:handle-key-up key))
	
	(:idle ()
	       (sdl:update-input-util 0.016)
       	       ;; fill the background
	       (sdl:clear-display (sdl:color :r #x00 :g #x00 :b #x00))
	       ;; draw A key status 
	       (sdl:draw-string-solid-* (format nil "A key time : ~06,2f" (sdl:key-held-time :SDL-KEY-A))
					20 20
					:color sdl:*white* :font font :justify :left)
	       (sdl:draw-string-solid-* (format nil "A key state : ~a" (get-key-status-as-string :SDL-KEY-A))
					20 40 
					:color sdl:*white* :font font :justify :left)

	       (sdl:update-display)))
      (sdl:quit-input-util))))


