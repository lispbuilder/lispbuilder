;; lispbuilder-sdl sample program
;; (C)2006 Frank Buss
;; see COPYING for license

;; To run this sample you need asdf, cffi and lispbuild-sdl installed, 
;; (asdf:operate 'asdf:load-op :cffi)
;; (asdf:operate 'asdf:load-op :lispbuilder-sdl)

;; (load "sdl_drawing")
;; (lispbuilder-sdl:mouse-painter)

(in-package #:sdl-examples)

(defconstant +screen-width+ 640)
(defconstant +screen-height+ 480)

(defmacro event-loop (event-function event &body events)
  (let ((rtype (gensym))
	(idle))
    `(with-foreign-object (,event 'sdl:SDL_Event)
      (loop do
       (if (= 1 (,event-function event))
	   (let ((,rtype (cffi:foreign-slot-value event 'sdl:SDL_Event
						  'sdl:type)))
	     (cond
	       ,@(loop for (type command) in events
		       if (eql type :idle) do (setf idle command)
		       else collect `((= ,rtype ,type) ,command))))
	   ,(when idle `(progn ,idle)))))))

(defun on-mouse-motion (surface event)
  (let ((state (cffi:foreign-slot-value event 'sdl:SDL_MouseMotionEvent 'sdl:state))
	(x (cffi:foreign-slot-value event 'sdl:SDL_MouseMotionEvent 'sdl:x))
	(y (cffi:foreign-slot-value event 'sdl:SDL_MouseMotionEvent 'sdl:y))
	(dx (cffi:foreign-slot-value event 'sdl:SDL_MouseMotionEvent 'sdl:xrel))
	(dy (cffi:foreign-slot-value event 'sdl:SDL_MouseMotionEvent 'sdl:yrel)))
    (when (= 1 state)
      (sdl:draw-line x y (- x dx) (- y dy) :surface surface :color #(0 0 0))
      (sdl:update-screen surface))))

(defun mouse-painter ()
  (unless (zerop (sdl:sdl_init sdl:SDL_INIT_VIDEO)) (error "Unable to start SDL"))
  (sdl:SDL_WM_SetCaption "Mouse Painter" "Mouse Painter")
  (let ((surface (sdl:SDL_SetVideoMode +screen-width+ +screen-height+ 0
				       sdl:SDL_ANYFORMAT)))
    (sdl:draw-rect (vector 0 0 +screen-width+ +screen-height+) :surface surface :color #(255 255 255))
    (event-loop sdl:SDL_WaitEvent event
		(sdl:SDL_QUIT (loop-finish))
		(sdl:SDL_MOUSEMOTION (on-mouse-motion surface event))
		(sdl:SDL_VIDEOEXPOSE (sdl:update-screen surface))
		(:idle #| use this and SDL_PollEvent if you want to animate something
		 |#)))
 (sdl:SDL_Quit))