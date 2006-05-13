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

(defun fill-rect (surface x y w h color)
  (with-foreign-object (rect 'sdl:SDL_Rect)
    (setf (cffi:foreign-slot-value rect 'sdl:SDL_Rect 'sdl:x) x)
    (setf (cffi:foreign-slot-value rect 'sdl:SDL_Rect 'sdl:y) y)
    (setf (cffi:foreign-slot-value rect 'sdl:SDL_Rect 'sdl:w) w)
    (setf (cffi:foreign-slot-value rect 'sdl:SDL_Rect 'sdl:h) h)
    (if (< 0 (sdl:SDL_FillRect surface rect color))
	(error "SDL_FillRect failed"))))

(defmacro check-bounds (min below &rest vars)
  (let (result)
    (loop for var in vars do
	  (push `(when (< ,var ,min) (setf ,var ,min)) result)
	  (push `(when (>= ,var ,below) (setf ,var (1- ,below))) result))
    (push 'progn result)
    result))

(defun draw-line (surface x0 y0 x1 y1 color)
  ;; use only integers
  (setf x0 (truncate (+ 0.5 x0)))
  (setf y0 (truncate (+ 0.5 y0)))
  (setf x1 (truncate (+ 0.5 x1)))
  (setf y1 (truncate (+ 0.5 y1)))

  ;; simple clipping, should be improved with Cohen-Sutherland line clipping
  (let ((width (cffi:foreign-slot-value surface 'sdl:SDL_Surface 'sdl:w))
	(height (cffi:foreign-slot-value surface 'sdl:SDL_Surface 'sdl:h)))
    (check-bounds 0 width x0 x1)
    (check-bounds 0 height y0 y1))

  ;; draw line with Bresenham algorithm
  (let (x y e dx dy)
    (when (> x0 x1)
      (rotatef x0 x1)
      (rotatef y0 y1))
    (setf e 0)
    (setf x x0)
    (setf y y0)
    (setf dx (- x1 x0))
    (setf dy (- y1 y0))
    (if (>= dy 0)
	(if (>= dx dy)
	    (loop for x from x0 to x1 do
		  (fill-rect surface x y 1 1 color)
		  (if (< (* 2 (+ e dy)) dx)
		      (incf e dy)
		      (progn
			(incf y)
			(incf e (- dy dx)))))
	    (loop for y from y0 to y1 do
		  (fill-rect surface x y 1 1 color)
		  (if (< (* 2 (+ e dx)) dy)
		      (incf e dx)
		      (progn
			(incf x)
			(incf e (- dx dy))))))
	(if (>= dx (- dy))
	    (loop for x from x0 to x1 do
		  (fill-rect surface x y 1 1 color)
		  (if (> (* 2 (+ e dy)) (- dx))
		      (incf e dy)
		      (progn
			(decf y)
			(incf e (+ dy dx)))))
	    (progn
	      (rotatef x0 x1)
	      (rotatef y0 y1)
	      (setf x x0)
	      (setf dx (- x1 x0))
	      (setf dy (- y1 y0))
	      (loop for y from y0 to y1 do
		    (fill-rect surface x y 1 1 color)
		    (if (> (* 2 (+ e dx)) (- dy))
			(incf e dx)
			(progn
			  (decf x)
			  (incf e (+ dx dy))))))))))

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

(defun create-color (surface r g b)
  (sdl:SDL_MapRGB (cffi:foreign-slot-value surface 'sdl:SDL_Surface 'sdl:format) r g b))

(defun on-mouse-motion (surface event)
  (let ((state (cffi:foreign-slot-value event 'sdl:SDL_MouseMotionEvent 'sdl:state))
	(x (cffi:foreign-slot-value event 'sdl:SDL_MouseMotionEvent 'sdl:x))
	(y (cffi:foreign-slot-value event 'sdl:SDL_MouseMotionEvent 'sdl:y))
	(dx (cffi:foreign-slot-value event 'sdl:SDL_MouseMotionEvent 'sdl:xrel))
	(dy (cffi:foreign-slot-value event 'sdl:SDL_MouseMotionEvent 'sdl:yrel)))
    (when (= 1 state)
      (draw-line surface x y (- x dx) (- y dy) (create-color surface 0 0 0))
      (sdl:SDL_UpdateRect surface 0 0 0 0))))

(defun mouse-painter ()
  (unless (zerop (sdl:sdl_init sdl:SDL_INIT_VIDEO)) (error "Unable to start SDL"))
  (sdl:SDL_WM_SetCaption "Mouse Painter" "Mouse Painter")
  (let ((surface (sdl:SDL_SetVideoMode +screen-width+ +screen-height+ 0
				       sdl:SDL_ANYFORMAT)))
    (fill-rect surface 0 0 +screen-width+ +screen-height+ (create-color
							   surface 255 255 255))
    (event-loop sdl:SDL_WaitEvent event
		(sdl:SDL_QUIT (loop-finish))
		(sdl:SDL_MOUSEMOTION (on-mouse-motion surface event))
		(sdl:SDL_VIDEOEXPOSE (sdl:SDL_UpdateRect surface 0 0 0 0))
		(:idle #| use this and SDL_PollEvent if you want to animate something
		 |#)))
 (sdl:SDL_Quit))