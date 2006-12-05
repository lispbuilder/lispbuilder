;; SDL (Simple Media Layer) library using CFFI for foreign function interfacing...
;; (C)2006 Luke Crook <luke@balooga.com>
;; Thanks to Frank Buss and Surendra Singh
;; see COPYING for license
;; This file contains some useful functions for using SDL from Common lisp
;; using sdl.lisp (the CFFI wrapper)

(in-package #:lispbuilder-sdl)


(defun new-event (&key (event-type 'SDL_Event))
  "Creates a new SDL_Event and sets the type to :event-type.
   If no type is specified, then an SDL_Event of type SDL_NOEVENT is returned.
   For example, to create a quit event use :event-type 'SDL_QuitEvent."
  (let ((event (cffi:foreign-alloc event-type)))
    (setf (cffi:foreign-slot-value event 'SDL_event 'type)
	  (case event-type
	    ('sdl_quitevent (cffi:foreign-enum-value 'SDL_EventType :SDL_QUIT))
	    (otherwise (cffi:foreign-enum-value 'SDL_EventType :SDL_NOEVENT))))
    event))

(defun push-quitevent ()
  "Pushes a new SDL_Event of type SDL_QUIT onto the event queue."
  (SDL_PushEvent (new-event :event-type 'sdl_quitevent)))

;;; Event Handling & Timing routine from here   -----------------------


(let ((timescale nil))
    (defun set-timescale (tscale)
        (setf timescale tscale))
    (defun get-timescale ()
        timescale))

(let ((ticks nil))
    (defun set-ticks (tcks)
        (setf ticks tcks))
    (defun get-ticks ()
        ticks))

(let ((worldtime 100))
    (defun set-worldtime (wtime)
        (setf worldtime wtime))
    (defun get-worldtime ()
        worldtime))

(defstruct fpsmanager
  (framecount 0 :type fixnum)
  (rate 30 :type fixnum)
  (rateticks (/ 1000.0 30.0) :type float)
  (lastticks 0 :type fixnum))

(let ((fpsmngr (make-fpsmanager)) (fps-upper-limit 200) (fps-lower-limit 1)
      (current-ticks 0) (target-ticks 0))
;  (declare (type fixnum fps-upper-limit fps-lower-limit current-ticks target-ticks))
  (defun init-framerate-manager()
    (setf fpsmngr (make-fpsmanager)))
  (defun set-framerate (rate)
    (if (> rate 0)
        (if (and (>= rate fps-lower-limit) (<= rate fps-upper-limit))
            (progn
              (setf (fpsmanager-framecount fpsmngr) 0)
              (setf (fpsmanager-rate fpsmngr) rate)
              (setf (fpsmanager-rateticks fpsmngr) (/ 1000.0 rate))
              t)
	    nil)
	(setf (fpsmanager-rate fpsmngr) rate)))
  (defun get-framerate ()
    (fpsmanager-rate fpsmngr))
  (defun framerate-delay ()
    (when (> (fpsmanager-rate fpsmngr) 0)
      (setf current-ticks (sdl_getticks))
      (incf (fpsmanager-framecount fpsmngr))
      (setf target-ticks (+ (fpsmanager-lastticks fpsmngr) 
			    (* (fpsmanager-framecount fpsmngr) (fpsmanager-rateticks fpsmngr))))
      (if (<= current-ticks target-ticks)
	  (sdl_delay (round (- target-ticks current-ticks)))
	  (progn
	    (setf (fpsmanager-framecount fpsmngr) 0)
	    (setf (fpsmanager-lastticks fpsmngr) (sdl_getticks)))))))

(defun expand-activeevent (sdl-event params forms)
    (let ((keyword-list nil)
	(keywords nil))
    (do ((keyword params (if (cdr keyword)
			     (cddr keyword)
			     nil)))
	((null keyword))
      (push (list (first keyword) (second keyword)) keyword-list))
    (setf keywords (mapcar #'(lambda (key)
			       (case (first key) 
				 (:gain
				  `(,(intern (format nil "~A" (second key)) :keyword)
				     (cffi:foreign-slot-value ,sdl-event 'SDL_ActiveEvent 'gain)))
				 (:state
				  `(,(intern (format nil "~A" (second key)) :keyword)
				     (cffi:foreign-slot-value ,sdl-event 'SDL_ActiveEvent 'state)))
				 (:t (error "Unknown keyword ~A" (first key)))))
			   keyword-list))

  `((eql (cffi:foreign-enum-value 'SDL_EventType :SDL_ACTIVEEVENT)
	 (cffi:foreign-slot-value ,sdl-event 'sdl_event 'type))
      (funcall #'(lambda (&key ,@(mapcar #'(lambda (key)
					     (second key))
					 keyword-list))
		   ,@forms)
	       ,@(reduce #'append keywords)))))

(defun expand-keydown (sdl-event params forms)
  (let ((keyword-list nil)
	(keywords nil))
    (do ((keyword params (if (cdr keyword)
			     (cddr keyword)
			     nil)))
	((null keyword))
      (push (list (first keyword) (second keyword)) keyword-list))
    (setf keywords (mapcar #'(lambda (key)
			       (case (first key) 
				 (:state
				  `(,(intern (format nil "~A" (second key)) :keyword)
				     (cffi:foreign-slot-value ,sdl-event 'SDL_KeyboardEvent 'state)))
				 (:scancode
				  `(,(intern (format nil "~A" (second key)) :keyword)
				     (cffi:foreign-slot-value (cffi:foreign-slot-pointer ,sdl-event
											 'sdl_keyboardevent
											 'keysym)
							      'SDL_keysym 'scancode)))
				 (:key
				  `(,(intern (format nil "~A" (second key)) :keyword)
				     (cffi:foreign-slot-value (cffi:foreign-slot-pointer ,sdl-event
											 'sdl_keyboardevent
											 'keysym)
							      'SDL_keysym 'sym)))
				 (:mod `(,(intern (format nil "~A" (second key)) :keyword)
					  (cffi:foreign-slot-value (cffi:foreign-slot-pointer ,sdl-event
											      'sdl_keyboardevent
											      'keysym)
								   'SDL_keysym 'mod)))
				 (:unicode
				  `(,(intern (format nil "~A" (second key)) :keyword)
				     (cffi:foreign-slot-value (cffi:foreign-slot-pointer ,sdl-event
											 'sdl_keyboardevent
											 'keysym)
							      'SDL_keysym 'unicode)))
				 (:t (error "Unknown keyword ~A" (first key)))))
			   keyword-list))
    
    `((eql (cffi:foreign-enum-value 'SDL_EventType :SDL_KEYDOWN)
	   (cffi:foreign-slot-value ,sdl-event 'sdl_event 'type))
      (funcall #'(lambda (&key ,@(mapcar #'(lambda (key)
					     (second key))
					 keyword-list))
		   ,@forms)
	       ,@(reduce #'append keywords)))))

(defun expand-keyup (sdl-event params forms)
  (let ((keyword-list nil)
	(keywords nil))
    (do ((keyword params (if (cdr keyword)
			     (cddr keyword)
			     nil)))
	((null keyword))
      (push (list (first keyword) (second keyword)) keyword-list))
    (setf keywords (mapcar #'(lambda (key)
			       (case (first key) 
				 (:state
				  `(,(intern (format nil "~A" (second key)) :keyword)
				     (cffi:foreign-slot-value ,sdl-event 'SDL_KeyboardEvent 'state)))
				 (:scancode
				  `(,(intern (format nil "~A" (second key)) :keyword)
				     (cffi:foreign-slot-value (cffi:foreign-slot-pointer ,sdl-event
											 'sdl_keyboardevent
											 'keysym)
							      'SDL_keysym 'scancode)))
				 (:key
				  `(,(intern (format nil "~A" (second key)) :keyword)
				     (cffi:foreign-slot-value (cffi:foreign-slot-pointer ,sdl-event
											 'sdl_keyboardevent
											 'keysym)
							      'SDL_keysym 'sym)))
				 (:mod
				  `(,(intern (format nil "~A" (second key)) :keyword)
				     (cffi:foreign-slot-value (cffi:foreign-slot-pointer ,sdl-event
											 'sdl_keyboardevent
											 'keysym)
							      'SDL_keysym 'mod)))
				 (:unicode
				  `(,(intern (format nil "~A" (second key)) :keyword)
				     (cffi:foreign-slot-value (cffi:foreign-slot-pointer ,sdl-event
											 'sdl_keyboardevent
											 'keysym)
							      'SDL_keysym 'unicode)))
				 (:t (error "Unknown keyword ~A" (first key)))))
			   keyword-list))
    
    `((eql (cffi:foreign-enum-value 'SDL_EventType :SDL_KEYUP)
	   (cffi:foreign-slot-value ,sdl-event 'sdl_event 'type))
      (funcall #'(lambda (&key ,@(mapcar #'(lambda (key)
					     (second key))
					 keyword-list))
		   ,@forms)
	       ,@(reduce #'append keywords)))))


(defun expand-mousemotion (sdl-event params forms)
  (let ((keyword-list nil)
	(keywords nil))
    (do ((keyword params (if (cdr keyword)
			     (cddr keyword)
			     nil)))
	((null keyword))
      (push (list (first keyword) (second keyword)) keyword-list))
    (setf keywords (mapcar #'(lambda (key)
			       (case (first key) 
				 (:state
				  `(,(intern (format nil "~A" (second key)) :keyword)
				     (cffi:foreign-slot-value ,sdl-event 'SDL_MouseMotionEvent 'state)))
				 (:x
				  `(,(intern (format nil "~A" (second key)) :keyword)
				     (cffi:foreign-slot-value ,sdl-event 'SDL_MouseMotionEvent 'x))) 
				 (:y
				  `(,(intern (format nil "~A" (second key)) :keyword)
				     (cffi:foreign-slot-value ,sdl-event 'SDL_MouseMotionEvent 'y)))
				 (:x-rel
				  `(,(intern (format nil "~A" (second key)) :keyword)
				     (cffi:foreign-slot-value ,sdl-event 'SDL_MouseMotionEvent 'xrel)))
				 (:y-rel
				  `(,(intern (format nil "~A" (second key)) :keyword)
				     (cffi:foreign-slot-value ,sdl-event 'SDL_MouseMotionEvent 'yrel)))
				 (:t (error "Unknown keyword ~A" (first key)))))
			   keyword-list))

    `((eql (cffi:foreign-enum-value 'SDL_EventType :SDL_MOUSEMOTION)
	   (cffi:foreign-slot-value ,sdl-event 'sdl_event 'type))
      (funcall #'(lambda (&key ,@(mapcar #'(lambda (key)
					     (second key))
					 keyword-list))
		   ,@forms)
	       ,@(reduce #'append keywords)))))

(defun expand-mousebuttondown (sdl-event params forms)
  (let ((keyword-list nil)
	(keywords nil))
    (do ((keyword params (if (cdr keyword)
			     (cddr keyword)
			     nil)))
	((null keyword))
      (push (list (first keyword) (second keyword)) keyword-list))
    (setf keywords (mapcar #'(lambda (key)
			       (case (first key) 
				 (:button
				  `(,(intern (format nil "~A" (second key)) :keyword)
				     (cffi:foreign-slot-value ,sdl-event 'SDL_MouseButtonEvent 'button)))
				 (:state
				  `(,(intern (format nil "~A" (second key)) :keyword)
				     (cffi:foreign-slot-value ,sdl-event 'SDL_MouseButtonEvent 'state)))
				 (:x
				  `(,(intern (format nil "~A" (second key)) :keyword)
				     (cffi:foreign-slot-value ,sdl-event 'SDL_MouseButtonEvent 'x)))
				 (:y
				  `(,(intern (format nil "~A" (second key)) :keyword)
				     (cffi:foreign-slot-value ,sdl-event 'SDL_MouseButtonEvent 'y)))
				 (:t (error "Unknown keyword ~A" (first key)))))
			   keyword-list))

    `((eql (cffi:foreign-enum-value 'SDL_EventType :sdl_mousebuttondown)
	   (cffi:foreign-slot-value ,sdl-event 'sdl_event 'type))
      (funcall #'(lambda (&key ,@(mapcar #'(lambda (key)
					     (second key))
					 keyword-list))
		   ,@forms)
	       ,@(reduce #'append keywords)))))

(defun expand-mousebuttonup (sdl-event params forms)
  (let ((keyword-list nil)
	(keywords nil))
    (do ((keyword params (if (cdr keyword)
			     (cddr keyword)
			     nil)))
	((null keyword))
      (push (list (first keyword) (second keyword)) keyword-list))
    (setf keywords (mapcar #'(lambda (key)
			       (case (first key) 
				 (:button
				  `(,(intern (format nil "~A" (second key)) :keyword)
				     (cffi:foreign-slot-value ,sdl-event 'SDL_MouseButtonEvent 'button)))
				 (:state
				  `(,(intern (format nil "~A" (second key)) :keyword)
				     (cffi:foreign-slot-value ,sdl-event 'SDL_MouseButtonEvent 'state)))
				 (:x
				  `(,(intern (format nil "~A" (second key)) :keyword)
				     (cffi:foreign-slot-value ,sdl-event 'SDL_MouseButtonEvent 'x)))
				 (:y
				  `(,(intern (format nil "~A" (second key)) :keyword)
				     (cffi:foreign-slot-value ,sdl-event 'SDL_MouseButtonEvent 'y)))
				 (:t (error "Unknown keyword ~A" (first key)))))
			   keyword-list))

    `((eql (cffi:foreign-enum-value 'SDL_EventType :sdl_mousebuttonup)
	   (cffi:foreign-slot-value ,sdl-event 'sdl_event 'type))
      (funcall #'(lambda (&key ,@(mapcar #'(lambda (key)
					     (second key))
					 keyword-list))
		   ,@forms)
	       ,@(reduce #'append keywords)))))

(defun expand-joyaxismotion (sdl-event params forms)
  (let ((keyword-list nil)
	(keywords nil))
    (do ((keyword params (if (cdr keyword)
			     (cddr keyword)
			     nil)))
	((null keyword))
      (push (list (first keyword) (second keyword)) keyword-list))
    (setf keywords (mapcar #'(lambda (key)
			       (case (first key) 
				 (:which
				  `(,(intern (format nil "~A" (second key)) :keyword)
				     (cffi:foreign-slot-value ,sdl-event 'SDL_JoyAxisEvent 'which)))
				 (:axis
				  `(,(intern (format nil "~A" (second key)) :keyword)
				     (cffi:foreign-slot-value ,sdl-event 'SDL_JoyAxisEvent 'axis)))
				 (:value
				  `(,(intern (format nil "~A" (second key)) :keyword)
				     (cffi:foreign-slot-value ,sdl-event 'SDL_JoyAxisEvent 'value)))
				 (:t (error "Unknown keyword ~A" (first key)))))
			   keyword-list))


  `((eql (cffi:foreign-enum-value 'SDL_EventType :SDL_JOYAXISMOTION)
	 (cffi:foreign-slot-value ,sdl-event 'sdl_event 'type))
      (funcall #'(lambda (&key ,@(mapcar #'(lambda (key)
					     (second key))
					 keyword-list))
		   ,@forms)
	       ,@(reduce #'append keywords)))))

(defun expand-joybuttondown (sdl-event params forms)
  (let ((keyword-list nil)
	(keywords nil))
    (do ((keyword params (if (cdr keyword)
			     (cddr keyword)
			     nil)))
	((null keyword))
      (push (list (first keyword) (second keyword)) keyword-list))
    (setf keywords (mapcar #'(lambda (key)
			       (case (first key) 
				 (:which
				  `(,(intern (format nil "~A" (second key)) :keyword)
				     (cffi:foreign-slot-value ,sdl-event 'SDL_JoyButtonEvent 'which)))
				 (:axis
				  `(,(intern (format nil "~A" (second key)) :keyword)
				     (cffi:foreign-slot-value ,sdl-event 'SDL_JoyButtonEvent 'axis)))
				 (:value
				  `(,(intern (format nil "~A" (second key)) :keyword)
				     (cffi:foreign-slot-value ,sdl-event 'SDL_JoyButtonEvent 'value)))
				 (:t (error "Unknown keyword ~A" (first key)))))
			   keyword-list))

    `((eql (cffi:foreign-enum-value 'SDL_EventType :SDL_JOYBUTTONDOWN)
	   (cffi:foreign-slot-value ,sdl-event 'sdl_event 'type))
      (funcall #'(lambda (&key ,@(mapcar #'(lambda (key)
					     (second key))
					 keyword-list))
		   ,@forms)
	       ,@(reduce #'append keywords)))))

(defun expand-joybuttonup (sdl-event params forms)
  (let ((keyword-list nil)
	(keywords nil))
    (do ((keyword params (if (cdr keyword)
			     (cddr keyword)
			     nil)))
	((null keyword))
      (push (list (first keyword) (second keyword)) keyword-list))
    (setf keywords (mapcar #'(lambda (key)
			       (case (first key) 
				 (:which
				  `(,(intern (format nil "~A" (second key)) :keyword)
				     (cffi:foreign-slot-value ,sdl-event 'SDL_JoyButtonEvent 'which)))
				 (:axis
				  `(,(intern (format nil "~A" (second key)) :keyword)
				     (cffi:foreign-slot-value ,sdl-event 'SDL_JoyButtonEvent 'axis)))
				 (:value
				  `(,(intern (format nil "~A" (second key)) :keyword)
				     (cffi:foreign-slot-value ,sdl-event 'SDL_JoyButtonEvent 'value)))
				 (:t (error "Unknown keyword ~A" (first key)))))
			   keyword-list))

    `((eql (cffi:foreign-enum-value 'SDL_EventType :SDL_JOYBUTTONUP)
	   (cffi:foreign-slot-value ,sdl-event 'sdl_event 'type))
      (funcall #'(lambda (&key ,@(mapcar #'(lambda (key)
					     (second key))
					 keyword-list))
		   ,@forms)
	       ,@(reduce #'append keywords)))))

(defun expand-joyhatmotion (sdl-event params forms)
  (let ((keyword-list nil)
	(keywords nil))
    (do ((keyword params (if (cdr keyword)
			     (cddr keyword)
			     nil)))
	((null keyword))
      (push (list (first keyword) (second keyword)) keyword-list))
    (setf keywords (mapcar #'(lambda (key)
			       (case (first key) 
				 (:which
				  `(,(intern (format nil "~A" (second key)) :keyword)
				     (cffi:foreign-slot-value ,sdl-event 'SDL_JoyHatEvent 'which)))
				 (:axis
				  `(,(intern (format nil "~A" (second key)) :keyword)
				     (cffi:foreign-slot-value ,sdl-event 'SDL_JoyHatEvent 'axis)))
				 (:value
				  `(,(intern (format nil "~A" (second key)) :keyword)
				     (cffi:foreign-slot-value ,sdl-event 'SDL_JoyHatEvent 'value)))
				 (:t (error "Unknown keyword ~A" (first key)))))
			   keyword-list))

    `((eql (cffi:foreign-enum-value 'SDL_EventType :SDL_JOYHATMOTION)
	   (cffi:foreign-slot-value ,sdl-event 'sdl_event 'type))
      (funcall #'(lambda (&key ,@(mapcar #'(lambda (key)
					     (second key))
					 keyword-list))
		   ,@forms)
	       ,@(reduce #'append keywords)))))

(defun expand-joyballmotion (sdl-event params forms)
  (let ((keyword-list nil)
	(keywords nil))
    (do ((keyword params (if (cdr keyword)
			     (cddr keyword)
			     nil)))
	((null keyword))
      (push (list (first keyword) (second keyword)) keyword-list))
    (setf keywords (mapcar #'(lambda (key)
			       (case (first key) 
				 (:which
				  `(,(intern (format nil "~A" (second key)) :keyword)
				     (cffi:foreign-slot-value ,sdl-event 'SDL_JoyBallEvent 'which)))
				 (:ball
				  `(,(intern (format nil "~A" (second key)) :keyword)
				     (cffi:foreign-slot-value ,sdl-event 'SDL_JoyBallEvent 'ball)))
				 (:x-rel
				  `(,(intern (format nil "~A" (second key)) :keyword)
				     (cffi:foreign-slot-value ,sdl-event 'SDL_JoyBallEvent 'xrel)))
				 (:y-rel
				  `(,(intern (format nil "~A" (second key)) :keyword)
				     (cffi:foreign-slot-value ,sdl-event 'SDL_JoyBallEvent 'yrel)))
				 (:t (error "Unknown keyword ~A" (first key)))))
			   keyword-list))

    `((eql (cffi:foreign-enum-value 'SDL_EventType :SDL_JOYBALLMOTION)
	   (cffi:foreign-slot-value ,sdl-event 'sdl_event 'type))
      (funcall #'(lambda (&key ,@(mapcar #'(lambda (key)
					     (second key))
					 keyword-list))
		   ,@forms)
	       ,@(reduce #'append keywords)))))

(defun expand-videoresize (sdl-event params forms)
  (let ((keyword-list nil)
	(keywords nil))
    (do ((keyword params (if (cdr keyword)
			     (cddr keyword)
			     nil)))
	((null keyword))
      (push (list (first keyword) (second keyword)) keyword-list))
    (setf keywords (mapcar #'(lambda (key)
			       (case (first key) 
				 (:w
				  `(,(intern (format nil "~A" (second key)) :keyword)
				     (cffi:foreign-slot-value ,sdl-event 'SDL_ResizeEvent 'w)))
				 (:h
				  `(,(intern (format nil "~A" (second key)) :keyword)
				     (cffi:foreign-slot-value ,sdl-event 'SDL_ResizeEvent 'h)))
				 (:t (error "Unknown keyword ~A" (first key)))))
			   keyword-list))

    `((eql (cffi:foreign-enum-value 'SDL_EventType :SDL_VIDEORESIZE)
	   (cffi:foreign-slot-value ,sdl-event 'sdl_event 'type))
      (funcall #'(lambda (&key ,@(mapcar #'(lambda (key)
					     (second key))
					 keyword-list))
		   ,@forms)
	       ,@(reduce #'append keywords)))))

(defun expand-videoexpose (sdl-event forms)
  `((eql (cffi:foreign-enum-value 'SDL_EventType :SDL_VIDEOEXPOSE)
	 (cffi:foreign-slot-value ,sdl-event 'sdl_event 'type))
    (funcall #'(lambda ()
                 ,@forms))))

(defun expand-syswmevent (sdl-event forms)
  `((eql (cffi:foreign-enum-value 'SDL_EventType :SDL_SYSWMEVENT)
	 (cffi:foreign-slot-value ,sdl-event 'sdl_event 'type))
    (funcall #'(lambda ()
                 ,@forms))))

(defun expand-quit (sdl-event forms quit)
  `((eql (cffi:foreign-enum-value 'SDL_EventType :SDL_QUIT)
	 (cffi:foreign-slot-value ,sdl-event 'sdl_event 'type))
    (setf ,quit (funcall #'(lambda ()
                             ,@forms)))))

(defun expand-userevent (sdl-event params forms)
  (let ((keyword-list nil)
	(keywords nil))
    (do ((keyword params (if (cdr keyword)
			     (cddr keyword)
			     nil)))
	((null keyword))
      (push (list (first keyword) (second keyword)) keyword-list))
    (setf keywords (mapcar #'(lambda (key)
			       (case (first key) 
				 (:type
				  `(,(intern (format nil "~A" (second key)) :keyword)
				     (cffi:foreign-slot-value ,sdl-event 'SDL_UserEvent 'type)))
				 (:code
				  `(,(intern (format nil "~A" (second key)) :keyword)
				     (cffi:foreign-slot-value ,sdl-event 'SDL_UserEvent 'code)))
				 (:data1
				  `(,(intern (format nil "~A" (second key)) :keyword)
				     (cffi:foreign-slot-pointer ,sdl-event 'SDL_UserEvent 'data1)))
				 (:data2
				  `(,(intern (format nil "~A" (second key)) :keyword)
				     (cffi:foreign-slot-pointer ,sdl-event 'SDL_UserEvent 'data2)))
				 (:t (error "Unknown keyword ~A" (first key)))))
			   keyword-list))

    `((and (>= (cffi:foreign-slot-value ,sdl-event 'sdl_event 'type)
	       (cffi:foreign-enum-value 'SDL_EventType :SDL_USEREVENT))
	   (< (cffi:foreign-slot-value ,sdl-event 'sdl_event 'type)
	      (- (cffi:foreign-enum-value 'SDL_EventType :SDL_NUMEVENTS) 1)))
      (funcall #'(lambda (&key ,@(mapcar #'(lambda (key)
					     (second key))
					 keyword-list))
		   ,@forms)
	       ,@(reduce #'append keywords)))))

(defun expand-idle (forms)
  `(progn
     ,@forms))

(defmacro with-events (args &rest events)
  "(with-sdl-events
     (:activeevent (:gain gain :state state)
		     t)
     (:keydown (:state state :scancode scancode :key key :mod mod :unicode unicode)
	       t)
     (:keyup (:state state :scancode scancode :key key :mod mod :unicode unicode)
	     t)
     (:mousemotion (:state state :x x :y y :xrel xrel :yrel yrel)
		   t)
     (:mousebuttondown (:button button :state state :x x :y y)
		       t)
     (:mousebuttonup (:button button :state state :x x :y y)
		     t)
     (:joyaxismotion (:which which :axis axis :value value)
		     t)
     (:joybuttondown (:which which :button button :state state)
		     t)
     (:joybuttonup (:which which :button button :state state)
		   t)
     (:joyhatmotion (:which which :hat hat :value value)
		    t)
     (:joyballmotion (:which which :ball ball :xrel xrel :yrel yrel)
		     t)
     (:videoresize (:w w :h h)
		   t)
     (:videoexpose ()
      t)
     (:syswmevent ()
      t)
     (:quit ()
      t)
     (:idle ()
      &body))
   NOTE: (:quit t) is mandatory if you ever want to exit your application."
  (declare (ignore args))
  (let ((quit (gensym "quit")) (sdl-event (gensym "sdl-event")) (poll-event (gensym "poll-event")) 
        (previous-ticks (gensym "previous-ticks")) (current-ticks (gensym "current-ticks")))
    `(let ((,sdl-event (new-event))
           (,quit nil)
           (,previous-ticks nil)
           (,current-ticks nil))
      ;(init-framerate-manager)
      (do ()
	  ((eql ,quit t))
	(do ((,poll-event (SDL_PollEvent ,sdl-event) (SDL_PollEvent ,sdl-event)))
	    ((eql ,poll-event 0) nil)
	  (cond
            ,@(remove nil 
                      (mapcar #'(lambda (event)
                                  (case (first event)
                                    (:activeevent
                                     (expand-activeevent sdl-event 
                                                         (first (rest event)) 
							 (rest (rest event))))
				    (:keydown
				     (expand-keydown sdl-event 
						     (first (rest event)) 
						     (rest (rest event))))
				    (:keyup
				     (expand-keyup sdl-event 
						   (first (rest event)) 
						   (rest (rest event))))
				    (:mousemotion
				     (expand-mousemotion sdl-event 
							 (first (rest event)) 
							 (rest (rest event))))
				    (:mousebuttondown
				     (expand-mousebuttondown sdl-event
							     (first (rest event)) 
							     (rest (rest event))))
				    (:mousebuttonup
				     (expand-mousebuttonup sdl-event 
							   (first (rest event)) 
							   (rest (rest event))))
				    (:joyaxismotion
				     (expand-joyaxismotion sdl-event 
							   (first (rest event)) 
							   (rest (rest event))))
				    (:joybuttondown
				     (expand-joybuttondown sdl-event 
							   (first (rest event)) 
							   (rest (rest event))))
				    (:joybuttonup
				     (expand-joybuttonup sdl-event 
							 (first (rest event)) 
							 (rest (rest event))))
				    (:joyhatmotion
				     (expand-joyhatmotion sdl-event 
							  (first (rest event)) 
							  (rest (rest event))))
				    (:joyballmotion
				     (expand-joyballmotion sdl-event 
							   (first (rest event)) 
							   (rest (rest event))))
				    (:videoresize
				     (expand-videoresize sdl-event 
							 (first (rest event)) 
							 (rest (rest event))))
				    (:videoexpose
				     (expand-videoexpose sdl-event 
							 (rest (rest event))))
				    (:syswmevent
				     (expand-syswmevent sdl-event 
							(rest (rest event))))
				    (:quit
				     (expand-quit sdl-event 
						  (rest (rest event)) 
						  quit))
				    (:userevent
				     (expand-userevent sdl-event 
						       (first (rest event)) 
						       (rest (rest event))))))
                              events))))
	(if (null ,previous-ticks)
	    (setf ,previous-ticks (SDL_GetTicks))
	    (setf ,previous-ticks ,current-ticks))
	(setf ,current-ticks (SDL_GetTicks))
	(set-timescale (/ 
			(set-ticks (- ,current-ticks ,previous-ticks)) 
			(get-worldtime)))
	,@(remove nil 
		  (mapcar #'(lambda (event)
			      (cond
				((eql :idle (first event))
				 (expand-idle (rest event)))))
			  events))
	(progn
	  (framerate-delay)))
      (cffi:foreign-free ,sdl-event))))
