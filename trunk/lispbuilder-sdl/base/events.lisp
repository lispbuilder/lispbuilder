;; SDL (Simple Media Layer) library using CFFI for foreign function interfacing...
;; (C)2006 Luke Crook <luke@balooga.com>
;; Thanks to Frank Buss and Surendra Singh
;; see COPYING for license
;; This file contains some useful functions for using SDL from Common lisp
;; using sdl.lisp (the CFFI wrapper)

(in-package #:lispbuilder-sdl-base)


(defun new-event (&optional (event-type :SDL-NO-EVENT))
  "Creates a new SDL_Event and sets the type to :event-type.
   If no type is specified, then an SDL_Event of type SDL_NOEVENT is returned.
   For example, to create a quit event use :event-type 'SDL_QuitEvent."
  (unless (cffi:foreign-enum-value 'sdl-cffi::SDL-Event-Type event-type :errorp nil)
    (error "new-event: event-type ~A is not a valid SDL event." event-type))
  (let ((event (cffi:foreign-alloc 'sdl-cffi::sdl-event)))
    (setf (cffi:foreign-slot-value event 'sdl-cffi::SDL-event 'type) (cffi:foreign-enum-value 'sdl-cffi::SDL-Event-Type event-type))
    event))

(defun push-quit-event ()
  "Pushes a new SDL_Event of type SDL_QUIT onto the event queue."
  (sdl-cffi::SDL-Push-Event (new-event :sdl-quit-event)))

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
      (setf current-ticks (sdl-cffi::sdl-get-ticks))
      (incf (fpsmanager-framecount fpsmngr))
      (setf target-ticks (+ (fpsmanager-lastticks fpsmngr) 
			    (* (fpsmanager-framecount fpsmngr) (fpsmanager-rateticks fpsmngr))))
      (if (<= current-ticks target-ticks)
	  (sdl-cffi::sdl-delay (round (- target-ticks current-ticks)))
	  (progn
	    (setf (fpsmanager-framecount fpsmngr) 0)
	    (setf (fpsmanager-lastticks fpsmngr) (sdl-cffi::sdl-get-ticks)))))))

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
				     (cffi:foreign-slot-value ,sdl-event 'sdl-cffi::Sdl-Active-Event 'sdl-cffi::gain)))
				 (:state
				  `(,(intern (format nil "~A" (second key)) :keyword)
				     (cffi:foreign-slot-value ,sdl-event 'sdl-cffi::Sdl-Active-Event 'sdl-cffi::state)))
				 (:t (error "Unknown keyword ~A" (first key)))))
			   keyword-list))

  `((eql (cffi:foreign-enum-value 'sdl-cffi::Sdl-Event-Type :SDL-ACTIVE-EVENT)
	 (cffi:foreign-slot-value ,sdl-event 'sdl-cffi::sdl-event 'sdl-cffi::type))
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
				     (cffi:foreign-slot-value ,sdl-event 'sdl-cffi::Sdl-Keyboard-Event 'sdl-cffi::state)))
				 (:scancode
				  `(,(intern (format nil "~A" (second key)) :keyword)
				     (cffi:foreign-slot-value (cffi:foreign-slot-pointer ,sdl-event
											 'sdl-cffi::sdl-keyboard-event
											 'sdl-cffi::keysym)
							      'sdl-cffi::sdl-key-sym 'sdl-cffi::scancode)))
				 (:key
				  `(,(intern (format nil "~A" (second key)) :keyword)
				     (cffi:foreign-slot-value (cffi:foreign-slot-pointer ,sdl-event
											 'sdl-cffi::sdl-keyboard-event
											 'sdl-cffi::keysym)
							      'sdl-cffi::sdl-key-sym 'sdl-cffi::sym)))
				 (:mod `(,(intern (format nil "~A" (second key)) :keyword)
					  (cffi:foreign-slot-value (cffi:foreign-slot-pointer ,sdl-event
											      'sdl-cffi::sdl-keyboard-event
											      'sdl-cffi::keysym)
								   'sdl-cffi::sdl-key-sym 'sdl-cffi::mod)))
				 (:unicode
				  `(,(intern (format nil "~A" (second key)) :keyword)
				     (cffi:foreign-slot-value (cffi:foreign-slot-pointer ,sdl-event
											 'sdl-cffi::sdl-keyboard-event
											 'sdl-cffi::keysym)
							      'sdl-cffi::sdl-key-sym 'sdl-cffi::unicode)))
				 (:t (error "Unknown keyword ~A" (first key)))))
			   keyword-list))
    
    `((eql (cffi:foreign-enum-value 'sdl-cffi::Sdl-Event-Type :SDL-KEY-DOWN-EVENT)
	   (cffi:foreign-slot-value ,sdl-event 'sdl-cffi::sdl-event 'sdl-cffi::type))
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
				     (cffi:foreign-slot-value ,sdl-event 'sdl-cffi::Sdl-Keyboard-Event 'sdl-cffi::state)))
				 (:scancode
				  `(,(intern (format nil "~A" (second key)) :keyword)
				     (cffi:foreign-slot-value (cffi:foreign-slot-pointer ,sdl-event
											 'sdl-cffi::sdl-keyboard-event
											 'sdl-cffi::keysym)
							      'sdl-cffi::sdl-key-sym 'sdl-cffi::scancode)))
				 (:key
				  `(,(intern (format nil "~A" (second key)) :keyword)
				     (cffi:foreign-slot-value (cffi:foreign-slot-pointer ,sdl-event
											 'sdl-cffi::sdl-keyboard-event
											 'sdl-cffi::keysym)
							      'sdl-cffi::sdl-key-sym 'sdl-cffi::sym)))
				 (:mod
				  `(,(intern (format nil "~A" (second key)) :keyword)
				     (cffi:foreign-slot-value (cffi:foreign-slot-pointer ,sdl-event
											 'sdl-cffi::sdl-keyboard-event
											 'sdl-cffi::keysym)
							      'sdl-cffi::sdl-key-sym 'sdl-cffi::mod)))
				 (:unicode
				  `(,(intern (format nil "~A" (second key)) :keyword)
				     (cffi:foreign-slot-value (cffi:foreign-slot-pointer ,sdl-event
											 'sdl-cffi::sdl-keyboard-event
											 'sdl-cffi::keysym)
							      'sdl-cffi::sdl-key-sym 'sdl-cffi::unicode)))
				 (:t (error "Unknown keyword ~A" (first key)))))
			   keyword-list))
    
    `((eql (cffi:foreign-enum-value 'sdl-cffi::Sdl-Event-Type :SDL-KEY-UP-EVENT)
	   (cffi:foreign-slot-value ,sdl-event 'sdl-cffi::sdl-event 'sdl-cffi::type))
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
				     (cffi:foreign-slot-value ,sdl-event 'sdl-cffi::SDL_MouseMotionEvent 'sdl-cffi::state)))
				 (:x
				  `(,(intern (format nil "~A" (second key)) :keyword)
				     (cffi:foreign-slot-value ,sdl-event 'sdl-cffi::SDL_MouseMotionEvent 'sdl-cffi::x))) 
				 (:y
				  `(,(intern (format nil "~A" (second key)) :keyword)
				     (cffi:foreign-slot-value ,sdl-event 'sdl-cffi::SDL_MouseMotionEvent 'sdl-cffi::y)))
				 (:x-rel
				  `(,(intern (format nil "~A" (second key)) :keyword)
				     (cffi:foreign-slot-value ,sdl-event 'sdl-cffi::SDL_MouseMotionEvent 'sdl-cffi::xrel)))
				 (:y-rel
				  `(,(intern (format nil "~A" (second key)) :keyword)
				     (cffi:foreign-slot-value ,sdl-event 'sdl-cffi::SDL_MouseMotionEvent 'sdl-cffi::yrel)))
				 (:t (error "Unknown keyword ~A" (first key)))))
			   keyword-list))

    `((eql (cffi:foreign-enum-value 'sdl-cffi::Sdl-Event-Type :SDL-MOUSE-MOTION-EVENT)
	   (cffi:foreign-slot-value ,sdl-event 'sdl-cffi::sdl-event 'sdl-cffi::type))
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
				     (cffi:foreign-slot-value ,sdl-event 'sdl-cffi::SDL_MouseButtonEvent 'sdl-cffi::button)))
				 (:state
				  `(,(intern (format nil "~A" (second key)) :keyword)
				     (cffi:foreign-slot-value ,sdl-event 'sdl-cffi::SDL_MouseButtonEvent 'sdl-cffi::state)))
				 (:x
				  `(,(intern (format nil "~A" (second key)) :keyword)
				     (cffi:foreign-slot-value ,sdl-event 'sdl-cffi::SDL_MouseButtonEvent 'sdl-cffi::x)))
				 (:y
				  `(,(intern (format nil "~A" (second key)) :keyword)
				     (cffi:foreign-slot-value ,sdl-event 'sdl-cffi::SDL_MouseButtonEvent 'sdl-cffi::y)))
				 (:t (error "Unknown keyword ~A" (first key)))))
			   keyword-list))

    `((eql (cffi:foreign-enum-value 'sdl-cffi::Sdl-Event-Type :SDL-MOUSE-BUTTON-DOWN-EVENT)
	   (cffi:foreign-slot-value ,sdl-event 'sdl-cffi::sdl-event 'sdl-cffi::type))
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
				     (cffi:foreign-slot-value ,sdl-event 'sdl-cffi::SDL_MouseButtonEvent 'sdl-cffi::button)))
				 (:state
				  `(,(intern (format nil "~A" (second key)) :keyword)
				     (cffi:foreign-slot-value ,sdl-event 'sdl-cffi::SDL_MouseButtonEvent 'sdl-cffi::state)))
				 (:x
				  `(,(intern (format nil "~A" (second key)) :keyword)
				     (cffi:foreign-slot-value ,sdl-event 'sdl-cffi::SDL_MouseButtonEvent 'sdl-cffi::x)))
				 (:y
				  `(,(intern (format nil "~A" (second key)) :keyword)
				     (cffi:foreign-slot-value ,sdl-event 'sdl-cffi::SDL_MouseButtonEvent 'sdl-cffi::y)))
				 (:t (error "Unknown keyword ~A" (first key)))))
			   keyword-list))

    `((eql (cffi:foreign-enum-value 'sdl-cffi::Sdl-Event-Type :SDL-MOUSE-BUTTON-UP-EVENT)
	   (cffi:foreign-slot-value ,sdl-event 'sdl-cffi::sdl-event 'sdl-cffi::type))
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
				     (cffi:foreign-slot-value ,sdl-event 'sdl-cffi::SDL_JoyAxisEvent 'sdl-cffi::which)))
				 (:axis
				  `(,(intern (format nil "~A" (second key)) :keyword)
				     (cffi:foreign-slot-value ,sdl-event 'sdl-cffi::SDL_JoyAxisEvent 'sdl-cffi::axis)))
				 (:value
				  `(,(intern (format nil "~A" (second key)) :keyword)
				     (cffi:foreign-slot-value ,sdl-event 'sdl-cffi::SDL_JoyAxisEvent 'sdl-cffi::value)))
				 (:t (error "Unknown keyword ~A" (first key)))))
			   keyword-list))


  `((eql (cffi:foreign-enum-value 'sdl-cffi::Sdl-Event-Type :SDL-JOY-AXIS-MOTION-EVENT)
	 (cffi:foreign-slot-value ,sdl-event 'sdl-cffi::sdl-event 'sdl-cffi::type))
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
				     (cffi:foreign-slot-value ,sdl-event 'sdl-cffi::SDL_JoyButtonEvent 'sdl-cffi::which)))
				 (:axis
				  `(,(intern (format nil "~A" (second key)) :keyword)
				     (cffi:foreign-slot-value ,sdl-event 'sdl-cffi::SDL_JoyButtonEvent 'sdl-cffi::axis)))
				 (:value
				  `(,(intern (format nil "~A" (second key)) :keyword)
				     (cffi:foreign-slot-value ,sdl-event 'sdl-cffi::SDL_JoyButtonEvent 'sdl-cffi::value)))
				 (:t (error "Unknown keyword ~A" (first key)))))
			   keyword-list))

    `((eql (cffi:foreign-enum-value 'sdl-cffi::Sdl-Event-Type :SDL-JOY-BUTTON-DOWN-EVENT)
	   (cffi:foreign-slot-value ,sdl-event 'sdl-cffi::sdl-event 'sdl-cffi::type))
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
				     (cffi:foreign-slot-value ,sdl-event 'sdl-cffi::SDL_JoyButtonEvent 'sdl-cffi::which)))
				 (:axis
				  `(,(intern (format nil "~A" (second key)) :keyword)
				     (cffi:foreign-slot-value ,sdl-event 'sdl-cffi::SDL_JoyButtonEvent 'sdl-cffi::axis)))
				 (:value
				  `(,(intern (format nil "~A" (second key)) :keyword)
				     (cffi:foreign-slot-value ,sdl-event 'sdl-cffi::SDL_JoyButtonEvent 'sdl-cffi::value)))
				 (:t (error "Unknown keyword ~A" (first key)))))
			   keyword-list))

    `((eql (cffi:foreign-enum-value 'sdl-cffi::Sdl-Event-Type :SDL-JOY-BUTTON-UP-EVENT)
	   (cffi:foreign-slot-value ,sdl-event 'sdl-cffi::sdl-event 'sdl-cffi::type))
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
				     (cffi:foreign-slot-value ,sdl-event 'sdl-cffi::SDL_JoyHatEvent 'sdl-cffi::which)))
				 (:axis
				  `(,(intern (format nil "~A" (second key)) :keyword)
				     (cffi:foreign-slot-value ,sdl-event 'sdl-cffi::SDL_JoyHatEvent 'sdl-cffi::axis)))
				 (:value
				  `(,(intern (format nil "~A" (second key)) :keyword)
				     (cffi:foreign-slot-value ,sdl-event 'sdl-cffi::SDL_JoyHatEvent 'sdl-cffi::value)))
				 (:t (error "Unknown keyword ~A" (first key)))))
			   keyword-list))

    `((eql (cffi:foreign-enum-value 'sdl-cffi::Sdl-Event-Type :SDL-JOY-HAT-MOTION-EVENT)
	   (cffi:foreign-slot-value ,sdl-event 'sdl-cffi::sdl-event 'sdl-cffi::type))
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
				     (cffi:foreign-slot-value ,sdl-event 'sdl-cffi::SDL_JoyBallEvent 'sdl-cffi::which)))
				 (:ball
				  `(,(intern (format nil "~A" (second key)) :keyword)
				     (cffi:foreign-slot-value ,sdl-event 'sdl-cffi::SDL_JoyBallEvent 'sdl-cffi::ball)))
				 (:x-rel
				  `(,(intern (format nil "~A" (second key)) :keyword)
				     (cffi:foreign-slot-value ,sdl-event 'sdl-cffi::SDL_JoyBallEvent 'sdl-cffi::xrel)))
				 (:y-rel
				  `(,(intern (format nil "~A" (second key)) :keyword)
				     (cffi:foreign-slot-value ,sdl-event 'sdl-cffi::SDL_JoyBallEvent 'sdl-cffi::yrel)))
				 (:t (error "Unknown keyword ~A" (first key)))))
			   keyword-list))

    `((eql (cffi:foreign-enum-value 'sdl-cffi::Sdl-Event-Type :SDL-JOY-BALL-MOTION-EVENT)
	   (cffi:foreign-slot-value ,sdl-event 'sdl-cffi::sdl-event 'sdl-cffi::type))
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
				     (cffi:foreign-slot-value ,sdl-event 'sdl-cffi::SDL_ResizeEvent 'sdl-cffi::w)))
				 (:h
				  `(,(intern (format nil "~A" (second key)) :keyword)
				     (cffi:foreign-slot-value ,sdl-event 'sdl-cffi::SDL_ResizeEvent 'sdl-cffi::h)))
				 (:t (error "Unknown keyword ~A" (first key)))))
			   keyword-list))

    `((eql (cffi:foreign-enum-value 'sdl-cffi::Sdl-Event-Type :SDL-VIDEO-RESIZE-EVENT)
	   (cffi:foreign-slot-value ,sdl-event 'sdl-cffi::sdl-event 'sdl-cffi::type))
      (funcall #'(lambda (&key ,@(mapcar #'(lambda (key)
					     (second key))
					 keyword-list))
		   ,@forms)
	       ,@(reduce #'append keywords)))))

(defun expand-videoexpose (sdl-event forms)
  `((eql (cffi:foreign-enum-value 'sdl-cffi::Sdl-Event-Type :SDL-VIDEO-EXPOSE-EVENT)
	 (cffi:foreign-slot-value ,sdl-event 'sdl-cffi::sdl-event 'sdl-cffi::type))
    (funcall #'(lambda ()
                 ,@forms))))

(defun expand-syswmevent (sdl-event forms)
  `((eql (cffi:foreign-enum-value 'sdl-cffi::Sdl-Event-Type :SDL-SYS-WM-EVENT)
	 (cffi:foreign-slot-value ,sdl-event 'sdl-cffi::sdl-event 'sdl-cffi::type))
    (funcall #'(lambda ()
                 ,@forms))))

(defun expand-quit (sdl-event forms quit)
  `((eql (cffi:foreign-enum-value 'sdl-cffi::Sdl-Event-Type :SDL-QUIT-EVENT)
	 (cffi:foreign-slot-value ,sdl-event 'sdl-cffi::sdl-event 'sdl-cffi::type))
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
				     (cffi:foreign-slot-value ,sdl-event 'sdl-cffi::SDL_UserEvent 'sdl-cffi::type)))
				 (:code
				  `(,(intern (format nil "~A" (second key)) :keyword)
				     (cffi:foreign-slot-value ,sdl-event 'sdl-cffi::SDL_UserEvent 'sdl-cffi::code)))
				 (:data1
				  `(,(intern (format nil "~A" (second key)) :keyword)
				     (cffi:foreign-slot-pointer ,sdl-event 'sdl-cffi::SDL_UserEvent 'sdl-cffi::data1)))
				 (:data2
				  `(,(intern (format nil "~A" (second key)) :keyword)
				     (cffi:foreign-slot-pointer ,sdl-event 'sdl-cffi::SDL_UserEvent 'sdl-cffi::data2)))
				 (:t (error "Unknown keyword ~A" (first key)))))
			   keyword-list))

    `((and (>= (cffi:foreign-slot-value ,sdl-event 'sdl-cffi::sdl-event 'sdl-cffi::type)
	       (cffi:foreign-enum-value 'sdl-cffi::Sdl-Event-Type :SDL-USER-EVENT))
	   (< (cffi:foreign-slot-value ,sdl-event 'sdl-cffi::sdl-event 'sdl-cffi::type)
	      (- (cffi:foreign-enum-value 'sdl-cffi::Sdl-Event-Type :SDL-NUM-EVENTS) 1)))
      (funcall #'(lambda (&key ,@(mapcar #'(lambda (key)
					     (second key))
					 keyword-list))
		   ,@forms)
	       ,@(reduce #'append keywords)))))

(defun expand-idle (forms)
  `(progn
     ,@forms))

(defmacro with-events (args &rest events)
  "(with-events
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
	(do ((,poll-event (sdl-cffi::SDL-Poll-Event ,sdl-event) (sdl-cffi::SDL-Poll-Event ,sdl-event)))
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
	    (setf ,previous-ticks (sdl-cffi::SDL-Get-Ticks))
	    (setf ,previous-ticks ,current-ticks))
	(setf ,current-ticks (sdl-cffi::SDL-Get-Ticks))
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
