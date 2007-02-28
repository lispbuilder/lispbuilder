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
  (defun set-frame-rate (rate)
    (if (> rate 0)
        (if (and (>= rate fps-lower-limit) (<= rate fps-upper-limit))
            (progn
              (setf (fpsmanager-framecount fpsmngr) 0)
              (setf (fpsmanager-rate fpsmngr) rate)
              (setf (fpsmanager-rateticks fpsmngr) (/ 1000.0 rate))
              t)
	    nil)
	(setf (fpsmanager-rate fpsmngr) rate)))
  (defun frame-rate ()
    (fpsmanager-rate fpsmngr))
  (defsetf frame-rate set-frame-rate)
  
  (defun frame-rate-delay ()
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
				     (cffi:foreign-slot-value ,sdl-event 'sdl-cffi::SDL-Mouse-Motion-Event 'sdl-cffi::state)))
				 (:x
				  `(,(intern (format nil "~A" (second key)) :keyword)
				     (cffi:foreign-slot-value ,sdl-event 'sdl-cffi::SDL-Mouse-Motion-Event 'sdl-cffi::x))) 
				 (:y
				  `(,(intern (format nil "~A" (second key)) :keyword)
				     (cffi:foreign-slot-value ,sdl-event 'sdl-cffi::SDL-Mouse-Motion-Event 'sdl-cffi::y)))
				 (:x-rel
				  `(,(intern (format nil "~A" (second key)) :keyword)
				     (cffi:foreign-slot-value ,sdl-event 'sdl-cffi::SDL-Mouse-Motion-Event 'sdl-cffi::xrel)))
				 (:y-rel
				  `(,(intern (format nil "~A" (second key)) :keyword)
				     (cffi:foreign-slot-value ,sdl-event 'sdl-cffi::SDL-Mouse-Motion-Event 'sdl-cffi::yrel)))
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
				     (cffi:foreign-slot-value ,sdl-event 'sdl-cffi::Sdl-Mouse-Button-Event 'sdl-cffi::button)))
				 (:state
				  `(,(intern (format nil "~A" (second key)) :keyword)
				     (cffi:foreign-slot-value ,sdl-event 'sdl-cffi::Sdl-Mouse-Button-Event 'sdl-cffi::state)))
				 (:x
				  `(,(intern (format nil "~A" (second key)) :keyword)
				     (cffi:foreign-slot-value ,sdl-event 'sdl-cffi::Sdl-Mouse-Button-Event 'sdl-cffi::x)))
				 (:y
				  `(,(intern (format nil "~A" (second key)) :keyword)
				     (cffi:foreign-slot-value ,sdl-event 'sdl-cffi::Sdl-Mouse-Button-Event 'sdl-cffi::y)))
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
				     (cffi:foreign-slot-value ,sdl-event 'sdl-cffi::Sdl-Mouse-Button-Event 'sdl-cffi::button)))
				 (:state
				  `(,(intern (format nil "~A" (second key)) :keyword)
				     (cffi:foreign-slot-value ,sdl-event 'sdl-cffi::Sdl-Mouse-Button-Event 'sdl-cffi::state)))
				 (:x
				  `(,(intern (format nil "~A" (second key)) :keyword)
				     (cffi:foreign-slot-value ,sdl-event 'sdl-cffi::Sdl-Mouse-Button-Event 'sdl-cffi::x)))
				 (:y
				  `(,(intern (format nil "~A" (second key)) :keyword)
				     (cffi:foreign-slot-value ,sdl-event 'sdl-cffi::Sdl-Mouse-Button-Event 'sdl-cffi::y)))
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
				     (cffi:foreign-slot-value ,sdl-event 'sdl-cffi::Sdl-Joy-Axis-Event 'sdl-cffi::which)))
				 (:axis
				  `(,(intern (format nil "~A" (second key)) :keyword)
				     (cffi:foreign-slot-value ,sdl-event 'sdl-cffi::Sdl-Joy-Axis-Event 'sdl-cffi::axis)))
				 (:value
				  `(,(intern (format nil "~A" (second key)) :keyword)
				     (cffi:foreign-slot-value ,sdl-event 'sdl-cffi::Sdl-Joy-Axis-Event 'sdl-cffi::value)))
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
				     (cffi:foreign-slot-value ,sdl-event 'sdl-cffi::Sdl-Joy-Button-Event 'sdl-cffi::which)))
				 (:axis
				  `(,(intern (format nil "~A" (second key)) :keyword)
				     (cffi:foreign-slot-value ,sdl-event 'sdl-cffi::Sdl-Joy-Button-Event 'sdl-cffi::axis)))
				 (:value
				  `(,(intern (format nil "~A" (second key)) :keyword)
				     (cffi:foreign-slot-value ,sdl-event 'sdl-cffi::Sdl-Joy-Button-Event 'sdl-cffi::value)))
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
				     (cffi:foreign-slot-value ,sdl-event 'sdl-cffi::Sdl-Joy-Button-Event 'sdl-cffi::which)))
				 (:axis
				  `(,(intern (format nil "~A" (second key)) :keyword)
				     (cffi:foreign-slot-value ,sdl-event 'sdl-cffi::Sdl-Joy-Button-Event 'sdl-cffi::axis)))
				 (:value
				  `(,(intern (format nil "~A" (second key)) :keyword)
				     (cffi:foreign-slot-value ,sdl-event 'sdl-cffi::Sdl-Joy-Button-Event 'sdl-cffi::value)))
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
				     (cffi:foreign-slot-value ,sdl-event 'sdl-cffi::Sdl-Joy-Hat-Event 'sdl-cffi::which)))
				 (:axis
				  `(,(intern (format nil "~A" (second key)) :keyword)
				     (cffi:foreign-slot-value ,sdl-event 'sdl-cffi::Sdl-Joy-Hat-Event 'sdl-cffi::axis)))
				 (:value
				  `(,(intern (format nil "~A" (second key)) :keyword)
				     (cffi:foreign-slot-value ,sdl-event 'sdl-cffi::Sdl-Joy-Hat-Event 'sdl-cffi::value)))
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
				     (cffi:foreign-slot-value ,sdl-event 'sdl-cffi::Sdl-Joy-Ball-Event 'sdl-cffi::which)))
				 (:ball
				  `(,(intern (format nil "~A" (second key)) :keyword)
				     (cffi:foreign-slot-value ,sdl-event 'sdl-cffi::Sdl-Joy-Ball-Event 'sdl-cffi::ball)))
				 (:x-rel
				  `(,(intern (format nil "~A" (second key)) :keyword)
				     (cffi:foreign-slot-value ,sdl-event 'sdl-cffi::Sdl-Joy-Ball-Event 'sdl-cffi::xrel)))
				 (:y-rel
				  `(,(intern (format nil "~A" (second key)) :keyword)
				     (cffi:foreign-slot-value ,sdl-event 'sdl-cffi::Sdl-Joy-Ball-Event 'sdl-cffi::yrel)))
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
				     (cffi:foreign-slot-value ,sdl-event 'sdl-cffi::Sdl-Resize-Event 'sdl-cffi::w)))
				 (:h
				  `(,(intern (format nil "~A" (second key)) :keyword)
				     (cffi:foreign-slot-value ,sdl-event 'sdl-cffi::Sdl-Resize-Event 'sdl-cffi::h)))
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
				     (cffi:foreign-slot-value ,sdl-event 'sdl-cffi::Sdl-User-Event 'sdl-cffi::type)))
				 (:code
				  `(,(intern (format nil "~A" (second key)) :keyword)
				     (cffi:foreign-slot-value ,sdl-event 'sdl-cffi::Sdl-User-Event 'sdl-cffi::code)))
				 (:data1
				  `(,(intern (format nil "~A" (second key)) :keyword)
				     (cffi:foreign-slot-pointer ,sdl-event 'sdl-cffi::Sdl-User-Event 'sdl-cffi::data1)))
				 (:data2
				  `(,(intern (format nil "~A" (second key)) :keyword)
				     (cffi:foreign-slot-pointer ,sdl-event 'sdl-cffi::Sdl-User-Event 'sdl-cffi::data2)))
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

(defmacro with-events (args &body events)
  "(with-events
     (:active-event (:gain gain :state state)
		     t)
     (:key-down-event (:state state :scancode scancode :key key :mod mod :unicode unicode)
	       t)
     (:key-up-event (:state state :scancode scancode :key key :mod mod :unicode unicode)
	     t)
     (:mouse-motion-event (:state state :x x :y y :x-rel xrel :y-rel yrel)
		   t)
     (:mouse-button-down-event (:button button :state state :x x :y y)
		       t)
     (:mouse-button-up-event (:button button :state state :x x :y y)
		     t)
     (:joy-axis-motion-event (:which which :axis axis :value value)
		     t)
     (:joy-button-down-event (:which which :button button :state state)
		     t)
     (:joy-button-up-event (:which which :button button :state state)
		   t)
     (:joy-hat-motion-event (:which which :hat hat :value value)
		    t)
     (:joy-ball-motion-event (:which which :ball ball :xrel xrel :yrel yrel)
		     t)
     (:video-resize-event (:w w :h h)
		   t)
     (:video-expose-event ()
      t)
     (:sys-wm-event ()
      t)
     (:quit-event ()
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
                                    (:active-event
                                     (expand-activeevent sdl-event 
                                                         (first (rest event)) 
							 (rest (rest event))))
				    (:key-down-event
				     (expand-keydown sdl-event 
						     (first (rest event)) 
						     (rest (rest event))))
				    (:key-up-event
				     (expand-keyup sdl-event 
						   (first (rest event)) 
						   (rest (rest event))))
				    (:mouse-motion-event
				     (expand-mousemotion sdl-event 
							 (first (rest event)) 
							 (rest (rest event))))
				    (:mouse-button-down-event
				     (expand-mousebuttondown sdl-event
							     (first (rest event)) 
							     (rest (rest event))))
				    (:mouse-button-up-event
				     (expand-mousebuttonup sdl-event 
							   (first (rest event)) 
							   (rest (rest event))))
				    (:joy-axis-motion-event
				     (expand-joyaxismotion sdl-event 
							   (first (rest event)) 
							   (rest (rest event))))
				    (:joy-button-down-event
				     (expand-joybuttondown sdl-event 
							   (first (rest event)) 
							   (rest (rest event))))
				    (:joy-button-up-event
				     (expand-joybuttonup sdl-event 
							 (first (rest event)) 
							 (rest (rest event))))
				    (:joy-hat-motion-event
				     (expand-joyhatmotion sdl-event 
							  (first (rest event)) 
							  (rest (rest event))))
				    (:joy-ball-motion-event
				     (expand-joyballmotion sdl-event 
							   (first (rest event)) 
							   (rest (rest event))))
				    (:video-resize-event
				     (expand-videoresize sdl-event 
							 (first (rest event)) 
							 (rest (rest event))))
				    (:video-expose-event
				     (expand-videoexpose sdl-event 
							 (rest (rest event))))
				    (:sys-wm-event
				     (expand-syswmevent sdl-event 
							(rest (rest event))))
				    (:quit-event
				     (expand-quit sdl-event 
						  (rest (rest event)) 
						  quit))
				    (:user-event
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
	  (frame-rate-delay)))
      (cffi:foreign-free ,sdl-event))))
