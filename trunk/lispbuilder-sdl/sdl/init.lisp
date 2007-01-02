
(in-package #:lispbuilder-sdl)

;;;; Functions


;; (defmacro with-init (renderer &body body)
;;   (let ((rend (gensym "rend-")))
;;     `(let ((,rend ,(if renderer
;; 		       `(make-instance ,@renderer)
;; 		       `(make-instance 'sdl))))
;;        (when (hook-initialize-engine ,rend)
;; 	 (unwind-protect
;; 	      (progn
;; ;		(init-timestep)
;; 		,@body)
;; 	   (hook-finalize-engine ,rend))))))

(defmacro with-init (flags &body body)
  "Initializes SDL \(using SDL_Init\) prior to intializing 
SDL subsystems using INIT-SUB-SYSTEMS.
Executes forms contained in BODY.
Uninitializes SDL subsystems using QUIT-SUB-SYSTEMS prior to 
quiting SDL \(using SDL_Quit\) when SDL-QUIT-ON-EXIT returns true T."
  (declare (ignore flags))
  `(block nil
     (unwind-protect
	  (when (init-sdl)
	    (init-sub-systems)
	    ,@body)
       (quit-sub-systems)
       (quit-sdl))))

(defun initialize-on-startup (&rest flags)
  "Sets the SDL sub-systems in FLAGS that will be initialized in 
calls to INIT-SUB-SYSTEMS. FLAGS may be one or more of
SDL-CFFI::SDL-INIT-EVERYTHING, SDL-CFFI::SDL-INIT-VIDEO, 
SDL-CFFI::SDL-INIT-CDROM, SDL-CFFI::SDL-INIT-AUDIO, 
SDL-CFFI::SDL-INIT-TIMER, SDL-CFFI::SDL-INIT-JOYSTICK,
SDL-CFFI::SDL-INIT-EVENTTHREAD, SDL-CFFI::SDL-INIT-NOPARACHUTE."
  (setf *initialize-on-startup* (apply #'logior flags)))

(defun quit-on-exit (&rest flags)
    "Sets the SDL sub-systems in FLAGS that will be uninitialized in 
calls to QUIT-SUB-SYSTEMS. FLAGS may be one or more of
SDL-CFFI::SDL-INIT-EVERYTHING, SDL-CFFI::SDL-INIT-VIDEO, 
SDL-CFFI::SDL-INIT-CDROM, SDL-CFFI::SDL-INIT-AUDIO, 
SDL-CFFI::SDL-INIT-TIMER, SDL-CFFI::SDL-INIT-JOYSTICK,
SDL-CFFI::SDL-INIT-EVENTTHREAD, SDL-CFFI::SDL-INIT-NOPARACHUTE."
  (setf *quit-on-exit* (apply #'logior flags)))

(defun list-sub-systems (flag)
  "Returns a new list of the SDL subsystems specified 
in the flag as an INTEGER bitmask."
  (let ((subsystems nil))
    (if (= flag sdl-cffi::sdl-init-everything)
	(push (list 'sdl-cffi::sdl-init-everything
		    sdl-cffi::sdl-init-everything)
	      subsystems)
	(progn
	  (when (/= 0 (logand flag sdl-cffi::sdl-init-video))
	    (push (list 'sdl-cffi::sdl-init-video
			sdl-cffi::sdl-init-video)
		  subsystems))
	  (when (/= 0 (logand flag sdl-cffi::sdl-init-cdrom))
	    (push (list 'sdl-cffi::sdl-init-cdrom
			sdl-cffi::sdl-init-cdrom)
		  subsystems))
	  (when (/= 0 (logand flag sdl-cffi::sdl-init-audio))
	    (push (list 'sdl-cffi::sdl-init-audio
			sdl-cffi::sdl-init-audio)
		  subsystems))
	  (when (/= 0 (logand flag sdl-cffi::sdl-init-timer))
	    (push (list 'sdl-cffi::sdl-init-timer
			sdl-cffi::sdl-init-timer)
		  subsystems))
	  (when (/= 0 (logand flag sdl-cffi::sdl-init-joystick))
	    (push (list 'sdl-cffi::sdl-init-joystick
			sdl-cffi::sdl-init-joystick)
		  subsystems))
	  (when (/= 0 (logand flag sdl-cffi::sdl-init-eventthread))
	    (push (list 'sdl-cffi::sdl-init-eventthread
			sdl-cffi::sdl-init-eventthread)
		  subsystems))
	  (when (/= 0 (logand flag sdl-cffi::sdl-init-noparachute))
	    (push (list 'sdl-cffi::sdl-init-noparachute
			sdl-cffi::sdl-init-noparachute)
		  subsystems))))
    subsystems))

(defun return-sub-systems-of-status (flags status)
  "Returns an INTEGER bitmask of the SDL subsystems specified in flag an INTEGER bitmask, 
that are already initialized when status is T, or are uninitialized with status in NIL."
  (let ((subsystems (sdl-cffi::sdl-was-init sdl-cffi::sdl-init-everything))
	(to-initialize 0)
	(status-fn (if status #'/= #'=)))
    (mapc #'(lambda (subsystem)
	      (let ((value (second subsystem)))
		(when (funcall status-fn 0 (logand subsystems value))
		  (setf to-initialize (logior to-initialize
					      value)))))
	  (list-sub-systems flags))
    to-initialize))

(defun init-sub-systems (&optional (flags *initialize-on-startup*))
  "Initializes the SDL subsystems in flags, an INTEGER bitmask, 
only if these subsystems are uninitialized.
INIT-SUB-SYSTEMS must be called after SDL is succesfully initialized \(SDL_Init, or INIT-SDL\)."
  (sdl-cffi::sdl-init-sub-system (return-sub-systems-of-status flags nil)))

(defun quit-sub-systems (&optional (flags *quit-on-exit*))
  "Uninitializes the SDL subsystems in flags, an INTEGER bitmask, 
only if these subsystems are initialized."
  (sdl-cffi::sdl-quit-sub-system (return-sub-systems-of-status flags t)))

(defun sdl-quit-on-exit ()
  "Returns T if SDL_Quit will be called when WITH-INIT exits.
Returns NIL if SDL_Quit is not called when WITH-INIT exits."
  *sdl-quit-on-exit*)
(defun set-sdl-quit-on-exit (val)
  (setf *sdl-init-on-startup* val
	*sdl-quit-on-exit* val))
(defsetf sdl-quit-on-exit set-sdl-quit-on-exit)

(defun initialized-sub-systems-p ()
  "Returns a new list of initialized SDL subsystems."
  (list-sub-systems (sdl-cffi::sdl-was-init sdl-cffi::sdl-init-everything)))

(defun init-sdl (&optional (init *sdl-init-on-startup*))
  (if (or init
	  (not *sdl-initialized*))
      (if (sdl-base::init-sdl :flags nil)
	  (setf *sdl-initialized* t)
	  nil)
      t))

(defun quit-sdl (&optional (quit *sdl-quit-on-exit*))
  (when quit
    (setf *sdl-initialized* nil)
    (sdl-cffi::SDL-Quit)))

(defun sdl-init-on-startup ()
  "Returns T if SDL_Init will be called when WITH-INIT starts.
Returns NIL if SDL_Init is not called when WITH-INIT starts."
  *sdl-init-on-startup*)
(defun set-sdl-init-on-startup (val)
  (setf *sdl-init-on-startup* val))
(defsetf sdl-init-on-startup set-sdl-init-on-startup)
