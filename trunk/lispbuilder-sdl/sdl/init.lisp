(in-package #:lispbuilder-sdl)

;;;; Functions


;; (defmacro with-init (renderer &body body)
;;   (let ((rend (gensym "rend-Initializes")))
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
  "`WITH-INIT` is a convenience macro that will attempt to initialise the SDL library and SDL subsystems 
prior to executing the forms in `BODY`. Upon exit `WITH-INIT` will uninitialize the SDL library and SDL subsystems.

The lispbuilder-sdl initialization routines are somewhat complicated by the fact that 
a Lisp development environment will load a foreign library once but then 
initialise and uninitialise the library multiple times. A C/C++ development environment will open and then 
close a library after each execution, freeing all resources left hanging by incomplete 
or buggy uninitialise functions. C libraries may therefore frequently core dump in a Lisp environment when 
resources are not feed properly prior to the library being reinitialized.

LISPBUILDER-SDL provides functionality affording the programmer a finer granularity of control 
of the initialisation/uninitialisation of foreign libraries. The
fuctions that provide these capabilities are as follows:
* [INITIALIZE-SUBSYSTEMS-ON-STARTUP](#initialize-subsystems-on-startup)
* [QUIT-SUBSYSTEMS-ON-EXIT](#quit-subsystems-on-exit)
* [LIST-SUBSYSTEMS](#list-subsystems)
* [RETURN-SUBSYSTEMS-OF-STATUS](#return-subsystems-of-status)
* [INIT-SUBSYSTEMS](#init-subsystems)
* [QUIT-SUBSYSTEMS](#quit-subsystems)
* [INIT-SDL](#init-sdl)
* [QUIT-SDL](#quit-sdl)

##### Defaults

* By default `WITH-INIT` will only initialise the `SDL-INIT-VIDEO` SDL subsystem. 
Additional SDL subsystems can be initialized by calling [INITIALIZE-SUBSYSTEMS-ON-STARTUP](#initialize-subsystems-on-startup).
* By default `WITH-INIT` will only uninitialise the `SDL-INIT-VIDEO` SDL subsystem. 
Additional SDL subsystems can be uninitialized by calling [QUIT-SUBSYSTEMS-ON-EXIT](#quit-subsystems-on-exit).

###### Initialisation/Uninitialisation of the SDL library

The SDL library is initialised only:
* If the library is not yet already initialized, or  
* [INIT-ON-STARTUP](#init-on-startup) is `T`.

The SDL library is uninitialised only:
* When [QUIT-ON-EXIT](#quit-on-exit) is `T`.

###### Initialisation/Uninitialisation of external libraries

Hooks are provided to allow external libraries to be initialized or uninitialised automatically following 
the initialisation or uninitialisation of the SDL library.

To initialise an external library, push a function that initialises the external library onto 
`\*EXTERNAL-INIT-SUBSYSTEMS-ON-STARTUP\*`. The function must take no arguments. For example:

    \(defun init-ttf \(\)
       \(if \(is-init\)
         t
         \(sdl-ttf-cffi::ttf-init\)\)\)
    \(pushnew 'init-ttf sdl:*external-init-subsystems-on-startup*\) 

To uninitialise an external library, push a function that uninitialises the external library onto
 `\*EXTERNAL-QUIT-SUBSYSTEMS-ON-EXIT\*`. The function must take no arguments. For example:

    \(defun quit-ttf \(\)
       \(if \(is-init\)
         \(sdl-ttf-cffi::ttf-quit\)\)\)
    \(pushnew 'quit-ttf sdl:*external-quit-subsystems-on-exit*\)

##### Parameters

* `FLAGS` may be one or more of: `SDL-INIT-EVERYTHING`, `SDL-INIT-VIDEO`, `SDL-INIT-CDROM`, `SDL-INIT-AUDIO`, 
`SDL-INIT-TIMER`, `SDL-INIT-JOYSTICK`, `SDL-INIT-EVENTTHREAD` and `SDL-INIT-NOPARACHUTE`. *Note*: When `FLAGS` is set by 
`WITH-INIT`, subsequent calls to [INITIALIZE-SUBSYSTEMS-ON-STARTUP](#initialize-subsystems-on-startup) and 
[QUIT-SUBSYSTEMS-ON-EXIT](#quit-subsystems-on-exit) are ignored. `WITH-INIT` will only initialize and uninitialize the subsytems 
specified in `FLAGS`.

##### Example

    \(with-init \(SDL-INIT-VIDEO SDL-INIT-CDROM SDL-INIT-AUDIO\)
      ....\)

    \(with-init \(\)
      \(INITIALIZE-SUBSYSTEMS-ON-STARTUP SDL-INIT-VIDEO SDL-INIT-CDROM SDL-INIT-AUDIO\)
      \(QUIT-SUBSYSTEMS-ON-EXIT SDL-INIT-VIDEO SDL-INIT-CDROM SDL-INIT-AUDIO\)
      ....\)"
  `(block nil
     (unwind-protect
	  (when (init-sdl)
	    ,(when flags
		   `(initialize-subsystems-on-startup ,@flags))
	    (init-subsystems)
	    ,@body)
       ,(when flags
	      `(quit-subsystems-on-exit ,@flags))
       (quit-subsystems)
       (quit-sdl))))

(defun initialize-subsystems-on-startup (&rest flags)
  "Sets the SDL subsystems that must be initialized in 
subsequent calls to [INIT-SUBSYSTEMS](#init-subsystems).

##### Parameters

* `FLAGS` may be one or more of: `SDL-INIT-EVERYTHING`, `SDL-INIT-VIDEO`, `SDL-INIT-CDROM`, `SDL-INIT-AUDIO`, 
`SDL-INIT-TIMER`, `SDL-INIT-JOYSTICK`, `SDL-INIT-EVENTTHREAD` and `SDL-INIT-NOPARACHUTE`.

##### Returns

* Returns an INTEGER bitmask of the SDL subsystems in `FLAGS`.

##### Example

    \(INITIALIZE-SUBSYSTEMS-ON-STARTUP SDL:SDL-INIT-VIDEO SDL:SDL-INIT-CDROM\)"
  (setf *initialize-subsystems-on-startup* (apply #'logior flags)))

(defun quit-subsystems-on-exit (&rest flags)
    "Sets one or more SDL subsystems that must be uninitialized in 
subsequent calls to [QUIT-SUBSYSTEMS](#quit-subsystems). 

##### Parameters

* `FLAGS` may be one or more of: `SDL-INIT-EVERYTHING`, `SDL-INIT-VIDEO`, `SDL-INIT-CDROM`, `SDL-INIT-AUDIO`, 
`SDL-INIT-TIMER`, `SDL-INIT-JOYSTICK`, `SDL-INIT-EVENTTHREAD`, `SDL-INIT-NOPARACHUTE`.

##### Returns

* Returns an INTEGER bitmask of the SDL subsystems in `FLAGS`.

##### Example

    \(QUIT-SUBSYSTEMS-ON-EXIT SDL:SDL-INIT-VIDEO SDL:SDL-INIT-CDROM\)"
  (setf *quit-subsystems-on-exit* (apply #'logior flags)))

(defun list-subsystems (flag)
  "Returns a list of SDL subsystems that are specified in `FLAGS`.

`FLAGS` is an `INTEGER` bitmask containing the logior of zero or more of: `SDL-INIT-EVERYTHING`, `SDL-INIT-VIDEO`, `SDL-INIT-CDROM`, `SDL-INIT-AUDIO`, 
`SDL-INIT-TIMER`, `SDL-INIT-JOYSTICK`, `SDL-INIT-EVENTTHREAD` and `SDL-INIT-NOPARACHUTE`."
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

(defun return-subsystems-of-status (flags status)
  "Returns the `STATUS` of the the specified SDL subsystems in `FLAGS` as an INTEGER bitmask. 

##### Parameters
* `FLAGS` must contain a logior of zero or more of: `SDL-INIT-EVERYTHING`, `SDL-INIT-VIDEO`, `SDL-INIT-CDROM`, `SDL-INIT-AUDIO`, 
`SDL-INIT-TIMER`, `SDL-INIT-JOYSTICK`, `SDL-INIT-EVENTTHREAD`, `SDL-INIT-NOPARACHUTE`.
* `STATUS` when `T` returns an INTEGER bitmask of the specified initialized subsystems.
`STATUS` when `NIL`, determines an INTEGER bitmask of the specified uninitialised subsystems."
  (let ((subsystems (sdl-cffi::sdl-was-init sdl-cffi::sdl-init-everything))
	(to-initialize 0)
	(status-fn (if status #'/= #'=)))
    (mapc #'(lambda (subsystem)
	      (let ((value (second subsystem)))
		(when (funcall status-fn 0 (logand subsystems value))
		  (setf to-initialize (logior to-initialize
					      value)))))
	  (list-subsystems flags))
    to-initialize))

(defun init-subsystems (&optional (flags *initialize-subsystems-on-startup*))
  "Initializes the SDL subsystems specified in `FLAGS`. 
`FLAGS` is an `INTEGER` bitmask containing the logior of zero or more of: `SDL-INIT-EVERYTHING`, `SDL-INIT-VIDEO`, `SDL-INIT-CDROM`, `SDL-INIT-AUDIO`, 
`SDL-INIT-TIMER`, `SDL-INIT-JOYSTICK`, `SDL-INIT-EVENTTHREAD` and `SDL-INIT-NOPARACHUTE`.

`INIT-SUBSYSTEMS` can be called only after SDL is succesfully initialized by [INIT-SDL](#init-sdl)."
  (sdl-cffi::sdl-init-subsystem (return-subsystems-of-status flags nil)))

(defun quit-subsystems (&optional (flags *quit-subsystems-on-exit*))
  "Uninitializes the SDL subsystems specified in the `INTEGER` bitmask `FLAGS`. 
`FLAGS` contains the logior of zero or more of: `SDL-INIT-EVERYTHING`, `SDL-INIT-VIDEO`, `SDL-INIT-CDROM`, `SDL-INIT-AUDIO`, 
`SDL-INIT-TIMER`, `SDL-INIT-JOYSTICK`, `SDL-INIT-EVENTTHREAD`, `SDL-INIT-NOPARACHUTE`.

`QUIT-SUBSYSTEMS` can be called only after SDL is successfully intialized using [INIT-SDL](#init-sdl)."
  (sdl-cffi::sdl-quit-subsystem (return-subsystems-of-status flags t)))

(defun quit-on-exit ()
  "Returns `T` if the SDL library will be uninitialised in a call to [QUIT-SDL](#quit-sdl), or [WITH-INIT](#with-init). 
Returns `NIL` otherwise."
  *quit-on-exit*)
(defun set-quit-on-exit (val)
  (if val
      (setf *init-on-startup* val
	    *quit-on-exit* val)
      (setf *quit-on-exit* val)))
(defsetf quit-on-exit set-quit-on-exit)

(defun initialized-subsystems-p ()
  "Returns a list of the initialized SDL subsystems."
  (list-subsystems (sdl-cffi::sdl-was-init sdl-cffi::sdl-init-everything)))

(defun init-sdl (&optional (init (init-on-startup)))
  "Initalizes the SDL library when the `OPTIONAL` parameter `INIT` is `T`, or 
the value returned by [INIT-ON-STARTUP](#init-on-startup) is `T`."
  (let ((initialized? (if (or init
			      (not *initialized*))
			  (if (sdl-base::init-sdl :flags nil)
			      (setf *initialized* t)
			      nil)
			  t)))
    (dolist (fn *external-init-subsystems-on-startup*)
      (funcall fn))
    (setf sdl-base::*default-fpsmanager* (make-instance 'sdl-base::fps-fixed))
    initialized?))

(defun quit-sdl (&optional (quit (quit-subsystems-on-exit)))
  "Uninitalizes the SDL library when the `OPTIONAL` parameter `QUIT` is `T`, or 
the value returned by [QUIT-SUBSYSTEMS-ON-EXIT](#quit-subsystems-on-exit) is `T`."
  (when quit
    (setf *initialized* nil)
    (sdl-cffi::SDL-Quit))
  (dolist (fn *external-quit-subsystems-on-exit*)
    (funcall fn)))

(defun init-on-startup ()
    "Returns `T` if the SDL library must be initialised in [INIT-SDL](#init-sdl), or [WITH-INIT](#with-init). 
Returns `NIL` otherwise."
  *init-on-startup*)
(defun set-init-on-startup (val)
  (setf *init-on-startup* val))
(defsetf init-on-startup set-init-on-startup)
