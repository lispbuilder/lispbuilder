(in-package #:lispbuilder-sdl-base) 

(cffi:defcenum Sdl-Event-Type
	(:SDL-NO-VENT 0)
	:SDL-ACTIVE-EVENT
	:SDL-KEY-DOWN
	:SDL-KEY-UP
	:SDL-MOUSE-MOTION
	:SDL-MOUSE-BUTTON-DOWN
	:SDL-MOUSE-BUTTON-UP
	:SDL-JOY-AXIS-MOTION
	:SDL-JOY-BALL-MOTION
	:SDL-JOY-HAT-MOTION
	:SDL-JOY-BUTTON-DOWN
	:SDL-JOY-BUTTON-UP
	:SDL-QUIT
	:SDL-SYS-WM-EVENT
	:SDL-EVENT-RESERVEDA
	:SDL-EVENT-RESERVEDB
	:SDL-VIDEO-RESIZE
	:SDL-VIDEO-EXPOSE
	:SDL-EVENT-RESERVED2
	:SDL-EVENT-RESERVED3
	:SDL-EVENT-RESERVED4
	:SDL-EVENT-RESERVED5
	:SDL-EVENT-RESERVED6
	:SDL-EVENT-RESERVED7
	(:SDL-USER-EVENT 24)
	(:SDL-NUM-EVENTS 32))

(cl:defconstant SDL-ALL-EVENTS #xFFFFFFFF)

(cffi:defcstruct SDL-Active-Event
	(type :unsigned-char)
	(gain :unsigned-char)
	(state :unsigned-char))

(cffi:defcstruct SDL-Keyboard-Event
	(type :unsigned-char)
	(which :unsigned-char)
	(state :unsigned-char)
	(keysym SDL-key-sym))

(cffi:defcstruct SDL-Mouse-Motion-Event
	(type :unsigned-char)
	(which :unsigned-char)
	(state :unsigned-char)
	(x :unsigned-short)
	(y :unsigned-short)
	(xrel :short)
	(yrel :short))

(cffi:defcstruct SDL-Mouse-Button-Event
	(type :unsigned-char)
	(which :unsigned-char)
	(button :unsigned-char)
	(state :unsigned-char)
	(x :unsigned-short)
	(y :unsigned-short))

(cffi:defcstruct SDL-Joy-Axis-Event
	(type :unsigned-char)
	(which :unsigned-char)
	(axis :unsigned-char)
	(value :short))

(cffi:defcstruct SDL-Joy-Ball-Event
	(type :unsigned-char)
	(which :unsigned-char)
	(ball :unsigned-char)
	(xrel :short)
	(yrel :short))

(cffi:defcstruct SDL-Joy-Hat-Event
	(type :unsigned-char)
	(which :unsigned-char)
	(hat :unsigned-char)
	(value :unsigned-char))

(cffi:defcstruct SDL-Joy-Button-Event
	(type :unsigned-char)
	(which :unsigned-char)
	(button :unsigned-char)
	(state :unsigned-char))

(cffi:defcstruct SDL-Resize-Event
	(type :unsigned-char)
	(w :int)
	(h :int))

(cffi:defcstruct SDL-Expose-Event
	(type :unsigned-char))

(cffi:defcstruct SDL-Quit-Event
	(type :unsigned-char))

(cffi:defcstruct SDL-User-Event
	(type :unsigned-char)
	(code :int)
	(data1 :pointer)
	(data2 :pointer))

(cffi:defcstruct SDL-Sys-WM-Event
	(type :unsigned-char)
	(msg :pointer))

(cffi:defcunion SDL-Event
	(type :unsigned-char)
	(active SDL-Active-Event)
	(key SDL-Keyboard-Event)
	(motion SDL-Mouse-Motion-Event)
	(button SDL-Mouse-Button-Event)
	(jaxis SDL-Joy-Axis-Event)
	(jball SDL-Joy-Ball-Event)
	(jhat SDL-Joy-Hat-Event)
	(jbutton SDL-Joy-Button-Event)
	(resize SDL-Resize-Event)
	(expose SDL-Expose-Event)
	(quit SDL-Quit-Event)
	(user SDL-User-Event)
	(syswm SDL-Sys-WM-Event))

(cffi:defcfun ("SDL_PumpEvents" SDL-PumpEvents) :void)

(cffi:defcenum SDL-event-action
	:SDL-ADD-EVENT
	:SDL-PEEK-EVENT
	:SDL-GET-EVENT)

(cffi:defcfun ("SDL_PeepEvents" SDL-Peep-Events) :int
  (events :pointer)
  (numevents :int)
  (action SDL-event-action)
  (mask :unsigned-int))

(cffi:defcfun ("SDL_PollEvent" SDL-Poll-Event) :int
  (event :pointer))

(cffi:defcfun ("SDL_WaitEvent" SDL-Wait-Event) :int
  (event :pointer))

(cffi:defcfun ("SDL_PushEvent" SDL-Push-Event) :int
  (event :pointer))

(cffi:defcfun ("SDL_SetEventFilter" SDL-Set-Event-Filter) :void
  (filter :pointer))

(cffi:defcfun ("SDL_GetEventFilter" SDL-Get-Event-Filter) :pointer)

(cl:defconstant SDL-QUERY -1)

(cl:defconstant SDL-IGNORE 0)

(cl:defconstant SDL-DISABLE 0)

(cl:defconstant SDL-ENABLE 1)

(cffi:defcfun ("SDL_EventState" SDL-Event-State) :unsigned-char
  (type :unsigned-char)
  (state :int))




(defun SDL-EVENT-MASK (X)
  (ash 1 X ))

(defun SDL-ACTIVE-EVENT-MASK ()
  (SDL-EVENT-MASK (cffi:foreign-enum-value 'Sdl-Event-Type :SDL-ACTIVE-EVENT)))

(defun SDL-KEY-DOWN-MASK ()
  (SDL-EVENT-MASK (cffi:foreign-enum-value 'Sdl-Event-Type :SDL-KEY-DOWN)))

(defun SDL-KEY-UP-MASK ()
  (SDL-EVENT-MASK (cffi:foreign-enum-value 'Sdl-Event-Type :SDL-KEY-UP)))

(defun SDL-KEY-EVENT-MASK ()
  (or (SDL-EVENT-MASK (cffi:foreign-enum-value 'Sdl-Event-Type :SDL-KEY-UP))
      (SDL-EVENT-MASK (cffi:foreign-enum-value 'Sdl-Event-Type :SDL-KEY-DOWN))))

(defun SDL-MOUSE-MOTION-MASK ()
  (SDL-EVENT-MASK (cffi:foreign-enum-value 'Sdl-Event-Type :SDL-MOUSE-MOTION)))

(defun SDL-MOUSE-BUTTON-DOWN-MASK ()
  (SDL-EVENT-MASK (cffi:foreign-enum-value 'Sdl-Event-Type :sdl-mouse-button-down)))

(defun SDL-MOUSE-BUTTON-UP-MASK ()
  (SDL-EVENT-MASK (cffi:foreign-enum-value 'Sdl-Event-Type :sdl-mouse-button-up)))

(defun SDL-MOUSE-EVENT-MASK ()
  (logior (SDL-EVENT-MASK (cffi:foreign-enum-value 'Sdl-Event-Type :SDL-MOUSE-MOTION))
          (SDL-EVENT-MASK (cffi:foreign-enum-value 'Sdl-Event-Type :sdl-mouse-button-down))
          (SDL-EVENT-MASK (cffi:foreign-enum-value 'Sdl-Event-Type :sdl-mouse-button-up))))

(defun SDL-JOY-AXIS-MOTION-MASK ()
  (SDL-EVENT-MASK (cffi:foreign-enum-value 'Sdl-Event-Type :SDL-JOY-AXIS-MOTION)))

(defun SDL-JOY-BALL-MOTION-MASK ()
  (SDL-EVENT-MASK (cffi:foreign-enum-value 'Sdl-Event-Type :SDL-JOY-BALL-MOTION)))

(defun SDL-JOY-HAT-MOTION-MASK ()
  (SDL-EVENT-MASK (cffi:foreign-enum-value 'Sdl-Event-Type :SDL-JOY-HAT-MOTION)))

(defun SDL-JOY-BUTTON-DOWN-MASK ()
  (SDL-EVENT-MASK (cffi:foreign-enum-value 'Sdl-Event-Type :SDL-JOY-BUTTON-DOWN)))

(defun SDL-JOY-BUTTON-UP-MASK ()
  (SDL-EVENT-MASK (cffi:foreign-enum-value 'Sdl-Event-Type :SDL-JOY-BUTTON-UP)))

(defun SDL-JOY-EVENT-MASK ()
  (logior (SDL-EVENT-MASK (cffi:foreign-enum-value 'Sdl-Event-Type :SDL-JOY-AXIS-MOTION))
          (SDL-EVENT-MASK (cffi:foreign-enum-value 'Sdl-Event-Type :SDL-JOY-BALL-MOTION))
          (SDL-EVENT-MASK (cffi:foreign-enum-value 'Sdl-Event-Type :SDL-JOY-HAT-MOTION))
          (SDL-EVENT-MASK (cffi:foreign-enum-value 'Sdl-Event-Type :SDL-JOY-BUTTON-DOWN))
          (SDL-EVENT-MASK (cffi:foreign-enum-value 'Sdl-Event-Type :SDL-JOY-BUTTON-UP))))

(defun SDL-VIDEO-RESIZE-MASK () 
  (SDL-EVENT-MASK (cffi:foreign-enum-value 'Sdl-Event-Type :SDL-VIDEO-RESIZE)))

(defun SDL-VIDEO-EXPOSE-MASK ()
  (SDL-EVENT-MASK (cffi:foreign-enum-value 'Sdl-Event-Type :SDL-VIDEO-EXPOSE)))

(defun SDL-QUIT-MASK ()
  (SDL-EVENT-MASK (cffi:foreign-enum-value 'Sdl-Event-Type :SDL-QUIT)))

(defun SDL-SYS-WM-EVENT-MASK ()
  (SDL-EVENT-MASK (cffi:foreign-enum-value 'Sdl-Event-Type :SDL-SYS-WM-EVENT)))
