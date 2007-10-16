(in-package #:lispbuilder-sdl-cffi) 

(cffi:defcenum Sdl-Event-Type
	(:SDL-NO-EVENT 0)
	:SDL-ACTIVE-EVENT
	:SDL-KEY-DOWN-EVENT
	:SDL-KEY-UP-EVENT
	:SDL-MOUSE-MOTION-EVENT
	:SDL-MOUSE-BUTTON-DOWN-EVENT
	:SDL-MOUSE-BUTTON-UP-EVENT
	:SDL-JOY-AXIS-MOTION-EVENT
	:SDL-JOY-BALL-MOTION-EVENT
	:SDL-JOY-HAT-MOTION-EVENT
	:SDL-JOY-BUTTON-DOWN-EVENT
	:SDL-JOY-BUTTON-UP-EVENT
	:SDL-QUIT-EVENT
	:SDL-SYS-WM-EVENT
	:SDL-EVENT-RESERVEDA
	:SDL-EVENT-RESERVEDB
	:SDL-VIDEO-RESIZE-EVENT
	:SDL-VIDEO-EXPOSE-EVENT
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

(cffi:defcfun ("SDL_PumpEvents" SDL-Pump-Events) :void)

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
  (1<< X))

(defun SDL-ACTIVE-EVENT-MASK ()
  (SDL-EVENT-MASK (cffi:foreign-enum-value 'Sdl-Event-Type :SDL-ACTIVE-EVENT)))

(defun SDL-KEY-DOWN-MASK ()
  (SDL-EVENT-MASK (cffi:foreign-enum-value 'Sdl-Event-Type :SDL-KEY-DOWN-EVENT)))

(defun SDL-KEY-UP-MASK ()
  (SDL-EVENT-MASK (cffi:foreign-enum-value 'Sdl-Event-Type :SDL-KEY-UP-EVENT)))

(defun SDL-KEY-EVENT-MASK ()
  (or (SDL-EVENT-MASK (cffi:foreign-enum-value 'Sdl-Event-Type :SDL-KEY-UP-EVENT))
      (SDL-EVENT-MASK (cffi:foreign-enum-value 'Sdl-Event-Type :SDL-KEY-DOWN-EVENT))))

(defun SDL-MOUSE-MOTION-MASK ()
  (SDL-EVENT-MASK (cffi:foreign-enum-value 'Sdl-Event-Type :SDL-MOUSE-MOTION-EVENT)))

(defun SDL-MOUSE-BUTTON-DOWN-MASK ()
  (SDL-EVENT-MASK (cffi:foreign-enum-value 'Sdl-Event-Type :sdl-mouse-button-down-EVENT)))

(defun SDL-MOUSE-BUTTON-UP-MASK ()
  (SDL-EVENT-MASK (cffi:foreign-enum-value 'Sdl-Event-Type :sdl-mouse-button-up-EVENT)))

(defun SDL-MOUSE-EVENT-MASK ()
  (logior (SDL-EVENT-MASK (cffi:foreign-enum-value 'Sdl-Event-Type :SDL-MOUSE-MOTION-EVENT))
          (SDL-EVENT-MASK (cffi:foreign-enum-value 'Sdl-Event-Type :sdl-mouse-button-down-EVENT))
          (SDL-EVENT-MASK (cffi:foreign-enum-value 'Sdl-Event-Type :sdl-mouse-button-up-EVENT))))

(defun SDL-JOY-AXIS-MOTION-MASK ()
  (SDL-EVENT-MASK (cffi:foreign-enum-value 'Sdl-Event-Type :SDL-JOY-AXIS-MOTION-EVENT)))

(defun SDL-JOY-BALL-MOTION-MASK ()
  (SDL-EVENT-MASK (cffi:foreign-enum-value 'Sdl-Event-Type :SDL-JOY-BALL-MOTION-EVENT)))

(defun SDL-JOY-HAT-MOTION-MASK ()
  (SDL-EVENT-MASK (cffi:foreign-enum-value 'Sdl-Event-Type :SDL-JOY-HAT-MOTION-EVENT)))

(defun SDL-JOY-BUTTON-DOWN-MASK ()
  (SDL-EVENT-MASK (cffi:foreign-enum-value 'Sdl-Event-Type :SDL-JOY-BUTTON-DOWN-EVENT)))

(defun SDL-JOY-BUTTON-UP-MASK ()
  (SDL-EVENT-MASK (cffi:foreign-enum-value 'Sdl-Event-Type :SDL-JOY-BUTTON-UP-EVENT)))

(defun SDL-JOY-EVENT-MASK ()
  (logior (SDL-EVENT-MASK (cffi:foreign-enum-value 'Sdl-Event-Type :SDL-JOY-AXIS-MOTION-EVENT))
          (SDL-EVENT-MASK (cffi:foreign-enum-value 'Sdl-Event-Type :SDL-JOY-BALL-MOTION-EVENT))
          (SDL-EVENT-MASK (cffi:foreign-enum-value 'Sdl-Event-Type :SDL-JOY-HAT-MOTION-EVENT))
          (SDL-EVENT-MASK (cffi:foreign-enum-value 'Sdl-Event-Type :SDL-JOY-BUTTON-DOWN-EVENT))
          (SDL-EVENT-MASK (cffi:foreign-enum-value 'Sdl-Event-Type :SDL-JOY-BUTTON-UP-EVENT))))

(defun SDL-VIDEO-RESIZE-MASK () 
  (SDL-EVENT-MASK (cffi:foreign-enum-value 'Sdl-Event-Type :SDL-VIDEO-RESIZE-EVENT)))

(defun SDL-VIDEO-EXPOSE-MASK ()
  (SDL-EVENT-MASK (cffi:foreign-enum-value 'Sdl-Event-Type :SDL-VIDEO-EXPOSE-EVENT)))

(defun SDL-QUIT-MASK ()
  (SDL-EVENT-MASK (cffi:foreign-enum-value 'Sdl-Event-Type :SDL-QUIT-EVENT)))

(defun SDL-SYS-WM-EVENT-MASK ()
  (SDL-EVENT-MASK (cffi:foreign-enum-value 'Sdl-Event-Type :SDL-SYS-WM-EVENT)))
