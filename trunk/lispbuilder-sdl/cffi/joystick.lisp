
(in-package #:lispbuilder-sdl-cffi)

(cffi:defcfun ("SDL_NumJoysticks" Num-Joysticks) :int)

(cffi:defcfun ("SDL_JoystickName" SDL-Joystick-Name) :string
  (device-index :int))

(cffi:defcfun ("SDL_JoystickOpen" SDL-Joystick-Open) :pointer
  (device-index :int))

(cffi:defcfun ("SDL_JoystickOpened" SDL-Joystick-Opened) :int
  (device-index :int))

(cffi:defcfun ("SDL_JoystickIndex" SDL-Joystick-Index) :int
  (joystick :pointer))

(cffi:defcfun ("SDL_JoystickNumAxes" SDL-Joystick-Num-Axes) :int
  (joystick :pointer))

(cffi:defcfun ("SDL_JoystickNumBalls" SDL-Joystick-Num-Balls) :int
  (joystick :pointer))

(cffi:defcfun ("SDL_JoystickNumHats" SDL-Joystick-Num-Hats) :int
  (joystick :pointer))

(cffi:defcfun ("SDL_JoystickNumButtons" SDL-Joystick-Num-Buttons) :int
  (joystick :pointer))

(cffi:defcfun ("SDL_JoystickUpdate" SDL-Joystick-Update) :void)

(cffi:defcfun ("SDL_JoystickEventState" SDL-Joystick-Event-State) :int
  (state :int))

(cffi:defcfun ("SDL_JoystickGetAxis" SDL-Joystick-Get-Axis) :short
  (joystick :pointer)
  (axis :int))

(cl:defconstant SDL-HAT-CENTERED #x00)

(cl:defconstant SDL-HAT-UP #x01)

(cl:defconstant SDL-HAT-RIGHT #x02)

(cl:defconstant SDL-HAT-DOWN #x04)

(cl:defconstant SDL-HAT-LEFT #x08)

(cffi:defcfun ("SDL_JoystickGetHat" SDL-Joystick-Get-Hat) :unsigned-char
  (joystick :pointer)
  (hat :int))

(cffi:defcfun ("SDL_JoystickGetBall" SDL-Joystick-Get-Ball) :int
  (joystick :pointer)
  (ball :int)
  (dx :pointer)
  (dy :pointer))

(cffi:defcfun ("SDL_JoystickGetButton" SDL-Joystick-Get-Button) :unsigned-char
  (joystick :pointer)
  (button :int))

(cffi:defcfun ("SDL_JoystickClose" SDL-Joystick-Close) :void
  (joystick :pointer))

(defconstant SDL-HAT-RIGHT-UP   (logior SDL-HAT-RIGHT SDL-HAT-UP))
(defconstant SDL-HAT-RIGHT-DOWN (logior SDL-HAT-RIGHT SDL-HAT-DOWN))
(defconstant SDL-HAT-LEFT-UP    (logior SDL-HAT-LEFT SDL-HAT-UP))
(defconstant SDL-HAT-LEFT-DOWN  (logior SDL-HAT-LEFT SDL-HAT-DOWN))
