;; SDL (Simple Media Layer) library using CFFI for foreign function interfacing...
;; (C)2006 Luke Crook <luke@balooga.com>
;; Thanks to Frank Buss and Surendra Singh
;; see COPYING for license
;; This file contains some useful functions for using SDL from Common lisp
;; using sdl.lisp (the CFFI wrapper)

(in-package #:lispbuilder-sdl)

(defun create-event (slot-fn)
  (let ((hash-table (make-hash-table)))
    (loop for (slot fn) in slot-fn
          do (setf (gethash slot hash-table) fn))
    hash-table))

(defun create-events (event-list)
  (let ((hash-table (make-hash-table)))
    (loop for (event slots) in event-list
          do (setf (gethash event hash-table) slots))
    hash-table))

(defparameter *events*
  (create-events (list (list :active-event
                             (create-event (list (list :gain 'active-event-gain)
                                                 (list :state 'active-event-state))))
                       (list :key-down-event
                             (create-event (list (list :state 'key-event-state)
                                                 (list :scancode 'key-event-scancode)
                                                 (list :key 'key-event-key)
                                                 (list :mod 'key-event-mod)
                                                 (list :mod-key 'key-event-mod-key)
                                                 (list :unicode 'key-event-unicode))))
                       (list :key-up-event
                             (create-event (list (list :state 'key-event-state)
                                                 (list :scancode 'key-event-scancode)
                                                 (list :key 'key-event-key)
                                                 (list :mod 'key-event-mod)
                                                 (list :mod-key 'key-event-mod-key)
                                                 (list :unicode 'key-event-unicode))))
                       (list :mouse-motion-event
                             (create-event (list (list :state 'mouse-motion-event-state)
                                                 (list :x 'mouse-motion-event-x)
                                                 (list :y 'mouse-motion-event-y)
                                                 (list :x-rel 'mouse-motion-event-x-rel)
                                                 (list :y-rel 'mouse-motion-event-y-rel))))
                       (list :mouse-button-down-event
                             (create-event (list (list :button 'mouse-button-event-button)
                                                 (list :state 'mouse-button-event-state)
                                                 (list :x 'mouse-button-event-x)
                                                 (list :y 'mouse-button-event-y))))
                       (list :mouse-button-up-event
                             (create-event (list (list :button 'mouse-button-event-button)
                                                 (list :state 'mouse-button-event-state)
                                                 (list :x 'mouse-button-event-x)
                                                 (list :y 'mouse-button-event-y))))
                       (list :joy-axis-motion-event
                             (create-event (list (list :which 'joy-axis-motion-event-which)
                                                 (list :axis 'joy-axis-motion-event-axis)
                                                 (list :value 'joy-axis-motion-event-value))))
                       (list :joy-button-down-event
                             (create-event (list (list :which 'joy-button-event-which)
                                                 (list :button 'joy-button-event-button)
                                                 (list :state 'joy-button-event-state))))
                       (list :joy-button-up-event
                             (create-event (list (list :which 'joy-button-event-which)
                                                 (list :button 'joy-button-event-button)
                                                 (list :state 'joy-button-event-state))))
                       (list :joy-hat-motion-event
                             (create-event (list (list :which 'joy-hat-motion-event-which)
                                                 (list :axis 'joy-hat-motion-event-axis)
                                                 (list :value 'joy-hat-motion-event-value))))
                       (list :joy-ball-motion-event
                             (create-event (list (list :which 'joy-ball-motion-event-which)
                                                 (list :ball 'joy-ball-motion-event-ball)
                                                 (list :x-rel 'joy-ball-motion-event-x-rel)
                                                 (list :y-rel 'joy-ball-motion-event-y-rel))))
                       (list :video-resize-event
                             (create-event (list (list :w 'video-resize-event-w)
                                                 (list :h 'video-resize-event-h))))
                       (list :video-expose-event
                             (create-event nil))
                       (list :sys-wm-event
                             (create-event nil))
                       (list :user-event
                             (create-event (list (list :type 'user-event-type)
                                                 (list :code 'user-event-code)
                                                 (list :data1 'user-event-data1)
                                                 (list :data2 'user-event-data2)))))))

(defun get-event-type (sdl-event)
  (cffi:foreign-enum-keyword 'sdl-cffi::Event-Type (cffi:foreign-slot-value sdl-event 'sdl-cffi::sdl-event 'sdl-cffi::type)))

(defun event= (sdl-event event-type &optional event-type-end)
  "Returns `T` if `SDL-EVENT` is of `EVENT-TYPE`.
`EVENT-TYPE` must be one of
:NO-EVENT, :ACTIVE-EVENT, :KEY-DOWN-EVENT, :KEY-UP-EVENT, :MOUSE-MOTION-EVENT, 
:MOUSE-BUTTON-DOWN-EVENT, :MOUSE-BUTTON-UP-EVENT, :JOY-AXIS-MOTION-EVENT, :JOY-BALL-MOTION-EVENT, 
:JOY-HAT-MOTION-EVENT, :JOY-BUTTON-DOWN-EVENT, :JOY-BUTTON-UP-EVENT, :QUIT-EVENT, 
:SYS-WM-EVENT, :VIDEO-RESIZE-EVENT, :VIDEO-EXPOSE-EVENT, :USER-EVENT."
  (if event-type-end
    (if (and (>= (cffi:foreign-slot-value sdl-event 'sdl-cffi::sdl-event 'sdl-cffi::type)
                 (cffi:foreign-enum-value 'sdl-cffi::Event-Type event-type))
             (< (cffi:foreign-slot-value sdl-event 'sdl-cffi::sdl-event 'sdl-cffi::type)
                (- (cffi:foreign-enum-value 'sdl-cffi::Event-Type event-type-end) 1)))
      t
      nil)
    (if (= (cffi:foreign-enum-value 'sdl-cffi::Event-Type event-type)
           (cffi:foreign-slot-value sdl-event 'sdl-cffi::sdl-event 'sdl-cffi::type))
      t
      nil)))

(defun active-event-gain (sdl-event)
  (cffi:foreign-slot-value sdl-event 'sdl-cffi::Sdl-Active-Event 'sdl-cffi::gain))

(defun active-event-state (sdl-event)
  (cffi:foreign-slot-value sdl-event 'sdl-cffi::Sdl-Active-Event 'sdl-cffi::state))

(defun key-event-state (sdl-event)
  (cffi:foreign-slot-value sdl-event 'sdl-cffi::Sdl-Keyboard-Event 'sdl-cffi::state))

(defun key-event-scancode (sdl-event)
  (cffi:foreign-slot-value (cffi:foreign-slot-pointer sdl-event
                                                      'sdl-cffi::sdl-keyboard-event
                                                      'sdl-cffi::keysym)
                           'sdl-cffi::sdl-key-sym 'sdl-cffi::scancode))

(defun key-event-key (sdl-event)
  (cffi:foreign-slot-value (cffi:foreign-slot-pointer sdl-event
                                                      'sdl-cffi::sdl-keyboard-event
                                                      'sdl-cffi::keysym)
                           'sdl-cffi::sdl-key-sym 'sdl-cffi::sym))

(defun key-event-mod (sdl-event)
  (cffi:foreign-slot-value (cffi:foreign-slot-pointer sdl-event
                                                      'sdl-cffi::sdl-keyboard-event
                                                      'sdl-cffi::keysym)
                           'sdl-cffi::sdl-key-sym 'sdl-cffi::mod))

(defun key-event-mod-key (sdl-event)
  (let ((mod-state (cffi:foreign-slot-value (cffi:foreign-slot-pointer sdl-event
                                                                       'sdl-cffi::sdl-keyboard-event
                                                                       'sdl-cffi::keysym)
                                            'sdl-cffi::sdl-key-sym 'sdl-cffi::mod)))
    (remove nil (loop for key in (cffi:foreign-enum-keyword-list 'sdl-cffi::sdl-mod)
                      collect (when (> (logand mod-state
                                               (foreign-enum-value 'sdl-cffi::sdl-mod key))
                                       0)
                                key)))))

(defun key-event-unicode (sdl-event)
  (cffi:foreign-slot-value (cffi:foreign-slot-pointer sdl-event
                                                      'sdl-cffi::sdl-keyboard-event
                                                      'sdl-cffi::keysym)
                           'sdl-cffi::sdl-key-sym 'sdl-cffi::unicode))

(defun mouse-motion-event-state (sdl-event)
  (cffi:foreign-slot-value sdl-event 'sdl-cffi::SDL-Mouse-Motion-Event 'sdl-cffi::state))
(defun mouse-motion-event-x (sdl-event)
  (cffi:foreign-slot-value sdl-event 'sdl-cffi::SDL-Mouse-Motion-Event 'sdl-cffi::x))
(defun mouse-motion-event-y (sdl-event)
  (cffi:foreign-slot-value sdl-event 'sdl-cffi::SDL-Mouse-Motion-Event 'sdl-cffi::y))
(defun mouse-motion-event-x-rel (sdl-event)
  (cffi:foreign-slot-value sdl-event 'sdl-cffi::SDL-Mouse-Motion-Event 'sdl-cffi::xrel))
(defun mouse-motion-event-y-rel (sdl-event)
  (cffi:foreign-slot-value sdl-event 'sdl-cffi::SDL-Mouse-Motion-Event 'sdl-cffi::yrel))

(defun mouse-button-event-button (sdl-event)
  (cffi:foreign-slot-value sdl-event 'sdl-cffi::Sdl-Mouse-Button-Event 'sdl-cffi::button))
(defun mouse-button-event-state (sdl-event)
  (cffi:foreign-slot-value sdl-event 'sdl-cffi::Sdl-Mouse-Button-Event 'sdl-cffi::state))
(defun mouse-button-event-x (sdl-event)
  (cffi:foreign-slot-value sdl-event 'sdl-cffi::Sdl-Mouse-Button-Event 'sdl-cffi::x))
(defun mouse-button-event-y (sdl-event)
  (cffi:foreign-slot-value sdl-event 'sdl-cffi::Sdl-Mouse-Button-Event 'sdl-cffi::y))

(defun joy-axis-motion-event-which (sdl-event)
  (cffi:foreign-slot-value sdl-event 'sdl-cffi::Sdl-Joy-Axis-Event 'sdl-cffi::which))
(defun joy-axis-motion-event-axis (sdl-event)
  (cffi:foreign-slot-value sdl-event 'sdl-cffi::Sdl-Joy-Axis-Event 'sdl-cffi::axis))
(defun joy-axis-motion-event-value (sdl-event)
  (cffi:foreign-slot-value sdl-event 'sdl-cffi::Sdl-Joy-Axis-Event 'sdl-cffi::value))

(defun joy-hat-motion-event-which (sdl-event)
  (cffi:foreign-slot-value sdl-event 'sdl-cffi::sdl-joy-hat-event 'sdl-cffi::which))
(defun joy-hat-motion-event-axis (sdl-event)
  (cffi:foreign-slot-value sdl-event 'sdl-cffi::sdl-joy-hat-event 'sdl-cffi::hat))
(defun joy-hat-motion-event-value (sdl-event)
  (cffi:foreign-slot-value sdl-event 'sdl-cffi::sdl-joy-hat-event 'sdl-cffi::value))

(defun joy-ball-motion-event-which (sdl-event)
  (cffi:foreign-slot-value sdl-event 'sdl-cffi::Sdl-Joy-Ball-Event 'sdl-cffi::which))
(defun joy-ball-motion-event-ball (sdl-event)
  (cffi:foreign-slot-value sdl-event 'sdl-cffi::Sdl-Joy-Ball-Event 'sdl-cffi::ball))
(defun joy-ball-motion-event-x-rel (sdl-event)
  (cffi:foreign-slot-value sdl-event 'sdl-cffi::Sdl-Joy-Ball-Event 'sdl-cffi::xrel))
(defun joy-ball-motion-event-y-rel (sdl-event)
  (cffi:foreign-slot-value sdl-event 'sdl-cffi::Sdl-Joy-Ball-Event 'sdl-cffi::yrel))

(defun joy-button-event-which (sdl-event)
  (cffi:foreign-slot-value sdl-event 'sdl-cffi::Sdl-Joy-Button-Event 'sdl-cffi::which))
(defun joy-button-event-button (sdl-event)
  (cffi:foreign-slot-value sdl-event 'sdl-cffi::Sdl-Joy-Button-Event 'sdl-cffi::button))
(defun joy-button-event-state (sdl-event)
  (cffi:foreign-slot-value sdl-event 'sdl-cffi::Sdl-Joy-Button-Event 'sdl-cffi::state))


(defun user-event-type (sdl-event)
  (cffi:foreign-slot-value sdl-event 'sdl-cffi::Sdl-User-Event 'sdl-cffi::type))
(defun user-event-code (sdl-event)
  (cffi:foreign-slot-value sdl-event 'sdl-cffi::Sdl-User-Event 'sdl-cffi::code))
(defun user-event-data1 (sdl-event)
  (cffi:foreign-slot-value sdl-event 'sdl-cffi::Sdl-User-Event 'sdl-cffi::data1))
(defun user-event-data2 (sdl-event)
  (cffi:foreign-slot-value sdl-event 'sdl-cffi::Sdl-User-Event 'sdl-cffi::data2))


(defun video-resize-event-w (sdl-event)
  (cffi:foreign-slot-value sdl-event 'sdl-cffi::Sdl-Resize-Event 'sdl-cffi::w))
(defun video-resize-event-h (sdl-event)
  (cffi:foreign-slot-value sdl-event 'sdl-cffi::Sdl-Resize-Event 'sdl-cffi::h))



(cffi:defcallback event-filter
    :int ((sdl-event sdl-cffi::sdl-event))
  ;; The following events are necessary for input-util.lisp.
  (case (get-event-type sdl-event)
    (:key-down-event (handle-key-down (key-event-key sdl-event)))
    (:key-up-event (handle-key-up (key-event-key sdl-event)))
    (:mouse-button-down-event (handle-mouse-down (mouse-button-event-button sdl-event)))
    (:mouse-button-up-event (handle-mouse-up (mouse-button-event-button sdl-event))))

  ;; Handle any user-specified event filters.
  (if *event-filters*
    (let ((hook (gethash (cffi:foreign-slot-value sdl-event 'sdl-cffi::sdl-event 'sdl-cffi::type) *filter-event-hooks*)))
      (if hook
        (if (funcall hook sdl-event)
          1
          0)
        1))
    1))

(defun set-event-filter (event event-filter-hook)
  "Sets the callback function that handles the event filter. Return `NIL` if the event is to be removed
from the event queue. Return `T` if the event is to remain on the event queue.

`EVENT-FILTER-HOOK` must use the following template;

  \#'(LAMBDA (SDL-EVENT)
         \t\)

For example;
\(setf \(SDL:SET-EVENT-FILTER :QUIT-EVENT \#(LAMBDA \(EVENT\) t\))"
  (setf (gethash (cffi:foreign-enum-value 'sdl-cffi::Event-Type event)
                 *filter-event-hooks*) event-filter-hook))

(defun remove-event-filter (sdl-event)
  "Removes any existing event filters. that were set with `SET-EVENT-FILTERS`"
  (setf (gethash sdl-event *filter-event-hooks*) nil))

(defun remove-event-filters ()
  "Removes any existing event filters. that were set with `SET-EVENT-FILTERS`"
  (setf *filter-event-hooks* (make-hash-table)))

(defun enable-event-filters ()
  "Enables event filters."
  (setf *event-filters* t))

(defun disable-event-filters ()
  "Disables event filters."
  (setf *event-filters* nil))

(defun pump-events ()
  "Pumps the event loop, gathering events from the input devices. 
`PUMP-EVENTS` gathers all the pending input information from devices and places it on the event queue.
Without calls to SDL_PumpEvents no events would ever be placed on the queue.
Often the need for calls to SDL_PumpEvents is hidden from the user since
[SDL-POLL-EVENT](#sdl-poll-event) and [SDL-WAIT-EVENT](#sdl-wait-event) implicitly call `PUMP-EVENTS`.
However, if you are not polling or waiting for events (e.g. you are filtering them), then you must call
`PUMP-EVENTS` to force an event queue update.

Note: Only call this function in the thread that set the video mode."
  (lispbuilder-sdl-cffi::sdl-pump-events))

(defun new-event (&optional (event-type :NO-EVENT))
  "Creates a new `SDL_Event` and of `EVENT-TYPE`.
 An event of type `:NO-EVENT` is created if 
the `OPTIONAL` event type `EVENT-TYPE` is unspecified.

##### Example

    \(NEW-EVENT :QUIT-EVENT\)"
  (unless (cffi:foreign-enum-value 'sdl-cffi::EVENT-TYPE event-type :errorp nil)
    (error "NEW-EVENT: EVENT-TYPE ~A is not a valid SDL event. Must be one of
:NO-EVENT, :ACTIVE-EVENT, :KEY-DOWN-EVENT, :KEY-UP-EVENT, :MOUSE-MOTION-EVENT, 
:MOUSE-BUTTON-DOWN-EVENT, :MOUSE-BUTTON-UP-EVENT, :JOY-AXIS-MOTION-EVENT, :JOY-BALL-MOTION-EVENT, 
:JOY-HAT-MOTION-EVENT, :JOY-BUTTON-DOWN-EVENT, :JOY-BUTTON-UP-EVENT, :QUIT-EVENT, 
:SYS-WM-EVENT, :VIDEO-RESIZE-EVENT, :VIDEO-EXPOSE-EVENT, :USER-EVENT."
	   event-type))
  (let ((event (cffi:foreign-alloc 'sdl-cffi::sdl-event)))
    (setf (cffi:foreign-slot-value event 'sdl-cffi::SDL-event 'type) (cffi:foreign-enum-value 'sdl-cffi::EVENT-TYPE event-type))
    event))

(defun push-quit-event ()
  "Pushes a new `SDL_Event` of type `:QUIT-EVENT` onto the event queue."
  (sdl-cffi::SDL-Push-Event (new-event :quit-event)))

;;/* There are no functions directly affecting the quit event */
;;#define SDL_QuitRequested() \
;;        (SDL_PumpEvents(), SDL_PeepEvents(NULL,0,SDL_PEEKEVENT,SDL_QUITMASK))
(defun quit-requested-p ()
  "Returns the number of quit-events on the queue or `NIL` otherwise."
  (pump-events)
  (let ((events (sdl-cffi::sdl-peep-events (cffi:null-pointer) 0 :peek-event (sdl-cffi::sdl-quit-mask))))
    (if (= -1 events)
      nil
      events)))

(defun push-user-event (&key (code 0) (data1 nil) (data2 nil))
  "Pushes a new `SDL_Event` of type `:USER-EVENT` onto the event queue."
  (let ((event (new-event :USER-EVENT)))
    (setf (cffi:foreign-slot-value event 'sdl-cffi::SDL-user-event 'sdl-cffi::code) code
	  (cffi:foreign-slot-value event 'sdl-cffi::SDL-user-event 'sdl-cffi::data1) (cffi:convert-to-foreign data1 :pointer)
          (cffi:foreign-slot-value event 'sdl-cffi::SDL-user-event 'sdl-cffi::data2) (cffi:convert-to-foreign data2 :pointer))
    (sdl-cffi::SDL-Push-Event event)))

;;; Event Handling from here   -----------------------

(defun expand-event (sdl-event event-type event params forms)
  (let ((keyword-list nil))
    (do ((keyword params (if (cdr keyword)
                           (cddr keyword)
                           nil)))
	((null keyword))
      (push (list (first keyword) (second keyword)) keyword-list))
    `((event= ,sdl-event ,event-type)
      (let (,@(mapcar #'(lambda (key)
                          (when (gethash (first key) event)
                            `(,(second key)
                              (,(gethash (first key) event) ,sdl-event))))
                      keyword-list))
        ,@forms))))

(defun expand-quit (sdl-event forms quit)
  `((event= ,sdl-event :QUIT-EVENT)
    (setf ,quit (funcall #'(lambda ()
			     ,@forms)))))

(defun expand-idle (forms)
  `(progn
     ,@forms))

(defmacro with-events ((&optional (type :poll)) &body events)
  "`WITH-EVENTS` is a convenience macro for managing the main game loop. It processes 
incoming SDL events and limits the game loop to the specified number of frames 
per second.

Both the [SDL-POLL-EVENT](#sdl-poll-event) and [SDL-WAIT-EVENT](#sdl-wait-event) 
event mechanisms are supported by specifying the `TYPE` as `:POLL` or `:WAIT` 
respectively. 

*NOTE:* `WITH-EVENTS` must be called in the same thread used to set 
the video mode.

##### Example

    \(SDL:WITH-EVENTS \(:POLL\)
      \(:QUIT-EVENT \(\) T\)
      \(:KEY-DOWN-EVENT \(:KEY KEY\)
          \(WHEN \(SDL:KEY= KEY :SDL-KEY-ESCAPE\)
            \(SDL:PUSH-QUIT-EVENT\)\)\)
      \(:VIDEO-EXPOSE-EVENT \(\) \(SDL:UPDATE-DISPLAY\)\)\)\)\)\)

##### Frame Rate Limiting

The frame rate is specified using [FRAME-RATE](#frame-rate). For example to set the frame rate to 60 frames per second:

    \(SETF \(SDL:FRAME-RATE\) 60\)

##### Event Syntax

Events are specified using the format `(:EVENT-TYPE \(\&KEYS KEYS\)\)`

* `EVENT-TYPE` must be one of the following `KEY`words; `:ACTIVE-EVENT, 
:KEY-DOWN-EVENT, :KEY-UP-EVENT, :MOUSE-MOTION-EVENT, :MOUSE-BUTTON-DOWN-EVENT, 
:MOUSE-BUTTON-UP-EVENT, :JOY-AXIS-MOTION-EVENT, :JOY-BUTTON-DOWN-EVENT, :JOY-BUTTON-UP-EVENT, 
:JOY-HAT-MOTION-EVENT, :JOY-BALL-MOTION-EVENT, :VIDEO-RESIZE-EVENT, :VIDEO-EXPOSE-EVENT, 
:SYS-WM-EVENT, :QUIT-EVENT, :USER-EVENT` or `:IDLE`.
* `KEYS` specify the members of the event to return and are specific to each event type. 
These are discussed in detail below.

*NOTE:* `:QUIT-EVENT` must return `T` to exit the `WITH-EVENT` macro.

*NOTE:* `:IDLE` is ignored when `TYPE` is `:WAIT`.

##### Polling for Events

When `TYPE` is `:POLL`, `WITH-EVENTS` will continually poll for currently pending events. If no
events are available then the game loop is run and the forms in `:IDLE` are executed.

##### Waiting for Events

When `TYPE` is `:WAIT`, `WITH-EVENTS` will sleep indefinitely for the next available event. If no
events are available then the game loop is paused.

##### The :IDLE Event

     \(:IDLE \(\)
        &BODY BODY\)

The `:IDLE` event is special in that it is not generated by SDL. Rather the forms
 in `:IDLE` are executed once each game loop after event queue is emptied. `:IDLE` is ignored 
when the event mechanism specified by `TYPE` is `:WAIT`.

##### Active Event

    \(:ACTIVE-EVENT \(:GAIN GAIN :STATE STATE\) 
       &BODY BODY\)
 
When the mouse leaves or enters the window area an `SDL-APP-MOUSE-FOCUS` type activation event is generated.  
If the mouse has entered the window then `GAIN` will be `1`, otherwise `GAIN` will be `0`. 
An `SDL-APP-INPUT-FOCUS` type activation event occurs when the application loses or gains keyboard focus, 
usually when a different application is made active. 
Finally, an `SDL-APP-ACTIVE` type event occurs when the application is either minimised/iconified, `GAIN` is `0`, 
or restored. A single event can have multiple values set in `STATE`. 
*Note:* This event does not occur when an application window is first created.

* `GAIN` is `0` if the event is a loss or `1` if it is a gain.
* `STATE` a bitmask of the following values: `SDL-APP-MOUSE-FOCUS` if mouse focus was gained or lost, 
`SDL-APP-INPUT-FOCUS` if input focus was gained or lost, and 
`SDL-APP-ACTIVE` if the application was iconified, `GAIN` is `0`, or restored `GAIN` is `1`.

##### Keyboard Events

    \(:KEY-DOWN-EVENT \(:STATE STATE :SCANCODE SCANCODE :KEY KEY :MOD MOD :MOD-KEY MOD-KEY :UNICODE UNICODE\)
       &BODY BODY\)
    \(:KEY-UP-EVENT \(:STATE STATE :SCANCODE SCANCODE :KEY KEY :MOD MOD :MOD-KEY MOD-KEY :UNICODE UNICODE\)
       &BODY BODY\)

A keyboard event generally occurs when a key is released or when a key is pressed. 
The information on the key that generated the event is stored in `KEY` and `MOD`. 

The `SDL-CAPS-LOCK` and `SDL-NUM-LOCK` keys are special cases and report an 
`SDL-KEY-DOWN` when first pressed, then an `SDL-RELEASED` when released and pressed again. 
These keys KEYUP and KEYDOWN events are therefore analogous to the state of
 the caps lock and num lock LEDs rather than the keys themselves. These special
 cases are required for compatibility with Sun workstations.

*Note:* Repeating `SDL-KEY-DOWN` events will occur if key repeat is enabled using
[SDL-ENABLE-KEY-REPEAT\](#sdl-enable-key-repeat).

* `STATE` is `SDL-PRESSED` or `SDL-RELEASED` if the key is pressed or released respectively.
* `SCANCODE` is the hardware-dependent scancode returned by the keyboard.
* `KEY` is is the SDL-defined value of the key that generated the event. The SDL-defined
value for `KEY` generally takes the following format: `:SDL-KEY-0` to `:SDL-KEY-1` for numeric keys. 
`SDL-KEY-a` to `SDL-KEY-z` for alpha keys in the range a-z.
Other keys are generally spelled out, for example `SDL-KEY-PAGEDOWN`, `SDL-KEY-F1` or 
`SDL-KEY-NUMLOCK` .
* `MOD` is the keyboard modifier state returned as an `INTEGER`.
* `MOD-KEY` is the current state of the keyboard modifiers as explained in SDL_GetModState. 
Returned as a `LIST` of one or more of `:SDL-KEY-MOD-LSHIFT, :SDL-KEY-MOD-RSHIFT,
:SDL-KEY-MOD-LCTRL, :SDL-KEY-MOD-RCTRL, :SDL-KEY-MOD-LALT, :SDL-KEY-MOD-RALT,
:SDL-KEY-MOD-LMETA, :SDL-KEY-MOD-RMETA, :SDL-KEY-MOD-NUM, :SDL-KEY-MOD-CAPS,
:SDL-KEY-MOD-MODE or :SDL-KEY-MOD-RESERVED`.
* `UNICODE` is the translated character. The unicode field is only used when 
UNICODE translation is enabled with SDL_EnableUNICODE. If unicode is non-zero 
then this is the UNICODE character corresponding to the keypress. 
If the high 9 bits of the character are 0, then this maps to the equivalent 
ASCII character.

##### Mouse Motion Event

    \(:MOUSE-MOTION-EVENT \(:STATE STATE :X X :Y Y :X-REL X-REL :Y-REL Y-REL\)
       &BODY BODY\)

A `MOUSE-MOTION-EVENT` event occurs when the mouse moves within the 
application window or when [SDL-WARP-MOUSE\](#SDL-WARP-MOUSE) is called. 
Both the absolute `X` and `Y` and relative `X-REL` and `Y-REL` coordinates are 
reported along with the current button state `STATE`. The button state can 
be interpreted using [SDL-BUTTON\](#sdl-button), see
[SDL-GET-MOUSE-STATE\](#sdl-get-mouse-state). 

If the cursor is hidden using [SDL-SHOW-CURSOR\](#sdl-show-cursor) and the 
input is grabbed using [SDL-WM-GRAB-INPUT\](#sdl-wm-grab-input), then the mouse 
will give relative motion events even when the cursor reaches the edge of 
the screen. This is currently only implemented on Windows and Linux/Unix-alikes.

* `STATE` is the current button state.
* `X` is the `X` coordinates of the mouse
* `Y` is the `Y` coordinates of the mouse
* `X-REL` is the relative motion in the `X` direction
* `Y-REL` is the relative motion in the `Y` direction

##### Mouse Button Events

     \(:MOUSE-BUTTON-DOWN-EVENT \(:BUTTON BUTTON :STATE STATE :X X :Y Y\)
       &BODY BODY\)
     \(:MOUSE-BUTTON-UP-EVENT \(:BUTTON BUTTON :STATE STATE :X X :Y Y\)
       &BODY BODY\)

When a mouse button press or release is detected the number of the button
pressed (from 1 to 255, with 1 usually being the left button and 2 the right)
is placed into `BUTTON`, the position of the mouse when this event occured is
stored in the `X` and the `Y` fields. 

Mouse wheel events are reported as buttons 4 (up) and 5 (down). Two events
are generated i.e. a `SDL-MOUSE-BUTTON-DOWN` followed by a `SDL-MOUSE-BUTTON-UP`
event.

* `BUTTON` is the mouse button index which is one of `SDL-BUTTON-LEFT`,
`SDL-BUTTON-MIDDLE`, `SDL-BUTTON-RIGHT`, `SDL-BUTTON-WHEELUP` or `SDL-BUTTON-WHEELDOWN`.
* `STATE` is the state of the button which is `SDL-PRESSED` or `SDL-RELEASED`.
* `X` is the `X` coordinates of the mouse at press/release time.
* `Y` is the `Y` coordinates of the mouse at press/release time.

##### Joystick Motion Event

    \(:JOY-AXIS-MOTION-EVENT \(:WHICH WHICH :AXIS AXIS :VALUE VALUE\)
       &BODY BODY\)

A JOY-AXIS-MOTION-EVENT event occurs whenever a user moves an axis on the joystick.

* `WHICH` is the joystick device index. The index of the joystick that reported the event.
* `AXIS` is the joystick axis index
* `VALUE` is the current position of the axis (range: -32768 to 32767)

##### Joystick Button Events

    \(:JOY-BUTTON-DOWN-EVENT \(:WHICH WHICH :BUTTON BUTTON :STATE STATE\)
       &BODY BODY\)
    \(:JOY-BUTTON-UP-EVENT (:WHICH WHICH :BUTTON BUTTON :STATE STATE\)
       &BODY BODY\)

A `JOY-BUTTON-DOWN-EVENT` or `JOY-BUTTON-DOWN-EVENT` event occurs whenever a
user presses or releases a button on a joystick.   

* `WHICH` is the index of the joystick that reported the event.
* `BUTTON` is the button pressed that caused the event.
* `STATE` is the current state of the button and is either
`SDL-PRESSED` or `SDL-RELEASED`.

##### Joystick Hat Motion Event

       \(:JOY-HAT-MOTION-EVENT \(:WHICH WHICH :HAT HAT :VALUE VALUE\)
         &BODY BODY\)

A `JOY-HAT-MOTION-EVENT` event occurs when ever a user moves a hat on the joystick.

* `WHICH` is the index of the joystick that reported the event.
* `HAT` is the index of the hat that generated the event.
* `VALUE` is the current position of the hat, a bitwise OR'd combination 
of the following values `SDL-HAT-CENTERED`, `SDL-HAT-UP`, `SDL-HAT-RIGHT`,
`SDL-HAT-DOWN`, `SDL-HAT-LEFT`, `SDL-HAT-RIGHT-UP`, `SDL-HAT-RIGHT-DOWN`, 
`SDL-HAT-LEFT-UP` and `SDL-HAT-LEFT-DOWN`.

##### Joystick Ball Motion Event

    \(:JOY-BALL-MOTION-EVENT \(:WHICH WHICH :BALL BALL :X-REL X-REL :Y-REL Y-REL\)
      &BODY BODY\)

A `JOY-BALL-MOTION-EVENT` event occurs when a user moves a trackball on the joystick.
Trackballs only return relative motion.

* `WHICH` is the index of the joystick that reported the event.
* `BALL` is the index of the trackball that generated the event.
* `X-REL` is the change in `X` position of the ball since it was last polled
\(last cycle of the event loop\).
* `Y-REL` is the change in `Y` position of the ball since it was last polled
\(last cycle of the event loop\).

##### Quit Event

    \(:QUIT-EVENT \(\) 
       &BODY BODY\)

If `QUIT-EVENT` is filtered or ignored then it is impossible for the user to
close the window. If `QUIT-EVENT` is accepted and returns `T` then the
application window will be closed. *Note:* Screen updates will continue to report success 
even though the application is no longer visible. If `QUIT-EVENT` is accepted and
returns `NIL` then the application window will _not_ be closed.
[QUIT-REQUESTED\](#quit-requested) will return non-zero if a `QUIT-EVENT`
event is pending.

##### SDL Window Resize Event

    \(:VIDEO-RESIZE-EVENT \(:W W :H H\)
       ...\)

When `SDL-RESIZABLE` is passed as a flag to [WINDOW\](#window), the user is allowed 
to resize the application window. When the window is resized a `VIDEO-RESIZE-EVENT`
event is reported, with the new window width and height values stored in `W` and `H`
respectively. When an `VIDEO-RESIZE-EVENT` event is recieved the window should
be resized to the new dimensions using [WINDOW\](#window).

* `W` is the window width as an `INTEGER`.
* `H` is the window height as an INTERGER`.

##### SDL Window Expose Event
      
    \(:VIDEO-EXPOSE-EVENT \(\)
       ...\)

`VIDEO-EXPOSE-EVENT` is triggered when the screen has been modified outside of
the application, usually by the window manager, and needs to be redrawn.
 
##### System Window Events

    \(:SYS-WM-EVENT \(\) 
       ...\)

The system window manager event contains a pointer to system-specific information
about unknown window manager events. If this event is enabled using
[SDL-EVENT-STATE\](#sdl-event-state), it will be generated whenever
unhandled events are received from the window manager. This can be used,
for example, to implement cut-and-paste in your application. If you want to
obtain system-specific information about the window manager, you can fill in
the version member of a `SDL-SYS-WM-INFO` structure
using [SDL-VERSION\](#sdl-version), and pass it to the function: 
[SDL-GET-WM-INFO\](#sdl-get-wm-info)

##### User

    \(:USER-EVENT \(:TYPE TYPE :CODE CODE :DATA1 DATA1 :DATA2 DATA2\)
       ...\)

`USER-EVENT` is unique in that it is created by the user not SDL.
`USER-EVENT` can be pushed onto the event queue using [PUSH-USER-EVENT\](#push-user-event).
The contents of the event are completely up to the programmer. 

* `TYPE` is  a value from `SDL-USER-EVENT to `\(- SDL-NUM-EVENTS 1\)` inclusive.
* `CODE` is a user defined event code
* `DATA1` is a user defined data pointer
* `DATA2` is a user defined data pointer

##### Syntax

    \(WITH-EVENTS \(TYPE\)
     \(:ACTIVE-EVENT \(:GAIN GAIN :STATE STATE\) 
        ... \)
     \(:KEY-DOWN-EVENT \(:STATE STATE :SCANCODE SCANCODE :KEY KEY :MOD MOD :MOD-KEY MOD-KEY :UNICODE UNICODE\)
        ... \)
     \(:KEY-UP-EVENT \(:STATE STATE :SCANCODE SCANCODE :KEY KEY :MOD MOD :MOD-KEY MOD-KEY :UNICODE UNICODE\)
        ...\)
     \(:MOUSE-MOTION-EVENT \(:STATE STATE :X X :Y Y :X-REL X-REL :Y-REL Y-REL\)
        ...\)
     \(:MOUSE-BUTTON-DOWN-EVENT \(:BUTTON BUTTON :STATE STATE :X X :Y Y\)
        ...\)
     \(:MOUSE-BUTTON-UP-EVENT \(:BUTTON BUTTON :STATE STATE :X X :Y Y\)
        ...\)
     \(:JOY-AXIS-MOTION-EVENT \(:WHICH WHICH :AXIS AXIS :VALUE VALUE\)
        ...\)
     \(:JOY-BUTTON-DOWN-EVENT \(:WHICH WHICH :BUTTON BUTTON :STATE STATE\)
        ...\)
     \(:JOY-BUTTON-UP-EVENT (:WHICH WHICH :BUTTON BUTTON :STATE STATE\)
        ...\)
     \(:JOY-HAT-MOTION-EVENT \(:WHICH WHICH :HAT HAT :VALUE VALUE\)
        ...\)
     \(:JOY-BALL-MOTION-EVENT \(:WHICH WHICH :BALL BALL :X-REL X-REL :Y-REL Y-REL\)
        ...\)
     \(:VIDEO-RESIZE-EVENT \(:W W :H H\)
        ...\)
     \(:VIDEO-EXPOSE-EVENT \(\)
        ...\)
     \(:SYS-WM-EVENT \(\) 
        ...\)
     \(:USER-EVENT \(:TYPE TYPE :CODE CODE :DATA1 DATA1 :DATA2 DATA2\)
        ...\)
     \(:QUIT-EVENT \(\) 
        ... 
        T\)
     \(:IDLE \(\)
        ... \)\)"
  (let ((quit (gensym "quit")) (sdl-event (gensym "sdl-event"))
	(idle-func (gensym "idle-func")))
    `(let ((,sdl-event (new-event))
           (,quit nil)
           (,idle-func nil))
       (setf ,idle-func #'(lambda ()
                            ,@(remove nil 
                                      (mapcar #'(lambda (event)
                                                  (cond
                                                   ((eql :idle (first event))
                                                    (expand-idle (rest event)))))
                                              events))))
       
       (loop until ,quit do
             (loop until ,(case type
                            (:poll `(= 0 (sdl-cffi::SDL-Poll-Event ,sdl-event)))
                            (:wait `(or ,quit (= 0 (sdl-cffi::SDL-Wait-Event ,sdl-event))))
                            (otherwise (error "WITH-EVENTS: TYPE ~A, must be :POLL or :WAIT." type))) do
                   (cond
                    ,@(remove nil 
                              (mapcar #'(lambda (event)
                                          (if (eq (first event) :quit-event)
                                            (expand-quit sdl-event 
                                                         (rest (rest event))
                                                         quit)
                                            (when (gethash (first event) *events*)
                                              (expand-event sdl-event
                                                            (first event)
                                                            (gethash (first event) *events*)
                                                            (first (rest event))
                                                            (rest (rest event))))))
                                      events))))
             (unless ,quit
               #+lispbuilder-sdl-audio(process-audio)
               (sdl:update-input-util (sdl:frame-time))
               (sdl-base::process-timestep sdl-base::*default-fpsmanager*
                                           ,idle-func)))
       (cffi:foreign-free ,sdl-event))))
