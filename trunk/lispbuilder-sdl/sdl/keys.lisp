
(in-package #:lispbuilder-sdl)

(defun enable-unicode-p ()
  "Queries the current state of Unicode keyboard translation. Returns T if enabled, NIL if disabled."
  (if (equal (sdl-cffi::SDL-Enable-UNICODE -1) 1)
      t
      nil))

(defun enable-unicode (state)
  "Unicode translation is enabled with STATE is T, and disabled when STATE is NIL.
To obtain the character codes corresponding to received keyboard events, Unicode translation must first be turned on 
using this function. The translation incurs a slight overhead for each keyboard event and is therefore disabled by default. 
For each subsequently received key down event, the unicode member of the SDL_keysym structure will then contain 
the corresponding character code, or zero for keysyms that do not correspond to any character code.
Note that only key press events will be translated, not release events.
Returns the previous unicode translation state."
  (if (equal (sdl-cffi::SDL-Enable-UNICODE (if state 1 0)) 1)
      t
      nil))

(defun enable-key-repeat (delay interval)
  "Enables the keyboard repeat rate. DELAY specifies how long the key must be pressed before it begins repeating, 
it then repeats at the speed specified by INTERVAL. Both DELAY and INTERVAL are expressed in milliseconds.
Setting DELAY or INTERVAL to NIL will set the default values of 
SDL-DEFAULT-REPEAT-DELAY and SDL-DEFAULT-REPEAT-INTERVAL respectively.
_NOTE_: `ENABLE-KEY-REPEAT` must be called after the SDL library has been
initialized to have an effect."
  (unless delay
    (setf delay sdl-cffi::SDL-DEFAULT-REPEAT-DELAY))
  (unless interval
    (setf interval sdl-cffi::SDL-DEFAULT-REPEAT-INTERVAL))
  (if (equal (sdl-cffi::SDL-Enable-Key-Repeat delay interval) 0)
      t
      nil))

(defun disable-key-repeat ()
  "Disables keyboard repeat."
  (if (equal (sdl-cffi::SDL-Enable-Key-Repeat 0 0) 0)
      t
      nil))

(defun enable-key-repeat-p ()
  "Returns the current keyboard DELAY and INTERVAL repeat rate in milliseconds as \(VALUES DELAY INTERVAL\)."
  (let ((delay 0) (interval 0))
    (cffi:with-foreign-objects ((fp-delay :int)
				(fp-interval :int))
      (sdl-cffi::SDL-Get-Key-Repeat fp-delay fp-interval)
      (setf delay (cffi:mem-aref fp-delay :int) 
	    interval (cffi:mem-aref fp-interval :int)))
    (values delay interval)))

(defun key-repeat-delay ()
  "Returns the current key repeat delay, in milliseconds."
  (enable-key-repeat-p))

(defun key-repeat-interval ()
    "Returns the current key repeat interval, in milliseconds."
  (multiple-value-bind (delay interval)    
      (enable-key-repeat-p)
    (declare (ignore delay))
    interval))

(defun get-key-state (key)
  "Returns the current keypress state of the key KEY.
Returns T if the SDL-KEY is pressed, returns NIL if SDL-KEY is not pressed.
Note: Use SDL_PumpEvents to update the state array.
Note: This function gives you the current state after all events have been processed, 
so if a key or button has been pressed and released before you process events, 
then the pressed state will never show up in the getstate calls.
Note: This function doesn't take into account whether shift has been pressed or not.
For example: \(GET-KEY-STATE :SDL-KEY-F1\)"
  (cffi:with-foreign-object (num-keys :int)
      (let ((key-states (sdl-cffi::SDL-Get-Key-state num-keys)))
	(if (equal (mem-aref key-states :uint8 (foreign-enum-value 'sdl-cffi::sdl-key key)) 
		   1)
	    t
	    nil))))

(defun key= (key1 key2)
  (eq key1 key2))

(defun modifier= (mod key)
  "Returns `MOD` if all of the modifiers in `MOD` exactly match `KEY`, or
returns `NIL` otherwise. `MOD` may be a list of modifiers, or a single modifier."
  ;; Make mod a list, if not already.
  (when (and (> (length (modifier-p key)) 0)
             (= (length (modifier-p key))
                (length (modifier-p key mod))))
    mod))

(defun modifier-p (key &optional
                       (mods (cffi:foreign-enum-keyword-list 'sdl-cffi::sdl-mod)))
  "Returns a list of the modifiers in `KEY` that match `MODS`,
or `NIL` if no modifiers match.
By default, `MODS` is the list of valid modifiers."
  ;; Make mod a list, if not already.
  (unless (listp mods)
    (setf mods (list mods)))
  (remove nil
          (loop for modifier in mods
                collect (when (/= (logand
                                   (cffi:foreign-enum-value 'sdl-cffi::SDL-Mod modifier)
                                   key)
                                  0)
                          modifier))))

(defun modifier-in (mod key)
  "Returns `MOD` if one or more of the modifiers in `MOD` is in `KEY`,
or returns `NIL otherwise. `MOD` may be a list of modifiers, or a single modifier."
  (modifier-p key mod))
