
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
  (cffi:with-foreign-object (num-keys :int)
      (let ((key-states (sdl-cffi::SDL-Get-Key-state num-keys)))
	(if (= (mem-aref key-states :uint8 (cffi:foreign-enum-value 'sdl-cffi::sdl-key key))
               1)
	    t
	    nil))))

(defun get-keys-state ()
  (cffi:with-foreign-object (num-keys :int)
    (let ((key-states (sdl-cffi::SDL-Get-Key-state num-keys)))
      (remove nil (loop for key in (cffi:foreign-enum-keyword-list 'sdl-cffi::sdl-key)
                        collect (when (= (mem-aref key-states :uint8 (cffi:foreign-enum-value 'sdl-cffi::sdl-key
                                                                                              key))
                                         1)
                                  key))))))

(defun key-state-p (&optional key)
  "Returns all keys that are currently depressed as a LIST. 
Returns KEY if KEY is currently depressed, returns NIL otherwise.
Note: Use SDL_PumpEvents to update the state array.
Note: This function gives you the current state after all events have been processed, 
so if a key or button has been pressed and released before you process events, 
then the pressed state will never show up in the getstate calls.
Note: This function doesn't take into account whether shift is currently depressed.
For example: \(KEY-STATE-P :SDL-KEY-F1\)"
  (if key
    (get-key-state key)
    (get-keys-state)))

(defun key-down-p (key)
  "Returns `KEY` if currently depressed, returns NIL otherwise.
Note: Use SDL_PumpEvents to update the state array.
Note: This function returns the current state after all events have been processed, 
so if a key or button has been pressed and released before you process events, 
then the pressed state will never show up in the getstate calls.
Note: This function doesn't take into account whether shift is currently depressed.
For example: \(KEY-DOWN-P :SDL-KEY-F1\)"
  (get-key-state key))

(defun keys-down-p ()
  "Returns all keys that are currently depressed as a LIST. 
Note: Use SDL_PumpEvents to update the state array.
Note: This function returns the current state after all events have been processed, 
so if a key or button has been pressed and released before you process events, 
then the pressed state will never show up in the getstate calls.
Note: This function doesn't take into account whether shift is currently depressed.
For example: \(KEYS-DOWN-P\)"
  (get-keys-state))

(defun get-mod-state (key)
  (if (> (logand (sdl-cffi:sdl-get-mod-state)
                 (cffi:foreign-enum-value 'sdl-cffi::sdl-mod key))
         0)
    key
    nil))

(defun get-mods-state ()
  (let ((mod-state (sdl-cffi::SDL-Get-mod-state)))
    (remove nil (loop for key in (cffi:foreign-enum-keyword-list 'sdl-cffi::sdl-mod)
                      collect (when (> (logand mod-state (cffi:foreign-enum-value 'sdl-cffi::sdl-mod key)) 0)
                                key)))))

(defun mod-state-p (&optional key)
  "Returns all modifier keys that are currently depressed as a LIST. 
Returns KEY if KEY is currently depressed, returns NIL otherwise.
Note: Use SDL_PumpEvents to update the state array.
Note: This function gives you the current state after all events have been processed, 
so if a key or button has been pressed and released before you process events, 
then the pressed state will never show up in the getstate calls.
For example: \(GET-MOD-STATE :SDL-KEY-MOD-LCTRL)"
  (if key
    (get-mod-state key)
    (get-mods-state)))

(defun mod-down-p (mod)
  "Returns `MOD` if currently depressed, returns NIL otherwise.
Note: Use SDL_PumpEvents to update the state array.
Note: This function gives you the current state after all events have been processed, 
so if a key or button has been pressed and released before you process events, 
then the pressed state will never show up in the getstate calls.
For example: \(MOD-DOWN-P :SDL-KEY-MOD-LCTRL\)"
  (get-mod-state mod))

(defun mods-down-p ()
  "Returns all modifier keys that are currently depressed as a LIST. 
Note: Use SDL_PumpEvents to update the state array.
Note: This function gives you the current state after all events have been processed, 
so if a key or button has been pressed and released before you process events, 
then the pressed state will never show up in the getstate calls.
For example: \(MODS-DOWN-P\)"
  (get-mods-state))

(defun key= (key1 key2)
  (eq key1 key2))

(defgeneric modifier= (mod key)
  (:documentation "Returns `MOD` only when all of the modifiers in `MOD` exactly match `KEY`, and
returns `NIL` otherwise. `MOD` may be a list of modifiers, or a single modifier.

##### Example

      \(:KEY-DOWN-EVENT \(:MOD MOD\)
          \(SDL:MODIFIER= '(:SDL-KEY-MOD-LCTRL :SDL-KEY-MOD-LALT) MOD\)\)"))
(defmethod modifier= (modifiers (key list))
  ;; Make modifiers a list, if not already.
  (let ((modifiers (if (listp modifiers) modifiers (list modifiers))))
    (if (= (length (intersection modifiers key))
           (length modifiers))
      modifiers
      nil)))

(defmethod modifier= (modifiers key)
  ;; Make modifiers a list, if not already.
  (let* ((key (modifier-p key modifiers)))
    (modifier= modifiers key)))

(defgeneric modifier-p (key &optional modifiers)
  (:documentation "Returns a list of the modifiers in `KEY` that match `MODIFIERS`,
or `NIL` if no modifiers match. By default, `MODIFIERS` is the list of valid modifiers.

##### Example 1

      \(:KEY-DOWN-EVENT \(:MOD MOD\)
          \(SDL:MODIFIER-P MOD '(:SDL-KEY-MOD-LCTRL :SDL-KEY-MOD-LALT)\)\)

##### Example 2

      \(:KEY-DOWN-EVENT \(:MOD MOD\)
          \(SDL:MODIFIER-P MOD\)\)"))
(defmethod modifier-p ((key list) &optional
                       (modifiers (cffi:foreign-enum-keyword-list 'sdl-cffi::sdl-mod)))
  ;; Make mod a list, if not already.
  (unless (listp modifiers)
    (setf modifiers (list modifiers)))
  (intersection key modifiers))

(defmethod modifier-p (key &optional
                           (modifiers (cffi:foreign-enum-keyword-list 'sdl-cffi::sdl-mod)))
  ;; Make mod a list, if not already.
  (unless (listp modifiers)
    (setf modifiers (list modifiers)))
  (remove nil
          (loop for modifier in modifiers
                collect (when (or (/= (logand (cffi:foreign-enum-value 'sdl-cffi::SDL-Mod modifier)
                                              key)
                                      0)
                                  ;; Check for :SDL-KEY-MOD-NONE
                                  (= (cffi:foreign-enum-value 'sdl-cffi::SDL-Mod modifier) key))
                          modifier))))

