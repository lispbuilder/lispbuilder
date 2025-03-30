
(in-package #:lispbuilder-sdl)

(defvar mouse-left       lispbuilder-sdl-cffi::SDL-BUTTON-LEFT)
(defvar mouse-middle     lispbuilder-sdl-cffi::SDL-BUTTON-MIDDLE)
(defvar mouse-right      lispbuilder-sdl-cffi::SDL-BUTTON-RIGHT)
(defvar mouse-wheel-up   lispbuilder-sdl-cffi::SDL-BUTTON-WHEEL-UP)
(defvar mouse-wheel-down lispbuilder-sdl-cffi::SDL-BUTTON-WHEEL-DOWN)
(defvar mouse-x1         LISPBUILDER-SDL-CFFI::SDL-BUTTON-X1)
(defvar mouse-x2         LISPBUILDER-SDL-CFFI::SDL-BUTTON-X2)

(defun get-mouse-status ()
  (cffi:with-foreign-objects ((x :int) (y :int))
    (let ((mouse-button (sdl-cffi::sdl-get-mouse-state x y)))
      (declare (ignorable mouse-button))
      (values (cffi:mem-aref x :int)
              (cffi:mem-aref y :int)
              mouse-button))))

(defun get-relative-mouse-status ()
  (cffi:with-foreign-objects ((x :int) (y :int))
    (let ((mouse-button (sdl-cffi::sdl-get-relative-mouse-state x y)))
      (declare (ignorable mouse-button))
      (values (cffi:mem-aref x :int)
              (cffi:mem-aref y :int)
              mouse-button))))

(defun get-mouse-button ()
  (multiple-value-bind (x y button)
      (get-mouse-status)
    (declare (ignore x y))
    button))

(defun mouse-x ()
  "Returns the absolute mouse x curser position."
  (multiple-value-bind (x)
      (get-mouse-status)
    x))

(defun mouse-y ()
  "Returns the absolute mouse y curser position."
  (multiple-value-bind (x y)
      (get-mouse-status)
    (declare (ignore x))
    y))

(defun mouse-position ()
  "Returns the absolute mouse x and y curser position as a `VECTOR`."
  (multiple-value-bind (x y)
      (get-mouse-status)
    (vector x y)))

(defun mouse-relative-position ()
  "Returns the relative mouse x and y curser position since the last call to
`MOUSE-RELATIVE-POSITION`."
  (multiple-value-bind (x y)
      (get-relative-mouse-status)
    (vector x y)))

(defun mouse-buttons (&optional (button (get-mouse-button)))
  "Returns a list of the currently depressed mouse buttons."
  (let ((button-status nil))
    (when (/= 0 (logand button (sdl-cffi::sdl-button-lmask)))
      (push :button-left button-status))
    (when (/= 0 (logand button (sdl-cffi::sdl-button-rmask)))
      (push :button-right button-status))
    (when (/= 0 (logand button (sdl-cffi::sdl-button-mmask)))
      (push :button-middle button-status))
    (when (/= 0 (logand button (sdl-cffi::sdl-button-wumask)))
      (push :button-wheel-up button-status))
    (when (/= 0 (logand button (sdl-cffi::sdl-button-wdmask)))
      (push :button-wheel-down button-status))
    (when (/= 0 (logand button (sdl-cffi::sdl-button-x1mask)))
      (push :button-x1 button-status))
    (when (/= 0 (logand button (sdl-cffi::sdl-button-x2mask)))
      (push :button-x2 button-status))
    button-status))

(defun mouse-left-p (&optional (button (get-mouse-button)))
  "Returns `T` when the left mouse button is depressed."
  (/= 0 (logand button (sdl-cffi::sdl-button-lmask))))

(defun mouse-right-p (&optional (button (get-mouse-button)))
  "Returns `T` when the right mouse button is depressed."
  (/= 0 (logand button (sdl-cffi::sdl-button-rmask))))

(defun mouse-middle-p (&optional (button (get-mouse-button)))
  "Returns `T` when the middle mouse button is depressed."
  (/= 0 (logand button (sdl-cffi::sdl-button-mmask))))

(defun mouse-wheel-up-p (&optional (button (get-mouse-button)))
  "Returns `T` when the mouse wheel has been moved up."
  (/= 0 (logand button (sdl-cffi::sdl-button-wumask))))

(defun mouse-wheel-down-p (&optional (button (get-mouse-button)))
  "Returns `T` when the mouse wheel has been moved down."
  (/= 0 (logand button (sdl-cffi::sdl-button-wdmask))))

(defun mouse-x1-p (&optional (button (get-mouse-button)))
  "Returns `T` when the X1 mouse button is depressed."
  (/= 0 (logand button (sdl-cffi::sdl-button-x1mask))))

(defun mouse-x2-p (&optional (button (get-mouse-button)))
  "Returns `T` when the X2 mouse button is depressed."
  (/= 0 (logand button (sdl-cffi::sdl-button-x2mask))))

(defun mouse-warp (&key (x (mouse-x)) (y (mouse-y)))
  "Moves the mouse cursor to the supplied X/Y position."
  (sdl-cffi::sdl-warp-mouse x y))


(defun mouse-create-cursor (cursor-shape &key (hot-x 0) (hot-y 0))
  "Creates an SDL hardware cursor
CURSOR-SHAPE: Must be a grid with width and height being the same and multiple of 8.
    The cursor sequence (list or vector) should contain numbers:
    0 = Transparent
    1 = white
    2 = black
    3 = inverted (if supported)
HOT-X/Y: The upper-left corner of the cursor pixels.
Returns cursor as `FOREIGN-OBJECT`."
  (multiple-value-bind (data mask size)
      (decode-cursor cursor-shape (multiples-of-8? cursor-shape))
    (let ((cursor (make-instance 'foreign-object :fp (sdl-cffi::sdl-create-cursor data mask size size hot-x hot-y)
				 :free 'sdl-cffi::sdl-free-cursor)))
      (cffi:foreign-free data)
      (cffi:foreign-free mask)
      (sdl-cffi::sdl-set-cursor (fp cursor))
      cursor)))

;;; Helpers for mouse-cursor

(defun multiples-of-8? (cursor-shape)
  "Check if the square root of the cursor-shape's length is indeed multiple of 8.
Returns said square root as an `INTEGER`."
  (let ((grid-size (truncate (sqrt (length cursor-shape)))))
    (if (= (mod grid-size 8) 0)
	grid-size
	(error "the provided pixel-list must be multiples of 8!"))))

(defun shift-left (byte &optional (shift-count 1))
  "Returns the byte shifted left by shift-count positions as an `INTEGER`."
  (ash byte shift-count))


(defun decode-body (col byte fdata fmask)
  "Processes the next bit in the BYTE, updating the foreign FDATA and FMASK pointers.
returns the byte as `INTEGER`"
  (if (/= (mod col 8) 0)
      (progn
	(setf #1=(cffi:mem-aref fmask :uint8 byte) (shift-left #1#))
	(setf #2=(cffi:mem-aref fdata :uint8 byte) (shift-left #2#)))
      
      (progn
	(incf byte)
	(setf #1# 0
	      #2# 0)))
  ;; Returns the byte index
  byte)


(defun decode-case (fdata fmask byte pixel)
  "Check what the current pixel (from the cursor-image sequence) is, then set the bit in the array(s)."
  (case pixel
    ;; White
    (1
     (setf #1=(cffi:mem-aref fmask :uint8 byte) (logior #1# #x01)))
    ;; Black
    (2
     (cond ((setf #2=(cffi:mem-aref fdata :uint8 byte) (logior #2# #x01))
	    (setf #1# (logior #1# #x01)))))
    ;; Inverted
    (3
     (setf #2# (logior #2# #x01)))))

(defun decode-cursor (image size)
  "Decodes a cursor shape of pixel sequence into SDL-compatible data and mask bitmaps.
Returns three values: The DATA bitmap `UINT8-ARRAY`, The MASK bitmap `UINT8-ARRAY`,
  and the cursor's grid size as `INTEGER`"
  (let* ((fdata (cffi:foreign-alloc :uint8 :count (* size 4)))
         (fmask (cffi:foreign-alloc :uint8 :count (* size 4))) 
         (image-pixel 0)
         (byte -1))
    (loop for row from 0 below size do
      (loop for col from 0 below size do
        (setf byte (decode-body col byte fdata fmask))
	
        (decode-case fdata fmask byte (elt image image-pixel))
	
	;; Increment for the next bit-information
	(incf image-pixel)))
    (values fdata fmask size)))
