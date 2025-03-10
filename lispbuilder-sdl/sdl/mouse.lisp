
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

(defun create-cursor (cursor-shape &key (hot-x 0) (hot-y 0))
  "Creates an SDL hardware cursor
CURSOR-SHAPE: Must be a grid with width and height being the same and multiple of 8.
    The cursor sequence (list or vector) should contain numbers:
    0 = Transparent
    1 = white
    2 = black
    3 = inverted (if supported)"
  (multiple-value-bind (data mask size)
      (decode-cursor cursor-shape (truncate (sqrt (length cursor-shape))))
    (let ((cursor (sdl-cffi::sdl-create-cursor data mask size size hot-x hot-y)))
      (cffi:foreign-free data)
      (cffi:foreign-free mask)
      cursor)))

(defun set-cursor (cursor)
  (sdl-cffi::sdl-set-cursor cursor))

(defun free-cursor (cursor)
  (sdl-cffi::sdl-free-cursor cursor))

;;; Helpers for create-cursor

(defun shift-left (byte &optional (shift-count 1))
  "Shifts all bits of the byte to the left by shift-count."
  (ash byte shift-count))

(defun decode-cursor (image size)
  "Translate a CL sequence into data/mask pointers (uint8) for C.
This is to create the mouse cursor.
IMAGE: must be multiples of 8 (8, 16, 32, 64)"
  (let* ((fdata (cffi:foreign-alloc :uint8 :count (* size 4)))
         (fmask (cffi:foreign-alloc :uint8 :count (* size 4))) 
         (image-bit 0)
         (bit -1))
    (when (/= (mod size 8) 0)
      (cffi:foreign-free fdata)
      (cffi:foreign-free fmask)
      (error "the provided pixle-list must be multiples of 8!"))
    
    (loop for row from 0 below size do
      (loop for col from 0 below size do
        (if (/= (mod col 8) 0)
            (progn
              (setf #1=(cffi:mem-aref fdata :uint8 bit)
		    (shift-left #1#))
              (setf #2=(cffi:mem-aref fmask :uint8 bit)
                    (shift-left #2#)))
	    
            (progn
              (incf bit)
              (setf #1# 0)
              (setf #2# 0)))
	
        
        (case (elt image image-bit)
	  ;; White
          (1
           (setf #2# (logior (cffi:mem-aref fmask :uint8 bit) #x01)))
	  ;; Black
          (2
	   (cond ((setf #1# (logior (cffi:mem-aref fdata :uint8 bit) #x01))
		  (setf #2# (logior (cffi:mem-aref fmask :uint8 bit) #x01)))))
	  ;; Transparent
	  (3
	   (setf #1# (logior #1# #x01))))

	;; Increment for the next bit-information
	(incf image-bit)))
    
    ;; End row loop
    (values fdata fmask size)))
