(in-package #:lispbuilder-sdl-base)

(defgeneric (setf target-frame-rate) (rate fpsmngr)
  (:documentation "Set the target frame rate for the game loop.
RATE > 0 will lock the game loop to the specified frame rate, and
calculate the average frame rate over a number of frames.
RATE = 0 will unlock the frame rate, and calculate the average
frame rate over a number of frames.
RATE < 0 will unlock the frame rate. The average frane rate is 
not calculated"))

(defgeneric process-timestep (fpsmngr &optional fn)
  (:documentation "Manages the timestep. Called once per game loop."))

(defclass fps-average ()
  ((index :accessor index :initform 0)
   (window :accessor average-window :initform nil)
   (calculated :accessor average-fps :initform 0)))

(defmethod update-fps-window ((self fps-average) current-ticks)
  (with-accessors ((index index) (average-window average-window)
                   (average-fps average-fps)) self
    (if (< index (length average-window))
      (progn
        (setf (svref average-window index) current-ticks)
        (incf index))
      (setf index 0
            average-fps (/ 1000
                           (/ (loop for i from 1 below (length average-window)
                                    for j from 0 below (length average-window)
                                    summing (- (svref average-window i)
                                               (svref average-window j)) into total
                                    finally (return total))
                              (length average-window)))))))

(defclass time-scale ()
  ((tscale :accessor tscale :initform 0)
   (world :accessor world :initform 1000)))

(defun calculate-time-scale (time-scale delta-ticks)
  (with-slots (tscale world) time-scale
    (setf tscale (/ delta-ticks world))))

(defclass fps-manager ()
  ((time-scale :reader time-scale :initform (make-instance 'time-scale))
   (fps-average :reader fps-average :initform (make-instance 'fps-average))
   (current-ticks :accessor current-ticks :initform (sdl-cffi::sdl-get-ticks))
   (last-ticks :accessor last-ticks :initform (sdl-cffi::sdl-get-ticks))
   (delta-ticks :accessor delta-ticks :initform 0))
  (:default-initargs
   :average-window 200
   :target-frame-rate 30))

(defclass fps-fixed (fps-manager)
  ((target-frame-rate :reader target-frame-rate)
   (frame-count :accessor frame-count :initform 0 :initarg :frame-count)
   (rate-ticks :accessor rate-ticks)
   (delay-ticks :accessor delay-ticks :initform (sdl-cffi::sdl-get-ticks))
   (upper-limit :accessor upper-limit :initform 1000 :initarg :upper-limit)
   (lower-limit :accessor lower-limit :initform 1 :initarg :lower-limit)
   (max-delta :accessor max-delta :initform 2000 :initarg :max-delta)))

(defclass fps-unlocked (fps-manager)
  ((fps-ticks
    :accessor fps-ticks
    :initform 0
    :initarg :time)
   (dt
    :accessor dt
    :initform 20.0
    :initarg :dt)
   (max-dt
    :accessor max-dt
    :initform 1000
    :initarg :max-dt)
   (accumulator
    :accessor accumulator
    :initform 0.0
    :initarg :accumulator)
   (physics-hook-function
    :accessor ps-fn
    :initform #'(lambda (fps-time dt)
                  (declare (ignorable fps-time dt))
                  nil)
    :initarg :ps-fn)))

(defmethod initialize-instance :after ((self fps-manager)
                                       &key
                                       average-window
                                       target-frame-rate
                                       &allow-other-keys)
  (when average-window
    (setf (average-window (fps-average self)) (make-array average-window
                                                          :initial-element 0)))
  (when target-frame-rate
    (setf (target-frame-rate self) target-frame-rate)))

(defmethod (setf target-frame-rate) (rate (self fps-manager))
  (let ((window (average-window (fps-average self))))
    (when (and (numberp rate) (> rate 0))
      (let ((inverse-rate (truncate (/ 1000 rate))))
        (loop for i from 0 below (length window)
              for j = 0 then (incf j inverse-rate) do
              (setf (svref window i) j))))
    (setf (index (fps-average self)) (length window))))

(defmethod (setf target-frame-rate) :around (rate (self fps-fixed))
  (if (and (numberp rate) (> rate 0))
    (when (and (>= rate (lower-limit self))
	       (<= rate (upper-limit self)))
      (setf (frame-count self) 0
            (rate-ticks self) (truncate (/ 1000 rate))
            (slot-value self 'target-frame-rate) rate))
    (setf (slot-value self 'target-frame-rate) rate))
  (call-next-method)
  (slot-value self 'target-frame-rate))

(defmethod process-timestep :around ((self fps-manager) &optional fn)
  (with-slots (current-ticks delta-ticks last-ticks time-scale fps-average) self
    (setf current-ticks (sdl-cffi::sdl-get-ticks)
          delta-ticks (- current-ticks last-ticks))
    (calculate-time-scale time-scale delta-ticks)
    (update-fps-window fps-average current-ticks))
  
  (call-next-method)
  
  (with-slots (current-ticks last-ticks) self
    (setf last-ticks current-ticks)))

(defmethod process-timestep ((self fps-manager) &optional fn)
  (when fn (funcall fn)))

;;;; --------------------------
;;;; Lock the game loop to a specified rate
;;;; This is a reimplementation of the algorithm for SDL_gfx
;;;; From http://www.ferzkopp.net/joomla/content/view/19/14/

(defmethod process-timestep :after ((self fps-fixed) &optional fn)
  (with-slots (target-frame-rate frame-count rate-ticks current-ticks delay-ticks
                                 max-delta) self
    ;; Delay game loop, if necessary
    (when (and target-frame-rate (> target-frame-rate 0))
      (incf frame-count)
      (let* ((target-ticks (+ delay-ticks (* frame-count rate-ticks)))
             (delta (truncate (- target-ticks current-ticks))))
        (if (> delta 0)
          (sdl-cffi::sdl-delay (if (> delta max-delta) max-delta delta))
          (setf frame-count 0
                delay-ticks current-ticks))))))

;;;; --------------------------
;;;; Lock timestep to Specified Rate
11;;;; From http://www.gaffer.org/game-physics/fix-your-timestep/

(defmethod process-timestep :before ((self fps-unlocked) &optional fn)
  (with-slots (fps-ticks delta-ticks dt max-dt accumulator physics-hook-function)
      self
    (incf accumulator (if (> delta-ticks max-dt)
                        max-dt delta-ticks))
    (loop until (< accumulator dt) do
          (progn
            (when physics-hook-function
              (funcall physics-hook-function fps-ticks dt))
            (incf fps-ticks dt)
            (decf accumulator dt)))))


    


