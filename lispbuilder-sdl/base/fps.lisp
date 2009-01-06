(in-package #:lispbuilder-sdl-base)

(defgeneric (setf target-frame-rate) (rate fpsmngr)
  (:documentation "Set the target frame rate for the game loop.
RATE > 0 will lock the game loop to the specified frame rate, and
calculate the average frame rate over a number of frames.
RATE = 0 will unlock the frame rate, and calculate the average
frame rate over a number of frames.
RATE < 0 will unlock the frame rate. The average frane rate is 
not calculated"))

(defgeneric init-fps-manager (fps-manager))
(defgeneric process-timestep (fpsmngr &optional fn)
  (:documentation "Manages the timestep. Called once per game loop."))

(defclass fps-average ()
  ((index :accessor index :initform 0)
   (window :accessor average-window :initform nil)
   (last-ticks :accessor last-ticks :initform 0)
   (calculated :accessor average-fps :initform 0)))

(defclass time-scale ()
  ((tscale :accessor tscale :initform 0)
   (delta :accessor time-delta :initform 0)
   (time-last :accessor time-last :initform 0)
   (world :accessor world :initform 1000)))

(defmethod update-fps-window ((self fps-average) current-ticks)
  (with-accessors ((index index) (average-window average-window)
                   (last-ticks last-ticks) (average-fps average-fps)) self
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
                              (length average-window)))))
    (setf last-ticks current-ticks)))

(defun calculate-time-scale (time-scale current-ticks)
  (with-accessors ((tscale tscale) (time-delta time-delta)
                   (time-last time-last) (world world)) time-scale
    (setf tscale (/ (setf time-delta (- current-ticks time-last))
                    world))
    (setf time-last current-ticks)))

(defclass fps-manager ()
  ((fps-average :reader fps-average :initform (make-instance 'fps-average))
   (time-scale :reader time-scale :initform (make-instance 'time-scale)))
  (:default-initargs
   :average-window 200
   :target-frame-rate 30))

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

(defmethod (setf target-frame-rate) (rate (self fps-manager)) nil)

(defmethod (setf target-frame-rate) :around (rate (self fps-manager))
  (let ((window (average-window (fps-average self))))
    (when (and rate (> rate 0))
      (let ((inverse-rate (truncate (/ 1000.0 rate))))
        (loop for i from 0 below (length window)
              for j = 0 then (incf j inverse-rate) do
              (setf (svref window i) j))))
    (setf (index (fps-average self)) (length window)))
  (call-next-method))

;;;; --------------------------
;;;; Lock the game loop to a specified rate
;;;; This is a reimplementation of the algorithm for SDL_gfx
;;;; From http://www.ferzkopp.net/joomla/content/view/19/14/

(defclass fps-fixed (fps-manager)
  ((frame-count :accessor frame-count :initform 0 :initarg frame-count)
   (frame-rate :reader target-frame-rate :initform 30 :initarg frame-rate)
   (rate-ticks :accessor rate-ticks :initform (/ 1000.0 30.0) :initarg rate-ticks)
   (last-ticks :accessor last-ticks :initform 0 :initarg last-ticks)
   (upper-limit :accessor upper-limit :initform 200 :initarg upper-limit)
   (lower-limit :accessor lower-limit :initform 1 :initarg lower-limit)))

(defmethod (setf target-frame-rate) (rate (self fps-fixed))
  (if (> rate 0)
      (if (and (>= rate (lower-limit self))
	       (<= rate (upper-limit self)))
	  (progn
	    (setf (frame-count self) 0)
	    (setf (slot-value self 'frame-rate) rate)
	    (setf (rate-ticks self) (/ 1000.0 rate))
	    t)
	  nil)
      (setf (slot-value self 'frame-rate) rate)))

(defmethod init-fps-manager ((fpsmanager fps-fixed)) nil)

(defmethod process-timestep ((fpsmngr fps-fixed) &optional fn)
  (let ((current-ticks 0) (target-ticks 0))
    (when fn
      (funcall fn))
    
    (when (> (target-frame-rate fpsmngr) -1)
      (setf current-ticks (sdl-cffi::sdl-get-ticks))

      (update-fps-window (fps-average fpsmngr) current-ticks)
      (calculate-time-scale (time-scale fpsmngr) current-ticks)
      
      ;; Delay game loop, if necessary
      (when (> (target-frame-rate fpsmngr) 0)
	(incf (frame-count fpsmngr))
	(setf target-ticks (+ (last-ticks fpsmngr)
			      (* (frame-count fpsmngr)
				 (rate-ticks fpsmngr))))
        (if (<= current-ticks target-ticks)
          (sdl-cffi::sdl-delay (truncate (- target-ticks current-ticks)))
          (progn
            (setf (frame-count fpsmngr) 0
                  (last-ticks fpsmngr) current-ticks)))))))
    
;;;; --------------------------
;;;; Lock timestep to Specified Rate
;;;; From http://www.gaffer.org/game-physics/fix-your-timestep/

(defclass fps-timestep (fps-manager)
  ((accumulator :accessor accumulator :initform 0 :initarg :accumulator)
   (current-time :accessor current-time :initform 0 :initarg :current-time)
   (new-time :accessor new-time :initform 0 :initarg :new-time)
   (delta-time :accessor delta-time :initform 0 :initarg :delta-time)
   (max-delta-time :accessor max-delta-time :initform 25 :initarg :max-delta-time)
   (previous-state :accessor previous-state :initform 0.0 :initarg :previous-state)
   (current-state :accessor current-state :initform 0.0 :initarg :current-state)
;;;;    (alpha :accessor alpha :initform 0.0 :initarg :alpha)
   (a-time :accessor a-time :initform 0 :initarg :a-time)
   (d-time :accessor d-time :initform 10 :initarg :d-time)))

(defmethod init-fps-manager ((fpsmngr fps-timestep))
  (setf (current-time fpsmngr) (sdl-cffi::sdl-get-ticks)
	(a-time fpsmngr) 0))

;; (defmethod process-timestep ((fpsmngr fps-timestep) &optional fn)
;;   (setf (new-time fpsmngr) (sdl-cffi::sdl-get-ticks)
;; 	(delta-time fpsmngr) (- (new-time fpsmngr)
;; 				(current-time fpsmngr))
;; 	(current-time fpsmngr) (new-time fpsmngr))
;;   (when (> (delta-time fpsmngr) (max-delta-time fpsmngr))
;;     (setf (delta-time fpsmngr) (max-delta-time fpsmngr)))
;;   (incf (accumulator fpsmngr) (delta-time fpsmngr))

;;   (do ()
;;       ((<= (accumulator fpsmngr) (delta-time fpsmngr)))
;;     (setf (previous-state fpsmngr) (current-state fpsmngr))
;;     (funcall )))

  ;;   (defun process-timestep (renderer)
;;     (setf new-time (sdl-cffi::sdl-get-ticks))
;;     (setf delta-time (- new-time current-time))
;;     (setf current-time new-time)
;;     (when (> delta-time max-delta-time)
;;       (setf delta-time max-delta-time))
;;     (incf accumulator delta-time)
    
;;     (do ()
;; 	((<= accumulator d-time))
;; ;;      (setf previous-state current-state)
;;       (hook-process-physics renderer current-state time d-time)
;; ;;    (incf time d-time)
;;       (decf accumulator d-time))

;; ;;     (setf alpha (/ accumulator d-time))
;; ;;     (+ (* current-state alpha)
;; ;;        (* previous-state (- 1.0 alpha)))
;;     ))


;; (let ((time 0) (d-time 10)
;;       (accumulator 0)
;;       (current-time 0) (new-time 0) (delta-time 0) (max-delta-time 25)
;;       (previous-state 0.0) (current-state 0.0)
;;       (alpha 0.0))
;;   (declare (type fixnum new-time delta-time current-time time d-time accumulator max-delta-time)
;; 	   (type single-float alpha)
;; 	   (type single-float previous-state current-state))

;;   (defun init-timestep ()
;;     (setf current-time (sdl-cffi::sdl-get-ticks)
;; 	  time 0))
  
;;   (defun process-timestep (renderer)
;;     (setf new-time (sdl-cffi::sdl-get-ticks))
;;     (setf delta-time (- new-time current-time))
;;     (setf current-time new-time)
;;     (when (> delta-time max-delta-time)
;;       (setf delta-time max-delta-time))
;;     (incf accumulator delta-time)
    
;;     (do ()
;; 	((<= accumulator d-time))
;; ;;      (setf previous-state current-state)
;;       (hook-process-physics renderer current-state time d-time)
;; ;;    (incf time d-time)
;;       (decf accumulator d-time))

;; ;;     (setf alpha (/ accumulator d-time))
;; ;;     (+ (* current-state alpha)
;; ;;        (* previous-state (- 1.0 alpha)))
;;     ))


;;;; --------------------------
;;;; Calculate FPS over a number of frames
;;;; From ...
;; (let* ((frame-values 60)
;;        (frame-times (make-array frame-values :initial-element 0 :element-type 'fixnum))
;;        (frame-time-last 0)
;;        (frame-count 0))
;;   (declare (type fixnum frame-values frame-time-last frame-count))

;;   (defun fps-init ()
;;     (dotimes (i frame-values)
;;       (setf (aref frame-times i) 0))
;;     (setf frame-count 0
;; 	  frame-time-last (the fixnum (sdl-cffi::SDL-get-ticks))))

;;   (defun display-fps (x y surface)
;;     (declare (optimize (safety 0) (speed 3))
;; 	     (type fixnum x y))
;;     (let ((get-ticks (the fixnum (sdl-cffi::SDL-get-ticks)))
;;           (frames-per-second 0.0))
;;       (declare (type fixnum get-ticks)
;; 	       (type single-float frames-per-second))
;;       (setf (aref frame-times frame-count) (the fixnum (- get-ticks frame-time-last)))
;;       (setf frame-time-last get-ticks)
;;       (incf frame-count)
;;       (when (>= frame-count frame-values)
;; 	(setf frame-count 0)
;; 	(dotimes (i frame-values)
;; 	  (incf frames-per-second (aref frame-times i)))
;; 	(setf frames-per-second (the single-float (/ 1000 (the single-float (/ frames-per-second frame-values)))))
;; 	(sdl:render-string-solid (format nil "FPS : ~2$" frames-per-second)
;; 				 :color sdl:*white*
;; 				 :cache t
;; 				 :free t))
;;      (sdl:draw-font-at-* x y :surface surface))))
