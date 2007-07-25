
(in-package #:lispbuilder-sdl-base)

(defclass fps-manager ()
  ((frame-index :accessor frame-index :initform 0)
   (frame-averages :accessor frame-averages :initform (make-array 60 :initial-element 0 :element-type 'fixnum))
   (average-last-ticks :accessor average-last-ticks :initform 0)
   (average-p :accessor average-p :initform nil)))

(defgeneric (setf target-frame-rate) (rate fpsmngr))
(defgeneric init-fps-manager (fps-manager))
(defgeneric process-timestep (fpsmngr &optional fn))
(defgeneric calculate-average-fps (fpsmngr))

(defmethod (setf target-frame-rate) (rate fpsmngr)
  (error "FPSMNGR cannot be NIL."))

(defmethod calculate-average-fps ((fpsmngr fps-manager))
  (cond
    ((average-p fpsmngr)
     (average-p fpsmngr))
    (t
     (setf (average-p fpsmngr) 1)
     (dotimes (i (length (frame-averages fpsmngr)))
       (incf (average-p fpsmngr) (aref (frame-averages fpsmngr) i)))
     (setf (average-p fpsmngr) (/ 1000
				  (/ (average-p fpsmngr)
				     (length (frame-averages fpsmngr))))))))

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
   (lower-limit :accessor lower-limit :initform 1 :initarg lower-limit)
   (time-scale :accessor time-scale :initform nil :initarg time-scale)
   (delta-ticks :accessor delta-ticks :initform nil :initarg delta-ticks)
   (world-time :accessor world-time :initform 100 :initarg world-time)))

(defmethod (setf target-frame-rate) (rate (fpsmngr fps-fixed))
  (if (> rate 0)
      (if (and (>= rate (lower-limit fpsmngr))
	       (<= rate (upper-limit fpsmngr)))
	  (progn
	    (setf (frame-count fpsmngr) 0)
	    (setf (slot-value fpsmngr 'frame-rate) rate)
	    (setf (rate-ticks fpsmngr) (/ 1000.0 rate))
	    t)
	  nil)
      (setf (slot-value fpsmngr 'frame-rate) rate)))

(defmethod init-fps-manager ((fpsmanager fps-fixed))
  nil)

(defmethod process-timestep ((fpsmngr fps-fixed) &optional fn)
  (let ((current-ticks 0) (target-ticks 0))
    (when fn
      (funcall fn))

    (setf current-ticks (sdl-cffi::sdl-get-ticks))

    ;; Track ticks over FRAME-AVERAGE-COUNT frames.
    (setf (aref (frame-averages fpsmngr) (frame-index fpsmngr))
	  (the fixnum (- current-ticks (average-last-ticks fpsmngr))))
    (incf (frame-index fpsmngr))
    (when (>= (frame-index fpsmngr) (length (frame-averages fpsmngr)))
      (setf (frame-index fpsmngr) 0
	    (average-p fpsmngr) nil))

    (setf (average-last-ticks fpsmngr) current-ticks)

    ;; Delay game loop, if necessary
    (when (> (target-frame-rate fpsmngr) 0)
      (setf (time-scale fpsmngr) (/ (setf (delta-ticks fpsmngr) (- current-ticks
								   (last-ticks fpsmngr)))
				    (world-time fpsmngr)))
      (incf (frame-count fpsmngr))
      (setf target-ticks (+ (last-ticks fpsmngr)
			    (* (frame-count fpsmngr)
			       (rate-ticks fpsmngr))))
      (if (<= current-ticks target-ticks)
	  (sdl-cffi::sdl-delay (round (- target-ticks current-ticks)))
	  (progn
	    (setf (frame-count fpsmngr) 0
		  (last-ticks fpsmngr) current-ticks))))))
    

;;;; --------------------------
;;;; Lock timestep to Specified Rate
;;;; From http://www.gaffer.org/game-physics/fix-your-timestep/

(defclass fps-timestep (fps-manager)
  ((accumulator :accessor accumulator :initform 0 :initarg accumulator)
   (current-time :accessor current-time :initform 0 :initarg current-time)
   (new-time :accessor new-time :initform 0 :initarg new-time)
   (delta-time :accessor delta-time :initform 0 :initarg delta-time)
   (max-delta-time :accessor max-delta-time :initform 25 :initarg max-delta-time)
   (previous-state :accessor previous-state :initform 0.0 :initarg previous-state)
   (current-state :accessor current-state :initform 0.0 :initarg current-state)
   (alpha :accessor alpha :initform 0.0 :initarg alpha)
   (a-time :accessor a-time :initform 0 :initarg a-time)
   (d-time :accessor d-time :initform 10 :initarg d-time)))

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
