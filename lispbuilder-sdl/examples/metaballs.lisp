;;;;; Converted from the "Liquid Balls" Processing example at:
;;;;; "http://processing.v3ga.net/show.php?id=10&type=0"
;;;;; Lisp version (C)2006 Luke J Crook

;; Press: 'h' to display a grid
;; Press: 'c' to display the center of each meta-ball
(in-package #:sdl-examples)

(defvar *draw-gridp* nil)
(defvar *draw-meta-centerp* nil)

(defstruct mmanager
  (screen-width 0 :type fixnum)
  (screen-height 0 :type fixnum)
  (iso-value 0.0 :type float)
  (viscosity 0.0 :type float)
  (min-viscosity 0.0 :type float)
  (max-viscosity 0.0 :type float)
  (d-viscosity 0.0 :type float)
  (x-squares 0 :type fixnum)
  (y-squares 0 :type fixnum)
  (x-res 0 :type fixnum)
  (y-res 0 :type fixnum)
  (grid-size-x 0 :type fixnum)
  (grid-size-y 0 :type fixnum)
  meta-grid
  square-edge
  offset
  line
  square-flag)

(defun new-mmanager (&key x-res y-res iso-value viscosity min-viscosity max-viscosity x-squares y-squares)
  (let ((manager (make-mmanager :screen-width (* x-res x-squares) :screen-height (* y-res y-squares) :iso-value iso-value
				:viscosity viscosity :min-viscosity min-viscosity :max-viscosity max-viscosity
				:x-squares x-squares :y-squares y-squares
				:x-res x-res :y-res y-res)))
    (setf (mmanager-d-viscosity manager) (/ (- max-viscosity min-viscosity) 250.0)
	  (mmanager-grid-size-x manager) (+ 1 x-squares)
	  (mmanager-grid-size-y manager) (+ 1 y-squares)
	  (mmanager-meta-grid manager) (make-array (list (+ 1 x-squares) (+ 1 y-squares))
						   :initial-element 0.0 :element-type 'float)
	  (mmanager-square-edge manager) (make-array '(4 2)
						     :initial-contents '((0 1) (1 2) (2 3) (3 0))
						     :element-type 'fixnum)
	  (mmanager-offset manager) (make-array '(4 2)
						:initial-contents '((0 0) (1 0) (1 1) (0 1))
						:element-type 'fixnum)
	  (mmanager-line manager) (make-array '(16 5)
					      :initial-contents '((-1 -1 -1 -1 -1)
								  (0   3 -1 -1 -1)
								  (0   1 -1 -1 -1) 
								  (3   1 -1 -1 -1)
								  (1   2 -1 -1 -1)
								  (1   2  0  3 -1)
								  (0   2 -1 -1 -1)
								  (3   2 -1 -1 -1)
								  (3   2 -1 -1 -1)
								  (0   2 -1 -1 -1)
								  (3   2  0  2 -1)
								  (1   2 -1 -1 -1)
								  (3   1 -1 -1 -1)
								  (0   1 -1 -1 -1)
								  (0   3 -1 -1 -1)
								  (-1 -1 -1 -1 -1))
					      :element-type 'fixnum)
	  (mmanager-square-flag manager) (make-array (list (+ 1 x-squares) (+ 1 y-squares))
						     :element-type 'fixnum))
    manager))

(defstruct metaball
  (center-x 0 :type fixnum) (center-y 0 :type fixnum)
  (center-i 0.0 :type float) (center-j 0.0 :type float)
  (strength 0 :type fixnum)
  (iso-value 0.0 :type float)
  (r 0.0 :type float)
  (sqr 0.0 :type float))

(defun set-radius (mball strength ival)
    (declare (optimize (safety 0) (speed 3) (space 1))
	   (type fixnum strength)
	   (type float ival))
  (setf (metaball-r mball) (sdl:cast float (sqrt (/ strength ival))))
  ;; Note: (/ 256 64) probably needs to change to screen-width and squares.
  (setf (metaball-sqr mball) (sdl:cast float (/ (metaball-r mball) (/ 256 64)))))

(defun set-strength (mball s)
  (declare (optimize (safety 0) (speed 3) (space 1))
	   (type fixnum s))
  (setf (metaball-strength mball) s)
  (set-radius mball (metaball-strength mball) (metaball-iso-value mball)))

(defun set-center-to (mball x y)
  (declare (optimize (safety 0) (speed 3) (space 1))
	   (type fixnum x y))
  (setf (metaball-center-x mball) x
	(metaball-center-y mball) y))

(defun get-field-at (mball x y)
  (declare (optimize (safety 0) (speed 3) (space 1))
	   (type fixnum x y))
  (let ((dx (- (metaball-center-x mball) x))
	(dy (- (metaball-center-y mball) y)))
    (declare (type fixnum dx dy))
  (sdl:cast float (/ (metaball-strength mball) (+ (* dx dx) (* dy dy)
					     0.01)))))

(defun square-coords (mball x y)
  (declare (optimize (safety 0) (speed 3) (space 1))
	   (type fixnum x y))
  (setf (metaball-center-i mball) (sdl:cast float (coerce (/ (metaball-center-x mball)
							x) 'float)))
  (setf (metaball-center-j mball) (sdl:cast float (coerce (/ (metaball-center-y mball)
							y) 'float))))

(defun get-square-coords-i (mball x y)
  (declare (ignore y))
  (declare (optimize (safety 0) (speed 3) (space 1))
	   (type fixnum x y))
  (setf (metaball-center-i mball) (sdl:cast float (coerce (/ (metaball-center-x mball)
							x) 'float))))

(defun get-square-coords-j (mball x y)
  (declare (ignore x))
  (declare (optimize (safety 0) (speed 3) (space 1))
	   (type fixnum x y))
  (setf (metaball-center-j mball) (sdl:cast float (coerce (/ (metaball-center-y mball)
							y) 'float))))

(defun new-metaball (iso-v &key (cx 0) (cy 0) strength)
  (declare (optimize (safety 0) (speed 3) (space 1))
	   (type float iso-v)
	   (type fixnum cx cy strength))
  (let ((mball (make-metaball :strength 30000 :iso-value iso-v :center-x cx :center-y cy)))
    (set-radius mball 30000 iso-v)
    (set-strength mball strength)
    mball))

(defun init-meta-grid (grid col row)
  (declare (optimize (safety 0) (speed 3) (space 1))
	   (type fixnum row col))
  (dotimes (j row)
    (dotimes (i col)
      (setf (aref grid i j) 0))))

(defun draw-grid (col row x-resolution y-resolution color surface)
  (declare (optimize (safety 0) (speed 3) (space 1))
	   (type fixnum col row x-resolution y-resolution)
	   (type vector color))
  ;; Draw the vertical lines
  (loop
     for x from 0 to col
     for x-pos = (* x x-resolution)
     do (sdl:draw-vline x-pos 0 (* y-resolution row) :surface surface :color color))
  ;; Draw the horizontal lines
  (loop
     for y from 0 to row
     for y-pos = (* y y-resolution)
     do (sdl:draw-hline 0 (* x-resolution col) y-pos :surface surface :color color)))

(defun draw-meta-center (manager meta-balls color surface)
  (declare (optimize (safety 0) (speed 3) (space 1)))
  (let ((i 0.0) (j 0.0)
	(x-resolution (mmanager-x-res manager))
	(y-resolution (mmanager-y-res manager)))
    (declare (type float i j)
	     (type fixnum x-resolution y-resolution))
    (dolist (metaball meta-balls)
      (when (and (>= (metaball-center-x metaball) 0)
		 (< (metaball-center-x metaball) (mmanager-screen-width manager))
		 (>= (metaball-center-y metaball) 0)
		 (< (metaball-center-y metaball) (mmanager-screen-height manager)))
	(setf i (get-square-coords-i metaball x-resolution y-resolution))
	(setf j (get-square-coords-j metaball x-resolution y-resolution))

	(sdl:draw-line-* (sdl:cast-to-int (* i x-resolution))
			 (sdl:cast-to-int (* j y-resolution))
			 (sdl:cast-to-int (* (+ i 1) x-resolution))
			 (sdl:cast-to-int (* (+ j 1) y-resolution))
			 :surface surface :color color)
	(sdl:draw-line-* (sdl:cast-to-int (* (+ i 1) x-resolution))
			 (sdl:cast-to-int (* j y-resolution))
			 (sdl:cast-to-int (* i x-resolution))
			 (sdl:cast-to-int (* (+ j 1) y-resolution))
			 :surface surface :color color)))))

(defun handle-keypress (key)
  (case key
    (:SDL-KEY-ESCAPE (sdl:push-quit-event))
    (:SDL-KEY-G (setf *draw-gridp* (not *draw-gridp*)))
;;     (:SDLK_PLUS (setf *viscosity* (sdl:clamp (incf *viscosity*) *min-viscosity* *max-viscosity*)))
;;     (:SDLK_MINUS (setf *viscosity* (sdl:clamp (decf *viscosity*) *min-viscosity* *max-viscosity*)))
    (:SDL-KEY-C (setf *draw-meta-centerp* (not *draw-meta-centerp*)))
    (t nil)))

(defun handle-mouse-moved (mouse-x mouse-y meta-ball)
  (declare (type fixnum mouse-x mouse-y))
  (set-center-to meta-ball mouse-x mouse-y))

(defun render-loop (manager meta-balls color surface)
  (declare (optimize (safety 0) (speed 3) (space 1)))
  (setf (mmanager-viscosity manager) (sdl:cast float (+ ( * 0.5 (mmanager-d-viscosity manager))
						   (mmanager-min-viscosity manager))))
  (let ((grid-size-y (mmanager-grid-size-y manager))
	(grid-size-x (mmanager-grid-size-x manager))
	(x-resolution (mmanager-x-res manager))
	(y-resolution (mmanager-y-res manager))
	(iso-value (mmanager-iso-value manager))
	(viscosity (mmanager-viscosity manager))
	(square-flag (mmanager-square-flag manager))
	(meta-grid (mmanager-meta-grid manager))
	(offset (mmanager-offset manager))
	(line (mmanager-line manager))
	(square-edge (mmanager-square-edge manager)))
    (declare (type fixnum grid-size-y grid-size-x x-resolution y-resolution)
	     (type float iso-value viscosity)
	     (type (array fixnum *) square-flag offset line square-edge)
	     (type (array float *) meta-grid))

    (loop for y from 0 below grid-size-y
       do (loop for x from 0 below grid-size-x
	     do (let ((meta-grid-target 0.0))
		  (declare (type float meta-grid-target))
		  (setf (aref square-flag x y) 0)
		  (dolist (meta-ball meta-balls)
		    (incf meta-grid-target (get-field-at meta-ball (* x-resolution x) (* y-resolution y))))
		  (incf (aref meta-grid x y) (sdl:cast float (/ (- meta-grid-target
							      (aref meta-grid x y))
							   viscosity))))))

    (let ((scan-imin 0) (scan-imax (mmanager-x-squares manager))
	  (scan-jmin 0) (scan-jmax (mmanager-y-squares manager)))
      (declare (type fixnum scan-imin scan-imax scan-jmin scan-jmax))
      (loop for j from scan-jmin below scan-jmax
	 do (loop for i from scan-imin below scan-imax
	       do (let ((square-idx 0)
			(val1 0.0) (val2 0.0) (temp 0.0)
			(iso-p1-x 0) (iso-p1-y 0) (iso-p2-x 0) (iso-p2-y 0)
			(p1-idx 0) (p2-idx 0))
		    (declare (type fixnum iso-p1-x iso-p1-y iso-p2-x iso-p2-y p1-idx p2-idx)
			     (type float temp val1 val1))
		    (unless (= (aref square-flag i j) 1)
		      (when (< (aref meta-grid i j) iso-value)
			(setf square-idx (logior square-idx 1)))
		      (when (< (aref meta-grid (+ i 1) j) iso-value)
			(setf square-idx (logior square-idx 2)))
		      (when (< (aref meta-grid (+ i 1) ( + j 1)) iso-value)
			(setf square-idx (logior square-idx 4)))
		      (when (< (aref meta-grid i (+ j 1)) iso-value)
			(setf square-idx (logior square-idx 8)))
		      (unless (or (= square-idx 0)
				  (= square-idx 15))
			(let ((n 0)
			      (edge-1-idx 0) (edge-2-idx 0))
			  (declare (type fixnum n edge-1-idx edge-2-idx))
			  (do ()
			      ((= -1 (aref line square-idx n)))
			    (setf edge-1-idx (aref line square-idx n))
			    (incf n)
			    (setf edge-2-idx (aref line square-idx n))
			    (incf n)
			      
			    ;; Edge 1
			    (setf p1-idx (aref square-edge edge-1-idx 0)
				  p2-idx (aref square-edge edge-1-idx 1))
				  
			    (setf val1 (aref meta-grid
					     (+ i (aref offset p1-idx 0))
					     (+ j (aref offset p1-idx 1)))
				  val2 (aref meta-grid
					     (+ i (aref offset p2-idx 0))
					     (+ j (aref offset p2-idx 1))))
			    (if (not (= (- val2 val1) 0))
				(setf temp (sdl:cast float (/ (- iso-value val1)
							 (- val2 val1))))
				(setf temp 0.5))
			    (setf iso-p1-x (sdl:cast-to-int (* x-resolution
							  (+ (* temp (- (+ i (aref offset p2-idx 0))
									(+ i (aref offset p1-idx 0))))
							     (+ i (aref offset p1-idx 0))))))
			    (setf iso-p1-y (sdl:cast-to-int (* y-resolution
							  (+ (* temp (- (+ j (aref offset p2-idx 1))
									(+ j (aref offset p1-idx 1))))
							     (+ j (aref offset p1-idx 1))))))
				  
			    ;; Edge 2
			    (setf p1-idx (aref square-edge edge-2-idx 0)
				  p2-idx (aref square-edge edge-2-idx 1))
				  
			    (setf val1 (aref meta-grid
					     (+ i (aref offset p1-idx 0))
					     (+ j (aref offset p1-idx 1)))
				  val2 (aref meta-grid
					     (+ i (aref offset p2-idx 0))
					     (+ j (aref offset p2-idx 1))))
			    (if (not (= (- val2 val1) 0))
				(setf temp (sdl:cast float (/ (- iso-value val1)
							 (- val2 val1))))
				(setf temp 0.5))
			    (setf iso-p2-x (sdl:cast-to-int (* x-resolution
							  (+ (* temp (- (+ i (aref offset p2-idx 0))
									(+ i (aref offset p1-idx 0))))
							     (+ i (aref offset p1-idx 0))))))
			    (setf iso-p2-y (sdl:cast-to-int (* y-resolution
							  (+ (* temp (- (+ j (aref offset p2-idx 1))
									(+ j (aref offset p1-idx 1))))
							     (+ j (aref offset p1-idx 1))))))

			    ;; Draw Line
			    (sdl:draw-line-* iso-p1-x iso-p1-y
					     iso-p2-x iso-p2-y
					     :color color :surface surface)
			    (setf (aref square-flag i j) 1)))))))))))


(defun setup ()
  (let ((iso-value 16.0))
    (list (new-metaball iso-value :cx 125 :cy 130 :strength 10000)
	  (new-metaball iso-value :cx 200 :cy 200 :strength 10000)
	  (new-metaball iso-value :cx 65 :cy 68 :strength 40000)
	  (new-metaball iso-value :cx 50 :cy 200 :strength 5000))))

(defun metaballs (&key (h-res 10) (v-res 10) (h-squares 25) (v-squares 30))
  (let* ((res-width h-res) (res-height v-res)
	 (horizontal-res h-squares) (vertical-res v-squares)
	 (meta-color (sdl:color :r 175 :g 175 :b 175)) (meta-center-color (sdl:color :r 200 :g 0 :b 0)) (grid-color (sdl:color :r 75 :g 75 :b 75))
	 (mb-pressed? nil)
	 (meta-balls (setup))
	 (manager (new-mmanager :y-res res-width :x-res res-height :iso-value 16.0 
				:viscosity 15.0 :min-viscosity 1.0 :max-viscosity 20.0 
				:x-squares horizontal-res :y-squares vertical-res))
	 (100-frames-p (every-n-frames 100)))

    (sdl:initialise-default-font sdl:*font-5x7*)
    (sdl:with-init ()
      (sdl:window (mmanager-screen-width manager) (mmanager-screen-height manager) :title-caption "Metaballs")
      (setf (sdl:frame-rate) 0)
      (sdl:clear-display (sdl:color :r 0 :g 0 :b 0))

      (sdl:initialise-default-font sdl:*font-5x7*)
      (draw-fps "Calculating FPS....." 10 10 sdl:*default-font* sdl:*default-surface* t)

      (sdl:with-surface (grid (sdl:create-surface (sdl:width sdl:*default-display*)
						  (sdl:height sdl:*default-display*)) t)
	(draw-grid (mmanager-x-squares manager) (mmanager-y-squares manager)
		   (mmanager-x-res manager) (mmanager-y-res manager)
		   grid-color grid)
	
	(sdl:with-events ()
	  (:quit-event () t)
	  (:key-down-event (:key key) (handle-keypress key))
	  (:mouse-button-down-event ()
				    (setf mb-pressed? t))
	  (:mouse-button-up-event ()
				  (setf mb-pressed? nil))
	  (:mouse-motion-event (:x x :y y)
			       (when mb-pressed?
				 (handle-mouse-moved x y (first meta-balls))))
	  (:idle ()
		 (if *draw-gridp*
		     (sdl:blit-surface grid sdl:*default-display*)
		     (sdl:clear-display (sdl:color :r 0 :g 0 :b 0)))
		 (when *draw-meta-centerp* ()
		       (draw-meta-center manager meta-balls meta-center-color sdl:*default-display*))
		 (render-loop manager meta-balls meta-color sdl:*default-display*)

		 ;; Optimization; Draw the font each frame,
		 ;; but only render the font once every 100 frames.
		 (draw-fps (format nil "FPS : ~2$" (sdl:average-fps))
			   10 10 sdl:*default-font* sdl:*default-display*
			   (funcall 100-frames-p))
		 (sdl:update-display)))))))
