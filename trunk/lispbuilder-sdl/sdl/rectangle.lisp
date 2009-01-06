;;;; lispbuilder-sdl
;;;; (C)2006 Luke Crook <luke@balooga.com>

(in-package #:lispbuilder-sdl)

(defclass rectangle (foreign-object) ()
  ;; ((rectangle-color
;;     :accessor rectangle-color
;;     :initform (color :r 255 :g 255 :b 255)
;;     :initarg :color))
  (:default-initargs
   :gc t
    :free #'cffi:foreign-free)
  (:documentation "A `RECTANGLE` object manages the foreign SDL_Rect object.
Free using [FREE](#free)."))

(defun rectangle (&key (x 0) (y 0) (w 0) (h 0) (fp nil))
  "Creates a new `RECTANGLE` from the specified `X`, `Y`, width `W` and height `H`.
If `FP' is `NIL` then a foreign SDL_Rect is created. If `FP` is a pointer to a foreign SDL_Rect object then `FP` is used."
  (declare (type fixnum x y w h))
  (make-instance 'rectangle :fp (if (sdl-base::is-valid-ptr fp)
				    (sdl-base::rectangle :src fp)
				    (sdl-base::rectangle :x x :y y :w w :h h))))

(defun random-rectangle (bound-w bound-h &optional (rectangle (rectangle)))
  "Creates and return s a new `RECTANGLE` of random x, y width and height within the specified
bounds of width `BOUND-W` and height `BOUND-H`. `RECTANGLE` if unset will force the creation of a 
new `RECTANGLE` object. `RECTANGLE` if set will be modified with the coordinates."
  (declare (type fixnum bound-w bound-h))
  (check-type rectangle rectangle)
  (let* ((x (random bound-w))
	 (y (random bound-h))
	 (w (random+1 (- bound-w x)))
	 (h (random+1 (- bound-h y))))
    (set-rectangle-* rectangle :x x :y y :w w :h h))
  rectangle)

;; (defun rectangle-from-wh (width height &key (position *default-position*))
;;   (rectangle :x (x position)
;; 	     :y (y position)
;; 	     :w (+ (x position) width)
;; 	     :h (+ (y position) height)))

(defun rectangle-from-edges (p1 p2 &optional (rectangle (rectangle)))
  "See [RECTANGLE-FROM-EDGES-*](#rectangle-from-edges-*).

* `P1` and `P2` are `POINTS` that specify the bounds of the `RECTANGLE`. 
`P1` specifies the top left coordinate. `P2` specifies the lower right coordinate."
  (rectangle-from-edges-* (x p1) (y p1) (x p2) (y p2) rectangle))

(defun rectangle-from-edges-* (x1 y1 x2 y2 &optional (rectangle (rectangle)))
  "Returns a new `RECTANGLE` using the bounds specified by the `INTEGERS` `X1`, `X2`, `Y1` and `Y2`. 
The coordinates of the rectangle are X = X1, Y = Y1, WIDTH = \(- X2 X1\), HEIGHT = \(- Y2 Y1\) 

##### Parameters

* `X1`, `Y1` specify the top left coordinate as `INTEGERS`. 
* `X2`, `Y2` specify the bottom right coordinate as `INTEGERS`. 
* `RECTANGLE` if unset will force the creation of a new `RECTANGLE` object. 
`RECTANGLE` if set will be modified with the coordinates."
  (check-type rectangle rectangle)
  (set-rectangle-* rectangle
		   :x x1
		   :y y1
		   :w (1+ (abs (- x2 x1)))
		   :h (1+ (abs (- y2 y1)))))

(defun rectangle-from-midpoint-* (x y w h &optional (rectangle (rectangle)))
  "Returns a `RECTANGLE` of width `W` and height `H` with the rectangle mid-point at coordinates `X` and `Y`. 
`RECTANGLE` if unset will force the creation of a new `RECTANGLE` object. 
`RECTANGLE` if set will be modified with the coordinates."
  (check-type rectangle rectangle)
  (set-rectangle-* rectangle
		   :x (- x (/ w 2))
		   :y (- y (/ h 2))
		   :w w
		   :h h))

(defmacro with-rectangle ((var &optional rectangle (free t))
			  &body body)
  "A convenience macro that binds `\*DEFAULT-RECTANGLE\*` to `VAR` within the scope of `WITH-RECTANGLE`. 
`VAR` must be of type `RECTANGLE`.
`VAR` is set to `RECTANGLE` when `RECTANGLE` is not `NIL`.
`VAR` is freed when `FREE` is `T`.

##### Example

    \(WITH-RECTANGLE \(a-rect \(RECTANGLE :x 0 :y 0 :w 100 :h 100\)\)
        ...\)"
  (let ((body-value (gensym "body-value-"))
	(free-value (gensym "free-value-")))
    `(let* ((,body-value nil)
	    (,free-value ,free)
	    (,@(if rectangle
		   `(,var ,rectangle)
		   `(,var ,var)))
	    (*default-rectangle* ,var))
       (declare (ignorable ,free-value))
       (symbol-macrolet ((,(intern (string-upcase (format nil "~A.x" var))) (x ,var))
			 (,(intern (string-upcase (format nil "~A.y" var))) (y ,var))
			 (,(intern (string-upcase (format nil "~A.w" var))) (width ,var))
			 (,(intern (string-upcase (format nil "~A.h" var))) (height ,var)))
	 (declare (ignorable ,(intern (string-upcase (format nil "~A.x" var)))
			     ,(intern (string-upcase (format nil "~A.y" var)))
			     ,(intern (string-upcase (format nil "~A.w" var)))
			     ,(intern (string-upcase (format nil "~A.h" var)))))
	 (setf ,body-value (progn ,@body))
	 ,(if free
	      `(progn
		 (when ,free-value
		   (free ,var)
		   ,body-value))
	      `(progn
		 ,body-value))))))

(defmacro with-rectangles (bindings &body body)
  "A convenience macro that binds multiple rectangles as per [WITH-RECTANGLE](#with-rectangle).

##### Example

    \(WITH-RECTANGLES \(\(a-rect \(RECTANGLE :x 0 :y 0 :w 100 :h 100\)\)
                      \(\(b-rect \(RECTANGLE :x 0 :y 100 :w 100 :h 100\)\)
                      \(\(c-rect \(RECTANGLE :x 0 :y 200 :w 100 :h 100\)\)\)
        ...\)"
  (if bindings
      (return-with-rectangle bindings body)))

(defun return-with-rectangle (bindings body)
  (if bindings
      `(with-rectangle (,@(car bindings))
	 ,(return-with-rectangle (cdr bindings) body))
      `(progn ,@body)))

(defmethod x ((rectangle rectangle))
  "Returns the `X` position coordinate of the rectangle `RECTANGLE` as an `INTEGER`."
  (sdl-base::rect-x (fp rectangle)))
(defmethod (setf x) (x-val (rectangle rectangle))
  "Sets the `X` position coordinate of the rectangle `RECTANGLE`."
  (setf (sdl-base::rect-x (fp rectangle)) x-val))

(defmethod y ((rectangle rectangle))
  "Returns the `Y` position coordinate of the rectangle `RECTANGLE` as an `INTEGER`."
  (sdl-base::rect-y (fp rectangle)))
(defmethod (setf y) (y-val (rectangle rectangle))
  "Sets the `Y` position coordinate of the rectangle `RECTANGLE`."
  (setf (sdl-base::rect-y (fp rectangle)) y-val))

(defmethod x2 ((rectangle rectangle))
  "Sets the WIDTH of the rectangle `RECTANGLE` to `\(- X2 X\)`"
  (+ (sdl-base::rect-x (fp rectangle))
     (sdl-base::rect-w (fp rectangle))))
(defmethod (setf x2) (h-val (rectangle rectangle))
  "Returns `\(+ X WIDTH\)` of the rectangle `RECTANGLE`."
  (setf (sdl-base::rect-w (fp rectangle)) (- h-val
					     (sdl-base::rect-x (fp rectangle)))))

(defmethod y2 ((rectangle rectangle))
  "Returns `\(+ Y HEIGHT\)` of the rectangle `RECTANGLE`."
  (+ (sdl-base::rect-y (fp rectangle))
     (sdl-base::rect-h (fp rectangle))))
(defmethod (setf y2) (h-val (rectangle rectangle))
  "Sets the HEIGHT of rectangle `RECTANGLE` to `\(- Y2 Y\)`"
  (setf (sdl-base::rect-h (fp rectangle)) (- h-val
					     (sdl-base::rect-y (fp rectangle)))))

(defmethod width ((rectangle rectangle))
  "Returns the `INTEGER` width of the rectangle `RECTANGLE`."
  (sdl-base::rect-w (fp rectangle)))
(defmethod (setf width) (w-val (rectangle rectangle))
  "Sets the `INTEGER` width of the rectangle `RECTANGLE`."
  (setf (sdl-base::rect-w (fp rectangle)) w-val))

(defmethod height ((rectangle rectangle))
  "Returns the `INTEGER` height of the rectangle `RECTANGLE`."
  (sdl-base::rect-h (fp rectangle)))
(defmethod (setf height) (h-val (rectangle rectangle))
  "Sets the `INTEGER` height of the rectangle `RECTANGLE`."
  (setf (sdl-base::rect-h (fp rectangle)) h-val))

(defmethod point-* ((rectangle rectangle))
  "Returns the `X` and `Y` coordinates of the rectangle `RECTANGLE` as a spread. 
  The `RESULT` is `\(VALUES X Y\)`"
  (values (x rectangle) (y rectangle)))

(defmethod get-point ((rectangle rectangle))
  "Returns the `X` and `Y` coordinates of rectangle `RECTANGLE` as a `POINT`."
  (vector (x rectangle) (y rectangle)))

(defmethod set-point ((rectangle rectangle) (position vector))
  "Copies the `X` and `Y` coordinates to the destination rectangle `RECTANGLE` from the source point `POSITION`."
  (set-rectangle-* rectangle :x (x position) :y (y position)))

(defmethod set-point-* ((rectangle rectangle) &key x y)
  "Sets the `X` and `Y` coordinates of the rectangle `RECTANGLE`. `X` and `Y` are `KEY`word parameters."
  (set-rectangle-* rectangle :x x :y y))

(defmethod position-* ((rectangle rectangle))
  "See [POINT-*](#point-*)"
  (values (x rectangle) (y rectangle)))

(defmethod get-position ((rectangle rectangle))
  "See [GET-POINT](#get-POINT)"
  (vector (x rectangle) (y rectangle)))

(defmethod set-position ((rectangle rectangle) (position vector))
  "Sets the `X` and `Y` coordinates of the rectangle `RECTANGLE` from the point `POSITION`."
  (set-rectangle-* rectangle :x (x position) :y (y position)))

(defmethod set-position-* ((rectangle rectangle) &key x y)
  "Sets the `X` and `Y` coordinates of the rectangle `RECTANGLE`. `X` and `Y` are `KEY`word parameters."
  (set-rectangle-* rectangle :x x :y y))

(defmethod rectangle-* ((rectangle rectangle))
  "Returns the `X`, `Y`, `WIDTH` and `HEIGHT` coordinates of the rectangle `RECTANGLE` as a spread. 
The `RESULT` is `\(VALUES X Y WIDTH HEIGHT\)`"
  (values (x rectangle) (y rectangle) (width rectangle) (height rectangle)))

(defmethod get-rectangle ((rectangle rectangle))
  "Returns the rectangle `RECTANGLE`."
  rectangle)

(defmethod set-rectangle ((dst rectangle) (src rectangle))
  "Copies the `X`, `Y`, `WIDTH` and `HEIGHT` coordinates to the destination rectangle `DST` from the source rectangle `SRC`."
  (set-rectangle-* dst :x (x src) :y (y src) :w (width src) :h (height src)))

(defmethod set-rectangle-* ((rectangle rectangle) &key x y w h)
  "Sets the coordinates of the rectangle `RECTANGLE` to the specified `X`, `Y` , width `W` and height `HEIGHT` coordinates.
`X`, `Y`, `W` and `H` are `KEY`word parameters of type `INTEGER`. 
Returns the rectangle `RECTANGLE` as RESULT."
  (sdl-base::with-rectangle (rect (fp rectangle) nil)
    (when x (setf rect.x (sdl-base:to-int x)))
    (when y (setf rect.y (sdl-base:to-int y)))
    (when w (setf rect.w (sdl-base:to-int w)))
    (when h (setf rect.h (sdl-base:to-int h))))
  rectangle)

;; (defmethod color-* ((rectangle rectangle))
;;   "Returns the color of the rectangle `RECTANGLE`."
;;   (color-* (rectangle-color rectangle)))

;; (defmethod set-color ((rectangle rectangle) (color color))
;;   (set-color-* rectangle :r (r color) :g (g color) :b (b color) :a (a color))
;;   rectangle)

;; (defmethod set-color-* ((rectangle rectangle) &key r g b a)
;;   (when r (setf (r (rectangle-color rectangle)) r))
;;   (when g (setf (g (rectangle-color rectangle)) g))
;;   (when b (setf (b (rectangle-color rectangle)) b))
;;   (when a (setf (a (rectangle-color rectangle)) a))
;;   rectangle)
