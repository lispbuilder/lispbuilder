;;;; Demonstration/Test of using the OpenGl gl library
;;;; using CFFI for foreign function interfacing...
;;;; (C)2006 Luke Crook
;;;; Updates by Justin Heyes-Jones
;;;; see COPYING for license

;;; The famous OpenGL gears example, by Brian Paul.
;;; 
;;; Operation:
;;; - Click & hold left mouse button and move mouse to rotate the gears in the x/y planes
;;; 
;;; Issues:
;;; - Do NOT attempt to run two instances of this program from your Lisp environment simultaneously. 
;;;   The SDL library is currently limited to a single instance only. Only a single OpenGL window may 
;;;   be open at any one time. SDL is a GAMES library, not a general purpose windowing library.

;;;; To run these samples
;;;; (opengl-gears)

(in-package #:lispbuilder-opengl-examples)

;; Generic parameters
(defparameter *world-ticks* 15)	; decouples any rotation or movement from the speed of the hardware. 
(defparameter *timescale* 0.3)

;; Application specific parameters
(defparameter *screen-width* 640)
(defparameter *screen-height* 480)
(defparameter *gear1* nil)
(defparameter *gear2* nil)
(defparameter *gear3* nil)
(defparameter *angle* 0)        ; Stores the angle of rotation for the gears.
(defparameter *view_rotx* 20.0)
(defparameter *view_roty* 30.0)
(defparameter *view_rotz* 0.0)

(defparameter *draw-outline* nil) ; Draws a white wireframe outline around each polygon

;;; Begin macro definitions.
; Many thanks, Chris Double
(defmacro with-glBegin (type &body body)
  `(progn
    (gl::glBegin ,type)
    (unwind-protect
	 (progn ,@body)
      (gl::glEnd))))

(defmacro with-glPushMatrix (&body body)
  `(progn
    (gl::glPushMatrix)
    (unwind-protect
	 (progn ,@body)
      (gl::glPopMatrix))))
 
;;; End macro definitions
;;;

;;;
;;; End SDL Events

(defun create-gear (inner_radius outer_radius width teeth tooth_depth)
  (let* ((ex_pi pi)
	 (r0 inner_radius)
	 (r1 (/ (- outer_radius tooth_depth) 2.0))
	 (r2 (/ (+ outer_radius tooth_depth) 2.0))
	 (angle 0.0)
	 (da (/ (/ (* 2.0 ex_pi) teeth) 4.0))
	 (u 0.0)
	 (v 0.0)
	 (len 0.0))
        
    (gl::glShadeModel gl::GL_FLAT)
        
    (gl::glNormal3f 0.0 0.0 1.0)        
					;draw front face
    (with-glBegin gl::GL_QUAD_STRIP
      (dotimes (i (+ 1 teeth))
	(setf angle (/ (* i 2.0 ex_pi) teeth))
	(gl::glVertex3f (coerce (* r0 (cos angle)) 'single-float)
			(coerce (* r0 (sin angle)) 'single-float)
			(coerce (* width 0.5) 'single-float))
	(gl::glVertex3f (coerce (* r1 (cos angle)) 'single-float)
			(coerce (* r1 (sin angle)) 'single-float)
			(coerce (* width 0.5) 'single-float))
	(if (< i teeth)
	    (progn
	      (gl::glVertex3f (coerce (* r0 (cos angle)) 'single-float)
			      (coerce (* r0 (sin angle)) 'single-float)
			      (coerce (* width 0.5) 'single-float))
	      (gl::glVertex3f (coerce (* r1 (cos (+ (* 3 da) angle))) 'single-float)
			      (coerce (* r1 (sin (+ (* 3 da) angle))) 'single-float)
			      (coerce (* width 0.5) 'single-float))))))
        
					;draw front sides of teeth
    (with-glBegin gl::GL_QUADS
      (setf da (/ (/ (* 2.0 ex_pi)
		     teeth) 
		  4.0))            
      (dotimes (i (+ 1 teeth))
	(setf angle (/ (* i 2.0 ex_pi) teeth))
	(gl::glVertex3f (coerce (* r1 (cos angle)) 'single-float)
			(coerce (* r1 (sin angle)) 'single-float)
			(coerce (* width 0.5) 'single-float))
	(gl::glVertex3f (coerce (* r2 (cos (+ angle da))) 'single-float)
			(coerce (* r2 (sin (+ angle da))) 'single-float)
			(coerce (* width 0.5) 'single-float))
	(gl::glVertex3f (coerce (* r2 (cos (+ angle (* 2 da)))) 'single-float)
			(coerce (* r2 (sin (+ angle (* 2 da)))) 'single-float)
			(coerce (* width 0.5) 'single-float))
	(gl::glVertex3f (coerce (* r1 (cos (+ angle (* 3 da)))) 'single-float)
			(coerce (* r1 (sin (+ angle (* 3 da)))) 'single-float)
			(coerce (* width 0.5) 'single-float))))
    
    (gl::glNormal3f 0.0 0.0 (- 1.0))
        
					;draw back face
    (with-glBegin gl::GL_QUAD_STRIP
      (dotimes (i (+ 1 teeth))
	(setf angle (/ (* i 2.0 ex_pi)
		       teeth))
	(gl::glVertex3f (coerce (* r1 (cos angle)) 'single-float)
			(coerce (* r1 (sin angle)) 'single-float)
			(coerce (* (- width) 0.5) 'single-float))
	(gl::glVertex3f (coerce (* r0 (cos angle)) 'single-float)
			(coerce (* r0 (sin angle)) 'single-float)
			(coerce (* (- width) 0.5) 'single-float))
	(if (< i teeth)
	    (progn
	      (gl::glVertex3f (coerce (* r1 (cos (+ (* 3 da) angle))) 'single-float)
			      (coerce (* r1 (sin (+ (* 3 da) angle))) 'single-float)
			      (coerce (* (- width) 0.5) 'single-float))
	      (gl::glVertex3f (coerce (* r0 (cos angle)) 'single-float)
			      (coerce (* r0 (sin angle)) 'single-float)
			      (coerce (* (- width) 0.5) 'single-float))))))

					;draw back sides of teeth
    (with-glBegin gl::GL_QUADS
      (setf da (/ (/ (* 2.0 ex_pi)
		     teeth) 
		  4.0))            
      (dotimes (i (+ 1 teeth))
	(setf angle (/ (* i 2.0 ex_pi) teeth))
	(gl::glVertex3f (coerce (* r1 (cos (+ angle (* 3 da)))) 'single-float)
			(coerce (* r1 (sin (+ angle (* 3 da)))) 'single-float)
			(coerce (* (- width) 0.5) 'single-float))
	(gl::glVertex3f (coerce (* r2 (cos (+ angle (* 2 da)))) 'single-float)
			(coerce (* r2 (sin (+ angle (* 2 da)))) 'single-float)
			(coerce (* (- width) 0.5) 'single-float))
	(gl::glVertex3f (coerce (* r2 (cos (+ angle da))) 'single-float)
			(coerce (* r2 (sin (+ angle da))) 'single-float)
			(coerce (* (- width) 0.5) 'single-float))
	(gl::glVertex3f (coerce (* r1 (cos angle)) 'single-float)
			(coerce (* r1 (sin angle)) 'single-float)
			(coerce (* (- width) 0.5) 'single-float))))
        
					;draw outward faces of teeth
    (with-glBegin gl::GL_QUAD_STRIP
      (dotimes (i (+ 1 teeth))
	(setf angle (/ (* i 2.0 ex_pi) teeth))            
	(gl::glVertex3f (coerce (* r1 (cos angle)) 'single-float)
			(coerce (* r1 (sin angle)) 'single-float)
			(coerce (* width 0.5) 'single-float))
	(gl::glVertex3f (coerce (* r1 (cos angle)) 'single-float)
			(coerce (* r1 (sin angle)) 'single-float)
			(coerce (* (- width) 0.5) 'single-float))
	(setf u (- (* r2 (cos (+ angle da)))
		   (* r1 (cos angle))))
	(setf v (- (* r2 (sin (+ angle da)))
		   (* r1 (sin angle))))
                
	(setf len (sqrt (+ (* u u) 
			   (* v v))))
	(setf u (/ u len))
	(setf v (/ v len))
    
	(gl::glNormal3f (coerce v 'single-float)
			(coerce (- u) 'single-float)
			(coerce 0.0 'single-float))
	(gl::glVertex3f (coerce (* r2 (cos (+ angle da))) 'single-float)
			(coerce (* r2 (sin (+ angle da))) 'single-float)
			(coerce (* width 0.5) 'single-float))
	(gl::glVertex3f (coerce (* r2 (cos (+ angle da))) 'single-float)
			(coerce (* r2 (sin (+ angle da))) 'single-float)
			(coerce (* (- width) 0.5) 'single-float))
	(gl::glNormal3f (coerce (cos angle) 'single-float)
			(coerce (sin angle) 'single-float)
			(coerce 0.0 'single-float))
	(gl::glVertex3f (coerce (* r2 (cos (+ angle (* 2 da)))) 'single-float)
			(coerce (* r2 (sin (+ angle (* 2 da)))) 'single-float)
			(coerce (* width 0.5) 'single-float))
	(gl::glVertex3f (coerce (* r2 (cos (+ angle (* 2 da)))) 'single-float)
			(coerce (* r2 (sin (+ angle (* 2 da)))) 'single-float)
			(coerce (* (- width) 0.5) 'single-float))
	(setf u (- (* r1 (cos (+ angle (* 3 da))))
		   (* r2 (cos (+ angle (* 2 da))))))
	(setf v (- (* r1 (sin (+ angle (* 3 da))))
		   (* r2 (sin (+ angle (* 2 da))))))
	(gl::glNormal3f (coerce v 'single-float)
			(coerce (- u) 'single-float)
			(coerce 0.0 'single-float))
	(gl::glVertex3f (coerce (* r1 (cos (+ angle (* 3 da)))) 'single-float)
			(coerce (* r1 (sin (+ angle (* 3 da)))) 'single-float)
			(coerce (* width 0.5) 'single-float))
	(gl::glVertex3f (coerce (* r1 (cos (+ angle (* 3 da)))) 'single-float)
			(coerce (* r1 (sin (+ angle (* 3 da)))) 'single-float)
			(coerce (* (- width) 0.5) 'single-float))
	(gl::glNormal3f (coerce (cos angle) 'single-float)
			(coerce (sin angle) 'single-float)
			(coerce 0.0 'single-float)))
            
      (gl::glVertex3f (coerce (* r1 (cos 0)) 'single-float)
		      (coerce (* r1 (sin 0)) 'single-float)
		      (coerce (* width 0.5) 'single-float))
      (gl::glVertex3f (coerce (* r1 (cos 0)) 'single-float)
		      (coerce (* r1 (sin 0)) 'single-float)
		      (coerce (* (- width) 0.5) 'single-float)))
        
    (gl::glShadeModel gl::GL_SMOOTH)
        
					;draw inside radius cylinder */
    (with-glBegin gl::GL_QUAD_STRIP
      (dotimes (i (+ 1 teeth))
	(setf angle (/ (* i 2.0 ex_pi) teeth))
	(gl::glNormal3f (coerce (- (cos angle)) 'single-float)
			(coerce (- (sin angle)) 'single-float)
			(coerce 0.0 'single-float))
	(gl::glVertex3f (coerce (* r0 (cos angle)) 'single-float)
			(coerce (* r0 (sin angle)) 'single-float)
			(coerce (* (- width) 0.5) 'single-float))
	(gl::glVertex3f (coerce (* r0 (cos angle)) 'single-float)
			(coerce (* r0 (sin angle)) 'single-float)
			(coerce (* width 0.5) 'single-float))))))
    
(defun move-objects ()
  ;; Insert a delay between screen updates. Used in testing to simulate 
  ;; heavy processor load / varying frame rates.
  (incf *angle* (* 1 *timescale*)))

(defun draw-objects ()
  (with-glPushMatrix
    (gl::glRotatef (coerce *view_rotx* 'single-float) 0.0 1.0 0.0)
    (gl::glRotatef (coerce *view_roty* 'single-float) 1.0 0.0 0.0)    
    (gl::glRotatef (coerce *view_rotz* 'single-float) 0.0 0.0 1.0)
    
    (with-glPushMatrix
      (gl::glTranslatef (- 3.0) (- 2.0) 0.0)
      (gl::glRotatef (coerce *angle* 'single-float) 0.0 0.0 1.0)
      (gl::glCallList *gear1*))
    
    (with-glPushMatrix
      (gl::glTranslatef 3.1 (- 2.0) 0.0)
      (gl::glRotatef (coerce (- (* (- 2.0) *angle*) 9.0) 'single-float) 0.0 0.0 1.0)
      (gl::glCallList *gear2*))
    
    (with-glPushMatrix
      (gl::glTranslatef (- 3.1) 4.2 0.0)
      (gl::glRotatef (coerce (- (* (- 2.0) *angle*) 25.0) 'single-float) 0.0 0.0 1.0)
      (gl::glCallList *gear3*))))

(defun draw-screen ()
  (gl::glClear (sdl-base::set-flags gl::GL_COLOR_BUFFER_BIT gl::GL_DEPTH_BUFFER_BIT))
    
  (gl::glEnable gl::GL_LIGHTING)
  (gl::glEnable gl::GL_LIGHT0)
    
  (when *draw-outline*
    (gl::glEnable gl::GL_POLYGON_OFFSET_FILL)
    (gl::glPolygonOffset 1.0 1.0))
    
  (draw-objects)
    
  (when *draw-outline*
    (gl::glDisable gl::GL_POLYGON_OFFSET_FILL)
    (gl::glDisable gl::GL_LIGHTING)
    (gl::glDisable gl::GL_LIGHT0)
    (gl::glColor3f 1.0 1.0 1.0)
    (gl::glPolygonMode gl::GL_FRONT_AND_BACK gl::GL_LINE)
    
    (draw-objects)
            
    (gl::glPolygonMode gl::GL_FRONT_AND_BACK gl::GL_FILL))
    
  (sdl-cffi::sdl-gl-swap-buffers))

(defun setup-opengl (width height)
  (let ((screen-ratio (coerce (/ height width) 'double-float)))        
    (gl::glViewport 0 0 width height)
    (gl::glMatrixMode gl::GL_PROJECTION)
    (gl::glLoadIdentity)
    (gl::glFrustum (- 1.0d0) 
		   1.0d0 
		   (- screen-ratio) 
		   screen-ratio 
		   5.0d0 
		   60.0d0)
    (gl::glMatrixMode gl::GL_MODELVIEW)
    (gl::glLoadIdentity)
    (gl::glTranslatef 0.0 0.0 (- 40.0))))

(defun create-display-lists ()
  (let ((light-position (cffi:foreign-alloc 'gl::GLfloat :count 4))
	(red (cffi:foreign-alloc 'gl::GLfloat :count 4))
	(green (cffi:foreign-alloc 'gl::GLfloat :count 4))        
	(blue (cffi:foreign-alloc 'gl::GLfloat :count 4)))
            
    (setf (cffi:mem-aref light-position 'gl::GLfloat 0) 5.0
	  (cffi:mem-aref light-position 'gl::GLfloat 1) 5.0
	  (cffi:mem-aref light-position 'gl::GLfloat 2) 10.0
	  (cffi:mem-aref light-position 'gl::GLfloat 3) 0.0)
    
    (setf (cffi:mem-aref red 'gl::GLfloat 0) 0.8
	  (cffi:mem-aref red 'gl::GLfloat 1) 0.1
	  (cffi:mem-aref red 'gl::GLfloat 2) 0.0
	  (cffi:mem-aref red 'gl::GLfloat 3) 1.0)
        
    (setf (cffi:mem-aref green 'gl::GLfloat 0) 0.0
	  (cffi:mem-aref green 'gl::GLfloat 1) 0.8
	  (cffi:mem-aref green 'gl::GLfloat 2) 0.2
	  (cffi:mem-aref green 'gl::GLfloat 3) 1.0)
    
    (setf (cffi:mem-aref blue 'gl::GLfloat 0) 0.2
	  (cffi:mem-aref blue 'gl::GLfloat 1) 0.2
	  (cffi:mem-aref blue 'gl::GLfloat 2) 1.0
	  (cffi:mem-aref blue 'gl::GLfloat 3) 1.0)    
    
    (gl::glLightfv gl::GL_LIGHT0 gl::GL_POSITION light-position)
   
    (gl::glEnable gl::GL_CULL_FACE)
    (gl::glEnable gl::GL_DEPTH_TEST)
    
					;Create the gears
    (setf *gear1* (gl::glGenLists 1))
    (gl::glNewList *gear1* gl::GL_COMPILE)
    (gl::glMaterialfv gl::GL_FRONT gl::GL_AMBIENT_AND_DIFFUSE red)
    (create-gear 1.0 8.0 1.0 20 0.7)
    (gl::glEndList)
    
    (setf *gear2* (gl::glGenLists 1))
    (gl::glNewList *gear2* gl::GL_COMPILE)
    (gl::glMaterialfv gl::GL_FRONT gl::GL_AMBIENT_AND_DIFFUSE green)
    (create-gear 0.5 4.0 2.0 10 0.7)
    (gl::glEndList)
    
    (setf *gear3* (gl::glGenLists 1))
    (gl::glNewList *gear3* gl::GL_COMPILE)
    (gl::glMaterialfv gl::GL_FRONT gl::GL_AMBIENT_AND_DIFFUSE blue)
    (create-gear 1.3 4.0 0.5 10 0.7)
    (gl::glEndList)
    
    (gl::glEnable gl::GL_NORMALIZE)))
    
(defun opengl-gears ()
  (sdl:with-init ()
    (sdl:window *screen-width* *screen-height*
		:flags sdl-cffi::sdl-opengl
		:title-caption "OpenGL Gears"
		:icon-caption "OpenGL Gears")
    (setup-opengl *screen-width* *screen-height*)
    (setf (sdl:frame-rate) 30)
    (create-display-lists)
    (sdl:with-events ()
      (:quit-event () t)
      (:key-down-event (:key key)
		       (if (sdl:key= key :SDL-KEY-ESCAPE)
			   (sdl:push-quit-event)))
      (:mouse-motion-event (:state state :x-rel xrel :y-rel yrel)
			   (cond
			     ((eql state 1)
			      (setf *view_rotx* (+ *view_rotx* xrel))
			      (setf *view_roty* (+ *view_roty* yrel )))))
      (:idle ()         
	     (move-objects)
	     (draw-screen)))))

;;; Some parameters to set while opengl-grears is running...
;;; (setf *timescale* 0.3)
;;; (setf *draw-outline* nil)
