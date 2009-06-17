;;;; Simple OpenGL example
;;;; Shows usage of cl-opengl from lispbuilder

; You need to asdf install cl-opengl and load up these systems
; for this example

;(asdf:oos 'asdf:load-op 'cl-opengl)
;(asdf:oos 'asdf:load-op 'cl-glu)
;(asdf:oos 'asdf:load-op 'cl-glut) ; not needed 

;; Application specific parameters
(defparameter *screen-width* 400)
(defparameter *screen-height* 400)

; Many thanks, Chris Double for macros
; Justinhj NOTE these are not used here but they're handy
; and a useful pattern 
(defmacro with-glBegin (type &body body)
  `(progn
    (gl::begin ,type)
    (unwind-protect
	 (progn ,@body)
      (gl::end))))

(defmacro with-glPushMatrix (&body body)
  `(progn
    (gl::push-matrix)
    (unwind-protect
	 (progn ,@body)
      (gl::pop-matrix))))
 
(defun init (width height)
  "Basic OpenGL setup"
  (gl::viewport 0 0 width height))

(defun setup(width height)
  "Setup your app"
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (if (<= width height)
      (glu:ortho-2d 0 30 0 (* 30 (/ height width)))
      (glu:ortho-2d 0 (* 30 (/ width height)) 0 30))
  (gl:matrix-mode :modelview)
  (gl:clear-color 0 0 0 0)
  (gl:shade-model :smooth))

(defun draw()
  "Draw function called each frame update"
  (gl:clear :color-buffer)

  (gl:with-primitives :triangles
    (gl:color 1 0 0)
    (gl:vertex 5 25)
    (gl:color 0 0 1)
    (gl:vertex 5 5)
    (gl:color 0 1 0)
    (gl:vertex 25 5)

    (gl:color 0 0 1)
    (gl:vertex 25 25)
    (gl:color 0 1 0)
    (gl:vertex 25 5)
    (gl:color 1 0 0)
    (gl:vertex 5 25))

  (gl:flush))

(defun opengl-simple ()
  (sdl:with-init ()
    (sdl:window *screen-width* *screen-height*
		:flags sdl-cffi::sdl-opengl
		:title-caption "OpenGL Simple"
		:icon-caption "OpenGL Simple")
    (init *screen-width* *screen-height*)
    (setup *screen-width* *screen-height*)
    (setf (sdl:frame-rate) 30)
    (sdl:with-events ()
      (:quit-event () t)
      (:key-down-event (:key key)
		       (if (sdl:key= key :SDL-KEY-ESCAPE)
			   (sdl:push-quit-event)))
      (:idle ()         
	     (draw)
	     (sdl:update-display)))))


