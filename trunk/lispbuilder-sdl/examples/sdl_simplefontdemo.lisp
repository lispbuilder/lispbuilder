;;;; demo of simple fixed with font code
; (C)2006 Justin Heyes-Jones

(in-package #:sdl-examples)  

; set load path
(defvar *font-path* (merge-pathnames "font.bmp" (or *load-truename* *default-pathname-defaults*)))
 
; window or screen height
(defparameter *WINDOW-WIDTH* 640)
(defparameter *WINDOW-HEIGHT* 480)
(defparameter *display-surface* nil)

(defun screen-center-x() (ash *window-width* -1))
(defun screen-center-y() (ash *window-height* -1))

(defun simple-font-demo()
  "example of simple font"
  (let ((width *window-width*) (height *window-height*))
    (sdl::with-init ();Initialize Systems
      ; init your game
      (sdl::set-framerate 10) ; Set target framerate (or 0 for unlimited)
      (let ((display (sdl::set-window width height :flags '(sdl::SDL_ANYFORMAT))))
	(let* ((small-font 
	       (sdl-simple-font:initialise-font (namestring *font-path*) 4 5 
						"abcdefghijklmnopqrstuvwxyz:'!?_-,.()#~0123456789" 99 0 0))
	      (text-image (sdl-simple-font:make-text-image small-font "draw text image")))
	  (sdl::with-events 
	    (:quit t)
	    (:idle
	     ;; fill the background
	     (sdl::fill-surface display #x22 #x22 #x44)
	   ; Do stuff
	     (sdl-simple-font:draw-string-right-justify display small-font 
					     (1- *WINDOW-WIDTH*) (screen-center-y)
					     "draw string right justified")
	     (sdl-simple-font:draw-string display small-font 
					     (+ (- (screen-center-x) 100) (random 200))
					     (+ (- (screen-center-y) 100) (random 200))
					     "draw string")
	     (sdl-simple-font:draw-text-image display text-image 
					     (+ (- (screen-center-x) 100) (random 200))
					     (+ (- (screen-center-y) 100) (random 200)))
	     (sdl-simple-font:draw-string-centered display small-font 
						   (screen-center-x) (screen-center-y)
					     "draw string centered")
 	   ;Update the whole screen 
	     (sdl::update-surface display)))
	  (sdl-simple-font:free-text-image text-image)
	  (sdl-simple-font:close-font small-font))))))
