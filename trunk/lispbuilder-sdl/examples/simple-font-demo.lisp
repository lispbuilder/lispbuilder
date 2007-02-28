;;;; demo of simple fixed with font code
; (C)2006 Justin Heyes-Jones

(in-package #:sdl-examples)  
 
; window or screen height
(defparameter *WINDOW-WIDTH* 640)
(defparameter *WINDOW-HEIGHT* 480)

(defun screen-center-x() (ash *window-width* -1))
(defun screen-center-y() (ash *window-height* -1))

(defun simple-font-demo()
  "example of simple font"
  (sdl:with-init ()			;Initialize Systems
    ;; init your game
    (sdl:window *WINDOW-WIDTH* *WINDOW-HEIGHT*)
    (setf (sdl:frame-rate) 5) ; Set target framerate (or 0 for unlimited)

    (let ((1st-font (sdl:initialise-default-font sdl:*font-10x20*))
	  (2nd-font (sdl:initialise-default-font sdl:*font-10x20*)))
      
      (sdl:with-events  ()
	(:quit-event () t)
	(:idle ()
	       ;; fill the background
	       (sdl:clear-display (sdl:color :r #x22 :g #x22 :b #x44))
	       ;; Do stuff
	       (sdl:draw-string-shaded-* "draw string random"
					 (+ (- (screen-center-x) 100) (random 200))
					 (+ (- (screen-center-y) 100) (random 200))
					 sdl:*black* sdl:*white* :font 2nd-font)
	       (sdl:draw-string-solid-* "draw string centered" (screen-center-x) (- (screen-center-y) 10)
					:justify :center :color sdl:*white* :font 1st-font)
	       (sdl:draw-string-solid-* "draw string left justified" (screen-center-x) (screen-center-y)
					:justify :left :color sdl:*white* :font 1st-font)
	       (sdl:draw-string-solid-* "draw string right justified" (screen-center-x) (+ (screen-center-y) 10)
					:justify :right :color sdl:*white* :font 1st-font)
	;; Update the whole screen 
	       (sdl:update-display))))))
