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
    (setf (sdl:frame-rate) 2) ; Set target framerate (or 0 for unlimited)

    (sdl:initialise-default-font)
    ;; Create an image from the string.
    ;; Then cache this image in the font to be used later.
    (sdl:make-text-image "draw text image" :cache t)

    (sdl:with-events  ()
      (:quit-event () t)
      (:idle ()
	     ;; fill the background
	     (sdl:clear-display (sdl:color :r #x22 :g #x22 :b #x44))
	     ;; Do stuff
	     (sdl:with-surface (disp sdl:*default-display*)

	       ;; Draw the font using the previously
	       ;; created cached image.
	       (sdl:draw-font-* (+ (- (screen-center-x) 100) (random 200))
				(+ (- (screen-center-y) 100) (random 200)))

	       (sdl:draw-string-* "draw string centered" (screen-center-x) (- (screen-center-y) 10)
				  :justify :center)
	       (sdl:draw-string-* "draw string left justified" (screen-center-x) (screen-center-y)
				  :justify :left)
	       (sdl:draw-string-* "draw string right justified" (screen-center-x) (+ (screen-center-y) 10)
				  :justify :right))
	     ;; Update the whole screen 
	     (sdl:update-display)))))
