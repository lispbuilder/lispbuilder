;;;; demo of simple fixed with font code
; (C)2006 Justin Heyes-Jones

(in-package #:sdl-examples)  
 
; window or screen height
(defparameter *WINDOW-WIDTH* 640)
(defparameter *WINDOW-HEIGHT* 480)
(defparameter *display-surface* nil)

(defun screen-center-x() (ash *window-width* -1))
(defun screen-center-y() (ash *window-height* -1))

(defun simple-font-demo()
  "example of simple font"
  (sdl:with-init ()			;Initialize Systems
    ;; init your game
    (sdl:set-framerate 2) ; Set target framerate (or 0 for unlimited)
    (sdl:with-display (*WINDOW-WIDTH* *WINDOW-HEIGHT* :flags sdl:SDL_ANYFORMAT)
      (let* ((small-font 
	      (sdl-simple-font:initialise-font (namestring (merge-pathnames "font.bmp" *font-path*)) 4 5
					       "abcdefghijklmnopqrstuvwxyz:'!?_-,.()#~0123456789" #(99 0 0)))
	     (text-image (sdl-simple-font:make-text-image small-font "draw text image")))
	(sdl:with-events  ()
	  (:quit () t)
	  (:idle ()
		 ;; fill the background
		 (sdl:clear-display :color (vector #x22 #x22 #x44))
		 ;; Do stuff
		 (sdl-simple-font:draw-string-right-justify "draw string right justified"
							    :surface sdl:*default-display*
							    :font small-font 
							    :position (sdl:point (1- *WINDOW-WIDTH*) (screen-center-y)))
		 (sdl-simple-font:draw-string "draw string"
					      :font small-font 
					      :position (sdl:point (+ (- (screen-center-x) 100) (random 200))
								   (+ (- (screen-center-y) 100) (random 200)))) 
		 (sdl-simple-font:draw-text-image sdl:*default-display* text-image 
						  (sdl:point (+ (- (screen-center-x) 100) (random 200))
							     (+ (- (screen-center-y) 100) (random 200))))
		 (sdl-simple-font:draw-string-centered "draw string centered"
						       :surface sdl:*default-display*
						       :font small-font 
						       :position (sdl:point (screen-center-x) (screen-center-y)))
		 ;; Update the whole screen 
		 (sdl:update-display)))
	(sdl-simple-font:free-text-image text-image)
	(sdl-simple-font:close-font small-font)))))
