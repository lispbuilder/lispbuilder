;;;; simple fixed height and width font

(defpackage :sdl-simple-font
  (:use #:cl #:lispbuilder-sdl)
  (:export 
   #:initialise-font #:close-font #:make-text-image #:draw-text-image
   #:free-text-image #:draw-character #:draw-string 
   #:draw-string-right-justify #:draw-string-centered))

(in-package :sdl-simple-font)

(defstruct font
  bmp w h char-map key-color)

(defun make-char-map(str)
  "given a string of characters make a hash table which returns an index from 0 to n where 0 is the first char, and n is the last"
  (let ((char-map (make-hash-table :test 'equal :size (length str))))
    (loop for n by 1 
	  for c across str 
	  do 
	  (setf (gethash c char-map) n))
    char-map))

(defun get-char-offset(font char)
  (gethash char (font-char-map font)))

(defun initialise-font(bmp-file-name font-width font-height char-map-string key-color)
  "initialise a simple font using a bmp with a strip of fixed width characters mapped by the char-map-string"
  (let ((font-surface (sdl:convert-surface-to-display-format :surface (sdl:load-bmp bmp-file-name) :key-color key-color)))
    (when font-surface
      (make-font :bmp font-surface 
		 :w font-width 
		 :h font-height 
		 :char-map (make-char-map char-map-string)
		 :key-color key-color))))

(defun close-font(font)
  "free up the font image surface"
  (if (font-bmp font)
      (sdl:SDL_FreeSurface (font-bmp font))))

(defun draw-text-image(screen text-image point &key free-text)
  "draw a prerendered string of text"
  (sdl:blit-surface :src text-image :dst screen :dst-rect point :free-p free-text))

(defun make-text-image(font string)
  "given an initialised font and a string, draw the string to a surface and return the surface pointer"
  (let ((surface-width (* (font-w font) (length string)))
	(surface-height (font-h font)))
    (let ((surface (sdl:create-surface surface-width surface-height
				       :surface (sdl:sdl_getvideosurface)  
				       :key-color (font-key-color font)
				       :accel t)))
      (sdl:fill-surface :surface surface :color (font-key-color font))
      (draw-string surface font #(0 0) string)
      surface)))

(defun free-text-image(image)
  "free the sdl surface a text image allocated"
  (if image
      (sdl:SDL_FreeSurface image)))  

(defun draw-character(screen font point char)
  "draw a single character at the x y position"
  (let ((image (font-bmp font))
	(w (font-w font))
	(h (font-h font))
	(char-offset (get-char-offset font char)))
    (if char-offset
	(sdl:blit-surface :src image
			  :dst screen
			  :src-rect (vector (* w char-offset) 0 w h) 
			  :dst-rect point)
	nil)))

(defun draw-string(screen font point str)
  "draw a string at the x y position"
  (loop for c across str do
	(unless (eql c #\space)
	  (draw-character screen font point c))
	(incf (sdl:point-x point) (font-w font))))

(defun draw-string-right-justify(screen font point str)
  "draw a string ending at the x y position"
  (let ((right-x (- (sdl:point-x point) (font-w font)))
	(rev-str (reverse str)))
    (loop for c across rev-str do
	  (unless (eql c #\space)
	    (draw-character screen font (sdl:point right-x (sdl:point-y point)) c))
	  (decf right-x (font-w font)))))

(defun draw-string-centered(screen font point str)
  "draw a string centered at x y"
  (let ((width (* (length str) (font-w font)))) 
    (let ((left-x (- (sdl:point-x point) (/ width 2))))
      (loop for c across str do
	    (unless (eql c #\space)
	      (draw-character screen font (sdl:point left-x (sdl:point-y point)) c))
	    (incf left-x (font-w font))))))

