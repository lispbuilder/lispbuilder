;;;; simple fixed height and width font

(defpackage :sdl-simple-font
  (:use #:cl #:lispbuilder-sdl)
  (:export 
   #:initialise-font #:close-font #:make-text-image #:draw-text-image
   #:free-text-image #:draw-character #:draw-string 
   #:draw-string-right-justify #:draw-string-centered))

(in-package :sdl-simple-font)

(defstruct font
  bmp w h char-map key-r key-g key-b)

(defun make-char-map(str)
  "given a string of characters make a hash table which returns an index from 0 to n where 0 is the first char, and n is the last"
  (let ((char-map (make-hash-table :test 'equal :size (length str))))
    (loop for n by 1 
	  for c across str 
	  do 
	  (setf (gethash c char-map) n))
    char-map))

(defun get-char-offset(font char)
  (let ((offset (gethash char (font-char-map font))))
    offset))

;;;; Should this be in util?
(defun load-image(filename &optional r g b)
  "load an image and sets transparency if r,g,b are non-null"
  (let ((loaded-image (sdl::load-bmp filename)))
    (when loaded-image
      (let ((optimized-image (sdl::SDL_DisplayFormat loaded-image)))
        (sdl::SDL_FreeSurface loaded-image)
	(when optimized-image
	  (if (and r g b)
	      (sdl::set-colorkey optimized-image r g b))
	  optimized-image)))))

(defun initialise-font(bmp-file-name font-width font-height char-map-string key-red key-green key-blue)
  "initialise a simple font using a bmp with a strip of fixed width characters mapped by the char-map-string"
  (let ((font-surface (load-image bmp-file-name key-red key-green key-blue)))
    (when font-surface
      (let ((font (make-font :bmp font-surface 
			     :w font-width 
			     :h font-height 
			     :char-map (make-char-map char-map-string)
			     :key-r key-red
			     :key-g key-green
			     :key-b key-blue)))
	font))))

(defun close-font(font)
  "free up the font image surface"
  (if (font-bmp font)
      (sdl::SDL_FreeSurface (font-bmp font))))

(defun draw-text-image(screen text-image x y)
  "draw a prerendered string of text"
  (let ((w (sdl::surf-w text-image))
	(h (sdl::surf-h text-image)))
    (sdl::apply-surface-free text-image
			     screen
			     :source-rect (sdl::rectangle 0 0 w h)
			     :destination-x x :destination-y y)))

(defun make-text-image(font string)
  "given an initialised font and a string, draw the string to a surface and return the surface pointer"
  (let ((surface-width (* (font-w font) (length string)))
	(surface-height (font-h font)))
    (let ((surface (sdl::create-surface (sdl::sdl_getvideosurface)  
					surface-width surface-height
					(font-key-r font) (font-key-g font) (font-key-b font))))
      (sdl::fill-surface surface (font-key-r font) (font-key-g font) (font-key-b font))
      (draw-string surface font 0 0 string)
      surface)))

(defun free-text-image(image)
  "free the sdl surface a text image allocated"
  (if image
      (sdl::SDL_FreeSurface image)))  

(defun draw-character(screen font x y char)
  "draw a single character at the x y position"
  (let ((image (font-bmp font)))
    (let ((w (font-w font))
	  (h (font-h font))
	  (char-offset (get-char-offset font char)))
      (if char-offset
	  (sdl::apply-surface-free image
				   screen
				   :source-rect (sdl::rectangle (* w char-offset) 0 w h) 
				   :destination-x x :destination-y y)
	nil))))

(defun draw-string(screen font x y str)
  "draw a string at the x y position"
  (loop for c across str do
	(unless (eql c #\space)
	  (draw-character screen font x y c))
	(incf x (font-w font))))

(defun draw-string-right-justify(screen font x y str)
  "draw a string ending at the x y position"
  (let ((right-x (- x (font-w font)))
	(rev-str (reverse str)))
    (loop for c across rev-str do
	  (unless (eql c #\space)
	    (draw-character screen font right-x y c))
	  (decf right-x (font-w font)))))

(defun draw-string-centered(screen font x y str)
  "draw a string centered at x y"
  (let ((width (* (length str) (font-w font)))) 
    (let ((left-x (- x (/ width 2))))
      (loop for c across str do
	    (unless (eql c #\space)
	      (draw-character screen font left-x y c))
	    (incf left-x (font-w font))))))

