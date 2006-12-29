;;;; simple fixed height and width font

(defpackage :sdl-simple-font
  (:use #:cl #:lispbuilder-sdl)
  (:export 
   #:initialise-font #:free-font #:make-text-image 
   #:free-text-image #:draw-character #:draw-string 
   #:draw-string-right-justify #:draw-string-centered
   #:with-open-font))

(in-package :sdl-simple-font)

(defstruct font
  surface width height char-map key-color)

(defun make-char-map (str)
  "given a string of characters make a hash table which returns an index from 0 to n where 0 is the first char, and n is the last"
  (let ((char-map (make-hash-table :test 'equal :size (length str))))
    (loop for n by 1 
	  for c across str 
	  do 
	  (setf (gethash c char-map) n))
    char-map))

(defun get-char-offset (font char)
  (gethash char (font-char-map font)))

(defun initialise-font (bmp-file-name bmp-path-name font-width font-height char-map-string key-color)
  "initialise a simple font using a bmp with a strip of fixed width characters mapped by the char-map-string"
  (let ((font-surface (sdl::load-image bmp-file-name bmp-path-name :key-color key-color)))
    (when font-surface
      (make-font :surface font-surface 
		 :width font-width 
		 :height font-height 
		 :char-map (make-char-map char-map-string)
		 :key-color key-color))))

(defun free-font (font)
  "free up the font image surface"
  (if (font-surface font)
      (sdl::free-surface (font-surface font))))

(defun make-text-image (font string)
  "given an initialised font and a string, draw the string to a surface and return the surface pointer"
  (let ((surface-width (* (font-width font) (length string)))
	(surface-height (font-height font)))
    (let ((surface (sdl::surface (sdl-base::create-surface surface-width surface-height
							   :surface (sdl::fp sdl::*default-display*)
							   :accel t))))
      (sdl::set-color-key (font-key-color font) :surface surface)
      (sdl::fill-surface (font-key-color font)
			 :dst surface)
      (draw-string string
		   :surface surface
		   :font font
		   :position (sdl::point :x 0 :y 0))
      surface)))

(defun draw-character (screen font point char)
  "draw a single character at the x y position"
  (let ((image (font-surface font))
	(w (font-width font))
	(h (font-height font))
	(char-offset (get-char-offset font char)))
    (if char-offset
	(sdl-base::blit-surface :src (sdl::fp image)
				:dst (sdl::fp screen)
				:src-rect (sdl::rectangle :x (* w char-offset)
							  :y 0
							  :w w :h h)
				:dst-rect (sdl::rectangle :x (sdl::x point)
							  :y (sdl::y point)))
	nil)))

(defun draw-string (str
		   &key (position sdl::*default-position*) (surface sdl::*default-surface*) (font sdl::*default-font*))
  "draw a string at the x y position"
  (let ((x (sdl::x position)))
    (loop for c across str do
	 (unless (eql c #\space)
	   (draw-character surface font (sdl::point :x x
						    :y (sdl::y position))
			   c))
	 (incf x (font-width font)))))

(defun draw-string-right-justify (str
				 &key (position sdl::*default-position*) (surface sdl::*default-surface*)
				 (font sdl::*default-font*))
  "draw a string ending at the x y position"
  (let ((right-x (- (sdl::x position) (font-width font)))
	(rev-str (reverse str)))
    (loop for c across rev-str do
	  (unless (eql c #\space)
	    (draw-character surface font (sdl::point :x right-x
						     :y (sdl::y position))
			    c))
	 (decf right-x (font-width font)))))

(defun draw-string-centered (str &key
			    (position sdl::*default-position*) (surface sdl::*default-surface*)
			    (font sdl::*default-font*))
  "draw a string centered at x y"
  (let* ((width (* (length str) (font-width font)))
	 (left-x (- (sdl::x position) (/ width 2))))
    (loop for c across str do
	 (unless (eql c #\space)
	   (draw-character surface font (sdl::point :x left-x
						    :y (sdl::y position))
			   c))
	 (incf left-x (font-width font)))))

(defmacro with-open-font ((font-image-name font-width font-height char-map-string key-color &optional (font-path ""))
			  &body body)
  `(let ((sdl::*default-font* (initialise-font ,font-image-name ,font-path
					       ,font-width ,font-height ,char-map-string ,key-color)))
     (if sdl::*default-font*
	 (progn
	   ,@body
	   (free-font sdl::*default-font*)))))
