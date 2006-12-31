;;;; simple fixed height and width font

(in-package :lispbuilder-sdl)

(defstruct font
  surface width height char-map key-color cached)

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

(defun make-text-image (string &key
			(cache nil) (font *default-font*))
  "given an initialised font and a string, draw the string to a surface and return the surface pointer"
  (let ((surface-width (* (font-width font) (length string)))
	(surface-height (font-height font)))
    (with-surface (surface (sdl::surface (sdl-base::create-surface surface-width surface-height
							   :surface (sdl::fp sdl::*default-display*)
							   :accel t)
					 nil))
      (sdl::set-color-key (font-key-color font))
      (sdl::fill-surface (font-key-color font))
      (draw-string string 0 0
		   :font font)
      (when cache
	(setf (font-cached font) surface))
      surface)))


(defun draw-character (font x y char &key
		       (surface sdl::*default-surface*))
  "draw a single character at the x y position"
  (let ((image (font-surface font))
	(w (font-width font))
	(h (font-height font))
	(char-offset (get-char-offset font char)))
    (if char-offset
	(sdl-base::with-rectangles ((src-rect) (dst-rect))
	  (setf src-rect.x (* w char-offset)
		src-rect.y 0
		src-rect.width w
		src-rect.height h)
	  (setf dst-rect.x x
		dst-rect.y y)
	  (sdl-base::blit-surface (sdl::fp image)
				  (sdl::fp surface)
				  src-rect
				  dst-rect
				  :update-p nil))
	nil)))

(defun draw-string (str x y &key
		    (justify :left)
		    (surface sdl::*default-surface*)
		    (font sdl::*default-font*))
  "draw a string at the x y position"
  (case justify
    (:left (draw-string-left-justify str x y :surface surface :font font))
    (:right (draw-string-right-justify str x y :surface surface :font font))
    (:center (draw-string-centered str x y :surface surface :font font))
    (otherwise (error ":JUSTIFY must be one of :LEFT, :RIGHT or :CENTER"))))

(defun draw-string-left-justify (str x y &key
				  (surface sdl::*default-surface*)
				  (font sdl::*default-font*))
  (loop for c across str do
       (unless (eql c #\space)
	 (draw-character font x y c
			 :surface surface))
       (incf x (font-width font))))

(defun draw-string-right-justify (str x y &key
				  (surface sdl::*default-surface*)
				  (font sdl::*default-font*))
  "draw a string ending at the x y position"
  (let ((right-x (- x (font-width font)))
	(rev-str (reverse str)))
    (loop for c across rev-str do
	 (unless (eql c #\space)
	   (draw-character font right-x y c
			   :surface surface))
	 (decf right-x (font-width font)))))

(defun draw-string-centered (str x y &key
			     (surface sdl::*default-surface*)
			     (font sdl::*default-font*))
  "draw a string centered at x y"
  (let* ((width (* (length str) (font-width font)))
	 (left-x (- x (/ width 2))))
    (loop for c across str do
	 (unless (eql c #\space)
	   (draw-character font left-x y c
			   :surface surface))
	 (incf left-x (font-width font)))))

(defmacro with-open-font ((font-image-name font-width font-height char-map-string key-color &optional (font-path ""))
			  &body body)
  `(let ((sdl::*default-font* (initialise-font ,font-image-name ,font-path
					       ,font-width ,font-height ,char-map-string ,key-color)))
     (if sdl::*default-font*
	 (progn
	   ,@body
	   (free-font sdl::*default-font*)))))

(defun initialise-default-font ()
  (let ((font-name "font.bmp")
	(font-path *default-font-path*)
	(width 4)
	(height 5)
	(char-map-string "abcdefghijklmnopqrstuvwxyz:'!?_-,.()#~0123456789")
	(key-color (color :r 99 :g 0 :b 0)))
    (unless (font-p *default-font*)
      (setf *default-font* (initialise-font font-name font-path width height char-map-string key-color)))))
