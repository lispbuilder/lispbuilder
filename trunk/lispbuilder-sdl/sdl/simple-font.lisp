;;;; simple fixed height and width font

(in-package :lispbuilder-sdl)

(defclass font ()
  ((font-surface :reader font-surface :initform nil :initarg :font-surface)
   (font-width :reader font-width :initform nil :initarg :font-width)
   (font-height :reader font-height :initform nil :initarg :font-height)
   (char-map :reader char-map :initform nil :initarg :char-map)
   (key-color :reader key-color :initform nil :initarg :key-color)
   (cached-surface :accessor cached-surface :initform nil)))

(defmethod fp ((font font))
  (fp (cached-surface font)))

(defmethod fp-position ((font font))
  (fp-position (cached-surface font)))

(defmethod width ((font font))
  (sdl-base::surf-w (fp (cached-surface font))))
(defmethod (setf width) (w-val (font font))
  (setf (sdl-base::rect-w (fp-position (cached-surface font))) w-val))

(defmethod height ((font font))
  (sdl-base::surf-h (fp (cached-surface font))))
(defmethod (setf height) (h-val (font font))
  (setf (sdl-base::rect-h (fp-position (cached-surface font))) h-val))

(defmethod x ((font font))
  (sdl-base::rect-x (fp-position (cached-surface font))))
(defmethod (setf x) (x-val (font font))
  (setf (sdl-base::rect-x (fp-position (cached-surface font))) x-val))

(defmethod y ((font font))
  (sdl-base::rect-y (fp-position (cached-surface font))))
(defmethod (setf y) (y-val (font font))
  (setf (sdl-base::rect-y (fp-position (cached-surface font))) y-val))

(defun make-char-map (str)
  "given a string of characters make a hash table which returns an index from 0 to n where 0 is the first char, and n is the last"
  (let ((char-map (make-hash-table :test 'equal :size (length str))))
    (loop for n by 1 
       for c across str 
       do 
       (setf (gethash c char-map) n))
    char-map))

(defun get-char-offset (font char)
  (gethash char (char-map font)))

(defun load-font (bmp-file-name bmp-path-name font-width font-height char-map-string key-color)
  "Load and initialise a simple font using a bmp with a strip of fixed width characters mapped by the char-map-string"
  (let ((font-surface (load-image bmp-file-name bmp-path-name :key-color key-color)))
    (if font-surface
	(make-instance 'font
		       :font-surface font-surface
		       :font-width font-width
		       :font-height font-height
		       :char-map (make-char-map char-map-string)
		       :key-color key-color)
	(error "LOAD-FONT: Font cannot be initialised."))))

(defun free-font (font)
  "free up the font image surface"
  (if (font-surface font)
      (free-surface (font-surface font)))
  (if (cached-surface font)
      (free-surface (cached-surface font)))
  ;;  (#-clisp(cffi:cancel-finalization font))
  )

(defun render-string (string &key
		      (font *default-font*))
  "given an initialised font and a string, draw the string to a new surface cached in the FONT."
  (let ((new-surface (convert-surface :surface (create-surface (* (font-width font)
								  (length string))
							       (font-height font)
							       :key-color (key-color font)))))
    (when (typep (cached-surface font) 'sdl-surface)
      (free-surface (cached-surface font)))
    (fill-surface (key-color font)
		  :surface new-surface)
    (draw-string-* string 0 0
		   :surface new-surface
		   :font font)
    (setf (cached-surface font) new-surface))
  (cached-surface font))

(defun draw-character-* (font x y char &key
			 (surface *default-surface*))
  "draw a single character at the x y position"
  (let ((image (font-surface font))
	(w (font-width font))
	(h (font-height font))
	(char-offset (get-char-offset font char)))
    (if char-offset
	(sdl-base::with-rectangles ((src-rect) (dst-rect))
	  (setf (sdl-base::rect-x src-rect) (* w char-offset)
		(sdl-base::rect-y src-rect) 0
		(sdl-base::rect-w src-rect) w
		(sdl-base::rect-h src-rect) h)
	  (setf (sdl-base::rect-x dst-rect) x
		(sdl-base::rect-y dst-rect) y)
	  (sdl-base::blit-surface (fp image)
				  (fp surface)
				  src-rect
				  dst-rect
				  :update-p nil))
	nil)))

(defun draw-string (str position &key
		    (justify :left)
		    (surface *default-surface*)
		    (font *default-font*))
  (draw-string-* str (x position) (y position)
		 :justify justify :surface surface) :font font)

(defun draw-string-* (str x y &key
		      (justify :left)
		      (surface *default-surface*)
		      (font *default-font*))
  "draw a string at the x y position"
  (case justify
    (:left (draw-string-left-justify-* str x y :surface surface :font font))
    (:right (draw-string-right-justify-* str x y :surface surface :font font))
    (:center (draw-string-centered-* str x y :surface surface :font font))
    (otherwise (error ":JUSTIFY must be one of :LEFT, :RIGHT or :CENTER"))))

(defun draw-string-left-justify-* (str x y &key
				 (surface *default-surface*)
				 (font *default-font*))
  (loop for c across str do
       (unless (eql c #\space)
	 (draw-character-* font x y c
			 :surface surface))
       (incf x (font-width font))))

(defun draw-string-right-justify-* (str x y &key
				  (surface *default-surface*)
				  (font *default-font*))
  "draw a string ending at the x y position"
  (let ((right-x (- x (font-width font)))
	(rev-str (reverse str)))
    (loop for c across rev-str do
	 (unless (eql c #\space)
	   (draw-character-* font right-x y c
			   :surface surface))
	 (decf right-x (font-width font)))))

(defun draw-string-centered-* (str x y &key
			     (surface *default-surface*)
			     (font *default-font*))
  "draw a string centered at x y"
  (let* ((width (* (length str) (font-width font)))
	 (left-x (- x (/ width 2))))
    (loop for c across str do
	 (unless (eql c #\space)
	   (draw-character-* font left-x y c
			   :surface surface))
	 (incf left-x (font-width font)))))

(defun draw-font (&key (font *default-font*) (surface *default-surface*))
  (blit-surface (cached-surface font) surface))

(defun draw-font-at (position &key (font *default-font*) (surface *default-surface*))
  (draw-surface-at (cached-surface font) position :surface surface))

(defun draw-font-at-* (x y &key (font *default-font*) (surface *default-surface*))
  (draw-surface-at-* (cached-surface font) x y :surface surface))

;; (defmacro with-open-font ((font-image-name font-width font-height char-map-string key-color &optional (font-path ""))
;; 			  &body body)
;;   `(let ((*default-font* (load-font ,font-image-name ,font-path
;; 					       ,font-width ,font-height ,char-map-string ,key-color)))
;;      (if *default-font*
;; 	 (progn
;; 	   ,@body
;; 	   (free-font *default-font*)))))

(defun initialise-default-font ()
  (let ((font-name "font.bmp")
	(font-path *default-font-path*)
	(width 4)
	(height 5)
	(char-map-string "abcdefghijklmnopqrstuvwxyz:'!?_-,.()#~0123456789")
	(key-color (color :r 99 :g 0 :b 0)))
    (unless (typep *default-font* 'font)
      (setf *default-font* (load-font font-name font-path width height char-map-string key-color)))))
