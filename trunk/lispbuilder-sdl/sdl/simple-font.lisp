;;;; simple fixed height and width font

(in-package :lispbuilder-sdl)

(defun retrieve-line (start-x y width surface color-key)
  (with-pixel (pix (sdl:fp surface))
    (loop for x from start-x below (+ start-x width)
          collect (multiple-value-bind (rgba r g b)
                      (read-pixel pix x y)
                    (declare (ignore rgba))
                    (if (and (equal (sdl:r color-key) r)
                             (equal (sdl:g color-key) g)
                             (equal (sdl:b color-key) b))
                      0 1)))))

(defun retrieve-mask (start-x start-y width height surface color-key)
  (loop for y from start-y below (+ start-y height)
        collect (retrieve-line start-x y width surface color-key)))

(defun retrieve-mask-map (mask surface color-key)
  (loop for (x y w h) in mask
        collect (retrieve-mask x y w h surface color-key)))

(defun convert-character-to-pitch (character pitch)
  (loop for line in character
        collect (append line (loop repeat (- (* pitch 8) (length line))
                                   collect 0))))

(defun split-character (character)
  (loop for line in character
        append (loop repeat (/ (length line) 8) 
                     for start = 0 then (incf start 8)
                     for end = (+ start 8) then (incf end 8)
                     collect (subseq line start end))))

(defun convert-character-to-bytes (character)
  (loop for byte in (split-character character)
        collect (loop for i from 7 downto 0
                      for b in byte
                      for val = (ash b i) then (logior val (ash b i))
                      finally (return val))))

(defun convert-character (character pitch)
  (convert-character-to-bytes (convert-character-to-pitch character pitch)))

(defun convert-simple-font (definition surface)
  (loop for c across (character-map definition)
        for m in (loop for char in (retrieve-mask-map (character-mask definition)
                                                           surface (color-key definition))
                       collect (convert-character char (char-pitch definition)))
        do (loop for i from (* (char-code c) (char-size definition)) below ( + (* (char-code c) (char-size definition)) (char-size definition))
                 for b in m
                 do (setf (aref (data definition) i) b)))
  definition)

(defun create-simple-font-definition (width height char-map char-mask color-key filename loader)
  (make-instance 'simple-font-definition
                 :char-width width :char-height height
                 :character-map char-map :character-mask char-mask :color-key color-key
                 :filename filename
                 :loader loader))

(defmethod initialise-font ((self simple-font-definition))
  (make-instance 'bitmap-font :font-definition
                 (convert-simple-font self (funcall (loader self) (filename self)))))
