;;;; simple fixed height and width font

(in-package :lispbuilder-sdl)

(defun convert-line (start-x start-y pad-x pad-y width height size characters
                             data surface color-key)
  (sdl-base::with-pixel (pix (sdl:fp surface))
    (loop for c across characters
          for i = 0 then (incf i)
          do (let* ((char-pos (* (char-code c) size))
                    (mask #x80))
               (dotimes (iy height)
                 (setf mask #x80)                       
                 (dotimes (ix width)
                   (multiple-value-bind (rgba r g b)
                       (sdl-base::read-pixel pix
                                             (+ start-x pad-x
                                                (* i width) ix)
                                             (+ start-y pad-y iy))
                     (declare (ignore rgba))
                     (unless (and (equal (sdl:r color-key) r)
                                  (equal (sdl:g color-key) g)
                                  (equal (sdl:b color-key) b))
                       (setf (aref data char-pos)
                             (logior mask (aref data char-pos)))))
                   (setf mask (ash mask -1))
                   (when (eq mask 0)
                     (setf mask #x80)
                     (incf char-pos)))
                 (incf char-pos))))))

(defun convert-simple-font (definition surface)
  (loop for characters in (character-map definition)
        for y = 0 then (incf y (char-height definition))
        do (convert-line 0 y (pad-x definition) (pad-y definition)
                         (char-width definition) (char-height definition)
                         (char-size definition) characters (data definition)
                         surface (color-key definition)))
  definition)

(defun create-simple-font-definition (width height char-map color-key filename
                                            &key (pad-x 0) (pad-y 0))
  (make-instance 'simple-font-definition
                 :char-width width :char-height height
                 :character-map char-map :color-key color-key
                 :filename filename
                 :pad-x pad-x :pad-y pad-y))

(defmethod initialise-font ((self simple-font-definition))
  (make-instance 'bitmap-font :font-definition
                 (convert-simple-font self (load-image (filename self)))))
