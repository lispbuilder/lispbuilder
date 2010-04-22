
(in-package #:sdl-examples)

;; Explosion textures from;
;; http://www.positech.co.uk/content/explosion/explosiongenerator.html

(defun explosion ()
  (sdl:with-init ()
    (sdl:window 200 100 :title-caption "Explosion" :icon-caption "Explosion")
    (setf (sdl:frame-rate) 25)
    
    (let* ((sprite-sheet (sdl:load-image (merge-pathnames "ani2.bmp" sdl:*default-asset-path*)))
           ;; Create the list of cells
           ;; Each 'cell' is the x/y width/height of an image in the sprite sheet
           (sprite-cells (loop for y from 0 to 192 by 64
                               append (loop for x from 0 to 192 by 64
                                        collect (list x y 64 64))))
           (current 0))

      ;; Set the the cells for the sprite-sheet
      (setf (sdl:cells sprite-sheet) sprite-cells)
      
      (sdl:with-events ()
        (:quit-event () t)
        (:key-down-event ()
         (when (sdl:key-down-p :sdl-key-escape)
           (sdl:push-quit-event)))
        (:video-expose-event () (sdl:update-display))
        (:idle ()
         (sdl:clear-display sdl:*black*)
         ;; Draw sprite in the middle of the window
         (sdl:draw-surface-at-* sprite-sheet (- 100 32) 10 :cell current)
         ;; Loop over each cell
         (when (>= (incf current) (length sprite-cells))
           (setf current 0))
         (sdl:update-display))))))


;;;;;
;;;;; This version will create separate surfaces instead of using an index into the sprite sheet.

(defun subsect-image (image sub)
  (let* ((x (elt sub 0)) (y (elt sub 1)) (w (elt sub 2)) (h (elt sub 3))
         (new-surf (sdl:create-surface w h
                                       :alpha (sdl:alpha-enabled-p image)
                                       :pixel-alpha (sdl:pixel-alpha-enabled-p image)
                                       )))
    (setf (sdl:cells image) (sdl:rectangle :x x :y y :w w :h h))
    (sdl:draw-surface image :surface new-surf)
    new-surf))

(defun generate-draw-frame (image-sequence)
  (let* ((current (copy-list image-sequence)))
    ;; Create a circular list, so that the last frame in the explosion sequence
    ;; points back to the first.
    (setf (cdr (last current)) current)
    #'(lambda (x y)
        (sdl:draw-surface-at-* (first current) x y)
        (setf current (cdr current)))))

(defun explosion-new ()
  (sdl:with-init ()
    (sdl:window 200 100 :title-caption "Explosion" :icon-caption "Explosion")
    (setf (sdl:frame-rate) 25)
    
    (let* ((sprite-sheet (sdl:load-image (merge-pathnames "ani2.bmp" sdl:*default-asset-path*)))
           ;; Create the list of cells
           ;; Each 'cell' is the x/y width/height of an image in the sprite sheet
           (sprite-cells (loop for y from 0 to 192 by 64
                               append (loop for x from 0 to 192 by 64
                                            collect (list x y 64 64))))
           (sprite-list (loop for cell in sprite-cells
                              collect (subsect-image sprite-sheet cell))))
      (let ((draw-frame (generate-draw-frame sprite-list)))
      
        (sdl:with-events ()
          (:quit-event () t)
          (:key-down-event ()
           (when (sdl:key-down-p :sdl-key-escape)
             (sdl:push-quit-event)))
          (:video-expose-event () (sdl:update-display))
          (:idle ()
           (sdl:clear-display sdl:*black*)
           ;; Draw sprite in the middle of the window
           (funcall draw-frame (- 100 32) 10)
           (sdl:update-display)))))))
