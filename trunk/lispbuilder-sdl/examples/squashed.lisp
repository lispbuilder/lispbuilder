;;;; (C) 2006 Jonathan Heusser, minor changes by Frank Buss
;;;; Additional changes by Luke Crook

(in-package #:lispbuilder-sdl-examples)

(defun show-score (score)
  (sdl:set-caption (format nil "Squashed - Score: ~a" score) "Squashed"))

(defun load-image (filename)
  (sdl:convert-surface :surface
                       (sdl:load-image (sdl:create-path filename
                                                        sdl:*default-asset-path*)
                                       :color-key (sdl:color :r 255 :g 255 :b 255))
                       :free t))

(defun load-audio (filename)
  (sdl:load-audio (sdl:create-path filename sdl:*default-asset-path*)))

(defun squashed ()
  "Squashed: main entry function"
  (sdl:with-init (sdl:sdl-init-video sdl:sdl-init-audio)
    (sdl:window 640 480)
    (setf (sdl:frame-rate) 30)
    ;; Open the audio mixer. Use a smaller buffer for lower latency.
    #+lispbuilder-sdl-audio
    (sdl:open-audio :audio-buffer-size 1024)
    (let ((bug (load-image "bug.bmp"))
	  (racket (load-image "racket.bmp"))
	  (blood (load-image "blood.bmp"))
	  (squash (load-image "squash.bmp"))
	  (levelticks 1000)
	  (last-squash-tick 0)
	  (lasttick 0)
	  (score 0)
	  (bug? t) (racket? t) (blood? nil) (squash? nil) (squashed? nil)
          #+lispbuilder-sdl-audio
          (swat-effect (load-audio "bookclose2.wav"))
          #+lispbuilder-sdl-audio
          (squash-effects (list (load-audio "splat1a.wav")
                                (load-audio "splat2a.wav")
                                (load-audio "splat3a.wav"))))

      (sdl:set-position-* bug :x (random 640) :y (random 480))
      (sdl:set-position-* racket :x 100 :y 100)
      (sdl:set-position-* squash :x 0 :y 0)
      
      (show-score score)
      (sdl:show-cursor nil)
      #+lispbuilder-sdl-audio
      (sdl:play-audio)
      (sdl:with-events ()
        (:quit-event () t)
        (:key-down-event (:key key)
         (when (sdl:key= key :sdl-key-escape) (sdl:push-quit-event)))
        (:mouse-motion-event (:x x :y y)
         (sdl:set-position-* racket :x x :y y))
        (:mouse-button-down-event (:x x :y y)
         ;; check if squashed
         (when (sdl:within-range (sdl:get-point racket)
                                 (sdl:get-point bug)
                                 17)
           (setf squashed? t
                 bug? nil
                 blood? t
                 squash? t)
           (sdl:set-position-* blood :x x :y y)
           (setf last-squash-tick (sdl-cffi::SDL-Get-Ticks))
           (show-score (incf score))
           ;; increase the bug jumping speed
           (when (> levelticks 200)
             (decf levelticks 100))
           ;; Play one of the squashed sound effects.
           #+lispbuilder-sdl-audio
           (sdl:play-audio (sdl:copy-audio (nth (random 3) squash-effects))))
         ;; Play the swatting sound effect.
         #+lispbuilder-sdl-audio
         (sdl:play-audio (sdl:copy-audio swat-effect)))
        (:idle ()
         (sdl:with-surface (disp sdl:*default-display*)
           ;; fill the background with white
           (sdl:clear-display (sdl:color :r 255 :g 255 :b 255))
           ;; draw images
           (if squashed?
             (if (> (- (sdl-cffi::SDL-Get-Ticks) last-squash-tick) 700)
               (setf squashed? nil
                     squash? nil
                     blood? nil
                     bug? t))
             (when (> (sdl-cffi::SDL-Get-Ticks) (+ lasttick levelticks))
               (setf lasttick (sdl-cffi::SDL-Get-Ticks))
               (sdl:set-position-* bug :x (random 640) :y (random 480))))
           (when squash?
             (sdl:draw-surface squash))
           (when bug? (sdl:draw-surface bug))
           (when blood? (sdl:draw-surface blood))
           (when racket? (sdl:draw-surface racket))
           (sdl:update-display)))))))
