
(in-package #:sdl-examples)

(defparameter *status* "")

(defparameter *sample* nil)

(defun mixer-test ()
  ;; Configure Lispworks to allow
  ;; callbacks from foreign threads
  #+(and lispworks (not lispworks5.1)) (system:setup-for-alien-threads)

  ;; Initialize SDL
  (sdl:with-init (sdl:sdl-init-video)
    (sdl:window 400 20 :title-caption "WAV playback" :icon-caption "WAV playback")
    (setf (sdl:frame-rate) 5)
    (sdl:initialise-default-font)
    
    ;; Open the audio device
    (unless (sdl:open-audio)
      (setf *status* "FAILED to open Audio device."))
     
    (when (sdl:audio-opened-p)
      ;; Open the WAV file
      (setf *sample* (sdl:load-audio (sdl:create-path *wav-file* *audio-path*)))
      
      ;; Start playing the audio stream
      (when *sample*
        (sdl:play-audio)
        (sdl:play-audio *sample*)
        (setf *status* "Audio playing.")))
    
    (sdl:with-events ()
      (:quit-event () t)
      (:video-expose-event () (sdl:update-display))
      (:key-down-event (:key key)
       (when (sdl:key= key :SDL-KEY-ESCAPE)
         (sdl:push-quit-event))
       (when (sdl:key= key :SDL-KEY-SPACE)
         (sdl:rewind-audio *sample*)
         (setf *status* "Audio playing."))
       (when (sdl:key= key :SDL-KEY-O)
         (when (> (incf (sdl:audio-volume sdl:*mixer*) 5) 255)
           (setf (sdl:audio-volume sdl:*mixer*) 255)))
       (when (sdl:key= key :SDL-KEY-L)
         (when (< (decf (sdl:audio-volume sdl:*mixer*) 5) 0)
           (setf (sdl:audio-volume sdl:*mixer*) 0)))
       (when (sdl:key= key :sdl-key-p)
         (if (sdl:audio-paused-p *sample*)
           (progn
             (sdl:resume-audio *sample*)
             (setf *status* "Audio playing."))
           (progn
             (sdl:pause-audio *sample*)
             (setf *status* "Audio paused."))))
       (when (sdl:key= key :sdl-key-a)
         (sdl:play-audio (sdl:copy-audio *sample*))))
      (:idle ()
       (when (and *sample* (sdl:audio-opened-p))
         (unless (sdl:audio-playing-p *sample*)
           (setf *status* "Audio complete. Press SPACE to restart.")))
       (sdl:clear-display sdl:*black*)
       (sdl:draw-string-solid *status* (sdl:point) :color sdl:*white*)
       (sdl:update-display)))))

;(let ((num #(25 50)))
;  (logior (ash (aref num 1) 8)
;          (aref num 0)))
;(defun conv (x)
;  (if (>= x #x8000)
;    (* -1 (- x (- x #x1000)))
;    x))

;(lambda (bit integer) (logand integer (lognot (ash -1
;            bit))))

;(defun sign-extend (bit integer) (- integer (* 2 (logand integer
;            (ash 1 bit)))))


;(- (logand integer (lognot (ash -1 bit))) ...)

;1) Invert all the bits in the number, i.e., apply the logical NOT function. 

;2) Add one to the inverted result.

;1001110001000000
;1001110001000000
;for for across (the simple-vector vector)
