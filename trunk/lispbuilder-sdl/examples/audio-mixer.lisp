
(in-package #:sdl-examples)

(defparameter *status* "")

(defun mixer-test ()
  ;; Configure Lispworks to allow
  ;; callbacks from foreign threads
  #+(and lispworks (not lispworks5.1)) (system:setup-for-alien-threads)
  (let ((sample nil))

    ;; Initialize SDL
    (sdl:with-init ()
      (sdl:window 400 20 :title-caption "WAV playback" :icon-caption "WAV playback")
      (setf (sdl:frame-rate) 5)
      (sdl:initialise-default-font)
       
      ;; Open the audio device. Use a smaller buffer size to reduce latency
      (unless (sdl:open-audio :audio-buffer-size 2048)
        (setf *status* "FAILED to open Audio device."))
     
      (when (sdl:audio-opened-p)
        ;; Open the WAV file
        (setf sample (sdl:load-audio (sdl:create-path *wav-file* *audio-path*)))
      
        ;; Start playing the audio stream
        (when sample
          (sdl:play-audio)
          (sdl:play-audio sample)))
      
      (sdl:with-events ()
        (:quit-event () t)
        (:video-expose-event ()
         (sdl:update-display))
        (:key-down-event (:key key)
         (when (sdl:key= key :SDL-KEY-ESCAPE)
           (sdl:push-quit-event))
         (when (sdl:key= key :SDL-KEY-SPACE)
           (sdl:play-audio (sdl:copy-audio sample))))
        (:idle ()
         (when (sdl:audio-opened-p)
           (if (sdl:audio-playing-p)
             (setf *status* (format nil "Number of audio samples playing: ~d"
                                    (sdl:audio-playing-p)))
             (setf *status* "Audio complete. Press SPACE to restart.")))
         (sdl:clear-display sdl:*black*)
         (sdl:draw-string-solid *status* (sdl:point) :color sdl:*white*)
         (sdl:update-display))))))

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
