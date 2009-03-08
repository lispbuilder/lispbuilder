;;;; Converted from an sdl mixer example at:
;;;; http://www.kekkai.org/roger/sdl/mixer/
;;;; Lisp version (C)2006 Justin Heyes-Jones
;;;; Thanks to Luke Crook for an excellent example of packaging and examples
;;;; to copy

(in-package #:sdl-mixer-examples)

(defvar *music* nil)
(defvar *sample* nil)
(defvar *mixer-opened* nil)
(defvar *music-file* "music.mp3")
(defvar *sample-file* "phaser.wav")

(defvar *status* "")
(defvar *music-status* "")
(defvar *sample-status* "")

; play music
(defun play-music()
  (if (sdl-mixer:music-playing-p)
      (progn
	(sdl-mixer:pause-Music)
	(setf *music-status* (format nil "Music \"~A\": Paused..." *music-file*)))
      (if (sdl-mixer:music-paused-p)
        (progn
          (sdl-mixer::resume-Music)
          (setf *music-status* (format nil "Music \"~A\": Resumed..." *music-file*)))
        (progn
          (sdl-mixer:play-music *music*)
          (setf *music-status* (format nil "Music \"~A\": Playing..." *music-file*))))))

(defun play-sample()
  (sdl-mixer:Play-sample *sample*)
  (setf *sample-status* (format nil "Samples playing: ~A..." (sdl-mixer:sample-playing-p nil))))
    
(defun handle-key(key)
  "handle key presses"
  (cond
    ((sdl:key= key :SDL-KEY-ESCAPE)
     (sdl:push-quit-event))
    ((sdl:key= key :SDL-KEY-M)
     (play-music))
    ((sdl:key= key :SDL-KEY-S)
     (play-sample))))

(defun clean-up ()
  (when *music*    
    (when (sdl-mixer:music-playing-p)
      (format t "\(sdl-mixer:Halt-Music\)~%")
      (force-output t)
      (sdl-mixer:Pause-Music)
      (sdl-mixer:Halt-Music))
    (sdl:Free *music*)
    (setf *music* nil))
  (when *sample*
    (when (sdl-mixer:sample-playing-p nil)
      (format t "\(sdl-mixer:Halt-Sample\)~%")
      (force-output t)
      (sdl-mixer::pause-sample t)
      (sdl-mixer:Halt-sample :channel t))
    (sdl:Free *sample*)
    (setf *sample* nil))
  (when *mixer-opened*
    (format t "\(sdl-mixer:Close-Audio\)~%")
    (sdl-mixer:Close-Audio t)
    (setf *mixer-opened* nil)))

(defun sample-finished-action ()
  (sdl-mixer:register-sample-finished
   (lambda (channel)
     (declare (ignore channel))
     (setf *sample-status* (format nil "Samples playing: ~A..." (sdl-mixer:sample-playing-p nil)))
     (sdl:clear-display sdl:*black*))))

(defun music-finished-action ()
  (sdl-mixer:register-music-finished
   (lambda ()
     (format t "MUSIC finished.~%"))))

(defun mixer()
  "Demonstrates music file basic playback"
  (sdl:with-init (sdl:sdl-init-video sdl:sdl-init-audio)
    (sdl:window 400 50
		:title-caption "Sample & Music playback"
		:icon-caption "Sample and Music playback")
    (setf (sdl:frame-rate) 60)
    (sdl:initialise-default-font)

    (setf *status* "Unable to open Audio Mixer.")
    
    (setf *mixer-opened* (sdl-mixer:OPEN-AUDIO :chunksize 1024))
    (when *mixer-opened*
      (setf *status* "Opened Audio Mixer.")
      (setf *music* (sdl-mixer:load-music (sdl:create-path *music-file* *audio-path*)))
      (setf *sample* (sdl-mixer:load-sample (sdl:create-path *sample-file* *audio-path*)))
      ;(music-finished-action)
      ;(sample-finished-action)
      (play-music)
      (play-sample))

    (sdl:with-events ()
      (:quit-event ()
       (clean-up)
       (format t "Done cleaning up.~%")
       t)
      (:key-down-event (:key key)
       (when *mixer-opened*
         (handle-key key))
       (sdl:clear-display sdl:*black*))
      (:idle ()
       (sdl:draw-pixel-* (random 400) (random 50)
                         :color (sdl:color :r (random 255)
                                           :g (random 255)
                                           :b (random 255))
                         :surface sdl:*default-display*)
       (sdl:draw-string-solid-* *status* 1 1 :surface sdl:*default-display* :color sdl:*white*)
       (sdl:draw-string-solid-* *music-status* 1 11 :surface sdl:*default-display* :color sdl:*white*)
       (sdl:draw-string-solid-* *sample-status* 1 21 :surface sdl:*default-display* :color sdl:*white*)
       (sdl:draw-string-solid-* "<M> Toggle Music. <S> Play Samples." 1 40 :surface sdl:*default-display* :color sdl:*white*)
       (sdl:update-display)))))
