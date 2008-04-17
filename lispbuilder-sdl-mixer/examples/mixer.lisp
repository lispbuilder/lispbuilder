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
	(sdl-mixer:Halt-Music)
	(setf *music-status* (format nil "Music \"~A\": Stopped..." *music-file*)))
      (progn
	(sdl-mixer:Play-Music *music*)
	(setf *music-status* (format nil "Music \"~A\": Playing..." *music-file*)))))

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
    (sdl-mixer:Halt-Music)
    (sdl-mixer:Free *music*)
    (setf *music* nil))
  (when *sample*
    (sdl-mixer:Halt-sample t)
    (sdl-mixer:Free *sample*)
    (setf *sample* nil))
  (when *mixer-opened*
    (sdl-mixer:Close-Audio t)
    (setf *mixer-opened* nil)))

(sdl-mixer:register-sample-finished
 (lambda (channel)
   (declare (ignore channel))
   (setf *sample-status* (format nil "Samples playing: ~A..." (sdl-mixer:sample-playing-p nil)))
   (sdl:clear-display sdl:*black*)))

(defun mixer()
  "Demonstrates music file basic playback"
  (sdl:with-init (sdl:sdl-init-video sdl:sdl-init-audio)
    (setf (sdl:sdl-quit-on-exit) t)

    (sdl:window 400 50
		:title-caption "Sample & Music playback"
		:icon-caption "Sample and Music playback")
    (setf (sdl:frame-rate) 10)
    (sdl:initialise-default-font)

    (setf *status* "Unable to open Audio Mixer.")
    
    (setf *mixer-opened* (sdl-mixer:OPEN-AUDIO))
    (when *mixer-opened*
      (setf *status* "Opened Audio Mixer.")
      (setf *music* (sdl-mixer:load-music (sdl:create-path *music-file* *audio-path*)))
      (setf *sample* (sdl-mixer:load-sample (sdl:create-path *sample-file* *audio-path*)))      
      (play-music)
      (play-sample))

    (sdl:with-events ()
      (:quit-event ()
		   (clean-up)
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

