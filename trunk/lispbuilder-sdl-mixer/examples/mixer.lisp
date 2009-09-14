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
          (sdl-mixer:resume-Music)
          (setf *music-status* (format nil "Music \"~A\": Resumed..." *music-file*)))
        (progn
          (sdl-mixer:play-music *music*)
          (setf *music-status* (format nil "Music \"~A\": Playing..." *music-file*))))))
    
(defun handle-key(key)
  "handle key presses"
  (cond
    ((sdl:key= key :SDL-KEY-ESCAPE)
     (sdl:push-quit-event))
    ((sdl:key= key :SDL-KEY-M)
     (play-music))
    ((sdl:key= key :SDL-KEY-S)
     (sdl-mixer:play-sample *sample*))))

(defun clean-up ()
  (when *music*    
    (when (sdl-mixer:music-playing-p)
      (sdl-mixer:Pause-Music)
      (sdl-mixer:Halt-Music))
    (sdl:Free *music*)
    (setf *music* nil))
  (when *sample*
    (when (sdl-mixer:sample-playing-p nil)
      (sdl-mixer:pause-sample t)
      (sdl-mixer:Halt-sample :channel t))
    (sdl:Free *sample*)
    (setf *sample* nil))
  (when *mixer-opened*
    (sdl-mixer:Close-Audio t)
    (setf *mixer-opened* nil)))

(defun sample-finished-action ()
  (sdl-mixer:register-sample-finished
   (lambda (channel)
     (declare (ignore channel))
     nil)))

(defun music-finished-action ()
  (sdl-mixer:register-music-finished
   (lambda ()
     nil
     )))

(defun mixer()
  "Demonstrates music file basic playback"
  (sdl:with-init ()
    (sdl:window 400 50
		:title-caption "Sample & Music playback"
		:icon-caption "Sample and Music playback")
    (setf (sdl:frame-rate) 60)
    (sdl:initialise-default-font)
    (sdl:enable-key-repeat 500 50)

    (setf *status* "Unable to open Audio Mixer.")
    
    (setf *mixer-opened* (sdl-mixer:OPEN-AUDIO :chunksize 1024 :enable-callbacks nil))
    (when *mixer-opened*
      (setf *status* "Opened Audio Mixer.")
      (setf *music* (sdl-mixer:load-music (sdl:create-path *music-file* *audio-path*)))
      (setf *sample* (sdl-mixer:load-sample (sdl:create-path *sample-file* *audio-path*)))

      ;; Seems in win32, that these callbacks are only really supported by Lispworks.
      (music-finished-action)
      (sample-finished-action)

      (sdl-mixer:allocate-channels 16)

      (play-music)
      (sdl-mixer:play-sample *sample*))

    (sdl:with-events ()
      (:quit-event ()
       (clean-up)
       t)
      (:key-down-event (:key key)
       (when *mixer-opened*
         (handle-key key)))
      (:idle ()
       (sdl:clear-display sdl:*black*)
       (sdl:draw-string-solid-* *status* 1 1 :surface sdl:*default-display* :color sdl:*white*)
       (sdl:draw-string-solid-* *music-status* 1 11 :surface sdl:*default-display* :color sdl:*white*)
       (sdl:draw-string-solid-* (format nil "Samples playing: ~A..." (sdl-mixer:sample-playing-p nil))
                                1 21 :surface sdl:*default-display* :color sdl:*white*)
       (sdl:draw-string-solid-* "<M> Toggle Music. <S> Play Samples." 1 40 :surface sdl:*default-display* :color sdl:*white*)
       (sdl:update-display)))))
