;;;; Converted from an sdl mixer example at:
;;;; http://www.kekkai.org/roger/sdl/mixer/
;;;; Lisp version (C)2006 Justin Heyes-Jones
;;;; Thanks to Luke Crook for an excellent example of packaging and examples
;;;; to copy

(in-package #:sdl-mixer-examples)

(defvar music nil)
(defvar mixer-loaded nil)
(defvar music-file "music.mp3")

; play music
(defun play-music()
  (if (null music)
      (progn 
	(setf music (sdl-mixer:mix-load-mus music-file *audio-path*))
	(sdl-mixer:Mix-Play-Music music 0)
	(format t "Music \"~A\" is currently playing...~%" music-file))
      (progn
	(sdl-mixer:Mix-Halt-Music)
	(sdl-mixer:Free music)
	(setf music nil)
	(format t "Music \"~A\" is stopped...~%" music-file))))
    
(defun handle-key(key)
  "handle key presses"
  (if (sdl:key= key :SDL-KEY-M)
      (play-music)
    (format t "Press M to play music (not ~a)~%" key)))

(defun mixer()
  "Demonstrates music file basic playback"
  (sdl:with-init (sdl:sdl-init-video sdl:sdl-init-audio)
    (setf (sdl:sdl-quit-on-exit) t)
    (unless mixer-loaded
      (if (= 0 (sdl-mixer:MIX-OPEN-AUDIO 22050 sdl-mixer:+MIX-DEFAULT-FORMAT+ 2 4096))
	  (setf mixer-loaded t)
	  (error "Unable to open audio mixer")))
    (play-music)
    (sdl:window 200 50 :title-caption "Mp3 playback" :icon-caption "Mp3 playback")
    (setf (sdl:frame-rate) 30)
    (sdl:with-events ()
      (:quit-event () 
		   (sdl-mixer:Mix-Close-Audio)
		   t)
      (:key-down-event (:key key)
		       (handle-key key))
      (:idle ()
	     (sdl:draw-pixel-* (random 200) (random 200)
			       :color (sdl:color :r (random 255)
						 :g (random 255)
						 :b (random 255))
			       :surface sdl:*default-display*)
	     (sdl:update-display)))))
