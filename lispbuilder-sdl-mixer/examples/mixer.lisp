;;;; Converted from an sdl mixer example at:
;;;; http://www.kekkai.org/roger/sdl/mixer/
;;;; Lisp version (C)2006 Justin Heyes-Jones
;;;; Thanks to Luke Crook for an excellent example of packaging and examples
;;;; to copy

(in-package #:sdl-mixer-examples)

(defvar music nil)
(defvar mixer-loaded nil)
(defvar music-file "music.mp3")
(defvar *status* "")

; play music
(defun play-music()
  (if (null music)
      (progn
	(setf music (sdl-mixer:mix-load-mus music-file *audio-path*))
	(sdl-mixer:Mix-Play-Music music 0)
	(sdl:render-string-shaded (format nil "Music \"~A\" is currently playing..." music-file)
				  sdl:*white* sdl:*black* :free t :cache t))
      (progn
	(sdl-mixer:Mix-Halt-Music)
	(sdl-mixer:Free music)
	(sdl:render-string-shaded (format nil "Music \"~A\" is stopped..." music-file)
				  sdl:*white* sdl:*black* :free t :cache t)
	(setf music nil))))
    
(defun handle-key(key)
  "handle key presses"
  (when (sdl:key= key :SDL-KEY-ESCAPE)
    (sdl:push-quit-event))
  (if (sdl:key= key :SDL-KEY-M)
      (play-music)
      (sdl:render-string-shaded (format nil "Press M to play music (not ~a)" key)
				sdl:*white* sdl:*black* :free t :cache t)))

(defun mixer()
  "Demonstrates music file basic playback"
  (sdl:with-init (sdl:sdl-init-video sdl:sdl-init-audio)
    (setf (sdl:sdl-quit-on-exit) t)

    (sdl:window 400 50 :title-caption "Mp3 playback" :icon-caption "Mp3 playback")
    (setf (sdl:frame-rate) 30)
    (sdl:initialise-default-font)
    
    (unless mixer-loaded
      (if (= 0 (sdl-mixer:MIX-OPEN-AUDIO 22050 sdl-mixer:+MIX-DEFAULT-FORMAT+ 2 4096))
	  (setf mixer-loaded t)
	  (sdl:render-string-shaded "Unable to open audio mixer"
				    sdl:*white* sdl:*black* :free t :cache t)))
    (when mixer-loaded
      (play-music))

    (sdl:with-events ()
      (:quit-event ()
		   (when music
		     (sdl-mixer:Mix-Halt-Music)
		     (sdl-mixer:Free music)
		     (setf music nil))
		   (when mixer-loaded
		     (sdl-mixer:Mix-Close-Audio)
		     (setf mixer-loaded nil))
		   t)
      (:key-down-event (:key key)
		       (handle-key key)
		       (sdl:clear-display sdl:*black*))
      (:idle ()
	     (sdl:draw-pixel-* (random 400) (random 50)
			       :color (sdl:color :r (random 255)
						 :g (random 255)
						 :b (random 255))
			       :surface sdl:*default-display*)
	     (sdl:draw-font-at-* 0 0 :surface sdl:*default-display*)
	     (sdl:update-display)))))
