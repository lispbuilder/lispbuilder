;;;; Converted from an sdl mixer example at:
;;;; http://www.kekkai.org/roger/sdl/mixer/
;;;; Lisp version (C)2006 Justin Heyes-Jones
;;;; Thanks to Luke Crook for an excellent example of packaging and examples
;;;; to copy

(in-package #:sdl-mixer-examples)

(defvar music nil)
(defvar mixer-loaded nil)
(defvar music-file "music.mp3")

(defun load-music (filename path)
  "load in the supplied filename, must be an mp3, wav or ogg file"
  (let ((file (namestring (merge-pathnames filename path))))
    (if (and (stringp file) (probe-file file)) ; LJC: Make sure filename is a string and the filename exists.
	(lispbuilder-sdl-mixer:Mix-Load-MUS file)	
	(error "Music file ~A does not exist." file))))

; play music
(defun play-music()
  (if (null music)
      (progn 
	(setf music (load-music music-file *audio-path*))
	(lispbuilder-sdl-mixer:Mix-Play-Music music 0)
	(if (cffi:null-pointer-p music)
	    (error "unable to load music")
	    (format t "Music \"~A\" is currently playing...~%" music-file)))
    (progn
      (lispbuilder-sdl-mixer:Mix-Halt-Music)
      (lispbuilder-sdl-mixer:Mix-Free-Music music)
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
      (if (= 0 (lispbuilder-sdl-mixer:MIX-OPEN-AUDIO 22050 sdl-cffi::AUDIO-S16 2 4096))
	  (setf mixer-loaded t)
	  (error "Unable to open audio mixer")))
    (play-music)
    (sdl:window 200 50 :title-caption "Mp3 playback" :icon-caption "Mp3 playback")
    (setf (sdl:frame-rate) 30)
    (sdl:with-events ()
      (:quit-event () 
		   (lispbuilder-sdl-mixer:Mix-Close-Audio)
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
