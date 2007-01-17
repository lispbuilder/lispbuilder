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

; do stuff when a key is pressed
(defun handle-key(key)
  "handle key presses"
  (if (sdl-base::key= key :SDL-KEY-M)
      (progn
	(format t "Music ~a~%" music)
	(if (null music)
	    (progn 
	      (setf music (load-music music-file *audio-path*))
	      (if (cffi:null-pointer-p music)
		  (error "unable to load music"))
	      (lispbuilder-sdl-mixer:Mix-Play-Music music 0))
	  (progn
	    (lispbuilder-sdl-mixer:Mix-Halt-Music)
	    (lispbuilder-sdl-mixer:Mix-Free-Music music)
	    (setf music nil))))
    (format t "Press M not ~a~%" key)))

(defun mixer()
  "Demonstrates music file basic playback"
  (sdl::initialize-on-startup sdl-cffi::sdl-init-video)
  (sdl::quit-on-exit sdl-cffi::sdl-init-video)
  (sdl::set-sdl-quit-on-exit nil)
  (sdl:with-init ()
		 (unless mixer-loaded
		   (if (= 0 (lispbuilder-sdl-mixer:MIX-OPEN-AUDIO 22050 sdl-cffi::AUDIO-S16 2 4096))
		       (setf mixer-loaded t)
		     (error "Unable to open audio mixer")))
		 (sdl::window 200 50 :title-caption "Mp3 playback" :icon-caption "Mp3 playback")
		 (setf (sdl-base::frame-rate) 30)
		 (sdl:with-events ()
				  (:quit-event () 
					       (lispbuilder-sdl-mixer:Mix-Close-Audio)
					       t)
				  (:key-down-event (:key key)
						  (handle-key key))
				  (:idle ()
					 (sdl::draw-point (sdl::point :x (random 200) :y (random 50))
							  :color (sdl::color :r (random 255)
									     :g (random 255)
									     :b (random 255))
							  :surface sdl::*default-display*)
					 (sdl::update-display)))))


      