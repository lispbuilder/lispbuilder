(in-package #:lispbuilder-sdl-cffi) 

(cl:defconstant SDL-MAX-TRACKS 99)

(cl:defconstant SDL-AUDIO-TRACK #x00)

(cl:defconstant SDL-DATA-TRACK #x04)

(cffi:defcenum CD-status
	:CD-TRAYEMPTY
	:CD-STOPPED
	:CD-PLAYING
	:CD-PAUSED
	(:CD-ERROR -1))

(cffi:defcstruct SDL-CD-track
	(id :unsigned-char)
	(type :unsigned-char)
	(unused :unsigned-short)
	(length :unsigned-int)
	(offset :unsigned-int))

(cffi:defcstruct SDL-CD
	(id :int)
	(status CD-status)
	(numtracks :int)
	(cur-track :int)
	(cur-frame :int)
	(track :pointer))

(cffi:defcfun ("SDL_CDNumDrives" SDL-CD-Num-Drives) :int)

(cffi:defcfun ("SDL_CDName" SDL-CD-Name) :string
  (drive :int))

(cffi:defcfun ("SDL_CDOpen" SDL-CD-Open) :pointer
  (drive :int))

(cffi:defcfun ("SDL_CDStatus" SDL-CD-Status) CD-status
  (cdrom :pointer))

(cffi:defcfun ("SDL_CDPlayTracks" SDL-CD-Play-Tracks) :int
  (cdrom :pointer)
  (start-track :int)
  (start-frame :int)
  (ntracks :int)
  (nframes :int))

(cffi:defcfun ("SDL_CDPlay" SDL-CD-Play) :int
  (cdrom :pointer)
  (start :int)
  (length :int))

(cffi:defcfun ("SDL_CDPause" SDL-CD-Pause) :int
  (cdrom :pointer))

(cffi:defcfun ("SDL_CDResume" SDL-CD-Resume) :int
  (cdrom :pointer))

(cffi:defcfun ("SDL_CDStop" SDL-CD-Stop) :int
  (cdrom :pointer))

(cffi:defcfun ("SDL_CDEject" SDL-CD-Eject) :int
  (cdrom :pointer))

(cffi:defcfun ("SDL_CDClose" SDL-CD-Close) :void
  (cdrom :pointer))

(defun CD-IN-DRIVE (status)
  (if (> status 0)
      t
    nil))

(defconstant CD-FPS 75)
(defun FRAMES-TO-MSF (f)
  (values 
   (mod f CD-FPS)
   (mod (/ f CD-FPS) 60)
   (/ (/ f CD-FPS) 60)))

(defun MSF-TO-FRAMES (M S F)
  (+ 
   (* M 60 CD-FPS)
   (* S CD-FPS)
   F))

