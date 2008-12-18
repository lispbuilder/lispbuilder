
(in-package #:sdl-mixer)

(defvar *default-music* nil)
(defvar *default-chunk* nil)

(defvar *channel-finished* (lambda (channel)
                             (format t "Sample finished on channel: ~A~%" channel)))
(defvar *music-finished* (lambda ()
                           (format t "Music finished~%")))
(defvar *audio-buffer* nil)
