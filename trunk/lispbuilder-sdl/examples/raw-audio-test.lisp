
(in-package #:sdl-examples)

(defparameter *wav-file* "example.wav")
(defparameter *audio-length* 0)
(defparameter *audio-remaining* 0)
(defparameter *audio-position* nil)
(defparameter *audio-buffer-start* nil)
(defparameter *file-found* nil)
(defparameter *audio-opened* nil)
(defparameter *status* "")

(cffi:defcallback fill-audio-buffer :pointer ((user-data :pointer)
					      (stream :pointer)
					      (len :int))
  (declare (ignore user-data))
  ;; Play audio only when data remains 
  (when (> *audio-remaining* 0)
    (let ((len (if (> len *audio-remaining*) *audio-remaining* len)))
      (sdl-cffi::sdl-mix-audio stream *audio-position* len sdl-cffi::SDL-MIX-MAXVOLUME)
      (cffi:incf-pointer *audio-position* len)
      (decf *audio-remaining* len)))
  (cffi:null-pointer))

(defun raw-audio-test ()
  ;; Configure Lispworks to allow
  ;; callbacks from foreign threads
   #+(and lispworks (not lispworks5.1)) (system:setup-for-alien-threads)

  ;; Initialize SDL
  (sdl:with-init (sdl:sdl-init-video sdl:sdl-init-audio)
    ;; Make sure SDL is closed on exit,
    ;; or the SDL library might become a little unstable
    (setf (sdl:quit-on-exit) t)
        
    (sdl:window 400 20 :title-caption "WAV playback" :icon-caption "WAV playback")
    (setf (sdl:frame-rate) 5)
    (sdl:initialise-default-font)

    (cffi:with-foreign-objects ((audio-spec-desired 'sdl-cffi::SDL-Audio-Spec)
                                (audio-spec-obtained 'sdl-cffi::SDL-Audio-Spec)
				(wav-buffer-handle :pointer)
				(wav-buffer-length :pointer))
      ;; Open the WAV file
      (if (cffi:null-pointer-p (sdl-cffi::sdl-load-wav (sdl:create-path *wav-file*
                                                                        sdl:*default-asset-path*)
                                                       audio-spec-desired
						       wav-buffer-handle
						       wav-buffer-length))
	  (setf *file-found* nil
		*status* (format nil "FAILED to load \"~A\"." (sdl:create-path *wav-file* *audio-path*)))
	  (setf *file-found* t))

      (when *file-found*
	;; Store the initial play position of the buffer
	(setf *audio-buffer-start* (cffi:make-pointer (cffi:pointer-address (cffi:mem-aref wav-buffer-handle :pointer)))
	      *audio-position* *audio-buffer-start*)
	;; Store the length of the WAV buffer
	(setf *audio-length* (cffi:mem-aref wav-buffer-length :unsigned-int)
	      *audio-remaining* *audio-length*)
        (cffi:with-foreign-slots ((sdl-cffi::callback 
				   sdl-cffi::userdata)
				  audio-spec-desired sdl-cffi::SDL-Audio-Spec)
	  ;; Set the callback used to fill the audio buffer
	  (setf sdl-cffi::callback (cffi:callback fill-audio-buffer)
		sdl-cffi::userdata (cffi:null-pointer)))

	;; Open the audio device
	(if (eql -1 (sdl-cffi::sdl-open-audio audio-spec-desired
					      audio-spec-obtained))
	    (setf *audio-opened* nil
		  *status* "FAILED to open Audio device.")
	    (setf *audio-opened* t)))

      (when *audio-opened*
	;; Start playing the audio stream
	(sdl-cffi::sdl-pause-audio 0)
	(setf *status* "Audio playing."))

      (sdl:with-events ()
	(:quit-event () t)
	(:video-expose-event () (sdl:update-display))
	(:key-down-event (:key key)
			 (if (sdl:key= key :SDL-KEY-ESCAPE)
			     (sdl:push-quit-event))
			 (if (sdl:key= key :SDL-KEY-SPACE)
			     (setf *audio-remaining* *audio-length*
				   *audio-position* *audio-buffer-start*
				   *status* "Audio playing.")))
	(:idle ()
	       (when *audio-opened*
		 (when (<= *audio-remaining* 0)
		   (setf *status* "Audio complete. Press SPACE to restart.")))
	       (sdl:clear-display sdl:*black*)
	       (sdl:draw-string-solid *status*
			     (sdl:point)
			     :color sdl:*white*)
	       (sdl:update-display)))

      (when *audio-opened*
	;; Pause the audio stream
	(sdl-cffi::sdl-pause-audio 1)
	;; Lock the audio device to halt all callbacks
	(sdl-cffi::sdl-unlock-audio)
	;; Close the audio device
	(sdl-cffi::sdl-close-audio))

      (when *file-found*
	;; Free the audio wav-buffer
	;; NOTE: Dereference the HANDLE returned from SDL-LOAD-WAV and pass
	;; this to SDL-FREE-WAV or a memory leak *will* result.
	;; Derefence the handle.
	(sdl-cffi::sdl-free-wav (cffi:make-pointer (cffi:pointer-address (cffi:mem-aref wav-buffer-handle :pointer))))))))
