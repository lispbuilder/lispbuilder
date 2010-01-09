
(in-package #:lispbuilder-sdl-cffi)

#+lispbuilder-sdl-audio
(progn
  (cffi:defcfun ("SDL_glue_SDL_OpenAudio"
		 sdl-glue-SDL-Open-Audio) :int
    (desired :pointer)
    (obtained :pointer))
	
  (cffi:defcfun ("SDL_glue_SDL_CloseAudio"
		 SDL-glue-SDL-Close-Audio) :void)
	
  (cffi:defcfun ("SDL_glue_SDL_BufferFilled"
		 SDL-glue-SDL-Buffer-Filled) :int)

  (cffi:defcfun ("SDL_glue_SDL_RequireBufferFill"
		 SDL-glue-SDL-Require-Buffer-Fill) :int)

  (cffi:defcfun ("SDL_glue_SDL_GetAudioBuffer"
		 SDL-glue-SDL-Get-Audio-Buffer) :pointer)

  (cffi:defcfun ("SDL_glue_SDL_GetAudioBufferLength"
		 SDL-glue-SDL-Get-Audio-Buffer-length) :int))

;; SBCL and CLISP should never, ever use the raw SDL audio routines without the glue library,
;; or nastly crashes will result.
(defun open-audio (desired-spec obtained-spec)
  (if (cffi:foreign-symbol-pointer "SDL_glue_SDL_OpenAudio")
      (cffi:foreign-funcall-pointer (cffi:foreign-symbol-pointer "SDL_glue_SDL_OpenAudio") () :pointer desired-spec :pointer obtained-spec :int)
      #+(or clisp sbcl)(progn -1)
      #-(or clisp sbcl)(cffi:foreign-funcall-pointer (cffi:foreign-symbol-pointer "SDL_OpenAudio" :library 'sdl) () :pointer desired-spec :pointer obtained-spec :int)))

;; SBCL and CLISP should never, ever use the raw SDL audio routines without the glue library,
;; or nastly crashes will result.
(defun close-audio ()
  (if (cffi:foreign-symbol-pointer "SDL_glue_SDL_CloseAudio")
      (cffi:foreign-funcall-pointer (cffi:foreign-symbol-pointer "SDL_glue_SDL_CloseAudio") () :void)
      #+(or clisp sbcl cmucl)(progn -1)
      #-(or clisp sbcl cmucl)(cffi:foreign-funcall-pointer (cffi:foreign-symbol-pointer "SDL_CloseAudio" :library 'sdl) () :void)))

(defun require-buffer-fill ()
  (let ((fp (cffi:foreign-symbol-pointer "SDL_glue_SDL_RequireBufferFill")))
    (when fp
      (when (= 1 (cffi:foreign-funcall-pointer fp () :int))
	t))))

(defun buffer-filled ()
  (let ((fp (cffi:foreign-symbol-pointer "SDL_glue_SDL_BufferFilled")))
    (when fp
      (cffi:foreign-funcall-pointer fp () :int))))

(defun get-audio-buffer ()
  (let ((fp (cffi:foreign-symbol-pointer "SDL_glue_SDL_GetAudioBuffer")))
    (when fp
      (cffi:foreign-funcall-pointer fp () :pointer))))

(defun get-audio-buffer-length ()
  (let ((fp (cffi:foreign-symbol-pointer "SDL_glue_SDL_GetAudioBufferLength")))
    (when fp
      (cffi:foreign-funcall-pointer fp () :int))))

