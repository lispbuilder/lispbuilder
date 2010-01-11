
(in-package #:lispbuilder-sdl-cffi)

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

