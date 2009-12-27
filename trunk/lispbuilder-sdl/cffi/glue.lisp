
(in-package #:lispbuilder-sdl-cffi)

#+(and sbcl lispbuilder-sdl-audio (or mswindows windows win32))
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
#-sbcl(progn
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
