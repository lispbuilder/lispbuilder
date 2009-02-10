
(in-package #:lispbuilder-sdl-cffi)

#+lispbuilder-sdl-audio
(cffi:defcfun ("SDL_glue_SDL_OpenAudio"
               sdl-glue-SDL-Open-Audio) :int
  (desired :pointer)
  (obtained :pointer))

#+lispbuilder-sdl-audio
(cffi:defcfun ("SDL_glue_SDL_CloseAudio"
               SDL-glue-SDL-Close-Audio) :void)

#+lispbuilder-sdl-audio
(cffi:defcfun ("SDL_glue_SDL_BufferFilled"
               SDL-glue-SDL-Buffer-Filled) :int)

#+lispbuilder-sdl-audio
(cffi:defcfun ("SDL_glue_SDL_RequireBufferFill"
               SDL-glue-SDL-Require-Buffer-Fill) :int)

#+lispbuilder-sdl-audio
(cffi:defcfun ("SDL_glue_SDL_GetAudioBuffer"
               SDL-glue-SDL-Get-Audio-Buffer) :pointer)

#+lispbuilder-sdl-audio
(cffi:defcfun ("SDL_glue_SDL_GetAudioBufferLength"
               SDL-glue-SDL-Get-Audio-Buffer-length) :int)
