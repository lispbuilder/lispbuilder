
;;;; SDL_mixer CFFI lisp wrapper
;;;; Part of the CL-Gardeners project
;;;; http://wiki.alu.org/Application_Builder
;;;; (C)2006 Luke J Crook
;;;; See COPYING for license
;;;;
;;;; This .i file has been tested with SDL_mixer version 1.2.7

;;; To generate the SDL_mixer bindings using SWIG:
;;;  - Copy the "begin_code.h" header file from SDL-1.2.11 into the SDL_mixer include directory.
;;;    "begin_code.h" includes "#define SDLCALL __cdecl"

(in-package #:lispbuilder-sdl-mixer)

;;; Macro to handle defenum (thanks to Frank Buss for this SWIG/CFFI feature)
;; this handles anonymous enum types differently

(defmacro defenum (&body enums)	
 `(progn ,@(loop for value in enums
                 for index = 0 then (1+ index)
                 when (listp value) do (setf index (second value)
                                             value (first value))
                 collect `(defconstant ,value ,index))))

;;; This is to handle a C macro where 1 is shifted left n times
(defun 1<<(x) (ash 1 x))

;;;; Overrides to C header files follow:
;;;;

;;;;
;;;; end Overrides



;;;SWIG wrapper code starts here

(defmacro defanonenum (&body enums)
   "Converts anonymous enums to defconstants."
  `(progn ,@(loop for value in enums
                  for index = 0 then (1+ index)
                  when (listp value) do (setf index (second value)
                                              value (first value))
                  collect `(defconstant ,value ,index))))

;;;SWIG wrapper code ends here


(cl:defconstant SDL_MIXER_MAJOR_VERSION 1)

(cl:defconstant SDL_MIXER_MINOR_VERSION 2)

(cl:defconstant SDL_MIXER_PATCHLEVEL 7)

(cl:defconstant MIX_MAJOR_VERSION 1)

(cl:defconstant MIX_MINOR_VERSION 2)

(cl:defconstant MIX_PATCHLEVEL 7)

(cffi:defcfun ("Mix_Linked_Version" Mix_Linked_Version) :pointer)

(cl:defconstant MIX_CHANNELS 8)

(cl:defconstant MIX_DEFAULT_FREQUENCY 22050)

(cl:defconstant MIX_DEFAULT_CHANNELS 2)

(cl:defconstant MIX_MAX_VOLUME 128)

(cffi:defcstruct Mix_Chunk
	(allocated :int)
	(abuf :pointer)
	(alen :unsigned-int)
	(volume :unsigned-char))

(cffi:defcenum Mix_Fading
	:MIX_NO_FADING
	:MIX_FADING_OUT
	:MIX_FADING_IN)

(cffi:defcenum Mix_MusicType
	:MUS_NONE
	:MUS_CMD
	:MUS_WAV
	:MUS_MOD
	:MUS_MID
	:MUS_OGG
	:MUS_MP3)

(cffi:defcfun ("Mix_OpenAudio" Mix_OpenAudio) :int
  (frequency :int)
  (format :unsigned-short)
  (channels :int)
  (chunksize :int))

(cffi:defcfun ("Mix_AllocateChannels" Mix_AllocateChannels) :int
  (numchans :int))

(cffi:defcfun ("Mix_QuerySpec" Mix_QuerySpec) :int
  (frequency :pointer)
  (format :pointer)
  (channels :pointer))

(cffi:defcfun ("Mix_LoadWAV_RW" Mix_LoadWAV_RW) :pointer
  (src :pointer)
  (freesrc :int))

(cffi:defcfun ("Mix_LoadMUS" Mix_LoadMUS) :pointer
  (file :string))

(cffi:defcfun ("Mix_LoadMUS_RW" Mix_LoadMUS_RW) :pointer
  (rw :pointer))

(cffi:defcfun ("Mix_QuickLoad_WAV" Mix_QuickLoad_WAV) :pointer
  (mem :pointer))

(cffi:defcfun ("Mix_QuickLoad_RAW" Mix_QuickLoad_RAW) :pointer
  (mem :pointer)
  (len :unsigned-int))

(cffi:defcfun ("Mix_FreeChunk" Mix_FreeChunk) :void
  (chunk :pointer))

(cffi:defcfun ("Mix_FreeMusic" Mix_FreeMusic) :void
  (music :pointer))

(cffi:defcfun ("Mix_GetMusicType" Mix_GetMusicType) Mix_MusicType
  (music :pointer))

(cffi:defcfun ("Mix_SetPostMix" Mix_SetPostMix) :void
  (mix_func :pointer)
  (arg :pointer))

(cffi:defcfun ("Mix_HookMusic" Mix_HookMusic) :void
  (mix_func :pointer)
  (arg :pointer))

(cffi:defcfun ("Mix_HookMusicFinished" Mix_HookMusicFinished) :void
  (music_finished :pointer))

(cffi:defcfun ("Mix_GetMusicHookData" Mix_GetMusicHookData) :pointer)

(cffi:defcfun ("Mix_ChannelFinished" Mix_ChannelFinished) :void
  (channel_finished :pointer))

(cl:defconstant MIX_CHANNEL_POST -2)

(cffi:defcfun ("Mix_RegisterEffect" Mix_RegisterEffect) :int
  (chan :int)
  (f :pointer)
  (d :pointer)
  (arg :pointer))

(cffi:defcfun ("Mix_UnregisterEffect" Mix_UnregisterEffect) :int
  (channel :int)
  (f :pointer))

(cffi:defcfun ("Mix_UnregisterAllEffects" Mix_UnregisterAllEffects) :int
  (channel :int))

(cffi:defcfun ("Mix_SetPanning" Mix_SetPanning) :int
  (channel :int)
  (left :unsigned-char)
  (right :unsigned-char))

(cffi:defcfun ("Mix_SetPosition" Mix_SetPosition) :int
  (channel :int)
  (angle :short)
  (distance :unsigned-char))

(cffi:defcfun ("Mix_SetDistance" Mix_SetDistance) :int
  (channel :int)
  (distance :unsigned-char))

(cffi:defcfun ("Mix_SetReverseStereo" Mix_SetReverseStereo) :int
  (channel :int)
  (flip :int))

(cffi:defcfun ("Mix_ReserveChannels" Mix_ReserveChannels) :int
  (num :int))

(cffi:defcfun ("Mix_GroupChannel" Mix_GroupChannel) :int
  (which :int)
  (tag :int))

(cffi:defcfun ("Mix_GroupChannels" Mix_GroupChannels) :int
  (from :int)
  (to :int)
  (tag :int))

(cffi:defcfun ("Mix_GroupAvailable" Mix_GroupAvailable) :int
  (tag :int))

(cffi:defcfun ("Mix_GroupCount" Mix_GroupCount) :int
  (tag :int))

(cffi:defcfun ("Mix_GroupOldest" Mix_GroupOldest) :int
  (tag :int))

(cffi:defcfun ("Mix_GroupNewer" Mix_GroupNewer) :int
  (tag :int))

(cffi:defcfun ("Mix_PlayChannelTimed" Mix_PlayChannelTimed) :int
  (channel :int)
  (chunk :pointer)
  (loops :int)
  (ticks :int))

(cffi:defcfun ("Mix_PlayMusic" Mix_PlayMusic) :int
  (music :pointer)
  (loops :int))

(cffi:defcfun ("Mix_FadeInMusic" Mix_FadeInMusic) :int
  (music :pointer)
  (loops :int)
  (ms :int))

(cffi:defcfun ("Mix_FadeInMusicPos" Mix_FadeInMusicPos) :int
  (music :pointer)
  (loops :int)
  (ms :int)
  (position :double))

(cffi:defcfun ("Mix_FadeInChannelTimed" Mix_FadeInChannelTimed) :int
  (channel :int)
  (chunk :pointer)
  (loops :int)
  (ms :int)
  (ticks :int))

(cffi:defcfun ("Mix_Volume" Mix_Volume) :int
  (channel :int)
  (volume :int))

(cffi:defcfun ("Mix_VolumeChunk" Mix_VolumeChunk) :int
  (chunk :pointer)
  (volume :int))

(cffi:defcfun ("Mix_VolumeMusic" Mix_VolumeMusic) :int
  (volume :int))

(cffi:defcfun ("Mix_HaltChannel" Mix_HaltChannel) :int
  (channel :int))

(cffi:defcfun ("Mix_HaltGroup" Mix_HaltGroup) :int
  (tag :int))

(cffi:defcfun ("Mix_HaltMusic" Mix_HaltMusic) :int)

(cffi:defcfun ("Mix_ExpireChannel" Mix_ExpireChannel) :int
  (channel :int)
  (ticks :int))

(cffi:defcfun ("Mix_FadeOutChannel" Mix_FadeOutChannel) :int
  (which :int)
  (ms :int))

(cffi:defcfun ("Mix_FadeOutGroup" Mix_FadeOutGroup) :int
  (tag :int)
  (ms :int))

(cffi:defcfun ("Mix_FadeOutMusic" Mix_FadeOutMusic) :int
  (ms :int))

(cffi:defcfun ("Mix_FadingMusic" Mix_FadingMusic) Mix_Fading)

(cffi:defcfun ("Mix_FadingChannel" Mix_FadingChannel) Mix_Fading
  (which :int))

(cffi:defcfun ("Mix_Pause" Mix_Pause) :void
  (channel :int))

(cffi:defcfun ("Mix_Resume" Mix_Resume) :void
  (channel :int))

(cffi:defcfun ("Mix_Paused" Mix_Paused) :int
  (channel :int))

(cffi:defcfun ("Mix_PauseMusic" Mix_PauseMusic) :void)

(cffi:defcfun ("Mix_ResumeMusic" Mix_ResumeMusic) :void)

(cffi:defcfun ("Mix_RewindMusic" Mix_RewindMusic) :void)

(cffi:defcfun ("Mix_PausedMusic" Mix_PausedMusic) :int)

(cffi:defcfun ("Mix_SetMusicPosition" Mix_SetMusicPosition) :int
  (position :double))

(cffi:defcfun ("Mix_Playing" Mix_Playing) :int
  (channel :int))

(cffi:defcfun ("Mix_PlayingMusic" Mix_PlayingMusic) :int)

(cffi:defcfun ("Mix_SetMusicCMD" Mix_SetMusicCMD) :int
  (command :string))

(cffi:defcfun ("Mix_SetSynchroValue" Mix_SetSynchroValue) :int
  (value :int))

(cffi:defcfun ("Mix_GetSynchroValue" Mix_GetSynchroValue) :int)

(cffi:defcfun ("Mix_GetChunk" Mix_GetChunk) :pointer
  (channel :int))

(cffi:defcfun ("Mix_CloseAudio" Mix_CloseAudio) :void)


