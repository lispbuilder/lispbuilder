;;; -*- lisp -*-

(in-package #:cl-user)

(defpackage #:lispbuilder-sdl-mixer
  (:use #:cl #:cffi)
  (:nicknames #:sdl-mixer)
  (:documentation "The main package of `lispbuilder-sdl-mixer'.")
  (:export

   ;; There are exports from sdl_mixer.lisp
   #:SDL_MIXER_MAJOR_VERSION
   #:SDL_MIXER_MINOR_VERSION
   #:SDL_MIXER_PATCHLEVEL
   #:MIX_MAJOR_VERSION
   #:MIX_MINOR_VERSION
   #:MIX_PATCHLEVEL
   #:Mix_Linked_Version
   #:MIX_CHANNELS
   #:MIX_DEFAULT_FREQUENCY
   #:MIX_DEFAULT_CHANNELS
   #:MIX_MAX_VOLUME
   #:Mix_Chunk
   #:allocated
   #:abuf
   #:alen
   #:volume
   #:Mix_Fading
   #:Mix_MusicType
   #:Mix_OpenAudio
   #:Mix_AllocateChannels
   #:Mix_QuerySpec
   #:Mix_LoadWAV_RW
   #:Mix_LoadMUS
   #:Mix_LoadMUS_RW
   #:Mix_QuickLoad_WAV
   #:Mix_QuickLoad_RAW
   #:Mix_FreeChunk
   #:Mix_FreeMusic
   #:Mix_GetMusicType
   #:Mix_SetPostMix
   #:Mix_HookMusic
   #:Mix_HookMusicFinished
   #:Mix_GetMusicHookData
   #:Mix_ChannelFinished
   #:MIX_CHANNEL_POST
   #:Mix_RegisterEffect
   #:Mix_UnregisterEffect
   #:Mix_UnregisterAllEffects
   #:Mix_SetPanning
   #:Mix_SetPosition
   #:Mix_SetDistance
   #:Mix_SetReverseStereo
   #:Mix_ReserveChannels
   #:Mix_GroupChannel
   #:Mix_GroupChannels
   #:Mix_GroupAvailable
   #:Mix_GroupCount
   #:Mix_GroupOldest
   #:Mix_GroupNewer
   #:Mix_PlayChannelTimed
   #:Mix_PlayMusic
   #:Mix_FadeInMusic
   #:Mix_FadeInMusicPos
   #:Mix_FadeInChannelTimed
   #:Mix_Volume
   #:Mix_VolumeChunk
   #:Mix_VolumeMusic
   #:Mix_HaltChannel
   #:Mix_HaltGroup
   #:Mix_HaltMusic
   #:Mix_ExpireChannel
   #:Mix_FadeOutChannel
   #:Mix_FadeOutGroup
   #:Mix_FadeOutMusic
   #:Mix_FadingMusic
   #:Mix_FadingChannel
   #:Mix_Pause
   #:Mix_Resume
   #:Mix_Paused
   #:Mix_PauseMusic
   #:Mix_ResumeMusic
   #:Mix_RewindMusic
   #:Mix_PausedMusic
   #:Mix_SetMusicPosition
   #:Mix_Playing
   #:Mix_PlayingMusic
   #:Mix_SetMusicCMD
   #:Mix_SetSynchroValue
   #:Mix_GetSynchroValue
   #:Mix_GetChunk
   #:Mix_CloseAudio
   
   ;; These are exports from translate.lisp
   
   ;; These are exports from util-sdl_mixer.lisp

   ;; These are exports from sdlmixerswig.i

   ;; These are exports from post-swig.lisp
   #:SDL_MIXER_VERSION
   #:MIX_VERSION
   #:MIX_DEFAULT_FORMAT
   #:Mix_LoadWAV
   #:MIX_EFFECTSMAXSPEED
   #:Mix_PlayChannel
   #:Mix_FadeInChannel
   #:Mix_GetError))