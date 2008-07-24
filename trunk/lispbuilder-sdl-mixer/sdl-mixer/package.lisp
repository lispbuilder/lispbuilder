;;; -*- lisp -*-

(in-package #:cl-user)

(defpackage #:lispbuilder-sdl-mixer
  (:use #:cl #:cffi)
  (:nicknames #:sdl-mixer)
  (:documentation "The main package of `lispbuilder-sdl-mixer'.")
  (:import-from #:lispbuilder-sdl-mixer-cffi

		 lispbuilder-sdl-cffi::+CHANNELS+
		 lispbuilder-sdl-cffi::+DEFAULT-FREQUENCY+
		 lispbuilder-sdl-cffi::+DEFAULT-CHANNELS+
		 lispbuilder-sdl-cffi::+MAX-VOLUME+
		 lispbuilder-sdl-cffi::+CHANNEL-POST+
		 lispbuilder-sdl-cffi::SDL-MIXER-VERSION
;;		 lispbuilder-sdl-cffi::VERSION
		 lispbuilder-sdl-cffi::+DEFAULT-FORMAT+

		 lispbuilder-sdl-cffi::CHUNK
		 lispbuilder-sdl-cffi::MUSIC
		 
		 ;; lispbuilder-sdl-cffi::this-fp
;; 		 lispbuilder-sdl-cffi::gc-p
;; 		 lispbuilder-sdl-cffi::simple-free
		 )

  (:export

   ;; 4.1 General 
   ;; 4.1.1 Mix_Linked_Version
   #:Linked-Version
   ;; 4.1.2 Mix_OpenAudio
   #:Open-Audio
   ;; 4.1.3 Mix_CloseAudio
   #:Close-Audio
   ;; 4.1.4 Mix_SetError 
   ;; 4.1.5 Mix_GetError 
   ;; 4.1.6 Mix_QuerySpec
   #:audio-frequency-p
   #:audio-format-p
   #:audio-channels-p
   #:audio-opened-p

   ;; 4.2 Samples 
   ;; 4.2.1 Mix_LoadWAV
   #:Load-sample
   ;; 4.2.2 Mix_LoadWAV_RW 
   ;; 4.2.3 Mix_QuickLoad_WAV 
   ;; 4.2.4 Mix_QuickLoad_RAW 
   ;; 4.2.5 Mix_VolumeChunk
   #:SAMPLE-VOLUME
   ;; 4.2.6 Mix_FreeChunk
   #:FREE

   ;; 4.3 Channels 
   ;; 4.3.1 Mix_AllocateChannels
   #:ALLOCATE-CHANNELS
   ;; 4.3.2 Mix_Volume 
   #:CHANNEL-VOLUME
   ;; 4.3.3 Mix_PlayChannel
   #:PLAY-SAMPLE
   ;; 4.3.4 Mix_PlayChannelTimed
   ;; PLAY-SAMPLE
   ;; 4.3.5 Mix_FadeInChannel
   ;; PLAY-SAMPLE
   ;; 4.3.6 Mix_FadeInChannelTimed
   ;; PLAY-SAMPLE
   ;; 4.3.7 Mix_Pause
   #:PAUSE-CHANNEL
   ;; 4.3.8 Mix_Resume
   #:RESUME-CHANNEL
   ;; 4.3.9 Mix_HaltChannel
   #:HALT-SAMPLE
   ;; 4.3.10 Mix_ExpireChannel
   ;; #:HALT-SAMPLE
   ;; 4.3.11 Mix_FadeOutChannel
   ;; #:HALT-SAMPLE
   ;; 4.3.12 Mix_ChannelFinished
   #:REGISTER-SAMPLE-FINISHED
   #:UNREGISTER-SAMPLE-FINISHED
   ;; 4.3.13 Mix_Playing
   #:SAMPLE-PLAYING-P
   #:CHANEL-HALTED-P
   ;; 4.3.14 Mix_Paused
   #:SAMPLE-PAUSED-P
   ;; 4.3.15 Mix_FadingChannel
   #:SAMPLE-FADING-P
   ;; 4.3.16 Mix_GetChunk
   #:SAMPLE-FROM-CHANNEL

   ;; 4.4 Groups 
   ;; 4.4.1 Mix_ReserveChannels
   #:RESERVE-CHANNELS
   ;; 4.4.2 Mix_GroupChannel
   #:GROUP-CHANNELS
   ;; 4.4.3 Mix_GroupChannels
   #:GROUP-CHANNELS
   ;; 4.4.4 Mix_GroupCount
   #:GROUP-CHANNEL-P
   ;; 4.4.5 Mix_GroupAvailable
   #:GROUP-AVAILABLE-P
   ;; 4.4.6 Mix_GroupOldest
   #:GROUP-OLDEST
   ;; 4.4.7 Mix_GroupNewer
   #:GROUP-RECENT
   ;; 4.4.8 Mix_FadeOutGroup
   ;; #:HALT-SAMPLE
   ;; 4.4.9 Mix_HaltGroup
   ;; #:HALT-SAMPLE

   ;; 4.5 Music 
   ;; 4.5.1 Mix_LoadMUS
   #:Load-MUSIC
   ;; 4.5.2 Mix_FreeMusic
   ;; #:FREE
   ;; 4.5.3 Mix_PlayMusic
   #:PLAY-MUSIC
   ;; 4.5.4 Mix_FadeInMusic
   ;; #:PLAY-MUSIC
   ;; 4.5.5 Mix_FadeInMusicPos
   ;; #:PLAY-MUSIC
   ;; 4.5.6 Mix_HookMusic
   #:REGISTER-MUSIC-MIXER
   #:UNREGISTER-MUSIC-MIXER
   ;; 4.5.7 Mix_VolumeMusic
   #:MUSIC-VOLUME
   ;; 4.5.8 Mix_PauseMusic
   #:PAUSE-MUSIC
   ;; 4.5.9 Mix_ResumeMusic
   #:RESUME-CHANNEL
   ;; 4.5.10 Mix_RewindMusic
   #:REWIND-MUSIC
   ;; 4.5.11 Mix_SetMusicPosition
   #:MUSIC-POSITION
   ;; 4.5.12 Mix_SetMusicCMD
   #:MUSIC-CMD
   ;; 4.5.13 Mix_HaltMusic
   #:HALT-MUSIC
   ;; 4.5.14 Mix_FadeOutMusic
   ;; #:HALT-MUSIC
   ;; 4.5.15 Mix_HookMusicFinished
   #:REGISTER-MUSIC-FINISHED
   #:UNREGISTER-MUSIC-FINISHED
   ;; 4.5.16 Mix_GetMusicType
   #:MUSIC-TYPE-P
   #:MUSIC-TYPE-OF
   ;; 4.5.17 Mix_PlayingMusic
   #:MUSIC-PLAYING-P
   ;; 4.5.18 Mix_PausedMusic
   #:MUSIC-PAUSED-P
   ;; 4.5.19 Mix_FadingMusic
   #:MUSIC-FADING-P
   ;; 4.5.20 Mix_GetMusicHookData 

   ;; 4.6 Effects 
   ;; 4.6.1 Mix_RegisterEffect 
   ;; 4.6.2 Mix_UnregisterEffect 
   ;; 4.6.3 Mix_UnregisterAllEffects 
   ;; 4.6.4 Mix_SetPostMix 
   ;; 4.6.5 Mix_SetPanning 
   ;; 4.6.6 Mix_SetDistance 
   ;; 4.6.7 Mix_SetPosition 
   ;; 4.6.8 Mix_SetReverseStereo


   #:+CHANNELS+
   #:+DEFAULT-FREQUENCY+
   #:+DEFAULT-CHANNELS+
   #:+MAX-VOLUME+
   #:+DEFAULT-FORMAT+
   #:+DEFAULT-SAMPLE-BUFFER+

   #:+CHANNEL-POST+
   #:SDL-MIXER-VERSION
;;   #:VERSION

;;    #:this-fp
;;    #:gc-p
;;    #:simple-free

   #:CHUNK
   #:MUSIC
   #:FREE
   
;;   #:*DEFAULT-MUSIC*
;;   #:*DEFAULT-CHUNK*

   ))