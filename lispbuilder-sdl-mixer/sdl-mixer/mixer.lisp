
;;;; SDL_mixer CFFI lisp wrapper
;;;; Part of the CL-Gardeners project
;;;; http://wiki.alu.org/Application_Builder
;;;; (C)2006 Luke J Crook
;;;; See COPYING for license
;;;;

(in-package #:lispbuilder-sdl-mixer)


(cl:defconstant SDL-MIXER-MAJOR-VERSION 1)

(cl:defconstant SDL-MIXER-MINOR-VERSION 2)

(cl:defconstant SDL-MIXER-PATCH-LEVEL 7)

(cl:defconstant MIX-MAJOR-VERSION 1)

(cl:defconstant MIX-MINOR-VERSION 2)

(cl:defconstant MIX-PATCH-LEVEL 7)

(cffi:defcfun ("Mix_Linked_Version" Mix-Linked-Version) :pointer)

(cl:defconstant MIX-CHANNELS 8)

(cl:defconstant MIX-DEFAULT-FREQUENCY 22050)

(cl:defconstant MIX-DEFAULT-CHANNELS 2)

(cl:defconstant MIX-MAX-VOLUME 128)

(cffi:defcstruct Mix-Chunk
	(allocated :int)
	(abuf :pointer)
	(alen :unsigned-int)
	(volume :unsigned-char))

(cffi:defcenum Mix-Fading
	:MIX-NO-FADING
	:MIX-FADING-OUT
	:MIX-FADING-IN)

(cffi:defcenum Mix-Music-Type
	:MUS-NONE
	:MUS-CMD
	:MUS-WAV
	:MUS-MOD
	:MUS-MID
	:MUS-OGG
	:MUS-MP3)

(cffi:defcfun ("Mix_OpenAudio" Mix-Open-Audio) :int
  (frequency :int)
  (format :unsigned-short)
  (channels :int)
  (chunksize :int))

(cffi:defcfun ("Mix_AllocateChannels" Mix-Allocate-Channels) :int
  (numchans :int))

(cffi:defcfun ("Mix_QuerySpec" Mix-Query-Spec) :int
  (frequency :pointer)
  (format :pointer)
  (channels :pointer))

(cffi:defcfun ("Mix_LoadWAV_RW" Mix-Load-WAV-RW) :pointer
  (src :pointer)
  (freesrc :int))

(cffi:defcfun ("Mix_LoadMUS" Mix-Load-MUS) :pointer
  (file :string))

(cffi:defcfun ("Mix_LoadMUS_RW" Mix-Load-MUS-RW) :pointer
  (rw :pointer))

(cffi:defcfun ("Mix_QuickLoad_WAV" Mix-Quick-Load-WAV) :pointer
  (mem :pointer))

(cffi:defcfun ("Mix_QuickLoad_RAW" Mix-Quick-Load-RAW) :pointer
  (mem :pointer)
  (len :unsigned-int))

(cffi:defcfun ("Mix_FreeChunk" Mix-Free-Chunk) :void
  (chunk :pointer))

(cffi:defcfun ("Mix_FreeMusic" Mix-Free-Music) :void
  (music :pointer))

(cffi:defcfun ("Mix_GetMusicType" Mix-Get-Music-Type) Mix-MusicType
  (music :pointer))

(cffi:defcfun ("Mix_SetPostMix" Mix-Set-Post-Mix) :void
  (mix-func :pointer)
  (arg :pointer))

(cffi:defcfun ("Mix_HookMusic" Mix-Hook-Music) :void
  (mix-func :pointer)
  (arg :pointer))

(cffi:defcfun ("Mix_HookMusicFinished" Mix-Hook-Music-Finished) :void
  (music-finished :pointer))

(cffi:defcfun ("Mix_GetMusicHookData" Mix-Get-Music-Hook-Data) :pointer)

(cffi:defcfun ("Mix_ChannelFinished" Mix-Channel-Finished) :void
  (channel-finished :pointer))

(cl:defconstant MIX-CHANNEL-POST -2)

(cffi:defcfun ("Mix_RegisterEffect" Mix-Register-Effect) :int
  (chan :int)
  (f :pointer)
  (d :pointer)
  (arg :pointer))

(cffi:defcfun ("Mix_UnregisterEffect" Mix-Unregister-Effect) :int
  (channel :int)
  (f :pointer))

(cffi:defcfun ("Mix_UnregisterAllEffects" Mix-Unregister-All-Effects) :int
  (channel :int))

(cffi:defcfun ("Mix_SetPanning" Mix-Set-Panning) :int
  (channel :int)
  (left :unsigned-char)
  (right :unsigned-char))

(cffi:defcfun ("Mix_SetPosition" Mix-Set-Position) :int
  (channel :int)
  (angle :short)
  (distance :unsigned-char))

(cffi:defcfun ("Mix_SetDistance" Mix-Set-Distance) :int
  (channel :int)
  (distance :unsigned-char))

(cffi:defcfun ("Mix_SetReverseStereo" Mix-Set-Reverse-Stereo) :int
  (channel :int)
  (flip :int))

(cffi:defcfun ("Mix_ReserveChannels" Mix-Reserve-Channels) :int
  (num :int))

(cffi:defcfun ("Mix_GroupChannel" Mix-Group-Channel) :int
  (which :int)
  (tag :int))

(cffi:defcfun ("Mix_GroupChannels" Mix-Group-Channels) :int
  (from :int)
  (to :int)
  (tag :int))

(cffi:defcfun ("Mix_GroupAvailable" Mix-Group-Available) :int
  (tag :int))

(cffi:defcfun ("Mix_GroupCount" Mix-Group-Count) :int
  (tag :int))

(cffi:defcfun ("Mix_GroupOldest" Mix-Group-Oldest) :int
  (tag :int))

(cffi:defcfun ("Mix_GroupNewer" Mix-Group-Newer) :int
  (tag :int))

(cffi:defcfun ("Mix_PlayChannelTimed" Mix-Play-Channel-Timed) :int
  (channel :int)
  (chunk :pointer)
  (loops :int)
  (ticks :int))

(cffi:defcfun ("Mix_PlayMusic" Mix-Play-Music) :int
  (music :pointer)
  (loops :int))

(cffi:defcfun ("Mix_FadeInMusic" Mix-Fade-In-Music) :int
  (music :pointer)
  (loops :int)
  (ms :int))

(cffi:defcfun ("Mix_FadeInMusicPos" Mix-Fade-In-Music-Pos) :int
  (music :pointer)
  (loops :int)
  (ms :int)
  (position :double))

(cffi:defcfun ("Mix_FadeInChannelTimed" Mix-Fade-In-Channel-Timed) :int
  (channel :int)
  (chunk :pointer)
  (loops :int)
  (ms :int)
  (ticks :int))

(cffi:defcfun ("Mix_Volume" Mix-Volume) :int
  (channel :int)
  (volume :int))

(cffi:defcfun ("Mix_VolumeChunk" Mix-Volume-Chunk) :int
  (chunk :pointer)
  (volume :int))

(cffi:defcfun ("Mix_VolumeMusic" Mix-Volume-Music) :int
  (volume :int))

(cffi:defcfun ("Mix_HaltChannel" Mix-Halt-Channel) :int
  (channel :int))

(cffi:defcfun ("Mix_HaltGroup" Mix-Halt-Group) :int
  (tag :int))

(cffi:defcfun ("Mix_HaltMusic" Mix-Halt-Music) :int)

(cffi:defcfun ("Mix_ExpireChannel" Mix-Expire-Channel) :int
  (channel :int)
  (ticks :int))

(cffi:defcfun ("Mix_FadeOutChannel" Mix-Fade-Out-Channel) :int
  (which :int)
  (ms :int))

(cffi:defcfun ("Mix_FadeOutGroup" Mix-Fade-Out-Group) :int
  (tag :int)
  (ms :int))

(cffi:defcfun ("Mix_FadeOutMusic" Mix-Fade-Out-Music) :int
  (ms :int))

(cffi:defcfun ("Mix_FadingMusic" Mix-Fading-Music) Mix-Fading)

(cffi:defcfun ("Mix_FadingChannel" Mix-Fading-Channel) Mix-Fading
  (which :int))

(cffi:defcfun ("Mix_Pause" Mix-Pause) :void
  (channel :int))

(cffi:defcfun ("Mix_Resume" Mix-Resume) :void
  (channel :int))

(cffi:defcfun ("Mix_Paused" Mix-Paused) :int
  (channel :int))

(cffi:defcfun ("Mix_PauseMusic" Mix-Pause-Music) :void)

(cffi:defcfun ("Mix_ResumeMusic" Mix-Resume-Music) :void)

(cffi:defcfun ("Mix_RewindMusic" Mix-Rewind-Music) :void)

(cffi:defcfun ("Mix_PausedMusic" Mix-Paused-Music) :int)

(cffi:defcfun ("Mix_SetMusicPosition" Mix-Set-Music-Position) :int
  (position :double))

(cffi:defcfun ("Mix_Playing" Mix-Playing) :int
  (channel :int))

(cffi:defcfun ("Mix_PlayingMusic" Mix-Playing-Music) :int)

(cffi:defcfun ("Mix_SetMusicCMD" Mix-Set-Music-CMD) :int
  (command :string))

(cffi:defcfun ("Mix_SetSynchroValue" Mix-Set-Synchro-Value) :int
  (value :int))

(cffi:defcfun ("Mix_GetSynchroValue" Mix-Get-Synchro-Value) :int)

(cffi:defcfun ("Mix_GetChunk" Mix-Get-Chunk) :pointer
  (channel :int))

(cffi:defcfun ("Mix_CloseAudio" Mix-Close-Audio) :void)


(defun SDL-MIXER-VERSION (x)
  (cffi:with-foreign-slots ((sdl:major sdl:minor sdl:patch) x sdl-cffi:sdl-version)
    (setf sdl:major SDL-MIXER-MAJOR-VERSION
          sdl:minor SDL-MIXER-MINOR-VERSION
          sdl:patch SDL-MIXER-PATCH-LEVEL)))

(defun MIX-VERSION (x)
  (SDL-MIXER-VERSION x))

#-(or little-endian PC386 X86 I386) (defconstant MIX-DEFAULT-FORMAT sdl-cffi::AUDIO-S16MSB) ;; Big Endian
#+(or little-endian PC386 X86 I386) (defconstant MIX-DEFAULT-FORMAT sdl-cffi::AUDIO-S16LSB) ;; Little Endian

(defun Mix-Load-WAV (file)
  (Mix-Load-WAV-RW (sdl-cffi::SDL-RW-FROM-FILE file "rb") 1))

(defconstant MIX-EFFECTS-MAX-SPEED "MIX_EFFECTSMAXSPEED")

(defun Mix-Play-Channel (channel chunk loops)
  (Mix-Play-Channel-Timed channel chunk loops -1))

(defun Mix-Fade-In-Channel (channel chunk loops ms)
  (Mix-Fade-In-Channel-Timed channel chunk loops ms -1))

(defun Mix-Get-Error ()
  (sdl-cffi::SDL-Get-Error))
