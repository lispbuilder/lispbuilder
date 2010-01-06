
;;;; SDL_mixer CFFI lisp wrapper
;;;; Part of the CL-Gardeners project
;;;; http://wiki.alu.org/Application_Builder
;;;; (C)2008 Luke J Crook <luke@balooga.com>
;;;; See COPYING for license
;;;;
;;;; This .i file successfully generates CFFI bindings for SDL_mixer version 1.2.8
;;;;
;;;; SWIG command line:
;;;; swig -cffi -I<path_to_SDL_mixer_includes> -I<path_to_SDL_includes> -Ilib -Ilib\cffi <location_of_openrmswig.i>

(in-package #:lispbuilder-sdl-mixer-cffi)

;;; This is to handle a C macro where 1 is shifted left n times
(defun 1<<(x) (ash 1 x))

;;;; These defctypes are used by the CFFI translation functions
;;;; see the typemap definition below.
(cffi:defctype mix-chunk-fp (:wrapper :pointer 
				      :to-c to-mix-chunk))
(cffi:defctype mix-music-fp (:wrapper :pointer 
				      :to-c to-mix-music))


(defconstant +SDL-MIXER-MAJOR-VERSION+ 1)
(defconstant +SDL-MIXER-MINOR-VERSION+ 2)
(defconstant +SDL-MIXER-PATCH-LEVEL+   11)

(defcfun ("Mix_Linked_Version" Linked-Version) sdl-cffi::sdl-version)

(cffi:defcenum Init-Flags
  (:INIT-FLAC #x01)
  (:INIT-MOD  #x02)
  (:INIT-MP3  #x04)
  (:INIT-OGG  #x08))

(cffi:defcfun ("Mix_FreeChunk" FREE-CHUNK) :void
  (chunk :pointer))

(cffi:defcfun ("Mix_FreeMusic" FREE-MUSIC) :void
  (music :pointer))

(defun SDL-MIXER-VERSION (sdl-version)
  (cffi:with-foreign-slots ((sdl-cffi::major sdl-cffi::minor sdl-cffi::patch) sdl-version sdl-cffi::sdl-version)
    (setf sdl-cffi::major +SDL-MIXER-MAJOR-VERSION+
          sdl-cffi::minor +SDL-MIXER-MINOR-VERSION+
          sdl-cffi::patch +SDL-MIXER-PATCH-LEVEL+)))

(defun VERSION (x)
  (SDL-MIXER-VERSION x))

#-(or little-endian PC386 X86 I386) (defconstant +DEFAULT-FORMAT+ sdl-cffi::AUDIO-S16MSB) ;; Big Endian
#+(or little-endian PC386 X86 I386) (defconstant +DEFAULT-FORMAT+ sdl-cffi::AUDIO-S16LSB) ;; Little Endian

(defun Load-WAV (file)
  (Load-WAV-RW (sdl-cffi::SDL-RW-FROM-FILE file "rb") 1))

(defun Play-Channel (channel chunk loops)
  (Play-Channel-Timed channel chunk loops -1))

(defun Fade-In-Channel (channel chunk loops ms)
  (Fade-In-Channel-Timed channel chunk loops ms -1))

(defun Get-Error ()
  (sdl-cffi::SDL-Get-Error))

(defconstant +CHANNELS+ 8)
(defconstant +DEFAULT-FREQUENCY+ 22050)
(defconstant +DEFAULT-CHANNELS+ 2)
(defconstant +MAX-VOLUME+ 128)

(cffi:defcstruct chunk
  (allocated :int)
  (abuf :pointer)
  (alen :unsigned-int)
  (volume :unsigned-char))

(cffi:defcenum fading
  :NO-FADING
  :FADING-OUT
  :FADING-IN)

(cffi:defcenum Music-Type
  :MUS-NONE
  :MUS-CMD
  :MUS-WAV
  :MUS-MOD
  :MUS-MID
  :MUS-OGG
  :MUS-MP3
  :MUS-MP3-MAD
  :MUS-FLAC)

(cffi:defcfun ("Mix_OpenAudio" Open-Audio) :int
  (frequency :int)
  (format :unsigned-short)
  (channels :int)
  (chunksize :int))

(cffi:defcfun ("Mix_AllocateChannels" Allocate-Channels) :int
  (numchans :int))

(cffi:defcfun ("Mix_QuerySpec" Query-Spec) :int
  (frequency :pointer)
  (format :pointer)
  (channels :pointer))

(cffi:defcfun ("Mix_LoadWAV_RW" Load-WAV-RW) :pointer
  (src :pointer)
  (freesrc :int))

(cffi:defcfun ("Mix_LoadMUS" Load-MUS) :pointer
  (file :string))

(cffi:defcfun ("Mix_LoadMUS_RW" Load-MUS-RW) :pointer
  (rw :pointer))

(cffi:defcfun ("Mix_QuickLoad_WAV" Quick-Load-WAV) :pointer
  (mem :pointer))

(cffi:defcfun ("Mix_QuickLoad_RAW" Quick-Load-RAW) :pointer
  (mem :pointer)
  (len :unsigned-int))

(cffi:defcfun ("Mix_GetMusicType" Get-Music-Type) Music-Type
  (music mix-music-fp))

(cffi:defcfun ("Mix_SetPostMix" Set-Post-Mix) :void
  (mix-func :pointer)
  (arg :pointer))

(cffi:defcfun ("Mix_HookMusic" Hook-Music) :void
  (mix-func :pointer)
  (arg :pointer))

(cffi:defcfun ("Mix_HookMusicFinished" Hook-Music-Finished) :void
  (music-finished :pointer))

(cffi:defcfun ("Mix_GetMusicHookData" Get-Music-Hook-Data) :pointer)

(cffi:defcfun ("Mix_ChannelFinished" Channel-Finished) :void
  (channel-finished :pointer))

(cl:defconstant +CHANNEL-POST+ -2)

(cffi:defcfun ("Mix_RegisterEffect" Register-Effect) :int
  (chan :int)
  (f :pointer)
  (d :pointer)
  (arg :pointer))

(cffi:defcfun ("Mix_UnregisterEffect" Unregister-Effect) :int
  (channel :int)
  (f :pointer))

(cffi:defcfun ("Mix_UnregisterAllEffects" Unregister-All-Effects) :int
  (channel :int))

(cffi:defcfun ("Mix_SetPanning" Set-Panning) :int
  (channel :int)
  (left :unsigned-char)
  (right :unsigned-char))

(cffi:defcfun ("Mix_SetPosition" Set-Position) :int
  (channel :int)
  (angle :short)
  (distance :unsigned-char))

(cffi:defcfun ("Mix_SetDistance" Set-Distance) :int
  (channel :int)
  (distance :unsigned-char))

(cffi:defcfun ("Mix_SetReverseStereo" Set-Reverse-Stereo) :int
  (channel :int)
  (flip :int))

(cffi:defcfun ("Mix_ReserveChannels" Reserve-Channels) :int
  (num :int))

(cffi:defcfun ("Mix_GroupChannel" Group-Channel) :int
  (which :int)
  (tag :int))

(cffi:defcfun ("Mix_GroupChannels" Group-Channels) :int
  (from :int)
  (to :int)
  (tag :int))

(cffi:defcfun ("Mix_GroupAvailable" Group-Available) :int
  (tag :int))

(cffi:defcfun ("Mix_GroupCount" Group-Count) :int
  (tag :int))

(cffi:defcfun ("Mix_GroupOldest" Group-Oldest) :int
  (tag :int))

(cffi:defcfun ("Mix_GroupNewer" Group-Newer) :int
  (tag :int))

(cffi:defcfun ("Mix_PlayChannelTimed" Play-Channel-Timed) :int
  (channel :int)
  (chunk mix-chunk-fp)
  (loops :int)
  (ticks :int))

(cffi:defcfun ("Mix_PlayMusic" Play-Music) :int
  (music mix-music-fp)
  (loops :int))

(cffi:defcfun ("Mix_FadeInMusic" Fade-In-Music) :int
  (music mix-music-fp)
  (loops :int)
  (ms :int))

(cffi:defcfun ("Mix_FadeInMusicPos" Fade-In-Music-Pos) :int
  (music mix-music-fp)
  (loops :int)
  (ms :int)
  (position :double))

(cffi:defcfun ("Mix_FadeInChannelTimed" Fade-In-Channel-Timed) :int
  (channel :int)
  (chunk mix-chunk-fp)
  (loops :int)
  (ms :int)
  (ticks :int))

(cffi:defcfun ("Mix_Volume" Volume) :int
  (channel :int)
  (volume :int))

(cffi:defcfun ("Mix_VolumeChunk" Volume-Chunk) :int
  (chunk mix-chunk-fp)
  (volume :int))

(cffi:defcfun ("Mix_VolumeMusic" Volume-Music) :int
  (volume :int))

(cffi:defcfun ("Mix_HaltChannel" Halt-Channel) :int
  (channel :int))

(cffi:defcfun ("Mix_HaltGroup" Halt-Group) :int
  (tag :int))

(cffi:defcfun ("Mix_HaltMusic" Halt-Music) :int)

(cffi:defcfun ("Mix_ExpireChannel" Expire-Channel) :int
  (channel :int)
  (ticks :int))

(cffi:defcfun ("Mix_FadeOutChannel" Fade-Out-Channel) :int
  (which :int)
  (ms :int))

(cffi:defcfun ("Mix_FadeOutGroup" Fade-Out-Group) :int
  (tag :int)
  (ms :int))

(cffi:defcfun ("Mix_FadeOutMusic" Fade-Out-Music) :int
  (ms :int))

(cffi:defcfun ("Mix_FadingMusic" Fading-Music) Fading)

(cffi:defcfun ("Mix_FadingChannel" Fading-Channel) Fading
  (which :int))

(cffi:defcfun ("Mix_Pause" Pause) :void
  (channel :int))

(cffi:defcfun ("Mix_Resume" Resume) :void
  (channel :int))

(cffi:defcfun ("Mix_Paused" Paused) :int
  (channel :int))

(cffi:defcfun ("Mix_PauseMusic" Pause-Music) :void)

(cffi:defcfun ("Mix_ResumeMusic" Resume-Music) :void)

(cffi:defcfun ("Mix_RewindMusic" Rewind-Music) :void)

(cffi:defcfun ("Mix_PausedMusic" Paused-Music) :int)

(cffi:defcfun ("Mix_SetMusicPosition" Set-Music-Position) :int
  (position :double))

(cffi:defcfun ("Mix_Playing" Playing) :int
  (channel :int))

(cffi:defcfun ("Mix_PlayingMusic" Playing-Music) :int)

(cffi:defcfun ("Mix_SetMusicCMD" Set-Music-CMD) :int
  (command :string))

(cffi:defcfun ("Mix_SetSynchroValue" Set-Synchro-Value) :int
  (value :int))

(cffi:defcfun ("Mix_GetSynchroValue" Get-Synchro-Value) :int)

(cffi:defcfun ("Mix_GetChunk" Get-Chunk) :pointer
  (channel :int))

(cffi:defcfun ("Mix_CloseAudio" Close-Audio) :void)

