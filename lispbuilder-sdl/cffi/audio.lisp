
(in-package #:lispbuilder-sdl-cffi)


(cffi:defcstruct sdl-Audio-Spec
	(freq :int)
	(format :unsigned-short)
	(channels :unsigned-char)
	(silence :unsigned-char)
	(samples :unsigned-short)
	(padding :unsigned-short)
	(size :unsigned-int)
	(callback :pointer)
	(userdata :pointer))

(cl:defconstant AUDIO-U8 #x0008)

(cl:defconstant AUDIO-S8 #x8008)

(cl:defconstant AUDIO-U16LSB #x0010)

(cl:defconstant AUDIO-S16LSB #x8010)

(cl:defconstant AUDIO-U16MSB #x1010)

(cl:defconstant AUDIO-S16MSB #x9010)

(cl:defconstant AUDIO-U16 #x0010)

(cl:defconstant AUDIO-S16 #x8010)

(cffi:defcstruct Sdl-Audio-Cvt
	(needed :int)
	(src-format :unsigned-short)
	(dst-format :unsigned-short)
	(rate-incr :double)
	(buf :pointer)
	(len :int)
	(len-cvt :int)
	(len-mult :int)
	(len-ratio :double)
	(filters :pointer :count 10)
	(filter-index :int))

(cffi:defcfun ("SDL_AudioInit" SDL-Audio-Init) :int
  (driver-name :string))

(cffi:defcfun ("SDL_AudioQuit" SDL-Audio-Quit) :void)

(cffi:defcfun ("SDL_AudioDriverName" SDL-Audio-Driver-Name) :string
  (namebuf :string)
  (maxlen :int))

(cffi:defcfun ("SDL_OpenAudio" SDL-Open-Audio) :int
  (desired :pointer)
  (obtained :pointer))

(cffi:defcenum SDL-audio-status
	(:SDL-AUDIO-STOPPED 0)
	:SDL-AUDIO-PLAYING
	:SDL-AUDIO-PAUSED)

(cffi:defcfun ("SDL_GetAudioStatus" SDL-Get-Audio-Status) SDL-audio-status)

(cffi:defcfun ("SDL_PauseAudio" SDL-Pause-Audio) :void
  (pause-on :int))

(cffi:defcfun ("SDL_LoadWAV_RW" SDL-Load-WAV-RW) :pointer
  (src :pointer)
  (freesrc :int)
  (spec :pointer)
  (audio-buf :pointer)
  (audio-len :pointer))

(cffi:defcfun ("SDL_FreeWAV" SDL-Free-WAV) :void
  (audio-buf :pointer))

(cffi:defcfun ("SDL_BuildAudioCVT" SDL-Build-Audio-CVT) :int
  (cvt :pointer)
  (src-format :unsigned-short)
  (src-channels :unsigned-char)
  (src-rate :int)
  (dst-format :unsigned-short)
  (dst-channels :unsigned-char)
  (dst-rate :int))

(cffi:defcfun ("SDL_ConvertAudio" SDL-Convert-Audio) :int
  (cvt :pointer))

(cl:defconstant SDL-MIX-MAXVOLUME 128)

(cffi:defcfun ("SDL_MixAudio" SDL-Mix-Audio) :void
  (dst :pointer)
  (src :pointer)
  (len :unsigned-int)
  (volume :int))

(cffi:defcfun ("SDL_LockAudio" SDL-Lock-Audio) :void)

(cffi:defcfun ("SDL_UnlockAudio" SDL-Unlock-Audio) :void)

(cffi:defcfun ("SDL_CloseAudio" SDL-Close-Audio) :void)


(defun SDL-Load-WAV (file spec audio-buf audio-len)
  (SDL-Load-WAV-RW (SDL-RW-FROM-FILE file "rb")
		  1
		  spec
		  audio-buf
		  audio-len))

;;;; Must define the CPU byte order.
#-(or little-endian PC386 X86 I386) (defconstant AUDIO-U16SYS AUDIO-U16MSB) ;; Big Endian
#-(or little-endian PC386 X86 I386) (defconstant AUDIO-S16SYS AUDIO-S16MSB) ;; Big Endian
#+(or little-endian PC386 X86 I386) (defconstant AUDIO-U16SYS AUDIO-U16LSB) ;; Little Endian
#+(or little-endian PC386 X86 I386) (defconstant AUDIO-S16SYS AUDIO-S16LSB) ;; Little Endian
