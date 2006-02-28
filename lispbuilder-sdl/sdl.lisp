
;;;; SDL CFFI lisp wrapper
;;;; Part of the CL-Gardeners project
;;;; http://wiki.alu.org/Application_Builder
;;;; (C)2006 Justin Heyes-Jones, Frank Buss, Luke Crook, Jeremy Smith
;;;; See COPYING for license

;;;; NOTES
;;;; SDL_endian.h contains a bunch of inline stuff which is not callable
;;;; and so I omit that header
;;;; I added some #ifdef SWIG directives to omit troublesome code
;;;; 

(in-package :lispbuilder-sdl)

;;;; the SDL_Event is a union which doesn't work yet with swig
(defcstruct SDL_Event
	(type :unsigned-char)	
	(pada :int)	
	(padb :int)	
	(padc :int)	
	(padd :int)	
	(pade :int)	
	(padf :int)	
	(padg :int))

;;;; Macro to handle defenum (thanks to Frank Buss for this SWIG/CFFI feature
; this handles anonymous enum types differently

(defmacro defenum (&body enums)	
 `(progn ,@(loop for value in enums
                 for index = 0 then (1+ index)
                 when (listp value) do (setf index (second value)
                                             value (first value))
                 collect `(defconstant ,value ,index))))


;;;; 02/17/06 LJC: CFFI does not currently support bitfields.
;;;; Redefined SDL_VideoInfo so that it is the same size as defined in
;;;; "SDL_video.h"

(defcstruct SDL_VideoInfo
       (int1 :uint8)
       (int2 :uint8)
       (int3 :uint16)
       (video_mem :unsigned-int)
       (vfmt :pointer))




(defcenum SDL_bool
	(:SDL_FALSE 0)
	(:SDL_TRUE 1))

(defcstruct Uint64
	(hi :unsigned-int)
	(lo :unsigned-int))

(defcenum SDL_DUMMY_ENUM
	:DUMMY_ENUM_VALUE)

(defconstant SDL_INIT_TIMER #x00000001)

(defconstant SDL_INIT_AUDIO #x00000010)

(defconstant SDL_INIT_VIDEO #x00000020)

(defconstant SDL_INIT_CDROM #x00000100)

(defconstant SDL_INIT_JOYSTICK #x00000200)

(defconstant SDL_INIT_NOPARACHUTE #x00100000)

(defconstant SDL_INIT_EVENTTHREAD #x01000000)

(defconstant SDL_INIT_EVERYTHING #x0000FFFF)

(defcfun ("SDL_Init" SDL_Init) :int
  (flags :unsigned-int))

(defcfun ("SDL_InitSubSystem" SDL_InitSubSystem) :int
  (flags :unsigned-int))

(defcfun ("SDL_QuitSubSystem" SDL_QuitSubSystem) :void
  (flags :unsigned-int))

(defcfun ("SDL_WasInit" SDL_WasInit) :unsigned-int
  (flags :unsigned-int))

(defcfun ("SDL_Quit" SDL_Quit) :void)

(defconstant SDL_ALPHA_OPAQUE 255)

(defconstant SDL_ALPHA_TRANSPARENT 0)

(defcstruct SDL_Rect
	(x :short)
	(y :short)
	(w :unsigned-short)
	(h :unsigned-short))

(defcstruct SDL_Color
	(r :unsigned-char)
	(g :unsigned-char)
	(b :unsigned-char)
	(unused :unsigned-char))

(defcstruct SDL_Palette
	(ncolors :int)
	(colors :pointer))

(defcstruct SDL_PixelFormat
	(palette :pointer)
	(BitsPerPixel :unsigned-char)
	(BytesPerPixel :unsigned-char)
	(Rloss :unsigned-char)
	(Gloss :unsigned-char)
	(Bloss :unsigned-char)
	(Aloss :unsigned-char)
	(Rshift :unsigned-char)
	(Gshift :unsigned-char)
	(Bshift :unsigned-char)
	(Ashift :unsigned-char)
	(Rmask :unsigned-int)
	(Gmask :unsigned-int)
	(Bmask :unsigned-int)
	(Amask :unsigned-int)
	(colorkey :unsigned-int)
	(alpha :unsigned-char))

(defcstruct SDL_Surface
	(flags :unsigned-int)
	(format :pointer)
	(w :int)
	(h :int)
	(pitch :unsigned-short)
	(pixels :pointer)
	(offset :int)
	(hwdata :pointer)
	(clip_rect :pointer)
	(unused1 :unsigned-int)
	(locked :unsigned-int)
	(map :pointer)
	(format_version :unsigned-int)
	(refcount :int))

(defconstant SDL_SWSURFACE #x00000000)

(defconstant SDL_HWSURFACE #x00000001)

(defconstant SDL_ASYNCBLIT #x00000004)

(defconstant SDL_ANYFORMAT #x10000000)

(defconstant SDL_HWPALETTE #x20000000)

(defconstant SDL_DOUBLEBUF #x40000000)

(defconstant SDL_FULLSCREEN #x80000000)

(defconstant SDL_OPENGL #x00000002)

(defconstant SDL_OPENGLBLIT #x0000000A)

(defconstant SDL_RESIZABLE #x00000010)

(defconstant SDL_NOFRAME #x00000020)

(defconstant SDL_HWACCEL #x00000100)

(defconstant SDL_SRCCOLORKEY #x00001000)

(defconstant SDL_RLEACCELOK #x00002000)

(defconstant SDL_RLEACCEL #x00004000)

(defconstant SDL_SRCALPHA #x00010000)

(defconstant SDL_PREALLOC #x01000000)

(defcstruct SDL_VideoInfo
	(hw_available :unsigned-int)
	(wm_available :unsigned-int)
	(UnusedBits1 :unsigned-int)
	(UnusedBits2 :unsigned-int)
	(blit_hw :unsigned-int)
	(blit_hw_CC :unsigned-int)
	(blit_hw_A :unsigned-int)
	(blit_sw :unsigned-int)
	(blit_sw_CC :unsigned-int)
	(blit_sw_A :unsigned-int)
	(blit_fill :unsigned-int)
	(UnusedBits3 :unsigned-int)
	(video_mem :unsigned-int)
	(vfmt :pointer))

(defconstant SDL_YV12_OVERLAY #x32315659)

(defconstant SDL_IYUV_OVERLAY #x56555949)

(defconstant SDL_YUY2_OVERLAY #x32595559)

(defconstant SDL_UYVY_OVERLAY #x59565955)

(defconstant SDL_YVYU_OVERLAY #x55595659)

(defcstruct SDL_Overlay
	(format :unsigned-int)
	(w :int)
	(h :int)
	(planes :int)
	(pitches :pointer)
	(pixels :pointer)
	(hwfuncs :pointer)
	(hwdata :pointer)
	(hw_overlay :unsigned-int)
	(UnusedBits :unsigned-int))

(defcenum SDL_GLattr
	:SDL_GL_RED_SIZE
	:SDL_GL_GREEN_SIZE
	:SDL_GL_BLUE_SIZE
	:SDL_GL_ALPHA_SIZE
	:SDL_GL_BUFFER_SIZE
	:SDL_GL_DOUBLEBUFFER
	:SDL_GL_DEPTH_SIZE
	:SDL_GL_STENCIL_SIZE
	:SDL_GL_ACCUM_RED_SIZE
	:SDL_GL_ACCUM_GREEN_SIZE
	:SDL_GL_ACCUM_BLUE_SIZE
	:SDL_GL_ACCUM_ALPHA_SIZE
	:SDL_GL_STEREO
	:SDL_GL_MULTISAMPLEBUFFERS
	:SDL_GL_MULTISAMPLESAMPLES)

(defconstant SDL_LOGPAL #x01)

(defconstant SDL_PHYSPAL #x02)

(defcfun ("SDL_VideoInit" SDL_VideoInit) :int
  (driver_name :string)
  (flags :unsigned-int))

(defcfun ("SDL_VideoQuit" SDL_VideoQuit) :void)

(defcfun ("SDL_VideoDriverName" SDL_VideoDriverName) :string
  (namebuf :string)
  (maxlen :int))

(defcfun ("SDL_GetVideoSurface" SDL_GetVideoSurface) :pointer)

(defcfun ("SDL_GetVideoInfo" SDL_GetVideoInfo) :pointer)

(defcfun ("SDL_VideoModeOK" SDL_VideoModeOK) :int
  (width :int)
  (height :int)
  (bpp :int)
  (flags :unsigned-int))

(defcfun ("SDL_ListModes" SDL_ListModes) :pointer
  (format :pointer)
  (flags :unsigned-int))

(defcfun ("SDL_SetVideoMode" SDL_SetVideoMode) :pointer
  (width :int)
  (height :int)
  (bpp :int)
  (flags :unsigned-int))

(defcfun ("SDL_UpdateRects" SDL_UpdateRects) :void
  (screen :pointer)
  (numrects :int)
  (rects :pointer))

(defcfun ("SDL_UpdateRect" SDL_UpdateRect) :void
  (screen :pointer)
  (x :int)
  (y :int)
  (w :unsigned-int)
  (h :unsigned-int))

(defcfun ("SDL_Flip" SDL_Flip) :int
  (screen :pointer))

(defcfun ("SDL_SetGamma" SDL_SetGamma) :int
  (red :float)
  (green :float)
  (blue :float))

(defcfun ("SDL_SetGammaRamp" SDL_SetGammaRamp) :int
  (red :pointer)
  (green :pointer)
  (blue :pointer))

(defcfun ("SDL_GetGammaRamp" SDL_GetGammaRamp) :int
  (red :pointer)
  (green :pointer)
  (blue :pointer))

(defcfun ("SDL_SetColors" SDL_SetColors) :int
  (surface :pointer)
  (colors :pointer)
  (firstcolor :int)
  (ncolors :int))

(defcfun ("SDL_SetPalette" SDL_SetPalette) :int
  (surface :pointer)
  (flags :int)
  (colors :pointer)
  (firstcolor :int)
  (ncolors :int))

(defcfun ("SDL_MapRGB" SDL_MapRGB) :unsigned-int
  (format :pointer)
  (r :unsigned-char)
  (g :unsigned-char)
  (b :unsigned-char))

(defcfun ("SDL_MapRGBA" SDL_MapRGBA) :unsigned-int
  (format :pointer)
  (r :unsigned-char)
  (g :unsigned-char)
  (b :unsigned-char)
  (a :unsigned-char))

(defcfun ("SDL_GetRGB" SDL_GetRGB) :void
  (pixel :unsigned-int)
  (fmt :pointer)
  (r :pointer)
  (g :pointer)
  (b :pointer))

(defcfun ("SDL_GetRGBA" SDL_GetRGBA) :void
  (pixel :unsigned-int)
  (fmt :pointer)
  (r :pointer)
  (g :pointer)
  (b :pointer)
  (a :pointer))

(defcfun ("SDL_CreateRGBSurface" SDL_CreateRGBSurface) :pointer
  (flags :unsigned-int)
  (width :int)
  (height :int)
  (depth :int)
  (Rmask :unsigned-int)
  (Gmask :unsigned-int)
  (Bmask :unsigned-int)
  (Amask :unsigned-int))

(defcfun ("SDL_CreateRGBSurfaceFrom" SDL_CreateRGBSurfaceFrom) :pointer
  (pixels :pointer)
  (width :int)
  (height :int)
  (depth :int)
  (pitch :int)
  (Rmask :unsigned-int)
  (Gmask :unsigned-int)
  (Bmask :unsigned-int)
  (Amask :unsigned-int))

(defcfun ("SDL_FreeSurface" SDL_FreeSurface) :void
  (surface :pointer))

(defcfun ("SDL_LockSurface" SDL_LockSurface) :int
  (surface :pointer))

(defcfun ("SDL_UnlockSurface" SDL_UnlockSurface) :void
  (surface :pointer))

(defcfun ("SDL_LoadBMP_RW" SDL_LoadBMP_RW) :pointer
  (src :pointer)
  (freesrc :int))

(defcfun ("SDL_SaveBMP_RW" SDL_SaveBMP_RW) :int
  (surface :pointer)
  (dst :pointer)
  (freedst :int))

(defcfun ("SDL_SetColorKey" SDL_SetColorKey) :int
  (surface :pointer)
  (flag :unsigned-int)
  (key :unsigned-int))

(defcfun ("SDL_SetAlpha" SDL_SetAlpha) :int
  (surface :pointer)
  (flag :unsigned-int)
  (alpha :unsigned-char))

(defcfun ("SDL_SetClipRect" SDL_SetClipRect) :pointer
  (surface :pointer)
  (rect :pointer))

(defcfun ("SDL_GetClipRect" SDL_GetClipRect) :void
  (surface :pointer)
  (rect :pointer))

(defcfun ("SDL_ConvertSurface" SDL_ConvertSurface) :pointer
  (src :pointer)
  (fmt :pointer)
  (flags :unsigned-int))

(defcfun ("SDL_UpperBlit" SDL_UpperBlit) :int
  (src :pointer)
  (srcrect :pointer)
  (dst :pointer)
  (dstrect :pointer))

(defcfun ("SDL_LowerBlit" SDL_LowerBlit) :int
  (src :pointer)
  (srcrect :pointer)
  (dst :pointer)
  (dstrect :pointer))

(defcfun ("SDL_FillRect" SDL_FillRect) :int
  (dst :pointer)
  (dstrect :pointer)
  (color :unsigned-int))

(defcfun ("SDL_DisplayFormat" SDL_DisplayFormat) :pointer
  (surface :pointer))

(defcfun ("SDL_DisplayFormatAlpha" SDL_DisplayFormatAlpha) :pointer
  (surface :pointer))

(defcfun ("SDL_CreateYUVOverlay" SDL_CreateYUVOverlay) :pointer
  (width :int)
  (height :int)
  (format :unsigned-int)
  (display :pointer))

(defcfun ("SDL_LockYUVOverlay" SDL_LockYUVOverlay) :int
  (overlay :pointer))

(defcfun ("SDL_UnlockYUVOverlay" SDL_UnlockYUVOverlay) :void
  (overlay :pointer))

(defcfun ("SDL_DisplayYUVOverlay" SDL_DisplayYUVOverlay) :int
  (overlay :pointer)
  (dstrect :pointer))

(defcfun ("SDL_FreeYUVOverlay" SDL_FreeYUVOverlay) :void
  (overlay :pointer))

(defcfun ("SDL_GL_LoadLibrary" SDL_GL_LoadLibrary) :int
  (path :string))

(defcfun ("SDL_GL_GetProcAddress" SDL_GL_GetProcAddress) :pointer
  (proc :string))

(defcfun ("SDL_GL_SetAttribute" SDL_GL_SetAttribute) :int
  (attr :pointer)
  (value :int))

(defcfun ("SDL_GL_GetAttribute" SDL_GL_GetAttribute) :int
  (attr :pointer)
  (value :pointer))

(defcfun ("SDL_GL_SwapBuffers" SDL_GL_SwapBuffers) :void)

(defcfun ("SDL_GL_UpdateRects" SDL_GL_UpdateRects) :void
  (numrects :int)
  (rects :pointer))

(defcfun ("SDL_GL_Lock" SDL_GL_Lock) :void)

(defcfun ("SDL_GL_Unlock" SDL_GL_Unlock) :void)

(defcfun ("SDL_WM_SetCaption" SDL_WM_SetCaption) :void
  (title :string)
  (icon :string))

(defcfun ("SDL_WM_GetCaption" SDL_WM_GetCaption) :void
  (title :pointer)
  (icon :pointer))

(defcfun ("SDL_WM_SetIcon" SDL_WM_SetIcon) :void
  (icon :pointer)
  (mask :pointer))

(defcfun ("SDL_WM_IconifyWindow" SDL_WM_IconifyWindow) :int)

(defcfun ("SDL_WM_ToggleFullScreen" SDL_WM_ToggleFullScreen) :int
  (surface :pointer))

(defcenum SDL_GrabMode
	(:SDL_GRAB_QUERY -1)
	(:SDL_GRAB_OFF 0)
	(:SDL_GRAB_ON 1)
	:SDL_GRAB_FULLSCREEN)

(defcfun ("SDL_WM_GrabInput" SDL_WM_GrabInput) :pointer
  (mode :pointer))

(defcfun ("SDL_SoftStretch" SDL_SoftStretch) :int
  (src :pointer)
  (srcrect :pointer)
  (dst :pointer)
  (dstrect :pointer))

(defenum 
	(SDL_NOEVENT 0)
	SDL_ACTIVEEVENT
	SDL_KEYDOWN
	SDL_KEYUP
	SDL_MOUSEMOTION
	SDL_MOUSEBUTTONDOWN
	SDL_MOUSEBUTTONUP
	SDL_JOYAXISMOTION
	SDL_JOYBALLMOTION
	SDL_JOYHATMOTION
	SDL_JOYBUTTONDOWN
	SDL_JOYBUTTONUP
	SDL_QUIT
	SDL_SYSWMEVENT
	SDL_EVENT_RESERVEDA
	SDL_EVENT_RESERVEDB
	SDL_VIDEORESIZE
	SDL_VIDEOEXPOSE
	SDL_EVENT_RESERVED2
	SDL_EVENT_RESERVED3
	SDL_EVENT_RESERVED4
	SDL_EVENT_RESERVED5
	SDL_EVENT_RESERVED6
	SDL_EVENT_RESERVED7
	(SDL_USEREVENT 24)
	(SDL_NUMEVENTS 32))

(defcstruct SDL_ActiveEvent
	(type :unsigned-char)
	(gain :unsigned-char)
	(state :unsigned-char))

(defcstruct SDL_KeyboardEvent
	(type :unsigned-char)
	(which :unsigned-char)
	(state :unsigned-char)
	(keysym :pointer))

(defcstruct SDL_MouseMotionEvent
	(type :unsigned-char)
	(which :unsigned-char)
	(state :unsigned-char)
	(x :unsigned-short)
	(y :unsigned-short)
	(xrel :short)
	(yrel :short))

(defcstruct SDL_MouseButtonEvent
	(type :unsigned-char)
	(which :unsigned-char)
	(button :unsigned-char)
	(state :unsigned-char)
	(x :unsigned-short)
	(y :unsigned-short))

(defcstruct SDL_JoyAxisEvent
	(type :unsigned-char)
	(which :unsigned-char)
	(axis :unsigned-char)
	(value :short))

(defcstruct SDL_JoyBallEvent
	(type :unsigned-char)
	(which :unsigned-char)
	(ball :unsigned-char)
	(xrel :short)
	(yrel :short))

(defcstruct SDL_JoyHatEvent
	(type :unsigned-char)
	(which :unsigned-char)
	(hat :unsigned-char)
	(value :unsigned-char))

(defcstruct SDL_JoyButtonEvent
	(type :unsigned-char)
	(which :unsigned-char)
	(button :unsigned-char)
	(state :unsigned-char))

(defcstruct SDL_ResizeEvent
	(type :unsigned-char)
	(w :int)
	(h :int))

(defcstruct SDL_ExposeEvent
	(type :unsigned-char))

(defcstruct SDL_QuitEvent
	(type :unsigned-char))

(defcstruct SDL_UserEvent
	(type :unsigned-char)
	(code :int)
	(data1 :pointer)
	(data2 :pointer))

(defcstruct SDL_SysWMEvent
	(type :unsigned-char)
	(msg :pointer))

(defcfun ("SDL_PumpEvents" SDL_PumpEvents) :void)

(defcenum SDL_eventaction
	:SDL_ADDEVENT
	:SDL_PEEKEVENT
	:SDL_GETEVENT)

(defcfun ("SDL_PeepEvents" SDL_PeepEvents) :int
  (events :pointer)
  (numevents :int)
  (action :pointer)
  (mask :unsigned-int))

(defcfun ("SDL_PollEvent" SDL_PollEvent) :int
  (event :pointer))

(defcfun ("SDL_WaitEvent" SDL_WaitEvent) :int
  (event :pointer))

(defcfun ("SDL_PushEvent" SDL_PushEvent) :int
  (event :pointer))

(defcfun ("SDL_SetEventFilter" SDL_SetEventFilter) :void
  (filter :pointer))

(defcfun ("SDL_GetEventFilter" SDL_GetEventFilter) :pointer)

(defconstant SDL_QUERY -1)

(defconstant SDL_IGNORE 0)

(defconstant SDL_DISABLE 0)

(defconstant SDL_ENABLE 1)

(defcfun ("SDL_EventState" SDL_EventState) :unsigned-char
  (type :unsigned-char)
  (state :int))

(defcfun ("SDL_GetError" SDL_GetError) :string)

(defcfun ("SDL_ClearError" SDL_ClearError) :void)

(defcenum SDL_errorcode
	:SDL_ENOMEM
	:SDL_EFREAD
	:SDL_EFWRITE
	:SDL_EFSEEK
	:SDL_LASTERROR)

(defcfun ("SDL_Error" SDL_Error) :void
  (code :pointer))

(defconstant SDL_APPMOUSEFOCUS #x01)

(defconstant SDL_APPINPUTFOCUS #x02)

(defconstant SDL_APPACTIVE #x04)

(defcfun ("SDL_GetAppState" SDL_GetAppState) :unsigned-char)

(defcstruct SDL_AudioSpec
	(freq :int)
	(format :unsigned-short)
	(channels :unsigned-char)
	(silence :unsigned-char)
	(samples :unsigned-short)
	(padding :unsigned-short)
	(size :unsigned-int)
	(callback :pointer)
	(userdata :pointer))

(defconstant AUDIO_U8 #x0008)

(defconstant AUDIO_S8 #x8008)

(defconstant AUDIO_U16LSB #x0010)

(defconstant AUDIO_S16LSB #x8010)

(defconstant AUDIO_U16MSB #x1010)

(defconstant AUDIO_S16MSB #x9010)

(defconstant AUDIO_U16 #x0010)

(defconstant AUDIO_S16 #x8010)

(defconstant AUDIO_U16SYS #x0010)

(defconstant AUDIO_S16SYS #x8010)

(defcstruct SDL_AudioCVT
	(needed :int)
	(src_format :unsigned-short)
	(dst_format :unsigned-short)
	(rate_incr :double)
	(buf :pointer)
	(len :int)
	(len_cvt :int)
	(len_mult :int)
	(len_ratio :double)
	(filters :pointer)
	(filter_index :int))

(defcfun ("SDL_AudioInit" SDL_AudioInit) :int
  (driver_name :string))

(defcfun ("SDL_AudioQuit" SDL_AudioQuit) :void)

(defcfun ("SDL_AudioDriverName" SDL_AudioDriverName) :string
  (namebuf :string)
  (maxlen :int))

(defcfun ("SDL_OpenAudio" SDL_OpenAudio) :int
  (desired :pointer)
  (obtained :pointer))

(defcenum SDL_audiostatus
	(:SDL_AUDIO_STOPPED 0)
	:SDL_AUDIO_PLAYING
	:SDL_AUDIO_PAUSED)

(defcfun ("SDL_GetAudioStatus" SDL_GetAudioStatus) :pointer)

(defcfun ("SDL_PauseAudio" SDL_PauseAudio) :void
  (pause_on :int))

(defcfun ("SDL_LoadWAV_RW" SDL_LoadWAV_RW) :pointer
  (src :pointer)
  (freesrc :int)
  (spec :pointer)
  (audio_buf :pointer)
  (audio_len :pointer))

(defcfun ("SDL_FreeWAV" SDL_FreeWAV) :void
  (audio_buf :pointer))

(defcfun ("SDL_BuildAudioCVT" SDL_BuildAudioCVT) :int
  (cvt :pointer)
  (src_format :unsigned-short)
  (src_channels :unsigned-char)
  (src_rate :int)
  (dst_format :unsigned-short)
  (dst_channels :unsigned-char)
  (dst_rate :int))

(defcfun ("SDL_ConvertAudio" SDL_ConvertAudio) :int
  (cvt :pointer))

(defconstant SDL_MIX_MAXVOLUME 128)

(defcfun ("SDL_MixAudio" SDL_MixAudio) :void
  (dst :pointer)
  (src :pointer)
  (len :unsigned-int)
  (volume :int))

(defcfun ("SDL_LockAudio" SDL_LockAudio) :void)

(defcfun ("SDL_UnlockAudio" SDL_UnlockAudio) :void)

(defcfun ("SDL_CloseAudio" SDL_CloseAudio) :void)

(defconstant SDL_LIL_ENDIAN 1234)

(defconstant SDL_BIG_ENDIAN 4321)

(defconstant SDL_BYTEORDER 4321)

(defconstant SDL_MAX_TRACKS 99)

(defconstant SDL_AUDIO_TRACK #x00)

(defconstant SDL_DATA_TRACK #x04)

(defcenum CDstatus
	:CD_TRAYEMPTY
	:CD_STOPPED
	:CD_PLAYING
	:CD_PAUSED
	(:CD_ERROR -1))

(defcstruct SDL_CDtrack
	(id :unsigned-char)
	(type :unsigned-char)
	(unused :unsigned-short)
	(length :unsigned-int)
	(offset :unsigned-int))

(defcstruct SDL_CD
	(id :int)
	(status :pointer)
	(numtracks :int)
	(cur_track :int)
	(cur_frame :int)
	(track :pointer))

(defconstant CD_FPS 75)

(defcfun ("SDL_CDNumDrives" SDL_CDNumDrives) :int)

(defcfun ("SDL_CDName" SDL_CDName) :string
  (drive :int))

(defcfun ("SDL_CDOpen" SDL_CDOpen) :pointer
  (drive :int))

(defcfun ("SDL_CDStatus" SDL_CDStatus) :pointer
  (cdrom :pointer))

(defcfun ("SDL_CDPlayTracks" SDL_CDPlayTracks) :int
  (cdrom :pointer)
  (start_track :int)
  (start_frame :int)
  (ntracks :int)
  (nframes :int))

(defcfun ("SDL_CDPlay" SDL_CDPlay) :int
  (cdrom :pointer)
  (start :int)
  (length :int))

(defcfun ("SDL_CDPause" SDL_CDPause) :int
  (cdrom :pointer))

(defcfun ("SDL_CDResume" SDL_CDResume) :int
  (cdrom :pointer))

(defcfun ("SDL_CDStop" SDL_CDStop) :int
  (cdrom :pointer))

(defcfun ("SDL_CDEject" SDL_CDEject) :int
  (cdrom :pointer))

(defcfun ("SDL_CDClose" SDL_CDClose) :void
  (cdrom :pointer))

(defcfun ("SDL_HasRDTSC" SDL_HasRDTSC) :pointer)

(defcfun ("SDL_HasMMX" SDL_HasMMX) :pointer)

(defcfun ("SDL_HasMMXExt" SDL_HasMMXExt) :pointer)

(defcfun ("SDL_Has3DNow" SDL_Has3DNow) :pointer)

(defcfun ("SDL_Has3DNowExt" SDL_Has3DNowExt) :pointer)

(defcfun ("SDL_HasSSE" SDL_HasSSE) :pointer)

(defcfun ("SDL_HasSSE2" SDL_HasSSE2) :pointer)

(defcfun ("SDL_HasAltiVec" SDL_HasAltiVec) :pointer)

(defcfun ("SDL_NumJoysticks" SDL_NumJoysticks) :int)

(defcfun ("SDL_JoystickName" SDL_JoystickName) :string
  (device_index :int))

(defcfun ("SDL_JoystickOpen" SDL_JoystickOpen) :pointer
  (device_index :int))

(defcfun ("SDL_JoystickOpened" SDL_JoystickOpened) :int
  (device_index :int))

(defcfun ("SDL_JoystickIndex" SDL_JoystickIndex) :int
  (joystick :pointer))

(defcfun ("SDL_JoystickNumAxes" SDL_JoystickNumAxes) :int
  (joystick :pointer))

(defcfun ("SDL_JoystickNumBalls" SDL_JoystickNumBalls) :int
  (joystick :pointer))

(defcfun ("SDL_JoystickNumHats" SDL_JoystickNumHats) :int
  (joystick :pointer))

(defcfun ("SDL_JoystickNumButtons" SDL_JoystickNumButtons) :int
  (joystick :pointer))

(defcfun ("SDL_JoystickUpdate" SDL_JoystickUpdate) :void)

(defcfun ("SDL_JoystickEventState" SDL_JoystickEventState) :int
  (state :int))

(defcfun ("SDL_JoystickGetAxis" SDL_JoystickGetAxis) :short
  (joystick :pointer)
  (axis :int))

(defconstant SDL_HAT_CENTERED #x00)

(defconstant SDL_HAT_UP #x01)

(defconstant SDL_HAT_RIGHT #x02)

(defconstant SDL_HAT_DOWN #x04)

(defconstant SDL_HAT_LEFT #x08)

(defconstant SDL_HAT_RIGHTUP #x0A)

(defconstant SDL_HAT_RIGHTDOWN #x06)

(defconstant SDL_HAT_LEFTUP #x09)

(defconstant SDL_HAT_LEFTDOWN #x0C)

(defcfun ("SDL_JoystickGetHat" SDL_JoystickGetHat) :unsigned-char
  (joystick :pointer)
  (hat :int))

(defcfun ("SDL_JoystickGetBall" SDL_JoystickGetBall) :int
  (joystick :pointer)
  (ball :int)
  (dx :pointer)
  (dy :pointer))

(defcfun ("SDL_JoystickGetButton" SDL_JoystickGetButton) :unsigned-char
  (joystick :pointer)
  (button :int))

(defcfun ("SDL_JoystickClose" SDL_JoystickClose) :void
  (joystick :pointer))

(defcstruct SDL_keysym
	(scancode :unsigned-char)
	(sym :pointer)
	(mod :pointer)
	(unicode :unsigned-short))

(defconstant SDL_ALL_HOTKEYS #xFFFFFFFF)

(defcfun ("SDL_EnableUNICODE" SDL_EnableUNICODE) :int
  (enable :int))

(defconstant SDL_DEFAULT_REPEAT_DELAY 500)

(defconstant SDL_DEFAULT_REPEAT_INTERVAL 30)

(defcfun ("SDL_EnableKeyRepeat" SDL_EnableKeyRepeat) :int
  (delay :int)
  (interval :int))

(defcfun ("SDL_GetKeyState" SDL_GetKeyState) :pointer
  (numkeys :pointer))

(defcfun ("SDL_GetModState" SDL_GetModState) :pointer)

(defcfun ("SDL_SetModState" SDL_SetModState) :void
  (modstate :pointer))

(defcfun ("SDL_GetKeyName" SDL_GetKeyName) :string
  (key :pointer))

(defcenum SDLKey
	(:SDLK_UNKNOWN 0)
	(:SDLK_FIRST 1)
	(:SDLK_BACKSPACE 8)
	(:SDLK_TAB 9)
	(:SDLK_CLEAR 12)
	(:SDLK_RETURN 13)
	(:SDLK_PAUSE 19)
	(:SDLK_ESCAPE 27)
	(:SDLK_SPACE 32)
	(:SDLK_EXCLAIM 33)
	(:SDLK_QUOTEDBL 34)
	(:SDLK_HASH 35)
	(:SDLK_DOLLAR 36)
	(:SDLK_AMPERSAND 38)
	(:SDLK_QUOTE 39)
	(:SDLK_LEFTPAREN 40)
	(:SDLK_RIGHTPAREN 41)
	(:SDLK_ASTERISK 42)
	(:SDLK_PLUS 43)
	(:SDLK_COMMA 44)
	(:SDLK_MINUS 45)
	(:SDLK_PERIOD 46)
	(:SDLK_SLASH 47)
	(:SDLK_0 48)
	(:SDLK_1 49)
	(:SDLK_2 50)
	(:SDLK_3 51)
	(:SDLK_4 52)
	(:SDLK_5 53)
	(:SDLK_6 54)
	(:SDLK_7 55)
	(:SDLK_8 56)
	(:SDLK_9 57)
	(:SDLK_COLON 58)
	(:SDLK_SEMICOLON 59)
	(:SDLK_LESS 60)
	(:SDLK_EQUALS 61)
	(:SDLK_GREATER 62)
	(:SDLK_QUESTION 63)
	(:SDLK_AT 64)
	(:SDLK_LEFTBRACKET 91)
	(:SDLK_BACKSLASH 92)
	(:SDLK_RIGHTBRACKET 93)
	(:SDLK_CARET 94)
	(:SDLK_UNDERSCORE 95)
	(:SDLK_BACKQUOTE 96)
	(:SDLK_a 97)
	(:SDLK_b 98)
	(:SDLK_c 99)
	(:SDLK_d 100)
	(:SDLK_e 101)
	(:SDLK_f 102)
	(:SDLK_g 103)
	(:SDLK_h 104)
	(:SDLK_i 105)
	(:SDLK_j 106)
	(:SDLK_k 107)
	(:SDLK_l 108)
	(:SDLK_m 109)
	(:SDLK_n 110)
	(:SDLK_o 111)
	(:SDLK_p 112)
	(:SDLK_q 113)
	(:SDLK_r 114)
	(:SDLK_s 115)
	(:SDLK_t 116)
	(:SDLK_u 117)
	(:SDLK_v 118)
	(:SDLK_w 119)
	(:SDLK_x 120)
	(:SDLK_y 121)
	(:SDLK_z 122)
	(:SDLK_DELETE 127)
	(:SDLK_WORLD_0 160)
	(:SDLK_WORLD_1 161)
	(:SDLK_WORLD_2 162)
	(:SDLK_WORLD_3 163)
	(:SDLK_WORLD_4 164)
	(:SDLK_WORLD_5 165)
	(:SDLK_WORLD_6 166)
	(:SDLK_WORLD_7 167)
	(:SDLK_WORLD_8 168)
	(:SDLK_WORLD_9 169)
	(:SDLK_WORLD_10 170)
	(:SDLK_WORLD_11 171)
	(:SDLK_WORLD_12 172)
	(:SDLK_WORLD_13 173)
	(:SDLK_WORLD_14 174)
	(:SDLK_WORLD_15 175)
	(:SDLK_WORLD_16 176)
	(:SDLK_WORLD_17 177)
	(:SDLK_WORLD_18 178)
	(:SDLK_WORLD_19 179)
	(:SDLK_WORLD_20 180)
	(:SDLK_WORLD_21 181)
	(:SDLK_WORLD_22 182)
	(:SDLK_WORLD_23 183)
	(:SDLK_WORLD_24 184)
	(:SDLK_WORLD_25 185)
	(:SDLK_WORLD_26 186)
	(:SDLK_WORLD_27 187)
	(:SDLK_WORLD_28 188)
	(:SDLK_WORLD_29 189)
	(:SDLK_WORLD_30 190)
	(:SDLK_WORLD_31 191)
	(:SDLK_WORLD_32 192)
	(:SDLK_WORLD_33 193)
	(:SDLK_WORLD_34 194)
	(:SDLK_WORLD_35 195)
	(:SDLK_WORLD_36 196)
	(:SDLK_WORLD_37 197)
	(:SDLK_WORLD_38 198)
	(:SDLK_WORLD_39 199)
	(:SDLK_WORLD_40 200)
	(:SDLK_WORLD_41 201)
	(:SDLK_WORLD_42 202)
	(:SDLK_WORLD_43 203)
	(:SDLK_WORLD_44 204)
	(:SDLK_WORLD_45 205)
	(:SDLK_WORLD_46 206)
	(:SDLK_WORLD_47 207)
	(:SDLK_WORLD_48 208)
	(:SDLK_WORLD_49 209)
	(:SDLK_WORLD_50 210)
	(:SDLK_WORLD_51 211)
	(:SDLK_WORLD_52 212)
	(:SDLK_WORLD_53 213)
	(:SDLK_WORLD_54 214)
	(:SDLK_WORLD_55 215)
	(:SDLK_WORLD_56 216)
	(:SDLK_WORLD_57 217)
	(:SDLK_WORLD_58 218)
	(:SDLK_WORLD_59 219)
	(:SDLK_WORLD_60 220)
	(:SDLK_WORLD_61 221)
	(:SDLK_WORLD_62 222)
	(:SDLK_WORLD_63 223)
	(:SDLK_WORLD_64 224)
	(:SDLK_WORLD_65 225)
	(:SDLK_WORLD_66 226)
	(:SDLK_WORLD_67 227)
	(:SDLK_WORLD_68 228)
	(:SDLK_WORLD_69 229)
	(:SDLK_WORLD_70 230)
	(:SDLK_WORLD_71 231)
	(:SDLK_WORLD_72 232)
	(:SDLK_WORLD_73 233)
	(:SDLK_WORLD_74 234)
	(:SDLK_WORLD_75 235)
	(:SDLK_WORLD_76 236)
	(:SDLK_WORLD_77 237)
	(:SDLK_WORLD_78 238)
	(:SDLK_WORLD_79 239)
	(:SDLK_WORLD_80 240)
	(:SDLK_WORLD_81 241)
	(:SDLK_WORLD_82 242)
	(:SDLK_WORLD_83 243)
	(:SDLK_WORLD_84 244)
	(:SDLK_WORLD_85 245)
	(:SDLK_WORLD_86 246)
	(:SDLK_WORLD_87 247)
	(:SDLK_WORLD_88 248)
	(:SDLK_WORLD_89 249)
	(:SDLK_WORLD_90 250)
	(:SDLK_WORLD_91 251)
	(:SDLK_WORLD_92 252)
	(:SDLK_WORLD_93 253)
	(:SDLK_WORLD_94 254)
	(:SDLK_WORLD_95 255)
	(:SDLK_KP0 256)
	(:SDLK_KP1 257)
	(:SDLK_KP2 258)
	(:SDLK_KP3 259)
	(:SDLK_KP4 260)
	(:SDLK_KP5 261)
	(:SDLK_KP6 262)
	(:SDLK_KP7 263)
	(:SDLK_KP8 264)
	(:SDLK_KP9 265)
	(:SDLK_KP_PERIOD 266)
	(:SDLK_KP_DIVIDE 267)
	(:SDLK_KP_MULTIPLY 268)
	(:SDLK_KP_MINUS 269)
	(:SDLK_KP_PLUS 270)
	(:SDLK_KP_ENTER 271)
	(:SDLK_KP_EQUALS 272)
	(:SDLK_UP 273)
	(:SDLK_DOWN 274)
	(:SDLK_RIGHT 275)
	(:SDLK_LEFT 276)
	(:SDLK_INSERT 277)
	(:SDLK_HOME 278)
	(:SDLK_END 279)
	(:SDLK_PAGEUP 280)
	(:SDLK_PAGEDOWN 281)
	(:SDLK_F1 282)
	(:SDLK_F2 283)
	(:SDLK_F3 284)
	(:SDLK_F4 285)
	(:SDLK_F5 286)
	(:SDLK_F6 287)
	(:SDLK_F7 288)
	(:SDLK_F8 289)
	(:SDLK_F9 290)
	(:SDLK_F10 291)
	(:SDLK_F11 292)
	(:SDLK_F12 293)
	(:SDLK_F13 294)
	(:SDLK_F14 295)
	(:SDLK_F15 296)
	(:SDLK_NUMLOCK 300)
	(:SDLK_CAPSLOCK 301)
	(:SDLK_SCROLLOCK 302)
	(:SDLK_RSHIFT 303)
	(:SDLK_LSHIFT 304)
	(:SDLK_RCTRL 305)
	(:SDLK_LCTRL 306)
	(:SDLK_RALT 307)
	(:SDLK_LALT 308)
	(:SDLK_RMETA 309)
	(:SDLK_LMETA 310)
	(:SDLK_LSUPER 311)
	(:SDLK_RSUPER 312)
	(:SDLK_MODE 313)
	(:SDLK_COMPOSE 314)
	(:SDLK_HELP 315)
	(:SDLK_PRINT 316)
	(:SDLK_SYSREQ 317)
	(:SDLK_BREAK 318)
	(:SDLK_MENU 319)
	(:SDLK_POWER 320)
	(:SDLK_EURO 321)
	(:SDLK_UNDO 322)
	:SDLK_LAST)

(defcenum SDLMod
	(:KMOD_NONE 0)
	(:KMOD_LSHIFT 1)
	(:KMOD_RSHIFT 2)
	(:KMOD_LCTRL 64)
	(:KMOD_RCTRL 128)
	(:KMOD_LALT 256)
	(:KMOD_RALT 512)
	(:KMOD_LMETA 1024)
	(:KMOD_RMETA 2048)
	(:KMOD_NUM 4096)
	(:KMOD_CAPS 8192)
	(:KMOD_MODE 16384)
	(:KMOD_RESERVED 32768))

(defconstant KMOD_CTRL 192)

(defconstant KMOD_SHIFT 3)

(defconstant KMOD_ALT 768)

(defconstant KMOD_META 3072)

(defcfun ("SDL_LoadObject" SDL_LoadObject) :pointer
  (sofile :string))

(defcfun ("SDL_LoadFunction" SDL_LoadFunction) :pointer
  (handle :pointer)
  (name :string))

(defcfun ("SDL_UnloadObject" SDL_UnloadObject) :void
  (handle :pointer))

(defcstruct SDL_Cursor
	(area :pointer)
	(hot_x :short)
	(hot_y :short)
	(data :pointer)
	(mask :pointer)
	(save :pointer)
	(wm_cursor :pointer))

(defcfun ("SDL_GetMouseState" SDL_GetMouseState) :unsigned-char
  (x :pointer)
  (y :pointer))

(defcfun ("SDL_GetRelativeMouseState" SDL_GetRelativeMouseState) :unsigned-char
  (x :pointer)
  (y :pointer))

(defcfun ("SDL_WarpMouse" SDL_WarpMouse) :void
  (x :unsigned-short)
  (y :unsigned-short))

(defcfun ("SDL_CreateCursor" SDL_CreateCursor) :pointer
  (data :pointer)
  (mask :pointer)
  (w :int)
  (h :int)
  (hot_x :int)
  (hot_y :int))

(defcfun ("SDL_SetCursor" SDL_SetCursor) :void
  (cursor :pointer))

(defcfun ("SDL_GetCursor" SDL_GetCursor) :pointer)

(defcfun ("SDL_FreeCursor" SDL_FreeCursor) :void
  (cursor :pointer))

(defcfun ("SDL_ShowCursor" SDL_ShowCursor) :int
  (toggle :int))

(defconstant SDL_BUTTON_LEFT 1)

(defconstant SDL_BUTTON_MIDDLE 2)

(defconstant SDL_BUTTON_RIGHT 3)

(defconstant SDL_BUTTON_WHEELUP 4)

(defconstant SDL_BUTTON_WHEELDOWN 5)

(defconstant SDL_MUTEX_TIMEDOUT 1)

(defcfun ("SDL_CreateMutex" SDL_CreateMutex) :pointer)

(defcfun ("SDL_mutexP" SDL_mutexP) :int
  (mutex :pointer))

(defcfun ("SDL_mutexV" SDL_mutexV) :int
  (mutex :pointer))

(defcfun ("SDL_DestroyMutex" SDL_DestroyMutex) :void
  (mutex :pointer))

(defcfun ("SDL_CreateSemaphore" SDL_CreateSemaphore) :pointer
  (initial_value :unsigned-int))

(defcfun ("SDL_DestroySemaphore" SDL_DestroySemaphore) :void
  (sem :pointer))

(defcfun ("SDL_SemWait" SDL_SemWait) :int
  (sem :pointer))

(defcfun ("SDL_SemTryWait" SDL_SemTryWait) :int
  (sem :pointer))

(defcfun ("SDL_SemWaitTimeout" SDL_SemWaitTimeout) :int
  (sem :pointer)
  (ms :unsigned-int))

(defcfun ("SDL_SemPost" SDL_SemPost) :int
  (sem :pointer))

(defcfun ("SDL_SemValue" SDL_SemValue) :unsigned-int
  (sem :pointer))

(defcfun ("SDL_CreateCond" SDL_CreateCond) :pointer)

(defcfun ("SDL_DestroyCond" SDL_DestroyCond) :void
  (cond :pointer))

(defcfun ("SDL_CondSignal" SDL_CondSignal) :int
  (cond :pointer))

(defcfun ("SDL_CondBroadcast" SDL_CondBroadcast) :int
  (cond :pointer))

(defcfun ("SDL_CondWait" SDL_CondWait) :int
  (cond :pointer)
  (mut :pointer))

(defcfun ("SDL_CondWaitTimeout" SDL_CondWaitTimeout) :int
  (cond :pointer)
  (mutex :pointer)
  (ms :unsigned-int))

(defconstant NeedFunctionPrototypes 1)

(defconstant GL_GLEXT_VERSION 21)

(defconstant GL_UNSIGNED_BYTE_3_3_2 #x8032)

(defconstant GL_UNSIGNED_SHORT_4_4_4_4 #x8033)

(defconstant GL_UNSIGNED_SHORT_5_5_5_1 #x8034)

(defconstant GL_UNSIGNED_INT_8_8_8_8 #x8035)

(defconstant GL_UNSIGNED_INT_10_10_10_2 #x8036)

(defconstant GL_RESCALE_NORMAL #x803A)

(defconstant GL_TEXTURE_BINDING_3D #x806A)

(defconstant GL_PACK_SKIP_IMAGES #x806B)

(defconstant GL_PACK_IMAGE_HEIGHT #x806C)

(defconstant GL_UNPACK_SKIP_IMAGES #x806D)

(defconstant GL_UNPACK_IMAGE_HEIGHT #x806E)

(defconstant GL_TEXTURE_3D #x806F)

(defconstant GL_PROXY_TEXTURE_3D #x8070)

(defconstant GL_TEXTURE_DEPTH #x8071)

(defconstant GL_TEXTURE_WRAP_R #x8072)

(defconstant GL_MAX_3D_TEXTURE_SIZE #x8073)

(defconstant GL_UNSIGNED_BYTE_2_3_3_REV #x8362)

(defconstant GL_UNSIGNED_SHORT_5_6_5 #x8363)

(defconstant GL_UNSIGNED_SHORT_5_6_5_REV #x8364)

(defconstant GL_UNSIGNED_SHORT_4_4_4_4_REV #x8365)

(defconstant GL_UNSIGNED_SHORT_1_5_5_5_REV #x8366)

(defconstant GL_UNSIGNED_INT_8_8_8_8_REV #x8367)

(defconstant GL_UNSIGNED_INT_2_10_10_10_REV #x8368)

(defconstant GL_BGR #x80E0)

(defconstant GL_BGRA #x80E1)

(defconstant GL_MAX_ELEMENTS_VERTICES #x80E8)

(defconstant GL_MAX_ELEMENTS_INDICES #x80E9)

(defconstant GL_CLAMP_TO_EDGE #x812F)

(defconstant GL_TEXTURE_MIN_LOD #x813A)

(defconstant GL_TEXTURE_MAX_LOD #x813B)

(defconstant GL_TEXTURE_BASE_LEVEL #x813C)

(defconstant GL_TEXTURE_MAX_LEVEL #x813D)

(defconstant GL_LIGHT_MODEL_COLOR_CONTROL #x81F8)

(defconstant GL_SINGLE_COLOR #x81F9)

(defconstant GL_SEPARATE_SPECULAR_COLOR #x81FA)

(defconstant GL_SMOOTH_POINT_SIZE_RANGE #x0B12)

(defconstant GL_SMOOTH_POINT_SIZE_GRANULARITY #x0B13)

(defconstant GL_SMOOTH_LINE_WIDTH_RANGE #x0B22)

(defconstant GL_SMOOTH_LINE_WIDTH_GRANULARITY #x0B23)

(defconstant GL_ALIASED_POINT_SIZE_RANGE #x846D)

(defconstant GL_ALIASED_LINE_WIDTH_RANGE #x846E)

(defconstant GL_CONSTANT_COLOR #x8001)

(defconstant GL_ONE_MINUS_CONSTANT_COLOR #x8002)

(defconstant GL_CONSTANT_ALPHA #x8003)

(defconstant GL_ONE_MINUS_CONSTANT_ALPHA #x8004)

(defconstant GL_BLEND_COLOR #x8005)

(defconstant GL_FUNC_ADD #x8006)

(defconstant GL_MIN #x8007)

(defconstant GL_MAX #x8008)

(defconstant GL_BLEND_EQUATION #x8009)

(defconstant GL_FUNC_SUBTRACT #x800A)

(defconstant GL_FUNC_REVERSE_SUBTRACT #x800B)

(defconstant GL_CONVOLUTION_1D #x8010)

(defconstant GL_CONVOLUTION_2D #x8011)

(defconstant GL_SEPARABLE_2D #x8012)

(defconstant GL_CONVOLUTION_BORDER_MODE #x8013)

(defconstant GL_CONVOLUTION_FILTER_SCALE #x8014)

(defconstant GL_CONVOLUTION_FILTER_BIAS #x8015)

(defconstant GL_REDUCE #x8016)

(defconstant GL_CONVOLUTION_FORMAT #x8017)

(defconstant GL_CONVOLUTION_WIDTH #x8018)

(defconstant GL_CONVOLUTION_HEIGHT #x8019)

(defconstant GL_MAX_CONVOLUTION_WIDTH #x801A)

(defconstant GL_MAX_CONVOLUTION_HEIGHT #x801B)

(defconstant GL_POST_CONVOLUTION_RED_SCALE #x801C)

(defconstant GL_POST_CONVOLUTION_GREEN_SCALE #x801D)

(defconstant GL_POST_CONVOLUTION_BLUE_SCALE #x801E)

(defconstant GL_POST_CONVOLUTION_ALPHA_SCALE #x801F)

(defconstant GL_POST_CONVOLUTION_RED_BIAS #x8020)

(defconstant GL_POST_CONVOLUTION_GREEN_BIAS #x8021)

(defconstant GL_POST_CONVOLUTION_BLUE_BIAS #x8022)

(defconstant GL_POST_CONVOLUTION_ALPHA_BIAS #x8023)

(defconstant GL_HISTOGRAM #x8024)

(defconstant GL_PROXY_HISTOGRAM #x8025)

(defconstant GL_HISTOGRAM_WIDTH #x8026)

(defconstant GL_HISTOGRAM_FORMAT #x8027)

(defconstant GL_HISTOGRAM_RED_SIZE #x8028)

(defconstant GL_HISTOGRAM_GREEN_SIZE #x8029)

(defconstant GL_HISTOGRAM_BLUE_SIZE #x802A)

(defconstant GL_HISTOGRAM_ALPHA_SIZE #x802B)

(defconstant GL_HISTOGRAM_LUMINANCE_SIZE #x802C)

(defconstant GL_HISTOGRAM_SINK #x802D)

(defconstant GL_MINMAX #x802E)

(defconstant GL_MINMAX_FORMAT #x802F)

(defconstant GL_MINMAX_SINK #x8030)

(defconstant GL_TABLE_TOO_LARGE #x8031)

(defconstant GL_COLOR_MATRIX #x80B1)

(defconstant GL_COLOR_MATRIX_STACK_DEPTH #x80B2)

(defconstant GL_MAX_COLOR_MATRIX_STACK_DEPTH #x80B3)

(defconstant GL_POST_COLOR_MATRIX_RED_SCALE #x80B4)

(defconstant GL_POST_COLOR_MATRIX_GREEN_SCALE #x80B5)

(defconstant GL_POST_COLOR_MATRIX_BLUE_SCALE #x80B6)

(defconstant GL_POST_COLOR_MATRIX_ALPHA_SCALE #x80B7)

(defconstant GL_POST_COLOR_MATRIX_RED_BIAS #x80B8)

(defconstant GL_POST_COLOR_MATRIX_GREEN_BIAS #x80B9)

(defconstant GL_POST_COLOR_MATRIX_BLUE_BIAS #x80BA)

(defconstant GL_POST_COLOR_MATRIX_ALPHA_BIAS #x80BB)

(defconstant GL_COLOR_TABLE #x80D0)

(defconstant GL_POST_CONVOLUTION_COLOR_TABLE #x80D1)

(defconstant GL_POST_COLOR_MATRIX_COLOR_TABLE #x80D2)

(defconstant GL_PROXY_COLOR_TABLE #x80D3)

(defconstant GL_PROXY_POST_CONVOLUTION_COLOR_TABLE #x80D4)

(defconstant GL_PROXY_POST_COLOR_MATRIX_COLOR_TABLE #x80D5)

(defconstant GL_COLOR_TABLE_SCALE #x80D6)

(defconstant GL_COLOR_TABLE_BIAS #x80D7)

(defconstant GL_COLOR_TABLE_FORMAT #x80D8)

(defconstant GL_COLOR_TABLE_WIDTH #x80D9)

(defconstant GL_COLOR_TABLE_RED_SIZE #x80DA)

(defconstant GL_COLOR_TABLE_GREEN_SIZE #x80DB)

(defconstant GL_COLOR_TABLE_BLUE_SIZE #x80DC)

(defconstant GL_COLOR_TABLE_ALPHA_SIZE #x80DD)

(defconstant GL_COLOR_TABLE_LUMINANCE_SIZE #x80DE)

(defconstant GL_COLOR_TABLE_INTENSITY_SIZE #x80DF)

(defconstant GL_CONSTANT_BORDER #x8151)

(defconstant GL_REPLICATE_BORDER #x8153)

(defconstant GL_CONVOLUTION_BORDER_COLOR #x8154)

(defconstant GL_TEXTURE0 #x84C0)

(defconstant GL_TEXTURE1 #x84C1)

(defconstant GL_TEXTURE2 #x84C2)

(defconstant GL_TEXTURE3 #x84C3)

(defconstant GL_TEXTURE4 #x84C4)

(defconstant GL_TEXTURE5 #x84C5)

(defconstant GL_TEXTURE6 #x84C6)

(defconstant GL_TEXTURE7 #x84C7)

(defconstant GL_TEXTURE8 #x84C8)

(defconstant GL_TEXTURE9 #x84C9)

(defconstant GL_TEXTURE10 #x84CA)

(defconstant GL_TEXTURE11 #x84CB)

(defconstant GL_TEXTURE12 #x84CC)

(defconstant GL_TEXTURE13 #x84CD)

(defconstant GL_TEXTURE14 #x84CE)

(defconstant GL_TEXTURE15 #x84CF)

(defconstant GL_TEXTURE16 #x84D0)

(defconstant GL_TEXTURE17 #x84D1)

(defconstant GL_TEXTURE18 #x84D2)

(defconstant GL_TEXTURE19 #x84D3)

(defconstant GL_TEXTURE20 #x84D4)

(defconstant GL_TEXTURE21 #x84D5)

(defconstant GL_TEXTURE22 #x84D6)

(defconstant GL_TEXTURE23 #x84D7)

(defconstant GL_TEXTURE24 #x84D8)

(defconstant GL_TEXTURE25 #x84D9)

(defconstant GL_TEXTURE26 #x84DA)

(defconstant GL_TEXTURE27 #x84DB)

(defconstant GL_TEXTURE28 #x84DC)

(defconstant GL_TEXTURE29 #x84DD)

(defconstant GL_TEXTURE30 #x84DE)

(defconstant GL_TEXTURE31 #x84DF)

(defconstant GL_ACTIVE_TEXTURE #x84E0)

(defconstant GL_CLIENT_ACTIVE_TEXTURE #x84E1)

(defconstant GL_MAX_TEXTURE_UNITS #x84E2)

(defconstant GL_TRANSPOSE_MODELVIEW_MATRIX #x84E3)

(defconstant GL_TRANSPOSE_PROJECTION_MATRIX #x84E4)

(defconstant GL_TRANSPOSE_TEXTURE_MATRIX #x84E5)

(defconstant GL_TRANSPOSE_COLOR_MATRIX #x84E6)

(defconstant GL_MULTISAMPLE #x809D)

(defconstant GL_SAMPLE_ALPHA_TO_COVERAGE #x809E)

(defconstant GL_SAMPLE_ALPHA_TO_ONE #x809F)

(defconstant GL_SAMPLE_COVERAGE #x80A0)

(defconstant GL_SAMPLE_BUFFERS #x80A8)

(defconstant GL_SAMPLES #x80A9)

(defconstant GL_SAMPLE_COVERAGE_VALUE #x80AA)

(defconstant GL_SAMPLE_COVERAGE_INVERT #x80AB)

(defconstant GL_MULTISAMPLE_BIT #x20000000)

(defconstant GL_NORMAL_MAP #x8511)

(defconstant GL_REFLECTION_MAP #x8512)

(defconstant GL_TEXTURE_CUBE_MAP #x8513)

(defconstant GL_TEXTURE_BINDING_CUBE_MAP #x8514)

(defconstant GL_TEXTURE_CUBE_MAP_POSITIVE_X #x8515)

(defconstant GL_TEXTURE_CUBE_MAP_NEGATIVE_X #x8516)

(defconstant GL_TEXTURE_CUBE_MAP_POSITIVE_Y #x8517)

(defconstant GL_TEXTURE_CUBE_MAP_NEGATIVE_Y #x8518)

(defconstant GL_TEXTURE_CUBE_MAP_POSITIVE_Z #x8519)

(defconstant GL_TEXTURE_CUBE_MAP_NEGATIVE_Z #x851A)

(defconstant GL_PROXY_TEXTURE_CUBE_MAP #x851B)

(defconstant GL_MAX_CUBE_MAP_TEXTURE_SIZE #x851C)

(defconstant GL_COMPRESSED_ALPHA #x84E9)

(defconstant GL_COMPRESSED_LUMINANCE #x84EA)

(defconstant GL_COMPRESSED_LUMINANCE_ALPHA #x84EB)

(defconstant GL_COMPRESSED_INTENSITY #x84EC)

(defconstant GL_COMPRESSED_RGB #x84ED)

(defconstant GL_COMPRESSED_RGBA #x84EE)

(defconstant GL_TEXTURE_COMPRESSION_HINT #x84EF)

(defconstant GL_TEXTURE_COMPRESSED_IMAGE_SIZE #x86A0)

(defconstant GL_TEXTURE_COMPRESSED #x86A1)

(defconstant GL_NUM_COMPRESSED_TEXTURE_FORMATS #x86A2)

(defconstant GL_COMPRESSED_TEXTURE_FORMATS #x86A3)

(defconstant GL_CLAMP_TO_BORDER #x812D)

(defconstant GL_CLAMP_TO_BORDER_SGIS #x812D)

(defconstant GL_COMBINE #x8570)

(defconstant GL_COMBINE_RGB #x8571)

(defconstant GL_COMBINE_ALPHA #x8572)

(defconstant GL_SOURCE0_RGB #x8580)

(defconstant GL_SOURCE1_RGB #x8581)

(defconstant GL_SOURCE2_RGB #x8582)

(defconstant GL_SOURCE0_ALPHA #x8588)

(defconstant GL_SOURCE1_ALPHA #x8589)

(defconstant GL_SOURCE2_ALPHA #x858A)

(defconstant GL_OPERAND0_RGB #x8590)

(defconstant GL_OPERAND1_RGB #x8591)

(defconstant GL_OPERAND2_RGB #x8592)

(defconstant GL_OPERAND0_ALPHA #x8598)

(defconstant GL_OPERAND1_ALPHA #x8599)

(defconstant GL_OPERAND2_ALPHA #x859A)

(defconstant GL_RGB_SCALE #x8573)

(defconstant GL_ADD_SIGNED #x8574)

(defconstant GL_INTERPOLATE #x8575)

(defconstant GL_SUBTRACT #x84E7)

(defconstant GL_CONSTANT #x8576)

(defconstant GL_PRIMARY_COLOR #x8577)

(defconstant GL_PREVIOUS #x8578)

(defconstant GL_DOT3_RGB #x86AE)

(defconstant GL_DOT3_RGBA #x86AF)

(defconstant GL_BLEND_DST_RGB #x80C8)

(defconstant GL_BLEND_SRC_RGB #x80C9)

(defconstant GL_BLEND_DST_ALPHA #x80CA)

(defconstant GL_BLEND_SRC_ALPHA #x80CB)

(defconstant GL_POINT_SIZE_MIN #x8126)

(defconstant GL_POINT_SIZE_MAX #x8127)

(defconstant GL_POINT_FADE_THRESHOLD_SIZE #x8128)

(defconstant GL_POINT_DISTANCE_ATTENUATION #x8129)

(defconstant GL_GENERATE_MIPMAP #x8191)

(defconstant GL_GENERATE_MIPMAP_HINT #x8192)

(defconstant GL_DEPTH_COMPONENT16 #x81A5)

(defconstant GL_DEPTH_COMPONENT24 #x81A6)

(defconstant GL_DEPTH_COMPONENT32 #x81A7)

(defconstant GL_MIRRORED_REPEAT #x8370)

(defconstant GL_FOG_COORDINATE_SOURCE #x8450)

(defconstant GL_FOG_COORDINATE #x8451)

(defconstant GL_FRAGMENT_DEPTH #x8452)

(defconstant GL_CURRENT_FOG_COORDINATE #x8453)

(defconstant GL_FOG_COORDINATE_ARRAY_TYPE #x8454)

(defconstant GL_FOG_COORDINATE_ARRAY_STRIDE #x8455)

(defconstant GL_FOG_COORDINATE_ARRAY_POINTER #x8456)

(defconstant GL_FOG_COORDINATE_ARRAY #x8457)

(defconstant GL_COLOR_SUM #x8458)

(defconstant GL_CURRENT_SECONDARY_COLOR #x8459)

(defconstant GL_SECONDARY_COLOR_ARRAY_SIZE #x845A)

(defconstant GL_SECONDARY_COLOR_ARRAY_TYPE #x845B)

(defconstant GL_SECONDARY_COLOR_ARRAY_STRIDE #x845C)

(defconstant GL_SECONDARY_COLOR_ARRAY_POINTER #x845D)

(defconstant GL_SECONDARY_COLOR_ARRAY #x845E)

(defconstant GL_MAX_TEXTURE_LOD_BIAS #x84FD)

(defconstant GL_TEXTURE_FILTER_CONTROL #x8500)

(defconstant GL_TEXTURE_LOD_BIAS #x8501)

(defconstant GL_INCR_WRAP #x8507)

(defconstant GL_DECR_WRAP #x8508)

(defconstant GL_TEXTURE_DEPTH_SIZE #x884A)

(defconstant GL_DEPTH_TEXTURE_MODE #x884B)

(defconstant GL_TEXTURE_COMPARE_MODE #x884C)

(defconstant GL_TEXTURE_COMPARE_FUNC #x884D)

(defconstant GL_COMPARE_R_TO_TEXTURE #x884E)

(defconstant GL_BUFFER_SIZE #x8764)

(defconstant GL_BUFFER_USAGE #x8765)

(defconstant GL_QUERY_COUNTER_BITS #x8864)

(defconstant GL_CURRENT_QUERY #x8865)

(defconstant GL_QUERY_RESULT #x8866)

(defconstant GL_QUERY_RESULT_AVAILABLE #x8867)

(defconstant GL_ARRAY_BUFFER #x8892)

(defconstant GL_ELEMENT_ARRAY_BUFFER #x8893)

(defconstant GL_ARRAY_BUFFER_BINDING #x8894)

(defconstant GL_ELEMENT_ARRAY_BUFFER_BINDING #x8895)

(defconstant GL_VERTEX_ARRAY_BUFFER_BINDING #x8896)

(defconstant GL_NORMAL_ARRAY_BUFFER_BINDING #x8897)

(defconstant GL_COLOR_ARRAY_BUFFER_BINDING #x8898)

(defconstant GL_INDEX_ARRAY_BUFFER_BINDING #x8899)

(defconstant GL_TEXTURE_COORD_ARRAY_BUFFER_BINDING #x889A)

(defconstant GL_EDGE_FLAG_ARRAY_BUFFER_BINDING #x889B)

(defconstant GL_SECONDARY_COLOR_ARRAY_BUFFER_BINDING #x889C)

(defconstant GL_FOG_COORDINATE_ARRAY_BUFFER_BINDING #x889D)

(defconstant GL_WEIGHT_ARRAY_BUFFER_BINDING #x889E)

(defconstant GL_VERTEX_ATTRIB_ARRAY_BUFFER_BINDING #x889F)

(defconstant GL_READ_ONLY #x88B8)

(defconstant GL_WRITE_ONLY #x88B9)

(defconstant GL_READ_WRITE #x88BA)

(defconstant GL_BUFFER_ACCESS #x88BB)

(defconstant GL_BUFFER_MAPPED #x88BC)

(defconstant GL_BUFFER_MAP_POINTER #x88BD)

(defconstant GL_STREAM_DRAW #x88E0)

(defconstant GL_STREAM_READ #x88E1)

(defconstant GL_STREAM_COPY #x88E2)

(defconstant GL_STATIC_DRAW #x88E4)

(defconstant GL_STATIC_READ #x88E5)

(defconstant GL_STATIC_COPY #x88E6)

(defconstant GL_DYNAMIC_DRAW #x88E8)

(defconstant GL_DYNAMIC_READ #x88E9)

(defconstant GL_DYNAMIC_COPY #x88EA)

(defconstant GL_SAMPLES_PASSED #x8914)

(defconstant GL_FOG_COORD_SOURCE #x8450)

(defconstant GL_FOG_COORD #x8451)

(defconstant GL_CURRENT_FOG_COORD #x8453)

(defconstant GL_FOG_COORD_ARRAY_TYPE #x8454)

(defconstant GL_FOG_COORD_ARRAY_STRIDE #x8455)

(defconstant GL_FOG_COORD_ARRAY_POINTER #x8456)

(defconstant GL_FOG_COORD_ARRAY #x8457)

(defconstant GL_FOG_COORD_ARRAY_BUFFER_BINDING #x889D)

(defconstant GL_SRC0_RGB #x8580)

(defconstant GL_SRC1_RGB #x8581)

(defconstant GL_SRC2_RGB #x8582)

(defconstant GL_SRC0_ALPHA #x8588)

(defconstant GL_SRC1_ALPHA #x8589)

(defconstant GL_SRC2_ALPHA #x858A)

(defconstant GL_TEXTURE0_ARB #x84C0)

(defconstant GL_TEXTURE1_ARB #x84C1)

(defconstant GL_TEXTURE2_ARB #x84C2)

(defconstant GL_TEXTURE3_ARB #x84C3)

(defconstant GL_TEXTURE4_ARB #x84C4)

(defconstant GL_TEXTURE5_ARB #x84C5)

(defconstant GL_TEXTURE6_ARB #x84C6)

(defconstant GL_TEXTURE7_ARB #x84C7)

(defconstant GL_TEXTURE8_ARB #x84C8)

(defconstant GL_TEXTURE9_ARB #x84C9)

(defconstant GL_TEXTURE10_ARB #x84CA)

(defconstant GL_TEXTURE11_ARB #x84CB)

(defconstant GL_TEXTURE12_ARB #x84CC)

(defconstant GL_TEXTURE13_ARB #x84CD)

(defconstant GL_TEXTURE14_ARB #x84CE)

(defconstant GL_TEXTURE15_ARB #x84CF)

(defconstant GL_TEXTURE16_ARB #x84D0)

(defconstant GL_TEXTURE17_ARB #x84D1)

(defconstant GL_TEXTURE18_ARB #x84D2)

(defconstant GL_TEXTURE19_ARB #x84D3)

(defconstant GL_TEXTURE20_ARB #x84D4)

(defconstant GL_TEXTURE21_ARB #x84D5)

(defconstant GL_TEXTURE22_ARB #x84D6)

(defconstant GL_TEXTURE23_ARB #x84D7)

(defconstant GL_TEXTURE24_ARB #x84D8)

(defconstant GL_TEXTURE25_ARB #x84D9)

(defconstant GL_TEXTURE26_ARB #x84DA)

(defconstant GL_TEXTURE27_ARB #x84DB)

(defconstant GL_TEXTURE28_ARB #x84DC)

(defconstant GL_TEXTURE29_ARB #x84DD)

(defconstant GL_TEXTURE30_ARB #x84DE)

(defconstant GL_TEXTURE31_ARB #x84DF)

(defconstant GL_ACTIVE_TEXTURE_ARB #x84E0)

(defconstant GL_CLIENT_ACTIVE_TEXTURE_ARB #x84E1)

(defconstant GL_MAX_TEXTURE_UNITS_ARB #x84E2)

(defconstant GL_TRANSPOSE_MODELVIEW_MATRIX_ARB #x84E3)

(defconstant GL_TRANSPOSE_PROJECTION_MATRIX_ARB #x84E4)

(defconstant GL_TRANSPOSE_TEXTURE_MATRIX_ARB #x84E5)

(defconstant GL_TRANSPOSE_COLOR_MATRIX_ARB #x84E6)

(defconstant GL_MULTISAMPLE_ARB #x809D)

(defconstant GL_SAMPLE_ALPHA_TO_COVERAGE_ARB #x809E)

(defconstant GL_SAMPLE_ALPHA_TO_ONE_ARB #x809F)

(defconstant GL_SAMPLE_COVERAGE_ARB #x80A0)

(defconstant GL_SAMPLE_BUFFERS_ARB #x80A8)

(defconstant GL_SAMPLES_ARB #x80A9)

(defconstant GL_SAMPLE_COVERAGE_VALUE_ARB #x80AA)

(defconstant GL_SAMPLE_COVERAGE_INVERT_ARB #x80AB)

(defconstant GL_MULTISAMPLE_BIT_ARB #x20000000)

(defconstant GL_NORMAL_MAP_ARB #x8511)

(defconstant GL_REFLECTION_MAP_ARB #x8512)

(defconstant GL_TEXTURE_CUBE_MAP_ARB #x8513)

(defconstant GL_TEXTURE_BINDING_CUBE_MAP_ARB #x8514)

(defconstant GL_TEXTURE_CUBE_MAP_POSITIVE_X_ARB #x8515)

(defconstant GL_TEXTURE_CUBE_MAP_NEGATIVE_X_ARB #x8516)

(defconstant GL_TEXTURE_CUBE_MAP_POSITIVE_Y_ARB #x8517)

(defconstant GL_TEXTURE_CUBE_MAP_NEGATIVE_Y_ARB #x8518)

(defconstant GL_TEXTURE_CUBE_MAP_POSITIVE_Z_ARB #x8519)

(defconstant GL_TEXTURE_CUBE_MAP_NEGATIVE_Z_ARB #x851A)

(defconstant GL_PROXY_TEXTURE_CUBE_MAP_ARB #x851B)

(defconstant GL_MAX_CUBE_MAP_TEXTURE_SIZE_ARB #x851C)

(defconstant GL_COMPRESSED_ALPHA_ARB #x84E9)

(defconstant GL_COMPRESSED_LUMINANCE_ARB #x84EA)

(defconstant GL_COMPRESSED_LUMINANCE_ALPHA_ARB #x84EB)

(defconstant GL_COMPRESSED_INTENSITY_ARB #x84EC)

(defconstant GL_COMPRESSED_RGB_ARB #x84ED)

(defconstant GL_COMPRESSED_RGBA_ARB #x84EE)

(defconstant GL_TEXTURE_COMPRESSION_HINT_ARB #x84EF)

(defconstant GL_TEXTURE_COMPRESSED_IMAGE_SIZE_ARB #x86A0)

(defconstant GL_TEXTURE_COMPRESSED_ARB #x86A1)

(defconstant GL_NUM_COMPRESSED_TEXTURE_FORMATS_ARB #x86A2)

(defconstant GL_COMPRESSED_TEXTURE_FORMATS_ARB #x86A3)

(defconstant GL_CLAMP_TO_BORDER_ARB #x812D)

(defconstant GL_POINT_SIZE_MIN_ARB #x8126)

(defconstant GL_POINT_SIZE_MAX_ARB #x8127)

(defconstant GL_POINT_FADE_THRESHOLD_SIZE_ARB #x8128)

(defconstant GL_POINT_DISTANCE_ATTENUATION_ARB #x8129)

(defconstant GL_MAX_VERTEX_UNITS_ARB #x86A4)

(defconstant GL_ACTIVE_VERTEX_UNITS_ARB #x86A5)

(defconstant GL_WEIGHT_SUM_UNITY_ARB #x86A6)

(defconstant GL_VERTEX_BLEND_ARB #x86A7)

(defconstant GL_CURRENT_WEIGHT_ARB #x86A8)

(defconstant GL_WEIGHT_ARRAY_TYPE_ARB #x86A9)

(defconstant GL_WEIGHT_ARRAY_STRIDE_ARB #x86AA)

(defconstant GL_WEIGHT_ARRAY_SIZE_ARB #x86AB)

(defconstant GL_WEIGHT_ARRAY_POINTER_ARB #x86AC)

(defconstant GL_WEIGHT_ARRAY_ARB #x86AD)

(defconstant GL_MODELVIEW0_ARB #x1700)

(defconstant GL_MODELVIEW1_ARB #x850A)

(defconstant GL_MODELVIEW2_ARB #x8722)

(defconstant GL_MODELVIEW3_ARB #x8723)

(defconstant GL_MODELVIEW4_ARB #x8724)

(defconstant GL_MODELVIEW5_ARB #x8725)

(defconstant GL_MODELVIEW6_ARB #x8726)

(defconstant GL_MODELVIEW7_ARB #x8727)

(defconstant GL_MODELVIEW8_ARB #x8728)

(defconstant GL_MODELVIEW9_ARB #x8729)

(defconstant GL_MODELVIEW10_ARB #x872A)

(defconstant GL_MODELVIEW11_ARB #x872B)

(defconstant GL_MODELVIEW12_ARB #x872C)

(defconstant GL_MODELVIEW13_ARB #x872D)

(defconstant GL_MODELVIEW14_ARB #x872E)

(defconstant GL_MODELVIEW15_ARB #x872F)

(defconstant GL_MODELVIEW16_ARB #x8730)

(defconstant GL_MODELVIEW17_ARB #x8731)

(defconstant GL_MODELVIEW18_ARB #x8732)

(defconstant GL_MODELVIEW19_ARB #x8733)

(defconstant GL_MODELVIEW20_ARB #x8734)

(defconstant GL_MODELVIEW21_ARB #x8735)

(defconstant GL_MODELVIEW22_ARB #x8736)

(defconstant GL_MODELVIEW23_ARB #x8737)

(defconstant GL_MODELVIEW24_ARB #x8738)

(defconstant GL_MODELVIEW25_ARB #x8739)

(defconstant GL_MODELVIEW26_ARB #x873A)

(defconstant GL_MODELVIEW27_ARB #x873B)

(defconstant GL_MODELVIEW28_ARB #x873C)

(defconstant GL_MODELVIEW29_ARB #x873D)

(defconstant GL_MODELVIEW30_ARB #x873E)

(defconstant GL_MODELVIEW31_ARB #x873F)

(defconstant GL_MATRIX_PALETTE_ARB #x8840)

(defconstant GL_MAX_MATRIX_PALETTE_STACK_DEPTH_ARB #x8841)

(defconstant GL_MAX_PALETTE_MATRICES_ARB #x8842)

(defconstant GL_CURRENT_PALETTE_MATRIX_ARB #x8843)

(defconstant GL_MATRIX_INDEX_ARRAY_ARB #x8844)

(defconstant GL_CURRENT_MATRIX_INDEX_ARB #x8845)

(defconstant GL_MATRIX_INDEX_ARRAY_SIZE_ARB #x8846)

(defconstant GL_MATRIX_INDEX_ARRAY_TYPE_ARB #x8847)

(defconstant GL_MATRIX_INDEX_ARRAY_STRIDE_ARB #x8848)

(defconstant GL_MATRIX_INDEX_ARRAY_POINTER_ARB #x8849)

(defconstant GL_COMBINE_ARB #x8570)

(defconstant GL_COMBINE_RGB_ARB #x8571)

(defconstant GL_COMBINE_ALPHA_ARB #x8572)

(defconstant GL_SOURCE0_RGB_ARB #x8580)

(defconstant GL_SOURCE1_RGB_ARB #x8581)

(defconstant GL_SOURCE2_RGB_ARB #x8582)

(defconstant GL_SOURCE0_ALPHA_ARB #x8588)

(defconstant GL_SOURCE1_ALPHA_ARB #x8589)

(defconstant GL_SOURCE2_ALPHA_ARB #x858A)

(defconstant GL_OPERAND0_RGB_ARB #x8590)

(defconstant GL_OPERAND1_RGB_ARB #x8591)

(defconstant GL_OPERAND2_RGB_ARB #x8592)

(defconstant GL_OPERAND0_ALPHA_ARB #x8598)

(defconstant GL_OPERAND1_ALPHA_ARB #x8599)

(defconstant GL_OPERAND2_ALPHA_ARB #x859A)

(defconstant GL_RGB_SCALE_ARB #x8573)

(defconstant GL_ADD_SIGNED_ARB #x8574)

(defconstant GL_INTERPOLATE_ARB #x8575)

(defconstant GL_SUBTRACT_ARB #x84E7)

(defconstant GL_CONSTANT_ARB #x8576)

(defconstant GL_PRIMARY_COLOR_ARB #x8577)

(defconstant GL_PREVIOUS_ARB #x8578)

(defconstant GL_DOT3_RGB_ARB #x86AE)

(defconstant GL_DOT3_RGBA_ARB #x86AF)

(defconstant GL_MIRRORED_REPEAT_ARB #x8370)

(defconstant GL_DEPTH_COMPONENT16_ARB #x81A5)

(defconstant GL_DEPTH_COMPONENT24_ARB #x81A6)

(defconstant GL_DEPTH_COMPONENT32_ARB #x81A7)

(defconstant GL_TEXTURE_DEPTH_SIZE_ARB #x884A)

(defconstant GL_DEPTH_TEXTURE_MODE_ARB #x884B)

(defconstant GL_TEXTURE_COMPARE_MODE_ARB #x884C)

(defconstant GL_TEXTURE_COMPARE_FUNC_ARB #x884D)

(defconstant GL_COMPARE_R_TO_TEXTURE_ARB #x884E)

(defconstant GL_TEXTURE_COMPARE_FAIL_VALUE_ARB #x80BF)

(defconstant GL_COLOR_SUM_ARB #x8458)

(defconstant GL_VERTEX_PROGRAM_ARB #x8620)

(defconstant GL_VERTEX_ATTRIB_ARRAY_ENABLED_ARB #x8622)

(defconstant GL_VERTEX_ATTRIB_ARRAY_SIZE_ARB #x8623)

(defconstant GL_VERTEX_ATTRIB_ARRAY_STRIDE_ARB #x8624)

(defconstant GL_VERTEX_ATTRIB_ARRAY_TYPE_ARB #x8625)

(defconstant GL_CURRENT_VERTEX_ATTRIB_ARB #x8626)

(defconstant GL_PROGRAM_LENGTH_ARB #x8627)

(defconstant GL_PROGRAM_STRING_ARB #x8628)

(defconstant GL_MAX_PROGRAM_MATRIX_STACK_DEPTH_ARB #x862E)

(defconstant GL_MAX_PROGRAM_MATRICES_ARB #x862F)

(defconstant GL_CURRENT_MATRIX_STACK_DEPTH_ARB #x8640)

(defconstant GL_CURRENT_MATRIX_ARB #x8641)

(defconstant GL_VERTEX_PROGRAM_POINT_SIZE_ARB #x8642)

(defconstant GL_VERTEX_PROGRAM_TWO_SIDE_ARB #x8643)

(defconstant GL_VERTEX_ATTRIB_ARRAY_POINTER_ARB #x8645)

(defconstant GL_PROGRAM_ERROR_POSITION_ARB #x864B)

(defconstant GL_PROGRAM_BINDING_ARB #x8677)

(defconstant GL_MAX_VERTEX_ATTRIBS_ARB #x8869)

(defconstant GL_VERTEX_ATTRIB_ARRAY_NORMALIZED_ARB #x886A)

(defconstant GL_PROGRAM_ERROR_STRING_ARB #x8874)

(defconstant GL_PROGRAM_FORMAT_ASCII_ARB #x8875)

(defconstant GL_PROGRAM_FORMAT_ARB #x8876)

(defconstant GL_PROGRAM_INSTRUCTIONS_ARB #x88A0)

(defconstant GL_MAX_PROGRAM_INSTRUCTIONS_ARB #x88A1)

(defconstant GL_PROGRAM_NATIVE_INSTRUCTIONS_ARB #x88A2)

(defconstant GL_MAX_PROGRAM_NATIVE_INSTRUCTIONS_ARB #x88A3)

(defconstant GL_PROGRAM_TEMPORARIES_ARB #x88A4)

(defconstant GL_MAX_PROGRAM_TEMPORARIES_ARB #x88A5)

(defconstant GL_PROGRAM_NATIVE_TEMPORARIES_ARB #x88A6)

(defconstant GL_MAX_PROGRAM_NATIVE_TEMPORARIES_ARB #x88A7)

(defconstant GL_PROGRAM_PARAMETERS_ARB #x88A8)

(defconstant GL_MAX_PROGRAM_PARAMETERS_ARB #x88A9)

(defconstant GL_PROGRAM_NATIVE_PARAMETERS_ARB #x88AA)

(defconstant GL_MAX_PROGRAM_NATIVE_PARAMETERS_ARB #x88AB)

(defconstant GL_PROGRAM_ATTRIBS_ARB #x88AC)

(defconstant GL_MAX_PROGRAM_ATTRIBS_ARB #x88AD)

(defconstant GL_PROGRAM_NATIVE_ATTRIBS_ARB #x88AE)

(defconstant GL_MAX_PROGRAM_NATIVE_ATTRIBS_ARB #x88AF)

(defconstant GL_PROGRAM_ADDRESS_REGISTERS_ARB #x88B0)

(defconstant GL_MAX_PROGRAM_ADDRESS_REGISTERS_ARB #x88B1)

(defconstant GL_PROGRAM_NATIVE_ADDRESS_REGISTERS_ARB #x88B2)

(defconstant GL_MAX_PROGRAM_NATIVE_ADDRESS_REGISTERS_ARB #x88B3)

(defconstant GL_MAX_PROGRAM_LOCAL_PARAMETERS_ARB #x88B4)

(defconstant GL_MAX_PROGRAM_ENV_PARAMETERS_ARB #x88B5)

(defconstant GL_PROGRAM_UNDER_NATIVE_LIMITS_ARB #x88B6)

(defconstant GL_TRANSPOSE_CURRENT_MATRIX_ARB #x88B7)

(defconstant GL_MATRIX0_ARB #x88C0)

(defconstant GL_MATRIX1_ARB #x88C1)

(defconstant GL_MATRIX2_ARB #x88C2)

(defconstant GL_MATRIX3_ARB #x88C3)

(defconstant GL_MATRIX4_ARB #x88C4)

(defconstant GL_MATRIX5_ARB #x88C5)

(defconstant GL_MATRIX6_ARB #x88C6)

(defconstant GL_MATRIX7_ARB #x88C7)

(defconstant GL_MATRIX8_ARB #x88C8)

(defconstant GL_MATRIX9_ARB #x88C9)

(defconstant GL_MATRIX10_ARB #x88CA)

(defconstant GL_MATRIX11_ARB #x88CB)

(defconstant GL_MATRIX12_ARB #x88CC)

(defconstant GL_MATRIX13_ARB #x88CD)

(defconstant GL_MATRIX14_ARB #x88CE)

(defconstant GL_MATRIX15_ARB #x88CF)

(defconstant GL_MATRIX16_ARB #x88D0)

(defconstant GL_MATRIX17_ARB #x88D1)

(defconstant GL_MATRIX18_ARB #x88D2)

(defconstant GL_MATRIX19_ARB #x88D3)

(defconstant GL_MATRIX20_ARB #x88D4)

(defconstant GL_MATRIX21_ARB #x88D5)

(defconstant GL_MATRIX22_ARB #x88D6)

(defconstant GL_MATRIX23_ARB #x88D7)

(defconstant GL_MATRIX24_ARB #x88D8)

(defconstant GL_MATRIX25_ARB #x88D9)

(defconstant GL_MATRIX26_ARB #x88DA)

(defconstant GL_MATRIX27_ARB #x88DB)

(defconstant GL_MATRIX28_ARB #x88DC)

(defconstant GL_MATRIX29_ARB #x88DD)

(defconstant GL_MATRIX30_ARB #x88DE)

(defconstant GL_MATRIX31_ARB #x88DF)

(defconstant GL_FRAGMENT_PROGRAM_ARB #x8804)

(defconstant GL_PROGRAM_ALU_INSTRUCTIONS_ARB #x8805)

(defconstant GL_PROGRAM_TEX_INSTRUCTIONS_ARB #x8806)

(defconstant GL_PROGRAM_TEX_INDIRECTIONS_ARB #x8807)

(defconstant GL_PROGRAM_NATIVE_ALU_INSTRUCTIONS_ARB #x8808)

(defconstant GL_PROGRAM_NATIVE_TEX_INSTRUCTIONS_ARB #x8809)

(defconstant GL_PROGRAM_NATIVE_TEX_INDIRECTIONS_ARB #x880A)

(defconstant GL_MAX_PROGRAM_ALU_INSTRUCTIONS_ARB #x880B)

(defconstant GL_MAX_PROGRAM_TEX_INSTRUCTIONS_ARB #x880C)

(defconstant GL_MAX_PROGRAM_TEX_INDIRECTIONS_ARB #x880D)

(defconstant GL_MAX_PROGRAM_NATIVE_ALU_INSTRUCTIONS_ARB #x880E)

(defconstant GL_MAX_PROGRAM_NATIVE_TEX_INSTRUCTIONS_ARB #x880F)

(defconstant GL_MAX_PROGRAM_NATIVE_TEX_INDIRECTIONS_ARB #x8810)

(defconstant GL_MAX_TEXTURE_COORDS_ARB #x8871)

(defconstant GL_MAX_TEXTURE_IMAGE_UNITS_ARB #x8872)

(defconstant GL_BUFFER_SIZE_ARB #x8764)

(defconstant GL_BUFFER_USAGE_ARB #x8765)

(defconstant GL_ARRAY_BUFFER_ARB #x8892)

(defconstant GL_ELEMENT_ARRAY_BUFFER_ARB #x8893)

(defconstant GL_ARRAY_BUFFER_BINDING_ARB #x8894)

(defconstant GL_ELEMENT_ARRAY_BUFFER_BINDING_ARB #x8895)

(defconstant GL_VERTEX_ARRAY_BUFFER_BINDING_ARB #x8896)

(defconstant GL_NORMAL_ARRAY_BUFFER_BINDING_ARB #x8897)

(defconstant GL_COLOR_ARRAY_BUFFER_BINDING_ARB #x8898)

(defconstant GL_INDEX_ARRAY_BUFFER_BINDING_ARB #x8899)

(defconstant GL_TEXTURE_COORD_ARRAY_BUFFER_BINDING_ARB #x889A)

(defconstant GL_EDGE_FLAG_ARRAY_BUFFER_BINDING_ARB #x889B)

(defconstant GL_SECONDARY_COLOR_ARRAY_BUFFER_BINDING_ARB #x889C)

(defconstant GL_FOG_COORDINATE_ARRAY_BUFFER_BINDING_ARB #x889D)

(defconstant GL_WEIGHT_ARRAY_BUFFER_BINDING_ARB #x889E)

(defconstant GL_VERTEX_ATTRIB_ARRAY_BUFFER_BINDING_ARB #x889F)

(defconstant GL_READ_ONLY_ARB #x88B8)

(defconstant GL_WRITE_ONLY_ARB #x88B9)

(defconstant GL_READ_WRITE_ARB #x88BA)

(defconstant GL_BUFFER_ACCESS_ARB #x88BB)

(defconstant GL_BUFFER_MAPPED_ARB #x88BC)

(defconstant GL_BUFFER_MAP_POINTER_ARB #x88BD)

(defconstant GL_STREAM_DRAW_ARB #x88E0)

(defconstant GL_STREAM_READ_ARB #x88E1)

(defconstant GL_STREAM_COPY_ARB #x88E2)

(defconstant GL_STATIC_DRAW_ARB #x88E4)

(defconstant GL_STATIC_READ_ARB #x88E5)

(defconstant GL_STATIC_COPY_ARB #x88E6)

(defconstant GL_DYNAMIC_DRAW_ARB #x88E8)

(defconstant GL_DYNAMIC_READ_ARB #x88E9)

(defconstant GL_DYNAMIC_COPY_ARB #x88EA)

(defconstant GL_QUERY_COUNTER_BITS_ARB #x8864)

(defconstant GL_CURRENT_QUERY_ARB #x8865)

(defconstant GL_QUERY_RESULT_ARB #x8866)

(defconstant GL_QUERY_RESULT_AVAILABLE_ARB #x8867)

(defconstant GL_SAMPLES_PASSED_ARB #x8914)

(defconstant GL_PROGRAM_OBJECT_ARB #x8B40)

(defconstant GL_SHADER_OBJECT_ARB #x8B48)

(defconstant GL_OBJECT_TYPE_ARB #x8B4E)

(defconstant GL_OBJECT_SUBTYPE_ARB #x8B4F)

(defconstant GL_FLOAT_VEC2_ARB #x8B50)

(defconstant GL_FLOAT_VEC3_ARB #x8B51)

(defconstant GL_FLOAT_VEC4_ARB #x8B52)

(defconstant GL_INT_VEC2_ARB #x8B53)

(defconstant GL_INT_VEC3_ARB #x8B54)

(defconstant GL_INT_VEC4_ARB #x8B55)

(defconstant GL_BOOL_ARB #x8B56)

(defconstant GL_BOOL_VEC2_ARB #x8B57)

(defconstant GL_BOOL_VEC3_ARB #x8B58)

(defconstant GL_BOOL_VEC4_ARB #x8B59)

(defconstant GL_FLOAT_MAT2_ARB #x8B5A)

(defconstant GL_FLOAT_MAT3_ARB #x8B5B)

(defconstant GL_FLOAT_MAT4_ARB #x8B5C)

(defconstant GL_OBJECT_DELETE_STATUS_ARB #x8B80)

(defconstant GL_OBJECT_COMPILE_STATUS_ARB #x8B81)

(defconstant GL_OBJECT_LINK_STATUS_ARB #x8B82)

(defconstant GL_OBJECT_VALIDATE_STATUS_ARB #x8B83)

(defconstant GL_OBJECT_INFO_LOG_LENGTH_ARB #x8B84)

(defconstant GL_OBJECT_ATTACHED_OBJECTS_ARB #x8B85)

(defconstant GL_OBJECT_ACTIVE_UNIFORMS_ARB #x8B86)

(defconstant GL_OBJECT_ACTIVE_UNIFORM_MAX_LENGTH_ARB #x8B87)

(defconstant GL_OBJECT_SHADER_SOURCE_LENGTH_ARB #x8B88)

(defconstant GL_VERTEX_SHADER_ARB #x8B31)

(defconstant GL_MAX_VERTEX_UNIFORM_COMPONENTS_ARB #x8B4A)

(defconstant GL_MAX_VARYING_FLOATS_ARB #x8B4B)

(defconstant GL_MAX_VERTEX_TEXTURE_IMAGE_UNITS_ARB #x8B4C)

(defconstant GL_MAX_COMBINED_TEXTURE_IMAGE_UNITS_ARB #x8B4D)

(defconstant GL_OBJECT_ACTIVE_ATTRIBUTES_ARB #x8B89)

(defconstant GL_OBJECT_ACTIVE_ATTRIBUTE_MAX_LENGTH_ARB #x8B8A)

(defconstant GL_FRAGMENT_SHADER_ARB #x8B30)

(defconstant GL_MAX_FRAGMENT_UNIFORM_COMPONENTS_ARB #x8B49)

(defconstant GL_POINT_SPRITE_ARB #x8861)

(defconstant GL_COORD_REPLACE_ARB #x8862)

(defconstant GL_ABGR_EXT #x8000)

(defconstant GL_CONSTANT_COLOR_EXT #x8001)

(defconstant GL_ONE_MINUS_CONSTANT_COLOR_EXT #x8002)

(defconstant GL_CONSTANT_ALPHA_EXT #x8003)

(defconstant GL_ONE_MINUS_CONSTANT_ALPHA_EXT #x8004)

(defconstant GL_BLEND_COLOR_EXT #x8005)

(defconstant GL_POLYGON_OFFSET_EXT #x8037)

(defconstant GL_POLYGON_OFFSET_FACTOR_EXT #x8038)

(defconstant GL_POLYGON_OFFSET_BIAS_EXT #x8039)

(defconstant GL_ALPHA4_EXT #x803B)

(defconstant GL_ALPHA8_EXT #x803C)

(defconstant GL_ALPHA12_EXT #x803D)

(defconstant GL_ALPHA16_EXT #x803E)

(defconstant GL_LUMINANCE4_EXT #x803F)

(defconstant GL_LUMINANCE8_EXT #x8040)

(defconstant GL_LUMINANCE12_EXT #x8041)

(defconstant GL_LUMINANCE16_EXT #x8042)

(defconstant GL_LUMINANCE4_ALPHA4_EXT #x8043)

(defconstant GL_LUMINANCE6_ALPHA2_EXT #x8044)

(defconstant GL_LUMINANCE8_ALPHA8_EXT #x8045)

(defconstant GL_LUMINANCE12_ALPHA4_EXT #x8046)

(defconstant GL_LUMINANCE12_ALPHA12_EXT #x8047)

(defconstant GL_LUMINANCE16_ALPHA16_EXT #x8048)

(defconstant GL_INTENSITY_EXT #x8049)

(defconstant GL_INTENSITY4_EXT #x804A)

(defconstant GL_INTENSITY8_EXT #x804B)

(defconstant GL_INTENSITY12_EXT #x804C)

(defconstant GL_INTENSITY16_EXT #x804D)

(defconstant GL_RGB2_EXT #x804E)

(defconstant GL_RGB4_EXT #x804F)

(defconstant GL_RGB5_EXT #x8050)

(defconstant GL_RGB8_EXT #x8051)

(defconstant GL_RGB10_EXT #x8052)

(defconstant GL_RGB12_EXT #x8053)

(defconstant GL_RGB16_EXT #x8054)

(defconstant GL_RGBA2_EXT #x8055)

(defconstant GL_RGBA4_EXT #x8056)

(defconstant GL_RGB5_A1_EXT #x8057)

(defconstant GL_RGBA8_EXT #x8058)

(defconstant GL_RGB10_A2_EXT #x8059)

(defconstant GL_RGBA12_EXT #x805A)

(defconstant GL_RGBA16_EXT #x805B)

(defconstant GL_TEXTURE_RED_SIZE_EXT #x805C)

(defconstant GL_TEXTURE_GREEN_SIZE_EXT #x805D)

(defconstant GL_TEXTURE_BLUE_SIZE_EXT #x805E)

(defconstant GL_TEXTURE_ALPHA_SIZE_EXT #x805F)

(defconstant GL_TEXTURE_LUMINANCE_SIZE_EXT #x8060)

(defconstant GL_TEXTURE_INTENSITY_SIZE_EXT #x8061)

(defconstant GL_REPLACE_EXT #x8062)

(defconstant GL_PROXY_TEXTURE_1D_EXT #x8063)

(defconstant GL_PROXY_TEXTURE_2D_EXT #x8064)

(defconstant GL_TEXTURE_TOO_LARGE_EXT #x8065)

(defconstant GL_PACK_SKIP_IMAGES_EXT #x806B)

(defconstant GL_PACK_IMAGE_HEIGHT_EXT #x806C)

(defconstant GL_UNPACK_SKIP_IMAGES_EXT #x806D)

(defconstant GL_UNPACK_IMAGE_HEIGHT_EXT #x806E)

(defconstant GL_TEXTURE_3D_EXT #x806F)

(defconstant GL_PROXY_TEXTURE_3D_EXT #x8070)

(defconstant GL_TEXTURE_DEPTH_EXT #x8071)

(defconstant GL_TEXTURE_WRAP_R_EXT #x8072)

(defconstant GL_MAX_3D_TEXTURE_SIZE_EXT #x8073)

(defconstant GL_FILTER4_SGIS #x8146)

(defconstant GL_TEXTURE_FILTER4_SIZE_SGIS #x8147)

(defconstant GL_HISTOGRAM_EXT #x8024)

(defconstant GL_PROXY_HISTOGRAM_EXT #x8025)

(defconstant GL_HISTOGRAM_WIDTH_EXT #x8026)

(defconstant GL_HISTOGRAM_FORMAT_EXT #x8027)

(defconstant GL_HISTOGRAM_RED_SIZE_EXT #x8028)

(defconstant GL_HISTOGRAM_GREEN_SIZE_EXT #x8029)

(defconstant GL_HISTOGRAM_BLUE_SIZE_EXT #x802A)

(defconstant GL_HISTOGRAM_ALPHA_SIZE_EXT #x802B)

(defconstant GL_HISTOGRAM_LUMINANCE_SIZE_EXT #x802C)

(defconstant GL_HISTOGRAM_SINK_EXT #x802D)

(defconstant GL_MINMAX_EXT #x802E)

(defconstant GL_MINMAX_FORMAT_EXT #x802F)

(defconstant GL_MINMAX_SINK_EXT #x8030)

(defconstant GL_TABLE_TOO_LARGE_EXT #x8031)

(defconstant GL_CONVOLUTION_1D_EXT #x8010)

(defconstant GL_CONVOLUTION_2D_EXT #x8011)

(defconstant GL_SEPARABLE_2D_EXT #x8012)

(defconstant GL_CONVOLUTION_BORDER_MODE_EXT #x8013)

(defconstant GL_CONVOLUTION_FILTER_SCALE_EXT #x8014)

(defconstant GL_CONVOLUTION_FILTER_BIAS_EXT #x8015)

(defconstant GL_REDUCE_EXT #x8016)

(defconstant GL_CONVOLUTION_FORMAT_EXT #x8017)

(defconstant GL_CONVOLUTION_WIDTH_EXT #x8018)

(defconstant GL_CONVOLUTION_HEIGHT_EXT #x8019)

(defconstant GL_MAX_CONVOLUTION_WIDTH_EXT #x801A)

(defconstant GL_MAX_CONVOLUTION_HEIGHT_EXT #x801B)

(defconstant GL_POST_CONVOLUTION_RED_SCALE_EXT #x801C)

(defconstant GL_POST_CONVOLUTION_GREEN_SCALE_EXT #x801D)

(defconstant GL_POST_CONVOLUTION_BLUE_SCALE_EXT #x801E)

(defconstant GL_POST_CONVOLUTION_ALPHA_SCALE_EXT #x801F)

(defconstant GL_POST_CONVOLUTION_RED_BIAS_EXT #x8020)

(defconstant GL_POST_CONVOLUTION_GREEN_BIAS_EXT #x8021)

(defconstant GL_POST_CONVOLUTION_BLUE_BIAS_EXT #x8022)

(defconstant GL_POST_CONVOLUTION_ALPHA_BIAS_EXT #x8023)

(defconstant GL_COLOR_MATRIX_SGI #x80B1)

(defconstant GL_COLOR_MATRIX_STACK_DEPTH_SGI #x80B2)

(defconstant GL_MAX_COLOR_MATRIX_STACK_DEPTH_SGI #x80B3)

(defconstant GL_POST_COLOR_MATRIX_RED_SCALE_SGI #x80B4)

(defconstant GL_POST_COLOR_MATRIX_GREEN_SCALE_SGI #x80B5)

(defconstant GL_POST_COLOR_MATRIX_BLUE_SCALE_SGI #x80B6)

(defconstant GL_POST_COLOR_MATRIX_ALPHA_SCALE_SGI #x80B7)

(defconstant GL_POST_COLOR_MATRIX_RED_BIAS_SGI #x80B8)

(defconstant GL_POST_COLOR_MATRIX_GREEN_BIAS_SGI #x80B9)

(defconstant GL_POST_COLOR_MATRIX_BLUE_BIAS_SGI #x80BA)

(defconstant GL_POST_COLOR_MATRIX_ALPHA_BIAS_SGI #x80BB)

(defconstant GL_COLOR_TABLE_SGI #x80D0)

(defconstant GL_POST_CONVOLUTION_COLOR_TABLE_SGI #x80D1)

(defconstant GL_POST_COLOR_MATRIX_COLOR_TABLE_SGI #x80D2)

(defconstant GL_PROXY_COLOR_TABLE_SGI #x80D3)

(defconstant GL_PROXY_POST_CONVOLUTION_COLOR_TABLE_SGI #x80D4)

(defconstant GL_PROXY_POST_COLOR_MATRIX_COLOR_TABLE_SGI #x80D5)

(defconstant GL_COLOR_TABLE_SCALE_SGI #x80D6)

(defconstant GL_COLOR_TABLE_BIAS_SGI #x80D7)

(defconstant GL_COLOR_TABLE_FORMAT_SGI #x80D8)

(defconstant GL_COLOR_TABLE_WIDTH_SGI #x80D9)

(defconstant GL_COLOR_TABLE_RED_SIZE_SGI #x80DA)

(defconstant GL_COLOR_TABLE_GREEN_SIZE_SGI #x80DB)

(defconstant GL_COLOR_TABLE_BLUE_SIZE_SGI #x80DC)

(defconstant GL_COLOR_TABLE_ALPHA_SIZE_SGI #x80DD)

(defconstant GL_COLOR_TABLE_LUMINANCE_SIZE_SGI #x80DE)

(defconstant GL_COLOR_TABLE_INTENSITY_SIZE_SGI #x80DF)

(defconstant GL_PIXEL_TEXTURE_SGIS #x8353)

(defconstant GL_PIXEL_FRAGMENT_RGB_SOURCE_SGIS #x8354)

(defconstant GL_PIXEL_FRAGMENT_ALPHA_SOURCE_SGIS #x8355)

(defconstant GL_PIXEL_GROUP_COLOR_SGIS #x8356)

(defconstant GL_PIXEL_TEX_GEN_SGIX #x8139)

(defconstant GL_PIXEL_TEX_GEN_MODE_SGIX #x832B)

(defconstant GL_PACK_SKIP_VOLUMES_SGIS #x8130)

(defconstant GL_PACK_IMAGE_DEPTH_SGIS #x8131)

(defconstant GL_UNPACK_SKIP_VOLUMES_SGIS #x8132)

(defconstant GL_UNPACK_IMAGE_DEPTH_SGIS #x8133)

(defconstant GL_TEXTURE_4D_SGIS #x8134)

(defconstant GL_PROXY_TEXTURE_4D_SGIS #x8135)

(defconstant GL_TEXTURE_4DSIZE_SGIS #x8136)

(defconstant GL_TEXTURE_WRAP_Q_SGIS #x8137)

(defconstant GL_MAX_4D_TEXTURE_SIZE_SGIS #x8138)

(defconstant GL_TEXTURE_4D_BINDING_SGIS #x814F)

(defconstant GL_TEXTURE_COLOR_TABLE_SGI #x80BC)

(defconstant GL_PROXY_TEXTURE_COLOR_TABLE_SGI #x80BD)

(defconstant GL_CMYK_EXT #x800C)

(defconstant GL_CMYKA_EXT #x800D)

(defconstant GL_PACK_CMYK_HINT_EXT #x800E)

(defconstant GL_UNPACK_CMYK_HINT_EXT #x800F)

(defconstant GL_TEXTURE_PRIORITY_EXT #x8066)

(defconstant GL_TEXTURE_RESIDENT_EXT #x8067)

(defconstant GL_TEXTURE_1D_BINDING_EXT #x8068)

(defconstant GL_TEXTURE_2D_BINDING_EXT #x8069)

(defconstant GL_TEXTURE_3D_BINDING_EXT #x806A)

(defconstant GL_DETAIL_TEXTURE_2D_SGIS #x8095)

(defconstant GL_DETAIL_TEXTURE_2D_BINDING_SGIS #x8096)

(defconstant GL_LINEAR_DETAIL_SGIS #x8097)

(defconstant GL_LINEAR_DETAIL_ALPHA_SGIS #x8098)

(defconstant GL_LINEAR_DETAIL_COLOR_SGIS #x8099)

(defconstant GL_DETAIL_TEXTURE_LEVEL_SGIS #x809A)

(defconstant GL_DETAIL_TEXTURE_MODE_SGIS #x809B)

(defconstant GL_DETAIL_TEXTURE_FUNC_POINTS_SGIS #x809C)

(defconstant GL_LINEAR_SHARPEN_SGIS #x80AD)

(defconstant GL_LINEAR_SHARPEN_ALPHA_SGIS #x80AE)

(defconstant GL_LINEAR_SHARPEN_COLOR_SGIS #x80AF)

(defconstant GL_SHARPEN_TEXTURE_FUNC_POINTS_SGIS #x80B0)

(defconstant GL_UNSIGNED_BYTE_3_3_2_EXT #x8032)

(defconstant GL_UNSIGNED_SHORT_4_4_4_4_EXT #x8033)

(defconstant GL_UNSIGNED_SHORT_5_5_5_1_EXT #x8034)

(defconstant GL_UNSIGNED_INT_8_8_8_8_EXT #x8035)

(defconstant GL_UNSIGNED_INT_10_10_10_2_EXT #x8036)

(defconstant GL_TEXTURE_MIN_LOD_SGIS #x813A)

(defconstant GL_TEXTURE_MAX_LOD_SGIS #x813B)

(defconstant GL_TEXTURE_BASE_LEVEL_SGIS #x813C)

(defconstant GL_TEXTURE_MAX_LEVEL_SGIS #x813D)

(defconstant GL_MULTISAMPLE_SGIS #x809D)

(defconstant GL_SAMPLE_ALPHA_TO_MASK_SGIS #x809E)

(defconstant GL_SAMPLE_ALPHA_TO_ONE_SGIS #x809F)

(defconstant GL_SAMPLE_MASK_SGIS #x80A0)

(defconstant GL_1PASS_SGIS #x80A1)

(defconstant GL_2PASS_0_SGIS #x80A2)

(defconstant GL_2PASS_1_SGIS #x80A3)

(defconstant GL_4PASS_0_SGIS #x80A4)

(defconstant GL_4PASS_1_SGIS #x80A5)

(defconstant GL_4PASS_2_SGIS #x80A6)

(defconstant GL_4PASS_3_SGIS #x80A7)

(defconstant GL_SAMPLE_BUFFERS_SGIS #x80A8)

(defconstant GL_SAMPLES_SGIS #x80A9)

(defconstant GL_SAMPLE_MASK_VALUE_SGIS #x80AA)

(defconstant GL_SAMPLE_MASK_INVERT_SGIS #x80AB)

(defconstant GL_SAMPLE_PATTERN_SGIS #x80AC)

(defconstant GL_RESCALE_NORMAL_EXT #x803A)

(defconstant GL_VERTEX_ARRAY_EXT #x8074)

(defconstant GL_NORMAL_ARRAY_EXT #x8075)

(defconstant GL_COLOR_ARRAY_EXT #x8076)

(defconstant GL_INDEX_ARRAY_EXT #x8077)

(defconstant GL_TEXTURE_COORD_ARRAY_EXT #x8078)

(defconstant GL_EDGE_FLAG_ARRAY_EXT #x8079)

(defconstant GL_VERTEX_ARRAY_SIZE_EXT #x807A)

(defconstant GL_VERTEX_ARRAY_TYPE_EXT #x807B)

(defconstant GL_VERTEX_ARRAY_STRIDE_EXT #x807C)

(defconstant GL_VERTEX_ARRAY_COUNT_EXT #x807D)

(defconstant GL_NORMAL_ARRAY_TYPE_EXT #x807E)

(defconstant GL_NORMAL_ARRAY_STRIDE_EXT #x807F)

(defconstant GL_NORMAL_ARRAY_COUNT_EXT #x8080)

(defconstant GL_COLOR_ARRAY_SIZE_EXT #x8081)

(defconstant GL_COLOR_ARRAY_TYPE_EXT #x8082)

(defconstant GL_COLOR_ARRAY_STRIDE_EXT #x8083)

(defconstant GL_COLOR_ARRAY_COUNT_EXT #x8084)

(defconstant GL_INDEX_ARRAY_TYPE_EXT #x8085)

(defconstant GL_INDEX_ARRAY_STRIDE_EXT #x8086)

(defconstant GL_INDEX_ARRAY_COUNT_EXT #x8087)

(defconstant GL_TEXTURE_COORD_ARRAY_SIZE_EXT #x8088)

(defconstant GL_TEXTURE_COORD_ARRAY_TYPE_EXT #x8089)

(defconstant GL_TEXTURE_COORD_ARRAY_STRIDE_EXT #x808A)

(defconstant GL_TEXTURE_COORD_ARRAY_COUNT_EXT #x808B)

(defconstant GL_EDGE_FLAG_ARRAY_STRIDE_EXT #x808C)

(defconstant GL_EDGE_FLAG_ARRAY_COUNT_EXT #x808D)

(defconstant GL_VERTEX_ARRAY_POINTER_EXT #x808E)

(defconstant GL_NORMAL_ARRAY_POINTER_EXT #x808F)

(defconstant GL_COLOR_ARRAY_POINTER_EXT #x8090)

(defconstant GL_INDEX_ARRAY_POINTER_EXT #x8091)

(defconstant GL_TEXTURE_COORD_ARRAY_POINTER_EXT #x8092)

(defconstant GL_EDGE_FLAG_ARRAY_POINTER_EXT #x8093)

(defconstant GL_GENERATE_MIPMAP_SGIS #x8191)

(defconstant GL_GENERATE_MIPMAP_HINT_SGIS #x8192)

(defconstant GL_LINEAR_CLIPMAP_LINEAR_SGIX #x8170)

(defconstant GL_TEXTURE_CLIPMAP_CENTER_SGIX #x8171)

(defconstant GL_TEXTURE_CLIPMAP_FRAME_SGIX #x8172)

(defconstant GL_TEXTURE_CLIPMAP_OFFSET_SGIX #x8173)

(defconstant GL_TEXTURE_CLIPMAP_VIRTUAL_DEPTH_SGIX #x8174)

(defconstant GL_TEXTURE_CLIPMAP_LOD_OFFSET_SGIX #x8175)

(defconstant GL_TEXTURE_CLIPMAP_DEPTH_SGIX #x8176)

(defconstant GL_MAX_CLIPMAP_DEPTH_SGIX #x8177)

(defconstant GL_MAX_CLIPMAP_VIRTUAL_DEPTH_SGIX #x8178)

(defconstant GL_NEAREST_CLIPMAP_NEAREST_SGIX #x844D)

(defconstant GL_NEAREST_CLIPMAP_LINEAR_SGIX #x844E)

(defconstant GL_LINEAR_CLIPMAP_NEAREST_SGIX #x844F)

(defconstant GL_TEXTURE_COMPARE_SGIX #x819A)

(defconstant GL_TEXTURE_COMPARE_OPERATOR_SGIX #x819B)

(defconstant GL_TEXTURE_LEQUAL_R_SGIX #x819C)

(defconstant GL_TEXTURE_GEQUAL_R_SGIX #x819D)

(defconstant GL_CLAMP_TO_EDGE_SGIS #x812F)

(defconstant GL_FUNC_ADD_EXT #x8006)

(defconstant GL_MIN_EXT #x8007)

(defconstant GL_MAX_EXT #x8008)

(defconstant GL_BLEND_EQUATION_EXT #x8009)

(defconstant GL_FUNC_SUBTRACT_EXT #x800A)

(defconstant GL_FUNC_REVERSE_SUBTRACT_EXT #x800B)

(defconstant GL_INTERLACE_SGIX #x8094)

(defconstant GL_PIXEL_TILE_BEST_ALIGNMENT_SGIX #x813E)

(defconstant GL_PIXEL_TILE_CACHE_INCREMENT_SGIX #x813F)

(defconstant GL_PIXEL_TILE_WIDTH_SGIX #x8140)

(defconstant GL_PIXEL_TILE_HEIGHT_SGIX #x8141)

(defconstant GL_PIXEL_TILE_GRID_WIDTH_SGIX #x8142)

(defconstant GL_PIXEL_TILE_GRID_HEIGHT_SGIX #x8143)

(defconstant GL_PIXEL_TILE_GRID_DEPTH_SGIX #x8144)

(defconstant GL_PIXEL_TILE_CACHE_SIZE_SGIX #x8145)

(defconstant GL_DUAL_ALPHA4_SGIS #x8110)

(defconstant GL_DUAL_ALPHA8_SGIS #x8111)

(defconstant GL_DUAL_ALPHA12_SGIS #x8112)

(defconstant GL_DUAL_ALPHA16_SGIS #x8113)

(defconstant GL_DUAL_LUMINANCE4_SGIS #x8114)

(defconstant GL_DUAL_LUMINANCE8_SGIS #x8115)

(defconstant GL_DUAL_LUMINANCE12_SGIS #x8116)

(defconstant GL_DUAL_LUMINANCE16_SGIS #x8117)

(defconstant GL_DUAL_INTENSITY4_SGIS #x8118)

(defconstant GL_DUAL_INTENSITY8_SGIS #x8119)

(defconstant GL_DUAL_INTENSITY12_SGIS #x811A)

(defconstant GL_DUAL_INTENSITY16_SGIS #x811B)

(defconstant GL_DUAL_LUMINANCE_ALPHA4_SGIS #x811C)

(defconstant GL_DUAL_LUMINANCE_ALPHA8_SGIS #x811D)

(defconstant GL_QUAD_ALPHA4_SGIS #x811E)

(defconstant GL_QUAD_ALPHA8_SGIS #x811F)

(defconstant GL_QUAD_LUMINANCE4_SGIS #x8120)

(defconstant GL_QUAD_LUMINANCE8_SGIS #x8121)

(defconstant GL_QUAD_INTENSITY4_SGIS #x8122)

(defconstant GL_QUAD_INTENSITY8_SGIS #x8123)

(defconstant GL_DUAL_TEXTURE_SELECT_SGIS #x8124)

(defconstant GL_QUAD_TEXTURE_SELECT_SGIS #x8125)

(defconstant GL_SPRITE_SGIX #x8148)

(defconstant GL_SPRITE_MODE_SGIX #x8149)

(defconstant GL_SPRITE_AXIS_SGIX #x814A)

(defconstant GL_SPRITE_TRANSLATION_SGIX #x814B)

(defconstant GL_SPRITE_AXIAL_SGIX #x814C)

(defconstant GL_SPRITE_OBJECT_ALIGNED_SGIX #x814D)

(defconstant GL_SPRITE_EYE_ALIGNED_SGIX #x814E)

(defconstant GL_TEXTURE_MULTI_BUFFER_HINT_SGIX #x812E)

(defconstant GL_POINT_SIZE_MIN_EXT #x8126)

(defconstant GL_POINT_SIZE_MAX_EXT #x8127)

(defconstant GL_POINT_FADE_THRESHOLD_SIZE_EXT #x8128)

(defconstant GL_DISTANCE_ATTENUATION_EXT #x8129)

(defconstant GL_POINT_SIZE_MIN_SGIS #x8126)

(defconstant GL_POINT_SIZE_MAX_SGIS #x8127)

(defconstant GL_POINT_FADE_THRESHOLD_SIZE_SGIS #x8128)

(defconstant GL_DISTANCE_ATTENUATION_SGIS #x8129)

(defconstant GL_INSTRUMENT_BUFFER_POINTER_SGIX #x8180)

(defconstant GL_INSTRUMENT_MEASUREMENTS_SGIX #x8181)

(defconstant GL_POST_TEXTURE_FILTER_BIAS_SGIX #x8179)

(defconstant GL_POST_TEXTURE_FILTER_SCALE_SGIX #x817A)

(defconstant GL_POST_TEXTURE_FILTER_BIAS_RANGE_SGIX #x817B)

(defconstant GL_POST_TEXTURE_FILTER_SCALE_RANGE_SGIX #x817C)

(defconstant GL_FRAMEZOOM_SGIX #x818B)

(defconstant GL_FRAMEZOOM_FACTOR_SGIX #x818C)

(defconstant GL_MAX_FRAMEZOOM_FACTOR_SGIX #x818D)

(defconstant GL_TEXTURE_DEFORMATION_BIT_SGIX #x00000001)

(defconstant GL_GEOMETRY_DEFORMATION_BIT_SGIX #x00000002)

(defconstant GL_GEOMETRY_DEFORMATION_SGIX #x8194)

(defconstant GL_TEXTURE_DEFORMATION_SGIX #x8195)

(defconstant GL_DEFORMATIONS_MASK_SGIX #x8196)

(defconstant GL_MAX_DEFORMATION_ORDER_SGIX #x8197)

(defconstant GL_REFERENCE_PLANE_SGIX #x817D)

(defconstant GL_REFERENCE_PLANE_EQUATION_SGIX #x817E)

(defconstant GL_DEPTH_COMPONENT16_SGIX #x81A5)

(defconstant GL_DEPTH_COMPONENT24_SGIX #x81A6)

(defconstant GL_DEPTH_COMPONENT32_SGIX #x81A7)

(defconstant GL_FOG_FUNC_SGIS #x812A)

(defconstant GL_FOG_FUNC_POINTS_SGIS #x812B)

(defconstant GL_MAX_FOG_FUNC_POINTS_SGIS #x812C)

(defconstant GL_FOG_OFFSET_SGIX #x8198)

(defconstant GL_FOG_OFFSET_VALUE_SGIX #x8199)

(defconstant GL_IMAGE_SCALE_X_HP #x8155)

(defconstant GL_IMAGE_SCALE_Y_HP #x8156)

(defconstant GL_IMAGE_TRANSLATE_X_HP #x8157)

(defconstant GL_IMAGE_TRANSLATE_Y_HP #x8158)

(defconstant GL_IMAGE_ROTATE_ANGLE_HP #x8159)

(defconstant GL_IMAGE_ROTATE_ORIGIN_X_HP #x815A)

(defconstant GL_IMAGE_ROTATE_ORIGIN_Y_HP #x815B)

(defconstant GL_IMAGE_MAG_FILTER_HP #x815C)

(defconstant GL_IMAGE_MIN_FILTER_HP #x815D)

(defconstant GL_IMAGE_CUBIC_WEIGHT_HP #x815E)

(defconstant GL_CUBIC_HP #x815F)

(defconstant GL_AVERAGE_HP #x8160)

(defconstant GL_IMAGE_TRANSFORM_2D_HP #x8161)

(defconstant GL_POST_IMAGE_TRANSFORM_COLOR_TABLE_HP #x8162)

(defconstant GL_PROXY_POST_IMAGE_TRANSFORM_COLOR_TABLE_HP #x8163)

(defconstant GL_IGNORE_BORDER_HP #x8150)

(defconstant GL_CONSTANT_BORDER_HP #x8151)

(defconstant GL_REPLICATE_BORDER_HP #x8153)

(defconstant GL_CONVOLUTION_BORDER_COLOR_HP #x8154)

(defconstant GL_TEXTURE_ENV_BIAS_SGIX #x80BE)

(defconstant GL_VERTEX_DATA_HINT_PGI #x1A22A)

(defconstant GL_VERTEX_CONSISTENT_HINT_PGI #x1A22B)

(defconstant GL_MATERIAL_SIDE_HINT_PGI #x1A22C)

(defconstant GL_MAX_VERTEX_HINT_PGI #x1A22D)

(defconstant GL_COLOR3_BIT_PGI #x00010000)

(defconstant GL_COLOR4_BIT_PGI #x00020000)

(defconstant GL_EDGEFLAG_BIT_PGI #x00040000)

(defconstant GL_INDEX_BIT_PGI #x00080000)

(defconstant GL_MAT_AMBIENT_BIT_PGI #x00100000)

(defconstant GL_MAT_AMBIENT_AND_DIFFUSE_BIT_PGI #x00200000)

(defconstant GL_MAT_DIFFUSE_BIT_PGI #x00400000)

(defconstant GL_MAT_EMISSION_BIT_PGI #x00800000)

(defconstant GL_MAT_COLOR_INDEXES_BIT_PGI #x01000000)

(defconstant GL_MAT_SHININESS_BIT_PGI #x02000000)

(defconstant GL_MAT_SPECULAR_BIT_PGI #x04000000)

(defconstant GL_NORMAL_BIT_PGI #x08000000)

(defconstant GL_TEXCOORD1_BIT_PGI #x10000000)

(defconstant GL_TEXCOORD2_BIT_PGI #x20000000)

(defconstant GL_TEXCOORD3_BIT_PGI #x40000000)

(defconstant GL_TEXCOORD4_BIT_PGI #x80000000)

(defconstant GL_VERTEX23_BIT_PGI #x00000004)

(defconstant GL_VERTEX4_BIT_PGI #x00000008)

(defconstant GL_PREFER_DOUBLEBUFFER_HINT_PGI #x1A1F8)

(defconstant GL_CONSERVE_MEMORY_HINT_PGI #x1A1FD)

(defconstant GL_RECLAIM_MEMORY_HINT_PGI #x1A1FE)

(defconstant GL_NATIVE_GRAPHICS_HANDLE_PGI #x1A202)

(defconstant GL_NATIVE_GRAPHICS_BEGIN_HINT_PGI #x1A203)

(defconstant GL_NATIVE_GRAPHICS_END_HINT_PGI #x1A204)

(defconstant GL_ALWAYS_FAST_HINT_PGI #x1A20C)

(defconstant GL_ALWAYS_SOFT_HINT_PGI #x1A20D)

(defconstant GL_ALLOW_DRAW_OBJ_HINT_PGI #x1A20E)

(defconstant GL_ALLOW_DRAW_WIN_HINT_PGI #x1A20F)

(defconstant GL_ALLOW_DRAW_FRG_HINT_PGI #x1A210)

(defconstant GL_ALLOW_DRAW_MEM_HINT_PGI #x1A211)

(defconstant GL_STRICT_DEPTHFUNC_HINT_PGI #x1A216)

(defconstant GL_STRICT_LIGHTING_HINT_PGI #x1A217)

(defconstant GL_STRICT_SCISSOR_HINT_PGI #x1A218)

(defconstant GL_FULL_STIPPLE_HINT_PGI #x1A219)

(defconstant GL_CLIP_NEAR_HINT_PGI #x1A220)

(defconstant GL_CLIP_FAR_HINT_PGI #x1A221)

(defconstant GL_WIDE_LINE_HINT_PGI #x1A222)

(defconstant GL_BACK_NORMALS_HINT_PGI #x1A223)

(defconstant GL_COLOR_INDEX1_EXT #x80E2)

(defconstant GL_COLOR_INDEX2_EXT #x80E3)

(defconstant GL_COLOR_INDEX4_EXT #x80E4)

(defconstant GL_COLOR_INDEX8_EXT #x80E5)

(defconstant GL_COLOR_INDEX12_EXT #x80E6)

(defconstant GL_COLOR_INDEX16_EXT #x80E7)

(defconstant GL_TEXTURE_INDEX_SIZE_EXT #x80ED)

(defconstant GL_CLIP_VOLUME_CLIPPING_HINT_EXT #x80F0)

(defconstant GL_LIST_PRIORITY_SGIX #x8182)

(defconstant GL_IR_INSTRUMENT1_SGIX #x817F)

(defconstant GL_CALLIGRAPHIC_FRAGMENT_SGIX #x8183)

(defconstant GL_TEXTURE_LOD_BIAS_S_SGIX #x818E)

(defconstant GL_TEXTURE_LOD_BIAS_T_SGIX #x818F)

(defconstant GL_TEXTURE_LOD_BIAS_R_SGIX #x8190)

(defconstant GL_SHADOW_AMBIENT_SGIX #x80BF)

(defconstant GL_INDEX_MATERIAL_EXT #x81B8)

(defconstant GL_INDEX_MATERIAL_PARAMETER_EXT #x81B9)

(defconstant GL_INDEX_MATERIAL_FACE_EXT #x81BA)

(defconstant GL_INDEX_TEST_EXT #x81B5)

(defconstant GL_INDEX_TEST_FUNC_EXT #x81B6)

(defconstant GL_INDEX_TEST_REF_EXT #x81B7)

(defconstant GL_IUI_V2F_EXT #x81AD)

(defconstant GL_IUI_V3F_EXT #x81AE)

(defconstant GL_IUI_N3F_V2F_EXT #x81AF)

(defconstant GL_IUI_N3F_V3F_EXT #x81B0)

(defconstant GL_T2F_IUI_V2F_EXT #x81B1)

(defconstant GL_T2F_IUI_V3F_EXT #x81B2)

(defconstant GL_T2F_IUI_N3F_V2F_EXT #x81B3)

(defconstant GL_T2F_IUI_N3F_V3F_EXT #x81B4)

(defconstant GL_ARRAY_ELEMENT_LOCK_FIRST_EXT #x81A8)

(defconstant GL_ARRAY_ELEMENT_LOCK_COUNT_EXT #x81A9)

(defconstant GL_CULL_VERTEX_EXT #x81AA)

(defconstant GL_CULL_VERTEX_EYE_POSITION_EXT #x81AB)

(defconstant GL_CULL_VERTEX_OBJECT_POSITION_EXT #x81AC)

(defconstant GL_YCRCB_422_SGIX #x81BB)

(defconstant GL_YCRCB_444_SGIX #x81BC)

(defconstant GL_FRAGMENT_LIGHTING_SGIX #x8400)

(defconstant GL_FRAGMENT_COLOR_MATERIAL_SGIX #x8401)

(defconstant GL_FRAGMENT_COLOR_MATERIAL_FACE_SGIX #x8402)

(defconstant GL_FRAGMENT_COLOR_MATERIAL_PARAMETER_SGIX #x8403)

(defconstant GL_MAX_FRAGMENT_LIGHTS_SGIX #x8404)

(defconstant GL_MAX_ACTIVE_LIGHTS_SGIX #x8405)

(defconstant GL_CURRENT_RASTER_NORMAL_SGIX #x8406)

(defconstant GL_LIGHT_ENV_MODE_SGIX #x8407)

(defconstant GL_FRAGMENT_LIGHT_MODEL_LOCAL_VIEWER_SGIX #x8408)

(defconstant GL_FRAGMENT_LIGHT_MODEL_TWO_SIDE_SGIX #x8409)

(defconstant GL_FRAGMENT_LIGHT_MODEL_AMBIENT_SGIX #x840A)

(defconstant GL_FRAGMENT_LIGHT_MODEL_NORMAL_INTERPOLATION_SGIX #x840B)

(defconstant GL_FRAGMENT_LIGHT0_SGIX #x840C)

(defconstant GL_FRAGMENT_LIGHT1_SGIX #x840D)

(defconstant GL_FRAGMENT_LIGHT2_SGIX #x840E)

(defconstant GL_FRAGMENT_LIGHT3_SGIX #x840F)

(defconstant GL_FRAGMENT_LIGHT4_SGIX #x8410)

(defconstant GL_FRAGMENT_LIGHT5_SGIX #x8411)

(defconstant GL_FRAGMENT_LIGHT6_SGIX #x8412)

(defconstant GL_FRAGMENT_LIGHT7_SGIX #x8413)

(defconstant GL_RASTER_POSITION_UNCLIPPED_IBM #x19262)

(defconstant GL_TEXTURE_LIGHTING_MODE_HP #x8167)

(defconstant GL_TEXTURE_POST_SPECULAR_HP #x8168)

(defconstant GL_TEXTURE_PRE_SPECULAR_HP #x8169)

(defconstant GL_MAX_ELEMENTS_VERTICES_EXT #x80E8)

(defconstant GL_MAX_ELEMENTS_INDICES_EXT #x80E9)

(defconstant GL_PHONG_WIN #x80EA)

(defconstant GL_PHONG_HINT_WIN #x80EB)

(defconstant GL_FOG_SPECULAR_TEXTURE_WIN #x80EC)

(defconstant GL_FRAGMENT_MATERIAL_EXT #x8349)

(defconstant GL_FRAGMENT_NORMAL_EXT #x834A)

(defconstant GL_FRAGMENT_COLOR_EXT #x834C)

(defconstant GL_ATTENUATION_EXT #x834D)

(defconstant GL_SHADOW_ATTENUATION_EXT #x834E)

(defconstant GL_TEXTURE_APPLICATION_MODE_EXT #x834F)

(defconstant GL_TEXTURE_LIGHT_EXT #x8350)

(defconstant GL_TEXTURE_MATERIAL_FACE_EXT #x8351)

(defconstant GL_TEXTURE_MATERIAL_PARAMETER_EXT #x8352)

(defconstant GL_ALPHA_MIN_SGIX #x8320)

(defconstant GL_ALPHA_MAX_SGIX #x8321)

(defconstant GL_PIXEL_TEX_GEN_Q_CEILING_SGIX #x8184)

(defconstant GL_PIXEL_TEX_GEN_Q_ROUND_SGIX #x8185)

(defconstant GL_PIXEL_TEX_GEN_Q_FLOOR_SGIX #x8186)

(defconstant GL_PIXEL_TEX_GEN_ALPHA_REPLACE_SGIX #x8187)

(defconstant GL_PIXEL_TEX_GEN_ALPHA_NO_REPLACE_SGIX #x8188)

(defconstant GL_PIXEL_TEX_GEN_ALPHA_LS_SGIX #x8189)

(defconstant GL_PIXEL_TEX_GEN_ALPHA_MS_SGIX #x818A)

(defconstant GL_BGR_EXT #x80E0)

(defconstant GL_BGRA_EXT #x80E1)

(defconstant GL_ASYNC_MARKER_SGIX #x8329)

(defconstant GL_ASYNC_TEX_IMAGE_SGIX #x835C)

(defconstant GL_ASYNC_DRAW_PIXELS_SGIX #x835D)

(defconstant GL_ASYNC_READ_PIXELS_SGIX #x835E)

(defconstant GL_MAX_ASYNC_TEX_IMAGE_SGIX #x835F)

(defconstant GL_MAX_ASYNC_DRAW_PIXELS_SGIX #x8360)

(defconstant GL_MAX_ASYNC_READ_PIXELS_SGIX #x8361)

(defconstant GL_ASYNC_HISTOGRAM_SGIX #x832C)

(defconstant GL_MAX_ASYNC_HISTOGRAM_SGIX #x832D)

(defconstant GL_PARALLEL_ARRAYS_INTEL #x83F4)

(defconstant GL_VERTEX_ARRAY_PARALLEL_POINTERS_INTEL #x83F5)

(defconstant GL_NORMAL_ARRAY_PARALLEL_POINTERS_INTEL #x83F6)

(defconstant GL_COLOR_ARRAY_PARALLEL_POINTERS_INTEL #x83F7)

(defconstant GL_TEXTURE_COORD_ARRAY_PARALLEL_POINTERS_INTEL #x83F8)

(defconstant GL_OCCLUSION_TEST_HP #x8165)

(defconstant GL_OCCLUSION_TEST_RESULT_HP #x8166)

(defconstant GL_PIXEL_TRANSFORM_2D_EXT #x8330)

(defconstant GL_PIXEL_MAG_FILTER_EXT #x8331)

(defconstant GL_PIXEL_MIN_FILTER_EXT #x8332)

(defconstant GL_PIXEL_CUBIC_WEIGHT_EXT #x8333)

(defconstant GL_CUBIC_EXT #x8334)

(defconstant GL_AVERAGE_EXT #x8335)

(defconstant GL_PIXEL_TRANSFORM_2D_STACK_DEPTH_EXT #x8336)

(defconstant GL_MAX_PIXEL_TRANSFORM_2D_STACK_DEPTH_EXT #x8337)

(defconstant GL_PIXEL_TRANSFORM_2D_MATRIX_EXT #x8338)

(defconstant GL_SHARED_TEXTURE_PALETTE_EXT #x81FB)

(defconstant GL_LIGHT_MODEL_COLOR_CONTROL_EXT #x81F8)

(defconstant GL_SINGLE_COLOR_EXT #x81F9)

(defconstant GL_SEPARATE_SPECULAR_COLOR_EXT #x81FA)

(defconstant GL_COLOR_SUM_EXT #x8458)

(defconstant GL_CURRENT_SECONDARY_COLOR_EXT #x8459)

(defconstant GL_SECONDARY_COLOR_ARRAY_SIZE_EXT #x845A)

(defconstant GL_SECONDARY_COLOR_ARRAY_TYPE_EXT #x845B)

(defconstant GL_SECONDARY_COLOR_ARRAY_STRIDE_EXT #x845C)

(defconstant GL_SECONDARY_COLOR_ARRAY_POINTER_EXT #x845D)

(defconstant GL_SECONDARY_COLOR_ARRAY_EXT #x845E)

(defconstant GL_PERTURB_EXT #x85AE)

(defconstant GL_TEXTURE_NORMAL_EXT #x85AF)

(defconstant GL_FOG_COORDINATE_SOURCE_EXT #x8450)

(defconstant GL_FOG_COORDINATE_EXT #x8451)

(defconstant GL_FRAGMENT_DEPTH_EXT #x8452)

(defconstant GL_CURRENT_FOG_COORDINATE_EXT #x8453)

(defconstant GL_FOG_COORDINATE_ARRAY_TYPE_EXT #x8454)

(defconstant GL_FOG_COORDINATE_ARRAY_STRIDE_EXT #x8455)

(defconstant GL_FOG_COORDINATE_ARRAY_POINTER_EXT #x8456)

(defconstant GL_FOG_COORDINATE_ARRAY_EXT #x8457)

(defconstant GL_SCREEN_COORDINATES_REND #x8490)

(defconstant GL_INVERTED_SCREEN_W_REND #x8491)

(defconstant GL_TANGENT_ARRAY_EXT #x8439)

(defconstant GL_BINORMAL_ARRAY_EXT #x843A)

(defconstant GL_CURRENT_TANGENT_EXT #x843B)

(defconstant GL_CURRENT_BINORMAL_EXT #x843C)

(defconstant GL_TANGENT_ARRAY_TYPE_EXT #x843E)

(defconstant GL_TANGENT_ARRAY_STRIDE_EXT #x843F)

(defconstant GL_BINORMAL_ARRAY_TYPE_EXT #x8440)

(defconstant GL_BINORMAL_ARRAY_STRIDE_EXT #x8441)

(defconstant GL_TANGENT_ARRAY_POINTER_EXT #x8442)

(defconstant GL_BINORMAL_ARRAY_POINTER_EXT #x8443)

(defconstant GL_MAP1_TANGENT_EXT #x8444)

(defconstant GL_MAP2_TANGENT_EXT #x8445)

(defconstant GL_MAP1_BINORMAL_EXT #x8446)

(defconstant GL_MAP2_BINORMAL_EXT #x8447)

(defconstant GL_COMBINE_EXT #x8570)

(defconstant GL_COMBINE_RGB_EXT #x8571)

(defconstant GL_COMBINE_ALPHA_EXT #x8572)

(defconstant GL_RGB_SCALE_EXT #x8573)

(defconstant GL_ADD_SIGNED_EXT #x8574)

(defconstant GL_INTERPOLATE_EXT #x8575)

(defconstant GL_CONSTANT_EXT #x8576)

(defconstant GL_PRIMARY_COLOR_EXT #x8577)

(defconstant GL_PREVIOUS_EXT #x8578)

(defconstant GL_SOURCE0_RGB_EXT #x8580)

(defconstant GL_SOURCE1_RGB_EXT #x8581)

(defconstant GL_SOURCE2_RGB_EXT #x8582)

(defconstant GL_SOURCE0_ALPHA_EXT #x8588)

(defconstant GL_SOURCE1_ALPHA_EXT #x8589)

(defconstant GL_SOURCE2_ALPHA_EXT #x858A)

(defconstant GL_OPERAND0_RGB_EXT #x8590)

(defconstant GL_OPERAND1_RGB_EXT #x8591)

(defconstant GL_OPERAND2_RGB_EXT #x8592)

(defconstant GL_OPERAND0_ALPHA_EXT #x8598)

(defconstant GL_OPERAND1_ALPHA_EXT #x8599)

(defconstant GL_OPERAND2_ALPHA_EXT #x859A)

(defconstant GL_LIGHT_MODEL_SPECULAR_VECTOR_APPLE #x85B0)

(defconstant GL_TRANSFORM_HINT_APPLE #x85B1)

(defconstant GL_FOG_SCALE_SGIX #x81FC)

(defconstant GL_FOG_SCALE_VALUE_SGIX #x81FD)

(defconstant GL_UNPACK_CONSTANT_DATA_SUNX #x81D5)

(defconstant GL_TEXTURE_CONSTANT_DATA_SUNX #x81D6)

(defconstant GL_GLOBAL_ALPHA_SUN #x81D9)

(defconstant GL_GLOBAL_ALPHA_FACTOR_SUN #x81DA)

(defconstant GL_RESTART_SUN #x0001)

(defconstant GL_REPLACE_MIDDLE_SUN #x0002)

(defconstant GL_REPLACE_OLDEST_SUN #x0003)

(defconstant GL_TRIANGLE_LIST_SUN #x81D7)

(defconstant GL_REPLACEMENT_CODE_SUN #x81D8)

(defconstant GL_REPLACEMENT_CODE_ARRAY_SUN #x85C0)

(defconstant GL_REPLACEMENT_CODE_ARRAY_TYPE_SUN #x85C1)

(defconstant GL_REPLACEMENT_CODE_ARRAY_STRIDE_SUN #x85C2)

(defconstant GL_REPLACEMENT_CODE_ARRAY_POINTER_SUN #x85C3)

(defconstant GL_R1UI_V3F_SUN #x85C4)

(defconstant GL_R1UI_C4UB_V3F_SUN #x85C5)

(defconstant GL_R1UI_C3F_V3F_SUN #x85C6)

(defconstant GL_R1UI_N3F_V3F_SUN #x85C7)

(defconstant GL_R1UI_C4F_N3F_V3F_SUN #x85C8)

(defconstant GL_R1UI_T2F_V3F_SUN #x85C9)

(defconstant GL_R1UI_T2F_N3F_V3F_SUN #x85CA)

(defconstant GL_R1UI_T2F_C4F_N3F_V3F_SUN #x85CB)

(defconstant GL_BLEND_DST_RGB_EXT #x80C8)

(defconstant GL_BLEND_SRC_RGB_EXT #x80C9)

(defconstant GL_BLEND_DST_ALPHA_EXT #x80CA)

(defconstant GL_BLEND_SRC_ALPHA_EXT #x80CB)

(defconstant GL_RED_MIN_CLAMP_INGR #x8560)

(defconstant GL_GREEN_MIN_CLAMP_INGR #x8561)

(defconstant GL_BLUE_MIN_CLAMP_INGR #x8562)

(defconstant GL_ALPHA_MIN_CLAMP_INGR #x8563)

(defconstant GL_RED_MAX_CLAMP_INGR #x8564)

(defconstant GL_GREEN_MAX_CLAMP_INGR #x8565)

(defconstant GL_BLUE_MAX_CLAMP_INGR #x8566)

(defconstant GL_ALPHA_MAX_CLAMP_INGR #x8567)

(defconstant GL_INTERLACE_READ_INGR #x8568)

(defconstant GL_INCR_WRAP_EXT #x8507)

(defconstant GL_DECR_WRAP_EXT #x8508)

(defconstant GL_422_EXT #x80CC)

(defconstant GL_422_REV_EXT #x80CD)

(defconstant GL_422_AVERAGE_EXT #x80CE)

(defconstant GL_422_REV_AVERAGE_EXT #x80CF)

(defconstant GL_NORMAL_MAP_NV #x8511)

(defconstant GL_REFLECTION_MAP_NV #x8512)

(defconstant GL_NORMAL_MAP_EXT #x8511)

(defconstant GL_REFLECTION_MAP_EXT #x8512)

(defconstant GL_TEXTURE_CUBE_MAP_EXT #x8513)

(defconstant GL_TEXTURE_BINDING_CUBE_MAP_EXT #x8514)

(defconstant GL_TEXTURE_CUBE_MAP_POSITIVE_X_EXT #x8515)

(defconstant GL_TEXTURE_CUBE_MAP_NEGATIVE_X_EXT #x8516)

(defconstant GL_TEXTURE_CUBE_MAP_POSITIVE_Y_EXT #x8517)

(defconstant GL_TEXTURE_CUBE_MAP_NEGATIVE_Y_EXT #x8518)

(defconstant GL_TEXTURE_CUBE_MAP_POSITIVE_Z_EXT #x8519)

(defconstant GL_TEXTURE_CUBE_MAP_NEGATIVE_Z_EXT #x851A)

(defconstant GL_PROXY_TEXTURE_CUBE_MAP_EXT #x851B)

(defconstant GL_MAX_CUBE_MAP_TEXTURE_SIZE_EXT #x851C)

(defconstant GL_WRAP_BORDER_SUN #x81D4)

(defconstant GL_MAX_TEXTURE_LOD_BIAS_EXT #x84FD)

(defconstant GL_TEXTURE_FILTER_CONTROL_EXT #x8500)

(defconstant GL_TEXTURE_LOD_BIAS_EXT #x8501)

(defconstant GL_TEXTURE_MAX_ANISOTROPY_EXT #x84FE)

(defconstant GL_MAX_TEXTURE_MAX_ANISOTROPY_EXT #x84FF)

(defconstant GL_MODELVIEW1_STACK_DEPTH_EXT #x8502)

(defconstant GL_MODELVIEW1_MATRIX_EXT #x8506)

(defconstant GL_VERTEX_WEIGHTING_EXT #x8509)

(defconstant GL_MODELVIEW1_EXT #x850A)

(defconstant GL_CURRENT_VERTEX_WEIGHT_EXT #x850B)

(defconstant GL_VERTEX_WEIGHT_ARRAY_EXT #x850C)

(defconstant GL_VERTEX_WEIGHT_ARRAY_SIZE_EXT #x850D)

(defconstant GL_VERTEX_WEIGHT_ARRAY_TYPE_EXT #x850E)

(defconstant GL_VERTEX_WEIGHT_ARRAY_STRIDE_EXT #x850F)

(defconstant GL_VERTEX_WEIGHT_ARRAY_POINTER_EXT #x8510)

(defconstant GL_MAX_SHININESS_NV #x8504)

(defconstant GL_MAX_SPOT_EXPONENT_NV #x8505)

(defconstant GL_VERTEX_ARRAY_RANGE_NV #x851D)

(defconstant GL_VERTEX_ARRAY_RANGE_LENGTH_NV #x851E)

(defconstant GL_VERTEX_ARRAY_RANGE_VALID_NV #x851F)

(defconstant GL_MAX_VERTEX_ARRAY_RANGE_ELEMENT_NV #x8520)

(defconstant GL_VERTEX_ARRAY_RANGE_POINTER_NV #x8521)

(defconstant GL_REGISTER_COMBINERS_NV #x8522)

(defconstant GL_VARIABLE_A_NV #x8523)

(defconstant GL_VARIABLE_B_NV #x8524)

(defconstant GL_VARIABLE_C_NV #x8525)

(defconstant GL_VARIABLE_D_NV #x8526)

(defconstant GL_VARIABLE_E_NV #x8527)

(defconstant GL_VARIABLE_F_NV #x8528)

(defconstant GL_VARIABLE_G_NV #x8529)

(defconstant GL_CONSTANT_COLOR0_NV #x852A)

(defconstant GL_CONSTANT_COLOR1_NV #x852B)

(defconstant GL_PRIMARY_COLOR_NV #x852C)

(defconstant GL_SECONDARY_COLOR_NV #x852D)

(defconstant GL_SPARE0_NV #x852E)

(defconstant GL_SPARE1_NV #x852F)

(defconstant GL_DISCARD_NV #x8530)

(defconstant GL_E_TIMES_F_NV #x8531)

(defconstant GL_SPARE0_PLUS_SECONDARY_COLOR_NV #x8532)

(defconstant GL_UNSIGNED_IDENTITY_NV #x8536)

(defconstant GL_UNSIGNED_INVERT_NV #x8537)

(defconstant GL_EXPAND_NORMAL_NV #x8538)

(defconstant GL_EXPAND_NEGATE_NV #x8539)

(defconstant GL_HALF_BIAS_NORMAL_NV #x853A)

(defconstant GL_HALF_BIAS_NEGATE_NV #x853B)

(defconstant GL_SIGNED_IDENTITY_NV #x853C)

(defconstant GL_SIGNED_NEGATE_NV #x853D)

(defconstant GL_SCALE_BY_TWO_NV #x853E)

(defconstant GL_SCALE_BY_FOUR_NV #x853F)

(defconstant GL_SCALE_BY_ONE_HALF_NV #x8540)

(defconstant GL_BIAS_BY_NEGATIVE_ONE_HALF_NV #x8541)

(defconstant GL_COMBINER_INPUT_NV #x8542)

(defconstant GL_COMBINER_MAPPING_NV #x8543)

(defconstant GL_COMBINER_COMPONENT_USAGE_NV #x8544)

(defconstant GL_COMBINER_AB_DOT_PRODUCT_NV #x8545)

(defconstant GL_COMBINER_CD_DOT_PRODUCT_NV #x8546)

(defconstant GL_COMBINER_MUX_SUM_NV #x8547)

(defconstant GL_COMBINER_SCALE_NV #x8548)

(defconstant GL_COMBINER_BIAS_NV #x8549)

(defconstant GL_COMBINER_AB_OUTPUT_NV #x854A)

(defconstant GL_COMBINER_CD_OUTPUT_NV #x854B)

(defconstant GL_COMBINER_SUM_OUTPUT_NV #x854C)

(defconstant GL_MAX_GENERAL_COMBINERS_NV #x854D)

(defconstant GL_NUM_GENERAL_COMBINERS_NV #x854E)

(defconstant GL_COLOR_SUM_CLAMP_NV #x854F)

(defconstant GL_COMBINER0_NV #x8550)

(defconstant GL_COMBINER1_NV #x8551)

(defconstant GL_COMBINER2_NV #x8552)

(defconstant GL_COMBINER3_NV #x8553)

(defconstant GL_COMBINER4_NV #x8554)

(defconstant GL_COMBINER5_NV #x8555)

(defconstant GL_COMBINER6_NV #x8556)

(defconstant GL_COMBINER7_NV #x8557)

(defconstant GL_FOG_DISTANCE_MODE_NV #x855A)

(defconstant GL_EYE_RADIAL_NV #x855B)

(defconstant GL_EYE_PLANE_ABSOLUTE_NV #x855C)

(defconstant GL_EMBOSS_LIGHT_NV #x855D)

(defconstant GL_EMBOSS_CONSTANT_NV #x855E)

(defconstant GL_EMBOSS_MAP_NV #x855F)

(defconstant GL_COMBINE4_NV #x8503)

(defconstant GL_SOURCE3_RGB_NV #x8583)

(defconstant GL_SOURCE3_ALPHA_NV #x858B)

(defconstant GL_OPERAND3_RGB_NV #x8593)

(defconstant GL_OPERAND3_ALPHA_NV #x859B)

(defconstant GL_COMPRESSED_RGB_S3TC_DXT1_EXT #x83F0)

(defconstant GL_COMPRESSED_RGBA_S3TC_DXT1_EXT #x83F1)

(defconstant GL_COMPRESSED_RGBA_S3TC_DXT3_EXT #x83F2)

(defconstant GL_COMPRESSED_RGBA_S3TC_DXT5_EXT #x83F3)

(defconstant GL_CULL_VERTEX_IBM 103050)

(defconstant GL_VERTEX_ARRAY_LIST_IBM 103070)

(defconstant GL_NORMAL_ARRAY_LIST_IBM 103071)

(defconstant GL_COLOR_ARRAY_LIST_IBM 103072)

(defconstant GL_INDEX_ARRAY_LIST_IBM 103073)

(defconstant GL_TEXTURE_COORD_ARRAY_LIST_IBM 103074)

(defconstant GL_EDGE_FLAG_ARRAY_LIST_IBM 103075)

(defconstant GL_FOG_COORDINATE_ARRAY_LIST_IBM 103076)

(defconstant GL_SECONDARY_COLOR_ARRAY_LIST_IBM 103077)

(defconstant GL_VERTEX_ARRAY_LIST_STRIDE_IBM 103080)

(defconstant GL_NORMAL_ARRAY_LIST_STRIDE_IBM 103081)

(defconstant GL_COLOR_ARRAY_LIST_STRIDE_IBM 103082)

(defconstant GL_INDEX_ARRAY_LIST_STRIDE_IBM 103083)

(defconstant GL_TEXTURE_COORD_ARRAY_LIST_STRIDE_IBM 103084)

(defconstant GL_EDGE_FLAG_ARRAY_LIST_STRIDE_IBM 103085)

(defconstant GL_FOG_COORDINATE_ARRAY_LIST_STRIDE_IBM 103086)

(defconstant GL_SECONDARY_COLOR_ARRAY_LIST_STRIDE_IBM 103087)

(defconstant GL_PACK_SUBSAMPLE_RATE_SGIX #x85A0)

(defconstant GL_UNPACK_SUBSAMPLE_RATE_SGIX #x85A1)

(defconstant GL_PIXEL_SUBSAMPLE_4444_SGIX #x85A2)

(defconstant GL_PIXEL_SUBSAMPLE_2424_SGIX #x85A3)

(defconstant GL_PIXEL_SUBSAMPLE_4242_SGIX #x85A4)

(defconstant GL_YCRCB_SGIX #x8318)

(defconstant GL_YCRCBA_SGIX #x8319)

(defconstant GL_DEPTH_PASS_INSTRUMENT_SGIX #x8310)

(defconstant GL_DEPTH_PASS_INSTRUMENT_COUNTERS_SGIX #x8311)

(defconstant GL_DEPTH_PASS_INSTRUMENT_MAX_SGIX #x8312)

(defconstant GL_COMPRESSED_RGB_FXT1_3DFX #x86B0)

(defconstant GL_COMPRESSED_RGBA_FXT1_3DFX #x86B1)

(defconstant GL_MULTISAMPLE_3DFX #x86B2)

(defconstant GL_SAMPLE_BUFFERS_3DFX #x86B3)

(defconstant GL_SAMPLES_3DFX #x86B4)

(defconstant GL_MULTISAMPLE_BIT_3DFX #x20000000)

(defconstant GL_MULTISAMPLE_EXT #x809D)

(defconstant GL_SAMPLE_ALPHA_TO_MASK_EXT #x809E)

(defconstant GL_SAMPLE_ALPHA_TO_ONE_EXT #x809F)

(defconstant GL_SAMPLE_MASK_EXT #x80A0)

(defconstant GL_1PASS_EXT #x80A1)

(defconstant GL_2PASS_0_EXT #x80A2)

(defconstant GL_2PASS_1_EXT #x80A3)

(defconstant GL_4PASS_0_EXT #x80A4)

(defconstant GL_4PASS_1_EXT #x80A5)

(defconstant GL_4PASS_2_EXT #x80A6)

(defconstant GL_4PASS_3_EXT #x80A7)

(defconstant GL_SAMPLE_BUFFERS_EXT #x80A8)

(defconstant GL_SAMPLES_EXT #x80A9)

(defconstant GL_SAMPLE_MASK_VALUE_EXT #x80AA)

(defconstant GL_SAMPLE_MASK_INVERT_EXT #x80AB)

(defconstant GL_SAMPLE_PATTERN_EXT #x80AC)

(defconstant GL_MULTISAMPLE_BIT_EXT #x20000000)

(defconstant GL_VERTEX_PRECLIP_SGIX #x83EE)

(defconstant GL_VERTEX_PRECLIP_HINT_SGIX #x83EF)

(defconstant GL_CONVOLUTION_HINT_SGIX #x8316)

(defconstant GL_PACK_RESAMPLE_SGIX #x842C)

(defconstant GL_UNPACK_RESAMPLE_SGIX #x842D)

(defconstant GL_RESAMPLE_REPLICATE_SGIX #x842E)

(defconstant GL_RESAMPLE_ZERO_FILL_SGIX #x842F)

(defconstant GL_RESAMPLE_DECIMATE_SGIX #x8430)

(defconstant GL_EYE_DISTANCE_TO_POINT_SGIS #x81F0)

(defconstant GL_OBJECT_DISTANCE_TO_POINT_SGIS #x81F1)

(defconstant GL_EYE_DISTANCE_TO_LINE_SGIS #x81F2)

(defconstant GL_OBJECT_DISTANCE_TO_LINE_SGIS #x81F3)

(defconstant GL_EYE_POINT_SGIS #x81F4)

(defconstant GL_OBJECT_POINT_SGIS #x81F5)

(defconstant GL_EYE_LINE_SGIS #x81F6)

(defconstant GL_OBJECT_LINE_SGIS #x81F7)

(defconstant GL_TEXTURE_COLOR_WRITEMASK_SGIS #x81EF)

(defconstant GL_DOT3_RGB_EXT #x8740)

(defconstant GL_DOT3_RGBA_EXT #x8741)

(defconstant GL_MIRROR_CLAMP_ATI #x8742)

(defconstant GL_MIRROR_CLAMP_TO_EDGE_ATI #x8743)

(defconstant GL_ALL_COMPLETED_NV #x84F2)

(defconstant GL_FENCE_STATUS_NV #x84F3)

(defconstant GL_FENCE_CONDITION_NV #x84F4)

(defconstant GL_MIRRORED_REPEAT_IBM #x8370)

(defconstant GL_EVAL_2D_NV #x86C0)

(defconstant GL_EVAL_TRIANGULAR_2D_NV #x86C1)

(defconstant GL_MAP_TESSELLATION_NV #x86C2)

(defconstant GL_MAP_ATTRIB_U_ORDER_NV #x86C3)

(defconstant GL_MAP_ATTRIB_V_ORDER_NV #x86C4)

(defconstant GL_EVAL_FRACTIONAL_TESSELLATION_NV #x86C5)

(defconstant GL_EVAL_VERTEX_ATTRIB0_NV #x86C6)

(defconstant GL_EVAL_VERTEX_ATTRIB1_NV #x86C7)

(defconstant GL_EVAL_VERTEX_ATTRIB2_NV #x86C8)

(defconstant GL_EVAL_VERTEX_ATTRIB3_NV #x86C9)

(defconstant GL_EVAL_VERTEX_ATTRIB4_NV #x86CA)

(defconstant GL_EVAL_VERTEX_ATTRIB5_NV #x86CB)

(defconstant GL_EVAL_VERTEX_ATTRIB6_NV #x86CC)

(defconstant GL_EVAL_VERTEX_ATTRIB7_NV #x86CD)

(defconstant GL_EVAL_VERTEX_ATTRIB8_NV #x86CE)

(defconstant GL_EVAL_VERTEX_ATTRIB9_NV #x86CF)

(defconstant GL_EVAL_VERTEX_ATTRIB10_NV #x86D0)

(defconstant GL_EVAL_VERTEX_ATTRIB11_NV #x86D1)

(defconstant GL_EVAL_VERTEX_ATTRIB12_NV #x86D2)

(defconstant GL_EVAL_VERTEX_ATTRIB13_NV #x86D3)

(defconstant GL_EVAL_VERTEX_ATTRIB14_NV #x86D4)

(defconstant GL_EVAL_VERTEX_ATTRIB15_NV #x86D5)

(defconstant GL_MAX_MAP_TESSELLATION_NV #x86D6)

(defconstant GL_MAX_RATIONAL_EVAL_ORDER_NV #x86D7)

(defconstant GL_DEPTH_STENCIL_NV #x84F9)

(defconstant GL_UNSIGNED_INT_24_8_NV #x84FA)

(defconstant GL_PER_STAGE_CONSTANTS_NV #x8535)

(defconstant GL_TEXTURE_RECTANGLE_NV #x84F5)

(defconstant GL_TEXTURE_BINDING_RECTANGLE_NV #x84F6)

(defconstant GL_PROXY_TEXTURE_RECTANGLE_NV #x84F7)

(defconstant GL_MAX_RECTANGLE_TEXTURE_SIZE_NV #x84F8)

(defconstant GL_OFFSET_TEXTURE_RECTANGLE_NV #x864C)

(defconstant GL_OFFSET_TEXTURE_RECTANGLE_SCALE_NV #x864D)

(defconstant GL_DOT_PRODUCT_TEXTURE_RECTANGLE_NV #x864E)

(defconstant GL_RGBA_UNSIGNED_DOT_PRODUCT_MAPPING_NV #x86D9)

(defconstant GL_UNSIGNED_INT_S8_S8_8_8_NV #x86DA)

(defconstant GL_UNSIGNED_INT_8_8_S8_S8_REV_NV #x86DB)

(defconstant GL_DSDT_MAG_INTENSITY_NV #x86DC)

(defconstant GL_SHADER_CONSISTENT_NV #x86DD)

(defconstant GL_TEXTURE_SHADER_NV #x86DE)

(defconstant GL_SHADER_OPERATION_NV #x86DF)

(defconstant GL_CULL_MODES_NV #x86E0)

(defconstant GL_OFFSET_TEXTURE_MATRIX_NV #x86E1)

(defconstant GL_OFFSET_TEXTURE_SCALE_NV #x86E2)

(defconstant GL_OFFSET_TEXTURE_BIAS_NV #x86E3)

(defconstant GL_OFFSET_TEXTURE_2D_MATRIX_NV #x86E1)

(defconstant GL_OFFSET_TEXTURE_2D_SCALE_NV #x86E2)

(defconstant GL_OFFSET_TEXTURE_2D_BIAS_NV #x86E3)

(defconstant GL_PREVIOUS_TEXTURE_INPUT_NV #x86E4)

(defconstant GL_CONST_EYE_NV #x86E5)

(defconstant GL_PASS_THROUGH_NV #x86E6)

(defconstant GL_CULL_FRAGMENT_NV #x86E7)

(defconstant GL_OFFSET_TEXTURE_2D_NV #x86E8)

(defconstant GL_DEPENDENT_AR_TEXTURE_2D_NV #x86E9)

(defconstant GL_DEPENDENT_GB_TEXTURE_2D_NV #x86EA)

(defconstant GL_DOT_PRODUCT_NV #x86EC)

(defconstant GL_DOT_PRODUCT_DEPTH_REPLACE_NV #x86ED)

(defconstant GL_DOT_PRODUCT_TEXTURE_2D_NV #x86EE)

(defconstant GL_DOT_PRODUCT_TEXTURE_CUBE_MAP_NV #x86F0)

(defconstant GL_DOT_PRODUCT_DIFFUSE_CUBE_MAP_NV #x86F1)

(defconstant GL_DOT_PRODUCT_REFLECT_CUBE_MAP_NV #x86F2)

(defconstant GL_DOT_PRODUCT_CONST_EYE_REFLECT_CUBE_MAP_NV #x86F3)

(defconstant GL_HILO_NV #x86F4)

(defconstant GL_DSDT_NV #x86F5)

(defconstant GL_DSDT_MAG_NV #x86F6)

(defconstant GL_DSDT_MAG_VIB_NV #x86F7)

(defconstant GL_HILO16_NV #x86F8)

(defconstant GL_SIGNED_HILO_NV #x86F9)

(defconstant GL_SIGNED_HILO16_NV #x86FA)

(defconstant GL_SIGNED_RGBA_NV #x86FB)

(defconstant GL_SIGNED_RGBA8_NV #x86FC)

(defconstant GL_SIGNED_RGB_NV #x86FE)

(defconstant GL_SIGNED_RGB8_NV #x86FF)

(defconstant GL_SIGNED_LUMINANCE_NV #x8701)

(defconstant GL_SIGNED_LUMINANCE8_NV #x8702)

(defconstant GL_SIGNED_LUMINANCE_ALPHA_NV #x8703)

(defconstant GL_SIGNED_LUMINANCE8_ALPHA8_NV #x8704)

(defconstant GL_SIGNED_ALPHA_NV #x8705)

(defconstant GL_SIGNED_ALPHA8_NV #x8706)

(defconstant GL_SIGNED_INTENSITY_NV #x8707)

(defconstant GL_SIGNED_INTENSITY8_NV #x8708)

(defconstant GL_DSDT8_NV #x8709)

(defconstant GL_DSDT8_MAG8_NV #x870A)

(defconstant GL_DSDT8_MAG8_INTENSITY8_NV #x870B)

(defconstant GL_SIGNED_RGB_UNSIGNED_ALPHA_NV #x870C)

(defconstant GL_SIGNED_RGB8_UNSIGNED_ALPHA8_NV #x870D)

(defconstant GL_HI_SCALE_NV #x870E)

(defconstant GL_LO_SCALE_NV #x870F)

(defconstant GL_DS_SCALE_NV #x8710)

(defconstant GL_DT_SCALE_NV #x8711)

(defconstant GL_MAGNITUDE_SCALE_NV #x8712)

(defconstant GL_VIBRANCE_SCALE_NV #x8713)

(defconstant GL_HI_BIAS_NV #x8714)

(defconstant GL_LO_BIAS_NV #x8715)

(defconstant GL_DS_BIAS_NV #x8716)

(defconstant GL_DT_BIAS_NV #x8717)

(defconstant GL_MAGNITUDE_BIAS_NV #x8718)

(defconstant GL_VIBRANCE_BIAS_NV #x8719)

(defconstant GL_TEXTURE_BORDER_VALUES_NV #x871A)

(defconstant GL_TEXTURE_HI_SIZE_NV #x871B)

(defconstant GL_TEXTURE_LO_SIZE_NV #x871C)

(defconstant GL_TEXTURE_DS_SIZE_NV #x871D)

(defconstant GL_TEXTURE_DT_SIZE_NV #x871E)

(defconstant GL_TEXTURE_MAG_SIZE_NV #x871F)

(defconstant GL_DOT_PRODUCT_TEXTURE_3D_NV #x86EF)

(defconstant GL_VERTEX_ARRAY_RANGE_WITHOUT_FLUSH_NV #x8533)

(defconstant GL_VERTEX_PROGRAM_NV #x8620)

(defconstant GL_VERTEX_STATE_PROGRAM_NV #x8621)

(defconstant GL_ATTRIB_ARRAY_SIZE_NV #x8623)

(defconstant GL_ATTRIB_ARRAY_STRIDE_NV #x8624)

(defconstant GL_ATTRIB_ARRAY_TYPE_NV #x8625)

(defconstant GL_CURRENT_ATTRIB_NV #x8626)

(defconstant GL_PROGRAM_LENGTH_NV #x8627)

(defconstant GL_PROGRAM_STRING_NV #x8628)

(defconstant GL_MODELVIEW_PROJECTION_NV #x8629)

(defconstant GL_IDENTITY_NV #x862A)

(defconstant GL_INVERSE_NV #x862B)

(defconstant GL_TRANSPOSE_NV #x862C)

(defconstant GL_INVERSE_TRANSPOSE_NV #x862D)

(defconstant GL_MAX_TRACK_MATRIX_STACK_DEPTH_NV #x862E)

(defconstant GL_MAX_TRACK_MATRICES_NV #x862F)

(defconstant GL_MATRIX0_NV #x8630)

(defconstant GL_MATRIX1_NV #x8631)

(defconstant GL_MATRIX2_NV #x8632)

(defconstant GL_MATRIX3_NV #x8633)

(defconstant GL_MATRIX4_NV #x8634)

(defconstant GL_MATRIX5_NV #x8635)

(defconstant GL_MATRIX6_NV #x8636)

(defconstant GL_MATRIX7_NV #x8637)

(defconstant GL_CURRENT_MATRIX_STACK_DEPTH_NV #x8640)

(defconstant GL_CURRENT_MATRIX_NV #x8641)

(defconstant GL_VERTEX_PROGRAM_POINT_SIZE_NV #x8642)

(defconstant GL_VERTEX_PROGRAM_TWO_SIDE_NV #x8643)

(defconstant GL_PROGRAM_PARAMETER_NV #x8644)

(defconstant GL_ATTRIB_ARRAY_POINTER_NV #x8645)

(defconstant GL_PROGRAM_TARGET_NV #x8646)

(defconstant GL_PROGRAM_RESIDENT_NV #x8647)

(defconstant GL_TRACK_MATRIX_NV #x8648)

(defconstant GL_TRACK_MATRIX_TRANSFORM_NV #x8649)

(defconstant GL_VERTEX_PROGRAM_BINDING_NV #x864A)

(defconstant GL_PROGRAM_ERROR_POSITION_NV #x864B)

(defconstant GL_VERTEX_ATTRIB_ARRAY0_NV #x8650)

(defconstant GL_VERTEX_ATTRIB_ARRAY1_NV #x8651)

(defconstant GL_VERTEX_ATTRIB_ARRAY2_NV #x8652)

(defconstant GL_VERTEX_ATTRIB_ARRAY3_NV #x8653)

(defconstant GL_VERTEX_ATTRIB_ARRAY4_NV #x8654)

(defconstant GL_VERTEX_ATTRIB_ARRAY5_NV #x8655)

(defconstant GL_VERTEX_ATTRIB_ARRAY6_NV #x8656)

(defconstant GL_VERTEX_ATTRIB_ARRAY7_NV #x8657)

(defconstant GL_VERTEX_ATTRIB_ARRAY8_NV #x8658)

(defconstant GL_VERTEX_ATTRIB_ARRAY9_NV #x8659)

(defconstant GL_VERTEX_ATTRIB_ARRAY10_NV #x865A)

(defconstant GL_VERTEX_ATTRIB_ARRAY11_NV #x865B)

(defconstant GL_VERTEX_ATTRIB_ARRAY12_NV #x865C)

(defconstant GL_VERTEX_ATTRIB_ARRAY13_NV #x865D)

(defconstant GL_VERTEX_ATTRIB_ARRAY14_NV #x865E)

(defconstant GL_VERTEX_ATTRIB_ARRAY15_NV #x865F)

(defconstant GL_MAP1_VERTEX_ATTRIB0_4_NV #x8660)

(defconstant GL_MAP1_VERTEX_ATTRIB1_4_NV #x8661)

(defconstant GL_MAP1_VERTEX_ATTRIB2_4_NV #x8662)

(defconstant GL_MAP1_VERTEX_ATTRIB3_4_NV #x8663)

(defconstant GL_MAP1_VERTEX_ATTRIB4_4_NV #x8664)

(defconstant GL_MAP1_VERTEX_ATTRIB5_4_NV #x8665)

(defconstant GL_MAP1_VERTEX_ATTRIB6_4_NV #x8666)

(defconstant GL_MAP1_VERTEX_ATTRIB7_4_NV #x8667)

(defconstant GL_MAP1_VERTEX_ATTRIB8_4_NV #x8668)

(defconstant GL_MAP1_VERTEX_ATTRIB9_4_NV #x8669)

(defconstant GL_MAP1_VERTEX_ATTRIB10_4_NV #x866A)

(defconstant GL_MAP1_VERTEX_ATTRIB11_4_NV #x866B)

(defconstant GL_MAP1_VERTEX_ATTRIB12_4_NV #x866C)

(defconstant GL_MAP1_VERTEX_ATTRIB13_4_NV #x866D)

(defconstant GL_MAP1_VERTEX_ATTRIB14_4_NV #x866E)

(defconstant GL_MAP1_VERTEX_ATTRIB15_4_NV #x866F)

(defconstant GL_MAP2_VERTEX_ATTRIB0_4_NV #x8670)

(defconstant GL_MAP2_VERTEX_ATTRIB1_4_NV #x8671)

(defconstant GL_MAP2_VERTEX_ATTRIB2_4_NV #x8672)

(defconstant GL_MAP2_VERTEX_ATTRIB3_4_NV #x8673)

(defconstant GL_MAP2_VERTEX_ATTRIB4_4_NV #x8674)

(defconstant GL_MAP2_VERTEX_ATTRIB5_4_NV #x8675)

(defconstant GL_MAP2_VERTEX_ATTRIB6_4_NV #x8676)

(defconstant GL_MAP2_VERTEX_ATTRIB7_4_NV #x8677)

(defconstant GL_MAP2_VERTEX_ATTRIB8_4_NV #x8678)

(defconstant GL_MAP2_VERTEX_ATTRIB9_4_NV #x8679)

(defconstant GL_MAP2_VERTEX_ATTRIB10_4_NV #x867A)

(defconstant GL_MAP2_VERTEX_ATTRIB11_4_NV #x867B)

(defconstant GL_MAP2_VERTEX_ATTRIB12_4_NV #x867C)

(defconstant GL_MAP2_VERTEX_ATTRIB13_4_NV #x867D)

(defconstant GL_MAP2_VERTEX_ATTRIB14_4_NV #x867E)

(defconstant GL_MAP2_VERTEX_ATTRIB15_4_NV #x867F)

(defconstant GL_TEXTURE_MAX_CLAMP_S_SGIX #x8369)

(defconstant GL_TEXTURE_MAX_CLAMP_T_SGIX #x836A)

(defconstant GL_TEXTURE_MAX_CLAMP_R_SGIX #x836B)

(defconstant GL_SCALEBIAS_HINT_SGIX #x8322)

(defconstant GL_INTERLACE_OML #x8980)

(defconstant GL_INTERLACE_READ_OML #x8981)

(defconstant GL_FORMAT_SUBSAMPLE_24_24_OML #x8982)

(defconstant GL_FORMAT_SUBSAMPLE_244_244_OML #x8983)

(defconstant GL_PACK_RESAMPLE_OML #x8984)

(defconstant GL_UNPACK_RESAMPLE_OML #x8985)

(defconstant GL_RESAMPLE_REPLICATE_OML #x8986)

(defconstant GL_RESAMPLE_ZERO_FILL_OML #x8987)

(defconstant GL_RESAMPLE_AVERAGE_OML #x8988)

(defconstant GL_RESAMPLE_DECIMATE_OML #x8989)

(defconstant GL_DEPTH_STENCIL_TO_RGBA_NV #x886E)

(defconstant GL_DEPTH_STENCIL_TO_BGRA_NV #x886F)

(defconstant GL_BUMP_ROT_MATRIX_ATI #x8775)

(defconstant GL_BUMP_ROT_MATRIX_SIZE_ATI #x8776)

(defconstant GL_BUMP_NUM_TEX_UNITS_ATI #x8777)

(defconstant GL_BUMP_TEX_UNITS_ATI #x8778)

(defconstant GL_DUDV_ATI #x8779)

(defconstant GL_DU8DV8_ATI #x877A)

(defconstant GL_BUMP_ENVMAP_ATI #x877B)

(defconstant GL_BUMP_TARGET_ATI #x877C)

(defconstant GL_FRAGMENT_SHADER_ATI #x8920)

(defconstant GL_REG_0_ATI #x8921)

(defconstant GL_REG_1_ATI #x8922)

(defconstant GL_REG_2_ATI #x8923)

(defconstant GL_REG_3_ATI #x8924)

(defconstant GL_REG_4_ATI #x8925)

(defconstant GL_REG_5_ATI #x8926)

(defconstant GL_REG_6_ATI #x8927)

(defconstant GL_REG_7_ATI #x8928)

(defconstant GL_REG_8_ATI #x8929)

(defconstant GL_REG_9_ATI #x892A)

(defconstant GL_REG_10_ATI #x892B)

(defconstant GL_REG_11_ATI #x892C)

(defconstant GL_REG_12_ATI #x892D)

(defconstant GL_REG_13_ATI #x892E)

(defconstant GL_REG_14_ATI #x892F)

(defconstant GL_REG_15_ATI #x8930)

(defconstant GL_REG_16_ATI #x8931)

(defconstant GL_REG_17_ATI #x8932)

(defconstant GL_REG_18_ATI #x8933)

(defconstant GL_REG_19_ATI #x8934)

(defconstant GL_REG_20_ATI #x8935)

(defconstant GL_REG_21_ATI #x8936)

(defconstant GL_REG_22_ATI #x8937)

(defconstant GL_REG_23_ATI #x8938)

(defconstant GL_REG_24_ATI #x8939)

(defconstant GL_REG_25_ATI #x893A)

(defconstant GL_REG_26_ATI #x893B)

(defconstant GL_REG_27_ATI #x893C)

(defconstant GL_REG_28_ATI #x893D)

(defconstant GL_REG_29_ATI #x893E)

(defconstant GL_REG_30_ATI #x893F)

(defconstant GL_REG_31_ATI #x8940)

(defconstant GL_CON_0_ATI #x8941)

(defconstant GL_CON_1_ATI #x8942)

(defconstant GL_CON_2_ATI #x8943)

(defconstant GL_CON_3_ATI #x8944)

(defconstant GL_CON_4_ATI #x8945)

(defconstant GL_CON_5_ATI #x8946)

(defconstant GL_CON_6_ATI #x8947)

(defconstant GL_CON_7_ATI #x8948)

(defconstant GL_CON_8_ATI #x8949)

(defconstant GL_CON_9_ATI #x894A)

(defconstant GL_CON_10_ATI #x894B)

(defconstant GL_CON_11_ATI #x894C)

(defconstant GL_CON_12_ATI #x894D)

(defconstant GL_CON_13_ATI #x894E)

(defconstant GL_CON_14_ATI #x894F)

(defconstant GL_CON_15_ATI #x8950)

(defconstant GL_CON_16_ATI #x8951)

(defconstant GL_CON_17_ATI #x8952)

(defconstant GL_CON_18_ATI #x8953)

(defconstant GL_CON_19_ATI #x8954)

(defconstant GL_CON_20_ATI #x8955)

(defconstant GL_CON_21_ATI #x8956)

(defconstant GL_CON_22_ATI #x8957)

(defconstant GL_CON_23_ATI #x8958)

(defconstant GL_CON_24_ATI #x8959)

(defconstant GL_CON_25_ATI #x895A)

(defconstant GL_CON_26_ATI #x895B)

(defconstant GL_CON_27_ATI #x895C)

(defconstant GL_CON_28_ATI #x895D)

(defconstant GL_CON_29_ATI #x895E)

(defconstant GL_CON_30_ATI #x895F)

(defconstant GL_CON_31_ATI #x8960)

(defconstant GL_MOV_ATI #x8961)

(defconstant GL_ADD_ATI #x8963)

(defconstant GL_MUL_ATI #x8964)

(defconstant GL_SUB_ATI #x8965)

(defconstant GL_DOT3_ATI #x8966)

(defconstant GL_DOT4_ATI #x8967)

(defconstant GL_MAD_ATI #x8968)

(defconstant GL_LERP_ATI #x8969)

(defconstant GL_CND_ATI #x896A)

(defconstant GL_CND0_ATI #x896B)

(defconstant GL_DOT2_ADD_ATI #x896C)

(defconstant GL_SECONDARY_INTERPOLATOR_ATI #x896D)

(defconstant GL_NUM_FRAGMENT_REGISTERS_ATI #x896E)

(defconstant GL_NUM_FRAGMENT_CONSTANTS_ATI #x896F)

(defconstant GL_NUM_PASSES_ATI #x8970)

(defconstant GL_NUM_INSTRUCTIONS_PER_PASS_ATI #x8971)

(defconstant GL_NUM_INSTRUCTIONS_TOTAL_ATI #x8972)

(defconstant GL_NUM_INPUT_INTERPOLATOR_COMPONENTS_ATI #x8973)

(defconstant GL_NUM_LOOPBACK_COMPONENTS_ATI #x8974)

(defconstant GL_COLOR_ALPHA_PAIRING_ATI #x8975)

(defconstant GL_SWIZZLE_STR_ATI #x8976)

(defconstant GL_SWIZZLE_STQ_ATI #x8977)

(defconstant GL_SWIZZLE_STR_DR_ATI #x8978)

(defconstant GL_SWIZZLE_STQ_DQ_ATI #x8979)

(defconstant GL_SWIZZLE_STRQ_ATI #x897A)

(defconstant GL_SWIZZLE_STRQ_DQ_ATI #x897B)

(defconstant GL_RED_BIT_ATI #x00000001)

(defconstant GL_GREEN_BIT_ATI #x00000002)

(defconstant GL_BLUE_BIT_ATI #x00000004)

(defconstant GL_2X_BIT_ATI #x00000001)

(defconstant GL_4X_BIT_ATI #x00000002)

(defconstant GL_8X_BIT_ATI #x00000004)

(defconstant GL_HALF_BIT_ATI #x00000008)

(defconstant GL_QUARTER_BIT_ATI #x00000010)

(defconstant GL_EIGHTH_BIT_ATI #x00000020)

(defconstant GL_SATURATE_BIT_ATI #x00000040)

(defconstant GL_COMP_BIT_ATI #x00000002)

(defconstant GL_NEGATE_BIT_ATI #x00000004)

(defconstant GL_BIAS_BIT_ATI #x00000008)

(defconstant GL_PN_TRIANGLES_ATI #x87F0)

(defconstant GL_MAX_PN_TRIANGLES_TESSELATION_LEVEL_ATI #x87F1)

(defconstant GL_PN_TRIANGLES_POINT_MODE_ATI #x87F2)

(defconstant GL_PN_TRIANGLES_NORMAL_MODE_ATI #x87F3)

(defconstant GL_PN_TRIANGLES_TESSELATION_LEVEL_ATI #x87F4)

(defconstant GL_PN_TRIANGLES_POINT_MODE_LINEAR_ATI #x87F5)

(defconstant GL_PN_TRIANGLES_POINT_MODE_CUBIC_ATI #x87F6)

(defconstant GL_PN_TRIANGLES_NORMAL_MODE_LINEAR_ATI #x87F7)

(defconstant GL_PN_TRIANGLES_NORMAL_MODE_QUADRATIC_ATI #x87F8)

(defconstant GL_STATIC_ATI #x8760)

(defconstant GL_DYNAMIC_ATI #x8761)

(defconstant GL_PRESERVE_ATI #x8762)

(defconstant GL_DISCARD_ATI #x8763)

(defconstant GL_OBJECT_BUFFER_SIZE_ATI #x8764)

(defconstant GL_OBJECT_BUFFER_USAGE_ATI #x8765)

(defconstant GL_ARRAY_OBJECT_BUFFER_ATI #x8766)

(defconstant GL_ARRAY_OBJECT_OFFSET_ATI #x8767)

(defconstant GL_VERTEX_SHADER_EXT #x8780)

(defconstant GL_VERTEX_SHADER_BINDING_EXT #x8781)

(defconstant GL_OP_INDEX_EXT #x8782)

(defconstant GL_OP_NEGATE_EXT #x8783)

(defconstant GL_OP_DOT3_EXT #x8784)

(defconstant GL_OP_DOT4_EXT #x8785)

(defconstant GL_OP_MUL_EXT #x8786)

(defconstant GL_OP_ADD_EXT #x8787)

(defconstant GL_OP_MADD_EXT #x8788)

(defconstant GL_OP_FRAC_EXT #x8789)

(defconstant GL_OP_MAX_EXT #x878A)

(defconstant GL_OP_MIN_EXT #x878B)

(defconstant GL_OP_SET_GE_EXT #x878C)

(defconstant GL_OP_SET_LT_EXT #x878D)

(defconstant GL_OP_CLAMP_EXT #x878E)

(defconstant GL_OP_FLOOR_EXT #x878F)

(defconstant GL_OP_ROUND_EXT #x8790)

(defconstant GL_OP_EXP_BASE_2_EXT #x8791)

(defconstant GL_OP_LOG_BASE_2_EXT #x8792)

(defconstant GL_OP_POWER_EXT #x8793)

(defconstant GL_OP_RECIP_EXT #x8794)

(defconstant GL_OP_RECIP_SQRT_EXT #x8795)

(defconstant GL_OP_SUB_EXT #x8796)

(defconstant GL_OP_CROSS_PRODUCT_EXT #x8797)

(defconstant GL_OP_MULTIPLY_MATRIX_EXT #x8798)

(defconstant GL_OP_MOV_EXT #x8799)

(defconstant GL_OUTPUT_VERTEX_EXT #x879A)

(defconstant GL_OUTPUT_COLOR0_EXT #x879B)

(defconstant GL_OUTPUT_COLOR1_EXT #x879C)

(defconstant GL_OUTPUT_TEXTURE_COORD0_EXT #x879D)

(defconstant GL_OUTPUT_TEXTURE_COORD1_EXT #x879E)

(defconstant GL_OUTPUT_TEXTURE_COORD2_EXT #x879F)

(defconstant GL_OUTPUT_TEXTURE_COORD3_EXT #x87A0)

(defconstant GL_OUTPUT_TEXTURE_COORD4_EXT #x87A1)

(defconstant GL_OUTPUT_TEXTURE_COORD5_EXT #x87A2)

(defconstant GL_OUTPUT_TEXTURE_COORD6_EXT #x87A3)

(defconstant GL_OUTPUT_TEXTURE_COORD7_EXT #x87A4)

(defconstant GL_OUTPUT_TEXTURE_COORD8_EXT #x87A5)

(defconstant GL_OUTPUT_TEXTURE_COORD9_EXT #x87A6)

(defconstant GL_OUTPUT_TEXTURE_COORD10_EXT #x87A7)

(defconstant GL_OUTPUT_TEXTURE_COORD11_EXT #x87A8)

(defconstant GL_OUTPUT_TEXTURE_COORD12_EXT #x87A9)

(defconstant GL_OUTPUT_TEXTURE_COORD13_EXT #x87AA)

(defconstant GL_OUTPUT_TEXTURE_COORD14_EXT #x87AB)

(defconstant GL_OUTPUT_TEXTURE_COORD15_EXT #x87AC)

(defconstant GL_OUTPUT_TEXTURE_COORD16_EXT #x87AD)

(defconstant GL_OUTPUT_TEXTURE_COORD17_EXT #x87AE)

(defconstant GL_OUTPUT_TEXTURE_COORD18_EXT #x87AF)

(defconstant GL_OUTPUT_TEXTURE_COORD19_EXT #x87B0)

(defconstant GL_OUTPUT_TEXTURE_COORD20_EXT #x87B1)

(defconstant GL_OUTPUT_TEXTURE_COORD21_EXT #x87B2)

(defconstant GL_OUTPUT_TEXTURE_COORD22_EXT #x87B3)

(defconstant GL_OUTPUT_TEXTURE_COORD23_EXT #x87B4)

(defconstant GL_OUTPUT_TEXTURE_COORD24_EXT #x87B5)

(defconstant GL_OUTPUT_TEXTURE_COORD25_EXT #x87B6)

(defconstant GL_OUTPUT_TEXTURE_COORD26_EXT #x87B7)

(defconstant GL_OUTPUT_TEXTURE_COORD27_EXT #x87B8)

(defconstant GL_OUTPUT_TEXTURE_COORD28_EXT #x87B9)

(defconstant GL_OUTPUT_TEXTURE_COORD29_EXT #x87BA)

(defconstant GL_OUTPUT_TEXTURE_COORD30_EXT #x87BB)

(defconstant GL_OUTPUT_TEXTURE_COORD31_EXT #x87BC)

(defconstant GL_OUTPUT_FOG_EXT #x87BD)

(defconstant GL_SCALAR_EXT #x87BE)

(defconstant GL_VECTOR_EXT #x87BF)

(defconstant GL_MATRIX_EXT #x87C0)

(defconstant GL_VARIANT_EXT #x87C1)

(defconstant GL_INVARIANT_EXT #x87C2)

(defconstant GL_LOCAL_CONSTANT_EXT #x87C3)

(defconstant GL_LOCAL_EXT #x87C4)

(defconstant GL_MAX_VERTEX_SHADER_INSTRUCTIONS_EXT #x87C5)

(defconstant GL_MAX_VERTEX_SHADER_VARIANTS_EXT #x87C6)

(defconstant GL_MAX_VERTEX_SHADER_INVARIANTS_EXT #x87C7)

(defconstant GL_MAX_VERTEX_SHADER_LOCAL_CONSTANTS_EXT #x87C8)

(defconstant GL_MAX_VERTEX_SHADER_LOCALS_EXT #x87C9)

(defconstant GL_MAX_OPTIMIZED_VERTEX_SHADER_INSTRUCTIONS_EXT #x87CA)

(defconstant GL_MAX_OPTIMIZED_VERTEX_SHADER_VARIANTS_EXT #x87CB)

(defconstant GL_MAX_OPTIMIZED_VERTEX_SHADER_LOCAL_CONSTANTS_EXT #x87CC)

(defconstant GL_MAX_OPTIMIZED_VERTEX_SHADER_INVARIANTS_EXT #x87CD)

(defconstant GL_MAX_OPTIMIZED_VERTEX_SHADER_LOCALS_EXT #x87CE)

(defconstant GL_VERTEX_SHADER_INSTRUCTIONS_EXT #x87CF)

(defconstant GL_VERTEX_SHADER_VARIANTS_EXT #x87D0)

(defconstant GL_VERTEX_SHADER_INVARIANTS_EXT #x87D1)

(defconstant GL_VERTEX_SHADER_LOCAL_CONSTANTS_EXT #x87D2)

(defconstant GL_VERTEX_SHADER_LOCALS_EXT #x87D3)

(defconstant GL_VERTEX_SHADER_OPTIMIZED_EXT #x87D4)

(defconstant GL_X_EXT #x87D5)

(defconstant GL_Y_EXT #x87D6)

(defconstant GL_Z_EXT #x87D7)

(defconstant GL_W_EXT #x87D8)

(defconstant GL_NEGATIVE_X_EXT #x87D9)

(defconstant GL_NEGATIVE_Y_EXT #x87DA)

(defconstant GL_NEGATIVE_Z_EXT #x87DB)

(defconstant GL_NEGATIVE_W_EXT #x87DC)

(defconstant GL_ZERO_EXT #x87DD)

(defconstant GL_ONE_EXT #x87DE)

(defconstant GL_NEGATIVE_ONE_EXT #x87DF)

(defconstant GL_NORMALIZED_RANGE_EXT #x87E0)

(defconstant GL_FULL_RANGE_EXT #x87E1)

(defconstant GL_CURRENT_VERTEX_EXT #x87E2)

(defconstant GL_MVP_MATRIX_EXT #x87E3)

(defconstant GL_VARIANT_VALUE_EXT #x87E4)

(defconstant GL_VARIANT_DATATYPE_EXT #x87E5)

(defconstant GL_VARIANT_ARRAY_STRIDE_EXT #x87E6)

(defconstant GL_VARIANT_ARRAY_TYPE_EXT #x87E7)

(defconstant GL_VARIANT_ARRAY_EXT #x87E8)

(defconstant GL_VARIANT_ARRAY_POINTER_EXT #x87E9)

(defconstant GL_INVARIANT_VALUE_EXT #x87EA)

(defconstant GL_INVARIANT_DATATYPE_EXT #x87EB)

(defconstant GL_LOCAL_CONSTANT_VALUE_EXT #x87EC)

(defconstant GL_LOCAL_CONSTANT_DATATYPE_EXT #x87ED)

(defconstant GL_MAX_VERTEX_STREAMS_ATI #x876B)

(defconstant GL_VERTEX_STREAM0_ATI #x876C)

(defconstant GL_VERTEX_STREAM1_ATI #x876D)

(defconstant GL_VERTEX_STREAM2_ATI #x876E)

(defconstant GL_VERTEX_STREAM3_ATI #x876F)

(defconstant GL_VERTEX_STREAM4_ATI #x8770)

(defconstant GL_VERTEX_STREAM5_ATI #x8771)

(defconstant GL_VERTEX_STREAM6_ATI #x8772)

(defconstant GL_VERTEX_STREAM7_ATI #x8773)

(defconstant GL_VERTEX_SOURCE_ATI #x8774)

(defconstant GL_ELEMENT_ARRAY_ATI #x8768)

(defconstant GL_ELEMENT_ARRAY_TYPE_ATI #x8769)

(defconstant GL_ELEMENT_ARRAY_POINTER_ATI #x876A)

(defconstant GL_QUAD_MESH_SUN #x8614)

(defconstant GL_TRIANGLE_MESH_SUN #x8615)

(defconstant GL_SLICE_ACCUM_SUN #x85CC)

(defconstant GL_MULTISAMPLE_FILTER_HINT_NV #x8534)

(defconstant GL_DEPTH_CLAMP_NV #x864F)

(defconstant GL_PIXEL_COUNTER_BITS_NV #x8864)

(defconstant GL_CURRENT_OCCLUSION_QUERY_ID_NV #x8865)

(defconstant GL_PIXEL_COUNT_NV #x8866)

(defconstant GL_PIXEL_COUNT_AVAILABLE_NV #x8867)

(defconstant GL_POINT_SPRITE_NV #x8861)

(defconstant GL_COORD_REPLACE_NV #x8862)

(defconstant GL_POINT_SPRITE_R_MODE_NV #x8863)

(defconstant GL_OFFSET_PROJECTIVE_TEXTURE_2D_NV #x8850)

(defconstant GL_OFFSET_PROJECTIVE_TEXTURE_2D_SCALE_NV #x8851)

(defconstant GL_OFFSET_PROJECTIVE_TEXTURE_RECTANGLE_NV #x8852)

(defconstant GL_OFFSET_PROJECTIVE_TEXTURE_RECTANGLE_SCALE_NV #x8853)

(defconstant GL_OFFSET_HILO_TEXTURE_2D_NV #x8854)

(defconstant GL_OFFSET_HILO_TEXTURE_RECTANGLE_NV #x8855)

(defconstant GL_OFFSET_HILO_PROJECTIVE_TEXTURE_2D_NV #x8856)

(defconstant GL_OFFSET_HILO_PROJECTIVE_TEXTURE_RECTANGLE_NV #x8857)

(defconstant GL_DEPENDENT_HILO_TEXTURE_2D_NV #x8858)

(defconstant GL_DEPENDENT_RGB_TEXTURE_3D_NV #x8859)

(defconstant GL_DEPENDENT_RGB_TEXTURE_CUBE_MAP_NV #x885A)

(defconstant GL_DOT_PRODUCT_PASS_THROUGH_NV #x885B)

(defconstant GL_DOT_PRODUCT_TEXTURE_1D_NV #x885C)

(defconstant GL_DOT_PRODUCT_AFFINE_DEPTH_REPLACE_NV #x885D)

(defconstant GL_HILO8_NV #x885E)

(defconstant GL_SIGNED_HILO8_NV #x885F)

(defconstant GL_FORCE_BLUE_TO_ONE_NV #x8860)

(defconstant GL_STENCIL_TEST_TWO_SIDE_EXT #x8910)

(defconstant GL_ACTIVE_STENCIL_FACE_EXT #x8911)

(defconstant GL_TEXT_FRAGMENT_SHADER_ATI #x8200)

(defconstant GL_UNPACK_CLIENT_STORAGE_APPLE #x85B2)

(defconstant GL_ELEMENT_ARRAY_APPLE #x8768)

(defconstant GL_ELEMENT_ARRAY_TYPE_APPLE #x8769)

(defconstant GL_ELEMENT_ARRAY_POINTER_APPLE #x876A)

(defconstant GL_DRAW_PIXELS_APPLE #x8A0A)

(defconstant GL_FENCE_APPLE #x8A0B)

(defconstant GL_VERTEX_ARRAY_BINDING_APPLE #x85B5)

(defconstant GL_VERTEX_ARRAY_RANGE_APPLE #x851D)

(defconstant GL_VERTEX_ARRAY_RANGE_LENGTH_APPLE #x851E)

(defconstant GL_VERTEX_ARRAY_STORAGE_HINT_APPLE #x851F)

(defconstant GL_VERTEX_ARRAY_RANGE_POINTER_APPLE #x8521)

(defconstant GL_STORAGE_CACHED_APPLE #x85BE)

(defconstant GL_STORAGE_SHARED_APPLE #x85BF)

(defconstant GL_YCBCR_422_APPLE #x85B9)

(defconstant GL_UNSIGNED_SHORT_8_8_APPLE #x85BA)

(defconstant GL_UNSIGNED_SHORT_8_8_REV_APPLE #x85BB)

(defconstant GL_RGB_S3TC #x83A0)

(defconstant GL_RGB4_S3TC #x83A1)

(defconstant GL_RGBA_S3TC #x83A2)

(defconstant GL_RGBA4_S3TC #x83A3)

(defconstant GL_MAX_DRAW_BUFFERS_ATI #x8824)

(defconstant GL_DRAW_BUFFER0_ATI #x8825)

(defconstant GL_DRAW_BUFFER1_ATI #x8826)

(defconstant GL_DRAW_BUFFER2_ATI #x8827)

(defconstant GL_DRAW_BUFFER3_ATI #x8828)

(defconstant GL_DRAW_BUFFER4_ATI #x8829)

(defconstant GL_DRAW_BUFFER5_ATI #x882A)

(defconstant GL_DRAW_BUFFER6_ATI #x882B)

(defconstant GL_DRAW_BUFFER7_ATI #x882C)

(defconstant GL_DRAW_BUFFER8_ATI #x882D)

(defconstant GL_DRAW_BUFFER9_ATI #x882E)

(defconstant GL_DRAW_BUFFER10_ATI #x882F)

(defconstant GL_DRAW_BUFFER11_ATI #x8830)

(defconstant GL_DRAW_BUFFER12_ATI #x8831)

(defconstant GL_DRAW_BUFFER13_ATI #x8832)

(defconstant GL_DRAW_BUFFER14_ATI #x8833)

(defconstant GL_DRAW_BUFFER15_ATI #x8834)

(defconstant GL_MODULATE_ADD_ATI #x8744)

(defconstant GL_MODULATE_SIGNED_ADD_ATI #x8745)

(defconstant GL_MODULATE_SUBTRACT_ATI #x8746)

(defconstant GL_RGBA_FLOAT32_ATI #x8814)

(defconstant GL_RGB_FLOAT32_ATI #x8815)

(defconstant GL_ALPHA_FLOAT32_ATI #x8816)

(defconstant GL_INTENSITY_FLOAT32_ATI #x8817)

(defconstant GL_LUMINANCE_FLOAT32_ATI #x8818)

(defconstant GL_LUMINANCE_ALPHA_FLOAT32_ATI #x8819)

(defconstant GL_RGBA_FLOAT16_ATI #x881A)

(defconstant GL_RGB_FLOAT16_ATI #x881B)

(defconstant GL_ALPHA_FLOAT16_ATI #x881C)

(defconstant GL_INTENSITY_FLOAT16_ATI #x881D)

(defconstant GL_LUMINANCE_FLOAT16_ATI #x881E)

(defconstant GL_LUMINANCE_ALPHA_FLOAT16_ATI #x881F)

(defconstant GL_FLOAT_R_NV #x8880)

(defconstant GL_FLOAT_RG_NV #x8881)

(defconstant GL_FLOAT_RGB_NV #x8882)

(defconstant GL_FLOAT_RGBA_NV #x8883)

(defconstant GL_FLOAT_R16_NV #x8884)

(defconstant GL_FLOAT_R32_NV #x8885)

(defconstant GL_FLOAT_RG16_NV #x8886)

(defconstant GL_FLOAT_RG32_NV #x8887)

(defconstant GL_FLOAT_RGB16_NV #x8888)

(defconstant GL_FLOAT_RGB32_NV #x8889)

(defconstant GL_FLOAT_RGBA16_NV #x888A)

(defconstant GL_FLOAT_RGBA32_NV #x888B)

(defconstant GL_TEXTURE_FLOAT_COMPONENTS_NV #x888C)

(defconstant GL_FLOAT_CLEAR_COLOR_VALUE_NV #x888D)

(defconstant GL_FLOAT_RGBA_MODE_NV #x888E)

(defconstant GL_MAX_FRAGMENT_PROGRAM_LOCAL_PARAMETERS_NV #x8868)

(defconstant GL_FRAGMENT_PROGRAM_NV #x8870)

(defconstant GL_MAX_TEXTURE_COORDS_NV #x8871)

(defconstant GL_MAX_TEXTURE_IMAGE_UNITS_NV #x8872)

(defconstant GL_FRAGMENT_PROGRAM_BINDING_NV #x8873)

(defconstant GL_PROGRAM_ERROR_STRING_NV #x8874)

(defconstant GL_HALF_FLOAT_NV #x140B)

(defconstant GL_WRITE_PIXEL_DATA_RANGE_NV #x8878)

(defconstant GL_READ_PIXEL_DATA_RANGE_NV #x8879)

(defconstant GL_WRITE_PIXEL_DATA_RANGE_LENGTH_NV #x887A)

(defconstant GL_READ_PIXEL_DATA_RANGE_LENGTH_NV #x887B)

(defconstant GL_WRITE_PIXEL_DATA_RANGE_POINTER_NV #x887C)

(defconstant GL_READ_PIXEL_DATA_RANGE_POINTER_NV #x887D)

(defconstant GL_PRIMITIVE_RESTART_NV #x8558)

(defconstant GL_PRIMITIVE_RESTART_INDEX_NV #x8559)

(defconstant GL_TEXTURE_UNSIGNED_REMAP_MODE_NV #x888F)

(defconstant GL_STENCIL_BACK_FUNC_ATI #x8800)

(defconstant GL_STENCIL_BACK_FAIL_ATI #x8801)

(defconstant GL_STENCIL_BACK_PASS_DEPTH_FAIL_ATI #x8802)

(defconstant GL_STENCIL_BACK_PASS_DEPTH_PASS_ATI #x8803)

(defconstant GL_DEPTH_BOUNDS_TEST_EXT #x8890)

(defconstant GL_DEPTH_BOUNDS_EXT #x8891)

(defconstant GL_MIRROR_CLAMP_EXT #x8742)

(defconstant GL_MIRROR_CLAMP_TO_EDGE_EXT #x8743)

(defconstant GL_MIRROR_CLAMP_TO_BORDER_EXT #x8912)

(defconstant GL_BLEND_EQUATION_RGB_EXT #x8009)

(defconstant GL_BLEND_EQUATION_ALPHA_EXT #x883D)

(defconstant GL_PACK_INVERT_MESA #x8758)

(defconstant GL_UNSIGNED_SHORT_8_8_MESA #x85BA)

(defconstant GL_UNSIGNED_SHORT_8_8_REV_MESA #x85BB)

(defconstant GL_YCBCR_MESA #x8757)

(defconstant GL_VERSION_1_2 1)

(defconstant GL_VERSION_1_3 1)

(defconstant GL_VERSION_1_4 1)

(defconstant GL_VERSION_1_5 1)

(defconstant GL_ARB_multitexture 1)

(defconstant GL_ARB_transpose_matrix 1)

(defconstant GL_ARB_multisample 1)

(defconstant GL_ARB_texture_env_add 1)

(defconstant GL_ARB_texture_cube_map 1)

(defconstant GL_ARB_texture_compression 1)

(defconstant GL_ARB_texture_border_clamp 1)

(defconstant GL_ARB_point_parameters 1)

(defconstant GL_ARB_vertex_blend 1)

(defconstant GL_ARB_matrix_palette 1)

(defconstant GL_ARB_texture_env_combine 1)

(defconstant GL_ARB_texture_env_crossbar 1)

(defconstant GL_ARB_texture_env_dot3 1)

(defconstant GL_ARB_texture_mirror_repeat 1)

(defconstant GL_ARB_depth_texture 1)

(defconstant GL_ARB_shadow 1)

(defconstant GL_ARB_shadow_ambient 1)

(defconstant GL_ARB_window_pos 1)

(defconstant GL_ARB_vertex_program 1)

(defconstant GL_ARB_fragment_program 1)

(defconstant GL_ARB_vertex_buffer_object 1)

(defconstant GL_ARB_occlusion_query 1)

(defconstant GL_ARB_shader_objects 1)

(defconstant GL_ARB_vertex_shader 1)

(defconstant GL_ARB_fragment_shader 1)

(defconstant GL_ARB_shading_language_100 1)

(defconstant GL_ARB_texture_non_power_of_two 1)

(defconstant GL_ARB_point_sprite 1)

(defconstant GL_EXT_abgr 1)

(defconstant GL_EXT_blend_color 1)

(defconstant GL_EXT_polygon_offset 1)

(defconstant GL_EXT_texture 1)

(defconstant GL_EXT_texture3D 1)

(defconstant GL_SGIS_texture_filter4 1)

(defconstant GL_EXT_subtexture 1)

(defconstant GL_EXT_copy_texture 1)

(defconstant GL_EXT_histogram 1)

(defconstant GL_EXT_convolution 1)

(defconstant GL_EXT_color_matrix 1)

(defconstant GL_SGI_color_table 1)

(defconstant GL_SGIX_pixel_texture 1)

(defconstant GL_SGIS_pixel_texture 1)

(defconstant GL_SGIS_texture4D 1)

(defconstant GL_SGI_texture_color_table 1)

(defconstant GL_EXT_cmyka 1)

(defconstant GL_EXT_texture_object 1)

(defconstant GL_SGIS_detail_texture 1)

(defconstant GL_SGIS_sharpen_texture 1)

(defconstant GL_EXT_packed_pixels 1)

(defconstant GL_SGIS_texture_lod 1)

(defconstant GL_SGIS_multisample 1)

(defconstant GL_EXT_rescale_normal 1)

(defconstant GL_EXT_vertex_array 1)

(defconstant GL_EXT_misc_attribute 1)

(defconstant GL_SGIS_generate_mipmap 1)

(defconstant GL_SGIX_clipmap 1)

(defconstant GL_SGIX_shadow 1)

(defconstant GL_SGIS_texture_edge_clamp 1)

(defconstant GL_SGIS_texture_border_clamp 1)

(defconstant GL_EXT_blend_minmax 1)

(defconstant GL_EXT_blend_subtract 1)

(defconstant GL_EXT_blend_logic_op 1)

(defconstant GL_SGIX_interlace 1)

(defconstant GL_SGIX_pixel_tiles 1)

(defconstant GL_SGIX_texture_select 1)

(defconstant GL_SGIX_sprite 1)

(defconstant GL_SGIX_texture_multi_buffer 1)

(defconstant GL_EXT_point_parameters 1)

(defconstant GL_SGIS_point_parameters 1)

(defconstant GL_SGIX_instruments 1)

(defconstant GL_SGIX_texture_scale_bias 1)

(defconstant GL_SGIX_framezoom 1)

(defconstant GL_SGIX_tag_sample_buffer 1)

(defconstant GL_SGIX_polynomial_ffd 1)

(defconstant GL_SGIX_reference_plane 1)

(defconstant GL_SGIX_flush_raster 1)

(defconstant GL_SGIX_depth_texture 1)

(defconstant GL_SGIS_fog_function 1)

(defconstant GL_SGIX_fog_offset 1)

(defconstant GL_HP_image_transform 1)

(defconstant GL_HP_convolution_border_modes 1)

(defconstant GL_SGIX_texture_add_env 1)

(defconstant GL_EXT_color_subtable 1)

(defconstant GL_PGI_vertex_hints 1)

(defconstant GL_PGI_misc_hints 1)

(defconstant GL_EXT_paletted_texture 1)

(defconstant GL_EXT_clip_volume_hint 1)

(defconstant GL_SGIX_list_priority 1)

(defconstant GL_SGIX_ir_instrument1 1)

(defconstant GL_SGIX_calligraphic_fragment 1)

(defconstant GL_SGIX_texture_lod_bias 1)

(defconstant GL_SGIX_shadow_ambient 1)

(defconstant GL_EXT_index_texture 1)

(defconstant GL_EXT_index_material 1)

(defconstant GL_EXT_index_func 1)

(defconstant GL_EXT_index_array_formats 1)

(defconstant GL_EXT_compiled_vertex_array 1)

(defconstant GL_EXT_cull_vertex 1)

(defconstant GL_SGIX_ycrcb 1)

(defconstant GL_SGIX_fragment_lighting 1)

(defconstant GL_IBM_rasterpos_clip 1)

(defconstant GL_HP_texture_lighting 1)

(defconstant GL_EXT_draw_range_elements 1)

(defconstant GL_WIN_phong_shading 1)

(defconstant GL_WIN_specular_fog 1)

(defconstant GL_EXT_light_texture 1)

(defconstant GL_SGIX_blend_alpha_minmax 1)

(defconstant GL_EXT_bgra 1)

(defconstant GL_SGIX_async 1)

(defconstant GL_SGIX_async_pixel 1)

(defconstant GL_SGIX_async_histogram 1)

(defconstant GL_INTEL_parallel_arrays 1)

(defconstant GL_HP_occlusion_test 1)

(defconstant GL_EXT_pixel_transform 1)

(defconstant GL_EXT_pixel_transform_color_table 1)

(defconstant GL_EXT_shared_texture_palette 1)

(defconstant GL_EXT_separate_specular_color 1)

(defconstant GL_EXT_secondary_color 1)

(defconstant GL_EXT_texture_perturb_normal 1)

(defconstant GL_EXT_multi_draw_arrays 1)

(defconstant GL_EXT_fog_coord 1)

(defconstant GL_REND_screen_coordinates 1)

(defconstant GL_EXT_coordinate_frame 1)

(defconstant GL_EXT_texture_env_combine 1)

(defconstant GL_APPLE_specular_vector 1)

(defconstant GL_APPLE_transform_hint 1)

(defconstant GL_SGIX_fog_scale 1)

(defconstant GL_SUNX_constant_data 1)

(defconstant GL_SUN_global_alpha 1)

(defconstant GL_SUN_triangle_list 1)

(defconstant GL_SUN_vertex 1)

(defconstant GL_EXT_blend_func_separate 1)

(defconstant GL_INGR_blend_func_separate 1)

(defconstant GL_INGR_color_clamp 1)

(defconstant GL_INGR_interlace_read 1)

(defconstant GL_EXT_stencil_wrap 1)

(defconstant GL_EXT_422_pixels 1)

(defconstant GL_NV_texgen_reflection 1)

(defconstant GL_SUN_convolution_border_modes 1)

(defconstant GL_EXT_texture_env_add 1)

(defconstant GL_EXT_texture_lod_bias 1)

(defconstant GL_EXT_texture_filter_anisotropic 1)

(defconstant GL_EXT_vertex_weighting 1)

(defconstant GL_NV_light_max_exponent 1)

(defconstant GL_NV_vertex_array_range 1)

(defconstant GL_NV_register_combiners 1)

(defconstant GL_NV_fog_distance 1)

(defconstant GL_NV_texgen_emboss 1)

(defconstant GL_NV_blend_square 1)

(defconstant GL_NV_texture_env_combine4 1)

(defconstant GL_MESA_resize_buffers 1)

(defconstant GL_MESA_window_pos 1)

(defconstant GL_IBM_cull_vertex 1)

(defconstant GL_IBM_multimode_draw_arrays 1)

(defconstant GL_IBM_vertex_array_lists 1)

(defconstant GL_SGIX_subsample 1)

(defconstant GL_SGIX_ycrcba 1)

(defconstant GL_SGIX_ycrcb_subsample 1)

(defconstant GL_SGIX_depth_pass_instrument 1)

(defconstant GL_3DFX_texture_compression_FXT1 1)

(defconstant GL_3DFX_multisample 1)

(defconstant GL_3DFX_tbuffer 1)

(defconstant GL_EXT_multisample 1)

(defconstant GL_SGIX_vertex_preclip 1)

(defconstant GL_SGIX_convolution_accuracy 1)

(defconstant GL_SGIX_resample 1)

(defconstant GL_SGIS_point_line_texgen 1)

(defconstant GL_SGIS_texture_color_mask 1)

(defconstant GL_SGIX_igloo_interface 1)

(defconstant GL_EXT_texture_env_dot3 1)

(defconstant GL_ATI_texture_mirror_once 1)

(defconstant GL_NV_fence 1)

(defconstant GL_NV_evaluators 1)

(defconstant GL_NV_packed_depth_stencil 1)

(defconstant GL_NV_register_combiners2 1)

(defconstant GL_NV_texture_compression_vtc 1)

(defconstant GL_NV_texture_rectangle 1)

(defconstant GL_NV_texture_shader 1)

(defconstant GL_NV_texture_shader2 1)

(defconstant GL_NV_vertex_array_range2 1)

(defconstant GL_NV_vertex_program 1)

(defconstant GL_SGIX_texture_coordinate_clamp 1)

(defconstant GL_SGIX_scalebias_hint 1)

(defconstant GL_OML_interlace 1)

(defconstant GL_OML_subsample 1)

(defconstant GL_OML_resample 1)

(defconstant GL_NV_copy_depth_to_color 1)

(defconstant GL_ATI_envmap_bumpmap 1)

(defconstant GL_ATI_fragment_shader 1)

(defconstant GL_ATI_pn_triangles 1)

(defconstant GL_ATI_vertex_array_object 1)

(defconstant GL_EXT_vertex_shader 1)

(defconstant GL_ATI_vertex_streams 1)

(defconstant GL_ATI_element_array 1)

(defconstant GL_SUN_mesh_array 1)

(defconstant GL_SUN_slice_accum 1)

(defconstant GL_NV_multisample_filter_hint 1)

(defconstant GL_NV_depth_clamp 1)

(defconstant GL_NV_occlusion_query 1)

(defconstant GL_NV_point_sprite 1)

(defconstant GL_NV_texture_shader3 1)

(defconstant GL_NV_vertex_program1_1 1)

(defconstant GL_EXT_shadow_funcs 1)

(defconstant GL_EXT_stencil_two_side 1)

(defconstant GL_ATI_text_fragment_shader 1)

(defconstant GL_APPLE_client_storage 1)

(defconstant GL_APPLE_element_array 1)

(defconstant GL_APPLE_fence 1)

(defconstant GL_APPLE_vertex_array_object 1)

(defconstant GL_APPLE_vertex_array_range 1)

(defconstant GL_APPLE_ycbcr_422 1)

(defconstant GL_S3_s3tc 1)

(defconstant GL_ATI_draw_buffers 1)

(defconstant GL_ATI_texture_env_combine3 1)

(defconstant GL_ATI_texture_float 1)

(defconstant GL_NV_float_buffer 1)

(defconstant GL_NV_fragment_program 1)

(defconstant GL_NV_half_float 1)

(defconstant GL_NV_pixel_data_range 1)

(defconstant GL_NV_primitive_restart 1)

(defconstant GL_NV_texture_expand_normal 1)

(defconstant GL_NV_vertex_program2 1)

(defconstant GL_ATI_map_object_buffer 1)

(defconstant GL_ATI_separate_stencil 1)

(defconstant GL_ATI_vertex_attrib_array_object 1)

(defconstant GL_EXT_depth_bounds_test 1)

(defconstant GL_EXT_texture_mirror_clamp 1)

(defconstant GL_EXT_blend_equation_separate 1)

(defconstant GL_MESA_pack_invert 1)

(defconstant GL_MESA_ycbcr_texture 1)

(defcstruct SDL_RWops
	(seek :pointer)
	(read :pointer)
	(write :pointer)
	(close :pointer)
	(type :unsigned-int)
	(hidden :pointer))

(defcunion SDL_RWops_hidden
	(unknown :pointer)
	(mem :pointer)
	(stdio :pointer))

(defcstruct SDL_RWops_hidden_unknown
	(data1 :pointer))

(defcstruct SDL_RWops_hidden_mem
	(base :pointer)
	(here :pointer)
	(stop :pointer))

(defcstruct SDL_RWops_hidden_stdio
	(autoclose :int)
	(fp :pointer))

(defcfun ("SDL_RWFromFile" SDL_RWFromFile) :pointer
  (file :string)
  (mode :string))

(defcfun ("SDL_RWFromFP" SDL_RWFromFP) :pointer
  (fp :pointer)
  (autoclose :int))

(defcfun ("SDL_RWFromMem" SDL_RWFromMem) :pointer
  (mem :pointer)
  (size :int))

(defcfun ("SDL_RWFromConstMem" SDL_RWFromConstMem) :pointer
  (mem :pointer)
  (size :int))

(defcfun ("SDL_AllocRW" SDL_AllocRW) :pointer)

(defcfun ("SDL_FreeRW" SDL_FreeRW) :void
  (area :pointer))

(defcstruct SDL_SysWMmsg
	(version :pointer)
	(data :int))

(defcstruct SDL_SysWMinfo
	(version :pointer)
	(data :int))

(defcfun ("SDL_GetWMInfo" SDL_GetWMInfo) :int
  (info :pointer))

(defcfun ("SDL_CreateThread" SDL_CreateThread) :pointer
  (fn :pointer)
  (data :pointer))

(defcfun ("SDL_ThreadID" SDL_ThreadID) :unsigned-int)

(defcfun ("SDL_GetThreadID" SDL_GetThreadID) :unsigned-int
  (thread :pointer))

(defcfun ("SDL_WaitThread" SDL_WaitThread) :void
  (thread :pointer)
  (status :pointer))

(defcfun ("SDL_KillThread" SDL_KillThread) :void
  (thread :pointer))

(defconstant SDL_TIMESLICE 10)

(defconstant TIMER_RESOLUTION 10)

(defcfun ("SDL_GetTicks" SDL_GetTicks) :unsigned-int)

(defcfun ("SDL_Delay" SDL_Delay) :void
  (ms :unsigned-int))

(defcfun ("SDL_SetTimer" SDL_SetTimer) :int
  (interval :unsigned-int)
  (callback :pointer))

(defcfun ("SDL_AddTimer" SDL_AddTimer) :pointer
  (interval :unsigned-int)
  (callback :pointer)
  (param :pointer))

(defcfun ("SDL_RemoveTimer" SDL_RemoveTimer) :pointer
  (timer :pointer))

(defconstant SDL_MAJOR_VERSION 1)

(defconstant SDL_MINOR_VERSION 2)

(defconstant SDL_PATCHLEVEL 9)

(defcstruct SDL_version
	(major :unsigned-char)
	(minor :unsigned-char)
	(patch :unsigned-char))

(defconstant SDL_VERSIONNUM 1209)

(defconstant SDL_COMPILEDVERSION 1209)

(defconstant SDL_VERSION_ATLEAST 1209)

(defcfun ("SDL_Linked_Version" SDL_Linked_Version) :pointer)


