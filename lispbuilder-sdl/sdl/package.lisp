;;; -*- lisp -*-

(in-package #:cl-user)

(defpackage #:lispbuilder-sdl
  (:use #:cl #:cffi)
  (:nicknames #:sdl)
  (:documentation "The main package of `lispbuilder-sdl'.")
  (:export

   ;; These are exports from util-sdl.lisp
   #:draw-rect #:draw-rect-end-points #:random+1 #:draw-random-rect #:copy-surf-to-rect #:blit-surface
   #:make-sdl-rect #:get-surface-rect #:load-bmp #:convert-surface-to-display-format #:with-surface-lock
   #:put-pixel #:get-pixel #:create-surface #:is-valid-ptr #:get-video-info #:is-key #:is-modifier
   #:new-event #:push-quitevent #:set-timescale #:get-timescale #:set-worldtime #:get-worldtime
   #:set-framerate #:get-framerate #:with-events #:set-flags #:init-sdl #:with-init #:to-radian #:to-degree
   #:sdl-must-lock #:with-must-locksurface #:set-screen #:set-window #:pixelformat #:update-surface #:fill-surface
   #:apply-surface #:apply-surface-free #:set-colorkey #:clear-colorkey #:moveto-rectangle #:moveby-rectangle
   #:rect-x #:rect-y #:rect-w #:rect-h #:new-rect #:rectangle #:copy-rectangle #:surf-w #:surf-h #:get-native-window
   
   ;; These are exports from sdlswig.i
   #:SDL_GetError #:SDL_ClearError #:SDL_errorcode #:SDL_Error #:SDL_RWops #:seek
   #:read #:write #:close #:type #:hidden #:SDL_RWops_hidden
   #:unknown #:mem #:stdio #:SDL_RWops_hidden_unknown #:data1 #:SDL_RWops_hidden_mem
   #:base #:here #:stop #:SDL_RWops_hidden_stdio #:autoclose #:fp
   #:SDL_RWFromFile #:SDL_RWFromFP #:SDL_RWFromMem #:SDL_RWFromConstMem #:SDL_AllocRW #:SDL_FreeRW
   #:SDL_TIMESLICE #:TIMER_RESOLUTION #:SDL_GetTicks #:SDL_Delay #:SDL_SetTimer #:SDL_AddTimer
   #:SDL_RemoveTimer #:SDL_AudioSpec #:freq #:format #:channels #:silence
   #:samples #:padding #:size #:callback #:userdata #:AUDIO_U8
   #:AUDIO_S8 #:AUDIO_U16LSB #:AUDIO_S16LSB #:AUDIO_U16MSB #:AUDIO_S16MSB #:AUDIO_U16
   #:AUDIO_S16 #:SDL_AudioCVT #:needed #:src_format #:dst_format #:rate_incr
   #:buf #:len #:len_cvt #:len_mult #:len_ratio #:filters
   #:filter_index #:SDL_AudioInit #:SDL_AudioQuit #:SDL_AudioDriverName #:SDL_OpenAudio #:SDL_audiostatus
   #:SDL_GetAudioStatus #:SDL_PauseAudio #:SDL_LoadWAV_RW #:SDL_FreeWAV #:SDL_BuildAudioCVT #:SDL_ConvertAudio
   #:SDL_MIX_MAXVOLUME #:SDL_MixAudio #:SDL_LockAudio #:SDL_UnlockAudio #:SDL_CloseAudio #:SDL_MAX_TRACKS
   #:SDL_AUDIO_TRACK #:SDL_DATA_TRACK #:CDstatus #:SDL_CDtrack #:id #:type
   #:unused #:length #:offset #:SDL_CD #:id #:status
   #:numtracks #:cur_track #:cur_frame #:track #:SDL_CDNumDrives #:SDL_CDName
   #:SDL_CDOpen #:SDL_CDStatus #:SDL_CDPlayTracks #:SDL_CDPlay #:SDL_CDPause #:SDL_CDResume
   #:SDL_CDStop #:SDL_CDEject #:SDL_CDClose #:SDL_NumJoysticks #:SDL_JoystickName #:SDL_JoystickOpen
   #:SDL_JoystickOpened #:SDL_JoystickIndex #:SDL_JoystickNumAxes #:SDL_JoystickNumBalls #:SDL_JoystickNumHats #:SDL_JoystickNumButtons
   #:SDL_JoystickUpdate #:SDL_JoystickEventState #:SDL_JoystickGetAxis #:SDL_HAT_CENTERED #:SDL_HAT_UP #:SDL_HAT_RIGHT
   #:SDL_HAT_DOWN #:SDL_HAT_LEFT #:SDL_JoystickGetHat #:SDL_JoystickGetBall #:SDL_JoystickGetButton #:SDL_JoystickClose
   #:SDL_APPMOUSEFOCUS #:SDL_APPINPUTFOCUS #:SDL_APPACTIVE #:SDL_GetAppState #:SDLKey #:SDLMod
   #:SDL_ALL_HOTKEYS #:SDL_EnableUNICODE #:SDL_DEFAULT_REPEAT_DELAY #:SDL_DEFAULT_REPEAT_INTERVAL #:SDL_EnableKeyRepeat #:SDL_GetKeyState
   #:SDL_GetModState #:SDL_SetModState #:SDL_GetKeyName #:SDL_Cursor #:area #:hot_x
   #:hot_y #:data #:mask #:save #:wm_cursor #:SDL_GetMouseState
   #:SDL_GetRelativeMouseState #:SDL_WarpMouse #:SDL_CreateCursor #:SDL_SetCursor #:SDL_GetCursor #:SDL_FreeCursor
   #:SDL_ShowCursor #:SDL_BUTTON_LEFT #:SDL_BUTTON_MIDDLE #:SDL_BUTTON_RIGHT #:SDL_BUTTON_WHEELUP #:SDL_BUTTON_WHEELDOWN
   #:SDL_NOEVENT #:SDL_ACTIVEEVENT #:SDL_KEYDOWN #:SDL_KEYUP #:SDL_MOUSEMOTION #:SDL_MOUSEBUTTONDOWN
   #:SDL_MOUSEBUTTONUP #:SDL_JOYAXISMOTION #:SDL_JOYBALLMOTION #:SDL_JOYHATMOTION #:SDL_JOYBUTTONDOWN #:SDL_JOYBUTTONUP
   #:SDL_QUIT #:SDL_SYSWMEVENT #:SDL_EVENT_RESERVEDA #:SDL_EVENT_RESERVEDB #:SDL_VIDEORESIZE #:SDL_VIDEOEXPOSE
   #:SDL_EVENT_RESERVED2 #:SDL_EVENT_RESERVED3 #:SDL_EVENT_RESERVED4 #:SDL_EVENT_RESERVED5 #:SDL_EVENT_RESERVED6 #:SDL_EVENT_RESERVED7
   #:SDL_USEREVENT #:SDL_NUMEVENTS #:SDL_ALLEVENTS #:SDL_ActiveEvent #:type #:gain
   #:state #:SDL_MouseMotionEvent #:type #:which #:state #:x
   #:y #:xrel #:yrel #:SDL_MouseButtonEvent #:type #:which
   #:button #:state #:x #:y #:SDL_JoyAxisEvent #:type
   #:which #:axis #:value #:SDL_JoyBallEvent #:type #:which
   #:ball #:xrel #:yrel #:SDL_JoyHatEvent #:type #:which
   #:hat #:value #:SDL_JoyButtonEvent #:type #:which #:button
   #:state #:SDL_ResizeEvent #:type #:w #:h #:SDL_ExposeEvent
   #:type #:SDL_QuitEvent #:type #:SDL_UserEvent #:type #:code
   #:data1 #:data2 #:SDL_SysWMEvent #:type #:msg #:SDL_PumpEvents
   #:SDL_eventaction #:SDL_PeepEvents #:SDL_PollEvent #:SDL_WaitEvent #:SDL_PushEvent #:SDL_SetEventFilter
   #:SDL_GetEventFilter #:SDL_QUERY #:SDL_IGNORE #:SDL_DISABLE #:SDL_ENABLE #:SDL_EventState
   #:SDL_MUTEX_TIMEDOUT #:SDL_CreateMutex #:SDL_mutexP #:SDL_mutexV #:SDL_DestroyMutex #:SDL_CreateSemaphore
   #:SDL_DestroySemaphore #:SDL_SemWait #:SDL_SemTryWait #:SDL_SemWaitTimeout #:SDL_SemPost #:SDL_SemValue
   #:SDL_CreateCond #:SDL_DestroyCond #:SDL_CondSignal #:SDL_CondBroadcast #:SDL_CondWait #:SDL_CondWaitTimeout
   #:SDL_ALPHA_OPAQUE #:SDL_ALPHA_TRANSPARENT #:SDL_Rect #:x #:y #:w
   #:h #:SDL_Color #:r #:g #:b #:unused
   #:SDL_Palette #:ncolors #:colors #:SDL_PixelFormat #:palette #:BitsPerPixel
   #:BytesPerPixel #:Rloss #:Gloss #:Bloss #:Aloss #:Rshift
   #:Gshift #:Bshift #:Ashift #:Rmask #:Gmask #:Bmask
   #:Amask #:colorkey #:alpha #:SDL_Surface #:flags #:format
   #:w #:h #:pitch #:pixels #:offset #:hwdata
   #:clip_rect #:unused1 #:locked #:map #:format_version #:refcount
   #:SDL_SWSURFACE #:SDL_HWSURFACE #:SDL_ASYNCBLIT #:SDL_ANYFORMAT #:SDL_HWPALETTE #:SDL_DOUBLEBUF
   #:SDL_FULLSCREEN #:SDL_OPENGL #:SDL_OPENGLBLIT #:SDL_RESIZABLE #:SDL_NOFRAME #:SDL_HWACCEL
   #:SDL_SRCCOLORKEY #:SDL_RLEACCELOK #:SDL_RLEACCEL #:SDL_SRCALPHA #:SDL_PREALLOC #:SDL_YV12_OVERLAY
   #:SDL_IYUV_OVERLAY #:SDL_YUY2_OVERLAY #:SDL_UYVY_OVERLAY #:SDL_YVYU_OVERLAY #:SDL_Overlay #:format
   #:w #:h #:planes #:pitches #:pixels #:hwfuncs
   #:hwdata #:hw_overlay #:UnusedBits #:SDL_GLattr #:SDL_LOGPAL #:SDL_PHYSPAL
   #:SDL_VideoInit #:SDL_VideoQuit #:SDL_VideoDriverName #:SDL_GetVideoSurface #:SDL_GetVideoInfo #:SDL_VideoModeOK
   #:SDL_ListModes #:SDL_SetVideoMode #:SDL_UpdateRects #:SDL_UpdateRect #:SDL_Flip #:SDL_SetGamma
   #:SDL_SetGammaRamp #:SDL_GetGammaRamp #:SDL_SetColors #:SDL_SetPalette #:SDL_MapRGB #:SDL_MapRGBA
   #:SDL_GetRGB #:SDL_GetRGBA #:SDL_CreateRGBSurface #:SDL_CreateRGBSurfaceFrom #:SDL_FreeSurface #:SDL_LockSurface
   #:SDL_UnlockSurface #:SDL_LoadBMP_RW #:SDL_SaveBMP_RW #:SDL_SetColorKey #:SDL_SetAlpha #:SDL_SetClipRect
   #:SDL_GetClipRect #:SDL_ConvertSurface #:SDL_UpperBlit #:SDL_LowerBlit #:SDL_FillRect #:SDL_DisplayFormat
   #:SDL_DisplayFormatAlpha #:SDL_CreateYUVOverlay #:SDL_LockYUVOverlay #:SDL_UnlockYUVOverlay #:SDL_DisplayYUVOverlay #:SDL_FreeYUVOverlay
   #:SDL_GL_LoadLibrary #:SDL_GL_GetProcAddress #:SDL_GL_SwapBuffers #:SDL_GL_UpdateRects #:SDL_GL_Lock #:SDL_GL_Unlock
   #:SDL_WM_SetCaption #:SDL_WM_GetCaption #:SDL_WM_SetIcon #:SDL_WM_IconifyWindow #:SDL_WM_ToggleFullScreen #:SDL_GrabMode
   #:SDL_WM_GrabInput #:SDL_SoftStretch #:SDL_MAJOR_VERSION #:SDL_MINOR_VERSION #:SDL_PATCHLEVEL #:SDL_version
   #:major #:minor #:patch #:SDL_Linked_Version #:SDL_INIT_TIMER #:SDL_INIT_AUDIO
   #:SDL_INIT_VIDEO #:SDL_INIT_CDROM #:SDL_INIT_JOYSTICK #:SDL_INIT_NOPARACHUTE #:SDL_INIT_EVENTTHREAD #:SDL_INIT_EVERYTHING
   #:SDL_Init #:SDL_InitSubSystem #:SDL_QuitSubSystem #:SDL_WasInit #:SDL_Quit

   ;; These are exports from post-swig.lisp
   #:hardware-flags #:SDL_VideoInfo #:video_mem #:vfmt #:SDL_keysym #:scancode #:sym #:mod #:unicode
   #:SDL_bool #:SDL_PRESSED #:SDL_RELEASED #:SDL_GL_SetAttribute #:attr #:value
   #:SDL_GL_GetAttribute
   #:SDL_KeyboardEvent #:type #:which #:state #:keysym
   #:SDL_Event #:active-event #:keyboard-event #:mouse-motion-event #:mouse-button-event #:joy-axis-event
   #:joy-ball-event #:joy-hat-event #:joy-button-event #:resize-event #:expose-event #:quit-event #:user-event
   #:sys-wm-event
   #:SDL_HAT_RIGHTUP #:SDL_HAT_RIGHTDOWN #:SDL_HAT_LEFTUP #:SDL_HAT_LEFTDOWN
   #:KMOD_CTRL #:KMOD_SHIFT #:KMOD_ALT #:KMOD_META
   #:SDL_SysWMmsg #:version #:hwnd #:msg #:wParam #:lParam
   #:SDL_SysWMinfo #:version #:window #:hglrc

   #:SDL_SYSWM_TYPE #:SDL_SysWMmsg_event #:xevent #:SDL_SysWMmsg #:version #:subsystem #:event
   #:SDL_SysWMinfo_info_x11 #:display #:window #:lock_func #:unlock_func #:fswindow #:wmwindow
   #:SDL_SysWMinfo_info #:x11
   #:SDL_SysWMinfo #:version #:subsystem #:info
   #:SDL_GetWMInfo #:info

   #:SDL_VERSION #:SDL_VERSIONNUM #:SDL_COMPILEDVERSION #:SDL_VERSION_ATLEAST #:SDL_LockMutex #:SDL_UnlockMutex
   #:SDL_MUSTLOCK
   #:SDL_LoadBMP #:SDL_SaveBMP #:SDL_BlitSurface
   #:CD_INDRIVE #:CD_FPS #:FRAMES_TO_MSF #:MSF_TO_FRAMES
   #:SDL_LoadWAV #:SDL_OutOfMemory
   #:SDL_BUTTON #:SDL_BUTTON_LMASK #:SDL_BUTTON_MMASK #:SDL_BUTTON_RMASK #:SDL_EVENTMASK #:SDL_ACTIVEEVENTMASK
   #:SDL_KEYDOWNMASK #:SDL_KEYUPMASK #:SDL_MOUSEMOTIONMASK #:SDL_MOUSEBUTTONDOWNMASK #:SDL_MOUSEBUTTONUPMASK
   #:SDL_MOUSEEVENTMASK #:SDL_JOYAXISMOTIONMASK #:SDL_JOYBALLMOTIONMASK #:SDL_JOYHATMOTIONMASK
   #:SDL_JOYBUTTONDOWNMASK #:SDL_JOYBUTTONUPMASK #:SDL_JOYEVENTMASK #:SDL_VIDEORESIZEMASK #:SDL_VIDEOEXPOSEMASK
   #:SDL_QUITMASK #:SDL_SYSWMEVENTMASK 

   ))