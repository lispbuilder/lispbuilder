
(in-package :lispbuilder-windows) 

(cl:defconstant CS_VREDRAW #x0001)

(cl:defconstant CS_HREDRAW #x0002)

(cl:defconstant CS_DBLCLKS #x0008)

(cl:defconstant CS_OWNDC #x0020)

(cl:defconstant CS_CLASSDC #x0040)

(cl:defconstant CS_PARENTDC #x0080)

(cl:defconstant CS_NOCLOSE #x0200)

(cl:defconstant CS_SAVEBITS #x0800)

(cl:defconstant CS_BYTEALIGNCLIENT #x1000)

(cl:defconstant CS_BYTEALIGNWINDOW #x2000)

(cl:defconstant CS_GLOBALCLASS #x4000)

(cffi:defcstruct WNDCLASS
  (style :unsigned-int)
  (lpfnWndProc :pointer)
  (cbClsExtra :int)
  (cbWndExtra :int)
  (hInstance :pointer)
  (hIcon :pointer)
  (hCursor :pointer)
  (hbrBackground :pointer)
  (lpszMenuName :string)
  (lpszClassName :string))

(cffi:defcfun ("CreateWindowExA" CreateWindowEx) :pointer
  (dwExStyle :unsigned-long)
  (lpClassName :string)
  (lpWindowName :string)
  (dwStyle :unsigned-long)
  (x :int)
  (y :int)
  (nWidth :int)
  (nHeight :int)
  (hWndParent :pointer)
  (hMenu :pointer)
  (hInstance :pointer)
  (lpParam :pointer))

(cffi:defcfun ("RegisterClassA" RegisterClass) :unsigned-short
  (lpWndClass :pointer))

(cffi:defcfun ("GetModuleHandleA" GetModuleHandle) :pointer
	(lpModuleName :string))

(cffi:defcfun ("DefWindowProcA" DefWindowProc) :long
  (hWnd :pointer)
  (Msg :unsigned-int)
  (wParam :unsigned-int)
  (lParam :long))

(cl:defconstant SW_HIDE 0)

(cl:defconstant SW_SHOWNORMAL 1)

(cl:defconstant SW_NORMAL 1)

(cl:defconstant SW_SHOWMINIMIZED 2)

(cl:defconstant SW_SHOWMAXIMIZED 3)

(cl:defconstant SW_MAXIMIZE 3)

(cl:defconstant SW_SHOWNOACTIVATE 4)

(cl:defconstant SW_SHOW 5)

(cl:defconstant SW_MINIMIZE 6)

(cl:defconstant SW_SHOWMINNOACTIVE 7)

(cl:defconstant SW_SHOWNA 8)

(cl:defconstant SW_RESTORE 9)

(cl:defconstant SW_SHOWDEFAULT 10)

(cl:defconstant SW_FORCEMINIMIZE 11)

(cl:defconstant SW_MAX 11)

(cffi:defcfun ("ShowWindow" ShowWindow) :int
  (hWnd :pointer)
  (nCmdShow :int))

(cffi:defcfun ("UpdateWindow" UpdateWindow) :int
  (hWnd :pointer))

(cl:defconstant WS_OVERLAPPED #x00000000)

(cl:defconstant WS_POPUP #x80000000)

(cl:defconstant WS_CHILD #x40000000)

(cl:defconstant WS_MINIMIZE #x20000000)

(cl:defconstant WS_VISIBLE #x10000000)

(cl:defconstant WS_DISABLED #x08000000)

(cl:defconstant WS_CLIPSIBLINGS #x04000000)

(cl:defconstant WS_CLIPCHILDREN #x02000000)

(cl:defconstant WS_MAXIMIZE #x01000000)

(cl:defconstant WS_CAPTION #x00C00000)

(cl:defconstant WS_BORDER #x00800000)

(cl:defconstant WS_DLGFRAME #x00400000)

(cl:defconstant WS_VSCROLL #x00200000)

(cl:defconstant WS_HSCROLL #x00100000)

(cl:defconstant WS_SYSMENU #x00080000)

(cl:defconstant WS_THICKFRAME #x00040000)

(cl:defconstant WS_GROUP #x00020000)

(cl:defconstant WS_TABSTOP #x00010000)

(cl:defconstant WS_MINIMIZEBOX #x00020000)

(cl:defconstant WS_MAXIMIZEBOX #x00010000)

(cl:defconstant WS_TILED #x00000000)

(cl:defconstant WS_ICONIC #x20000000)

(cl:defconstant WS_SIZEBOX #x00040000)

(cl:defconstant WS_OVERLAPPEDWINDOW (logior #x00000000 #x00C00000 #x00080000 #x00040000 #x00020000 #x00010000))

(cl:defconstant WS_POPUPWINDOW (logior #x80000000 #x00800000 #x00080000))

(cl:defconstant WS_CHILDWINDOW #x40000000)

(cffi:defcstruct POINT
  (x :long)
  (y :long))

(cffi:defcstruct MSG
  (hwnd :pointer)
  (message :unsigned-int)
  (wParam :unsigned-int)
  (lParam :long)
  (time :unsigned-long)
  (pt :pointer))

(cffi:defcfun ("TranslateMessage" TranslateMessage) :int
  (lpMsg :pointer))

(cffi:defcfun ("DispatchMessageA" DispatchMessage) :long
  (lpMsg :pointer))

(cffi:defcfun ("GetMessageA" GetMessage) :int
  (lpMsg :pointer)
  (hWnd :pointer)
  (wMsgFilterMin :unsigned-int)
  (wMsgFilterMax :unsigned-int))

(cffi:defcstruct RECT
  (left :long)
  (top :long)
  (right :long)
  (bottom :long))

(cffi:defcstruct PAINTSTRUCT
  (hdc :pointer)
  (fErase :int)
  (rcPaint :pointer)
  (fRestore :int)
  (fIncUpdate :int)
  (rgbReserved :pointer))

(cffi:defcfun ("BeginPaint" BeginPaint) :pointer
  (hwnd :pointer)
  (lpPaint :pointer))

(cffi:defcfun ("SelectObject" SelectObject) :pointer
  (hdc :pointer)
  (hgdiobj :pointer))

(cffi:defcfun ("GetStockObject" GetStockObject) :pointer
  (fnObject :int))

(cffi:defcfun ("MoveToEx" MoveToEx) :int
  (hdc :pointer)
  (X :int)
  (Y :int)
  (lpPoint :pointer))

(cffi:defcfun ("LineTo" LineTo) :int
  (hdc :pointer)
  (nXEnd :int)
  (nYEnd :int))

(cffi:defcfun ("EndPaint" EndPaint) :int
  (hWnd :pointer)
  (lpPaint :pointer))

(cffi:defcfun ("PostQuitMessage" PostQuitMessage) :void
  (nExitCode :int))

(cl:defconstant WHITE_BRUSH 0)

(cl:defconstant LTGRAY_BRUSH 1)

(cl:defconstant GRAY_BRUSH 2)

(cl:defconstant DKGRAY_BRUSH 3)

(cl:defconstant BLACK_BRUSH 4)

(cl:defconstant NULL_BRUSH 5)

(cl:defconstant HOLLOW_BRUSH 5)

(cl:defconstant WHITE_PEN 6)

(cl:defconstant BLACK_PEN 7)

(cl:defconstant NULL_PEN 8)

(cl:defconstant OEM_FIXED_FONT 10)

(cl:defconstant ANSI_FIXED_FONT 11)

(cl:defconstant ANSI_VAR_FONT 12)

(cl:defconstant SYSTEM_FONT 13)

(cl:defconstant DEVICE_DEFAULT_FONT 14)

(cl:defconstant DEFAULT_PALETTE 15)

(cl:defconstant SYSTEM_FIXED_FONT 16)

(cl:defconstant STOCK_LAST 16)

(cl:defconstant CLR_INVALID #xFFFFFFFF)

(cl:defconstant BS_SOLID 0)

(cl:defconstant BS_NULL 1)

(cl:defconstant BS_HOLLOW 1)

(cl:defconstant BS_HATCHED 2)

(cl:defconstant BS_PATTERN 3)

(cl:defconstant BS_INDEXED 4)

(cl:defconstant BS_DIBPATTERN 5)

(cl:defconstant BS_DIBPATTERNPT 6)

(cl:defconstant BS_PATTERN8X8 7)

(cl:defconstant BS_DIBPATTERN8X8 8)

(cl:defconstant BS_MONOPATTERN 9)

(cl:defconstant HS_HORIZONTAL 0)

(cl:defconstant HS_VERTICAL 1)

(cl:defconstant HS_FDIAGONAL 2)

(cl:defconstant HS_BDIAGONAL 3)

(cl:defconstant HS_CROSS 4)

(cl:defconstant HS_DIAGCROSS 5)

(cl:defconstant PS_SOLID 0)

(cl:defconstant PS_DASH 1)

(cl:defconstant PS_DOT 2)

(cl:defconstant PS_DASHDOT 3)

(cl:defconstant PS_DASHDOTDOT 4)

(cl:defconstant PS_NULL 5)

(cl:defconstant PS_INSIDEFRAME 6)

(cl:defconstant PS_USERSTYLE 7)

(cl:defconstant PS_ALTERNATE 8)

(cl:defconstant PS_STYLE_MASK #x0000000F)

(cl:defconstant PS_ENDCAP_ROUND #x00000000)

(cl:defconstant PS_ENDCAP_SQUARE #x00000100)

(cl:defconstant PS_ENDCAP_FLAT #x00000200)

(cl:defconstant PS_ENDCAP_MASK #x00000F00)

(cl:defconstant PS_JOIN_ROUND #x00000000)

(cl:defconstant PS_JOIN_BEVEL #x00001000)

(cl:defconstant PS_JOIN_MITER #x00002000)

(cl:defconstant PS_JOIN_MASK #x0000F000)

(cl:defconstant PS_COSMETIC #x00000000)

(cl:defconstant PS_GEOMETRIC #x00010000)

(cl:defconstant PS_TYPE_MASK #x000F0000)

(cl:defconstant AD_COUNTERCLOCKWISE 1)

(cl:defconstant AD_CLOCKWISE 2)

(cffi:defcfun ("MessageBoxA" MessageBox) :int
  (hWnd :pointer)
  (lpText :string)
  (lpCaption :string)
  (uType :unsigned-int))

(cl:defconstant MB_OK #x00000000)

(cl:defconstant MB_OKCANCEL #x00000001)

(cl:defconstant MB_ABORTRETRYIGNORE #x00000002)

(cl:defconstant MB_YESNOCANCEL #x00000003)

(cl:defconstant MB_YESNO #x00000004)

(cl:defconstant MB_RETRYCANCEL #x00000005)

(cl:defconstant MB_ICONHAND #x00000010)

(cl:defconstant MB_ICONQUESTION #x00000020)

(cl:defconstant MB_ICONEXCLAMATION #x00000030)

(cl:defconstant MB_ICONASTERISK #x00000040)

(cl:defconstant MB_ICONINFORMATION #x00000040)

(cl:defconstant MB_ICONSTOP #x00000010)

(cl:defconstant MB_DEFBUTTON1 #x00000000)

(cl:defconstant MB_DEFBUTTON2 #x00000100)

(cl:defconstant MB_DEFBUTTON3 #x00000200)

(cl:defconstant MB_APPLMODAL #x00000000)

(cl:defconstant MB_SYSTEMMODAL #x00001000)

(cl:defconstant MB_TASKMODAL #x00002000)

(cl:defconstant MB_NOFOCUS #x00008000)

(cl:defconstant MB_SETFOREGROUND #x00010000)

(cl:defconstant MB_DEFAULT_DESKTOP_ONLY #x00020000)

(cl:defconstant MB_TYPEMASK #x0000000F)

(cl:defconstant MB_ICONMASK #x000000F0)

(cl:defconstant MB_DEFMASK #x00000F00)

(cl:defconstant MB_MODEMASK #x00003000)

(cl:defconstant MB_MISCMASK #x0000C000)

(cl:defconstant WM_NULL #x0000)

(cl:defconstant WM_CREATE #x0001)

(cl:defconstant WM_DESTROY #x0002)

(cl:defconstant WM_MOVE #x0003)

(cl:defconstant WM_SIZE #x0005)

(cl:defconstant WM_ACTIVATE #x0006)

(cl:defconstant WA_INACTIVE 0)

(cl:defconstant WA_ACTIVE 1)

(cl:defconstant WA_CLICKACTIVE 2)

(cl:defconstant WM_SETFOCUS #x0007)

(cl:defconstant WM_KILLFOCUS #x0008)

(cl:defconstant WM_ENABLE #x000A)

(cl:defconstant WM_SETREDRAW #x000B)

(cl:defconstant WM_SETTEXT #x000C)

(cl:defconstant WM_GETTEXT #x000D)

(cl:defconstant WM_GETTEXTLENGTH #x000E)

(cl:defconstant WM_PAINT #x000F)

(cl:defconstant WM_CLOSE #x0010)

(cl:defconstant WM_QUERYENDSESSION #x0011)

(cl:defconstant WM_QUERYOPEN #x0013)

(cl:defconstant WM_ENDSESSION #x0016)

(cl:defconstant WM_QUIT #x0012)

(cl:defconstant WM_ERASEBKGND #x0014)

(cl:defconstant WM_SYSCOLORCHANGE #x0015)

(cl:defconstant WM_SHOWWINDOW #x0018)

(cl:defconstant WM_WININICHANGE #x001A)

(cl:defconstant WM_DEVMODECHANGE #x001B)

(cl:defconstant WM_ACTIVATEAPP #x001C)

(cl:defconstant WM_FONTCHANGE #x001D)

(cl:defconstant WM_TIMECHANGE #x001E)

(cl:defconstant WM_CANCELMODE #x001F)

(cl:defconstant WM_SETCURSOR #x0020)

(cl:defconstant WM_MOUSEACTIVATE #x0021)

(cl:defconstant WM_CHILDACTIVATE #x0022)

(cl:defconstant WM_QUEUESYNC #x0023)

(cl:defconstant WM_GETMINMAXINFO #x0024)

(cffi:defcstruct MINMAXINFO
  (ptReserved :pointer)
  (ptMaxSize :pointer)
  (ptMaxPosition :pointer)
  (ptMinTrackSize :pointer)
  (ptMaxTrackSize :pointer))

(cl:defconstant WM_PAINTICON #x0026)

(cl:defconstant WM_ICONERASEBKGND #x0027)

(cl:defconstant WM_NEXTDLGCTL #x0028)

(cl:defconstant WM_SPOOLERSTATUS #x002A)

(cl:defconstant WM_DRAWITEM #x002B)

(cl:defconstant WM_MEASUREITEM #x002C)

(cl:defconstant WM_DELETEITEM #x002D)

(cl:defconstant WM_VKEYTOITEM #x002E)

(cl:defconstant WM_CHARTOITEM #x002F)

(cl:defconstant WM_SETFONT #x0030)

(cl:defconstant WM_GETFONT #x0031)

(cl:defconstant WM_SETHOTKEY #x0032)

(cl:defconstant WM_GETHOTKEY #x0033)

(cl:defconstant WM_QUERYDRAGICON #x0037)

(cl:defconstant WM_COMPAREITEM #x0039)

(cl:defconstant WM_COMPACTING #x0041)

(cl:defconstant WM_COMMNOTIFY #x0044)

(cl:defconstant WM_WINDOWPOSCHANGING #x0046)

(cl:defconstant WM_WINDOWPOSCHANGED #x0047)

(cl:defconstant WM_POWER #x0048)

(cl:defconstant PWR_OK 1)

(cl:defconstant PWR_FAIL -1)

(cl:defconstant PWR_SUSPENDREQUEST 1)

(cl:defconstant PWR_SUSPENDRESUME 2)

(cl:defconstant PWR_CRITICALRESUME 3)

(cl:defconstant WM_COPYDATA #x004A)

(cl:defconstant WM_CANCELJOURNAL #x004B)

(cffi:defcstruct COPYDATASTRUCT
  (dwData :unsigned-long)
  (cbData :unsigned-long)
  (lpData :pointer))

(cl:defconstant WM_NCCREATE #x0081)

(cl:defconstant WM_NCDESTROY #x0082)

(cl:defconstant WM_NCCALCSIZE #x0083)

(cl:defconstant WM_NCHITTEST #x0084)

(cl:defconstant WM_NCPAINT #x0085)

(cl:defconstant WM_NCACTIVATE #x0086)

(cl:defconstant WM_GETDLGCODE #x0087)

(cl:defconstant WM_SYNCPAINT #x0088)

(cl:defconstant WM_NCMOUSEMOVE #x00A0)

(cl:defconstant WM_NCLBUTTONDOWN #x00A1)

(cl:defconstant WM_NCLBUTTONUP #x00A2)

(cl:defconstant WM_NCLBUTTONDBLCLK #x00A3)

(cl:defconstant WM_NCRBUTTONDOWN #x00A4)

(cl:defconstant WM_NCRBUTTONUP #x00A5)

(cl:defconstant WM_NCRBUTTONDBLCLK #x00A6)

(cl:defconstant WM_NCMBUTTONDOWN #x00A7)

(cl:defconstant WM_NCMBUTTONUP #x00A8)

(cl:defconstant WM_NCMBUTTONDBLCLK #x00A9)

(cl:defconstant WM_KEYFIRST #x0100)

(cl:defconstant WM_KEYDOWN #x0100)

(cl:defconstant WM_KEYUP #x0101)

(cl:defconstant WM_CHAR #x0102)

(cl:defconstant WM_DEADCHAR #x0103)

(cl:defconstant WM_SYSKEYDOWN #x0104)

(cl:defconstant WM_SYSKEYUP #x0105)

(cl:defconstant WM_SYSCHAR #x0106)

(cl:defconstant WM_SYSDEADCHAR #x0107)

(cl:defconstant WM_KEYLAST #x0108)

(cl:defconstant WM_INITDIALOG #x0110)

(cl:defconstant WM_COMMAND #x0111)

(cl:defconstant WM_SYSCOMMAND #x0112)

(cl:defconstant WM_TIMER #x0113)

(cl:defconstant WM_HSCROLL #x0114)

(cl:defconstant WM_VSCROLL #x0115)

(cl:defconstant WM_INITMENU #x0116)

(cl:defconstant WM_INITMENUPOPUP #x0117)

(cl:defconstant WM_MENUSELECT #x011F)

(cl:defconstant WM_MENUCHAR #x0120)

(cl:defconstant WM_ENTERIDLE #x0121)

(cl:defconstant WM_CTLCOLORMSGBOX #x0132)

(cl:defconstant WM_CTLCOLOREDIT #x0133)

(cl:defconstant WM_CTLCOLORLISTBOX #x0134)

(cl:defconstant WM_CTLCOLORBTN #x0135)

(cl:defconstant WM_CTLCOLORDLG #x0136)

(cl:defconstant WM_CTLCOLORSCROLLBAR #x0137)

(cl:defconstant WM_CTLCOLORSTATIC #x0138)

(cl:defconstant MN_GETHMENU #x01E1)

(cl:defconstant WM_MOUSEFIRST #x0200)

(cl:defconstant WM_MOUSEMOVE #x0200)

(cl:defconstant WM_LBUTTONDOWN #x0201)

(cl:defconstant WM_LBUTTONUP #x0202)

(cl:defconstant WM_LBUTTONDBLCLK #x0203)

(cl:defconstant WM_RBUTTONDOWN #x0204)

(cl:defconstant WM_RBUTTONUP #x0205)

(cl:defconstant WM_RBUTTONDBLCLK #x0206)

(cl:defconstant WM_MBUTTONDOWN #x0207)

(cl:defconstant WM_MBUTTONUP #x0208)

(cl:defconstant WM_MBUTTONDBLCLK #x0209)

(cl:defconstant WM_MOUSELAST #x0209)

(cl:defconstant WM_PARENTNOTIFY #x0210)

(cl:defconstant WM_ENTERMENULOOP #x0211)

(cl:defconstant WM_EXITMENULOOP #x0212)

(cl:defconstant WM_MDICREATE #x0220)

(cl:defconstant WM_MDIDESTROY #x0221)

(cl:defconstant WM_MDIACTIVATE #x0222)

(cl:defconstant WM_MDIRESTORE #x0223)

(cl:defconstant WM_MDINEXT #x0224)

(cl:defconstant WM_MDIMAXIMIZE #x0225)

(cl:defconstant WM_MDITILE #x0226)

(cl:defconstant WM_MDICASCADE #x0227)

(cl:defconstant WM_MDIICONARRANGE #x0228)

(cl:defconstant WM_MDIGETACTIVE #x0229)

(cl:defconstant WM_MDISETMENU #x0230)

(cl:defconstant WM_ENTERSIZEMOVE #x0231)

(cl:defconstant WM_EXITSIZEMOVE #x0232)

(cl:defconstant WM_DROPFILES #x0233)

(cl:defconstant WM_MDIREFRESHMENU #x0234)

(cl:defconstant WM_CUT #x0300)

(cl:defconstant WM_COPY #x0301)

(cl:defconstant WM_PASTE #x0302)

(cl:defconstant WM_CLEAR #x0303)

(cl:defconstant WM_UNDO #x0304)

(cl:defconstant WM_RENDERFORMAT #x0305)

(cl:defconstant WM_RENDERALLFORMATS #x0306)

(cl:defconstant WM_DESTROYCLIPBOARD #x0307)

(cl:defconstant WM_DRAWCLIPBOARD #x0308)

(cl:defconstant WM_PAINTCLIPBOARD #x0309)

(cl:defconstant WM_VSCROLLCLIPBOARD #x030A)

(cl:defconstant WM_SIZECLIPBOARD #x030B)

(cl:defconstant WM_ASKCBFORMATNAME #x030C)

(cl:defconstant WM_CHANGECBCHAIN #x030D)

(cl:defconstant WM_HSCROLLCLIPBOARD #x030E)

(cl:defconstant WM_QUERYNEWPALETTE #x030F)

(cl:defconstant WM_PALETTEISCHANGING #x0310)

(cl:defconstant WM_PALETTECHANGED #x0311)

(cl:defconstant WM_HOTKEY #x0312)

(cl:defconstant WM_PENWINFIRST #x0380)

(cl:defconstant WM_PENWINLAST #x038F)

(cl:defconstant WM_USER #x0400)

(cl:defconstant HTERROR -2)

(cl:defconstant HTTRANSPARENT -1)

(cl:defconstant HTNOWHERE 0)

(cl:defconstant HTCLIENT 1)

(cl:defconstant HTCAPTION 2)

(cl:defconstant HTSYSMENU 3)

(cl:defconstant HTGROWBOX 4)

(cl:defconstant HTSIZE 4)

(cl:defconstant HTMENU 5)

(cl:defconstant HTHSCROLL 6)

(cl:defconstant HTVSCROLL 7)

(cl:defconstant HTMINBUTTON 8)

(cl:defconstant HTMAXBUTTON 9)

(cl:defconstant HTLEFT 10)

(cl:defconstant HTRIGHT 11)

(cl:defconstant HTTOP 12)

(cl:defconstant HTTOPLEFT 13)

(cl:defconstant HTTOPRIGHT 14)

(cl:defconstant HTBOTTOM 15)

(cl:defconstant HTBOTTOMLEFT 16)

(cl:defconstant HTBOTTOMRIGHT 17)

(cl:defconstant HTBORDER 18)

(cl:defconstant HTREDUCE 8)

(cl:defconstant HTZOOM 9)

(cl:defconstant HTSIZEFIRST 10)

(cl:defconstant HTSIZELAST 17)

(cl:defconstant SMTO_NORMAL #x0000)

(cl:defconstant SMTO_BLOCK #x0001)

(cl:defconstant SMTO_ABORTIFHUNG #x0002)

(cl:defconstant MA_ACTIVATE 1)

(cl:defconstant MA_ACTIVATEANDEAT 2)

(cl:defconstant MA_NOACTIVATE 3)

(cl:defconstant MA_NOACTIVATEANDEAT 4)

(cl:defconstant ICON_SMALL 0)

(cl:defconstant ICON_BIG 1)

(cl:defconstant SIZE_RESTORED 0)

(cl:defconstant SIZE_MINIMIZED 1)

(cl:defconstant SIZE_MAXIMIZED 2)

(cl:defconstant SIZE_MAXSHOW 3)

(cl:defconstant SIZE_MAXHIDE 4)

(cl:defconstant SIZENORMAL 0)

(cl:defconstant SIZEICONIC 1)

(cl:defconstant SIZEFULLSCREEN 2)

(cl:defconstant SIZEZOOMSHOW 3)

(cl:defconstant SIZEZOOMHIDE 4)

(cffi:defcstruct WINDOWPOS
  (hwnd :pointer)
  (hwndInsertAfter :pointer)
  (x :int)
  (y :int)
  (cx :int)
  (cy :int)
  (flags :unsigned-int))

(cffi:defcstruct NCCALCSIZE_PARAMS
  (rgrc :pointer)
  (lppos :pointer))

(cl:defconstant WVR_ALIGNTOP #x0010)

(cl:defconstant WVR_ALIGNLEFT #x0020)

(cl:defconstant WVR_ALIGNBOTTOM #x0040)

(cl:defconstant WVR_ALIGNRIGHT #x0080)

(cl:defconstant WVR_HREDRAW #x0100)

(cl:defconstant WVR_VREDRAW #x0200)

(cl:defconstant WVR_REDRAW (logior #x0100 #x0200))

(cl:defconstant WVR_VALIDRECTS #x0400)

(cl:defconstant MK_LBUTTON #x0001)

(cl:defconstant MK_RBUTTON #x0002)

(cl:defconstant MK_SHIFT #x0004)

(cl:defconstant MK_CONTROL #x0008)

(cl:defconstant MK_MBUTTON #x0010)

(cffi:defcfun ("LoadCursorA" LoadCursor) :pointer
  (hInstance :pointer)
  (lpCursorName :int))

(cl:defconstant IDC_ARROW 32512)

(cl:defconstant IDC_IBEAM 32513)

(cl:defconstant IDC_WAIT 32514)

(cl:defconstant IDC_CROSS 32515)

(cl:defconstant IDC_UPARROW 32516)

(cl:defconstant IDC_SIZE 32640)

(cl:defconstant IDC_ICON 32641)

(cl:defconstant IDC_SIZENWSE 32642)

(cl:defconstant IDC_SIZENESW 32643)

(cl:defconstant IDC_SIZEWE 32644)

(cl:defconstant IDC_SIZENS 32645)

(cl:defconstant IDC_SIZEALL 32646)

(cl:defconstant IDC_NO 32648)

(cl:defconstant IDC_APPSTARTING 32650)

(cffi:defcfun ("GetClientRect" GetClientRect) :int
  (hWnd :pointer)
  (lpRect :pointer))

(cffi:defcfun ("MoveWindow" MoveWindow) :int
  (hWnd :pointer)
  (X :int)
  (Y :int)
  (nWidth :int)
  (nHeight :int)
  (bRepaint :int))

(cffi:defcstruct RGBQUAD
  (rgbBlue :char)
  (rgbGreen :char)
  (rgbRed :char)
  (rgbReserved :char))

(cffi:defcstruct BITMAPINFOHEADER
  (biSize :unsigned-long)
  (biWidth :long)
  (biHeight :long)
  (biPlanes :unsigned-short)
  (biBitCount :unsigned-short)
  (biCompression :unsigned-long)
  (biSizeImage :unsigned-long)
  (biXPelsPerMeter :long)
  (biYPelsPerMeter :long)
  (biClrUsed :unsigned-long)
  (biClrImportant :unsigned-long))

(cffi:defcstruct BITMAPINFO
  (bmiHeader BITMAPINFOHEADER)
  (bmiColors RGBQUAD))

(cffi:defcfun ("CreateCompatibleDC" CreateCompatibleDC) :pointer
  (hdc :pointer))

(cffi:defcfun ("CreateDIBSection" CreateDIBSection) :pointer
  (hdc :pointer)
  (pbmi :pointer)
  (iUsage :unsigned-int)
  (ppvBits :pointer)
  (hSection :pointer)
  (dwOffset :unsigned-long))

(cffi:defcfun ("BitBlt" BitBlt) :int
  (hdcDest :pointer)
  (nXDest :int)
  (nYDest :int)
  (nWidth :int)
  (nHeight :int)
  (hdcSrc :pointer)
  (nXSrc :int)
  (nYSrc :int)
  (dwRop :unsigned-long))

(cl:defconstant SRCCOPY #x00CC0020)

(cl:defconstant SRCPAINT #x00EE0086)

(cl:defconstant SRCAND #x008800C6)

(cl:defconstant SRCINVERT #x00660046)

(cl:defconstant SRCERASE #x00440328)

(cl:defconstant NOTSRCCOPY #x00330008)

(cl:defconstant NOTSRCERASE #x001100A6)

(cl:defconstant MERGECOPY #x00C000CA)

(cl:defconstant MERGEPAINT #x00BB0226)

(cl:defconstant PATCOPY #x00F00021)

(cl:defconstant PATPAINT #x00FB0A09)

(cl:defconstant PATINVERT #x005A0049)

(cl:defconstant DSTINVERT #x00550009)

(cl:defconstant BLACKNESS #x00000042)

(cl:defconstant WHITENESS #x00FF0062)

(cl:defconstant DIB_RGB_COLORS 0)

(cl:defconstant DIB_PAL_COLORS 1)

(cl:defconstant BI_RGB 0)

(cl:defconstant BI_RLE8 1)

(cl:defconstant BI_RLE4 2)

(cl:defconstant BI_BITFIELDS 3)

(cl:defconstant BI_JPEG 4)

(cl:defconstant BI_PNG 5)

(cffi:defcfun ("InvalidateRect" InvalidateRect) :int
  (hWnd :pointer)
  (lpRect :pointer)
  (bErase :int))

(cl:defconstant WS_EX_DLGMODALFRAME #x00000001)

(cl:defconstant WS_EX_NOPARENTNOTIFY #x00000004)

(cl:defconstant WS_EX_TOPMOST #x00000008)

(cl:defconstant WS_EX_ACCEPTFILES #x00000010)

(cl:defconstant WS_EX_TRANSPARENT #x00000020)

(cl:defconstant ES_LEFT #x0000)

(cl:defconstant ES_CENTER #x0001)

(cl:defconstant ES_RIGHT #x0002)

(cl:defconstant ES_MULTILINE #x0004)

(cl:defconstant ES_UPPERCASE #x0008)

(cl:defconstant ES_LOWERCASE #x0010)

(cl:defconstant ES_PASSWORD #x0020)

(cl:defconstant ES_AUTOVSCROLL #x0040)

(cl:defconstant ES_AUTOHSCROLL #x0080)

(cl:defconstant ES_NOHIDESEL #x0100)

(cl:defconstant ES_OEMCONVERT #x0400)

(cl:defconstant ES_READONLY #x0800)

(cl:defconstant ES_WANTRETURN #x1000)

(cl:defconstant EN_SETFOCUS #x0100)

(cl:defconstant EN_KILLFOCUS #x0200)

(cl:defconstant EN_CHANGE #x0300)

(cl:defconstant EN_UPDATE #x0400)

(cl:defconstant EN_ERRSPACE #x0500)

(cl:defconstant EN_MAXTEXT #x0501)

(cl:defconstant EN_HSCROLL #x0601)

(cl:defconstant EN_VSCROLL #x0602)

(cl:defconstant EM_GETSEL #x00B0)

(cl:defconstant EM_SETSEL #x00B1)

(cl:defconstant EM_GETRECT #x00B2)

(cl:defconstant EM_SETRECT #x00B3)

(cl:defconstant EM_SETRECTNP #x00B4)

(cl:defconstant EM_SCROLL #x00B5)

(cl:defconstant EM_LINESCROLL #x00B6)

(cl:defconstant EM_SCROLLCARET #x00B7)

(cl:defconstant EM_GETMODIFY #x00B8)

(cl:defconstant EM_SETMODIFY #x00B9)

(cl:defconstant EM_GETLINECOUNT #x00BA)

(cl:defconstant EM_LINEINDEX #x00BB)

(cl:defconstant EM_SETHANDLE #x00BC)

(cl:defconstant EM_GETHANDLE #x00BD)

(cl:defconstant EM_GETTHUMB #x00BE)

(cl:defconstant EM_LINELENGTH #x00C1)

(cl:defconstant EM_REPLACESEL #x00C2)

(cl:defconstant EM_GETLINE #x00C4)

(cl:defconstant EM_LIMITTEXT #x00C5)

(cl:defconstant EM_CANUNDO #x00C6)

(cl:defconstant EM_UNDO #x00C7)

(cl:defconstant EM_FMTLINES #x00C8)

(cl:defconstant EM_LINEFROMCHAR #x00C9)

(cl:defconstant EM_SETTABSTOPS #x00CB)

(cl:defconstant EM_SETPASSWORDCHAR #x00CC)

(cl:defconstant EM_EMPTYUNDOBUFFER #x00CD)

(cl:defconstant EM_GETFIRSTVISIBLELINE #x00CE)

(cl:defconstant EM_SETREADONLY #x00CF)

(cl:defconstant EM_SETWORDBREAKPROC #x00D0)

(cl:defconstant EM_GETWORDBREAKPROC #x00D1)

(cl:defconstant EM_GETPASSWORDCHAR #x00D2)

(cl:defconstant WB_LEFT 0)

(cl:defconstant WB_RIGHT 1)

(cl:defconstant WB_ISDELIMITER 2)

(cl:defconstant BS_PUSHBUTTON #x00000000)

(cl:defconstant BS_DEFPUSHBUTTON #x00000001)

(cl:defconstant BS_CHECKBOX #x00000002)

(cl:defconstant BS_AUTOCHECKBOX #x00000003)

(cl:defconstant BS_RADIOBUTTON #x00000004)

(cl:defconstant BS_3STATE #x00000005)

(cl:defconstant BS_AUTO3STATE #x00000006)

(cl:defconstant BS_GROUPBOX #x00000007)

(cl:defconstant BS_USERBUTTON #x00000008)

(cl:defconstant BS_AUTORADIOBUTTON #x00000009)

(cl:defconstant BS_PUSHBOX #x0000000A)

(cl:defconstant BS_OWNERDRAW #x0000000B)

(cl:defconstant BS_TYPEMASK #x0000000F)

(cl:defconstant BS_LEFTTEXT #x00000020)

(cl:defconstant BN_CLICKED 0)

(cl:defconstant BN_PAINT 1)

(cl:defconstant BN_HILITE 2)

(cl:defconstant BN_UNHILITE 3)

(cl:defconstant BN_DISABLE 4)

(cl:defconstant BN_DOUBLECLICKED 5)

(cl:defconstant BM_GETCHECK #x00F0)

(cl:defconstant BM_SETCHECK #x00F1)

(cl:defconstant BM_GETSTATE #x00F2)

(cl:defconstant BM_SETSTATE #x00F3)

(cl:defconstant BM_SETSTYLE #x00F4)

(cl:defconstant SS_LEFT #x00000000)

(cl:defconstant SS_CENTER #x00000001)

(cl:defconstant SS_RIGHT #x00000002)

(cl:defconstant SS_ICON #x00000003)

(cl:defconstant SS_BLACKRECT #x00000004)

(cl:defconstant SS_GRAYRECT #x00000005)

(cl:defconstant SS_WHITERECT #x00000006)

(cl:defconstant SS_BLACKFRAME #x00000007)

(cl:defconstant SS_GRAYFRAME #x00000008)

(cl:defconstant SS_WHITEFRAME #x00000009)

(cl:defconstant SS_USERITEM #x0000000A)

(cl:defconstant SS_SIMPLE #x0000000B)

(cl:defconstant SS_LEFTNOWORDWRAP #x0000000C)

(cl:defconstant SS_NOPREFIX #x00000080)

(cl:defconstant STM_SETICON #x0170)

(cl:defconstant STM_GETICON #x0171)

(cl:defconstant STM_MSGMAX #x0174)

(cffi:defcfun ("GetWindowTextA" GetWindowText) :int
  (hWnd :pointer)
  (lpString :string)
  (nMaxCount :int))

(cffi:defcfun ("SendMessageA" SendMessage) :long
  (hWnd :pointer)
  (Msg :unsigned-int)
  (wParam :unsigned-int)
  (lParam :long))

(cffi:defcfun ("SetTimer" SetTimer) :unsigned-int
  (hWnd :pointer)
  (nIDEvent :unsigned-int)
  (uElapse :unsigned-int)
  (lpTimerFunc :pointer))

(cffi:defcfun ("KillTimer" KillTimer) :int
  (hWnd :pointer)
  (uIDEvent :unsigned-int))

(cffi:defcfun ("FillRect" FillRect) :int
  (hDC :pointer)
  (lprc :pointer)
  (hbr :pointer))

(cffi:defcfun ("CreateSolidBrush" CreateSolidBrush) :pointer
  (crColor :unsigned-long))

(cffi:defcfun ("DeleteObject" DeleteObject) :int
  (hObject :pointer))

(cffi:defcfun ("CreateFontA" CreateFont) :pointer
  (nHeight :int)
  (nWidth :int)
  (nEscapement :int)
  (nOrientation :int)
  (fnWeight :int)
  (fdwItalic :unsigned-long)
  (fdwUnderline :unsigned-long)
  (fdwStrikeOut :unsigned-long)
  (fdwCharSet :unsigned-long)
  (fdwOutputPrecision :unsigned-long)
  (fdwClipPrecision :unsigned-long)
  (fdwQuality :unsigned-long)
  (fdwPitchAndFamily :unsigned-long)
  (lpszFace :string))

(cffi:defcfun ("ExtTextOutA" ExtTextOut) :int
  (hdc :pointer)
  (X :int)
  (Y :int)
  (fuOptions :unsigned-int)
  (lprc :pointer)
  (lpString :string)
  (cbCount :unsigned-int)
  (lpDx :pointer))

(cffi:defcfun ("SetMapMode" SetMapMode) :int
  (hdc :pointer)
  (fnMapMode :int))

(cl:defconstant OUT_DEFAULT_PRECIS 0)

(cl:defconstant OUT_STRING_PRECIS 1)

(cl:defconstant OUT_CHARACTER_PRECIS 2)

(cl:defconstant OUT_STROKE_PRECIS 3)

(cl:defconstant OUT_TT_PRECIS 4)

(cl:defconstant OUT_DEVICE_PRECIS 5)

(cl:defconstant OUT_RASTER_PRECIS 6)

(cl:defconstant OUT_TT_ONLY_PRECIS 7)

(cl:defconstant OUT_OUTLINE_PRECIS 8)

(cl:defconstant OUT_SCREEN_OUTLINE_PRECIS 9)

(cl:defconstant OUT_PS_ONLY_PRECIS 10)

(cl:defconstant CLIP_DEFAULT_PRECIS 0)

(cl:defconstant CLIP_CHARACTER_PRECIS 1)

(cl:defconstant CLIP_STROKE_PRECIS 2)

(cl:defconstant CLIP_MASK #xf)

(cl:defconstant CLIP_LH_ANGLES 16)

(cl:defconstant CLIP_TT_ALWAYS 32)

(cl:defconstant CLIP_EMBEDDED 128)

(cl:defconstant DEFAULT_QUALITY 0)

(cl:defconstant DRAFT_QUALITY 1)

(cl:defconstant PROOF_QUALITY 2)

(cl:defconstant DEFAULT_PITCH 0)

(cl:defconstant FIXED_PITCH 1)

(cl:defconstant VARIABLE_PITCH 2)

(cl:defconstant ANSI_CHARSET 0)

(cl:defconstant DEFAULT_CHARSET 1)

(cl:defconstant SYMBOL_CHARSET 2)

(cl:defconstant SHIFTJIS_CHARSET 128)

(cl:defconstant HANGEUL_CHARSET 129)

(cl:defconstant HANGUL_CHARSET 129)

(cl:defconstant GB2312_CHARSET 134)

(cl:defconstant CHINESEBIG5_CHARSET 136)

(cl:defconstant OEM_CHARSET 255)

(cl:defconstant FF_DONTCARE 0)

(cl:defconstant FF_ROMAN 16)

(cl:defconstant FF_SWISS 32)

(cl:defconstant FF_MODERN 64)

(cl:defconstant FF_SCRIPT 128)

(cl:defconstant FF_DECORATIVE 256)

(cl:defconstant FW_DONTCARE 0)

(cl:defconstant FW_THIN 100)

(cl:defconstant FW_EXTRALIGHT 200)

(cl:defconstant FW_LIGHT 300)

(cl:defconstant FW_NORMAL 400)

(cl:defconstant FW_MEDIUM 500)

(cl:defconstant FW_SEMIBOLD 600)

(cl:defconstant FW_BOLD 700)

(cl:defconstant FW_EXTRABOLD 800)

(cl:defconstant FW_HEAVY 900)

(cl:defconstant FW_ULTRALIGHT 200)

(cl:defconstant FW_REGULAR 400)

(cl:defconstant FW_DEMIBOLD 600)

(cl:defconstant FW_ULTRABOLD 800)

(cl:defconstant FW_BLACK 900)

(cl:defconstant PANOSE_COUNT 10)

(cl:defconstant PAN_FAMILYTYPE_INDEX 0)

(cl:defconstant PAN_SERIFSTYLE_INDEX 1)

(cl:defconstant PAN_WEIGHT_INDEX 2)

(cl:defconstant PAN_PROPORTION_INDEX 3)

(cl:defconstant PAN_CONTRAST_INDEX 4)

(cl:defconstant PAN_STROKEVARIATION_INDEX 5)

(cl:defconstant PAN_ARMSTYLE_INDEX 6)

(cl:defconstant PAN_LETTERFORM_INDEX 7)

(cl:defconstant PAN_MIDLINE_INDEX 8)

(cl:defconstant PAN_XHEIGHT_INDEX 9)

(cl:defconstant PAN_CULTURE_LATIN 0)

(cl:defconstant PAN_ANY 0)

(cl:defconstant PAN_NO_FIT 1)

(cl:defconstant PAN_FAMILY_TEXT_DISPLAY 2)

(cl:defconstant PAN_FAMILY_SCRIPT 3)

(cl:defconstant PAN_FAMILY_DECORATIVE 4)

(cl:defconstant PAN_FAMILY_PICTORIAL 5)

(cl:defconstant PAN_SERIF_COVE 2)

(cl:defconstant PAN_SERIF_OBTUSE_COVE 3)

(cl:defconstant PAN_SERIF_SQUARE_COVE 4)

(cl:defconstant PAN_SERIF_OBTUSE_SQUARE_COVE 5)

(cl:defconstant PAN_SERIF_SQUARE 6)

(cl:defconstant PAN_SERIF_THIN 7)

(cl:defconstant PAN_SERIF_BONE 8)

(cl:defconstant PAN_SERIF_EXAGGERATED 9)

(cl:defconstant PAN_SERIF_TRIANGLE 10)

(cl:defconstant PAN_SERIF_NORMAL_SANS 11)

(cl:defconstant PAN_SERIF_OBTUSE_SANS 12)

(cl:defconstant PAN_SERIF_PERP_SANS 13)

(cl:defconstant PAN_SERIF_FLARED 14)

(cl:defconstant PAN_SERIF_ROUNDED 15)

(cl:defconstant PAN_WEIGHT_VERY_LIGHT 2)

(cl:defconstant PAN_WEIGHT_LIGHT 3)

(cl:defconstant PAN_WEIGHT_THIN 4)

(cl:defconstant PAN_WEIGHT_BOOK 5)

(cl:defconstant PAN_WEIGHT_MEDIUM 6)

(cl:defconstant PAN_WEIGHT_DEMI 7)

(cl:defconstant PAN_WEIGHT_BOLD 8)

(cl:defconstant PAN_WEIGHT_HEAVY 9)

(cl:defconstant PAN_WEIGHT_BLACK 10)

(cl:defconstant PAN_WEIGHT_NORD 11)

(cl:defconstant PAN_PROP_OLD_STYLE 2)

(cl:defconstant PAN_PROP_MODERN 3)

(cl:defconstant PAN_PROP_EVEN_WIDTH 4)

(cl:defconstant PAN_PROP_EXPANDED 5)

(cl:defconstant PAN_PROP_CONDENSED 6)

(cl:defconstant PAN_PROP_VERY_EXPANDED 7)

(cl:defconstant PAN_PROP_VERY_CONDENSED 8)

(cl:defconstant PAN_PROP_MONOSPACED 9)

(cl:defconstant PAN_CONTRAST_NONE 2)

(cl:defconstant PAN_CONTRAST_VERY_LOW 3)

(cl:defconstant PAN_CONTRAST_LOW 4)

(cl:defconstant PAN_CONTRAST_MEDIUM_LOW 5)

(cl:defconstant PAN_CONTRAST_MEDIUM 6)

(cl:defconstant PAN_CONTRAST_MEDIUM_HIGH 7)

(cl:defconstant PAN_CONTRAST_HIGH 8)

(cl:defconstant PAN_CONTRAST_VERY_HIGH 9)

(cl:defconstant PAN_STROKE_GRADUAL_DIAG 2)

(cl:defconstant PAN_STROKE_GRADUAL_TRAN 3)

(cl:defconstant PAN_STROKE_GRADUAL_VERT 4)

(cl:defconstant PAN_STROKE_GRADUAL_HORZ 5)

(cl:defconstant PAN_STROKE_RAPID_VERT 6)

(cl:defconstant PAN_STROKE_RAPID_HORZ 7)

(cl:defconstant PAN_STROKE_INSTANT_VERT 8)

(cl:defconstant PAN_STRAIGHT_ARMS_HORZ 2)

(cl:defconstant PAN_STRAIGHT_ARMS_WEDGE 3)

(cl:defconstant PAN_STRAIGHT_ARMS_VERT 4)

(cl:defconstant PAN_STRAIGHT_ARMS_SINGLE_SERIF 5)

(cl:defconstant PAN_STRAIGHT_ARMS_DOUBLE_SERIF 6)

(cl:defconstant PAN_BENT_ARMS_HORZ 7)

(cl:defconstant PAN_BENT_ARMS_WEDGE 8)

(cl:defconstant PAN_BENT_ARMS_VERT 9)

(cl:defconstant PAN_BENT_ARMS_SINGLE_SERIF 10)

(cl:defconstant PAN_BENT_ARMS_DOUBLE_SERIF 11)

(cl:defconstant PAN_LETT_NORMAL_CONTACT 2)

(cl:defconstant PAN_LETT_NORMAL_WEIGHTED 3)

(cl:defconstant PAN_LETT_NORMAL_BOXED 4)

(cl:defconstant PAN_LETT_NORMAL_FLATTENED 5)

(cl:defconstant PAN_LETT_NORMAL_ROUNDED 6)

(cl:defconstant PAN_LETT_NORMAL_OFF_CENTER 7)

(cl:defconstant PAN_LETT_NORMAL_SQUARE 8)

(cl:defconstant PAN_LETT_OBLIQUE_CONTACT 9)

(cl:defconstant PAN_LETT_OBLIQUE_WEIGHTED 10)

(cl:defconstant PAN_LETT_OBLIQUE_BOXED 11)

(cl:defconstant PAN_LETT_OBLIQUE_FLATTENED 12)

(cl:defconstant PAN_LETT_OBLIQUE_ROUNDED 13)

(cl:defconstant PAN_LETT_OBLIQUE_OFF_CENTER 14)

(cl:defconstant PAN_LETT_OBLIQUE_SQUARE 15)

(cl:defconstant PAN_MIDLINE_STANDARD_TRIMMED 2)

(cl:defconstant PAN_MIDLINE_STANDARD_POINTED 3)

(cl:defconstant PAN_MIDLINE_STANDARD_SERIFED 4)

(cl:defconstant PAN_MIDLINE_HIGH_TRIMMED 5)

(cl:defconstant PAN_MIDLINE_HIGH_POINTED 6)

(cl:defconstant PAN_MIDLINE_HIGH_SERIFED 7)

(cl:defconstant PAN_MIDLINE_CONSTANT_TRIMMED 8)

(cl:defconstant PAN_MIDLINE_CONSTANT_POINTED 9)

(cl:defconstant PAN_MIDLINE_CONSTANT_SERIFED 10)

(cl:defconstant PAN_MIDLINE_LOW_TRIMMED 11)

(cl:defconstant PAN_MIDLINE_LOW_POINTED 12)

(cl:defconstant PAN_MIDLINE_LOW_SERIFED 13)

(cl:defconstant PAN_XHEIGHT_CONSTANT_SMALL 2)

(cl:defconstant PAN_XHEIGHT_CONSTANT_STD 3)

(cl:defconstant PAN_XHEIGHT_CONSTANT_LARGE 4)

(cl:defconstant PAN_XHEIGHT_DUCKING_SMALL 5)

(cl:defconstant PAN_XHEIGHT_DUCKING_STD 6)

(cl:defconstant PAN_XHEIGHT_DUCKING_LARGE 7)

(cl:defconstant ELF_VENDOR_SIZE 4)

(cl:defconstant ELF_VERSION 0)

(cl:defconstant ELF_CULTURE_LATIN 0)

(cl:defconstant RASTER_FONTTYPE #x0001)

(cl:defconstant DEVICE_FONTTYPE #x002)

(cl:defconstant TRUETYPE_FONTTYPE #x004)

(cl:defconstant PC_RESERVED #x01)

(cl:defconstant PC_EXPLICIT #x02)

(cl:defconstant PC_NOCOLLAPSE #x04)

(cl:defconstant TRANSPARENT 1)

(cl:defconstant OPAQUE 2)

(cl:defconstant BKMODE_LAST 2)

(cl:defconstant GM_COMPATIBLE 1)

(cl:defconstant GM_ADVANCED 2)

(cl:defconstant GM_LAST 2)

(cl:defconstant PT_CLOSEFIGURE #x01)

(cl:defconstant PT_LINETO #x02)

(cl:defconstant PT_BEZIERTO #x04)

(cl:defconstant PT_MOVETO #x06)

(cl:defconstant MM_TEXT 1)

(cl:defconstant MM_LOMETRIC 2)

(cl:defconstant MM_HIMETRIC 3)

(cl:defconstant MM_LOENGLISH 4)

(cl:defconstant MM_HIENGLISH 5)

(cl:defconstant MM_TWIPS 6)

(cl:defconstant MM_ISOTROPIC 7)

(cl:defconstant MM_ANISOTROPIC 8)

(cl:defconstant MM_MIN 1)

(cl:defconstant MM_MAX 8)

(cl:defconstant MM_MAX_FIXEDSCALE 6)

(cl:defconstant ABSOLUTE 1)

(cl:defconstant RELATIVE 2)

(cl:defconstant ETO_OPAQUE #x0002)

(cl:defconstant ETO_CLIPPED #x0004)

(cffi:defcfun ("SetBkColor" SetBkColor) :unsigned-long
  (hdc :pointer)
  (crColor :unsigned-long))

(cffi:defcfun ("GetSystemMetrics" GetSystemMetrics) :int
  (nIndex :int))

(cl:defconstant SM_CXSCREEN 0)

(cl:defconstant SM_CYSCREEN 1)

(cl:defconstant SM_CXVSCROLL 2)

(cl:defconstant SM_CYHSCROLL 3)

(cl:defconstant SM_CYCAPTION 4)

(cl:defconstant SM_CXBORDER 5)

(cl:defconstant SM_CYBORDER 6)

(cl:defconstant SM_CXDLGFRAME 7)

(cl:defconstant SM_CYDLGFRAME 8)

(cl:defconstant SM_CYVTHUMB 9)

(cl:defconstant SM_CXHTHUMB 10)

(cl:defconstant SM_CXICON 11)

(cl:defconstant SM_CYICON 12)

(cl:defconstant SM_CXCURSOR 13)

(cl:defconstant SM_CYCURSOR 14)

(cl:defconstant SM_CYMENU 15)

(cl:defconstant SM_CXFULLSCREEN 16)

(cl:defconstant SM_CYFULLSCREEN 17)

(cl:defconstant SM_CYKANJIWINDOW 18)

(cl:defconstant SM_MOUSEPRESENT 19)

(cl:defconstant SM_CYVSCROLL 20)

(cl:defconstant SM_CXHSCROLL 21)

(cl:defconstant SM_DEBUG 22)

(cl:defconstant SM_SWAPBUTTON 23)

(cl:defconstant SM_RESERVED1 24)

(cl:defconstant SM_RESERVED2 25)

(cl:defconstant SM_RESERVED3 26)

(cl:defconstant SM_RESERVED4 27)

(cl:defconstant SM_CXMIN 28)

(cl:defconstant SM_CYMIN 29)

(cl:defconstant SM_CXSIZE 30)

(cl:defconstant SM_CYSIZE 31)

(cl:defconstant SM_CXFRAME 32)

(cl:defconstant SM_CYFRAME 33)

(cl:defconstant SM_CXMINTRACK 34)

(cl:defconstant SM_CYMINTRACK 35)

(cl:defconstant SM_CXDOUBLECLK 36)

(cl:defconstant SM_CYDOUBLECLK 37)

(cl:defconstant SM_CXICONSPACING 38)

(cl:defconstant SM_CYICONSPACING 39)

(cl:defconstant SM_MENUDROPALIGNMENT 40)

(cl:defconstant SM_PENWINDOWS 41)

(cl:defconstant SM_DBCSENABLED 42)

(cl:defconstant SM_CMOUSEBUTTONS 43)

(cl:defconstant SM_SHOWSOUNDS 70)

(cl:defconstant SM_CMETRICS 76)
