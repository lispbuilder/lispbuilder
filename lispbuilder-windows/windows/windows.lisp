(in-package :lispbuilder-windows) 

(defconstant CS_VREDRAW #x0001)

(defconstant CS_HREDRAW #x0002)

(defconstant CS_DBLCLKS #x0008)

(defconstant CS_OWNDC #x0020)

(defconstant CS_CLASSDC #x0040)

(defconstant CS_PARENTDC #x0080)

(defconstant CS_NOCLOSE #x0200)

(defconstant CS_SAVEBITS #x0800)

(defconstant CS_BYTEALIGNCLIENT #x1000)

(defconstant CS_BYTEALIGNWINDOW #x2000)

(defconstant CS_GLOBALCLASS #x4000)

(defcstruct WNDCLASS
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

(defcfun ("CreateWindowExA" CreateWindowEx) :pointer
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

(defcfun ("RegisterClassA" RegisterClass) :unsigned-short
  (lpWndClass :pointer))

(defcfun ("GetModuleHandleA" GetModuleHandle) :pointer
  (lpModuleName :string))

(defcfun ("DefWindowProcA" DefWindowProc) :long
  (hWnd :pointer)
  (Msg :unsigned-int)
  (wParam :unsigned-int)
  (lParam :long))

(defconstant SW_HIDE 0)

(defconstant SW_SHOWNORMAL 1)

(defconstant SW_NORMAL 1)

(defconstant SW_SHOWMINIMIZED 2)

(defconstant SW_SHOWMAXIMIZED 3)

(defconstant SW_MAXIMIZE 3)

(defconstant SW_SHOWNOACTIVATE 4)

(defconstant SW_SHOW 5)

(defconstant SW_MINIMIZE 6)

(defconstant SW_SHOWMINNOACTIVE 7)

(defconstant SW_SHOWNA 8)

(defconstant SW_RESTORE 9)

(defconstant SW_SHOWDEFAULT 10)

(defconstant SW_FORCEMINIMIZE 11)

(defconstant SW_MAX 11)

(defcfun ("ShowWindow" ShowWindow) :int
  (hWnd :pointer)
  (nCmdShow :int))

(defcfun ("UpdateWindow" UpdateWindow) :int
  (hWnd :pointer))

(defconstant WS_OVERLAPPED #x00000000)

(defconstant WS_POPUP #x80000000)

(defconstant WS_CHILD #x40000000)

(defconstant WS_MINIMIZE #x20000000)

(defconstant WS_VISIBLE #x10000000)

(defconstant WS_DISABLED #x08000000)

(defconstant WS_CLIPSIBLINGS #x04000000)

(defconstant WS_CLIPCHILDREN #x02000000)

(defconstant WS_MAXIMIZE #x01000000)

(defconstant WS_CAPTION #x00C00000)

(defconstant WS_BORDER #x00800000)

(defconstant WS_DLGFRAME #x00400000)

(defconstant WS_VSCROLL #x00200000)

(defconstant WS_HSCROLL #x00100000)

(defconstant WS_SYSMENU #x00080000)

(defconstant WS_THICKFRAME #x00040000)

(defconstant WS_GROUP #x00020000)

(defconstant WS_TABSTOP #x00010000)

(defconstant WS_MINIMIZEBOX #x00020000)

(defconstant WS_MAXIMIZEBOX #x00010000)

(defconstant WS_TILED #x00000000)

(defconstant WS_ICONIC #x20000000)

(defconstant WS_SIZEBOX #x00040000)

(defconstant WS_OVERLAPPEDWINDOW (logior #x00000000 #x00C00000 #x00080000 #x00040000 #x00020000 #x00010000))

(defconstant WS_POPUPWINDOW (logior #x80000000 #x00800000 #x00080000))

(defconstant WS_CHILDWINDOW #x40000000)

(defcstruct POINT
  (x :long)
  (y :long))

(defcstruct MSG
  (hwnd :pointer)
  (message :unsigned-int)
  (wParam :unsigned-int)
  (lParam :long)
  (time :unsigned-long)
  (pt :pointer))

(defcfun ("TranslateMessage" TranslateMessage) :int
  (lpMsg :pointer))

(defcfun ("DispatchMessageA" DispatchMessage) :long
  (lpMsg :pointer))

(defcfun ("GetMessageA" GetMessage) :int
  (lpMsg :pointer)
  (hWnd :pointer)
  (wMsgFilterMin :unsigned-int)
  (wMsgFilterMax :unsigned-int))

(defcstruct RECT
  (left :long)
  (top :long)
  (right :long)
  (bottom :long))

(defcstruct PAINTSTRUCT
  (hdc :pointer)
  (fErase :int)
  (rcPaint :pointer)
  (fRestore :int)
  (fIncUpdate :int)
  (rgbReserved :pointer))

(defcfun ("BeginPaint" BeginPaint) :pointer
  (hwnd :pointer)
  (lpPaint :pointer))

(defcfun ("SelectObject" SelectObject) :pointer
  (hdc :pointer)
  (hgdiobj :pointer))

(defcfun ("GetStockObject" GetStockObject) :pointer
  (fnObject :int))

(defcfun ("MoveToEx" MoveToEx) :int
  (hdc :pointer)
  (X :int)
  (Y :int)
  (lpPoint :pointer))

(defcfun ("LineTo" LineTo) :int
  (hdc :pointer)
  (nXEnd :int)
  (nYEnd :int))

(defcfun ("EndPaint" EndPaint) :int
  (hWnd :pointer)
  (lpPaint :pointer))

(defcfun ("PostQuitMessage" PostQuitMessage) :void
  (nExitCode :int))

(defconstant WHITE_BRUSH 0)

(defconstant LTGRAY_BRUSH 1)

(defconstant GRAY_BRUSH 2)

(defconstant DKGRAY_BRUSH 3)

(defconstant BLACK_BRUSH 4)

(defconstant NULL_BRUSH 5)

(defconstant HOLLOW_BRUSH 5)

(defconstant WHITE_PEN 6)

(defconstant BLACK_PEN 7)

(defconstant NULL_PEN 8)

(defconstant OEM_FIXED_FONT 10)

(defconstant ANSI_FIXED_FONT 11)

(defconstant ANSI_VAR_FONT 12)

(defconstant SYSTEM_FONT 13)

(defconstant DEVICE_DEFAULT_FONT 14)

(defconstant DEFAULT_PALETTE 15)

(defconstant SYSTEM_FIXED_FONT 16)

(defconstant STOCK_LAST 16)

(defconstant CLR_INVALID #xFFFFFFFF)

(defconstant BS_SOLID 0)

(defconstant BS_NULL 1)

(defconstant BS_HOLLOW 1)

(defconstant BS_HATCHED 2)

(defconstant BS_PATTERN 3)

(defconstant BS_INDEXED 4)

(defconstant BS_DIBPATTERN 5)

(defconstant BS_DIBPATTERNPT 6)

(defconstant BS_PATTERN8X8 7)

(defconstant BS_DIBPATTERN8X8 8)

(defconstant BS_MONOPATTERN 9)

(defconstant HS_HORIZONTAL 0)

(defconstant HS_VERTICAL 1)

(defconstant HS_FDIAGONAL 2)

(defconstant HS_BDIAGONAL 3)

(defconstant HS_CROSS 4)

(defconstant HS_DIAGCROSS 5)

(defconstant PS_SOLID 0)

(defconstant PS_DASH 1)

(defconstant PS_DOT 2)

(defconstant PS_DASHDOT 3)

(defconstant PS_DASHDOTDOT 4)

(defconstant PS_NULL 5)

(defconstant PS_INSIDEFRAME 6)

(defconstant PS_USERSTYLE 7)

(defconstant PS_ALTERNATE 8)

(defconstant PS_STYLE_MASK #x0000000F)

(defconstant PS_ENDCAP_ROUND #x00000000)

(defconstant PS_ENDCAP_SQUARE #x00000100)

(defconstant PS_ENDCAP_FLAT #x00000200)

(defconstant PS_ENDCAP_MASK #x00000F00)

(defconstant PS_JOIN_ROUND #x00000000)

(defconstant PS_JOIN_BEVEL #x00001000)

(defconstant PS_JOIN_MITER #x00002000)

(defconstant PS_JOIN_MASK #x0000F000)

(defconstant PS_COSMETIC #x00000000)

(defconstant PS_GEOMETRIC #x00010000)

(defconstant PS_TYPE_MASK #x000F0000)

(defconstant AD_COUNTERCLOCKWISE 1)

(defconstant AD_CLOCKWISE 2)

(defcfun ("MessageBoxA" MessageBox) :int
  (hWnd :pointer)
  (lpText :string)
  (lpCaption :string)
  (uType :unsigned-int))

(defconstant MB_OK #x00000000)

(defconstant MB_OKCANCEL #x00000001)

(defconstant MB_ABORTRETRYIGNORE #x00000002)

(defconstant MB_YESNOCANCEL #x00000003)

(defconstant MB_YESNO #x00000004)

(defconstant MB_RETRYCANCEL #x00000005)

(defconstant MB_ICONHAND #x00000010)

(defconstant MB_ICONQUESTION #x00000020)

(defconstant MB_ICONEXCLAMATION #x00000030)

(defconstant MB_ICONASTERISK #x00000040)

(defconstant MB_ICONINFORMATION #x00000040)

(defconstant MB_ICONSTOP #x00000010)

(defconstant MB_DEFBUTTON1 #x00000000)

(defconstant MB_DEFBUTTON2 #x00000100)

(defconstant MB_DEFBUTTON3 #x00000200)

(defconstant MB_APPLMODAL #x00000000)

(defconstant MB_SYSTEMMODAL #x00001000)

(defconstant MB_TASKMODAL #x00002000)

(defconstant MB_NOFOCUS #x00008000)

(defconstant MB_SETFOREGROUND #x00010000)

(defconstant MB_DEFAULT_DESKTOP_ONLY #x00020000)

(defconstant MB_TYPEMASK #x0000000F)

(defconstant MB_ICONMASK #x000000F0)

(defconstant MB_DEFMASK #x00000F00)

(defconstant MB_MODEMASK #x00003000)

(defconstant MB_MISCMASK #x0000C000)

(defconstant WM_NULL #x0000)

(defconstant WM_CREATE #x0001)

(defconstant WM_DESTROY #x0002)

(defconstant WM_MOVE #x0003)

(defconstant WM_SIZE #x0005)

(defconstant WM_ACTIVATE #x0006)

(defconstant WA_INACTIVE 0)

(defconstant WA_ACTIVE 1)

(defconstant WA_CLICKACTIVE 2)

(defconstant WM_SETFOCUS #x0007)

(defconstant WM_KILLFOCUS #x0008)

(defconstant WM_ENABLE #x000A)

(defconstant WM_SETREDRAW #x000B)

(defconstant WM_SETTEXT #x000C)

(defconstant WM_GETTEXT #x000D)

(defconstant WM_GETTEXTLENGTH #x000E)

(defconstant WM_PAINT #x000F)

(defconstant WM_CLOSE #x0010)

(defconstant WM_QUERYENDSESSION #x0011)

(defconstant WM_QUERYOPEN #x0013)

(defconstant WM_ENDSESSION #x0016)

(defconstant WM_QUIT #x0012)

(defconstant WM_ERASEBKGND #x0014)

(defconstant WM_SYSCOLORCHANGE #x0015)

(defconstant WM_SHOWWINDOW #x0018)

(defconstant WM_WININICHANGE #x001A)

(defconstant WM_DEVMODECHANGE #x001B)

(defconstant WM_ACTIVATEAPP #x001C)

(defconstant WM_FONTCHANGE #x001D)

(defconstant WM_TIMECHANGE #x001E)

(defconstant WM_CANCELMODE #x001F)

(defconstant WM_SETCURSOR #x0020)

(defconstant WM_MOUSEACTIVATE #x0021)

(defconstant WM_CHILDACTIVATE #x0022)

(defconstant WM_QUEUESYNC #x0023)

(defconstant WM_GETMINMAXINFO #x0024)

(defcstruct MINMAXINFO
  (ptReserved :pointer)
  (ptMaxSize :pointer)
  (ptMaxPosition :pointer)
  (ptMinTrackSize :pointer)
  (ptMaxTrackSize :pointer))

(defconstant WM_PAINTICON #x0026)

(defconstant WM_ICONERASEBKGND #x0027)

(defconstant WM_NEXTDLGCTL #x0028)

(defconstant WM_SPOOLERSTATUS #x002A)

(defconstant WM_DRAWITEM #x002B)

(defconstant WM_MEASUREITEM #x002C)

(defconstant WM_DELETEITEM #x002D)

(defconstant WM_VKEYTOITEM #x002E)

(defconstant WM_CHARTOITEM #x002F)

(defconstant WM_SETFONT #x0030)

(defconstant WM_GETFONT #x0031)

(defconstant WM_SETHOTKEY #x0032)

(defconstant WM_GETHOTKEY #x0033)

(defconstant WM_QUERYDRAGICON #x0037)

(defconstant WM_COMPAREITEM #x0039)

(defconstant WM_COMPACTING #x0041)

(defconstant WM_COMMNOTIFY #x0044)

(defconstant WM_WINDOWPOSCHANGING #x0046)

(defconstant WM_WINDOWPOSCHANGED #x0047)

(defconstant WM_POWER #x0048)

(defconstant PWR_OK 1)

(defconstant PWR_FAIL -1)

(defconstant PWR_SUSPENDREQUEST 1)

(defconstant PWR_SUSPENDRESUME 2)

(defconstant PWR_CRITICALRESUME 3)

(defconstant WM_COPYDATA #x004A)

(defconstant WM_CANCELJOURNAL #x004B)

(defcstruct COPYDATASTRUCT
  (dwData :unsigned-long)
  (cbData :unsigned-long)
  (lpData :pointer))

(defconstant WM_NCCREATE #x0081)

(defconstant WM_NCDESTROY #x0082)

(defconstant WM_NCCALCSIZE #x0083)

(defconstant WM_NCHITTEST #x0084)

(defconstant WM_NCPAINT #x0085)

(defconstant WM_NCACTIVATE #x0086)

(defconstant WM_GETDLGCODE #x0087)

(defconstant WM_SYNCPAINT #x0088)

(defconstant WM_NCMOUSEMOVE #x00A0)

(defconstant WM_NCLBUTTONDOWN #x00A1)

(defconstant WM_NCLBUTTONUP #x00A2)

(defconstant WM_NCLBUTTONDBLCLK #x00A3)

(defconstant WM_NCRBUTTONDOWN #x00A4)

(defconstant WM_NCRBUTTONUP #x00A5)

(defconstant WM_NCRBUTTONDBLCLK #x00A6)

(defconstant WM_NCMBUTTONDOWN #x00A7)

(defconstant WM_NCMBUTTONUP #x00A8)

(defconstant WM_NCMBUTTONDBLCLK #x00A9)

(defconstant WM_KEYFIRST #x0100)

(defconstant WM_KEYDOWN #x0100)

(defconstant WM_KEYUP #x0101)

(defconstant WM_CHAR #x0102)

(defconstant WM_DEADCHAR #x0103)

(defconstant WM_SYSKEYDOWN #x0104)

(defconstant WM_SYSKEYUP #x0105)

(defconstant WM_SYSCHAR #x0106)

(defconstant WM_SYSDEADCHAR #x0107)

(defconstant WM_KEYLAST #x0108)

(defconstant WM_INITDIALOG #x0110)

(defconstant WM_COMMAND #x0111)

(defconstant WM_SYSCOMMAND #x0112)

(defconstant WM_TIMER #x0113)

(defconstant WM_HSCROLL #x0114)

(defconstant WM_VSCROLL #x0115)

(defconstant WM_INITMENU #x0116)

(defconstant WM_INITMENUPOPUP #x0117)

(defconstant WM_MENUSELECT #x011F)

(defconstant WM_MENUCHAR #x0120)

(defconstant WM_ENTERIDLE #x0121)

(defconstant WM_CTLCOLORMSGBOX #x0132)

(defconstant WM_CTLCOLOREDIT #x0133)

(defconstant WM_CTLCOLORLISTBOX #x0134)

(defconstant WM_CTLCOLORBTN #x0135)

(defconstant WM_CTLCOLORDLG #x0136)

(defconstant WM_CTLCOLORSCROLLBAR #x0137)

(defconstant WM_CTLCOLORSTATIC #x0138)

(defconstant MN_GETHMENU #x01E1)

(defconstant WM_MOUSEFIRST #x0200)

(defconstant WM_MOUSEMOVE #x0200)

(defconstant WM_LBUTTONDOWN #x0201)

(defconstant WM_LBUTTONUP #x0202)

(defconstant WM_LBUTTONDBLCLK #x0203)

(defconstant WM_RBUTTONDOWN #x0204)

(defconstant WM_RBUTTONUP #x0205)

(defconstant WM_RBUTTONDBLCLK #x0206)

(defconstant WM_MBUTTONDOWN #x0207)

(defconstant WM_MBUTTONUP #x0208)

(defconstant WM_MBUTTONDBLCLK #x0209)

(defconstant WM_MOUSELAST #x0209)

(defconstant WM_PARENTNOTIFY #x0210)

(defconstant WM_ENTERMENULOOP #x0211)

(defconstant WM_EXITMENULOOP #x0212)

(defconstant WM_MDICREATE #x0220)

(defconstant WM_MDIDESTROY #x0221)

(defconstant WM_MDIACTIVATE #x0222)

(defconstant WM_MDIRESTORE #x0223)

(defconstant WM_MDINEXT #x0224)

(defconstant WM_MDIMAXIMIZE #x0225)

(defconstant WM_MDITILE #x0226)

(defconstant WM_MDICASCADE #x0227)

(defconstant WM_MDIICONARRANGE #x0228)

(defconstant WM_MDIGETACTIVE #x0229)

(defconstant WM_MDISETMENU #x0230)

(defconstant WM_ENTERSIZEMOVE #x0231)

(defconstant WM_EXITSIZEMOVE #x0232)

(defconstant WM_DROPFILES #x0233)

(defconstant WM_MDIREFRESHMENU #x0234)

(defconstant WM_CUT #x0300)

(defconstant WM_COPY #x0301)

(defconstant WM_PASTE #x0302)

(defconstant WM_CLEAR #x0303)

(defconstant WM_UNDO #x0304)

(defconstant WM_RENDERFORMAT #x0305)

(defconstant WM_RENDERALLFORMATS #x0306)

(defconstant WM_DESTROYCLIPBOARD #x0307)

(defconstant WM_DRAWCLIPBOARD #x0308)

(defconstant WM_PAINTCLIPBOARD #x0309)

(defconstant WM_VSCROLLCLIPBOARD #x030A)

(defconstant WM_SIZECLIPBOARD #x030B)

(defconstant WM_ASKCBFORMATNAME #x030C)

(defconstant WM_CHANGECBCHAIN #x030D)

(defconstant WM_HSCROLLCLIPBOARD #x030E)

(defconstant WM_QUERYNEWPALETTE #x030F)

(defconstant WM_PALETTEISCHANGING #x0310)

(defconstant WM_PALETTECHANGED #x0311)

(defconstant WM_HOTKEY #x0312)

(defconstant WM_PENWINFIRST #x0380)

(defconstant WM_PENWINLAST #x038F)

(defconstant WM_USER #x0400)

(defconstant HTERROR -2)

(defconstant HTTRANSPARENT -1)

(defconstant HTNOWHERE 0)

(defconstant HTCLIENT 1)

(defconstant HTCAPTION 2)

(defconstant HTSYSMENU 3)

(defconstant HTGROWBOX 4)

(defconstant HTSIZE 4)

(defconstant HTMENU 5)

(defconstant HTHSCROLL 6)

(defconstant HTVSCROLL 7)

(defconstant HTMINBUTTON 8)

(defconstant HTMAXBUTTON 9)

(defconstant HTLEFT 10)

(defconstant HTRIGHT 11)

(defconstant HTTOP 12)

(defconstant HTTOPLEFT 13)

(defconstant HTTOPRIGHT 14)

(defconstant HTBOTTOM 15)

(defconstant HTBOTTOMLEFT 16)

(defconstant HTBOTTOMRIGHT 17)

(defconstant HTBORDER 18)

(defconstant HTREDUCE 8)

(defconstant HTZOOM 9)

(defconstant HTSIZEFIRST 10)

(defconstant HTSIZELAST 17)

(defconstant SMTO_NORMAL #x0000)

(defconstant SMTO_BLOCK #x0001)

(defconstant SMTO_ABORTIFHUNG #x0002)

(defconstant MA_ACTIVATE 1)

(defconstant MA_ACTIVATEANDEAT 2)

(defconstant MA_NOACTIVATE 3)

(defconstant MA_NOACTIVATEANDEAT 4)

(defconstant ICON_SMALL 0)

(defconstant ICON_BIG 1)

(defconstant SIZE_RESTORED 0)

(defconstant SIZE_MINIMIZED 1)

(defconstant SIZE_MAXIMIZED 2)

(defconstant SIZE_MAXSHOW 3)

(defconstant SIZE_MAXHIDE 4)

(defconstant SIZENORMAL 0)

(defconstant SIZEICONIC 1)

(defconstant SIZEFULLSCREEN 2)

(defconstant SIZEZOOMSHOW 3)

(defconstant SIZEZOOMHIDE 4)

(defcstruct WINDOWPOS
  (hwnd :pointer)
  (hwndInsertAfter :pointer)
  (x :int)
  (y :int)
  (cx :int)
  (cy :int)
  (flags :unsigned-int))

(defcstruct NCCALCSIZE_PARAMS
  (rgrc :pointer)
  (lppos :pointer))

(defconstant WVR_ALIGNTOP #x0010)

(defconstant WVR_ALIGNLEFT #x0020)

(defconstant WVR_ALIGNBOTTOM #x0040)

(defconstant WVR_ALIGNRIGHT #x0080)

(defconstant WVR_HREDRAW #x0100)

(defconstant WVR_VREDRAW #x0200)

(defconstant WVR_REDRAW (logior #x0100 #x0200))

(defconstant WVR_VALIDRECTS #x0400)

(defconstant MK_LBUTTON #x0001)

(defconstant MK_RBUTTON #x0002)

(defconstant MK_SHIFT #x0004)

(defconstant MK_CONTROL #x0008)

(defconstant MK_MBUTTON #x0010)

(defcfun ("LoadCursorA" LoadCursor) :pointer
  (hInstance :pointer)
  (lpCursorName :int))

(defconstant IDC_ARROW 32512)

(defconstant IDC_IBEAM 32513)

(defconstant IDC_WAIT 32514)

(defconstant IDC_CROSS 32515)

(defconstant IDC_UPARROW 32516)

(defconstant IDC_SIZE 32640)

(defconstant IDC_ICON 32641)

(defconstant IDC_SIZENWSE 32642)

(defconstant IDC_SIZENESW 32643)

(defconstant IDC_SIZEWE 32644)

(defconstant IDC_SIZENS 32645)

(defconstant IDC_SIZEALL 32646)

(defconstant IDC_NO 32648)

(defconstant IDC_APPSTARTING 32650)

(defcfun ("GetClientRect" GetClientRect) :int
  (hWnd :pointer)
  (lpRect :pointer))

(defcfun ("MoveWindow" MoveWindow) :int
  (hWnd :pointer)
  (X :int)
  (Y :int)
  (nWidth :int)
  (nHeight :int)
  (bRepaint :int))

(defcstruct RGBQUAD
  (rgbBlue :char)
  (rgbGreen :char)
  (rgbRed :char)
  (rgbReserved :char))

(defcstruct BITMAPINFOHEADER
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

(defcstruct BITMAPINFO
  (bmiHeader BITMAPINFOHEADER)
  (bmiColors RGBQUAD))

(defcfun ("CreateCompatibleDC" CreateCompatibleDC) :pointer
  (hdc :pointer))

(defcfun ("CreateDIBSection" CreateDIBSection) :pointer
  (hdc :pointer)
  (pbmi :pointer)
  (iUsage :unsigned-int)
  (ppvBits :pointer)
  (hSection :pointer)
  (dwOffset :unsigned-long))

(defcfun ("BitBlt" BitBlt) :int
  (hdcDest :pointer)
  (nXDest :int)
  (nYDest :int)
  (nWidth :int)
  (nHeight :int)
  (hdcSrc :pointer)
  (nXSrc :int)
  (nYSrc :int)
  (dwRop :unsigned-long))

(defconstant SRCCOPY #x00CC0020)

(defconstant SRCPAINT #x00EE0086)

(defconstant SRCAND #x008800C6)

(defconstant SRCINVERT #x00660046)

(defconstant SRCERASE #x00440328)

(defconstant NOTSRCCOPY #x00330008)

(defconstant NOTSRCERASE #x001100A6)

(defconstant MERGECOPY #x00C000CA)

(defconstant MERGEPAINT #x00BB0226)

(defconstant PATCOPY #x00F00021)

(defconstant PATPAINT #x00FB0A09)

(defconstant PATINVERT #x005A0049)

(defconstant DSTINVERT #x00550009)

(defconstant BLACKNESS #x00000042)

(defconstant WHITENESS #x00FF0062)

(defconstant DIB_RGB_COLORS 0)

(defconstant DIB_PAL_COLORS 1)

(defconstant BI_RGB 0)

(defconstant BI_RLE8 1)

(defconstant BI_RLE4 2)

(defconstant BI_BITFIELDS 3)

(defconstant BI_JPEG 4)

(defconstant BI_PNG 5)

(defcfun ("InvalidateRect" InvalidateRect) :int
  (hWnd :pointer)
  (lpRect :pointer)
  (bErase :int))

(defconstant WS_EX_DLGMODALFRAME #x00000001)

(defconstant WS_EX_NOPARENTNOTIFY #x00000004)

(defconstant WS_EX_TOPMOST #x00000008)

(defconstant WS_EX_ACCEPTFILES #x00000010)

(defconstant WS_EX_TRANSPARENT #x00000020)

(defconstant ES_LEFT #x0000)

(defconstant ES_CENTER #x0001)

(defconstant ES_RIGHT #x0002)

(defconstant ES_MULTILINE #x0004)

(defconstant ES_UPPERCASE #x0008)

(defconstant ES_LOWERCASE #x0010)

(defconstant ES_PASSWORD #x0020)

(defconstant ES_AUTOVSCROLL #x0040)

(defconstant ES_AUTOHSCROLL #x0080)

(defconstant ES_NOHIDESEL #x0100)

(defconstant ES_OEMCONVERT #x0400)

(defconstant ES_READONLY #x0800)

(defconstant ES_WANTRETURN #x1000)

(defconstant EN_SETFOCUS #x0100)

(defconstant EN_KILLFOCUS #x0200)

(defconstant EN_CHANGE #x0300)

(defconstant EN_UPDATE #x0400)

(defconstant EN_ERRSPACE #x0500)

(defconstant EN_MAXTEXT #x0501)

(defconstant EN_HSCROLL #x0601)

(defconstant EN_VSCROLL #x0602)

(defconstant EM_GETSEL #x00B0)

(defconstant EM_SETSEL #x00B1)

(defconstant EM_GETRECT #x00B2)

(defconstant EM_SETRECT #x00B3)

(defconstant EM_SETRECTNP #x00B4)

(defconstant EM_SCROLL #x00B5)

(defconstant EM_LINESCROLL #x00B6)

(defconstant EM_SCROLLCARET #x00B7)

(defconstant EM_GETMODIFY #x00B8)

(defconstant EM_SETMODIFY #x00B9)

(defconstant EM_GETLINECOUNT #x00BA)

(defconstant EM_LINEINDEX #x00BB)

(defconstant EM_SETHANDLE #x00BC)

(defconstant EM_GETHANDLE #x00BD)

(defconstant EM_GETTHUMB #x00BE)

(defconstant EM_LINELENGTH #x00C1)

(defconstant EM_REPLACESEL #x00C2)

(defconstant EM_GETLINE #x00C4)

(defconstant EM_LIMITTEXT #x00C5)

(defconstant EM_CANUNDO #x00C6)

(defconstant EM_UNDO #x00C7)

(defconstant EM_FMTLINES #x00C8)

(defconstant EM_LINEFROMCHAR #x00C9)

(defconstant EM_SETTABSTOPS #x00CB)

(defconstant EM_SETPASSWORDCHAR #x00CC)

(defconstant EM_EMPTYUNDOBUFFER #x00CD)

(defconstant EM_GETFIRSTVISIBLELINE #x00CE)

(defconstant EM_SETREADONLY #x00CF)

(defconstant EM_SETWORDBREAKPROC #x00D0)

(defconstant EM_GETWORDBREAKPROC #x00D1)

(defconstant EM_GETPASSWORDCHAR #x00D2)

(defconstant WB_LEFT 0)

(defconstant WB_RIGHT 1)

(defconstant WB_ISDELIMITER 2)

(defconstant BS_PUSHBUTTON #x00000000)

(defconstant BS_DEFPUSHBUTTON #x00000001)

(defconstant BS_CHECKBOX #x00000002)

(defconstant BS_AUTOCHECKBOX #x00000003)

(defconstant BS_RADIOBUTTON #x00000004)

(defconstant BS_3STATE #x00000005)

(defconstant BS_AUTO3STATE #x00000006)

(defconstant BS_GROUPBOX #x00000007)

(defconstant BS_USERBUTTON #x00000008)

(defconstant BS_AUTORADIOBUTTON #x00000009)

(defconstant BS_PUSHBOX #x0000000A)

(defconstant BS_OWNERDRAW #x0000000B)

(defconstant BS_TYPEMASK #x0000000F)

(defconstant BS_LEFTTEXT #x00000020)

(defconstant BN_CLICKED 0)

(defconstant BN_PAINT 1)

(defconstant BN_HILITE 2)

(defconstant BN_UNHILITE 3)

(defconstant BN_DISABLE 4)

(defconstant BN_DOUBLECLICKED 5)

(defconstant BM_GETCHECK #x00F0)

(defconstant BM_SETCHECK #x00F1)

(defconstant BM_GETSTATE #x00F2)

(defconstant BM_SETSTATE #x00F3)

(defconstant BM_SETSTYLE #x00F4)

(defconstant SS_LEFT #x00000000)

(defconstant SS_CENTER #x00000001)

(defconstant SS_RIGHT #x00000002)

(defconstant SS_ICON #x00000003)

(defconstant SS_BLACKRECT #x00000004)

(defconstant SS_GRAYRECT #x00000005)

(defconstant SS_WHITERECT #x00000006)

(defconstant SS_BLACKFRAME #x00000007)

(defconstant SS_GRAYFRAME #x00000008)

(defconstant SS_WHITEFRAME #x00000009)

(defconstant SS_USERITEM #x0000000A)

(defconstant SS_SIMPLE #x0000000B)

(defconstant SS_LEFTNOWORDWRAP #x0000000C)

(defconstant SS_NOPREFIX #x00000080)

(defconstant STM_SETICON #x0170)

(defconstant STM_GETICON #x0171)

(defconstant STM_MSGMAX #x0174)

(defcfun ("GetWindowTextA" GetWindowText) :int
  (hWnd :pointer)
  (lpString :string)
  (nMaxCount :int))

(defcfun ("SendMessageA" SendMessage) :long
  (hWnd :pointer)
  (Msg :unsigned-int)
  (wParam :unsigned-int)
  (lParam :long))

(defcfun ("SetTimer" SetTimer) :unsigned-int
  (hWnd :pointer)
  (nIDEvent :unsigned-int)
  (uElapse :unsigned-int)
  (lpTimerFunc :pointer))

(defcfun ("KillTimer" KillTimer) :int
  (hWnd :pointer)
  (uIDEvent :unsigned-int))

(defcfun ("FillRect" FillRect) :int
  (hDC :pointer)
  (lprc :pointer)
  (hbr :pointer))

(defcfun ("CreateSolidBrush" CreateSolidBrush) :pointer
  (crColor :unsigned-long))

(defcfun ("DeleteObject" DeleteObject) :int
  (hObject :pointer))

(defcfun ("CreateFontA" CreateFont) :pointer
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

(defcfun ("ExtTextOutA" ExtTextOut) :int
  (hdc :pointer)
  (X :int)
  (Y :int)
  (fuOptions :unsigned-int)
  (lprc :pointer)
  (lpString :string)
  (cbCount :unsigned-int)
  (lpDx :pointer))

(defcfun ("SetMapMode" SetMapMode) :int
  (hdc :pointer)
  (fnMapMode :int))

(defconstant OUT_DEFAULT_PRECIS 0)

(defconstant OUT_STRING_PRECIS 1)

(defconstant OUT_CHARACTER_PRECIS 2)

(defconstant OUT_STROKE_PRECIS 3)

(defconstant OUT_TT_PRECIS 4)

(defconstant OUT_DEVICE_PRECIS 5)

(defconstant OUT_RASTER_PRECIS 6)

(defconstant OUT_TT_ONLY_PRECIS 7)

(defconstant OUT_OUTLINE_PRECIS 8)

(defconstant OUT_SCREEN_OUTLINE_PRECIS 9)

(defconstant OUT_PS_ONLY_PRECIS 10)

(defconstant CLIP_DEFAULT_PRECIS 0)

(defconstant CLIP_CHARACTER_PRECIS 1)

(defconstant CLIP_STROKE_PRECIS 2)

(defconstant CLIP_MASK #xf)

(defconstant CLIP_LH_ANGLES 16)

(defconstant CLIP_TT_ALWAYS 32)

(defconstant CLIP_EMBEDDED 128)

(defconstant DEFAULT_QUALITY 0)

(defconstant DRAFT_QUALITY 1)

(defconstant PROOF_QUALITY 2)

(defconstant DEFAULT_PITCH 0)

(defconstant FIXED_PITCH 1)

(defconstant VARIABLE_PITCH 2)

(defconstant ANSI_CHARSET 0)

(defconstant DEFAULT_CHARSET 1)

(defconstant SYMBOL_CHARSET 2)

(defconstant SHIFTJIS_CHARSET 128)

(defconstant HANGEUL_CHARSET 129)

(defconstant HANGUL_CHARSET 129)

(defconstant GB2312_CHARSET 134)

(defconstant CHINESEBIG5_CHARSET 136)

(defconstant OEM_CHARSET 255)

(defconstant FF_DONTCARE 0)

(defconstant FF_ROMAN 16)

(defconstant FF_SWISS 32)

(defconstant FF_MODERN 64)

(defconstant FF_SCRIPT 128)

(defconstant FF_DECORATIVE 256)

(defconstant FW_DONTCARE 0)

(defconstant FW_THIN 100)

(defconstant FW_EXTRALIGHT 200)

(defconstant FW_LIGHT 300)

(defconstant FW_NORMAL 400)

(defconstant FW_MEDIUM 500)

(defconstant FW_SEMIBOLD 600)

(defconstant FW_BOLD 700)

(defconstant FW_EXTRABOLD 800)

(defconstant FW_HEAVY 900)

(defconstant FW_ULTRALIGHT 200)

(defconstant FW_REGULAR 400)

(defconstant FW_DEMIBOLD 600)

(defconstant FW_ULTRABOLD 800)

(defconstant FW_BLACK 900)

(defconstant PANOSE_COUNT 10)

(defconstant PAN_FAMILYTYPE_INDEX 0)

(defconstant PAN_SERIFSTYLE_INDEX 1)

(defconstant PAN_WEIGHT_INDEX 2)

(defconstant PAN_PROPORTION_INDEX 3)

(defconstant PAN_CONTRAST_INDEX 4)

(defconstant PAN_STROKEVARIATION_INDEX 5)

(defconstant PAN_ARMSTYLE_INDEX 6)

(defconstant PAN_LETTERFORM_INDEX 7)

(defconstant PAN_MIDLINE_INDEX 8)

(defconstant PAN_XHEIGHT_INDEX 9)

(defconstant PAN_CULTURE_LATIN 0)

(defconstant PAN_ANY 0)

(defconstant PAN_NO_FIT 1)

(defconstant PAN_FAMILY_TEXT_DISPLAY 2)

(defconstant PAN_FAMILY_SCRIPT 3)

(defconstant PAN_FAMILY_DECORATIVE 4)

(defconstant PAN_FAMILY_PICTORIAL 5)

(defconstant PAN_SERIF_COVE 2)

(defconstant PAN_SERIF_OBTUSE_COVE 3)

(defconstant PAN_SERIF_SQUARE_COVE 4)

(defconstant PAN_SERIF_OBTUSE_SQUARE_COVE 5)

(defconstant PAN_SERIF_SQUARE 6)

(defconstant PAN_SERIF_THIN 7)

(defconstant PAN_SERIF_BONE 8)

(defconstant PAN_SERIF_EXAGGERATED 9)

(defconstant PAN_SERIF_TRIANGLE 10)

(defconstant PAN_SERIF_NORMAL_SANS 11)

(defconstant PAN_SERIF_OBTUSE_SANS 12)

(defconstant PAN_SERIF_PERP_SANS 13)

(defconstant PAN_SERIF_FLARED 14)

(defconstant PAN_SERIF_ROUNDED 15)

(defconstant PAN_WEIGHT_VERY_LIGHT 2)

(defconstant PAN_WEIGHT_LIGHT 3)

(defconstant PAN_WEIGHT_THIN 4)

(defconstant PAN_WEIGHT_BOOK 5)

(defconstant PAN_WEIGHT_MEDIUM 6)

(defconstant PAN_WEIGHT_DEMI 7)

(defconstant PAN_WEIGHT_BOLD 8)

(defconstant PAN_WEIGHT_HEAVY 9)

(defconstant PAN_WEIGHT_BLACK 10)

(defconstant PAN_WEIGHT_NORD 11)

(defconstant PAN_PROP_OLD_STYLE 2)

(defconstant PAN_PROP_MODERN 3)

(defconstant PAN_PROP_EVEN_WIDTH 4)

(defconstant PAN_PROP_EXPANDED 5)

(defconstant PAN_PROP_CONDENSED 6)

(defconstant PAN_PROP_VERY_EXPANDED 7)

(defconstant PAN_PROP_VERY_CONDENSED 8)

(defconstant PAN_PROP_MONOSPACED 9)

(defconstant PAN_CONTRAST_NONE 2)

(defconstant PAN_CONTRAST_VERY_LOW 3)

(defconstant PAN_CONTRAST_LOW 4)

(defconstant PAN_CONTRAST_MEDIUM_LOW 5)

(defconstant PAN_CONTRAST_MEDIUM 6)

(defconstant PAN_CONTRAST_MEDIUM_HIGH 7)

(defconstant PAN_CONTRAST_HIGH 8)

(defconstant PAN_CONTRAST_VERY_HIGH 9)

(defconstant PAN_STROKE_GRADUAL_DIAG 2)

(defconstant PAN_STROKE_GRADUAL_TRAN 3)

(defconstant PAN_STROKE_GRADUAL_VERT 4)

(defconstant PAN_STROKE_GRADUAL_HORZ 5)

(defconstant PAN_STROKE_RAPID_VERT 6)

(defconstant PAN_STROKE_RAPID_HORZ 7)

(defconstant PAN_STROKE_INSTANT_VERT 8)

(defconstant PAN_STRAIGHT_ARMS_HORZ 2)

(defconstant PAN_STRAIGHT_ARMS_WEDGE 3)

(defconstant PAN_STRAIGHT_ARMS_VERT 4)

(defconstant PAN_STRAIGHT_ARMS_SINGLE_SERIF 5)

(defconstant PAN_STRAIGHT_ARMS_DOUBLE_SERIF 6)

(defconstant PAN_BENT_ARMS_HORZ 7)

(defconstant PAN_BENT_ARMS_WEDGE 8)

(defconstant PAN_BENT_ARMS_VERT 9)

(defconstant PAN_BENT_ARMS_SINGLE_SERIF 10)

(defconstant PAN_BENT_ARMS_DOUBLE_SERIF 11)

(defconstant PAN_LETT_NORMAL_CONTACT 2)

(defconstant PAN_LETT_NORMAL_WEIGHTED 3)

(defconstant PAN_LETT_NORMAL_BOXED 4)

(defconstant PAN_LETT_NORMAL_FLATTENED 5)

(defconstant PAN_LETT_NORMAL_ROUNDED 6)

(defconstant PAN_LETT_NORMAL_OFF_CENTER 7)

(defconstant PAN_LETT_NORMAL_SQUARE 8)

(defconstant PAN_LETT_OBLIQUE_CONTACT 9)

(defconstant PAN_LETT_OBLIQUE_WEIGHTED 10)

(defconstant PAN_LETT_OBLIQUE_BOXED 11)

(defconstant PAN_LETT_OBLIQUE_FLATTENED 12)

(defconstant PAN_LETT_OBLIQUE_ROUNDED 13)

(defconstant PAN_LETT_OBLIQUE_OFF_CENTER 14)

(defconstant PAN_LETT_OBLIQUE_SQUARE 15)

(defconstant PAN_MIDLINE_STANDARD_TRIMMED 2)

(defconstant PAN_MIDLINE_STANDARD_POINTED 3)

(defconstant PAN_MIDLINE_STANDARD_SERIFED 4)

(defconstant PAN_MIDLINE_HIGH_TRIMMED 5)

(defconstant PAN_MIDLINE_HIGH_POINTED 6)

(defconstant PAN_MIDLINE_HIGH_SERIFED 7)

(defconstant PAN_MIDLINE_CONSTANT_TRIMMED 8)

(defconstant PAN_MIDLINE_CONSTANT_POINTED 9)

(defconstant PAN_MIDLINE_CONSTANT_SERIFED 10)

(defconstant PAN_MIDLINE_LOW_TRIMMED 11)

(defconstant PAN_MIDLINE_LOW_POINTED 12)

(defconstant PAN_MIDLINE_LOW_SERIFED 13)

(defconstant PAN_XHEIGHT_CONSTANT_SMALL 2)

(defconstant PAN_XHEIGHT_CONSTANT_STD 3)

(defconstant PAN_XHEIGHT_CONSTANT_LARGE 4)

(defconstant PAN_XHEIGHT_DUCKING_SMALL 5)

(defconstant PAN_XHEIGHT_DUCKING_STD 6)

(defconstant PAN_XHEIGHT_DUCKING_LARGE 7)

(defconstant ELF_VENDOR_SIZE 4)

(defconstant ELF_VERSION 0)

(defconstant ELF_CULTURE_LATIN 0)

(defconstant RASTER_FONTTYPE #x0001)

(defconstant DEVICE_FONTTYPE #x002)

(defconstant TRUETYPE_FONTTYPE #x004)

(defconstant PC_RESERVED #x01)

(defconstant PC_EXPLICIT #x02)

(defconstant PC_NOCOLLAPSE #x04)

(defconstant TRANSPARENT 1)

(defconstant OPAQUE 2)

(defconstant BKMODE_LAST 2)

(defconstant GM_COMPATIBLE 1)

(defconstant GM_ADVANCED 2)

(defconstant GM_LAST 2)

(defconstant PT_CLOSEFIGURE #x01)

(defconstant PT_LINETO #x02)

(defconstant PT_BEZIERTO #x04)

(defconstant PT_MOVETO #x06)

(defconstant MM_TEXT 1)

(defconstant MM_LOMETRIC 2)

(defconstant MM_HIMETRIC 3)

(defconstant MM_LOENGLISH 4)

(defconstant MM_HIENGLISH 5)

(defconstant MM_TWIPS 6)

(defconstant MM_ISOTROPIC 7)

(defconstant MM_ANISOTROPIC 8)

(defconstant MM_MIN 1)

(defconstant MM_MAX 8)

(defconstant MM_MAX_FIXEDSCALE 6)

(defconstant ABSOLUTE 1)

(defconstant RELATIVE 2)

(defconstant ETO_OPAQUE #x0002)

(defconstant ETO_CLIPPED #x0004)

(defcfun ("SetBkColor" SetBkColor) :unsigned-long
  (hdc :pointer)
  (crColor :unsigned-long))

(defcfun ("GetSystemMetrics" GetSystemMetrics) :int
  (nIndex :int))

(defconstant SM_CXSCREEN 0)

(defconstant SM_CYSCREEN 1)

(defconstant SM_CXVSCROLL 2)

(defconstant SM_CYHSCROLL 3)

(defconstant SM_CYCAPTION 4)

(defconstant SM_CXBORDER 5)

(defconstant SM_CYBORDER 6)

(defconstant SM_CXDLGFRAME 7)

(defconstant SM_CYDLGFRAME 8)

(defconstant SM_CYVTHUMB 9)

(defconstant SM_CXHTHUMB 10)

(defconstant SM_CXICON 11)

(defconstant SM_CYICON 12)

(defconstant SM_CXCURSOR 13)

(defconstant SM_CYCURSOR 14)

(defconstant SM_CYMENU 15)

(defconstant SM_CXFULLSCREEN 16)

(defconstant SM_CYFULLSCREEN 17)

(defconstant SM_CYKANJIWINDOW 18)

(defconstant SM_MOUSEPRESENT 19)

(defconstant SM_CYVSCROLL 20)

(defconstant SM_CXHSCROLL 21)

(defconstant SM_DEBUG 22)

(defconstant SM_SWAPBUTTON 23)

(defconstant SM_RESERVED1 24)

(defconstant SM_RESERVED2 25)

(defconstant SM_RESERVED3 26)

(defconstant SM_RESERVED4 27)

(defconstant SM_CXMIN 28)

(defconstant SM_CYMIN 29)

(defconstant SM_CXSIZE 30)

(defconstant SM_CYSIZE 31)

(defconstant SM_CXFRAME 32)

(defconstant SM_CYFRAME 33)

(defconstant SM_CXMINTRACK 34)

(defconstant SM_CYMINTRACK 35)

(defconstant SM_CXDOUBLECLK 36)

(defconstant SM_CYDOUBLECLK 37)

(defconstant SM_CXICONSPACING 38)

(defconstant SM_CYICONSPACING 39)

(defconstant SM_MENUDROPALIGNMENT 40)

(defconstant SM_PENWINDOWS 41)

(defconstant SM_DBCSENABLED 42)

(defconstant SM_CMOUSEBUTTONS 43)

(defconstant SM_SHOWSOUNDS 70)

(defconstant SM_CMETRICS 76)
