;; SDL (Simple Media Layer) library using CFFI for foreign function interfacing...
;; (C)2006 Justin Heyes-Jones <justinhj@gmail.com> and Luke Crook <luke@balooga.com>
;; Thanks to Frank Buss and Surendra Singh
;; see COPYING for license
;; This file contains some useful functions for using SDL from Common lisp
;; using sdl.lisp (the CFFI wrapper)

(in-package #:lispbuilder-sdl) 

; drawing to surfaces

;; (defun draw-rect(surface_ptr x y w h color)
;;   "Given a surface pointer draw a rectangle with the specified x,y, width, height and color"
;;   (with-foreign-object (rect_ptr 'SDL_Rect)
;;     (setf (foreign-slot-value rect_ptr 'SDL_Rect 'x) x)
;;     (setf (foreign-slot-value rect_ptr 'SDL_Rect 'y) y)
;;     (setf (foreign-slot-value rect_ptr 'SDL_Rect 'w) w)
;;     (setf (foreign-slot-value rect_ptr 'SDL_Rect 'h) h)
;;     (if (< 0 (SDL_FillRect surface_ptr rect_ptr color))
;; 	(error "SDL_FillRect failed"))))


(defun draw-rect(surface_ptr x y w h color &optional rect_ptr)
  "Given a surface pointer draw a rectangle with the specified x,y, width, height and color"
  (if rect_ptr
      (if (< 0 (SDL_FillRect surface_ptr rect_ptr color))
	  (error "SDL_FillRect failed"))
      (with-foreign-object (rect_ptr 'SDL_Rect)
	(setf (cffi:foreign-slot-value rect_ptr 'SDL_Rect 'x) x)
	(setf (cffi:foreign-slot-value rect_ptr 'SDL_Rect 'y) y)
	(setf (cffi:foreign-slot-value rect_ptr 'SDL_Rect 'w) w)
	(setf (cffi:foreign-slot-value rect_ptr 'SDL_Rect 'h) h)
	(if (< 0 (SDL_FillRect surface_ptr rect_ptr color))
	    (error "SDL_FillRect failed"))))
  rect_ptr)

(defun draw-rect-end-points(surface_ptr x1 y1 x2 y2 color)
  "Given a surface pointer draw a rectangle with the specified corner co-ordinates and color"
  (let
      ((w (1+ (abs (- x1 x2))))
       (h (1+ (abs (- y1 y2)))))
    (draw-rect surface_ptr x1 y1 w h color)))

;; (defun draw-random-rect (surface_ptr width height)
;;   "Given a surface pointer draw a random rectangle within specified width and height"
;;   (let*
;;       ((x (random width))
;;        (y (random height))
;;        (w (random width))
;;        (h (random height))
;;        (color (random (expt 2 24))))
;;     (draw-rect surface_ptr x y w h color)))

(defun random+1 (rnd)
  (+ 1 (random rnd)))

(defun draw-random-rect (surface_ptr width height &optional rect_ptr)
  "Given a surface pointer draw a random rectangle within specified width and height"
  (let ((x (random+1 width))
	(y (random+1 height))
	(w (random+1 width))
	(h (random+1 height))
	;; (color (sdl_maprgb (pixelformat surface_ptr) (random 255) (random 255) (random 255)))
	(color (random (expt 2 24))))
    (if rect_ptr
	(draw-rect surface_ptr nil nil nil nil color (rectangle x y w h rect_ptr))
	(draw-rect surface_ptr x y w h color)))
  rect_ptr)

;; (defun blit-surface(src dest x y)
;;   "blit the whole of surface src to the dest at position x y"
;;   (let ((src-rect (get-surface-rect src)))
;;     (let ((dest-rect (make-sdl-rect x y 
;; 				    (cffi:foreign-slot-value src-rect 'SDL_Rect 'w)
;; 				    (cffi:foreign-slot-value src-rect 'SDL_Rect 'h))))
;;       (SDL_UpperBlit src src-rect dest dest-rect)
;;       (cffi:foreign-free src-rect)
;;       (cffi:foreign-free dest-rect))))

(defun copy-surf-to-rect (surface rect)
  "Copy the width and height of the surface to the rectangle.
   The x and y do not need to be copied as these values have no meaning for a surface." 
  (cffi:with-foreign-slots ((w h) rect SDL_Rect)
    (setf w (cffi:foreign-slot-value surface 'SDL_Surface 'w)
	  h (cffi:foreign-slot-value surface 'SDL_Surface 'h))))

(defun blit-surface(src dest x y)
  "blit the whole of surface src to the dest at position x y.
   Performs the same function as blit-surface, however internal temporary rectangles
   may be allocated on the stack, if supported buy the Lisp environment."
  (cffi:with-foreign-objects ((src-rect 'SDL_Rect) (dest-rect 'SDL_Rect))
    (copy-surf-to-rect src src-rect)
    (setf (rect-x dest-rect) x
	  (rect-y dest-rect) y
	  (rect-w dest-rect) (rect-w src-rect)
	  (rect-h dest-rect) (rect-h src-rect))
    (SDL_UpperBlit src src-rect dest dest-rect)))


; NOTE this will leak memory if you don't free up the allocated rectangle yourself

(defun make-sdl-rect(x y w h)
  "allocate and return an SDL_Rect structure with the provided paramters"
  (let ((pr (cffi:foreign-alloc 'SDL_Rect)))
    (setf (cffi:foreign-slot-value pr 'SDL_Rect 'x) x)
    (setf (cffi:foreign-slot-value pr 'SDL_Rect 'y) y)
    (setf (cffi:foreign-slot-value pr 'SDL_Rect 'w) w)
    (setf (cffi:foreign-slot-value pr 'SDL_Rect 'h) h)
    pr))

; NOTE this will leak memory if you don't free up the allocated rectangle yourself

(defun get-surface-rect(surface)
  "allocates and returns a rectangle containing the bmp's width and height"
  (let ((pr (cffi:foreign-alloc 'SDL_Rect)))
    (setf (cffi:foreign-slot-value pr 'SDL_Rect 'x) 0)
    (setf (cffi:foreign-slot-value pr 'SDL_Rect 'y) 0)
    (setf (cffi:foreign-slot-value pr 'SDL_Rect 'w) (cffi:foreign-slot-value surface 'SDL_Surface 'w))
    (setf (cffi:foreign-slot-value pr 'SDL_Rect 'h) (cffi:foreign-slot-value surface 'SDL_Surface 'h))
    pr))


; bmp utils

(defun load-bmp(filename)
  "load in the supplied filename, must be a bmp file"
  (if (and (stringp filename) (probe-file filename)) ; LJC: Make sure filename is a string and the filename exists.
      (let ((pstr-filename (cffi:foreign-string-alloc filename))
	    (pstr-mode (cffi:foreign-string-alloc "rb")))
	(let ((prwops (SDL_RWFromFile pstr-filename pstr-mode)))
	  (cffi:foreign-string-free pstr-filename)
	  (cffi:foreign-string-free pstr-mode)
	  (let ((psurface (SDL_LoadBMP_RW prwops 1)))
	    psurface)))
      nil))

(defun convert-surface-to-display-format(src &key (alpha nil) (free-src t))
  "converts a surface to display format and free's the source surface
    :alpha t will convert the surface and add an alpha channel.
    :free nil will not free src.
   returns NIL if the surface cannot be converted."
  ;; LJC: Added support for converting to an alpha surface.
  ;; LJC: Freeing src is now optional.
  (if (is-valid-ptr src)
      (let ((display-surface (if alpha
				 (SDL_DisplayFormatAlpha src)
				 (SDL_DisplayFormat src))))
	(if free-src
	    (SDL_FreeSurface src))
	(if (is-valid-ptr display-surface)
	    display-surface
	    nil))
      nil))

; surface utilities

(defmacro with-surface-lock(surface &body body)
  (let ((surf (gensym)))
    `(let ((,surf ,surface))
      (progn
	(SDL_LockSurface ,surf)
	,@body
	(SDL_UnlockSurface ,surf)))))

; put and get pixel

(defun put-pixel(surface x y pixel)
  "Set the pixel at (x, y) to the given value
NOTE: The surface must be locked before calling this.
Also NOTE: Have not tested 1,2,3 bpp surfaces, only 4 bpp"
  (let* ((format (foreign-slot-value surface 'SDL_Surface 'format))
	 (bpp (foreign-slot-value format 'SDL_PixelFormat 'BytesPerPixel))
	 (offset (+ (* y (foreign-slot-value surface 'SDL_Surface 'Pitch)) (* x bpp)))
	 (pixel-address (foreign-slot-value surface 'SDL_Surface 'Pixels)))
    (cond
      ((= bpp 1) 
       (setf (mem-aref pixel-address :unsigned-char offset) pixel))
      ((= bpp 2) 
       (setf (mem-aref pixel-address :unsigned-short (/ offset 2)) pixel))
      ((= bpp 3) 
       (if (eq SDL_BYTEORDER SDL_BIG_ENDIAN)
	   (progn
	     (setf (mem-aref pixel-address :char offset) (logand (ash pixel -16) #xff))
	     (setf (mem-aref pixel-address :char (1+ offset)) (logand (ash pixel -8) #xff))
	     (setf (mem-aref pixel-address :char (+ 2 offset)) (logand pixel #xff)))
	   (progn
	     (setf (mem-aref pixel-address :char offset) (logand pixel #xff))
	     (setf (mem-aref pixel-address :char (1+ offset)) (logand (ash pixel -8) #xff))
	     (setf (mem-aref pixel-address :char (+ 2 offset)) (logand (ash pixel -16) #xff)))))
      ((= bpp 4) 
       (setf (mem-aref pixel-address :unsigned-int (/ offset 4)) pixel)))))

#|
Reference source
void putpixel(SDL_Surface *surface, int x, int y, Uint32 pixel)
{
    int bpp = surface->format->BytesPerPixel;
    /* Here p is the address to the pixel we want to set */
    Uint8 *p = (Uint8 *)surface->pixels + y * surface->pitch + x * bpp;

    switch(bpp) {
    case 1:
        *p = pixel;
        break;

    case 2:
        *(Uint16 *)p = pixel;
        break;

    case 3:
        if(SDL_BYTEORDER == SDL_BIG_ENDIAN) {
            p[0] = (pixel >> 16) & 0xff;
            p[1] = (pixel >> 8) & 0xff;
            p[2] = pixel & 0xff;
        } else {
            p[0] = pixel & 0xff;
            p[1] = (pixel >> 8) & 0xff;
            p[2] = (pixel >> 16) & 0xff;
        }
        break;

    case 4:
        *(Uint32 *)p = pixel;
        break;
    }
}
|#

(defun get-pixel(surface x y)
  "Get the pixel at (x, y) as a Uint32 color value
NOTE: The surface must be locked before calling this.
Also NOTE: Have not tested 1,2,3 bpp surfaces, only 4 bpp"
  (let* ((format (cffi:foreign-slot-value surface 'SDL_Surface 'format))
	 (bpp (foreign-slot-value format 'SDL_PixelFormat 'BytesPerPixel))
	 (offset (+ (* y (foreign-slot-value surface 'SDL_Surface 'Pitch)) (* x bpp)))
	 (pixel-address (foreign-slot-value surface 'SDL_Surface 'Pixels)))
    (cond
      ((= bpp 1) 
       (mem-aref pixel-address :unsigned-char offset))
      ((= bpp 2) 
       (mem-aref pixel-address :unsigned-short (/ offset 2)))
      ((= bpp 3) 
;	 (if (eq SDL_BYTEORDER SDL_BIG_ENDIAN) ; TODO
	 (error "3 byte per pixel surfaces not supported yet"))
      ((= bpp 4) 
       (mem-aref pixel-address :unsigned-int (/ offset 4))))))



#|

/*
 * Return the pixel value at (x, y)
 * NOTE: The surface must be locked before calling this!
 */
Uint32 getpixel(SDL_Surface *surface, int x, int y)
{
    int bpp = surface->format->BytesPerPixel;
    /* Here p is the address to the pixel we want to retrieve */
    Uint8 *p = (Uint8 *)surface->pixels + y * surface->pitch + x * bpp;

    switch(bpp) {
    case 1:
        return *p;

    case 2:
        return *(Uint16 *)p;

    case 3:
        if(SDL_BYTEORDER == SDL_BIG_ENDIAN)
            return p[0] << 16 | p[1] << 8 | p[2];
        else
            return p[0] | p[1] << 8 | p[2] << 16;

    case 4:
        return *(Uint32 *)p;

    default:
        return 0;       /* shouldn't happen, but avoids warnings */
    }
}

|#

(defun create-surface(display width height key-r key-g key-b)
  "create a surface compatible with the supplied surface"
  (let ((format (cffi:foreign-slot-value display 'SDL_Surface 'format))
	(surface nil))
    (with-foreign-slots ((BitsPerPixel Rmask Gmask Bmask Amask) format SDL_PixelFormat)
			(setf surface (SDL_CreateRGBSurface (logior SDL_SWSURFACE SDL_SRCCOLORKEY)
					      width height BitsPerPixel Rmask Gmask Bmask Amask))
			(set-colorkey surface key-r key-g key-b))
    surface))

(defun is-valid-ptr (pointer)
  "IS-VALID-PTR <CFFI pointer>
  Will return T if 'pointer' is a valid <CFFI pointer> and is non-null."
  (and (cffi:pointerp pointer) (not (cffi:null-pointer-p pointer))))

(defun get-video-info (&key (video-info (SDL_GetVideoInfo)) (info :video-mem))
  "Returns information about the video hardware.
  GET-VIDEO-INFO :video-info <pointer to a SDL_VIDEOINFO structure>
                 :info :hw_available | :wm_available |
                       :blit_hw | :blit_hw_cc | :blit_hw_a |
                       :blit_sw | :blit_sw_cc | :blit_sw_a |
                       :blit_fill |
                       :video_mem |
                       :pixelformat
  Usage: get-video-info should be called after sdl_init but before sdl_setvideomode.
         e.g (get-video-info :info :video_mem), or
             (get-video-info :video-info (sdl_getvideoinfo) :info :video_mem)
         Will return the amount video memory available."
  (if (is-valid-ptr video-info)
      (case info
	(:video-mem
	 (cffi:foreign-slot-value video-info 'sdl_videoinfo 'video_mem))
	(:pixelformat
	 (cffi:foreign-slot-value video-info 'sdl_videoinfo 'vfmt))
	(otherwise
	 (member info (cffi:foreign-slot-value video-info 'sdl_videoinfo 'flags))))
      nil))

(defun is-key (key1 key2)
  "Returns t if the keypress 'key1' is equal to the specified 'key2'.
   (cffi:foreign-enum-value 'SDLKey key2)."
  (equal key1 (cffi:foreign-enum-value 'SDLKey key2)))

(defun is-modifier (mod key)
  "Returns t if the keypress modifier 'mod' is equal to the specified 'key'.
   (cffi:foreign-enum-value 'SDLMod key)."
  (equal mod (cffi:foreign-enum-value 'SDLMod key)))

(defun new-event (&key (event-type 'SDL_Event))
  "Creates a new SDL_Event and sets the type to :event-type.
   If no type is specified, then an SDL_Event of type SDL_NOEVENT is returned.
   For example, to create a quit event use :event-type 'SDL_QuitEvent."
  (let ((event (cffi:foreign-alloc event-type)))
    (setf (cffi:foreign-slot-value event 'SDL_event 'type)
	  (case event-type
	    ('sdl_quitevent SDL_QUIT)
	    (otherwise SDL_NOEVENT)))
    event))

(defun push-quitevent ()
  "Pushes a new SDL_Event of type SDL_QUIT onto the event queue."
  (SDL_PushEvent (new-event :event-type 'sdl_quitevent)))

(let ((timescale nil))
    (defun set-timescale (tscale)
        (setf timescale tscale))
    (defun get-timescale ()
        timescale))

(let ((ticks nil))
    (defun set-ticks (tcks)
        (setf ticks tcks))
    (defun get-ticks ()
        ticks))

(let ((worldtime 100))
    (defun set-worldtime (wtime)
        (setf worldtime wtime))
    (defun get-worldtime ()
        worldtime))

(let ((delay nil))
    (defun set-delay (dtime)
        (setf delay dtime))
    (defun get-delay ()
        delay))

(defstruct fpsmanager
  (framecount 0 :type fixnum)
  (rate 30 :type fixnum)
  (rateticks (/ 1000.0 30.0) :type float)
  (lastticks 0 :type fixnum))

(let ((fpsmngr (make-fpsmanager)) (fps-upper-limit 200) (fps-lower-limit 1)
      (current-ticks 0) (target-ticks 0))
;  (declare (type fixnum fps-upper-limit fps-lower-limit current-ticks target-ticks))
  (defun init-framerate-manager()
    (setf fpsmngr (make-fpsmanager)))
  (defun set-framerate (rate)
    (if (> rate 0)
        (if (and (>= rate fps-lower-limit) (<= rate fps-upper-limit))
            (progn
              (setf (fpsmanager-framecount fpsmngr) 0)
              (setf (fpsmanager-rate fpsmngr) rate)
              (setf (fpsmanager-rateticks fpsmngr) (/ 1000.0 rate))
              t)
	    nil)
	(setf (fpsmanager-rate fpsmngr) rate)))
  (defun get-framerate ()
    (fpsmanager-rate fpsmngr))
  (defun framerate-delay ()
    (when (> (fpsmanager-rate fpsmngr) 0)
      (setf current-ticks (sdl_getticks))
      (incf (fpsmanager-framecount fpsmngr))
      (setf target-ticks (+ (fpsmanager-lastticks fpsmngr) 
			    (* (fpsmanager-framecount fpsmngr) (fpsmanager-rateticks fpsmngr))))
      (if (<= current-ticks target-ticks)
	  (sdl_delay (round (- target-ticks current-ticks)))
	  (progn
	    (setf (fpsmanager-framecount fpsmngr) 0)
	    (setf (fpsmanager-lastticks fpsmngr) (sdl_getticks)))))))

(defun expand-activeevent (sdl-event params forms)
  `((eql SDL_ACTIVEEVENT (cffi:foreign-slot-value ,sdl-event 'sdl_event 'type))
    (funcall #'(lambda ,params
                 ,@forms)
     (cffi:foreign-slot-value ,sdl-event 'SDL_ActiveEvent 'gain)
     (cffi:foreign-slot-value ,sdl-event 'SDL_ActiveEvent 'state))))

(defun expand-keydown (sdl-event params forms)
  `((eql SDL_KEYDOWN (cffi:foreign-slot-value ,sdl-event 'sdl_event 'type))
    (funcall #'(lambda ,params
		   ,@forms)
             
     (cffi:foreign-slot-value ,sdl-event 'SDL_KeyboardEvent 'state)

     (cffi:foreign-slot-value (cffi:foreign-slot-pointer ,sdl-event 'sdl_keyboardevent 'keysym) 'SDL_keysym 'scancode)
     (cffi:foreign-slot-value (cffi:foreign-slot-pointer ,sdl-event 'sdl_keyboardevent 'keysym) 'SDL_keysym 'sym)
     (cffi:foreign-slot-value (cffi:foreign-slot-pointer ,sdl-event 'sdl_keyboardevent 'keysym) 'SDL_keysym 'mod)
     (cffi:foreign-slot-value (cffi:foreign-slot-pointer ,sdl-event 'sdl_keyboardevent 'keysym) 'SDL_keysym 'unicode))))

(defun expand-keyup (sdl-event params forms)
  `((eql SDL_KEYUP (cffi:foreign-slot-value ,sdl-event 'sdl_event 'type))
    (funcall #'(lambda ,params
                 ,@forms)

     (cffi:foreign-slot-value ,sdl-event 'SDL_KeyboardEvent 'state)

     (cffi:foreign-slot-value (cffi:foreign-slot-pointer ,sdl-event 'sdl_keyboardevent 'keysym) 'SDL_keysym 'scancode)
     (cffi:foreign-slot-value (cffi:foreign-slot-pointer ,sdl-event 'sdl_keyboardevent 'keysym) 'SDL_keysym 'sym)
     (cffi:foreign-slot-value (cffi:foreign-slot-pointer ,sdl-event 'sdl_keyboardevent 'keysym) 'SDL_keysym 'mod)
     (cffi:foreign-slot-value (cffi:foreign-slot-pointer ,sdl-event 'sdl_keyboardevent 'keysym) 'SDL_keysym 'unicode))))

(defun expand-mousemotion (sdl-event params forms)
  `((eql SDL_MOUSEMOTION (cffi:foreign-slot-value ,sdl-event 'sdl_event 'type))
    (funcall #'(lambda ,params
                 ,@forms)
     (cffi:foreign-slot-value ,sdl-event 'SDL_MouseMotionEvent 'state)

     (cffi:foreign-slot-value ,sdl-event 'SDL_MouseMotionEvent 'x)
     (cffi:foreign-slot-value ,sdl-event 'SDL_MouseMotionEvent 'y)
     (cffi:foreign-slot-value ,sdl-event 'SDL_MouseMotionEvent 'xrel)
     (cffi:foreign-slot-value ,sdl-event 'SDL_MouseMotionEvent 'yrel))))

(defun expand-mousebuttondown (sdl-event params forms)
  `((eql sdl_mousebuttondown (cffi:foreign-slot-value ,sdl-event 'sdl_event 'type))
    (funcall #'(lambda ,params
                 ,@forms)

     (cffi:foreign-slot-value ,sdl-event 'SDL_MouseButtonEvent 'button)
     (cffi:foreign-slot-value ,sdl-event 'SDL_MouseButtonEvent 'state)
     (cffi:foreign-slot-value ,sdl-event 'SDL_MouseButtonEvent 'x)
     (cffi:foreign-slot-value ,sdl-event 'SDL_MouseButtonEvent 'y))))

(defun expand-mousebuttonup (sdl-event params forms)
  `((eql sdl_mousebuttonup (cffi:foreign-slot-value ,sdl-event 'sdl_event 'type))
    (funcall #'(lambda ,params
                 ,@forms)
     (cffi:foreign-slot-value ,sdl-event 'SDL_MouseButtonEvent 'button)
     (cffi:foreign-slot-value ,sdl-event 'SDL_MouseButtonEvent 'state)
     (cffi:foreign-slot-value ,sdl-event 'SDL_MouseButtonEvent 'x)
     (cffi:foreign-slot-value ,sdl-event 'SDL_MouseButtonEvent 'y))))

(defun expand-joyaxismotion (sdl-event params forms)
  `((eql SDL_JOYAXISMOTION (cffi:foreign-slot-value ,sdl-event 'sdl_event 'type))
    (funcall #'(lambda ,params
                 ,@forms)
     (cffi:foreign-slot-value ,sdl-event 'SDL_JoyAxisEvent 'which)
     (cffi:foreign-slot-value ,sdl-event 'SDL_JoyAxisEvent 'axis)
     (cffi:foreign-slot-value ,sdl-event 'SDL_JoyAxisEvent 'value))))

(defun expand-joybuttondown (sdl-event params forms)
  `((eql SDL_JOYBUTTONDOWN (cffi:foreign-slot-value ,sdl-event 'sdl_event 'type))
    (funcall #'(lambda ,params
                 ,@forms)
     (cffi:foreign-slot-value ,sdl-event 'SDL_JoyButtonEvent 'which)
     (cffi:foreign-slot-value ,sdl-event 'SDL_JoyButtonEvent 'axis)
     (cffi:foreign-slot-value ,sdl-event 'SDL_JoyButtonEvent 'value))))

(defun expand-joybuttonup (sdl-event params forms)
  `((eql SDL_JOYBUTTONUP (cffi:foreign-slot-value ,sdl-event 'sdl_event 'type))
    (funcall #'(lambda ,params
                 ,@forms)
     (cffi:foreign-slot-value ,sdl-event 'SDL_JoyButtonEvent 'which)
     (cffi:foreign-slot-value ,sdl-event 'SDL_JoyButtonEvent 'axis)
     (cffi:foreign-slot-value ,sdl-event 'SDL_JoyButtonEvent 'value))))

(defun expand-joyhatmotion (sdl-event params forms)
  `((eql SDL_JOYHATMOTION (cffi:foreign-slot-value ,sdl-event 'sdl_event 'type))
    (funcall #'(lambda ,params
                 ,@forms)
     (cffi:foreign-slot-value ,sdl-event 'SDL_JoyHatEvent 'which)
     (cffi:foreign-slot-value ,sdl-event 'SDL_JoyHatEvent 'axis)
     (cffi:foreign-slot-value ,sdl-event 'SDL_JoyHatEvent 'value))))

(defun expand-joyballmotion (sdl-event params forms)
  `((eql SDL_JOYBALLMOTION (cffi:foreign-slot-value ,sdl-event 'sdl_event 'type))
    (funcall #'(lambda ,params
                 ,@forms)
     (cffi:foreign-slot-value ,sdl-event 'SDL_JoyBallEvent 'which)
     (cffi:foreign-slot-value ,sdl-event 'SDL_JoyBallEvent 'ball)
     (cffi:foreign-slot-value ,sdl-event 'SDL_JoyBallEvent 'xrel)
     (cffi:foreign-slot-value ,sdl-event 'SDL_JoyBallEvent 'yrel))))

(defun expand-videoresize (sdl-event params forms)
  `((eql SDL_VIDEORESIZE (cffi:foreign-slot-value ,sdl-event 'sdl_event 'type))
    (funcall #'(lambda ,params
                 ,@forms)
     (cffi:foreign-slot-value ,sdl-event 'SDL_ResizeEvent 'w)
     (cffi:foreign-slot-value ,sdl-event 'SDL_ResizeEvent 'h))))

(defun expand-videoexpose (sdl-event forms)
  `((eql SDL_VIDEOEXPOSE (cffi:foreign-slot-value ,sdl-event 'sdl_event 'type))
    (funcall #'(lambda ()
                 ,@forms))))

(defun expand-syswmevent (sdl-event forms)
  `((eql SDL_SYSWMEVENT (cffi:foreign-slot-value ,sdl-event 'sdl_event 'type))
    (funcall #'(lambda ()
                 ,@forms))))

(defun expand-quit (sdl-event forms quit)
  `((eql SDL_QUIT (cffi:foreign-slot-value ,sdl-event 'sdl_event 'type))
    (setf ,quit (funcall #'(lambda ()
                             ,@forms)))))

(defun expand-userevent (sdl-event params forms)
  `((and (>= (cffi:foreign-slot-value ,sdl-event 'sdl_event 'type)
	  SDL_MOUSEMOTION)
     (< (cffi:foreign-slot-value ,sdl-event 'sdl_event 'type)
      (- SDL_NUMEVENTS 1)))
    (funcall #'(lambda ,params
                 ,@forms)
     (cffi:foreign-slot-value ,sdl-event 'SDL_UserEvent 'type)
     (cffi:foreign-slot-value ,sdl-event 'SDL_UserEvent 'code)
     (cffi:foreign-slot-pointer ,sdl-event 'SDL_UserEvent 'data1)
     (cffi:foreign-slot-pointer ,sdl-event 'SDL_UserEvent 'data2))))

(defun expand-idle (forms)
  `(progn
     ,@forms))

(defmacro with-events (&body events)
  "(with-sdl-events
       (:activeevent (gain state)
		     t)
     (:keydown (state keysym)
	       t)
     (:keyup (state keysm)
	     t)
     (:mousemotion (state x y xrel yrel)
		   t)
     (:mousebuttondown (button state x y)
		       t)
     (:mousebuttonup (button state x y)
		     t)
     (:joyaxismotion (which axis value)
		     t)
     (:joybuttondown (which button state)
		     t)
     (:joybuttonup (which button state)
		   t)
     (:joyhatmotion (which hat value)
		    t)
     (:joyballmotion (which ball xrel yrel)
		     t)
     (:videoresize (w h)
		   t)
     (:videoexpose
      t)
     (:syswmevent
      t)
     (:quit 
      t)
     (:idle
      &body))
   NOTE: (:quit t) is mandatory if you ever want to exit your application."
  (let ((quit (gensym "quit")) (sdl-event (gensym "sdl-event")) (poll-event (gensym "poll-event")) 
        (previous-ticks (gensym "previous-ticks")) (current-ticks (gensym "current-ticks")))
    `(let ((,sdl-event (new-event))
           (,quit nil)
           (,previous-ticks nil)
           (,current-ticks nil))
      ;(init-framerate-manager)
      (do ()
	  ((eql ,quit t))
	(do ((,poll-event (SDL_PollEvent ,sdl-event) (SDL_PollEvent ,sdl-event)))
	    ((eql ,poll-event 0) nil)
	  (cond
            ,@(remove nil 
                      (mapcar #'(lambda (event)
                                  (case (first event)
                                    (:activeevent
                                     (expand-activeevent sdl-event 
                                                         (first (rest event)) 
                                                         (cons `(declare (ignore ,@(first (rest event))))
							       (rest (rest event)))))
				    (:keydown
				     (expand-keydown sdl-event 
						     (first (rest event)) 
						     (cons `(declare (ignore ,@(first (rest event))))
							   (rest (rest event)))))
				    (:keyup
				     (expand-keyup sdl-event 
						   (first (rest event)) 
						   (cons `(declare (ignore ,@(first (rest event))))
							 (rest (rest event)))))
				    (:mousemotion
				     (expand-mousemotion sdl-event 
							 (first (rest event)) 
							 (cons `(declare (ignore ,@(first (rest event))))
							       (rest (rest event)))))
				    (:mousebuttondown
				     (expand-mousebuttondown sdl-event
							     (first (rest event)) 
							     (cons `(declare (ignore ,@(first (rest event))))
								   (rest (rest event)))))
				    (:mousebuttonup
				     (expand-mousebuttonup sdl-event 
							   (first (rest event)) 
							   (cons `(declare (ignore ,@(first (rest event))))
								 (rest (rest event)))))
				    (:joyaxismotion
				     (expand-joyaxismotion sdl-event 
							   (first (rest event)) 
							   (cons `(declare (ignore ,@(first (rest event))))
								 (rest (rest event)))))
				    (:joybuttondown
				     (expand-joybuttondown sdl-event 
							   (first (rest event)) 
							   (cons `(declare (ignore ,@(first (rest event))))
								 (rest (rest event)))))
				    (:joybuttonup
				     (expand-joybuttonup sdl-event 
							 (first (rest event)) 
							 (cons `(declare (ignore ,@(first (rest event))))
							       (rest (rest event)))))
				    (:joyhatmotion
				     (expand-joyhatmotion sdl-event 
							  (first (rest event)) 
							  (cons `(declare (ignore ,@(first (rest event))))
								(rest (rest event)))))
				    (:joyballmotion
				     (expand-joyballmotion sdl-event 
							   (first (rest event)) 
							   (cons `(declare (ignore ,@(first (rest event))))
								 (rest (rest event)))))
				    (:videoresize
				     (expand-videoresize sdl-event 
							 (first (rest event)) 
							 (cons `(declare (ignore ,@(first (rest event))))
							       (rest (rest event)))))
				    (:videoexpose
				     (expand-videoexpose sdl-event 
							 (rest event)))
				    (:syswmevent
				     (expand-syswmevent sdl-event 
							(rest event)))
				    (:quit
				     (expand-quit sdl-event 
						  (rest event) 
						  quit))
				    (:userevent
				     (expand-userevent sdl-event 
						       (first (rest event)) 
						       (cons `(declare (ignore ,@(first (rest event))))
							     (rest (rest event)))))))
                              events))))
	(if (null ,previous-ticks)
	    (setf ,previous-ticks (SDL_GetTicks))
	    (setf ,previous-ticks ,current-ticks))
	(setf ,current-ticks (SDL_GetTicks))
	(set-timescale (/ 
			(set-ticks (- ,current-ticks ,previous-ticks)) 
			(get-worldtime)))
	,@(remove nil 
		  (mapcar #'(lambda (event)
			      (cond
				((eql :idle (first event))
				 (expand-idle (rest event)))))
			  events))
	(progn
	  (framerate-delay)))
      (cffi:foreign-free ,sdl-event))))

(defun set-flags (&rest keyword-args)
  (if (listp (first keyword-args))
      (let ((keywords 
	     (mapcar #'(lambda (x)
			 (eval x))
		     (first keyword-args))))
	(apply #'logior keywords))
      (apply #'logior keyword-args)))

(defun init-sdl (&key (flags SDL_INIT_VIDEO))
  (if (equal 0 (SDL_Init (set-flags flags)))
      t
      nil))

(defmacro with-init (init-flags &body body)
  "Attempts to initialize the SDL subsystems using SDL_Init.
   Automatically shuts down the SDL subsystems using SDL_Quit upon normal application termination or
if any fatal error occurs within &body.
   init-flags can be any combination of SDL_INIT_TIMER, SDL_INIT_AUDIO, SDL_INIT_VIDEO, SDL_INIT_CDROM,
SDL_INIT_JOYSTICK, SDL_INIT_NOPARACHUTE, SDL_INIT_EVENTTHREAD or SDL_INIT_EVERYTHING."
  `(block nil
    (unwind-protect
	 (when (init-sdl :flags (list ,@init-flags))
	   ,@body)
      (SDL_Quit))))

(defun to-radian (degree)
  "converts degrees to radians."
  (* degree (/ PI 180)))

(defun to-degree (radian)
  "converts radians to degrees."
  (/ radian (/ PI 180)))

(defun sdl-must-lock (surface)
  "Checks if a surface can be locked.
   Re-implementation of the SDL_MUSTLOCK macro.
   Returns
    T if the surface can be locked.
    NIL if the surface cannot be locked."
  (if (> 0 (cffi:foreign-slot-value surface 'SDL_Surface 'offset))
      t
      (if (not (eql 0 (logand 
		       (cffi:foreign-slot-value surface 'SDL_Surface 'flags)
		       (logior SDL_HWSURFACE SDL_ASYNCBLIT SDL_RLEACCEL))))
	  t
	  nil)))

(defmacro with-must-locksurface (surface &body body)
  "WITH-MUST-LOCKSURFACE sets up a surface for directly accessing the pixels using SDL_LockSurface.
   WITH-MUST-LOCKSURFACE uses SDL_MUSTLOCK to first check if the surface should be locked.
   Within WITH-MUST-LOCKSURFACE you can write to and read from surface->pixels, using the pixel format 
stored in surface->format."
  (let ((surf (gensym)))
    `(let ((,surf ,surface))
      (block nil
	(when (sdl-must-lock ,surf)
	  (if (>= (SDL_LockSurface ,surf) 0)
	      (progn
		,@body)
	      (error "Cannot lock surface")))
	(when (sdl-must-lock ,surf)
	  (SDL_UnlockSurface ,surf))))))

(defun set-screen (width height &key (bpp 0) (flags '(SDL_HWSURFACE SDL_FULLSCREEN SDL_HWACCEL)))
  "Will attempt to create a full screen, hardware accelerated window using SDL_SetVideoMode.
   Overriding :flags will allow any type of window to be created.
   Returns
    a new SDL_Surface if successful.
    NIL if failed."
  (let ((surface (SDL_SetVideoMode width height bpp (set-flags flags))))
    (if (is-valid-ptr surface)
	surface
	nil)))

(defun set-window (width height &key (bpp 0) (flags SDL_SWSURFACE))
  "Will attempt to create a window using software surfaces using SDL_SetVideoMode.
   Overriding :flags will allow any type of window to be created.
   Returns
    a new SDL_Surface if successful.
    NIL if failed."
  (set-screen width height :bpp bpp :flags flags))

(defun pixelformat (surface)
  "Returns the pixelformat of a surface."
  (cffi:foreign-slot-value surface 'SDL_Surface 'format))

(defun update-surface (surface &key (x 0) (y 0) (w 0) (h 0) (template nil))
  "Updates the screen using the keyword co-orditates :X :Y :W :H, or :template of type SDL_Rect.
   All co-ordinates default to 0, updating the entire screen."
  (if (is-valid-ptr surface)
      (if (is-valid-ptr template)
	  (SDL_UpdateRect surface 
			  (rect-x template)
			  (rect-y template)
			  (rect-w template)
			  (rect-h template))
	  (SDL_UpdateRect surface x y w h)))
  template)

(defun fill-surface (surface r g b &key (a nil) (template (cffi:null-pointer)) (update-surface nil))
  "fill the entire surface with the specified R G B and optional A color.
   Use :template to specify the SDL_Rect to be used as the fill template.
   Use :update-surface to call SDL_UpdateRect, using :template if provided. This allows for a 
    'dirty recs' screen update."
  (when (is-valid-ptr surface)
    (if a
	(SDL_FillRect surface template (SDL_MapRGBA (pixelformat surface) r g b a))
	(SDL_FillRect surface template (SDL_MapRGB (pixelformat surface) r g b)))
    (if update-surface
	(update-surface surface :template template)))
  template)

(defun apply-surface (source destination &key 
		      (source-rect (cffi:null-pointer))
		      (destination-rect (cffi:null-pointer)))
  "Blits the entire SOURCE SDL_Surface to the DESTINATION SDL_Surface using SDL_BlitSurface.
   use :source-rect SDL_Rect to position the SOURCE on the DESTINATION surface.
   Use :destination-rect SDL_Rect to blit only a portion of the SOURCE to the DESTINATION surface."
  (sdl::SDL_UpperBlit source source-rect destination destination-rect))

(defun apply-surface-free (source destination &key (source-rect (cffi:null-pointer))
			   (destination-x 0) (destination-y 0))
  "Like APPLY-SURFACE just that DESTINATION-X and DESTINATION-Y define where the SOURCE is blitted to. The height and
   width of the destination rectangle is taken from SOURCE-RECT, if available. If no keys are used then the whole SOURCE
   is blit to (0,0) on DESTINATION."
  (let ((drect (if (is-valid-ptr source-rect)
		   (sdl::rectangle destination-x destination-y (sdl::rect-w source-rect) (sdl::rect-h source-rect))
		   (cffi:null-pointer))))
    (apply-surface source destination :source-rect source-rect :destination-rect drect)
    (when (is-valid-ptr drect)
      (cffi:foreign-free drect))))

(defun set-colorkey (surface r g b &key (accel nil))
  "Sets the key color for the given surface. The key color is made transparent."
  (when (is-valid-ptr surface)
    (if accel
	(setf accel SDL_RLEACCEL)
	(setf accel 0))
    (SDL_SetColorKey surface (logior SDL_SRCCOLORKEY accel)
		     (SDL_MapRGB (pixelformat surface) r g b))))

(defun clear-colorkey (surface &key (accel nil))
  "Removes the key color from the given surface."
  (when (is-valid-ptr surface)
    (if accel
	(setf accel SDL_RLEACCEL)
	(setf accel 0))
    (SDL_SetColorKey surface accel 0)))

(defun moveto-rectangle (rectangle dx dy)
  "set the x and y position of the SDL_Rect."
  (cffi:with-foreign-slots ((x y) rectangle SDL_rect)
    (setf x dx
          y dy)))

(defun moveby-rectangle (rectangle dx dy)
  "add dx and dy to the x and y positions of the SDL_Rect." 
  (cffi:with-foreign-slots ((x y) rectangle SDL_rect)
    (setf x (+ x dx)
          y (+ y dy))))

(defun rect-x (rectangle)
  "return the x position of the SDL_Rect."
  (cffi:foreign-slot-value rectangle 'SDL_Rect 'x))

(defun (setf rect-x) (x-val rectangle)
  "set the x position of the SDL_Rect."
  (setf (cffi:foreign-slot-value rectangle 'SDL_Rect 'x) x-val))

(defun rect-y (rectangle)
  "return the y position of the SDL_Rect."
  (cffi:foreign-slot-value rectangle 'SDL_Rect 'y))

(defun (setf rect-y) (y-val rectangle)
  "set the y position of the SDL_Rect."
  (setf (cffi:foreign-slot-value rectangle 'SDL_Rect 'y) y-val))

(defun rect-w (rectangle)
  "return the width of the SDL_Rect."
  (cffi:foreign-slot-value rectangle 'SDL_Rect 'w))

(defun (setf rect-w) (w-val rectangle)
  "set the width  of the SDL_Rect."
  (setf (cffi:foreign-slot-value rectangle 'SDL_Rect 'w) w-val))

(defun rect-h (rectangle)
  "return the height of the SDL_Rect."
  (cffi:foreign-slot-value rectangle 'SDL_Rect 'h))

(defun (setf rect-h) (h-val rectangle)
  "set the height of the SDL_Rect."
  (setf (cffi:foreign-slot-value rectangle 'SDL_Rect 'h) h-val))

(defun new-rect ()
  "Creates a new SDL_Rect using malloc."
  (cffi:foreign-alloc 'SDL_Rect))

(defun rectangle (x y w h &optional (rect (new-rect)))
  "allocate and return an SDL_Rect structure initialized to the provided parameters.
   If the &optional rec contains an SDL_Rect, then this structure is initialized and returned."
  (labels ((set-rectangle (dx dy dw dh)
	     (cffi:with-foreign-slots ((x y w h) rect SDL_rect)
	       (setf x dx
		     y dy
		     w dw
		     h dh))))
    (if (is-valid-ptr rect)
	(set-rectangle x y w h)))
  rect)

(defun copy-rectangle (src dest)
  "Copy the x, y, w and h values from the src to the dest rectangle."
  (setf (cffi:foreign-slot-value dest 'SDL_Rect 'x) (cffi:foreign-slot-value src 'SDL_Rect 'x)
        (cffi:foreign-slot-value dest 'SDL_Rect 'y) (cffi:foreign-slot-value src 'SDL_Rect 'y)
        (cffi:foreign-slot-value dest 'SDL_Rect 'w) (cffi:foreign-slot-value src 'SDL_Rect 'w)
        (cffi:foreign-slot-value dest 'SDL_Rect 'h) (cffi:foreign-slot-value src 'SDL_Rect 'h)))

(defun surf-w (surface)
  "return the width of the SDL_surface."
  (cffi:foreign-slot-value surface 'SDL_Surface 'w))

(defun surf-h (surface)
  "return the height of the SDL_Surface." 
  (cffi:foreign-slot-value surface 'SDL_Surface 'h))

(defun get-hwnd ()
  (let ((wm-info (cffi:foreign-alloc 'sdl::SDL_SysWMinfo)))
      ;; Set the wm-info structure to the current SDL version.
      (sdl::sdl_version (cffi:foreign-slot-value wm-info 'sdl::SDL_SysWMinfo 'sdl::version))
      (sdl::SDL_GetWMInfo wm-info)
      (cffi:foreign-slot-pointer wm-info 'sdl::SDL_SysWMinfo 'sdl::window)))

(defun get-native-window ()
  (let ((wm-info (cffi:foreign-alloc 'sdl::SDL_SysWMinfo)))
      ;; Set the wm-info structure to the current SDL version.
      (sdl::sdl_version (cffi:foreign-slot-value wm-info 'sdl::SDL_SysWMinfo 'sdl::version))
      (sdl::SDL_GetWMInfo wm-info)
      ;; For Windows
      #+win32(cffi:foreign-slot-pointer wm-info 'sdl::SDL_SysWMinfo 'sdl::window)
      ;; For something other that Windows, i.e. X
      #-win32(cffi:foreign-slot-value (cffi:foreign-slot-value (cffi:foreign-slot-value wm-info
											'SDL_SysWMinfo
											'sdl::info)
							       'sdl::SDL_SysWMinfo_info
							       'sdl::x11)
				      'sdl::SDL_SysWMinfo_info_x11
				      'sdl::window)))

