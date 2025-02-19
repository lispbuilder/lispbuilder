;;;; Demonstration/Test of using the SDL_ttf library
;;;; using CFFI for foreign function interfacing...
;;;; (C)2006 Rune Nesheim
;;;; see COPYING for license

(in-package #:sdl-ttf-examples)

(defun utf8-example ()
  (sdl:with-init ()
    (sdl:window 620 300 :title-caption "SDL-TTF Font Example" :icon-caption "SDL-TTF Font Example")
    (setf (sdl:frame-rate) 30)
    (sdl:fill-surface sdl:*white* :surface sdl:*default-display*)
    (unless (sdl:initialise-default-font sdl:*ttf-font-vera*)
      (error "FONT-EXAMPLE: Cannot initialize the default font."))

    ;; Couldn't load the library if I explictedly mentioned extended latin characters like זרו, so use the code-char's instead.
    (sdl::draw-utf8-solid-* (format nil "Extended Latin[~a~a~a] - solid" (code-char 230) (code-char 248) (code-char 229))
			    0 50
                            :color sdl:*black*)
    (sdl:draw-string-shaded-* (format nil "Extended Latin[~a~a~a] - Shaded" (code-char 230) (code-char 248) (code-char 229))
			      0 150
                              sdl:*black*
                              sdl:*yellow*)
    (sdl:draw-string-blended-*(format nil "Extended Latin[~a~a~a] - Blended" (code-char 230) (code-char 248) (code-char 229))
			      0 250
                              :color sdl:*black*)

    (sdl:update-display)
    (sdl:with-events ()
      (:quit-event () t)
      (:video-expose-event () (sdl:update-display))
      (:key-down-event ()
		       (when (sdl:key-down-p :sdl-key-escape)
			 (sdl:push-quit-event))))))
