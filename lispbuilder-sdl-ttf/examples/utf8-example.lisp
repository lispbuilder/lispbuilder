;;;; Demonstration/Test of using the SDL_ttf library
;;;; using CFFI for foreign function interfacing...
;;;; (C)2006 Rune Nesheim
;;;; see COPYING for license

(in-package #:sdl-ttf-examples)

(defun utf8-example ()
  (sdl:with-init ()
    (sdl:window 650 600 :title-caption "SDL-TTF Font Example" :icon-caption "SDL-TTF Font Example")
    (setf (sdl:frame-rate) 30)
    (sdl:fill-surface sdl:*white* :surface sdl:*default-display*)
    (unless (sdl:initialise-default-font sdl:*ttf-font-vera*)
      (error "FONT-EXAMPLE: Cannot initialize the default font."))

    ;; Couldn't load the library if I explictedly mentioned extended latin characters like ���, so use the character macros instead.
    (sdl::draw-string-solid-* (format nil "Extended Latin[~a~a~a] - solid"
				    #\LATIN_SMALL_LETTER_AE
				    #\LATIN_SMALL_LETTER_O_WITH_STROKE
				    #\LATIN_SMALL_LETTER_A_WITH_RING_ABOVE)
			    0 50
                            :color sdl:*black*)
    (sdl:draw-string-shaded-* (format nil "Extended Latin[~a~a~a] - Shaded" 
				    #\LATIN_SMALL_LETTER_AE
				    #\LATIN_SMALL_LETTER_O_WITH_STROKE
				    #\LATIN_SMALL_LETTER_A_WITH_RING_ABOVE)
			      0 150
                              sdl:*black*
                              sdl:*yellow*)
    (sdl:draw-string-blended-*(format nil "Extended Latin[~a~a~a] - Blended" 
				    #\LATIN_SMALL_LETTER_AE
				    #\LATIN_SMALL_LETTER_O_WITH_STROKE
				    #\LATIN_SMALL_LETTER_A_WITH_RING_ABOVE)
			      0 250
                              :color sdl:*black*)
    (sdl::draw-string-solid-* (format nil "Extended Latin[~a~a~a] - utf8 - solid"
				    #\LATIN_SMALL_LETTER_AE
				    #\LATIN_SMALL_LETTER_O_WITH_STROKE
				    #\LATIN_SMALL_LETTER_A_WITH_RING_ABOVE)
			    0 350
                            :color sdl:*black*
			    :utf8 t)
    (sdl:draw-string-shaded-* (format nil "Extended Latin[~a~a~a] - utf8 - Shaded" 
				    #\LATIN_SMALL_LETTER_AE
				    #\LATIN_SMALL_LETTER_O_WITH_STROKE
				    #\LATIN_SMALL_LETTER_A_WITH_RING_ABOVE)
			      0 450
                              sdl:*black*
                              sdl:*yellow*
			      :utf8 t)
    (sdl:draw-string-blended-*(format nil "Extended Latin[~a~a~a] - utf8 - Blended" 
				    #\LATIN_SMALL_LETTER_AE
				    #\LATIN_SMALL_LETTER_O_WITH_STROKE
				    #\LATIN_SMALL_LETTER_A_WITH_RING_ABOVE)
			      0 550
                              :color sdl:*black*
			      :utf8 t)

    (sdl:update-display)
    (sdl:with-events ()
      (:quit-event () t)
      (:video-expose-event () (sdl:update-display))
      (:key-down-event ()
		       (when (sdl:key-down-p :sdl-key-escape)
			 (sdl:push-quit-event))))))
