;;;; Demonstration/Test of using the SDL_gfx library
;;;; using CFFI for foreign function interfacing...
;;;; (C)2006 Luke Crook
;;;; see COPYING for license

(in-package #:sdl-gfx-examples) 

(defvar *current-y* 0)
(defvar *prev-font-height* 0)

(defmacro display-font (font-def)
  (let ((font (gensym "font-")))
    `(let* ((,font (sdl:initialise-font ,font-def)))
       (sdl:draw-string-solid (format nil "Hello World!!!! : font - ~A" ',font-def)
                              (sdl:point :x 10
                                         :y (incf *current-y* (+ 2 *prev-font-height*)))
                              :color sdl:*white*
                              :font ,font)
       (setf *prev-font-height* (sdl:char-height ,font)))))

(defun inbuilt-fonts ()
  (sdl:with-init ()
    (sdl:window 640 480 :title-caption "Inbuilt Fonts")
    (setf (sdl:frame-rate) 2)
    (setf *current-y* 0)
    (setf *prev-font-height* 0)

    (display-font sdl:*gfx-font-5x7*)
    (display-font sdl:*gfx-font-5x8*)
    (display-font sdl:*gfx-font-6x9*)
    (display-font sdl:*gfx-font-6x10*)
    (display-font sdl:*gfx-font-6x12*)
    (display-font sdl:*gfx-font-6x13*)
    (display-font sdl:*gfx-font-6x13B*)
    (display-font sdl:*gfx-font-6x13O*)
    (display-font sdl:*gfx-font-7x13*)
    (display-font sdl:*gfx-font-7x13B*)
    (display-font sdl:*gfx-font-7x13O*)
    (display-font sdl:*gfx-font-7x14*)
    (display-font sdl:*gfx-font-7x14B*)
    (display-font sdl:*gfx-font-8x8*)
    (display-font sdl:*gfx-font-8x13*)
    (display-font sdl:*gfx-font-8x13B*)
    (display-font sdl:*gfx-font-8x13O*)
    (display-font sdl:*gfx-font-9x15*)
    (display-font sdl:*gfx-font-9x15B*)
    (display-font sdl:*gfx-font-9x18*)
    (display-font sdl:*gfx-font-9x18B*)
    (display-font sdl:*gfx-font-10x20*)

    (sdl:update-display)
    (sdl:with-events ()
      (:quit-event () t)
      (:key-down-event ()
       (when (sdl:key-pressed-p :sdl-key-escape)
         (sdl:push-quit-event)))
      (:video-expose-event () (sdl:update-display)))))
