
(in-package #:lispbuilder-sdl)


(defparameter *font-6x13*
  (make-instance 'bitmap-font-definition
   :width 6
   :height 13
   :data #(0 0 168 0 136 0 136 0 136 0 168 0 0 0 0 0 0 32 112 248 112 32 0 0 0 0 168 84
	   168 84 168 84 168 84 168 84 168 84 168 0 0 160 160 224 160 160 56 16 16 16 0 0
	   0 0 224 128 192 128 184 32 48 32 32 0 0 0 0 96 128 128 96 48 40 48 40 40 0 0 0
	   0 128 128 128 224 56 32 48 32 32 0 0 0 0 48 72 72 48 0 0 0 0 0 0 0 0 0 0 32 32
	   248 32 32 0 248 0 0 0 0 0 144 208 176 144 32 32 32 32 56 0 0 0 0 160 160 160
	   64 64 56 16 16 16 0 0 32 32 32 32 32 32 224 0 0 0 0 0 0 0 0 0 0 0 0 224 32 32
	   32 32 32 32 0 0 0 0 0 0 60 32 32 32 32 32 32 32 32 32 32 32 32 60 0 0 0 0 0 0
	   32 32 32 32 32 32 252 32 32 32 32 32 32 252 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 252
	   0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 252 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 252 0 0 0 0 0
	   0 0 0 0 0 0 0 0 0 0 252 32 32 32 32 32 32 60 32 32 32 32 32 32 32 32 32 32 32
	   32 224 32 32 32 32 32 32 32 32 32 32 32 32 252 0 0 0 0 0 0 0 0 0 0 0 0 252 32
	   32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 0 0 0 24 96 128 96 24 0
	   248 0 0 0 0 0 0 192 48 8 48 192 0 248 0 0 0 0 0 0 0 0 248 80 80 80 80 80 0 0 0
	   0 0 0 0 8 248 32 248 128 0 0 0 0 0 48 72 64 64 224 64 64 72 176 0 0 0 0 0 0 0
	   0 48 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 32 32 32 32 32 32 32 0 32 0 0 0
	   0 80 80 80 0 0 0 0 0 0 0 0 0 0 0 80 80 248 80 248 80 80 0 0 0 0 0 32 120 160
	   160 112 40 40 240 32 0 0 0 0 72 168 80 16 32 64 80 168 144 0 0 0 0 0 64 160
	   160 64 160 152 144 104 0 0 0 0 32 32 32 0 0 0 0 0 0 0 0 0 16 32 32 64 64 64 64
	   64 32 32 16 0 0 64 32 32 16 16 16 16 16 32 32 64 0 0 0 0 32 168 248 112 248
	   168 32 0 0 0 0 0 0 0 32 32 248 32 32 0 0 0 0 0 0 0 0 0 0 0 0 0 48 32 64 0 0 0
	   0 0 0 0 248 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 32 112 32 0 0 0 8 8 16 16 32 64 64
	   128 128 0 0 0 0 32 80 136 136 136 136 136 80 32 0 0 0 0 32 96 160 32 32 32 32
	   32 248 0 0 0 0 112 136 136 8 16 32 64 128 248 0 0 0 0 248 8 16 32 112 8 8 136
	   112 0 0 0 0 16 16 48 80 80 144 248 16 16 0 0 0 0 248 128 128 176 200 8 8 136
	   112 0 0 0 0 112 136 128 128 240 136 136 136 112 0 0 0 0 248 8 16 16 32 32 64
	   64 64 0 0 0 0 112 136 136 136 112 136 136 136 112 0 0 0 0 112 136 136 136 120
	   8 8 136 112 0 0 0 0 0 0 32 112 32 0 0 32 112 32 0 0 0 0 0 32 112 32 0 0 48 32
	   64 0 0 0 8 16 32 64 128 64 32 16 8 0 0 0 0 0 0 0 248 0 0 248 0 0 0 0 0 0 128
	   64 32 16 8 16 32 64 128 0 0 0 0 112 136 136 8 16 32 32 0 32 0 0 0 0 112 136
	   136 152 168 168 176 128 120 0 0 0 0 32 80 136 136 136 248 136 136 136 0 0 0 0
	   240 72 72 72 112 72 72 72 240 0 0 0 0 112 136 128 128 128 128 128 136 112 0 0
	   0 0 240 72 72 72 72 72 72 72 240 0 0 0 0 248 128 128 128 240 128 128 128 248 0
	   0 0 0 248 128 128 128 240 128 128 128 128 0 0 0 0 112 136 128 128 128 152 136
	   136 112 0 0 0 0 136 136 136 136 248 136 136 136 136 0 0 0 0 112 32 32 32 32 32
	   32 32 112 0 0 0 0 56 16 16 16 16 16 16 144 96 0 0 0 0 136 136 144 160 192 160
	   144 136 136 0 0 0 0 128 128 128 128 128 128 128 128 248 0 0 0 0 136 136 216
	   168 168 136 136 136 136 0 0 0 0 136 200 200 168 168 152 152 136 136 0 0 0 0
	   112 136 136 136 136 136 136 136 112 0 0 0 0 240 136 136 136 240 128 128 128
	   128 0 0 0 0 112 136 136 136 136 136 136 168 112 8 0 0 0 240 136 136 136 240
	   160 144 136 136 0 0 0 0 112 136 128 128 112 8 8 136 112 0 0 0 0 248 32 32 32
	   32 32 32 32 32 0 0 0 0 136 136 136 136 136 136 136 136 112 0 0 0 0 136 136 136
	   136 80 80 80 32 32 0 0 0 0 136 136 136 136 168 168 168 168 80 0 0 0 0 136 136
	   80 80 32 80 80 136 136 0 0 0 0 136 136 80 80 32 32 32 32 32 0 0 0 0 248 8 16
	   16 32 64 64 128 248 0 0 0 112 64 64 64 64 64 64 64 64 64 112 0 0 0 128 128 64
	   64 32 16 16 8 8 0 0 0 112 16 16 16 16 16 16 16 16 16 112 0 0 0 32 80 136 0 0 0
	   0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 248 0 0 32 16 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
	   112 8 120 136 152 104 0 0 0 0 128 128 128 240 136 136 136 136 240 0 0 0 0 0 0
	   0 112 136 128 128 136 112 0 0 0 0 8 8 8 120 136 136 136 136 120 0 0 0 0 0 0 0
	   112 136 248 128 136 112 0 0 0 0 48 72 64 64 240 64 64 64 64 0 0 0 0 0 0 0 112
	   136 136 136 120 8 136 112 0 0 128 128 128 176 200 136 136 136 136 0 0 0 0 0 32
	   0 96 32 32 32 32 112 0 0 0 0 0 16 0 48 16 16 16 16 144 144 96 0 0 128 128 128
	   144 160 192 160 144 136 0 0 0 0 96 32 32 32 32 32 32 32 112 0 0 0 0 0 0 0 208
	   168 168 168 168 136 0 0 0 0 0 0 0 176 200 136 136 136 136 0 0 0 0 0 0 0 112
	   136 136 136 136 112 0 0 0 0 0 0 0 240 136 136 136 240 128 128 128 0 0 0 0 0
	   120 136 136 136 120 8 8 8 0 0 0 0 0 176 200 128 128 128 128 0 0 0 0 0 0 0 112
	   136 96 16 136 112 0 0 0 0 0 64 64 240 64 64 64 72 48 0 0 0 0 0 0 0 136 136 136
	   136 152 104 0 0 0 0 0 0 0 136 136 136 80 80 32 0 0 0 0 0 0 0 136 136 168 168
	   168 80 0 0 0 0 0 0 0 136 80 32 32 80 136 0 0 0 0 0 0 0 136 136 136 152 104 8
	   136 112 0 0 0 0 0 248 16 32 64 128 248 0 0 0 24 32 32 32 32 192 32 32 32 32 24
	   0 0 0 32 32 32 32 32 32 32 32 32 0 0 0 192 32 32 32 32 24 32 32 32 32 192 0 0
	   0 72 168 144 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
	   0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
	   0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
	   0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
	   0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
	   0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
	   0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
	   0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
	   0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
	   0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
	   0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
	   0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 32 0 32 32 32 32 32
	   32 32 0 0 0 0 32 112 168 160 160 168 112 32 0 0 0 0 0 48 72 64 64 224 64 64 72
	   176 0 0 0 0 0 0 136 112 80 80 112 136 0 0 0 0 0 136 136 80 80 248 32 248 32 32
	   0 0 0 0 32 32 32 32 0 32 32 32 32 0 0 0 48 72 64 48 72 72 48 8 72 48 0 0 0 80
	   80 0 0 0 0 0 0 0 0 0 0 0 112 136 168 216 200 216 168 136 112 0 0 0 0 0 112 8
	   120 136 120 0 248 0 0 0 0 0 0 0 0 40 80 160 160 80 40 0 0 0 0 0 0 0 0 0 248 8
	   8 0 0 0 0 0 0 0 0 0 0 112 0 0 0 0 0 0 0 112 136 232 216 216 232 216 136 112 0
	   0 0 0 0 248 0 0 0 0 0 0 0 0 0 0 0 0 48 72 72 48 0 0 0 0 0 0 0 0 0 0 32 32 248
	   32 32 0 248 0 0 0 0 64 160 32 64 224 0 0 0 0 0 0 0 0 64 160 64 32 192 0 0 0 0
	   0 0 0 0 16 32 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 136 136 136 136 152 232 128 128 0
	   0 120 232 232 232 232 104 40 40 40 0 0 0 0 0 0 0 0 48 0 0 0 0 0 0 0 0 0 0 0 0
	   0 0 0 0 0 16 32 0 64 192 64 64 224 0 0 0 0 0 0 0 0 0 112 136 136 136 112 0 248
	   0 0 0 0 0 0 0 0 160 80 40 40 80 160 0 0 0 0 64 192 64 64 224 8 24 40 56 8 0 0
	   0 64 192 64 64 224 16 40 8 16 56 0 0 0 64 160 64 32 160 72 24 40 56 8 0 0 0 0
	   32 0 32 32 64 128 136 136 112 0 0 0 64 32 0 32 80 136 136 248 136 136 0 0 0 16
	   32 0 32 80 136 136 248 136 136 0 0 0 48 72 0 32 80 136 136 248 136 136 0 0 0
	   40 80 0 32 80 136 136 248 136 136 0 0 0 80 80 0 32 80 136 136 248 136 136 0 0
	   0 32 80 32 32 80 136 136 248 136 136 0 0 0 0 88 160 160 160 176 224 160 160
	   184 0 0 0 0 112 136 128 128 128 128 128 136 112 32 64 0 64 32 0 248 128 128
	   240 128 128 248 0 0 0 16 32 0 248 128 128 240 128 128 248 0 0 0 48 72 0 248
	   128 128 240 128 128 248 0 0 0 80 80 0 248 128 128 240 128 128 248 0 0 0 64 32
	   0 112 32 32 32 32 32 112 0 0 0 16 32 0 112 32 32 32 32 32 112 0 0 0 48 72 0
	   112 32 32 32 32 32 112 0 0 0 80 80 0 112 32 32 32 32 32 112 0 0 0 0 240 72 72
	   72 232 72 72 72 240 0 0 0 40 80 0 136 136 200 168 152 136 136 0 0 0 64 32 0
	   112 136 136 136 136 136 112 0 0 0 16 32 0 112 136 136 136 136 136 112 0 0 0 48
	   72 0 112 136 136 136 136 136 112 0 0 0 40 80 0 112 136 136 136 136 136 112 0 0
	   0 80 80 0 112 136 136 136 136 136 112 0 0 0 0 0 0 0 136 80 32 80 136 0 0 0 0 8
	   112 152 152 168 168 168 200 200 112 128 0 0 64 32 0 136 136 136 136 136 136
	   112 0 0 0 16 32 0 136 136 136 136 136 136 112 0 0 0 48 72 0 136 136 136 136
	   136 136 112 0 0 0 80 80 0 136 136 136 136 136 136 112 0 0 0 16 32 0 136 136 80
	   32 32 32 32 0 0 0 0 128 240 136 136 136 240 128 128 128 0 0 0 0 96 144 144 160
	   160 144 136 136 176 0 0 0 0 64 32 0 112 8 120 136 152 104 0 0 0 0 16 32 0 112
	   8 120 136 152 104 0 0 0 0 48 72 0 112 8 120 136 152 104 0 0 0 0 40 80 0 112 8
	   120 136 152 104 0 0 0 0 80 80 0 112 8 120 136 152 104 0 0 0 48 72 48 0 112 8
	   120 136 152 104 0 0 0 0 0 0 0 112 40 112 160 168 80 0 0 0 0 0 0 0 112 136 128
	   128 136 112 32 64 0 0 64 32 0 112 136 248 128 136 112 0 0 0 0 16 32 0 112 136
	   248 128 136 112 0 0 0 0 48 72 0 112 136 248 128 136 112 0 0 0 0 80 80 0 112
	   136 248 128 136 112 0 0 0 0 64 32 0 96 32 32 32 32 112 0 0 0 0 16 32 0 96 32
	   32 32 32 112 0 0 0 0 48 72 0 96 32 32 32 32 112 0 0 0 0 80 80 0 96 32 32 32 32
	   112 0 0 0 80 32 96 16 112 136 136 136 136 112 0 0 0 0 40 80 0 176 200 136 136
	   136 136 0 0 0 0 64 32 0 112 136 136 136 136 112 0 0 0 0 16 32 0 112 136 136
	   136 136 112 0 0 0 0 48 72 0 112 136 136 136 136 112 0 0 0 0 40 80 0 112 136
	   136 136 136 112 0 0 0 0 80 80 0 112 136 136 136 136 112 0 0 0 0 0 32 32 0 248
	   0 32 32 0 0 0 0 0 0 0 8 112 152 168 168 200 112 128 0 0 0 64 32 0 136 136 136
	   136 152 104 0 0 0 0 16 32 0 136 136 136 136 152 104 0 0 0 0 48 72 0 136 136
	   136 136 152 104 0 0 0 0 80 80 0 136 136 136 136 152 104 0 0 0 0 16 32 0 136
	   136 136 152 104 8 136 112 0 0 0 128 128 176 200 136 136 200 176 128 128 0 0 80
	   80 0 136 136 136 152 104 8 136 112))
  "Contains the font data for an 6x13 bitmap font.

##### Packages

* Also supported in _LISPBUILDER-SDL-GFX_")


