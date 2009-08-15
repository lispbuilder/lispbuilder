
(in-package #:lispbuilder-sdl)


(defparameter *font-8x13O*
  (make-instance 'bitmap-font-definition
   :width 8
   :height 13
   :data #(0 0 85 0 65 0 65 0 130 0 170 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
	   0 0 0 0 0 80 80 96 160 174 4 4 8 8 0 0 0 0 112 64 96 128 142 8 12 16 16 0 0 0
	   0 48 64 128 128 108 10 12 20 20 0 0 0 0 64 64 128 128 238 8 12 16 16 0 0 0 0
	   24 36 36 24 0 0 0 0 0 0 0 0 0 0 8 8 62 16 16 0 124 0 0 0 0 0 72 72 104 176 148
	   4 8 8 14 0 0 0 0 72 72 80 80 46 4 4 8 8 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
	   0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
	   0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
	   0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
	   0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
	   0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 7 24 96 16 14 0 254 0 0 0
	   0 0 0 112 12 3 28 224 0 254 0 0 0 0 0 0 0 254 68 68 136 136 136 0 0 0 0 0 4 8
	   126 16 16 252 32 64 0 0 0 0 14 17 16 56 16 32 32 98 220 0 0 0 0 0 0 0 0 24 0 0
	   0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 8 8 8 8 16 16 16 0 16 0 0 0 0 36 36 36 0
	   0 0 0 0 0 0 0 0 0 0 36 36 126 36 252 72 72 0 0 0 0 0 8 30 40 40 28 20 20 120
	   16 0 0 0 0 34 82 36 8 16 32 72 148 136 0 0 0 0 0 12 18 20 24 42 68 76 50 0 0 0
	   0 16 16 16 0 0 0 0 0 0 0 0 0 0 4 8 16 16 32 32 16 16 8 0 0 0 0 16 8 8 4 4 8 8
	   16 32 0 0 0 0 0 0 36 24 126 48 72 0 0 0 0 0 0 0 0 16 16 124 32 32 0 0 0 0 0 0
	   0 0 0 0 0 0 0 28 24 32 0 0 0 0 0 0 0 124 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 16 56
	   16 0 0 0 1 1 2 4 24 32 64 128 128 0 0 0 0 24 36 66 66 66 132 132 72 48 0 0 0 0
	   8 24 40 8 8 16 16 16 124 0 0 0 0 60 66 66 2 12 48 64 128 252 0 0 0 0 126 2 4 8
	   24 4 4 132 120 0 0 0 0 2 6 10 18 36 68 126 8 8 0 0 0 0 62 32 32 92 98 2 4 132
	   120 0 0 0 0 28 34 64 64 120 132 132 132 120 0 0 0 0 126 2 4 8 16 32 32 64 64 0
	   0 0 0 60 66 66 66 124 132 132 132 120 0 0 0 0 60 66 66 70 58 4 4 8 112 0 0 0 0
	   0 0 8 28 8 0 0 16 56 16 0 0 0 0 0 8 28 8 0 0 56 48 64 0 0 0 4 8 16 32 64 64 32
	   16 8 0 0 0 0 0 0 0 126 0 0 252 0 0 0 0 0 0 32 16 8 4 4 8 16 32 64 0 0 0 0 28
	   34 66 4 8 16 16 0 16 0 0 0 0 60 66 66 78 82 166 152 128 120 0 0 0 0 24 36 66
	   66 66 252 132 132 132 0 0 0 0 120 68 66 68 120 136 132 136 240 0 0 0 0 60 66
	   64 64 64 128 128 132 120 0 0 0 0 120 68 66 66 66 132 132 136 240 0 0 0 0 126
	   64 64 64 120 128 128 128 252 0 0 0 0 126 64 64 64 120 128 128 128 128 0 0 0 0
	   60 66 64 64 64 156 132 140 116 0 0 0 0 66 66 66 66 124 132 132 132 132 0 0 0 0
	   62 8 8 8 8 16 16 16 124 0 0 0 0 30 4 4 4 4 8 8 136 112 0 0 0 0 66 68 72 80 96
	   160 144 136 132 0 0 0 0 32 32 32 32 32 64 64 64 126 0 0 0 0 65 65 99 85 73 130
	   130 130 130 0 0 0 0 66 66 98 82 74 140 132 132 132 0 0 0 0 60 66 66 66 66 132
	   132 132 120 0 0 0 0 124 66 66 66 124 128 128 128 128 0 0 0 0 60 66 66 66 132
	   132 164 148 120 4 0 0 0 124 66 66 66 124 160 144 136 132 0 0 0 0 60 66 64 64
	   56 4 4 132 120 0 0 0 0 254 16 16 16 16 32 32 32 32 0 0 0 0 66 66 66 66 132 132
	   132 132 120 0 0 0 0 130 130 132 68 72 72 80 80 32 0 0 0 0 65 65 65 65 73 146
	   146 170 68 0 0 0 0 65 65 34 20 24 40 68 130 130 0 0 0 0 130 130 68 40 16 16 32
	   32 32 0 0 0 0 126 2 4 8 16 32 64 128 252 0 0 0 0 60 32 32 32 32 64 64 64 120 0
	   0 0 0 64 64 32 16 8 8 4 2 2 0 0 0 0 60 4 4 4 4 8 8 8 120 0 0 0 0 16 40 68 0 0
	   0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 254 0 0 16 8 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
	   60 2 124 132 140 116 0 0 0 0 64 64 64 92 98 66 132 196 184 0 0 0 0 0 0 0 60 66
	   128 128 132 120 0 0 0 0 2 2 2 58 68 132 132 140 116 0 0 0 0 0 0 0 60 66 124
	   128 132 120 0 0 0 0 28 34 32 32 124 32 64 64 64 0 0 0 0 0 0 0 58 68 136 112
	   128 120 132 120 0 0 32 32 64 92 98 66 132 132 132 0 0 0 0 0 8 0 24 8 8 16 16
	   124 0 0 0 0 0 2 0 6 2 2 4 4 68 68 56 0 0 32 32 32 34 44 48 72 68 66 0 0 0 0 24
	   8 8 8 8 16 16 16 124 0 0 0 0 0 0 0 118 73 73 146 146 130 0 0 0 0 0 0 0 92 98
	   66 132 132 132 0 0 0 0 0 0 0 60 66 66 132 132 120 0 0 0 0 0 0 0 46 49 33 98 92
	   64 128 128 0 0 0 0 0 58 70 132 140 116 4 8 8 0 0 0 0 0 92 34 32 64 64 64 0 0 0
	   0 0 0 0 60 66 32 24 132 120 0 0 0 0 0 32 32 124 32 64 64 68 56 0 0 0 0 0 0 0
	   34 34 34 68 68 58 0 0 0 0 0 0 0 68 68 72 80 80 32 0 0 0 0 0 0 0 65 65 146 146
	   170 68 0 0 0 0 0 0 0 66 36 24 48 72 132 0 0 0 0 0 0 0 66 66 132 140 116 4 132
	   120 0 0 0 0 0 126 4 24 32 64 252 0 0 0 0 28 32 32 16 96 32 64 64 56 0 0 0 0 8
	   8 8 8 8 16 16 16 16 0 0 0 0 56 4 4 8 12 16 8 8 112 0 0 0 0 36 84 72 0 0 0 0 0
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
	   0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
	   0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 8 0 8 8 8 16 16 16 16 0 0 0 0 8 28 42 40
	   80 84 56 16 0 0 0 0 0 14 17 16 56 16 32 32 98 220 0 0 0 0 0 0 66 60 36 72 120
	   132 0 0 0 0 0 130 130 68 40 124 16 248 32 32 0 0 0 0 8 8 8 8 0 16 16 16 16 0 0
	   0 24 36 32 24 36 72 48 8 72 48 0 0 0 36 36 0 0 0 0 0 0 0 0 0 0 0 0 28 34 73 85
	   162 170 146 68 56 0 0 0 0 28 2 60 68 60 0 124 0 0 0 0 0 0 0 18 36 72 144 144
	   72 36 0 0 0 0 0 0 0 0 0 126 2 4 4 0 0 0 0 0 0 0 0 0 60 0 0 0 0 0 0 0 0 28 34
	   89 85 170 178 170 68 56 0 0 0 0 126 0 0 0 0 0 0 0 0 0 0 0 0 24 36 36 24 0 0 0
	   0 0 0 0 0 0 0 8 8 62 16 16 0 124 0 0 0 0 24 36 4 56 64 120 0 0 0 0 0 0 0 56 4
	   24 8 72 48 0 0 0 0 0 0 0 8 16 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 66 66 66 132 204
	   180 128 0 0 0 62 116 116 116 52 40 40 40 40 0 0 0 0 0 0 0 0 24 0 0 0 0 0 0 0 0
	   0 0 0 0 0 0 0 0 0 8 24 0 8 24 8 16 16 56 0 0 0 0 0 0 0 0 12 18 36 24 0 120 0 0
	   0 0 0 0 0 0 72 36 18 18 36 72 144 0 0 0 0 16 48 16 32 34 118 10 20 30 4 0 0 0
	   32 96 32 64 76 242 4 24 32 60 0 0 0 56 4 24 8 74 54 10 20 30 4 0 0 0 0 8 0 8 8
	   16 32 66 68 56 0 0 0 16 8 0 24 36 66 66 124 132 132 0 0 0 8 16 0 24 36 66 66
	   124 132 132 0 0 0 24 36 0 24 36 66 66 124 132 132 0 0 0 50 76 0 24 36 66 66
	   124 132 132 0 0 0 36 36 0 24 36 66 66 124 132 132 0 0 0 24 36 24 24 36 66 66
	   124 132 132 0 0 0 0 55 72 72 72 78 112 144 144 158 0 0 0 0 60 66 64 64 64 128
	   128 132 120 16 32 0 16 8 0 126 64 64 112 128 128 252 0 0 0 8 16 0 126 64 64
	   112 128 128 252 0 0 0 24 36 0 126 64 64 112 128 128 252 0 0 0 36 36 0 126 64
	   64 112 128 128 252 0 0 0 16 8 0 62 8 8 16 16 16 124 0 0 0 4 8 0 62 8 8 16 16
	   16 124 0 0 0 12 18 0 62 8 8 16 16 16 124 0 0 0 34 34 0 62 8 8 16 16 16 124 0 0
	   0 0 120 68 66 66 226 68 132 136 240 0 0 0 50 76 0 65 97 81 146 138 134 130 0 0
	   0 16 8 0 62 65 65 130 130 130 124 0 0 0 4 8 0 62 65 65 130 130 130 124 0 0 0
	   12 18 0 62 65 65 130 130 130 124 0 0 0 50 76 0 62 65 65 130 130 130 124 0 0 0
	   34 34 0 62 65 65 130 130 130 124 0 0 0 0 0 0 66 36 24 48 72 132 0 0 0 0 2 60
	   70 74 74 82 164 164 196 120 128 0 0 32 16 0 66 66 66 132 132 132 120 0 0 0 8
	   16 0 66 66 66 132 132 132 120 0 0 0 24 36 0 66 66 66 132 132 132 120 0 0 0 36
	   36 0 66 66 66 132 132 132 120 0 0 0 8 16 0 68 68 40 48 32 32 32 0 0 0 0 64 124
	   66 66 66 124 128 128 128 0 0 0 0 28 34 34 36 40 68 66 66 92 0 0 0 0 16 8 0 60
	   2 124 132 140 116 0 0 0 0 4 8 0 60 2 124 132 140 116 0 0 0 0 24 36 0 60 2 124
	   132 140 116 0 0 0 0 50 76 0 60 2 124 132 140 116 0 0 0 0 36 36 0 60 2 124 132
	   140 116 0 0 0 24 36 24 0 60 2 124 132 140 116 0 0 0 0 0 0 0 54 9 126 144 146
	   108 0 0 0 0 0 0 0 60 66 128 128 132 120 16 32 0 0 16 8 0 60 66 124 128 132 120
	   0 0 0 0 8 16 0 60 66 124 128 132 120 0 0 0 0 24 36 0 60 66 124 128 132 120 0 0
	   0 0 36 36 0 60 66 124 128 132 120 0 0 0 0 16 8 0 24 8 8 16 16 124 0 0 0 0 8 16
	   0 24 8 8 16 16 124 0 0 0 0 24 36 0 24 8 8 16 16 124 0 0 0 0 36 36 0 24 8 8 16
	   16 124 0 0 0 36 24 40 4 60 66 66 132 132 120 0 0 0 0 50 76 0 92 98 66 132 132
	   132 0 0 0 0 32 16 0 60 66 66 132 132 120 0 0 0 0 8 16 0 60 66 66 132 132 120 0
	   0 0 0 24 36 0 60 66 66 132 132 120 0 0 0 0 50 76 0 60 66 66 132 132 120 0 0 0
	   0 36 36 0 60 66 66 132 132 120 0 0 0 0 0 8 8 0 126 0 16 16 0 0 0 0 0 0 0 2 60
	   70 90 164 196 120 128 0 0 0 16 8 0 34 34 34 68 68 58 0 0 0 0 4 8 0 34 34 34 68
	   68 58 0 0 0 0 12 18 0 34 34 34 68 68 58 0 0 0 0 20 20 0 34 34 34 68 68 58 0 0
	   0 0 8 16 0 66 66 132 140 116 4 132 120 0 0 0 64 64 92 98 66 132 196 184 128
	   128 0 0 36 36 0 66 66 132 140 116 4 132 120))
  "Contains the font data for an 8x13 bitmap font.

##### Packages

* Also supported in _LISPBUILDER-SDL-GFX_")


