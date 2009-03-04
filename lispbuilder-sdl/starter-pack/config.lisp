(:babel "BABEL."
       "http://common-lisp.net/project/babel/releases/babel_0.3.0.tar.gz")

(:alexandria "ALEXANDRIA."
       "http://www.balooga.com/lispbuilder/alexandria.tar.gz")

(:trivial-features "TRIVIAL-FEATURES."
       "http://common-lisp.net/~loliveira/tarballs/trivial-features/trivial-features_latest.tar.gz")

(:cffi "CFFI."
       "http://common-lisp.net/project/cffi/releases/cffi_0.10.4.tar.gz"
       :babel
       :trivial-features
       :alexandria)

(:trivial-garbage "TRIVIAL-GARBAGE."
       "http://common-lisp.net/~loliveira/tarballs/trivial-garbage/trivial-garbage_latest.tar.gz")

(:cl-vectors "cl-vectors."
	     "http://projects.tuxee.net/cl-vectors/files/cl-vectors-0.1.3.tar.gz")

(:zpb-ttf "zpb-ttf."
	     "http://www.balooga.com/lispbuilder/zpb-ttf.tgz")

(:salza2 "salza2."
	"http://www.balooga.com/lispbuilder/salza2.tgz")

(:salza-png "salza-png."
	    "http://www.balooga.com/lispbuilder/salza-png.tgz"
	    :salza2)

(:zpng "zpng."
	    "http://www.balooga.com/lispbuilder/zpng.tgz"
	    :salza2)

(:vecto "vecto."
	"http://www.balooga.com/lispbuilder/vecto.tgz"
	:cl-vectors
	:zpb-ttf
	:salza2
	:salza-png
        :zpng)

(:lispbuilder-sdl-binaries "lispbuilder-sdl-binaries."
			   "http://www.balooga.com/lispbuilder/win32-lispbuilder-sdl-binaries.tgz")

(:lispbuilder-sdl "lispbuilder-sdl."
		  "http://www.balooga.com/lispbuilder/lispbuilder-sdl.tgz"
		  :trivial-garbage
                  :cffi
		  :lispbuilder-sdl-binaries
		  :vecto)

(:lispbuilder-sdl-gfx-binaries "lispbuilder-sdl-gfx-binaries."
			       "http://www.balooga.com/lispbuilder/win32-lispbuilder-sdl-gfx-binaries.tgz")

(:lispbuilder-sdl-gfx "lispbuilder-sdl-gfx."
		      "http://www.balooga.com/lispbuilder/lispbuilder-sdl-gfx.tgz"
		      :lispbuilder-sdl-gfx-binaries
		      :lispbuilder-sdl)

(:lispbuilder-sdl-image-binaries "lispbuilder-sdl-image-binaries."
				 "http://www.balooga.com/lispbuilder/win32-lispbuilder-sdl-image-binaries.tgz")

(:lispbuilder-sdl-image "lispbuilder-sdl-image."
			"http://www.balooga.com/lispbuilder/lispbuilder-sdl-image.tgz"
			:lispbuilder-sdl-image-binaries
			:lispbuilder-sdl)

(:lispbuilder-sdl-ttf-binaries "lispbuilder-sdl-ttf-binaries."
			       "http://www.balooga.com/lispbuilder/win32-lispbuilder-sdl-ttf-binaries.tgz")

(:lispbuilder-sdl-ttf "lispbuilder-sdl-ttf."
		      "http://www.balooga.com/lispbuilder/lispbuilder-sdl-ttf.tgz"
		      :lispbuilder-sdl-ttf-binaries
		      :lispbuilder-sdl)

;; (:lispbuilder-sdl-mixer-binaries "lispbuilder-sdl-mixer-binaries."
;; 				 "http://www.balooga.com/lispbuilder/win32-lispbuilder-sdl-mixer-binaries.tgz")

;; (:lispbuilder-sdl-mixer "lispbuilder-sdl-mixer."
;; 			"http://www.balooga.com/lispbuilder/lispbuilder-sdl-mixer.tgz"
;; 			:lispbuilder-sdl-mixer-binaries
;; 			:lispbuilder-sdl)

;; (:lispbuilder-openrm-binaries "lispbuilder-openrm-binaries."
;; 			      "http://www.balooga.com/lispbuilder/win32-lispbuilder-openrm-binaries.tgz")

;; (:lispbuilder-openrm "lispbuilder-openrm."
;; 		     "http://www.balooga.com/lispbuilder/lispbuilder-openrm.tgz"
;; 		     :lispbuilder-openrm-binaries
;; 		     :lispbuilder-sdl)
