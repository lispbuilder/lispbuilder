# this should work for FreeBSD and most Linux distros

# lispbuilder-sdl-ttf-glue.so:
# 	gcc -I/usr/local/include -fPIC -c lispbuilder-sdl-ttf-glue.c
# 	ld -shared -lgd -lz -lpng -ljpeg -lfreetype -liconv -lm -lc lispbuider-sdl-ttf-glue.o -o lispbuider-sdl-ttf-glue.dll -L/.
# 	rm lispbuider-sdl-ttf-glue.o

lispbuilder-sdl-ttf-glue.dll:
	gcc -ISDL-1.2.11/include -ISDL_ttf-2.0.8 -c lispbuilder-sdl-ttf-glue.c
	ld -shared -LSDL_ttf-2.0.8/SDL_ttf.dll lispbuilder-sdl-ttf-glue.o -o lispbuider-sdl-ttf-glue.dll
	rm lispbuilder-sdl-ttf-glue.o

# # this should work for Mac OS X

# cl-gd-glue.dylib:
# 	gcc -lgd -ljpeg -dynamiclib cl-gd-glue.c -o cl-gd-glue.dylib
