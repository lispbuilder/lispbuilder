#### -*- Mode: Makefile -*-
#### Author: Matthew Danish <mrd@debian.org>
#### C glue GNU makefile

include ../Makefile.opts
include ../Makefile.common

all: $(STUBS)
	mv $(STUBS) ../build

lispbuilder-sdl-ttf-glue.$(SHLIB_SUFFIX): lispbuilder-sdl-ttf-glue.c
	$(CC) $(SDL_TTF_CFLAGS) -o $@ $< $(SDL_TTF_LDFLAGS)

clean:
	rm -f $(STUBS)