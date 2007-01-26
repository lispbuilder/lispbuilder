#### -*- Mode: Makefile -*-

include ../Makefile.opts
include ../Makefile.common

DESTDIR=/usr/local
SDL_LIBDIR=$(DESTDIR)/lib/cl-sdl/

all:

clean:
	$(RM) $(STUBS)

install_ttf:
	install -m 0644 lispbuilder-sdl-ttf-glue.$(SHLIB_SUFFIX) $(SDL_LIBDIR)
