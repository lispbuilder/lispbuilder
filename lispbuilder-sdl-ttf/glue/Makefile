#### -*- Mode: Makefile -*-
#### Author: Matthew Danish <mrd@debian.org>
#### C glue GNU makefile

#### Modified by: Luke Crook <luke@balooga.com>


include ../Makefile.opts
include ../Makefile.common

all: $(STUBS)
	mv $(STUBS) ../build

$(SO_LIB_NAME).$(SHLIB_SUFFIX): $(LIB_NAME).c
	$(CC) $(SDL_TTF_CFLAGS) -o $@ $< $(SDL_TTF_LDFLAGS)

clean:
	rm -f $(STUBS)