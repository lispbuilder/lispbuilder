#### -*- Mode: Makefile -*-
#### Author: Matthew Danish <mrd@debian.org>
#### C glue GNU makefile

#### Modified by: Luke Crook <luke@balooga.com>


include ../Makefile.opts
include ../Makefile.common

all: $(SO_LIB_NAME).$(SHLIB_SUFFIX)
	mv $(SO_LIB_NAME).$(SHLIB_SUFFIX) ../build

$(LIB_NAME).o: $(LIB_NAME).c
	$(CC) $(CPPFLAGS) $(SDL_TTF_CFLAGS) -c -o $@ $<

$(SO_LIB_NAME).$(SHLIB_SUFFIX): $(LIB_NAME).o
	$(CC) $(SDL_TTF_LDFLAGS) -o $@ $<

clean:
	rm -f $(STUBS)
