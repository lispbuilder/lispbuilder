#### -*- Mode: Makefile -*-

include ../Makefile.opts
include ../Makefile.common

USR_SDL_LIBDIR=$(DESTDIR)/local/lib/
SDL_LIBDIR=$(DESTDIR)/lib/

all:

clean:
	$(RM) $(STUBS)

install_glue:
	install -m 0644 $(SHLIB_NAME).$(SHLIB_SUFFIX) $(USR_SDL_LIBDIR)

ifeq ($(wildcard $(SDL_LIBDIR)$(SHLIB_NAME).$(SHLIB_SUFFIX)),)
	@echo "Creating a soft link from $(SHLIB_NAME).$(SHLIB_SUFFIX) in $(USR_SDL_LIBDIR) to $(SDL_LIBDIR)"
	ln -s $(USR_SDL_LIBDIR)$(SHLIB_NAME).$(SHLIB_SUFFIX) $(SDL_LIBDIR)$(SHLIB_NAME).$(SHLIB_SUFFIX)
else
	@echo "Link exists from $(SHLIB_NAME).$(SHLIB_SUFFIX) in $(USR_SDL_LIBDIR) to $(SDL_LIBDIR)"
endif
