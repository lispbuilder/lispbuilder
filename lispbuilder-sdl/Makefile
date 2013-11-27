#### -*- Mode: Makefile -*-
#### Author: Matthew Danish <mrd@debian.org>
#### Main GNU Makefile

#### Modified by: Luke Crook <luke@balooga.com>

DESTDIR=/usr

all: Makefile.opts
	${MAKE} -C glue

clean: Makefile.opts
	${MAKE} -C glue clean
	${MAKE} -C build clean
	$(RM) Makefile.opts

Makefile.opts:
	(sh select_platform.sh)

install_glue:
	${MAKE} -C build install_glue DESTDIR=$(DESTDIR)

install: install_glue
