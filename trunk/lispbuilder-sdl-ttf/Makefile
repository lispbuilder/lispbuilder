#### -*- Mode: Makefile -*-
#### Author: Matthew Danish <mrd@debian.org>
#### Main GNU Makefile

#### Modified by: Luke Crook <luke@balooga.com>

DESTDIR=/usr

all: Makefile.opts
	(cd glue; ${MAKE})

clean: Makefile.opts
	(cd build; ${MAKE} clean)
	$(RM) Makefile.opts

Makefile.opts:
	(sh select_platform.sh)

install_ttf:
	(cd build; ${MAKE} install_ttf DESTDIR=$(DESTDIR))

install: install_ttf 
