#!/bin/bash

SYS=""
case `uname` in
    Linux) SYS=linux ;;
    FreeBSD) SYS=fbsd ;;
    *CYGWIN*) SYS=cygwin ;;
    *MINGW*) SYS=mingw ;;
    Darwin) SYS=darwin ;;
esac

if [ -z "$SYS" ]; then
    echo "System not found"
    exit 1;
fi

ln -sf Makefile.$SYS Makefile.opts
