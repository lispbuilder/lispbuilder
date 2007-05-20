#!/bin/sh

scons system=macosx
mkdir -p bin-macosx
cp -f libnet.dylib server client bin-macosx
scons -c system=macosx
