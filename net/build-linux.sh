#!/bin/sh

scons system=linux
mkdir -p bin-linux
cp -f libnet.so server client bin-linux
scons -c system=linux
