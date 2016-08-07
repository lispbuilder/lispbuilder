# Lispbuilder

*Provides several useful cross-platform packages for Common Lisp*

## Quick Links

  * [Wiki](https://github.com/lispbuilder/lispbuilder/wiki)
  * [Download & Installation](https://github.com/lispbuilder/lispbuilder/wiki/DownloadInstallationIntro)
    * [Windows](https://github.com/lispbuilder/lispbuilder/wiki/WindowsInstallation)
    * [OS X, Linux, and BSD](https://github.com/lispbuilder/lispbuilder/wiki/DownloadInstallation)
  * [Screenshots](https://github.com/lispbuilder/lispbuilder/wiki/Screenshots)
  * Documentation
    * [User Guide](https://github.com/lispbuilder/lispbuilder/wiki/UsingLispbuilderSDL)
    * [API Reference Manual](https://lispbuilder.github.io/documentation)
    * [How-To's](https://github.com/lispbuilder/lispbuilder/wiki/HowTos)
  * [Community](https://github.com/lispbuilder/lispbuilder/wiki/Community)
  * [License](https://github.com/lispbuilder/lispbuilder/wiki/License)
  * [Contributors](https://github.com/lispbuilder/lispbuilder/wiki/Contributors)

## Introduction

Lispbuilder, or Common Lisp Application Builder, is an umbrella
project that provides cross-platform packages for building large,
interactive applications in Common Lisp. The most notable of these is
[Lispbuilder-SDL](https://github.com/lispbuilder/lispbuilder/wiki/LispbuilderSDL),
a wrapper for [SDL](https://www.libsdl.org/), a library commonly used
for game development. Other packages provide 3D graphics and
animation, networking, text processing, and other miscellaneous
functionality.

### Game and Multimedia Development

  * [**lispbuilder-sdl:**](https://github.com/lispbuilder/lispbuilder/wiki/LispbuilderSDL) SDL game and multimedia library.
      * Straightforward event loop, supporting either event polling or event waiting,
      * Many graphics drawing primitives,
      * Bitmap and vector graphic support,
      * Load diverse image types,
      * Play samples and stream music including mp3 and OGG,
      * Bitmap and True type font support.
  * **lispbuilder-opengl:** deprecated, use [cl-opengl](https://common-lisp.net/project/cl-opengl/) instead
  * **lispbuilder-openrm:** OpenRM 3d scene graph
  * **lispbuilder-cal3d:** Cal3d character animation

### Text Processing

  * **lispbuilder-regex:** Regular expression support
  * **lispbuilder-clawk:** Awk in CL
  * **lispbuilder-lexer:** Lex in CL
  * **lispbuilder-yacc:** Yacc in CL

### Networking

  * **lispbuilder-net**: deprecated, use [usocket](https://common-lisp.net/project/usocket/) instead

### Windows

  * **lispbuilder-windows:** windows.h wrapper
