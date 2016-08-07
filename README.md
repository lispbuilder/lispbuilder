# Lispbuilder

Lispbuilder, or Common Lisp Application Builder, is an umbrella
project that provides cross-platform packages for building large,
interactive applications in Common Lisp. The most notable of these is
[lispbuilder-sdl](https://github.com/lispbuilder/lispbuilder/wiki/LispbuilderSDL),
a wrapper for [SDL](https://www.libsdl.org/), a library commonly used
for game development. Other packages provide 3D graphics and
animation, networking, text processing, and other miscellaneous
functionality.

## Links

  * [Wiki](https://github.com/lispbuilder/lispbuilder/wiki)
  * [Download & Install](https://github.com/lispbuilder/lispbuilder/wiki/DownloadInstallationIntro)
  * [Screenshots](https://github.com/lispbuilder/lispbuilder/wiki/Screenshots)
  * Documentation
    * [User Guide](https://github.com/lispbuilder/lispbuilder/wiki/UsingLispbuilderSDL)
    * [API Reference Manual](https://lispbuilder.github.io/documentation)
    * [How-To's](https://github.com/lispbuilder/lispbuilder/wiki/HowTos)
  * [Community](https://github.com/lispbuilder/lispbuilder/wiki/Community)
  * [License](https://github.com/lispbuilder/lispbuilder/wiki/License)
  * [Contributors](https://github.com/lispbuilder/lispbuilder/wiki/Contributors)

## Quickstart for Ubuntu

Assuming you have a Common Lisp implementation with
[Quicklisp](https://www.quicklisp.org/) installed, run the following
to install and run a lispbuilder-sdl example.

```bash
$ sudo apt-get install libsdl1.2-dev
$ sbcl # or your own Lisp
* (ql:quickload :lispbuilder-sdl-examples)
* (sdl-examples:mandelbrot)
```

## Packages

All packages can be installed with
[Quicklisp](https://www.quicklisp.org/). Dependencies which are C/C++
libraries must be [installed
separately](https://github.com/lispbuilder/lispbuilder/wiki/DownloadInstallationIntro).

### Game and Multimedia Development

  * [**lispbuilder-sdl:**](https://github.com/lispbuilder/lispbuilder/wiki/LispbuilderSDL) SDL game and multimedia library.
      * Straightforward event loop, supporting either event polling or event waiting,
      * Many graphics drawing primitives,
      * Bitmap and vector graphic support,
      * Load diverse image types,
      * Play samples and stream music including mp3 and OGG,
      * Bitmap and True type font support.
  * **lispbuilder-openrm:** OpenRM 3d scene graph
  * **lispbuilder-cal3d:** Cal3d character animation
  * **lispbuilder-opengl:** deprecated, use [cl-opengl](https://common-lisp.net/project/cl-opengl/) instead
      * Note: OpenGL by itself does not provide windowing support; lispbuilder-sdl can be used with cl-opengl to provide windowing support for OpenGL

### Text Processing

  * **lispbuilder-regex:** Regular expression support
  * **lispbuilder-clawk:** Awk in CL
  * **lispbuilder-lexer:** Lex in CL
  * **lispbuilder-yacc:** Yacc in CL

### Networking

  * **lispbuilder-net**: deprecated, use [usocket](https://common-lisp.net/project/usocket/) instead

### Windows

  * **lispbuilder-windows:** windows.h wrapper
