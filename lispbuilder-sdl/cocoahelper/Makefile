# look for sdl-config; else assume framework is in /Library/Frameworks
ifneq ($(shell which sdl-config),)
SDL_CFLAGS=$(CFLAGS) $(shell sdl-config --cflags)
SDL_LDFLAGS=$(LDFLAGS) $(shell sdl-config --libs)
else
SDL_CFLAGS=$(CFLAGS) -D_GNU_SOURCE=1 -D_THREAD_SAFE -I/Library/Frameworks/SDL.framework/Headers/
SDL_LDFLAGS=$(LDFLAGS) -framework SDL -framework Cocoa
endif

cocoahelper.dylib : cocoahelper.m
	gcc -fPIC $(SDL_CFLAGS) -c cocoahelper.m -o cocoahelper.o
	gcc -dynamiclib $(SDL_LDFLAGS) -o cocoahelper.dylib cocoahelper.o

cocoahelper.framework: cocoahelper.dylib
	rm -rf cocoahelper.framework
	mkdir -p cocoahelper.framework/Versions/A/Resources
	cp cocoahelper.dylib cocoahelper.framework/Versions/A/Resources/cocoahelper
	cp Info.plist cocoahelper.framework/Versions/A/Resources
	mkdir -p cocoahelper.framework/Versions/A/Headers
	ln -fs Versions/A/Resources/cocoahelper cocoahelper.framework/cocoahelper
	ln -fs Versions/A/Resources cocoahelper.framework/Resources
	ln -fs Versions/A/Headers cocoahelper.framework/Headers
	ln -fs A cocoahelper.framework/Versions/Current
# The following two commands fix the paths for the cocoahelper framework:
# The one first fixes the name so that it refers to its location inside the
# framework. (As far as I can tell, this doesn't actually affect anything).
	install_name_tool -id @executable_path/../Frameworks/Version/A/Resources/cocoahelper cocoahelper.framework/cocoahelper
# The second one changes the load path to be relative to the location of the
# framework rather than relative to the executable that loaded the framework.
	install_name_tool -change @executable_path/../Frameworks/SDL.framework/Versions/A/SDL @loader_path/../../../../SDL.framework/Versions/A/SDL cocoahelper.framework/cocoahelper

clean:
	rm -rf cocoahelper.dylib cocoahelper.o cocoahelper.framework

