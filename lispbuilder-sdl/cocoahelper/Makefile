cocoahelper.dylib : cocoahelper.m
	gcc -c cocoahelper.m -o cocoahelper.o -fPIC `sdl-config --cflags`
	gcc -dynamiclib -o cocoahelper.dylib cocoahelper.o -framework Cocoa `sdl-config --libs`

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

clean:
	rm -rf cocoahelper.dylib cocoahelper.o cocoahelper.framework

