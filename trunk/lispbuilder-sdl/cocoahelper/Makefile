cocoahelper.dylib : cocoahelper.m
	gcc -c cocoahelper.m -o cocoahelper.o -fPIC `sdl-config --cflags`
	gcc -dynamiclib -o cocoahelper.dylib cocoahelper.o -framework Cocoa `sdl-config --libs`

clean:
	rm -rf cocoahelper.dylib cocoahelper.o

