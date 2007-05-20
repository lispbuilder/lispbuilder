call scons system=win32
mkdir bin-win32
xcopy /y net.dll bin-win32
xcopy /y server.exe bin-win32
xcopy /y client.exe bin-win32
call scons -c system=win32
