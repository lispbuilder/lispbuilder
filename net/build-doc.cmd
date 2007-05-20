c:\Programme\doxygen\bin\doxygen.exe
xcopy /y doxygen-doc\*.gif documentation
xcopy /y doxygen-doc\*.png documentation
xcopy /y doxygen-doc\*.css documentation
xcopy /y doxygen-doc\net_8h.html documentation
rmdir /s /q doxygen-doc
