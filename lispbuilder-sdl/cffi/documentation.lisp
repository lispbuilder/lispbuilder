
(setf (documentation 'SDL-WM-GRAB-INPUT 'function)
      "The mouse is confined to the application window, and nearly all keyboard input is passed directly 
to the application, and not interpreted by a window manager, if any.
`SDL-GRAB-MODE` can be one of:

* `:SDL-GRAB-QUERY`     : Returns the current grab mode. 
* `:SDL-GRAB-OFF`       : Turns off grab mode.
* `:SDL-GRAB-ON`        : Turns on grab mode.")
