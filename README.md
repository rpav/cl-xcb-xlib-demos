# cl-xcb-xlib-demos

This is a place for demos for cl-xcb-xlib.  Currently only one exists:

    (asdf:load-system :cl-xcb-xlib-demos)
    (xcb.clx.demos:run-gears)

## glxgears

The main part of this was ripped off from the demo in PORTABLE-CLX,
and converted to work with cl-opengl and the newer GL calls required
for direct GLX.  You can optionally specify :SCREEN N to make it run
on a different head (useful if one has faster GL than the default).
