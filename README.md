# cl-xcb-xlib-demos

This is a place for demos for cl-xcb-xlib.  Try these:

    (asdf:load-system :cl-xcb-xlib-demos)
    (xcb.clx.demos:run-gears)
    (xcb.clx.demos:cairo-run 'cairo-demos:arc1)
    (xcb.clx.demos:cairo-run 'cairo-demos:mesh1)

## glxgears

The main part of this was ripped off from the demo in PORTABLE-CLX,
and converted to work with cl-opengl and the newer GL calls required
for direct GLX.  You can optionally specify :SCREEN N to make it run
on a different head (useful if one has faster GL than the default).

## cairo-arc

**Updated:** Cairo demos are now going to be in cl-cairo2 itself.
They can be run with only cl-cairo2 using an image-surface, or using
cairo-xcb.  The stuff below still applies, but the drawing code is
in cl-cairo2.

The first sample adapted from cairographics.org/samples/.  While the
output isn't particularly interesting, it illustrates a number of
things:

   - Making a CLX window and dealing with closing nicely (see
     MAKE-X-WINDOW and the :client-message case in CAIRO-RUN).

   - Dealing with configuration changes and resizing the cairo
     surface.

   - Mostly proper handling for cleanup of surfaces, windows,
     displays, etc*

(*Note: Cleanup could actually be better; creation of the display and
window should really be inside its own UNWIND-PROTECT.)
