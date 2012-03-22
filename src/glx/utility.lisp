(in-package #:xcb.clx.demos)

(defun make-glx-window (display &key (screen 0) (x 0) (y 0) (w 300) (h 300))
  (handler-case
      (with-display display
        (let* ((screen (nth screen (display-roots display)))
               (fbconfig (glx:choose-fbconfig
                          screen '(:render-type :rgba-bit
                                   :drawable-type :window-bit
                                   :double-buffer t
                                   :x-renderable t
                                   :red-size 4
                                   :green-size 4
                                   :blue-size 4)))
               (cm (xlib:create-colormap (glx:get-visualid-from-fbconfig display fbconfig)
                                         (screen-root screen)))
               (ctx (glx::create-new-context display fbconfig nil t))
               (window (create-window :parent (screen-root screen)
                                      :depth 24
                                      :visual (glx:get-visualid-from-fbconfig display fbconfig)
                                      :colormap cm
                                      :x x :y y :width w :height h
                                      :background (screen-black-pixel screen)
                                      :event-mask (xlib:make-event-mask :exposure
                                                                        :button-press))))
          (let ((drawable (glx:create-glx-window display fbconfig window)))
            (xlib:map-window window)
            (xlib:display-force-output display)

            (glx:make-context-current drawable drawable ctx)
            
            (values window drawable ctx))))
    (error (e)
      (warn "Apparently opening a GL context didn't work on your system.
This may be due to the depth (24) being unacceptable, or perhaps your GL
version is incorrect.  You probably want to check how this is being
created in XCB.GLX.DEMOS::MAKE-GLX-WINDOW.  Error now...")
      (error e))))

(defun destroy-glx-window (window drawable context)
  (let ((display (display-for window)))
    (glx:destroy-context context display)
    (glx:destroy-glx-window drawable)
    (destroy-window window)
    (display-finish-output display)))
