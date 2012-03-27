(in-package :xcb.clx.demos)

(defun make-x-window (display &key (screen 0) (x 0) (y 0) (w 300) (h 300))
  (with-display display
    (let* ((screen (nth screen (display-roots display)))
           (xwin (create-window
                  :parent (screen-root screen)
                  :x x
                  :y y
                  :width w
                  :height h
                  :event-mask (make-event-mask :exposure
                                               :key-press
                                               :structure-notify
                                               :button-press))))
      (change-property xwin
                       :wm_protocols
                       (list (find-atom display :wm_delete_window))
                       :atom 32)
      xwin)))
