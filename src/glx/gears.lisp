;;;; This is ripped off from the glxgears demo in CLX, which appears
;;;; to be a fairly direct translation of glxgears.c .. thus the
;;;; license probably falls under one of those.
;;;;
;;;; This has a number of modifications to make it work with
;;;; cl-opengl, cl-xcb-xlib, and to improve performance.  For the
;;;; latter, I mostly just cleaned up the hints given by SBCL by
;;;; declaring type on as much as possible.  This improved performance
;;;; for me from about 1400-1500fps to something like 3333fps.  C
;;;; speed of glxgears.c is about 5000fps, so it's not quite there
;;;; yet, but better.
;;;;
;;;; Also I've used the "raw" %gl methods, mostly because it was
;;;; easier to convert, except in a few places.
;;;;
;;;; Run it by loading cl-xcb-xlib-demos and doing:
;;;;
;;;;    (XCB.CLX.DEMOS:RUN-GEARS)
;;;;

(in-package #:xcb.clx.demos)

(defconstant +pi+ (coerce pi 'single-float))
(declaim (type single-float +pi+))

(defun gear (inner-radius outer-radius width teeth tooth-depth)
  (declare (optimize (debug 1) (safety 1) (speed 2))
           (type fixnum teeth)
           (type single-float inner-radius outer-radius width tooth-depth))
  (let ((r0 inner-radius)
        (r1 (/ (- outer-radius tooth-depth) 2.0s0))
        (r2 (/ (+ outer-radius tooth-depth) 2.0s0))
        (da (/ (* 2.0s0 +pi+) teeth 4.0s0)))
    (declare (type single-float r0 r1 r2 da))
    (%gl:shade-model :flat)
    (%gl:normal-3f 0.0s0 0.0s0 1.0s0)

    ;; Front face.
    (%gl:begin :quad-strip)
    (dotimes (i (1+ teeth))
      (let ((angle (/ (* i 2.0 +pi+) teeth)))
        (declare (type single-float angle))
        (%gl:vertex-3f (* r0 (cos angle))
                      (* r0 (sin angle))
                      (* width 0.5s0))
        (%gl:vertex-3f (* r1 (cos angle))
                      (* r1 (sin angle))
                      (* width 0.5s0))
        (when (< i teeth)
          (%gl:vertex-3f (* r0 (cos angle))
                        (* r0 (sin angle))
                        (* width 0.5s0))
          (%gl:vertex-3f (* r1 (cos (+ angle (* 3 da))))
                        (* r1 (sin (+ angle (* 3 da))))
                        (* width 0.5s0)))))
    (%gl:end)


    ;; Draw front sides of teeth.
    (%gl:begin :quads)
    (setf da (/ (* 2.0s0 +pi+) teeth 4.0s0))
    (dotimes (i teeth)
      (let ((angle (/ (* i 2.0s0 +pi+) teeth)))
        (declare (type single-float angle))
        (%gl:vertex-3f (* r1 (cos angle))
                      (* r1 (sin angle))
                      (* width 0.5s0))
        (%gl:vertex-3f (* r2 (cos (+ angle da)))
                      (* r2 (sin (+ angle da)))
                      (* width 0.5s0))
        (%gl:vertex-3f (* r2 (cos (+ angle (* 2 da))))
                      (* r2 (sin (+ angle (* 2 da))))
                      (* width 0.5s0))
        (%gl:vertex-3f (* r1 (cos (+ angle (* 3 da))))
                      (* r1 (sin (+ angle (* 3 da))))
                      (* width 0.5s0))))
    (%gl:end)

    (%gl:normal-3f 0.0s0 0.0s0 -1.0s0)
                 
    ;; Draw back face.
    (%gl:begin :quad-strip)
    (dotimes (i (1+ teeth))
      (let ((angle (/ (* i 2.0s0 +pi+) teeth)))
        (declare (type single-float angle))
        (%gl:vertex-3f (* r1 (cos angle))
                      (* r1 (sin angle))
                      (* width -0.5s0))
        (%gl:vertex-3f (* r0 (cos angle))
                      (* r0 (sin angle))
                      (* width -0.5s0))
        (when (< i teeth)
          (%gl:vertex-3f (* r1 (cos (+ angle (* 3 da))))
                        (* r1 (sin (+ angle (* 3 da))))
                        (* width -0.5s0))
          (%gl:vertex-3f (* r0 (cos angle))
                        (* r0 (sin angle))
                        (* width 0.5s0)))))
    (%gl:end)

    ;; Draw back sides of teeth.
    (%gl:begin :quads)
    (setf da (/ (* 2.0s0 +pi+) teeth 4.0s0))
    (dotimes (i teeth)
      (let ((angle (/ (* i 2.0s0 +pi+) teeth)))
        (declare (type single-float angle))
        (%gl:vertex-3f (* r1 (cos (+ angle (* 3 da))))
                      (* r1 (sin (+ angle (* 3 da))))
                      (* width -0.5s0))
        (%gl:vertex-3f (* r2 (cos (+ angle (* 2 da))))
                      (* r2 (sin (+ angle (* 2 da))))
                      (* width -0.5s0))
        (%gl:vertex-3f (* r2 (cos (+ angle da)))
                      (* r2 (sin (+ angle da)))
                      (* width -0.5s0))
        (%gl:vertex-3f (* r1 (cos angle))
                      (* r1 (sin angle))
                      (* width -0.5s0))))
    (%gl:end)

    ;; Draw outward faces of teeth.
    (%gl:begin :quad-strip)
    (dotimes (i teeth)
      (let ((angle (/ (* i 2.0s0 +pi+) teeth)))
        (declare (type single-float angle))
        (%gl:vertex-3f (* r1 (cos angle))
                      (* r1 (sin angle))
                      (* width 0.5s0))
        (%gl:vertex-3f (* r1 (cos angle))
                      (* r1 (sin angle))
                      (* width -0.5s0))
        (let* ((u (- (* r2 (cos (+ angle da))) (* r1 (cos angle))))
               (v (- (* r2 (sin (+ angle da))) (* r1 (sin angle))))
               (len (sqrt (+ (* u u) (* v v)))))
          (declare (type single-float len))
          (setf u (/ u len)
                v (/ v len))
          (%gl:normal-3f v u 0.0s0)
          (%gl:vertex-3f (* r2 (cos (+ angle da)))
                        (* r2 (sin (+ angle da)))
                        (* width 0.5s0))
          (%gl:vertex-3f (* r2 (cos (+ angle da)))
                        (* r2 (sin (+ angle da)))
                        (* width -0.5s0))
          (%gl:normal-3f (cos angle) (sin angle) 0.0s0)
          (%gl:vertex-3f (* r2 (cos (+ angle (* 2 da))))
                        (* r2 (sin (+ angle (* 2 da))))
                        (* width 0.5s0))
          (%gl:vertex-3f (* r2 (cos (+ angle (* 2 da))))
                        (* r2 (sin (+ angle (* 2 da))))
                        (* width -0.5s0))
          (setf u (- (* r1 (cos (+ angle (* 3 da)))) (* r2 (cos (+ angle (* 2 da)))))
                v (- (* r1 (sin (+ angle (* 3 da)))) (* r2 (sin (+ angle (* 2 da))))))
          (%gl:normal-3f v (- u) 0.0s0)
          (%gl:vertex-3f (* r1 (cos (+ angle (* 3 da))))
                        (* r1 (sin (+ angle (* 3 da))))
                        (* width 0.5s0))
          (%gl:vertex-3f (* r1 (cos (+ angle (* 3 da))))
                        (* r1 (sin (+ angle (* 3 da))))
                        (* width -0.5s0))
          (%gl:normal-3f (cos angle) (sin angle) 0.0s0))))

    (%gl:vertex-3f (* r1 (cos 0)) (* r1 (sin 0)) (* width 0.5s0))
    (%gl:vertex-3f (* r1 (cos 0)) (* r1 (sin 0)) (* width -0.5s0))

    (%gl:end)

    (%gl:shade-model :smooth)
                 
    ;; Draw inside radius cylinder.
    (%gl:begin :quad-strip)
    (dotimes (i (1+ teeth))
      (let ((angle (/ (* i 2.0s0 +pi+) teeth)))
        (declare (type single-float angle))
        (%gl:normal-3f (- (cos angle)) (- (sin angle)) 0.0s0)
        (%gl:vertex-3f (* r0 (cos angle)) (* r0 (sin angle)) (* width -0.5s0))
        (%gl:vertex-3f (* r0 (cos angle)) (* r0 (sin angle)) (* width 0.5s0))))
    (%gl:end)))


(defun reshape (width height)
  (%gl:viewport 0 0 width height)
  (let ((h (coerce (/ height width) 'double-float)))
    (%gl:matrix-mode :projection)
    (%gl:load-identity)
    (%gl:frustum -1.0d0 1.0d0 (- h) h 5.0d0 60.0d0))

  (%gl:matrix-mode :modelview)
  (%gl:load-identity)
  (%gl:translate-f 0.0s0 0.0s0 -40.0s0))

(defun gears* (display window)
  (declare (optimize (debug 1) (safety 1) (speed 2)))
  (%gl:push-matrix)
  
  (%gl:enable :cull-face)
  (%gl:enable :lighting)
  (%gl:enable :light0)
  (%gl:enable :normalize)
  (%gl:enable :depth-test)

  (reshape 300 300)

  (loop
    with angle single-float = 0.0s0
    with dt = 0.004s0
    repeat 5000
    do (progn

         (incf angle (* 70.0s0 dt))       ; 70 degrees per second
         (when (< 3600.0s0 angle)
           (decf angle 3600.0s0))

         (gl:clear :color-buffer-bit :depth-buffer-bit)

         (%gl:push-matrix)
         (%gl:rotate-f 20.0s0 0.0s0 1.0s0 0.0s0)


         (%gl:push-matrix)
         (%gl:translate-f -3.0s0 -2.0s0 0.0s0)
         (%gl:rotate-f angle 0.0s0 0.0s0 1.0s0)
         (gl:material :front :ambient-and-diffuse '(0.8s0 0.1s0 0.0s0 1.0s0))
         (gear 1.0s0 8.0s0 2.0s0 20 0.7s0)
         (%gl:pop-matrix)

         
         (%gl:push-matrix)
         (%gl:translate-f 3.1s0 -2.0s0 0.0s0)
         (%gl:rotate-f (- (* angle -2.0s0) 9.0s0) 0.0s0 0.0s0 1.0s0)
         (gl:material :front :ambient-and-diffuse '(0.0s0 0.8s0 0.2s0 1.0s0))
         (gear 0.5s0 4.0s0 2.0s0 10 0.7s0)
         (%gl:pop-matrix)


         (%gl:push-matrix)
         (%gl:translate-f -3.1s0 4.2s0 0.0s0)
         (%gl:rotate-f (- (* angle -2.s0) 25.0s0) 0.0s0 0.0s0 1.0s0)
         (gl:material :front :ambient-and-diffuse '(0.2s0 0.2s0 1.0s0 1.0s0))
         (gear 1.3s0 4.0s0 0.5s0 10 0.7s0)
         (%gl:pop-matrix)

         (%gl:pop-matrix)

         (glx:swap-buffers display window)))
  (%gl:pop-matrix))

(defun run-gears (&key display (screen 0))
  (let ((d (or display (open-display ""))))
    (multiple-value-bind (xwin glwin ctx)
        (make-glx-window d :screen screen)
      (time
       (with-display d
         (gears* d glwin)
         (destroy-glx-window xwin glwin ctx)
         (unless display
           (display-force-output d)
           (close-display d)))))))
