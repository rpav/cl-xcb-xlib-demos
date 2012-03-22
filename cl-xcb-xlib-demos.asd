(defsystem #:cl-xcb-xlib-demos
  :description "Demos for xcb.clx"
  :author "Ryan Pavik"
  :license "NewBSD, LLGPL, various?"

  :depends-on (:cl-xcb-xlib :cl-opengl)

  :pathname "src"
  :serial t

  :components
  ((:module "glx"
    :pathname "glx"
    :serial t
    :components
    ((:file "package")
     (:file "utility")
     (:file "gears")))))
