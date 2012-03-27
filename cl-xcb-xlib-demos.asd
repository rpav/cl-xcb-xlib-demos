(defsystem #:cl-xcb-xlib-demos
  :description "Demos for xcb.clx"
  :author "Ryan Pavik"
  :license "NewBSD, LLGPL, various?"

  :depends-on (:cl-xcb-xlib :cl-opengl)

  :pathname "src"
  :serial t

  :components
  ((:file "package")
   (:file "utility")
   (:module "glx"
    :pathname "glx"
    :serial t
    :components
    ((:file "utility")
     (:file "gears")))
   (:module "cairo"
    :pathname "cairo"
    :serial t
    :components
    ((:file "simple")))))
