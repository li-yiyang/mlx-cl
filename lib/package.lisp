;;;; package.lisp --- Package definition for mlx-cl.lib

(uiop:define-package #:mlx-cl.lib
  (:use :cl)
  (:export
   #:libmlxc
   #:*debug-output*
   #:*libmlxc-flags*
   #:install-libmlxc
   #:load-libmlxc
   #:clean-build))

;;;; package.lisp ends here
