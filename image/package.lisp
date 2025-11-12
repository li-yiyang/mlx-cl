;;;; package.lisp --- Package definition for mlx-cl.image

(uiop:define-package #:mlx-cl.image
  (:use :mlx-cl)
  (:nicknames :mlx.img)
  (:export
   #:image
   #:w #:h #:d
   #:colorspace
   #:as-colorspace

   ;; Sugar
   #:colorspace=))

(in-package :mlx-cl.image)

;;;; package.lisp ends here
