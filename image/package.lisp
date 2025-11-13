;;;; package.lisp --- Package definition for mlx-cl.image

(uiop:define-package #:mlx-cl.image
  (:use :mlx-cl)
  (:nicknames :mlx.img)
  (:export
   #:image
   #:w #:h #:d
   #:channels
   #:colorspace
   #:colorspace=

   ;;; colorspace
   #:*colorspace*
   #:define-colorspace
   #:define-colorspace-convert
   #:as-colorspace

   #:rgb
   #:rgba
   #:grayscale
   #:grayscale-alpha

   ;;; color
   #:color
   #:define-color
   ))

(in-package :mlx-cl.image)

;;;; package.lisp ends here
