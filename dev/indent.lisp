;;;; indent.lisp --- Define indension of mlx-cl functions

;;; Dev Note

;;

(in-package :mlx-cl.dev)


;;; MLX-CL

(define-indentation mlx::defmlx-method (2 &lambda &body))

(define-indentation mlx::defmlx-func   (2 &lambda &body))

(define-indentation mlx::with-op-template (&lambda 4 &body))

(define-indentation mlx::format-case (4 &body))

(define-indentation mlx::defmlx-extension (4 &body))


;;; MLX-CL.IMAGE

(define-indentation mlx.img::define-colorspace-convert (4 &lambda &body))

;;;; indent.lisp
