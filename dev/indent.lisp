;;;; indent.lisp --- Define indension of mlx-cl functions

;;; Dev Note

;;

(in-package :mlx-cl.dev)

(define-indentation mlx::defmlx-method (2 &lambda &body))

(define-indentation mlx::with-op-template (&lambda 4 &body))

;;;; indent.lisp
