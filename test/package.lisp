;;;; package.lisp --- package definition for mlx-cl.test

(uiop:define-package #:mlx-cl.test
  (:use :mlx-cl :fiveam)
  (:export
   #:run-tests))

(in-package :mlx-cl.test)

(defun run-tests ()
  "Run all the test for `mlx-cl' package. "
  (5am:run! 'mlx-test))

(def-suite* mlx-test
  :description "Test MLX-CL. ")

;;;; package.lisp ends here
