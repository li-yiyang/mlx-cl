;;;; package.lisp --- package definition for mlx-cl.test

(uiop:define-package #:mlx-cl.test
  (:use :mlx-cl :fiveam)
  (:local-nicknames
   (:rnd :mlx-cl.random)
   (:fft :mlx-cl.fft))
  (:shadowing-import-from #:mlx #:!)
  (:export
   #:run-tests))

(in-package :mlx-cl.test)

(def-suite* mlx-test
  :description "Test MLX-CL. ")

(defun run-tests ()
  "Run all the test for `mlx-cl' package. "
  (5am:run! 'mlx-test))

;;;; package.lisp ends here
