;;;; package.lisp --- Package definition for mlx-cl.test.image

(uiop:define-package #:mlx-cl.test.image
  (:use :mlx-cl :mlx-cl.image :fiveam)
  (:shadowing-import-from #:mlx #:!)
  (:export
   #:run-tests))

(in-package :mlx-cl.test.image)

(def-suite* mlx-image-test
  :description "Test MLX-CL.IMAGE. "
  :in mlx-cl.test::mlx-test)

(defun run-tests ()
  "Run all test for `mlx-cl.image' package. "
  (5am:run! 'mlx-image-test))

;;;; package.lisp ends here
