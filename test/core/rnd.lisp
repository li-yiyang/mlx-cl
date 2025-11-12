;;;; rnd.lisp --- Test for mlx-cl.random

(in-package :mlx-cl.test)

(def-suite* mlx-random
  :description "Tests for `mlx-cl.random'. "
  :in mlx-test)


;;; Seed
(def-suite* mlx-seed
  :description "Test for seed"
  :in mlx-random)



;;;; rnd.lisp ends here
