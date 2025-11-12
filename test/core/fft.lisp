;;;; fft.lisp --- Test for mlx-cl.fft

(in-package :mlx-cl.test)

(def-suite* mlx-fft
  :description "Test for FFT bindings. "
  :in mlx-test)

(test 1dfft
  (let ((arr (rnd:uniform 10 :shape 10)))
    (is (lisp<- (~= (realpart (fft:1difft (fft:1dfft  arr))) arr)))))

(test 2dfft
  (let ((arr (rnd:uniform 10 :shape '(5 5))))
    (is (lisp<- (~= (realpart (fft:2difft (fft:2dfft arr)))  arr)))))

(test fft
  (let ((arr (rnd:uniform 10 :shape 25)))
    (is (lisp<- (~= (fft:1dfft arr)         (fft:fft arr))))
    (is (lisp<- (~= (fft:1dfft arr :size 5) (fft:fft arr :size 5)))))

  (let ((arr (rnd:uniform 10 :shape '(5 5))))
    (is (lisp<- (~= (fft:2dfft arr)          (fft:fft arr))))
    (is (lisp<- (~= (fft:2dfft arr)          (fft:fft arr :dim 2))))
    (is (lisp<- (~= (fft:1dfft arr)          (fft:fft arr :dim 1))))
    (is (lisp<- (~= (fft:1dfft arr)          (fft:fft arr :axis -1))))))

;;;; fft.lisp ends here
