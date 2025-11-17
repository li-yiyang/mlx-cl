;;;; linalg.lisp --- Test for mlx-cl.linalg

(in-package :mlx-cl.test)

(def-suite* mlx-linalg
  :description "Test for mlx-cl.linalg. "
  :in          mlx-test)

(test matinv
  (with-mlx-device :cpu
    (let* ((mat (mlx-array #2A((1 2)
                               (3 4))
                           :dtype :float32))
           (inv (linalg:matinv mat)))
      (is (equal (@ mat inv) (@ inv mat)))
      (is (equal (@ mat inv) (eye (shape mat)))))))

(test tri-inv
  (with-mlx-device :cpu
    (let ((*absolute-tolerance* 1e-5))
      (let* ((mat (mlx-array #2A((1 2 3)
                                 (0 4 5)
                                 (0 0 6))
                             :dtype :float32))
             (inv (linalg:tri-inv mat :upper)))
        (is (lisp<- (~= inv (linalg:matinv mat))))
        (is (lisp<- (~= (@ mat inv) (@ inv mat))))
        (is (lisp<- (~= (@ mat inv) (eye (shape mat))))))

      (let* ((mat (mlx-array #2A((1 0 0)
                                 (2 3 0)
                                 (4 5 6))
                             :dtype :float32))
             (inv (linalg:tri-inv mat :lower)))
        (is (lisp<- (~= inv (linalg:matinv mat))))
        (is (lisp<- (~= (@ mat inv) (@ inv mat))))
        (is (lisp<- (~= (@ mat inv) (eye (shape mat)))))))))

(test norm
  (let* ((a (- (arange 9 :dtype :int32) 4))
         (b (reshape a #(3 3))))
    (is (lisp<- (~= (linalg:norm a)            7.74597)))
    (is (lisp<- (~= (linalg:norm b)            7.74597)))
    (is (lisp<- (~= (linalg:norm b :fro)       7.74597)))
    (is (lisp<- (~= (linalg:norm a :inf)       4)))
    (is (lisp<- (~= (linalg:norm b :inf)       9)))
    (is (lisp<- (~= (linalg:norm a :neg-inf)   0)))
    (is (lisp<- (~= (linalg:norm b :neg-inf)   2)))
    (is (lisp<- (~= (linalg:norm a 1)          20)))
    (is (lisp<- (~= (linalg:norm b 1)          7)))
    (is (lisp<- (~= (linalg:norm a -1)         0)))
    (is (lisp<- (~= (linalg:norm b -1)         6)))
    (is (lisp<- (~= (linalg:norm a 2)          7.74597)))
    (is (lisp<- (~= (linalg:norm a 3)          5.84804)))
    (is (lisp<- (~= (linalg:norm a -3)         0))))

  (let ((c (mlx-array #2A((1  2 3)
                          (-1 1 4))
                      :dtype :int32)))
    (is (lisp<- (~= (linalg:norm c nil :axis 0)
                    #(1.41421 2.23607 5))))
    (is (lisp<- (~= (linalg:norm c nil :axis 1)
                    #(3.74166 4.24264))))
    (is (lisp<- (~= (linalg:norm c 1   :axis 1)
                    #(6 6)))))

  (let ((m (reshape (arange 8) #(2 2 2))))
    (is (lisp<- (~= (linalg:norm m nil :axis #(1 2))
                    #(3.74166 11.225))))
    (is (lisp<- (~= (linalg:norm (at m 0))
                    3.74166)))
    (is (lisp<- (~= (linalg:norm (at m 1))
                    11.225)))))

(test eigvals
  (with-mlx-device :cpu
    (let ((a (mlx-array #2A(( 1.0 -2.0)
                            (-2.0  1.0)))))
      (is (lisp<- (~= (linalg:eigvals a) #(3 -1)))))))

(test eig
  (with-mlx-device :cpu
    (multiple-value-bind (w v)
        (linalg:eig #2A((1.0 -2.0)
                        (-2.0 1.0)))
      (is (lisp<- (~= w #(3 -1))))
      (is (lisp<- (~= v #2A(( 0.707107 0.707107)
                            (-0.707107 0.707107))))))))

(test eigh
  (with-mlx-device :cpu
    (multiple-value-bind (w v)
        (linalg:eigh (mlx-array #2A(( 1.0 -2.0)
                                    (-2.0  1.0))))
      (is (lisp<- (~= w #(-1 3))))
      (is (lisp<- (~= v #2A((-0.707107 -0.707107)
                            (-0.707107  0.707107))))))))

;;;; linalg.lisp ends here
