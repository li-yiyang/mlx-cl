;;;; ops.lisp --- Test array operators bindings

(in-package :mlx-cl.test)

(def-suite* mlx-ops
  :description "Test array operators bindings. "
  :in mlx-test)

(test full-ones-zeros
  (macrolet ((testing (&rest tests)
               `(progn
                  ,@(loop :for (x shape lisp type) :in tests
                          :collect
                          `(let ((x ,x))
                             ,@(when type  `(is (equal ,type  (dtype x))))
                             ,@(when shape `(is (equal ,shape (shape x))))
                             ,@(when lisp  `(is (equal ,lisp  (lisp<- x)))))))))
    (testing
     ((full 2      3.0)       '(2)   #(3 3)                       nil)
     ((full '(2 3) 2.0)       '(2 3) #2A((2 2 2) (2 2 2))         *default-mlx-float-dtype*)
     ((full '(3 2) '(nil t))   nil   #2A((nil t) (nil t) (nil t)) nil)
     ((full '(3 2) #(2.0 3.0)) nil   #2A((2 3) (2 3) (2 3))       nil)
     ((zeros 2)               '(2)   #(0 0)                       nil)
     ((ones  2)               '(2)   #(1 1)                       nil)))

  (loop :with zeros := (mlx-array #2A((0 0) (0 0)))
        :for dtype :in '(:bool :int32 :float32)
        :for x := (zeros '(2 2) :dtype dtype)
        :for y := (zeros x      :dtype dtype)
        :do
           (is (equal (dtype x) dtype))
           (is (equal x zeros))
           (is (equal (dtype x) dtype))
           (is (equal x y))))

(test scalar-inputs
  (macrolet ((test-add (&rest tests)
               `(progn
                  ,@(loop :for (a b dtype lisp) :in tests
                          :collect
                          `(let ((a (mlx::add ,a ,b)))
                             (is (equal ,dtype (dtype  a))
                                 ,(format nil "dtype of ~A + ~A = ~~A should be ~A. " a b dtype)
                                 a)
                             (is (equal ,lisp  (lisp<- a))
                                 ,(format nil "~A + ~A = ~~A should be ~A. " a b lisp)
                                 a))))))
    (test-add
     (nil t   :bool                     t)
     (t   nil :bool                     t)
     (1   2   *default-mlx-int-dtype*   3)
     (1.0 2.0 *default-mlx-float-dtype* 3.0)
     (t   2   *default-mlx-int-dtype*   3)
     (t   2.0 *default-mlx-float-dtype* 3.0)
     (1   2.0 *default-mlx-float-dtype* 3.0)
     (2   t   *default-mlx-int-dtype*   3)
     (2.0 t   *default-mlx-float-dtype* 3.0)
     (2.0 1   *default-mlx-float-dtype* 3.0)
     ((mlx-array t)   nil :bool                     t)
     ((mlx-array 1)   nil *default-mlx-int-dtype*   1)
     ((mlx-array t)   1   *default-mlx-int-dtype*   2)
     ((mlx-array 1.0) 1   *default-mlx-float-dtype* 2.0)
     (1 (mlx-array 1.0)   *default-mlx-float-dtype* 2.0))))

(test mean
  (let* ((*keep-dim-p* nil)
         (x (mlx-array #2A((1 2) (3 4))))
         (y (mean x :keep-dim-p t)))
    (is (equal (mean x)  2.5))
    (is (equal y         (mlx-array #2A((2.5)))))
    (is (equal (shape y) '(1 1)))
    (is (equal (mean x :axis 0) #(2 3)))
    (is (equal (mean x :axis 1) #(1.5 3.5)))))

(test var
  (let* ((*keep-dim-p* nil)
         (x (mlx-array #2A((1 2) (3 4))))
         (y (var x :keep-dim-p t)))
    (is (equal (var x)   1.25))
    (is (equal y         (mlx-array #2A((1.25)))))
    (is (equal (shape y) '(1 1)))
    (is (equal (var x :axis 0) #(1 1)))
    (is (equal (var x :axis 1) #(0.25 0.25))))

  (let ((x (mlx-array #(1.0 2.0))))
    (is (inf-p (var x :ddof 2))))

  (let ((x (mlx-array #(1.0 2.0))))
    (is (inf-p (var x :ddof 3)))))

(test abs
  (let ((a (mlx-array #(-1.0 1.0 -2.0 3.0))))
    (is (equal (abs a) #(1.0 1.0 2.0 3.0)))))

(test negative
  (let ((a (mlx-array #(-1.0 1.0 -2.0 3.0))))
    (is (equal (negative a) #(1.0 -1.0 2.0 -3.0)))
    (is (equal (- a)        #(1.0 -1.0 2.0 -3.0)))))

;;;; ops.lisp ends here
