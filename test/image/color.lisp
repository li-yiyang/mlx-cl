;;;; color.lisp --- Test for color and colorspace conversion.

(in-package :mlx-cl.test.image)

(def-suite* mlx-image-test-color
  :description "Test for color and colorspace conversion. "
  :in mlx-image-test)

;;; Create color

(test color-array
  (let ((color (color #(255 255 0) :dtype :uint8)))
    (is (equal (colorspace color) :rgb)
        "The colorspace should be automatically determined. ")
    (is (equal (channels   color) 3)
        "`color' is just like image. ")
    (is (equal (w color) 1)
        "`color' is (1 1 3) `image'. ")
    (is (equal (h color) 1)
        "`color' is (1 1 3) `image'. ")
    (is (equal (dtype color) :float32)
        "`color' is a `:float32' array. ")))

(test color-mlx-array
  (let ((color (color (mlx-array #(255 255 0) :dtype :uint8))))
    (is (equal (colorspace color) :rgb)
        "The colorspace should be automatically determined. ")))

(test color-color-name
  (let ((color (color :black)))
    (is (equal color #3A(((0 0 0))))
        "`color' is internally represented as a float array (1 1 3), but got:~%~A"
        (mlx-array color))))

(test color-colorspace
  (let ((color (color :black :colorspace :gray)))
    (is (equal (colorspace color) :grayscale)
        "`color' should be graysacle (set mannually)")
    (is (equal color #3A(((0))))
        "`:grayscale' color is (1 1 1) array as float array, but got:~%~A"
        (mlx-array color)))

  (let* ((*colorspace* :grayscale)
         (color        (color :black)))
    (is (equal (colorspace color) :grayscale)
        "`color' should be grayscale (default as `*colorspace*')")
    (is (equal color #3A(((0))))
        "`:grayscale' color is (1 1 1) array as float array, but got:~%~A"
        (mlx-array color))))

;;;; color.lisp ends here
