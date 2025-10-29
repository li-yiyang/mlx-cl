;;;; api.lisp ---- Trivial API test

(in-package :mlx-cl.test)

(def-suite* mlx-api
  :description
  "Test MLX-CL's lisp API.

Goal:
The `mlx-cl' package should provide a numpy like API for
multi dimensional array operation. However, since Lisp
shares a different expression (S-expression) like Python's
syntax, some API may need modification to make it lispy.

Here within the test suite, the API of `mlx-cl' should be
introduced and then implemented. If you are a developer
who want to implement a new set of API for `mlx-cl', please
add the test first and then pull a pr. "
  :in mlx-test)


(def-suite* create-mlx-array
  :description "To create a `mlx-array'. "
  :in mlx-api)

(test mlx-array-scalar
  (let ((true  (mlx-array t))
        (false (mlx-array nil)))
    (is (equal true  t)   "`t' <-> mlx-array for true")
    (is (equal false nil) "`nil' <-> mlx-array for false"))

  (let ((true  (mlx-array 1  :dtype :bool))
        (false (mlx-array 0  :dtype :bool))
        (true1 (mlx-array (cl:1+ (cl:random 2333)) :dtype :bool))
        (true2 (mlx-array (cl:- (cl:1+ (cl:random 2333))) :dtype :bool)))
    (is (equal true  t)   "1 (dtype=bool) = true")
    (is (equal false nil) "0 (dtype=bool) = false")
    (is (equal true1 t)   "non-zero (dtype=bool) = true")
    (is (equal true2 t)   "non-zero (dtype=bool) = true"))

  (let* ((rnd    (cl:random 2333))
         (rndf   (cl:float rnd))
         (int    (mlx-array rnd))
         (uint   (mlx-array rnd :dtype :uint32))
         (float  (mlx-array rndf))
         (float2 (mlx-array rndf :dtype :float32)))
    (is (equal (dtype int)    *default-mlx-int-dtype*))
    (is (equal (dtype uint)   :uint32))
    (is (equal (dtype float)  *default-mlx-float-dtype*))
    (is (equal (dtype float2) :float32))))

(test mlx-array-list
  (is (equal (mlx-array '((1 2) (3 4))) #2A((1 2) (3 4)))
      "Convert from list into `mlx-array'. ")

  (is (equal (dtype (mlx-array '((1 2) (3 4.0)))) *default-mlx-float-dtype*)
      "Should auto determine dtype from input list. ")

  (is (equal (dtype (mlx-array '(1 2) :dtype :int32)) :int32)
      "Force convert of data dtype. ")

  ;; error for not uniform list
  (signals mlx-array-error
    (mlx-array '((1 2) (3) (4 5)))))

(test mlx-array-array
  (is (equal (mlx-array #(1 2)) #(1 2)))

  (let* ((arr (make-array '(2 2) :element-type '(unsigned-byte 8)
                                 :initial-contents '((1 2) (3 4))))
         (mlx (mlx-array arr)))
    (is (equal (shape arr) (shape mlx)))
    (is (equal (dtype arr) (dtype mlx)))
    (is (equal (dtype mlx) :uint8))))

(test arange
  (is (equal (arange 5)   #(0 1 2 3 4))     "(arange STOP)")
  (is (equal (arange 1 5) #(1 2 3 4))       "(arange START STOP)")
  (is (equal (arange 5 :step 2) #(0 2 4))   "(arange STOP :step STEP)")
  (is (equal (arange 1 5 2)     #(1 3))     "(arange START STOP STEP)")
  (is (equal (arange 0 5 :step 2) #(0 2 4)) "(arange START STOP :step STEP)"))

(test zeros
  (is (equal (zeros 3) #(0 0 0))
      "(zeros LEN) => 1-D mlx-array of LEN's zeros")
  (is (equal (shape (zeros '(1 2))) '(1 2))
      "(zeros SHAPE)")
  (is (equal (shape (zeros (mlx-array #2A((1 2) (3 4))))) '(2 2))
      "(zeros mlx-array) => zeros like"))

(test ones
  (is (equal (ones 3) #(1 1 1))
      "(ones LEN) => 1-D mlx-array of LEN's 1")
  (is (equal (shape (ones '(1 2))) '(1 2))
      "(ones SHAPE)")
  (is (equal (shape (ones (mlx-array #2A((1 2) (3 4))))) '(2 2))
      "(ones mlx-array) => ones like"))

(test full
  (is (full 5 3) #(3 3 3 3 3)
      "(full SHAPE CONST) return a array of SHAPE with each value as CONST")
  (is (equal (full '(2 2) 1) (ones  '(2 2))))
  (is (equal (full '(1 2) 0) (zeros '(1 2)))))


(def-suite* mlx-array-attributes
  :description "Get attributes from mlx-array. "
  :in mlx-api)

(test shape-of-number
  (is (equal (shape 2.0) ())
      "Shape of scalar is (). ")

  (signals mlx::mlx-axis-error
    (shape 2.0 :axis 1)
    "The shape of scalar does not have axis. "))

(test shape-of-list
  (let ((lst '((1 2 3) (4 5 6))))
    (is (equal (shape lst) '(2 3)))
    (is (equal (shape lst :axis 1) 3))
    (is (equal (shape lst :axis '(1 0)) '(3 2)))
    (is (equal (shape lst :axes '(1 0)) '(3 2)))))

(test shape-of-array
  (let ((arr (make-array '(2 3))))
    (is (equal (shape arr) '(2 3)))
    (is (equal (shape arr :axis 1) 3))
    (is (equal (shape arr :axis '(1 0)) '(3 2)))
    (is (equal (shape arr :axes '(1 0)) '(3 2)))))

(test shape-of-mlx-array
  (let ((arr (ones '(2 3))))
    (is (equal (shape arr) '(2 3)))
    (is (equal (shape arr :axis 1) 3))
    (is (equal (shape arr :axis '(1 0)) '(3 2)))
    (is (equal (shape arr :axes '(1 0)) '(3 2)))))


(def-suite* basic-operations
  :description "Some basic operations"
  :in mlx-api)

(test concat
  (is (equal (shape (concat '(#(1 2) #(3 4)))) '(4))
      "(concat arrays) -> axis is `nil' return 1-d array")
  (is (equal (shape (concat '(#2A((1 2))
                              #2A((3 4)))
                            :axis 0))
             '(4))
      "concat on axis"))

(test outer
  (is (equal (outer #(1.0 2.0 3.0) #(10.0 100.0 1000.0))
             #2A((10 100 1000)
                 (20 200 2000)
                 (30 300 3000)))))

(test inner
  (is (equal (inner #(1.0 2.0 3.0) #(4.0 5.0 6.0)) 32)))

(test ~=
  ;; ~= should test all elem with all-close
  (is (equal (~= 2.33000 2.33001) t))
  (is (equal (~= 2.33000 2.33003) nil))
  (is (equal (~= 2.33001 2.33003) t))
  (is (equal (~= 2.33000 2.33001 2.33003) nil)))

(test ~
  (is (equal (~ 10)            '(~ 0 10 1)))
  (is (equal (~ 5  10)         '(~ 5 10 1)))
  (is (equal (~ 5     :step 2) '(~ 0  5 2)))
  (is (equal (~ 5  10 3)       '(~ 5 10 3))))

(test slice
  (let ((x (reshape (arange (cl:* 3 3 3)) '(3 3 3))))
    (is (equal (slice x 1)
               #3A(((9  10 11)
                    (12 13 14)
                    (15 16 17))))
        "The integer should be single slice. ")

    (loop :for s :in '(:first :second :third :fourth :fifth
                       :sixth :seventh :eighth :ninth)
          :for i :from 0
          :do (is (equal (slice x s) (slice x i))
                  "The keyword `~S' should be equal to `~D'. "
                  s i))

    (is (equal (slice x 1/2)
               #3A((( 0  1  2)
                    ( 3  4  5)
                    ( 6  7  8))
                   (( 9 10 11)
                    (12 13 14)
                    (15 16 17))))
        "The rational should take 0-⌈shape * rational⌉. ")
    (is (equal (slice x -1/2)
               #3A((( 9 10 11)
                    (12 13 14)
                    (15 16 17))
                   ((18 19 20)
                    (21 22 23)
                    (24 25 26))))
        "The negative rational should take ⌊shape * rational⌋-shape. ")

    (is (equal (dim (slice x 0 0 0)) (dim x))
        "The slice of array should keep same dim of original array")
    (is (equal (squeeze (slice x 0 0 0)) 0)
        "Use squeeze to flatten the sliced array")

    (is (equal (slice x :* (~ 1 3) 1)
               #3A(((4)  (7))
                   ((13) (16))
                   ((22) (25))))
        "`~' take a range of subsets of [1, 3). ")

    (is (equal (slice x 0 (~~ 1 2))
               (slice x 0 (~  1 3)))
        "`~~' take a range of subsets of [1, 2]. ")

    ;; Note: currently mlx-c API does not support negative step.
    ;; need to fix it.
    (is (equal (squeeze (slice x (~ -1 :step -1) :first :first))
               #(18 9 0))
        "reverse slice")))


(def-suite* conv-operation
  :description "Testing for Conv relative operations. "
  :in mlx-api)

(test pad
  (is (equal (pad #2A((1 2)
                      (3 4)))
             (pad #2A((1 2)
                      (3 4))
                  1))
      "`pad' default padding width is 1")
  (is (equal (pad #2A((1 2) (3 4)) 1)
             #2A((0 0 0 0)
                 (0 1 2 0)
                 (0 3 4 0)
                 (0 0 0 0)))
      "`pad' around 2-D array. ")
  (is (equal (pad #2A((1 2) (3 4)) '(0 1))
             #2A((0 1 2 0)
                 (0 3 4 0)))
      "`pad' first axis with 0 padding, second axis with 1 padding. ")
  (is (equal (pad #(1 2) '(1 . 2))
             #(0 1 2 0 0))
      "`pad' left with 1, right with 2")
  (is (equal (pad #(1 2) 1 :mode :edge)
             #(1 1 2 2))
      "`pad' in `:edge' mode"))

;;;; api.lisp ends here
