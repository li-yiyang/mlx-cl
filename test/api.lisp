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

;; TODO: #mlx-cl #api
;; a new arange API should be considered
(test arange
  (is (equal (arange 5)   #(0 1 2 3 4))     "(arange STOP)")
  (is (equal (arange 1 5) #(1 2 3 4))       "(arange START STOP)")
  (is (equal (arange 5 :step 2) #(0 2 4))   "(arange STOP :step STEP)")
  (is (equal (arange 1 5 2)     #(1 3))     "(arange START STOP STEP)")
  (is (equal (arange 0 5 :step 2) #(0 2 4)) "(arange START STOP :step STEP)")

  ;; (is (equal (arange (mlx-array 0) 5 :step 2)
  ;;            (arange 0             5 :step 2))
  ;;     "Although not recommanded, the input parameter could be mlx-array scalar. ")
  )

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


(def-suite* indexing
  :description "Indexing elements in mlx-array. "
  :in mlx-api)

(test reorder~
  (is (equal (~)            '(~ :* :*  1))
      "(~) take range all (all elements on axis)")
  (is (equal (~ :step -1)   '(~ :* :* -1))
      "(~ :step -1) take range all in reverse")
  (is (equal (~ 5)          '(~ :* 5   1))
      "(~ 5) take 0-5 elements")
  (is (equal (~ 5 :step -1) '(~ :* 5  -1))
      "(~ :* 5 -1) take [0, 5) elements in reverse")
  (is (equal (~ 2 4)        '(~ 2  4   1))
      "(~ 2 4) take [2, 4) elements")
  (is (equal (~ 2 10 2)     '(~ 2  10  2))
      "(~ 2 10 2) take [2, 10) by step 2. "))

(test at
  (let ((arr (arange 10)))
    (is (equal (at arr 0) 0))
    (is (equal (at arr :first) 0))
    (is (equal (at arr '(:first 3))  #(0 1 2)))
    (is (equal (at arr '(:first -3)) #(2 1 0)))
    (is (equal (at arr '(:last  1))  9))
    (is (equal (at arr '(:last  3))  #(7 8 9)))
    (is (equal (at arr '(:last -3))  #(9 8 7)))
    (is (equal (at arr 1/2)          #(0 1 2 3 4)))
    (is (equal (at arr -1/2)         #(5 6 7 8 9)))
    (is (equal (at arr 1/3)          #(0 1 2 3)))))

(test at-2d
  (let ((arr (stack (arange 10) (arange 10))))
    (is (equal (at arr :all   1)     #(1 1)))
    (is (equal (at arr :first)       #(0 1 2 3 4 5 6 7 8 9)))
    (is (equal (at arr :first :even) #(0 2 4 6 8)))))

(test at-last
  (let ((arr (arange 10)))
    (is (equal (at arr :last) 9))
    (is (equal (at arr '(:last 2)) #(8 9)))
    (is (equal (at arr '(:last -2)) #(9 8)))))

(test at-middle
  (let ((arr (arange 5)))
    ;; 0 1 2 3 4
    (is (equal (at arr :middle) 2))
    (is (equal (at arr '(:middle 2)) #(1 2)))
    (is (equal (at arr '(:middle 3)) #(1 2 3)))
    (is (equal (at arr '(:middle -1)) 2))
    (is (equal (at arr '(:middle -2)) #(2 1)))
    (is (equal (at arr '(:middle -3)) #(3 2 1)))))


(def-suite* basic-operations
  :description "Some basic operations"
  :in mlx-api)

(test elementry-operation
  (loop :for i :from 1 :below 4
        :for args := (loop :repeat i :collect (1+ (cl:random 233)))
        :for arg2 := (mapcar #'mlx-array args)
        :do (loop :for mlx-op :in '(+     -    *     /)
                  :for cl-op  :in '(cl:+  cl:- cl:*  /)
                  :do (is (equal (apply mlx-op args) (apply cl-op args))
                          "For normal number operation, ~A is equal to ~A (args=~A)"
                          mlx-op cl-op args)
                  :do (is (lisp<- (~= (apply mlx-op arg2) (apply cl-op args)))
                          "For scalar operation, ~A is equal to ~A, too (args=~A)"
                          mlx-op cl-op args))))

(test single-op-inv
  (loop :repeat 10
        :for arg := (mlx-array (random 2333.0))
        :do (is (equal (/ arg) (reciprocal arg))
                "(/ ARG) = (reciprocal ARG)")
            (is (equal (- arg) (negative   arg))
                "(- ARG) = (negative ARG)")))

(test concat
  (is (equal (shape (concat '(#(1 2) #(3 4)))) '(4))
      "(concat arrays) -> axis is `nil' return 1-d array")
  (is (equal (shape (concat '(#2A((1 2))
                              #2A((3 4)))
                            :axis 0))
             '(2 2))
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
