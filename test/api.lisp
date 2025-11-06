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

(def-suite* mlx-array
  :description "Test for `mlx-array'. "
  :in create-mlx-array)

(test mlx-array-bool
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
    (is (equal true2 t)   "non-zero (dtype=bool) = true")))

(test mlx-array-scalar
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

(test mlx-array-complex
  (let* ((c (complex (random 2333.0) (random 2333.0)))
         (s (mlx-array c)))
    (is (equal (dtype c) :complex64))
    (is (equal c s))))

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
  (is (equal (mlx-array #(1 2 3)) #(1 2 3)))

  (let* ((arr (make-array '(2 2) :element-type '(unsigned-byte 8)
                                 :initial-contents '((1 2) (3 4))))
         (mlx (mlx-array arr)))
    (is (equal (shape arr) (shape mlx)))
    (is (equal (dtype arr) (dtype mlx)))
    (is (equal (dtype mlx) :uint8)
        "dtype should be inferred"))

  (is (equal (dtype (mlx-array #(1 2 3) :dtype :float32)) :float32)
      "dtype could be mannually set"))

(test mlx-array-complex-array
  (let* ((arr0 #2A((#C(1 2) #C(1 2) #C(1 2))
                   (#C(1 2) #C(1 2) #C(1 2))))
         (arr1 (tile #C(1 2) 2 3))
         (arr2 (mlx-array arr0)))
    (is (equal arr0 arr1))
    (is (equal arr1 arr2))
    (is (equal arr2 arr0))))

(test mlx-array-mlx-array
  (let* ((arr  (mlx-array #(1 2 3)))
         (arr2 (mlx-array arr :dtype :float32)))
    (is (equal (mlx-array arr) arr))
    (is (equal arr arr2))
    (is (equal (dtype arr2) :float32))))

(test arange
  (is (equal (arange 5)   #(0 1 2 3 4))     "(arange STOP)")
  (is (equal (arange 1 5) #(1 2 3 4))       "(arange START STOP)")
  (is (equal (arange 5 :step 2) #(0 2 4))   "(arange STOP :step STEP)")
  (is (equal (arange 1 5 2)     #(1 3))     "(arange START STOP STEP)")
  (is (equal (arange 0 5 :step 2) #(0 2 4)) "(arange START STOP :step STEP)")

  (is (equal (arange (mlx-array 0) 5 :step 2)
             (arange 0             5 :step 2))
      "Although not recommanded, the input parameter could be mlx-array scalar. "))

(test zeros
  (is (equal (zeros 3) #(0 0 0))
      "(zeros LEN) => 1-D mlx-array of LEN's zeros")
  (is (equal (shape (zeros '(1 2))) '(1 2))
      "(zeros SHAPE)")

  (let ((arr (mlx-array #2A((1 2) (3 4)))))
    (is (equal (zeros (shape arr)) (zeros-like arr))
        "(zeros mlx-array) => (zeros-like arr)")))

(test ones
  (is (equal (ones 3) #(1 1 1))
      "(ones LEN) => 1-D mlx-array of LEN's 1")
  (is (equal (shape (ones '(1 2))) '(1 2))
      "(ones SHAPE)")

  (let ((arr (mlx-array #2A((1 2) (3 4)))))
    (is (equal (ones (shape arr)) (ones-like arr))
        "(ones (shape mlx-array)) => (ones-like mlx-array)")))

(test full
  (is (full 5 3) #(3 3 3 3 3)
      "(full SHAPE CONST) return a array of SHAPE with each value as CONST")
  (is (equal (full '(2 2) 1) (ones  '(2 2))))
  (is (equal (full '(1 2) 0) (zeros '(1 2)))))

(test id
  (let ((id (id 2)))
    (is (equal (shape id) '(2 2)))
    (is (equal id #2A((1 0)
                      (0 1))))

    (let ((arr (mlx-array #2A((2 3)
                              (4 5)))))
      (is (equal (@ id arr) arr)))

    (is (equal (id #(2 2)) id)))

  ;; Expect a square shape
  (signals error
    (id #(2 3))))

(test eye
  (is (equal (eye 2) (id 2)))
  (is (equal (shape (eye '(2 3))) '(2 3)))
  (is (equal (eye '(2 3) :diag 1)  #2A((0 1 0)
                                       (0 0 1))))
  (is (equal (eye '(2 3) :diag -1) #2A((0 0 0)
                                       (1 0 0)))))

(test tri
  (is (equal (tri 2)
             #2A((1 0)
                 (1 1))))
  (is (equal (tri 2 3)
             #2A((1 0 0)
                 (1 1 0))))
  (is (equal (tri 2 3 :diag 1)
             #2A((1 1 0)
                 (1 1 1))))
  (is (equal (tri 2 3 :diag -1)
             #2A((0 0 0)
                 (1 0 0))))

  (is (equal (tri 2 3 :pos :upper)
             #2A((1 1 1)
                 (0 1 1))))
  (is (equal (tri 2 3 :pos :upper :diag 1)
             #2A((0 1 1)
                 (0 0 1))))
  (is (equal (tri 2 3 :pos :upper :diag -1)
             #2A((1 1 1)
                 (1 1 1)))))

(test linspace
  (let ((arr1 (linspace 0 1))
        (arr2 (linspace 0 1 10)))
    (is (equal (dim arr1) 1))
    (is (equal (shape arr1 :axis -1) 50)
        "Default linspace NUM is 50. ")
    (is (equal (sum arr1)
               (/ (* (+ 0 1) 50) 2))
        "0 + ... + 10 = (0 + 10) * N_{0 to 10} / 2")
    (is (equal (dim arr2) 1))
    (is (equal (shape arr2 :axis -1) 10))))

(test meshgrid
  (destructuring-bind (x y)
      (meshgrid (arange 2) (arange 2))
    (is (equal x #2A((0 1)
                     (0 1))))
    (is (equal y #2A((0 0)
                     (1 1)))))
  (destructuring-bind (x y)
      (meshgrid (arange 2) (arange 2) :indexing :ij)
    (is (equal x #2A((0 0)
                     (1 1))))
    (is (equal y #2A((0 1)
                     (0 1))))))


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

(test tr
  (is (equal (tr #2A((1 0)
                     (0 1)))
             2)))

(test diag
  (let ((dat (mlx-array #(1 2) :dtype :int8)))
    (is (equal (diag dat)
               #2A((1 0)
                   (0 2)))
        "Make a diagonal matrix from 1-D array. ")
    (is (equal (diag dat 1)
               #2A((0 1 0)
                   (0 0 2)
                   (0 0 0)))
        "N for diagonal offset")
    (is (equal (diag dat -1)
               #2A((0 0 0)
                   (1 0 0)
                   (0 2 0)))
        "N(<0) for offset down"))

  (let ((dat (mlx-array #2a((1 2 3 4)
                            (5 6 7 8)
                            (9 0 1 2)
                            (3 4 5 6))
                        :dtype :int8)))
    (is (equal (diag dat) #(1 6 1 6))
        "extract diagonal. ")
    (is (equal (diag dat  1) #(2 7 2))
        "offset")
    (is (equal (diag dat -1) #(5 0 5)))))

(test dtype
  (let ((arr (mlx-array 2 :dtype :float32)))
    (is (equal (dtype arr) :float32))
    (setf (dtype arr) :complex64)
    (is (equal (dtype arr) :complex64))
    (is (equal (lisp<- arr) #C(2 0)))))

(test finite/nan/inf/neg-inf/pos-inf-p
  )

(test num<-nan
  )

(test std-var
  )


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

(defmacro is-at (arr expect &rest slices
                 &aux
                   (got (gensym "GOT"))
                   (exp (gensym "EXPECT")))
  "(is (equal (at ARR . SLICES) EXPECT) ...)"
  `(let ((,got (at ,arr ,@slices))
         (,exp ,expect))
     (is (equal ,got ,exp)
         "Slice ~{~S~^ ~} shall return ~A, but got:~%~A. "
         ',slices ,exp ,got)))

(test at
  (let ((arr (arange 10)))
    (is-at arr 0 0)
    (is-at arr 0 :first)
    (is-at arr #(0 1 2)      '(:first  3))
    (is-at arr #(2 1 0)      '(:first -3))
    (is-at arr 9             '(:last   1))
    (is-at arr #(7 8 9)      '(:last   3))
    (is-at arr #(9 8 7)      '(:last  -3))
    (is-at arr #(0 1 2 3 4)  1/2)
    (is-at arr #(5 6 7 8 9) -1/2)
    (is-at arr #(0 1 2 3)    1/3)))

(test evaluate~
  (let ((arr (arange 10)))
    ;; step > 0
    (is-at arr #(0 1 2 3 4 5 6 7 8 9) (~))
    (is-at arr #(0 1 2 3 4 5 6 7 8)   (~ -1)) ; last element would be ignored
    (is-at arr #(0 1 2 3 4 5 6 7)     (~ -2)) ; ignore last two elements
    (is-at arr #(0 1 2 3 4)           (~ -5))
    (is-at arr #()                    (~ -10)) ; nothing would be sliced
    (is-at arr #(0 1 2 3 4)           (~  5))  ; [0, 5)
    (is-at arr #(0 1 2 3 4 5 6 7 8 9) (~  10)) ; [0, 10) => whole
    (is-at arr #(5 6 7 8 9)           (~ -5 :*)) ; starting from -5
    (is-at arr #(5 6 7 8 9)           (~  5 :*)) ; starting from  5
    ;; step < 0
    (is-at arr #(9 8 7 6 5 4 3 2 1 0) (~ :step -1))
    (is-at arr #(4 3 2 1 0)           (~ -5    :step -1))
    (is-at arr #(4 3 2 1 0)           (~  5    :step -1))
    (is-at arr #(9 8 7 6 5)           (~ -5 :* :step -1))
    (is-at arr #(9 8 7 6 5)           (~  5 :* :step -1))))

(test evaluate~~
  (let ((arr (arange 10)))
    ;; step > 0
    (is-at arr #(0 1 2 3 4 5 6 7 8 9) (~~))      ; should be same as (~)
    (is-at arr #(0 1 2 3 4 5 6 7 8 9) (~~ -1))   ; last element would be included
    (is-at arr #(0 1 2 3 4 5 6 7 8)   (~~ -2))   ; ignore last one elements
    (is-at arr #(0 1 2 3 4 5)         (~~ -5))
    (is-at arr #(0)                   (~~ -10))  ; first element would be included
    (is-at arr #(0 1 2 3 4 5)         (~~  5))   ; [0, 5]
    (is-at arr #(0 1 2 3 4 5 6 7 8 9) (~~  9))   ; [0, 9] => whole
    (is-at arr #(5 6 7 8 9)           (~~ -5 :*)) ; starting from -5
    (is-at arr #(5 6 7 8 9)           (~~  5 :*)) ; starting from  5
    ;; step < 0
    (is-at arr #(9 8 7 6 5 4 3 2 1 0) (~~ :step -1))
    (is-at arr #(5 4 3 2 1 0)         (~~ -5    :step -1))
    (is-at arr #(5 4 3 2 1 0)         (~~  5    :step -1))
    (is-at arr #(9 8 7 6 5)           (~~ -5 :* :step -1))
    (is-at arr #(9 8 7 6 5)           (~~  5 :* :step -1))))

(test at-2d
  (let ((arr (stack (arange 10) (arange 10))))
    (is-at arr #(1 1)                 :all 1)
    (is-at arr #(0 1 2 3 4 5 6 7 8 9) :first)
    (is-at arr #(0 2 4 6 8)           :first :even)))

(test at-keep-dim-p
  (let ((arr (stack (arange 10) (arange 10)))
        (*keep-dim-p* t))
    (is (equal (dim (at arr :first)) (dim arr))
        "If `*keep-dim-p*' is non-nil, the `at' result as same `dim' as input. ")))

(test at-take
  (let ((arr (arange 10)))
    (is (equal (at arr '(1 2 3)) #(1 2 3))
        "If given sequence of index as slice, would take element on the axis. ")))

(test at-setf
  (let ((arr (arange 5)))
    (setf (at arr :last) 5)
    (is (equal arr #(0 1 2 3 5))
        "(setf at) would modify the given array value")))

(test at-first
  (let ((arr (arange 10)))
    (is-at arr 0        :first)
    (is-at arr #(0 1) '(:first  2))
    (is-at arr #(1 0) '(:first -2))))

(test at-last
  (let ((arr (arange 10)))
    (is-at arr 9        :last)
    (is-at arr #(8 9) '(:last  2))
    (is-at arr #(9 8) '(:last -2))))

(test at-nth
  (let ((arr (arange 10)))
    (loop :for i :from 1
          :for k :in  '(:second :third :fourth :fifth :sixth
                        :seventh :eighth :ninth :tenth)
          :do (is (equal (at arr i) (at arr k))
                  "Slice ~S should be alias of ~A. "
                  k i))))

(test at-middle
  (let ((arr (arange 5)))
    ;; 0 1 2 3 4
    (is-at arr 2          :middle)
    (is-at arr #(1 2)   '(:middle  2))
    (is-at arr #(1 2 3) '(:middle  3))
    (is-at arr 2        '(:middle -1))
    (is-at arr #(2 1)   '(:middle -2))
    (is-at arr #(3 2 1) '(:middle -3))))

(test at-all
  (let ((arr (arange 10)))
    (is (equal (at arr :*)   (at arr :all))
        "Slice :* should be alias of :all")
    (is-at arr #(0 1 2 3 4 5 6 7 8 9) :all)))

(test at-reverse
  (let ((arr (arange 10)))
    (is (equal (at arr :reverse) (at arr (~ :step -1))))
    (is-at arr #(9 8 7 6 5 4 3 2 1 0) :reverse)))

(test at-skip
  (let ((arr (arange 10)))
    (is-at arr #(0 2 4 6 8)   :skip)
    (is-at arr #(0 3 6 9)   '(:skip 2))
    (is-at arr #(1 3 5 7 9) '(:skip 1 :start 1))))

(test at-odd/even
  (let ((arr (arange 10)))
    (is (equal (at arr :odd)  (at arr '(:skip 1 :start 1)))
        "Slice :odd should be alias of (:skip 1 :start 1)")
    (is (equal (at arr :even) (at arr '(:skip 1 :start 0)))
        "Slice :even should be alias of (:skip 1 :start 0)")))


(def-suite* basic-operations
  :description "Basic Operations"
  :in mlx-api)

;; Test for `add', `sub', `mul', `div'.
;; And test for `+', `-', `*', `/'.
(test elementry-operation
  (loop :for i :from 1 :below 4
        :for args := (loop :repeat i :collect (1+ (cl:random 233)))
        :for arg2 := (mapcar #'mlx-array args)
        :do (loop :for mlx-op :in '(+     -    *     /)
                  :for cl-op  :in '(cl:+  cl:- cl:*  cl:/)
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


(def-suite* 1arg-op-with-cl-wrap
  :description "Test operators with 1 arguments and fallback to common-lisp functions. "
  :in basic-operations)

(test abs
  (loop :for i :in '(1 -1 0)
        :do (is (equal (abs 1) (cl:abs 1)))
            (is (lisp<- (~= (abs (mlx-array 1)) (cl:abs 1))))))

(test exp-log
  (let ((args (linspace -5 5 10))
        (arg  (- (cl:random 10.0) 5.0)))
    (is (equal (exp arg) (cl:exp arg)))
    (is (lisp<- (~= (exp args)
                    (map 'vector #'cl:exp (lisp<- args))))))

  (let ((args (linspace 0.5 10 10))
        (arg  (+ (cl:random 10.0) 0.5)))
    (is (equal (log arg) (cl:log arg)))
    (is (lisp<- (~= (log args) (map 'vector #'cl:log (lisp<- args)))))))

(test square-sqrt
  (let* ((args (linspace 0 10 10))
         (arg* (lisp<- args))
         (arg  (cl:random 10.0)))
    (is (equal (square arg) (cl:* arg arg)))
    (is (lisp<- (~= (square args)
                    (map 'vector (lambda (x) (cl:* x x)) arg*))))

    (is (equal (sqrt arg) (cl:sqrt arg)))
    (is (lisp<- (~= (sqrt args)
                    (map 'vector #'cl:sqrt arg*))))))

(test trigonometric-functions
  (let* ((args (linspace 0.0 0.5 10))
         (arg* (lisp<- args))
         (arg  (cl:random 0.5)))
    (loop :for f :in '(sin     cos     tan     sinh     cosh     tanh
                       asin    acos    atan    asinh             atanh)
          :for c :in '(cl:sin  cl:cos  cl:tan  cl:sinh  cl:cosh  cl:tanh
                       cl:asin cl:acos cl:atan cl:asinh          cl:atanh)
          :do (is (equal (funcall f arg) (funcall c arg)))
              (is (lisp<- (~= (funcall f args)
                              (map 'vector c arg*)))
                  "Test (~S [...]) == (map 'vector ~S [...])" f c)))

  (let* ((args (linspace 2 10 10))
         (arg* (lisp<- args))
         (arg  (cl:+ (cl:random 10) 2)))
    (is (equal (acosh arg) (cl:acosh arg)))
    (is (lisp<- (~= (acosh args) (map 'vector #'cl:acosh arg*))))))


(def-suite* 2args-op-with-cl-wrap
  :description "Test operators with 2 arguments and fallback to common-lisp functions. "
  :in basic-operations)

(test remainder
  (let ((num 9)
        (div 2))
    (is (equal (remainder num div) (cl:mod num div))))
  (let ((num #(9 2 8 3 4 5))
        (div 2))
    (is (equal (remainder num div) (map 'vector (lambda (n) (cl:mod n div)) num))))
  (let ((num #(3 9 3 0 9 4))
        (div #(1 2 3 4 5 6)))
    (is (equal (remainder num div) (map 'vector #'cl:mod num div)))))

(test divmod
  (loop :for (num div) :in '((9 2)
                             (#(1 2 3 4 5 6 7 8 9) 4)
                             (#(1 2 3 4 5)         #(6 5 3 2 8)))
        :do (multiple-value-bind (n r) (mod num div)
              (is (equal num (+ (* n div) r))))))


(def-suite* clipping
  :description ""
  :in basic-operations)

(test floor
  (is (equal (floor 1.23) (cl:floor 1.23)))
  (is (equal (floor #(1.23 3.632 0.234 2.0)) #(1 3 0 2)))
  (is (equal (floor #(4.32 3.132 4.0) 2.0)   #(2 1 2))))

(test ceiling
  (is (equal (ceiling 1.23) (cl:ceiling 1.23)))
  (is (equal (ceiling #(1.23 3.632 0.234 2.0)) #(2 4 1 2)))
  (is (equal (ceiling #(4.32 3.132 4.0)  2.0)  #(3 2 2))))

(test clip
  (is (equal (clip #(1 2 3 4 5 6 7 8 9) :min 3 :max 6)
             #(3 3 3 4 5 6 6 6 6)))
  (is (equal (clip #(1 2 3 4 5 6 7) :min 3)
             #(3 3 3 4 5 6 7)))
  (is (equal (clip #(1 2 3 4 5 6 7) :max 6)
             #(1 2 3 4 5 6 6))))

(test round
  (is (equal (round 2.34)    2))
  (is (equal (round 2.34  1) 2.3))
  (is (equal (round 23.4 -1) 20)))


(def-suite* products
  :description "Test product operators. "
  :in basic-operations)

;; Test for `matmul', `@'.
(test matmul
  (let ((arr (mlx-array #2A((1 2) (3 4)))))
    (is (equal (matmul (id 2) arr) arr))
    (is (equal (@ (id 2) (eye #(2 3))
                  (mlx-array #2A((1 2 3)
                                 (4 5 6)
                                 (7 8 9))))
               #2A((1 2 3)
                   (4 5 6))))))

(test inner
  (is (equal (inner #(1.0 2.0 3.0) #(4.0 5.0 6.0)) 32)))

(test outer
  (is (equal (outer #(1.0 2.0 3.0) #(10.0 100.0 1000.0))
             #2A((10 100 1000)
                 (20 200 2000)
                 (30 300 3000)))))

(test kron
  (is (equal (kron (id 2) #2A((1 2)
                              (3 4)))
             #2A((1 2 0 0)
                 (3 4 0 0)
                 (0 0 1 2)
                 (0 0 3 4)))))

(test tensordot
  (is (equal (tensordot #(1.0 2.0 3.0) #(4.0 5.0 6.0) :axis 1)
             (inner     #(1.0 2.0 3.0) #(4.0 5.0 6.0))))
  (is (equal (tensordot (id 2) #2A((1 2) (3 4)) :axis 1)
             (matmul    (id 2) #2A((1 2) (3 4)))))

  (let ((a (reshape (arange 24) #(2 3 4)))
        (b (reshape (arange 12) #(3 4))))
    (is (equal (tensordot a b :axes1 #(1 2) :axes2 #(0 1))
               (einsum "ijk,jk->i" a b)))))


(test bitwise-ops
  (let* ((args (arange 5 :dtype :int8))
         (arg  (random 5))
         (arg* (mlx-array arg :dtype :int8)))
    (is (equal (lognot arg*) (cl:lognot arg)))
    (is (equal (lognot args) (map 'vector #'cl:lognot (lisp<- args)))))

  (let ((args1 (mlx-array (loop :repeat 5 :collect (random 5)) :dtype :int8))
        (args2 (mlx-array (loop :repeat 5 :collect (random 5)) :dtype :int8))
        (arg1  (random 5))
        (arg2  (random 5)))
    (loop :for f :in '(logand bit-and
                       logxor bit-xor
                       logior bit-ior bit-or)
          :for c :in '(cl:logand cl:logand
                       cl:logxor cl:logxor
                       cl:logior cl:logior cl:logior)
          :do (is (equal (funcall f
                                  (mlx-array arg1 :dtype :int8)
                                  (mlx-array arg2 :dtype :int8))
                         (funcall c arg1 arg2)))
              (is (equal (funcall f args1 args2)
                         (map 'vector c (lisp<- args1) (lisp<- args2)))))))


(def-suite* miscs
  :description "Misc operations"
  :in basic-operations)


(def-suite* control-flow
  :description "Control flow"
  :in basic-operations)

;; Test for `op2<', `op2<=', `op2>', `op2>=', `op2=', `op2/='
(test comparing
  )

;; Test for `op2and', `op2or', `logical-and', `logial-or', `logical-not'
(test logical-operation
  )

(test ~=
  ;; ~= should test all elem with all-close
  (is (equal (~= 2.33000 2.33001) t))
  (is (equal (~= 2.33000 2.33003) nil))
  (is (equal (~= 2.33001 2.33003) t))
  (is (equal (~= 2.33000 2.33001 2.33003) nil)))


(def-suite* operation-on-shape
  :description "Operations on `mlx-array' shape "
  :in mlx-api)

(test roll
  ;; mx.roll(mx.array([1, 2, 3, 4]), 2)
  (is (equal (roll #(1 2 3 4) 2) #(3 4 1 2)))
  ;; mx.roll(mx.array([[1, 2], [3, 4]]), 2)
  (is (equal (roll #2A((1 2) (3 4)) 2)
             #2A((3 4)
                 (1 2))))
  ;; mx.roll(mx.array([[1, 2], [3, 4]]), 1, axis=(1, 0))
  (is (equal (roll #2A((1 2) (3 4)) 1 :axes #(1 0))
             #2A((4 3) (2 1)))))

(test atleast
  (destructuring-bind (a b c)
      (atleast 1 #(1 2) #2A((1 2) (3 4)))
    (is (dim>= a 1))
    (is (dim>= b 1))
    (is (dim>= c 1)))

  (destructuring-bind (a b c)
      (atleast 1 #(1 2) #2A((1 2) (3 4)) :dim 2)
    (is (dim>= a 2))
    (is (dim>= b 2))
    (is (dim>= c 2)))

  (destructuring-bind (a b c)
      (atleast 1 #(1 2) #2A((1 2) (3 4)) :dim 3)
    (is (dim>= a 3))
    (is (dim>= b 3))
    (is (dim>= c 3)))

  (signals error
    (atleast 1 #(1 2) #2A((1 2) (3 4)) :dim 4)))

(test expand-dims
  (is (equal (shape (expand-dims (ones 3) 0))          '(1 3)))
  (is (equal (shape (expand-dims (ones 3) 1))          '(3 1)))
  (is (equal (shape (expand-dims (ones '(2 3 4)) 1 2)) '(2 1 1 3 4))))

(test reshape
  (let ((arr (arange 6)))
    (is (equal (shape (reshape arr '(2 3))) '(2 3)))))

(test swap-axes
  (let ((arr (reshape (arange 8) '(2 2 2))))
    ;; mx.swapaxes(mx.reshape(mx.arange(8), (2, 2, 2)), 0, 1)
    (is (equal (swap-axes arr 0 1)
               #3A(((0 1) (4 5))
                   ((2 3) (6 7)))))
    (is (equal (swap-axes arr 0 2)
               #3A(((0 4) (2 6))
                   ((1 5) (3 7)))))
    (is (equal (swap-axes arr 1 2)
               #3A(((0 2) (1 3))
                   ((4 6) (5 7)))))))

(test transpose
  (let ((arr (mlx-array #2A((1 2)
                            (3 4)))))
    (is (equal (transpose arr)
               #2A((1 3)
                   (2 4)))))

  (let ((arr (reshape (arange 8) '(2 2 2))))
    ;; #(0 1 2) -> #(2 1 0)
    (is (equal (transpose arr :axes #(2 1 0))
               (swap-axes arr 0 2)))))

(test flatten
  (let ((arr (mlx-array #3A(((1) (2))
                            ((3) (4))))))
    (is (equal (flatten arr)          #(1 2 3 4)))
    (is (equal (flatten arr :start 1) #2A((1 2) (3 4))))
    (is (equal (flatten arr :stop -2) #2A((1) (2) (3) (4))))))

(test unflatten
  (let ((arr (mlx-array #2A((1 2 3 4)
                            (3 4 5 6)))))
    (is (equal (unflatten arr 1 '(2 -1)) #3A(((1 2) (3 4))
                                             ((3 4) (5 6)))))))


(def-suite* conc-and-split-mlx-array
  :description "Construct mlx-array or split mlx-array. "
  :in mlx-api)

(test concat
  (is (equal (concat #(1 2)
                     #(3 4))
             #(1 2 3 4)))
  (is (equal (concat #2A((1 2))
                     #2A((3 4))
                     :axis nil)
             #(1 2 3 4)))
  (is (equal (concat #2A((1 2))
                     #2A((3 4))
                     :axis 0)
             #2A((1 2) (3 4))))
  (is (equal (concat #2A((1 2))
                     #2A((3 4))
                     :axis 1)
             #2A((1 2 3 4)))))

(test stack
  ;; mx.stack((mx.array([[1, 2]]), mx.array([[3, 4]])), axis=0)
  (let ((arr (stack #(1 2 3)
                    #(4 5 6))))
    (is (equal (shape arr) '(2 3)))
    (is (equal arr #2A((1 2 3)
                       (4 5 6)))))
  ;; mx.stack((mx.array([[1, 2]]), mx.array([[3, 4]])), axis=1)
  (let ((arr (stack #2A((1 2))
                    #2A((3 4))
                    :axis 1)))
    (is (equal (shape arr) '(1 2 2)))
    (is (equal arr #3A(((1 2)
                        (3 4))))))

  ;; mx.stack((mx.array([[1, 2]]), mx.array([[3, 4]])), axis=2)
  (let ((arr (stack #2A((1 2))
                    #2A((3 4))
                    :axis 2)))
    (is (equal (shape arr) '(1 2 2)))
    (is (equal arr #3A(((1 3)
                        (2 4))))))

  (signals mlx-error
   (stack #2A((1 2))
          #2A((3 4))
          :axis 3)))

(test tile
  (is (equal (tile #(1 2) 2)
             #(1 2 1 2)))
  (is (equal (tile #(1 2) 2 2)
             #2A((1 2 1 2)
                 (1 2 1 2)))))

(test repeat
  (is (repeat 2 3) #(2 2 2))
  (is (repeat #2A((1 2)) 2 :axis 1)
      #2A((1 1 2 2))))

(test split
  (let ((arr (arange 10)))
    ;; mx.split(mx.arange(10), 2)
    (is (= (length (split arr 2)) 2))
    (destructuring-bind (a b) (split arr 2)
      (is (equal a #(0 1 2 3 4)))
      (is (equal b #(5 6 7 8 9))))

    ;; mx.split(mx.arange(10), (2, 4))
    (is (= (length (split arr '(2 4))) 3))
    (destructuring-bind (a b c) (split arr '(2 4))
      (is (= (shape a :axis 0) 2))
      (is (equal a #(0 1)))
      (is (= (shape b :axis 0) 2))
      (is (equal b #(2 3)))
      (is (= (shape c :axis 0) 2))
      (is (equal c #(4 5 6 7 8 9))))))


(def-suite* sorting-operation
  :description "Operations that sorts the mlx-array. "
  :in mlx-api)

(test sort
  (let ((seq (copy #(2 3 1 2 3 9 2 1 2))))
    (is (equal (sort seq) (cl:sort seq #'cl:<)))))

(test topk
  (is (equal (topk (arange 5) 3) #(2 3 4)))
  (is (equal (topk #2A((1 2 3 4 5)
                       (0 9 8 7 6))
                   3 :axis 1)
             #2A((3 4 5)
                 (7 8 9)))))


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

(test conv
  )


(def-suite* cummulative-operation
  :description "Test for cummulative operations. "
  :in mlx-api)


(def-suite* einsum
  :description "Testing for einsum. "
  :in mlx-api)

(test einsum
  )


(def-suite* better-for-dev
  :description "API should better preserved for developement use only. "
  :in mlx-api)

;;;; api.lisp ends here
