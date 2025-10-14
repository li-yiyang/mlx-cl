;;;; ops.lisp --- Operators for MLX

(in-package :mlx)


;;; Config

(declaim
 (type boolean *nan-equal-p* *keep-dim-p*)
 (type float
       *relative-tolerance*
       *absolute-tolerance*))

(defparameter *relative-tolerance* 1e-5
  "Default relative tolerance.

See `all-close' and `~='. ")

(defparameter *absolute-tolerance* 1e-8
  "Default absolute tolerance.

See `all-close' and `~='. ")

(defparameter *nan-equal-p* nil
  "Whether NaN is equal to other value. ")

(defparameter *keep-dim-p* nil
  "Whether keep dimensions at computing. ")


;;; Wrapper

(defmacro with-mlx-op (name &body arg-type?)
  "Wrap a MLX calling with NAME and ARG-TYPE?.

Syntax:

    (with-mlx-op
      { name | (name &key alloc wrap) }
      { arg | (arg type) }*
      )
"
  (let ((res  (gensym "RES"))
        (res& (gensym "RES&")))
    (destructuring-bind
        (name &key (alloc 'mlx_array_new) (wrap 'wrap-as-mlx-array))
        (listfy name)
      `(with-elem& (,res ,res& :type  :pointer
                               :alloc (,alloc))
         (ensure-success ,name
           :pointer ,res&
           ,@(loop :for arg-type :in arg-type?
                   :for (arg type) := (if (listp arg-type)
                                          arg-type
                                          (list arg-type 'mlx-object))
                   :if (cl:and (cl:not (keywordp type))
                               (subtypep type 'mlx-object))
                     :collect :pointer
                     :and :collect `(mlx-object-pointer ,arg)
                   :else
                     :collect type
                     :and :collect arg)
           :pointer (mlx-object-pointer (default-mlx-stream)))
         (,wrap ,res)))))

;; (trivial-indent:define-indentation defmlx-method (2 &lambda &body))
(defmacro defmlx-method (name lambda-list docstring &body body)
  "Define a MLX method with name.

Parameters:
+ NAME: a symbol as method name
+ LAMBDA-LIST: (arg... &key key... &aux ...)
+ DOCSTRING: a documentation string
+ BODY: method body
"
  (flet ((collect-before (list &rest test)
           (loop :for arg :in list
                 :if (find arg test)
                   :return collect
                 :else :collect arg :into collect
                 :finally (return collect)))
         (collect-after (list &rest test)
           (loop :with flag := nil
                 :for (arg . rest) :on list
                 :if (find arg test)
                   :return rest))
         (atomize (elem)
           (if (listp elem) (car elem) elem)))
  (let* ((flat-lambda-list (mapcar #'atomize (collect-before lambda-list '&aux)))
         (mlx-array-args   (collect-before lambda-list '&key '&aux))
         (key-args         (collect-before (collect-after lambda-list '&key) '&aux))
         (aux-args         (collect-after  lambda-list '&aux)))
    `(defgeneric ,name ,flat-lambda-list
       (:documentation ,docstring)
       (:method (,@(mapcar #'atomize mlx-array-args)
                 ,@(when key-args
                     (cons '&key (loop :for key :in key-args
                                       :if (listp key)
                                         :collect (list (first key) (second key))
                                       :else
                                         :collect key))))
         (,name ,@(loop :for arg :in mlx-array-args
                        :if (listp arg)
                          :collect (car arg)
                        :else
                          :collect `(mlx-array ,arg))
                ,@(loop :for arg* :in key-args
                        :for arg := (if (listp arg*) (car arg*) arg*)
                        :collect (intern (symbol-name arg) :keyword)
                        :collect arg)))
       (:method (,@(loop :for arg :in mlx-array-args
                         :if (listp arg)
                           :collect arg
                         :else
                           :collect `(,arg mlx-array))
                 ,@(when key-args
                     (cons '&key key-args))
                 ,@(when aux-args
                     (cons '&aux aux-args)))
         ,@body)))))

;; single argument operator, with Common Lisp functions as fallback
(macrolet ((1ops (&body ops)
             `(progn
                ,@(loop :for (op cffi cl . docstring) :in ops
                        :collect
                        `(defgeneric ,op (X)
                           (:documentation
                            ,(format nil "Return ~A(X).
~{~A~%~}"
                                     op
                                     docstring))
                           ,@(when cl
                               `((:method ((num number))
                                   (,cl num))))
                           (:method (elem)
                             (,op (mlx-array elem)))
                           (:method ((arr mlx-array))
                             (with-mlx-op ,cffi arr)))))))
  (1ops
   (abs         "mlx_abs"         cl:abs)
   (square      "mlx_square"      square)
   (sqrt        "mlx_sqrt"        cl:sqrt)
   (rsqrt       "mlx_rsqrt"       nil
                "rsqrt(x) -> sqrt(1 / x)")

   (negative    "mlx_negative"    nil)

   (ceiling     "mlx_ceil"        cl:ceiling  ; TODO: (ceiling num &optional div)
                "ceiling(x) -> ⌈x⌉")
   (floor       "mlx_floor"       cl:floor    ; TODO: (floor   num &optional div)
                "floor(x) -> ⌊x⌋")

   (conjugate   "mlx_conjugate"   cl:conjugate)
   (copy        "mlx_copy"        nil)
   (degrees     "mlx_degrees"     nil)
   (logical-not "mlx_logical_not" nil)

   (loge        "mlx_log"         cl:log)
   (log10       "mlx_log10"       nil)
   (log2        "mlx_log2"        nil)
   (log1+       "mlx_log1p"       nil)

   (finite-p    "mlx_finite"      nil)
   (nan-p       "mlx_isnan"       nil)
   (inf-p       "mlx_isinf"       nil)
   (neg-inf-p   "mlx_isneginf"    nil)
   (pos-inf-p   "mlx_isposinf"    nil)

   (sigmoid     "mlx_sigmoid"     nil)

   (sin         "mlx_sin"         cl:sin)
   (cos         "mlx_cos"         cl:cos)
   (tan         "mlx_tan"         cl:tan)
   (sinh        "mlx_sinh"        cl:sin)
   (cosh        "mlx_cosh"        cl:cos)
   (tanh        "mlx_tanh"        cl:tan)
   (arcsin      "mlx_arcsin"      cl:asin)
   (arccos      "mlx_arccos"      cl:acos)
   (arctan      "mlx_arctan"      cl:atan)
   (arcsinh     "mlx_arcsinh"     cl:asinh)
   (arccosh     "mlx_arccosh"     cl:acosh)
   (arctanh     "mlx_arctanh"     cl:atanh)))

(macrolet ((1ops-dtype (&rest ops)
             `(progn
                ,@(loop :for (op mlx-fn . docstring) :in ops
                        :collect
                        `(defmlx-method ,op (array (dtype t)
                                              &aux (dtype! (ensure-mlx-dtype dtype)))
                           ,(format nil "~{~A~%~}
Parameters:
+ ARRAY: Input array or scalar.
+ DTYPE: The data type to change to."
                                    docstring)
                           (with-mlx-op ,(format nil "mlx_~(~A~)" mlx-fn)
                             array
                             (dtype! mlx-dtype)))))))
  (1ops-dtype
   (dtype<- "astype" "Convert array as a different type. ")
   (view    "view"   "View the array as a different type."
            "The output shape changes along the last axis if the input array’s
type and the input dtype do not have the same size.

Note: the view op does not imply that the input and output arrays share their
underlying data. The view only gaurantees that the binary representation of
each element (or group of elements) is the same.")))

(macrolet ((2ops (&body ops)
             `(progn
                ,@(loop :for (op expr cffi cl) :in ops
                        :collect
                        `(defgeneric ,op (a b)
                           (:documentation
                            (format nil "Return ~A.

Definition:
~A(a, b) : a, b is scalar -> a ~A b
~A(a, b) : a, b is array  -> { ai ~A bi }
~A(a, b) : a is scalar, b is array -> { a ~A bi }
"
                                    expr
                                    op expr
                                    op expr
                                    op expr))
                           (:method (elem1 elem2)
                             (,op (mlx-array elem1) (mlx-array elem2)))
                           ,@(when cl
                               `((:method ((num1 number) (num2 number))
                                   (,cl num1 num2))))
                           (:method ((arr mlx-array) num)
                             (,op arr (mlx-array num)))
                           (:method (num (arr mlx-array))
                             (,op (mlx-array num) arr))
                           (:method ((arr1 mlx-array) (arr2 mlx-array))
                             (with-mlx-op ,cffi arr1 arr2)))))))
  (2ops
   (add         "A + B"                "mlx_add"           cl:+)
   (sub         "A - B"                "mlx_subtract"      cl:-)
   (mul         "A * B"                "mlx_multiply"      cl:*)
   (div         "A / B"                "mlx_divide"        cl:/)
   (mod         "A % B"                "mlx_divmod"        cl:mod)
   (matmul      "A . B"                "mlx_matmul"        nil)

   (arctan2     "tan(A / B)"           "mlx_arctan2"       nil)

   (op2<        "A < B"                "mlx_less"          cl:<)
   (op2<=       "A <= B"               "mlx_less_equal"    cl:<=)
   (op2>        "A > B"                "mlx_greater"       cl:>)
   (op2>=       "A >= B"               "mlx_greater_equal" cl:>=)

   (logaddexp   "log(exp(A) + exp(B))" "mlx_logaddexp"     nil)
   (logical-and "A ∧ B"                "mlx_logical_and"   nil)
   (logical-or  "A ∨ B"                "mlx_logical_or"    nil)

   (logand      "A & B"                "mlx_bitwise_and"   cl:logand)
   (logxor      "A ^ B"                "mlx_bitwise_xor"   cl:logxor)
   (logior      "A | B"                "mlx_bitwise_or"    cl:logior)))

(macrolet ((2ops-reduce (&rest op-reduce)
             `(progn
                ,@(loop :for (op reduce single . docstring) :in op-reduce
                        :collect `(defun ,op (elem &rest more-elem)
                                    ,(format nil "~{~A~%~}see `~S'. " docstring reduce)
                                    (if (endp more-elem) ,single
                                        (reduce #',reduce more-elem :initial-value elem)))))))
  (2ops-reduce
   (+ add (copy elem)     "Return sum of ELEM and MORE-ELEM")
   (- sub (negative elem) "Return ELEM substract MORE-ELEM. "
      "If given only one ELEM, return -ELEM. ")
   (* mul (copy elem)     "Return multiply of ELEM and MORE-ELEM")
   (/ div (div (ones elem) elem) "Return ELEM divided by MORE-ELEM. "
      "If given only one ELEM, return 1/ELEM. ")))

(macrolet ((1ops-single-or-multiply (&rest ops)
             `(progn
                ,@(loop :for (op mlx-fn . docstring) :in ops
                        :collect
                        `(defun ,op (array &rest more-arrays)
                           ,(format nil "~{~A~%~}" docstring)
                           (flet ((f (x) (with-mlx-op ,mlx-fn
                                           ((mlx-object-pointer x) :pointer))))
                             (if (endp more-arrays)
                                 (f array)
                                 (mapcar #'f (cons array more-arrays)))))))))
  (1ops-single-or-multiply
   (at-least-1d "mlx_atleast_1d" "Convert all arrays to have at least one dimension.")
   (at-least-2d "mlx_atleast_2d" "Convert all arrays to have at least two dimensions.")
   (at-least-3d "mlx_atleast_3d" "Convert all arrays to have at least three dimensions.")))


(macrolet ((1ops-axis-axes-keepdims-ddof (&rest ops)
             `(progn
                ,@(loop :for (name mlx-fn . docstring) :in ops
                        :collect
                        `(defmlx-method ,name
                             (array &key axis axes ddof divisor (keep-dim-p *keep-dim-p*)
                              &aux
                                (axis* (cl:or axes axis))
                                (div   (cl:or divisor ddof 0))
                                (keepdimsp (the boolean (cl:and keep-dim-p t))))
                           ,(format nil "~{~A~%~}

Parameters:
+ ARRAY: input array
+ DIVISOR (DDOF): the divisor to compute the variance is N - ddof (default 0)
+ AXIS (AXES): optional axis or axes to reduce over.
  If unspecified this defaults to reducing over the entire array.
  + if AXIS (AXES) is an index, reduce on the specified AXIS (AXES)
  + if AXIS (AXES) is a sequence of index, reduce on the specified AXIS (AXES)
+ KEEPDIMS: keep reduced axes as singleton dimensions, defaults to `nil'.
"
                                    docstring)
                           (declare (type (cl:or null integer sequence) axis*)
                                    (type integer div))
                           (etypecase axis*
                             (null
                              (with-mlx-op ,(format nil "mlx_~A" mlx-fn)
                                array
                                (keepdimsp :bool)
                                (div       :int)))
                             (integer
                              (with-mlx-op ,(format nil "mlx_~A_axis" mlx-fn)
                                array
                                (axis*     :int)
                                (keepdimsp :bool)
                                (div       :int)))
                             (sequence
                              (with-foreign<-sequence (axes axis* :int len)
                                (with-mlx-op ,(format nil "mlx_~A_axes" mlx-fn)
                                  array
                                  (axes      :pointer)
                                  (len       :size)
                                  (keepdimsp :bool)
                                  (div       :int))))))))))
  (1ops-axis-axes-keepdims-ddof
   (std "std" "Compute the standard deviation(s) over the given axes.")
   (var "var" "Compute the variance(s) over the given axes.")))

(macrolet ((1ops-axis-axes-keepdims (&rest ops)
             `(progn
                ,@(loop :for (name mlx-fn . docstring) :in ops
                        :collect
                        `(defmlx-method ,name (array
                                               &key axis axes
                                                 (keep-dim-p *keep-dim-p*)
                                               &aux
                                                 (axis* (cl:or axes axis 0))
                                                 (keepdimsp (cl:and keep-dim-p t)))
                           "~{~A~%~}
Parameters:
+ ARRAY: input array
+ AXIS (AXES): optional axis or axes to reduce over (default 0)
  If unspecified this defaults to reducing over the entire array.
  + if AXIS (AXES) is an index, reduce on the specified AXIS (AXES)
  + if AXIS (AXES) is a sequence of index, reduce on the specified AXIS (AXES)
+ KEEP-DIM-P: keep reduced axes as singleton dimensions, defaults to `*keep-dim-p*'.
"
                           (etypecase axis*
                             (null
                              (with-mlx-op ,(format nil "mlx_~A" mlx-fn)
                                array
                                (keepdimsp :bool)))
                             (integer
                              (with-mlx-op ,(format nil "mlx_~A_axis" mlx-fn)
                                array
                                (axis :int)
                                (keepdimsp :bool)))
                             (sequence
                              (with-foreign<-sequence (axes axis* :int len)
                                (with-mlx-op ,(format nil "mlx_~A_axes" mlx-fn)
                                  array
                                  (axes :pointer)
                                  (len  :size)
                                  (keepdimsp :bool))))))))))
  (1ops-axis-axes-keepdims
   (all       "all"       "An `and' reduction over the given axes. ")
   (any       "any"       "An `or'  reduction over the given axes. ")

   (logsumexp "logsumexp" "A log-sum-exp reduction over the given axes."
              "The log-sum-exp reduction is a numerically stable version of:

     log(sum(exp(a), axis))
"
              "Return `mlx-array'. ")

   ;; DEV: should keep the API same as Common Lisp `min' and `max'
   ;; or keep the API same as MLX?
   (maximum   "max"       "A `max' reduction over the given axes. ")
   (minimum   "min"       "A `min' reduction over the given axes. ")

   (mean      "mean"      "Compute the mean(s) over the given axes.")

   (sum       "sum"       "Sum reduce the array over the given axes. "
              "Return `mlx-array' with the corresponding axes reduced. ")))

(macrolet ((1ops-axis-axes (&rest ops)
             `(progn
                ,@(loop :for (name mlx-fn . docstring) :in ops
                        :collect
                        `(defmlx-method ,name (array
                                               &key axis axes
                                               &aux (axis* (cl:or axes axis 0)))
                           "~{~A~%~}
Parameters:
+ ARRAY: input array
+ AXIS (AXES): optional axis or axes to reduce over (default 0)
  If unspecified this defaults to reducing over the entire array.
  + if AXIS (AXES) is an index, reduce on the specified AXIS (AXES)
  + if AXIS (AXES) is a sequence of index, reduce on the specified AXIS (AXES)
"
                           (etypecase axis*
                             (null
                              (with-mlx-op ,(format nil "mlx_~A" mlx-fn)
                                array))
                             (integer
                              (with-mlx-op ,(format nil "mlx_~A_axis" mlx-fn)
                                array
                                (axis :int)))
                             (sequence
                              (with-foreign<-sequence (axes axis* :int len)
                                (with-mlx-op ,(format nil "mlx_~A_axes" mlx-fn)
                                  array
                                  (axes :pointer)
                                  (len  :size))))))))))
  (1ops-axis-axes
   (softmax "softmax" "Perform the softmax along the given axis. "
            "Return `mlx-array'. "
            "This operation is a numerically stable version of:

  exp(a) / sum(exp(a), axis, keepdims=True)

")))

(macrolet ((1ops-axis-keepdims (&rest ops)
             `(progn
                ,@(loop :for (name mlx-fn . docstring) :in ops
                        :collect
                        `(defmlx-method ,name (array
                                               &key axis
                                                 (keep-dim-p *keep-dim-p*)
                                               &aux
                                                 (keepdimsp (cl:and keep-dim-p t)))
                           "~{~A~%~}
Parameters:
+ ARRAY: input array
+ AXIS (AXES): optional axis or axes to reduce over.
  If unspecified this defaults to reducing over the entire array.
+ KEEPDIMS: keep reduced axes as singleton dimensions, defaults to `nil'.
"
                           (declare (type (cl:or null integer) axis))
                           (if axis
                               (with-mlx-op ,(format nil "mlx_~A_axis" mlx-fn)
                                 array
                                 (axis :int)
                                 (keepdimsp :bool))
                               (with-mlx-op ,(format nil "mlx_~A" mlx-fn)
                                 array
                                 (keepdimsp :bool))))))))
  (1ops-axis-keepdims
   (argmin  "argmin"  "Indices of the minimum values along the axis.")
   (argmax  "argmax"  "Indices of the maximum values along the axis.")
   ))

;; (macrolet ((1ops-keepdims (&rest ops)
;;              `(progn
;;                 ,@(loop :for (name mlx-fn . docstring) :in ops
;;                         :collect
;;                         `(defmlx-method ,name (array &key (keep-dim-p *keep-dim-p*)
;;                                                &aux (keepdimsp (cl:and keep-dim-p t)))
;;                            "~{~A~%~}
;;
;; Parameters:
;; + ARRAY: input array
;; + KEEP-DIM-P: keep reduced axes as singleton dimensions (default `*keep-dim-p*')
;; "
;;                            (with-mlx-op ,mlx-fn
;;                              array
;;                              (keepdimsp :bool)))))))
;;   (1ops-keepdims
;;    (sum "mlx_sum" )))

(macrolet ((1ops-axis (&rest ops)
             `(progn
                ,@(loop :for (name mlx-fn . docstring) :in ops
                        :collect
                        `(defmlx-method ,name (array &key axis)
                           "~{~A~%~}
Parameters:
+ ARRAY: input array
+ AXIS (AXES): optional axis or axes to reduce over.
  If unspecified this defaults to reducing over the entire array.
"
                           (declare (type (cl:or null integer) axis))
                           (if axis
                               (with-mlx-op ,(format nil "mlx_~A_axis" mlx-fn)
                                 array
                                 (axis :int))
                               (with-mlx-op ,(format nil "mlx_~A" mlx-fn)
                                 array)))))))
  (1ops-axis
   (argsort      "argsort"      "Returns the indices that sort the array.")
   (stack        "stack"        "Stacks the arrays along a new axis.")
   (sort         "sort"         "Returns a sorted copy of the array.")))

(macrolet ((1ops-diagonal (&rest ops)
             `(progn
                ,@(loop :for (op mlx-fn one-doc . docstring) :in ops
                        :collect `(defmlx-method ,op (array &key diag diagonal
                                                      &aux (k (cl:or diagonal diag 0)))
                                    ,(format nil
                                             "~A

Parameters:
+ ARRAY: input array
+ DIAGONAL (DIAG): the diagonal of the 2-D array (defaults 0).

~{~A~%~}"
                                             one-doc
                                             docstring)
                                    (declare (type integer k))
                                    (with-mlx-op ,mlx-fn
                                      array
                                      (k :int)))))))
  (1ops-diagonal
   (tri-lower "mlx_tril" "Zeros the ARRAY above the given diagonal. ")
   (tri-upper "mlx_triu" "Zeros the ARRAY below the given diagonal. ")))

(macrolet ((gen-by-shape (&rest patterns)
             `(progn
                ,@(loop :for pat :in patterns
                        :collect
                        `(defgeneric ,pat (shape &key dtype)
                           (:documentation
                            (format nil "Make an `mlx-array' with SHAPE of ~(~A~).

Parameter:
+ SHAPE: number or sequence of shape of the target array
  if given is a `mlx-array', should be equal to calling:

     (~A (shape mlx-array))

  If SIZE < 0, an `mlx-runtime-error' would raise for negative dimension.

+ DTYPE: target zeros array data type
  if given SHAPE is `mlx-array', it would be ignored
"
                                    pat pat))
                           (:method ((size integer) &key (dtype *default-mlx-dtype*))
                             (,pat (list size) :dtype dtype))
                           (:method ((shape sequence) &key (dtype *default-mlx-dtype*)
                                     &aux (dtype! (ensure-mlx-dtype dtype)))
                             (with-foreign<-sequence (shape* shape :int len)
                               (with-mlx-op ,(format nil "mlx_~(~A~)" pat)
                                 (shape* :pointer)
                                 (len    :size)
                                 (dtype! mlx-dtype))))

                           (:method ((arr mlx-array) &key dtype)
                             (declare (ignore dtype))
                             (with-mlx-op ,(format nil "mlx_~(~A~)_like" pat)
                               arr)))))))
  (gen-by-shape
   ones
   zeros))


;;; Manual bindings

;; TODO: a compiler macro for the `+' and `add'
;; Example:
;; + (+ (* alpha a b) (* beta c))
;; + (add (mul alpha a b) (mul beta c))
;; where ALPHA and BETA are const value
;;
(defmlx-method addmm (a b c &key (alpha 1.0) (beta 1.0))
  "Matrix multiplication with addition and optional scaling.
Return ALPHA * (A @ B) + BETA * C.

Parameters:
+ A, B, C: mlx-array
+ ALPHA and BETA: scaling parameters
"
  (with-mlx-op "mlx_addmm" c a b (alpha :float) (beta :float)))

(defmlx-method all-close (array1 array2
                          &key
                            relative-tolerance rtol
                            absolute-tolerance atol
                            (nan-equal-p *nan-equal-p*)
                          &aux
                            (rtol* (coerce (cl:or relative-tolerance rtol *relative-tolerance*)
                                           'double-float))
                            (atol* (coerce (cl:or absolute-tolerance atol *absolute-tolerance*)
                                           'double-float))
                            (equal-nan-p (the boolean (cl:and nan-equal-p t))))
  "Approximate comparison of two arrays.

Definition:
If ARRAY1 and ARRAY2 are all close to each other,
it should ensure that:

    all(abs(a - b) <= (atol + rtol * abs(b)))

Parameters:
+ ARRAY1, ARRAY2
+ RELATIVE-TOLERANCE (RTOL): relative tolerance (default 1e-5)
+ ABSOLUTE-TOLERANCE (ATOL): absolute tolerance (default 1e-8)
+ EQUAL-NAN: if non-nil, NaN would be considered as equal
"
  (declare (type float rtol* atol*)
           (type boolean equal-nan-p))
  (with-mlx-op "mlx_allclose"
    array1 array2
    (rtol* :double)
    (atol* :double)
    (equal-nan-p :bool)))

(defun %arange (start stop step dtype
                &aux (dtype! (ensure-mlx-dtype dtype)))
  (declare (type real start stop step))
  (let ((coerce (%mlx-dtype-coerce :float64)))
    (with-mlx-op "mlx_arange"
      ((funcall coerce start) :double)
      ((funcall coerce stop)  :double)
      ((funcall coerce step)  :double)
      (dtype!                 mlx-dtype))))

(defmacro arange (&rest range-specification)
  "Generate ranges of numbers.
Return `mlx-array'.

Syntax:

    (arange stop       &key dtype step)
    (arange start stop &key dtype step)
    (arange start stop step &key dtype)
"
  (multiple-value-bind (range keys)
      (split-args-keys range-specification)
    (let ((len (cl:length range)))
      (cond ((cl:= len 1)
             (destructuring-bind
                 (&key (dtype *default-mlx-dtype*) (step  1))
                 keys
               `(%arange 0 ,@range ,step ,dtype)))
            ((cl:= len 2)
             (destructuring-bind
                 (&key (dtype *default-mlx-dtype*) (step  1))
                 keys
               `(%arange ,@range ,step ,dtype)))
            ((cl:= len 3)
             (destructuring-bind
                 (&key (dtype *default-mlx-dtype*))
                 keys
               `(%arange ,@range ,dtype)))))))

(defmlx-method argpartition (array (nth integer) &key (axis -1))
  "Returns the indices that partition the array.

Parameter:
+ ARRAY: input array
+ NTH: element index at the NTH position in the output will give
  the sorted position. All indices before the NTH position will be
  of elements less or equal to the element at the NTH index and all
  indices after will be of elements greater or equal to the element
  at the NTH index.
+ AXIS: optional axis to partition over.
  + If None, this partitions over the flattened array.
  + If unspecified, it defaults to -1.
"
  (declare (type (cl:or null integer) axis))
  (if axis
      (with-mlx-op "mlx_argpartition_axis"
        array
        (nth  :int)
        (axis :int))
      (with-mlx-op "mlx_argpartition"
        array
        (nth  :int))))

(defun list-cumprod (list)
  "Cumprod of LIST. "
  (loop :for elem :in list
        :for prod := elem :then (cl:* elem prod)
        :collect prod))

(defgeneric as-strided (array &key shape strides offset)
  (:documentation
   "Create a view into the array with the given shape and strides.
Return `mlx-array' which is the strided view of the input.

Parameters:
+ ARRAY: input array
+ SHAPE: shape of the resulting array
  + integer or sequence: shape of the target array
  + `nil' or defaults to (shape ARRAY)
+ STRIDES: strides of the resulting array
  + integer or sequence:
  + `nil' or defaults to the reverse exclusive cumulative
    product of (shape ARRAY)
+ OFFSET: skip that many elements from the beginning of
  the input array (default 0)

Note:
Note that this function should be used with caution as it changes
the shape and strides of the array directly. This can lead to the
resulting array pointing to invalid memory locations which can
result into crashes.
")
  (:method ((arr mlx-array) &key shape strides (offset 0)
            &aux
              (shape!   (if shape (sequencefy shape) (shape arr)))
              (strides! (if strides
                            (sequencefy shape)
                            (list-cumprod (cl:reverse (shape arr))))))
    (declare (type (integer 0) offset))
    (with-foreign<-sequence (shape* shape! :int shape-num)
      (with-foreign<-sequence (strides* strides! :int strides-num)
        (with-mlx-op "mlx_as_strided"
          arr
          (shape*      :pointer)
          (shape-num   :int)
          (strides*    :pointer)
          (strides-num :int)
          (offset      :size)))))
  (:method (arr &key shape strides (offset 0))
    (as-strided (mlx-array arr) :shape shape :strides strides :offset offset)))

(macrolet ((2ops-reduce-test (&rest ops)
             `(progn
                ,@(loop :for (op 2op reduce init-value property . docstring) :in ops
                        :collect
                        `(defun ,op (elem &rest more-elem)
                           ,(format nil
                                    "Test if ELEM are ~A to MORE-ELEM.
Return `mlx-array' boolean.

~{~A~^~%~}"
                                    property
                                    docstring)
                           (reduce (lambda (res new) (,reduce res (,2op elem new))) more-elem
                                   :initial-value ,init-value))))))
  (2ops-reduce-test
   (= op2= logical-and +mlx-true+ "equal"
      "See `equal'. ")))

(macrolet ((2ops-windowing (&rest ops)
             `(progn
                ,@(loop :for (op 2op reduce init-value property) :in ops
                        :collect
                        `(defun ,op (elem &rest more-elem)
                           ,(format nil
                                    "Test if ELEM and MORE-ELEM are ~A to each other
Return `mlx-array' boolean. "
                                    property)
                           (loop :with res := ,init-value
                                 :for (a b . rest) :on (cons elem more-elem)
                                 :if (endp rest)
                                   :return res
                                 :do (setf res (,reduce res (,2op a b)))
                                 :finally (return res)))))))
  (2ops-windowing
   (<= op2<= logical-and +mlx-true+ "less or equal")
   (<  op2<  logical-and +mlx-true+ "less")
   (>= op2>= logical-and +mlx-true+ "greater or equal")
   (>  op2>  logical-and +mlx-true+ "greater")))

(macrolet ((2ops-every-pair (&rest ops)
             `(progn
                ,@(loop :for (op 2op reduce init-value property one-doc . more-docs) :in ops
                        :collect
                        `(defun ,op (elem &rest more-elem)
                           ,(format nil "~A
Return `mlx-array' boolean.

Definition:
If ELEM and MORE-ELEM are ~A,
every pair of ELEM and MORE-ELEM should `~A'.

~{~A~%~}"
                                    one-doc
                                    property
                                    2op
                                    more-docs)
                           (loop :for (first . rest) :on (cons elem more-elem)
                                 :for res := ,init-value
                                 :do (loop :for then :in rest
                                           :do (setf rest (,reduce res (,2op first then))))
                                 :finally (return res)))))))
  (2ops-every-pair
   (~= all-close logical-and +mlx-true+ "approximately equal"
       "Approximate comparison of ELEM and MORE-ELEM."
       "See `all-close', `*relative-tolerance*', `*absolute-tolerance*'. ")
   (/= op2/= logical-and +mlx-true+ "not equal"
       "Test if ELEM and MORE-ELEM are not equal to each other. "
       "See `not-equal'. ")))

(defmlx-method op2= (arr1 arr2)
  "Element-wise equality.

Equality comparison on two arrays with numpy-style broadcasting semantics.
Either or both input arrays can also be scalars.

Parameters:
+ "
  (with-mlx-op "mlx_array_equal"
    arr1 arr2
    (*nan-equal-p* :bool)))

(defmethod op2= ((num1 number) (num2 number))
  (if (cl:= num1 num2) +mlx-true+ +mlx-false+))

(defmethod op2= (num (arr mlx-array))
  (op2= (mlx-array num) arr))

(defmethod op2= ((arr mlx-array) num)
  (op2= arr (mlx-array num)))

(defun = (elem &rest more-elem)
  "Ensure ELEM and MORE-ELEM are all equal to each other. "
  (reduce (lambda (res new) (logical-and res (op2= elem new))) more-elem
          :initial-value +mlx-true+))

(defmlx-method op2/= (arr1 arr2)
  "Element-wise not equal.

Not equal comparison on two arrays with numpy-style broadcasting semantics.
Either or both input arrays can also be scalars.

Parameters:
+ ARR1, ARR2: input array or scalar"
  (with-mlx-op "mlx_not_equal"
    arr1 arr2))



(defgeneric split (array split-or-indices &key axis)
  (:documentation
   "Split an array along a given axis.
Return `mlx-array'")
  (:method (arr (num-splits integer) &key (axis 0))
    (split (mlx-array arr) num-splits :axis axis))
  (:method (arr (indices sequence) &key (axis 0))
    (split (mlx-array arr) indices :axis axis))
  (:method ((arr mlx-array) (num-splits integer) &key (axis 0))
    (declare (type integer num-splits axis))
    (with-mlx-op ("mlx_split" :alloc mlx_vector_array_new
                              :wrap  wrap-as-mlx-array-list)
      arr
      (num-splits :int)
      (axis       :int)))
  (:method ((arr mlx-array) (indices sequence) &key (axis 0))
    (declare (type integer axis))
    (assert (every #'integerp indices))
    (with-foreign<-sequence (indices* indices :int len)
      (with-mlx-op ("mlx_split_sections" :alloc mlx_vector_array_new
                                         :wrap  wrap-as-mlx-array-list)
        arr
        (indices* :pointer)
        (len      :int)
        (axis     :int)))))

(defmlx-method swap-axes (array (axis1 integer) (axis2 integer))
  "Swap two axes of an ARRAY.
Return swaped `mlx-array'.

Parameters:
+ ARRAY: input array
+ AXIS1, AXIS2: specific axis index
"
  (with-mlx-op "mlx_swapaxes"
    array
    (axis1 :int)
    (axis2 :int)))

;; (defgeneric take (array indices &key axes axis along-axis-p
;;                   &aux (axis* (cl:or axes axis)))
;;   (:documentation
;;    "Take elements along an axis.
;; Return `mlx-array'.
;;
;; The elements are taken from indices along the specified axis.
;; If the axis is not specified the array is treated as a flattened
;; 1-D array prior to performing the take.
;;
;; Parameters:
;; + ARRAY: input array
;; + INDICES: integer index or input array with integral type
;; + AXIS (AXES):
;; + ALONG-AXIS-P:
;;
;; As an example, if the axis=1 this is equivalent to a[:, indices, ...].")
;;   (:method ()))

(defmlx-method tensordot (array1 array2 &key axes axis
                          &aux (axis* (cl:or axes axis 2)))
  "Compute the tensor dot product along the specified axes.
Return `mlx-array'.

Parameters:
+ ARRAY1, ARRAY2: input array
+ AXIS (AXES): the number of dimensions to sum over (default 2)
  + integer: sum over the last axes dimensions of ARRAY1
    and the first axes dimensions of ARRAY2
  + list: sum over the corresponding dimensions of ARRAY1 and ARRAY2
    this should be like (AXES1 AXES2), where AXES1 and AXES2 are
    sequence
"
  (etypecase axis*
    (integer
     (with-mlx-op "mlx_tensordot_axis"
       array1
       array2
       (axis* :int)))
    (list
     (destructuring-bind (axes1 axes2) (mapcar #'sequencefy axis*)
       (with-foreign<-sequence (axes1* axes1 :int len1)
         (with-foreign<-sequence (axes2* axes2 :int len2)
           (with-mlx-op "mlx_tensordot"
             array1
             array2
             (axes1* :pointer)
             (len1   :int)
             (axes2* :pointer)
             (len2   :int))))))))

(defgeneric tile (array repeat)
  (:documentation
   "Construct an array by repeating ARRAY the number of times given by REPEAT.
Return `mlx-array'.

Parameters:
+ ARRAY: input array
+ REPEAT: the number of times to repeat ARRAY along each axis.
")
  (:method (array (repeat sequence))
    (tile (mlx-array array) repeat))
  (:method (array (repeat integer))
    (tile array (list repeat)))
  (:method ((array mlx-array) (repeat sequence))
    (assert (every #'integerp repeat))
    (with-foreign<-sequence (reps* repeat :int len)
      (with-mlx-op "mlx_tile"
        array
        (reps* :pointer)
        (len   :size)))))

(defmlx-method topk (array (k integer) &key axis)
  "Return K largest elements in ARRAY along given AXIS. "
  (declare (type (cl:or null integer) axis))
  (if axis
      (with-mlx-op "mlx_topk_axis"
        array
        (k    :int)
        (axis :int))
      (with-mlx-op "mlx_topk"
        array
        (k :int))))

(defmlx-method trace (array
                      &key (offset 0) (axis1 0) (axis2 1) (dtype (mlx-dtype array) dtype?)
                      &aux (dtype! (if dtype? (ensure-mlx-dtype dtype) dtype)))
  "Return the sum along a specified diagonal in the given array.

Parameters:
+ ARRAY: input array
+ OFFSET: offset of the diagonal from the main diagonal.
  Can be positive or negative. (default 0)
+ AXIS1, AXIS2: axis of the 2-D sub-arrays from which the diagonals should be taken
+ DTYPE: data type of the output array. (default `mlx-dtype' of ARRAY)
"
  (declare (type integer offset axis1 axis2))
  (with-mlx-op "mlx_trace"
    array
    (offset :int)
    (axis1  :int)
    (axis2  :int)
    (dtype! mlx-dtype)))

;; TODO: arrange

(defmlx-method transpose (array &key axes)
  "Transpose the dimensions of the ARRAY.
Returns the transposed array.

Parameters:
+ ARRAY: Input array
+ AXES: the source axis for each axis in the new array.
  The default is to reverse the axes.
"
  (declare (type (cl:or null sequence) axes))
  (cond ((null axes)
         (with-mlx-op "mlx_transpose"
           array))
        (t
         (assert (every #'integerp axes))
         (with-foreign<-sequence (axes* axes :int len)
           (with-mlx-op "mlx_transpose_axes"
             array
             (axes* :pointer)
             (len   :int))))))

(defgeneric tri (rows &key cols diag diagonal dtype)
  (:documentation
   "Make an array with ones at and below the given diagonal and zeros elsewhere.
Return `mlx-array'.

Parameter:
+ ROWS: number of rows of the triangle array
+ COLS: number of cols of the triangle array (default to ROWS)
+ DIAGONAL (DIAG): diagonal of 2-D array
+ DTYPE: `mlx-dtype' or something can convert into `mlx-dtype'
")
  (:method ((rows integer) &key (cols rows) diag diagonal (dtype *default-mlx-dtype*)
            &aux
              (k (cl:or diagonal diag 0))
              (dtype! (ensure-mlx-dtype dtype)))
    (declare (type integer rows cols))
    (with-mlx-op "mlx_tri"
      (rows   :int)
      (cols   :int)
      (k      :int)
      (dtype! mlx-dtype))))

(defmlx-method unflatten (array (axis integer) (shape sequence))
  "Unflatten an axis of an array to a shape.

Parameter:
+ ARRAY: input array
+ AXIS: the axis to unflatten
+ SHAPE: the shape to unflatten to.
  At most one entry can be -1 in which case the corresponding
  size will be inferred. "
  (assert (every #'integerp shape))
  (with-foreign<-sequence (shape* shape :int len)
    (with-mlx-op "mlx_unflatten"
      array
      (axis :int)
      (shape* :pointer)
      (len    :size))))

;; TODO: where

;;;; ops.lisp ends here
