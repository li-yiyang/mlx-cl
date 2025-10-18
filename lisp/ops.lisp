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

   (erf         "mlx_erf"         nil
                "Return the element wise error function. "
                "Definition:

            2   /` x
  erf(x) = ---  |    exp(- t^2) dt
           √π  _/ 0
")
   (erfinv      "mlx_erfinv"      nil
                "Return the Element-wise inverse of `erf'. ")

   (square      "mlx_square"      square)
   (sqrt        "mlx_sqrt"        cl:sqrt)
   (rsqrt       "mlx_rsqrt"       nil
                "rsqrt(x) -> sqrt(1 / x)")

   (stop-gradient "mlx_stop_gradient" nil
                  "Stop gradients from being computed."
                  "The operation is the identity but it prevents gradients from
flowing through the array.")

   ;; TODO: #mlx-cl #missing
   ;; what is scale? and mlx_optional_float?
   (hadamard-transform "mlx_hadamard_transform" nil
                       "Perform the Walsh-Hadamard transform along the final axis."
                       "Definition:

   return (hadamard-matrix(len(ARRAY)) @ ARRAY) * scale

where hadamard-matrix is defined as:

          1  / H_{m-1}  H_{m-1}  \
   H_m = --- |                   |
         √2  \ H_{m-1}  -H_{m-1} /

which could be considered as a general Fourier Transformation.")

   (negative    "mlx_negative"    nil)
   (reciprocal  "mlx_reciprocal"  nil)

   (conj        "mlx_conjugate"   cl:conjugate
                "Return the elementwise complex conjugate of the input.")
   (imagpart    "mlx_imag"        cl:imagpart
                "Returns the imaginary part of a complex array. ")
   (realpart    "mlx_real"        cl:realpart
                "Returns the real part of a complex array.")

   (copy        "mlx_copy"        nil)

   (degrees     "mlx_degrees"     nil
                "Convert angles from radians to degrees.")
   (radians     "mlx_radians"     nil
                "Convert angles from degrees to radians.")

   (logical-not "mlx_logical_not" nil)
   (lognot      "mlx_bitwise_invert" cl:lognot)

   (expm1       "mlx_expm1"       nil
                "Element-wise exponential minus 1."
                "Returns exp(x) - 1 with greater precision for small x. ")

   (loge        "mlx_log"         cl:log)
   (log10       "mlx_log10"       nil)
   (log2        "mlx_log2"        nil)
   (log1+       "mlx_log1p"       nil)

   (sign        "mlx_sign"        nil)
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
   (as-type "astype" "Convert array as a different type. ")
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
   (remainder   "A = div * B + remainder"  "mlx_remainder" nil)
   (expt        "A ^ B"                "mlx_power"         cl:expt
                "Return element-wise power operation.")
   (matmul      "A . B"                "mlx_matmul"        nil)
   (inner       "A B"                  "mlx_inner"         nil
                "Compute the inner product of A and B. ")
   (outer       "A × B"                "mlx_outer"         nil
                "Compute the outer product of two 1-D arrays.
If the array’s passed are not 1-D a flatten op will be run beforehand.")
   (kron        "A ⊗ B"                "mlx_kron"          nil
                "Compute the Kronecker product of A and B. ")

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
              "Return `mlx-array' with the corresponding axes reduced. ")
   (prod      "prod"      "An product reduction over the given axes.")))

(macrolet ((1ops-axis-axes (&rest ops)
             `(progn
                ,@(loop :for (name mlx-fn default-axis . docstring) :in ops
                        :collect
                        `(defmlx-method ,name (array
                                               &key axis axes
                                               &aux (axis* (cl:or axes axis ,default-axis)))
                           ,(format nil "~{~A~%~}
Parameters:
+ ARRAY: input array
+ AXIS (AXES): optional axis or axes to reduce over (default 0)
  If unspecified this defaults to reducing over the entire array.
  + if AXIS (AXES) is an index, reduce on the specified AXIS (AXES)
  + if AXIS (AXES) is a sequence of index, reduce on the specified AXIS (AXES)
"
                                    docstring)
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
   (softmax "softmax" 0
            "Perform the softmax along the given axis. "
            "Return `mlx-array'. "
            "This operation is a numerically stable version of:

  exp(a) / sum(exp(a), axis, keepdims=True)

")
   (squeeze "squeeze" nil
            "Remove length one axes from an array.")))

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
                ,@(loop :for (name mlx-fn default-axis . docstring) :in ops
                        :collect
                        `(defmlx-method ,name (array &key (axis ,default-axis))
                           ,(format nil "~{~A~%~}
Parameters:
+ ARRAY: input array
+ AXIS (AXES): optional axis (default ~A)
"
                                    docstring default-axis)
                           (declare (type (cl:or null integer) axis))
                           (if axis
                               (with-mlx-op ,(format nil "mlx_~A_axis" mlx-fn)
                                 array
                                 (axis :int))
                               (with-mlx-op ,(format nil "mlx_~A" mlx-fn)
                                 array)))))))
  (1ops-axis
   (argsort      "argsort"  -1   "Returns the indices that sort the array.")
   (stack        "stack"    nil  "Stacks the arrays along a new axis.")
   (sort         "sort"     -1   "Returns a sorted copy of the array.")))

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

(macrolet ((cum* (&rest ops)
             `(progn
                ,@(loop :for (op mlx-fn . docstring) :in ops
                        :collect
                        `(defmlx-method ,op (array &key axis (reverse nil) (includsive nil)
                                             &aux
                                               (reverse! (cl:and reverse t))
                                               (includsive! (cl:and includsive t)))
                           ,(format nil "~{~A~%~}

Parameters:
+ ARRAY: input array
+ AXIS:
+ REVERSE:
+ INCLUDSIVE:
"
                                    docstring)
                           (declare (type (cl:or null integer) axis))
                           (let ((array (if axis array (reshape array '(-1))))
                                 (axis  (cl:or axis 0)))
                             (with-mlx-op ,mlx-fn
                               array
                               (axis        :int)
                               (reverse!    :bool)
                               (includsive! :bool))))))))
  (cum*
   (cummax  "mlx_cummax"  "Return the cumulative maximum of the elements along the given axis.")
   (cummin  "mlx_cummin"  "Return the cumulative minimum of the elements along the given axis.")
   (cumprod "mlx_cumprod" "Return the cumulative product of the elements along the given axis.")
   (cumsum  "mlx_cumsum"  "Return the cumulative sum of the elements along the given axis. ")
   (logcumsumexp "mlx_logcumsumexp"
                 "Return the cumulative logsumexp of the elements along the given axis.")))

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

(macrolet ((1op-nth-axis (&rest ops)
             `(progn
                ,@(loop :for (op mlx-fn . docstring) :in ops
                        :collect
                        `(defmlx-method ,op (array (nth integer) &key (axis -1))
                           ,(format nil "~{~A~^~%~}

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
                                    docstring)
                           (declare (type (cl:or null integer) axis))
                           (if axis
                               (with-mlx-op ,(format nil "mlx_~A_axis" mlx-fn)
                                 array
                                 (nth  :int)
                                 (axis :int))
                               (with-mlx-op ,(format nil "mlx_~A" mlx-fn)
                                 array
                                 (nth  :int))))))))
  (1op-nth-axis
   (argpartition "argpartition"
                 "Returns the indices that partition the array.")
   (partition    "partition"
                 "Returns a partitioned copy of ARRAY such that smaller NTH elements are first.")))

(flet ((list-cumprod (list)
         "Cumprod of LIST. "
         (loop :for elem :in list
               :for prod := elem :then (cl:* elem prod)
               :collect prod)))
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
      (as-strided (mlx-array arr) :shape shape :strides strides :offset offset))))

(defmlx-method atleast (array &key (dim 1))
  "Convert all ARRAY to have at least NDIM dimension. "
  (declare (type (member 1 2 3) dim))
  (ecase dim
    (1 (at-least-1d array))
    (2 (at-least-2d array))
    (3 (at-least-3d array))))

;; TODO: boradcast

(defgeneric floor (array &optional divisor)
  (:documentation
   "Return ⌊ARRAY / DIVISOR⌋.

Parameters:
+ ARRAY: input array
+ DIVISOR: by default, divisor is 1
")
  (:method ((num number) &optional divisor)
    (mlx-array (cl:floor num divisor)))
  (:method (array &optional divisor)
    (floor (mlx-array array) divisor))
  (:method ((arr mlx-array) &optional divisor)
    (if divisor
        (with-mlx-op "mlx_floor_divide"
          arr
          ((mlx-array divisor) mlx-array))
        (with-mlx-op "mlx_floor" arr))))

(defgeneric ceiling (array &optional divisor)
  (:documentation
   "Return ⌈ARRAY / DIVISOR⌉.

Parameters:
+ ARRAY: input array
+ DIVISOR: by default, divisor is 1
")
  (:method ((num number) &optional divisor)
    (mlx-array (cl:ceiling num divisor)))
  (:method (array &optional divisor)
    (ceiling (mlx-array array) divisor))
  (:method ((arr mlx-array) &optional divisor)
    (if divisor
        (ceiling (div arr divisor))
        (with-mlx-op "mlx_ceil" arr))))

(defgeneric clip (array &key min max)
  (:documentation
   "Clip the values of the ARRAY between the given MIN and MAX.

Parameters:
+ ARRAY: input array
+ MIN, MAX: range to clip,
  if not given, the corresponding edge would be ignored;
  if both are not given, return array itself.
")
  (:method (array &key min max)
    (clip (mlx-array array) :min min :max max))
  (:method ((arr mlx-array) &key min max)
    (flet ((ptr (obj) (if obj (mlx-object-pointer obj) (null-pointer))))
      (with-mlx-op "mlx_clip"
        arr
        ((ptr min) :pointer)
        ((ptr max) :pointer)))))

(defun concat (mlx-array-sequence &key axis)
  "Concatenate a sequence of `mlx-array' along the given AXIS.

Parameter:
+ MLX-ARRAY-SEQUENCE: a sequence of `mlx-array' objects,
  if element of MLX-ARRAY-SEQUENCE is not `mlx-array',
  will use `mlx-array' to convert them into `mlx-array'
  behind the scene;
+ AXIS: axis to concatenate along (default 0)"
  (declare (type sequence mlx-array-sequence)
           (type (cl:or null integer) axis))
  (with-array-vector<-sequence (arrays mlx-array-sequence)
    (if (cl:and (integerp axis)
                (cl:not (zerop axis)))
        (with-mlx-op "mlx_concatenate_axis"
          (arrays :pointer)
          (axis   :int))
        (with-mlx-op "mlx_concatenate"
          (arrays :pointer)))))

(defmlx-method contiguous (array &key (major :row))
  "Force an ARRAY to be MAJOR contiguous. Copy if necessary.
Return the row or col contiguous output.

Parameters:
+ ARRAY: input array
+ MAJOR: `:row' or `:col' for row major or col major
"
  (declare (type (member :row :col :column) major))
  (with-mlx-op "mlx_contiguous"
    array
    ((if (cl:eq major :row) nil t) :bool)))

;; (defun conv1d (input weight &key (stride 1) (padding 0) (dialation 1) (groups 1))
;;   "")

;; (defun conv2d (input weight &key (stride 1) (padding 0) (dialation 1) (groups 1))
;;   "")

;; (defun conv3d (input weight &key (stride 1) (padding 0) (dialation 1) (groups 1))
;;   "")

;; (defmlx-method conv (input weight &key (stride 1) (padding 0) (dialation 1) (groups 1))
;;   "")

;; (defgeneric depends (inputs dependencies))

;; TODO: #mlx-cl #missing
;; what is quantize mode?
;;
;; (defmlx-method dequantize (array ...))
;; (defmlx-method quantize   (array ...))

(defgeneric diag (array &optional dim)
  (:documentation
   "Extract a diagonal or construct a diagonal matrix.
Return the extracted diagonal or the constructed diagonal matrix.

Parameters:
+ ARRAY: input array

  If ARRAY is
  + 1-D then a diagonal matrix is constructed with ARRAY
    on the Nth diagonal.
  + 2-D then the Nth diagonal is returned.
+ N: diagonal to extract or construct (default 0)
")
  (:method (arr &optional (dim 0))
    (declare (type integer dim))
    (diag (mlx-array arr) dim))
  (:method ((array mlx-array) &optional (dim 0))
    (declare (type integer dim))
    (with-mlx-op "mlx_diag"
      array
      (dim :int))))

(defgeneric einsum (subscripts &rest operands)
  (:documentation
   "Perform the Einstein summation convention on the operands.

Parameters:
+ SUBSCRIPTS: the Einstein summation convention equation
+ OPERANDS: input arrays

  if given with zero operands, return a function of lambda list

     (lambda (&rest operands) ...)

  if given none-zero operands, calculate einsum on the operands
")
  (:method ((subscripts string) &rest operands)
    (if (endp operands)
        (lambda (&rest operands) (einsum subscripts operands))
        (with-foreign-string (str subscripts)
          (with-array-vector<-sequence (vec operands)
            (with-mlx-op "mlx_einsum"
              (str :string)
              (vec :pointer)))))))

(defgeneric expand-dims (array axis &rest axes)
  (:documentation
   "Add a size one dimension at the given axis.
Return `mlx-array' with inserted dimensions.

Parameters:
+ ARRAY: input array
+ AXIS, AXES: index of the inserted dimensions. ")
  (:method (num (axis integer) &rest axes)
    (apply #'expand-dims (mlx-array num) axis axes))
  (:method ((arr mlx-array) (axis integer) &rest axes)
    (if (endp axes)
        (with-mlx-op "mlx_expand_dims"
          arr
          (axis :int))
        (with-foreign<-sequence (axes* (cons axis axes) :int len)
          (with-mlx-op "mlx_expand_dims_axes"
            arr
            (axes* :pointer)
            :size)))))

(defgeneric eye (shape &key diag dtype)
  (:documentation
   "Create an identity matrix or a general diagonal matrix.
Return `mlx-array' where all elements are equal to zero,
except for the DIAG diagonal, whose values are equal to one.

Parameters:
+ SHAPE: shape of the identity matrix
+ DIAG: diagonal index to be one (default 0)
+ DTYPE: new matrix mlx-dtype (default `*default-mlx-dtype*')
")
  (:method ((n integer) &key (diag 0) (dtype *default-mlx-dtype*))
    (eye (list n n) :diag diag :dtype dtype))
  (:method ((shape list) &key (diag 0) (dtype *default-mlx-dtype*))
    (destructuring-bind (n &optional (m n)) shape
      (with-mlx-op "mlx_eye"
        (n :int)
        (m :int)
        (diag :int)
        (dtype mlx-dtype)))))

(defmlx-method flatten (array &key (start 0) (end -1))
  "Flatten ARRAY between START axis and END axis.
Return the flattend `mlx-array'.

Parameters:
+ ARRAY: input array
+ START: the first dimension to flatten (default 0)
+ END: the last dimension to flatten (default -1)
"
  (declare (type integer start end))
  (with-mlx-op "mlx_flatten"
    array
    (start :int)
    (end   :int)))

(defgeneric full (shape value &key dtype)
  (:documentation
   "Construct an array of SHAPE filling with with the given VALUE.
Return a `mlx-array' of SHAPE filled with VALUE.

Parameters
+ SHAPE: shape of the array
+ VALUE: value to fill the array
+ DTYPE: data type of the output array.
  if unspecified the output type is inferred from vals
  or `*default-mlx-dtype*'

Examples:
+ (full shape 1) is equal to (ones  shape)
+ (full shape 0) is equal to (zeros shape)
")
  (:method ((shape sequence) value &key dtype)
    (full shape (mlx-array value) :dtype (cl:or dtype (mlx-dtype value))))
  (:method ((shape sequence) (value mlx-array) &key (dtype (mlx-dtype value) dtype?)
            &aux (dtype! (if (cl:and dtype? dtype)
                             dtype
                             (ensure-mlx-dtype dtype))))
    (with-foreign<-sequence (shape* shape :int len)
      (with-mlx-op "mlx_full"
        (shape* :pointer)
        (len    :int)
        value
        (dtype! mlx-dtype)))))

;; TODO: #mlx-cl #missing
;; what is gather?
;; (defgeneric gather (array indices axes slice))
;; (defgeneric gather-mm (array1 array2 lhs-indices rhs-indices sorted-indices))
;; (defgeneric gather-qmm (array weight scales biases lhs-indices rhs-indices transpost group-size bits sorted-indices))

(defgeneric identity (shape &key dtype)
  (:documentation
   "Create a square identity matrix.

Parameters:
+ SHAPE: shape of the identity matrix
+ DTYPE: `mlx-dtype' of new matrix (default `*default-mlx-dtype*')
")
  (:method ((shape integer) &key (dtype *default-mlx-dtype* dtype?)
            &aux (dtype! (if dtype? (ensure-mlx-dtype dtype) dtype)))
    (with-mlx-op "mlx_identity"
      (shape  :int)
      (dtype! mlx-dtype))))

(defgeneric linspace (start stop &optional num &key dtype)
  (:documentation
   "Generate num evenly spaced numbers over interval [start, stop].

Parameters:
+ START: starting value
+ STOP: stopping value
+ NUM: number of samples (default 50)
+ DTYPE: `mlx-dtype' of the new array (default `*default-mlx-dtype*')
")
  (:method ((start number) (stop number) &optional (num 50)
            &key (dtype *default-mlx-dtype* dtype?)
            &aux (dtype! (if dtype? (ensure-mlx-dtype dtype) dtype)))
    (declare (type (integer 0) num))
    (let ((coerce (%mlx-dtype-coerce :float64)))
      (with-mlx-op "mlx_linspace"
        ((funcall coerce start) :double)
        ((funcall coerce stop)  :double)
        (num                    :int)
        (dtype!                 mlx-dtype)))))

;; TODO: #mlx-cl #optimization
;; store INDEXING cstring as constants for fater calling
(defgeneric meshgrid (arrays &key spares indexing)
  (:documentation
   "Generate multidimensional coordinate grids from 1-D coordinate arrays.

Parameters:
+ ARRAYS: a sequence of `mlx-array'
+ SPARES: return dense or spare array (default `nil')
  + `t': a sparse grid is returned in which each output array
    has a single non-zero element
  + `nil': a dense grid is returned
+ INDEXING: how to index the output array
  + `:xy': Cartesian
  + `:ij': Matrix
")
  (:method ((arrays sequence) &key spares (indexing :xy))
    (declare (type (member :xy :ij) indexing))
    (with-foreign-string (idx (ecase indexing
                                (:xy "xy")
                                (:ij "ij")))
      (with-array-vector<-sequence (vec arrays)
        (with-mlx-op "mlx_meshgrid"
          (vec :pointer)
          ((cl:and spares t) :bool)
          (idx :string))))))

(defmlx-method moveaxis (array (from integer) (to integer))
  "Move an axis to a new position.

Parameter:
+ ARRAY: input array
+ FROM: source axis
+ TO: destination axis
"
  (with-mlx-op "mlx_moveaxis"
    array
    (from :int)
    (to   :int)))

(defmlx-method num<-nan (array &key (nan 0) posinf neginf)
  "Replace NaN and Inf values with finite numbers. "
  (declare (type number nan)
           (type (cl:or number null) posinf neginf))
  (let ((coerce (%mlx-dtype-coerce :float32)))
    (with-mlx-op "mlx_nan_to_num"
      array
      ((funcall coerce nan) :float)
      ;; TODO #mlx-cl #patch
      ;; use mlx-optional-float to fix this
      ((if posinf (funcall coerce posinf) 0.0f0) :float)
      ((cl:and posinf t)                         :bool)
      ((if neginf (funcall coerce neginf) 0.0f0) :float)
      ((cl:and neginf t)                         :bool))))

;; TODO: #mlx-cl #missing
;; (defgeneric pad (array axes ))

(defmlx-method put-along-axis (array indices values &key axis)
  "Put values along an axis at the specified indices.

Parameters:
+ ARRAY: destination array
+ INDICES: indices array.
  should be broadcastable with the input array excluding the axis dimension
+ VALUES: values array
  should be broadcastable with the indices
+ AXIS: axis in the destination to put the values to
  + `integer': the axis to put the values to
  + `nil': the ARRAY is flattened prior to the put operation.
"
  (declare (type (cl:or integer null) axis))
  (if axis
      (with-mlx-op "mlx_put_along_axis"
        array
        indices
        values
        (axis :int))
      (let ((shape (shape array)))
        (reshape (put-along-axis (reshape array '(-1)) indices values :axis 0) shape))))

;; TODO: #mlx-cl #missing
;; should have `mode' key, but the mlx-c seems to not support it
(defgeneric quantize (weight &key group-size bits)
  (:documentation
   "Quantize the matrix w using bits bits per element.
Return values of quantized version of WEIGHT, quantization scales, and bias.

Parameters:
+ WEIGHT: matrix to be quantized
+ GROUP-SIZE: size of the group in w that shares a scale and bias (default 64)
+ BITS: number of bits occupied by each element of w in the
  returned quantized matrix (default 4)

Note:
Quantization is a technique to reduce the computational and memory costs of
running inference by representing the weights and activations with
low-precision data types like 8-bit integer (int8) instead of the usual
32-bit floating point (float32).
")
  (:method (matrix &key (group-size 64) (bits 4))
    (quantize (mlx-array matrix) :group-size group-size :bits bits))
  (:method ((w mlx-array) &key (group-size 64) (bits 4))
    (declare (type (integer 0) group-size bits))
    (with-elem& (res0& res0 :type :pointer :alloc (mlx_array_new))
      (with-elem& (res1& res1 :type :pointer :alloc (mlx_array_new))
        (with-elem& (res2& res2 :type :pointer :alloc (mlx_array_new))
          (ensure-success "mlx_quantize"
            :pointer res0&
            :pointer res1&
            :pointer res2&
            :pointer (mlx-object-pointer w)
            :int     group-size
            :int     bits)
          (values (wrap-as-mlx-array res0&)
                  (wrap-as-mlx-array res1&)
                  (wrap-as-mlx-array res2&)))))))

;; TODO: #mlx-cl #optimization
;; constant 0 should be preallocated as `+mlx-zero+'
;; (preallocated `mlx-array' of `0')
(defmlx-method quantized-matmul (array weight scales
                                 &key bias (transpose t) (group-size 64) bits
                                 &aux (bias! (cl:or bias 0))
                                   (transpose! (cl:and transpose t)))
  "Perform the matrix multiplication with the quantized matrix WEIGHT.
Return a manipulation of `mlx-array'.

The quantization uses one floating point SCALE and BIAS per GROUP-SIZE of elements.
Each element in WEIGHT takes BITS and is packed in an unsigned 32 bit integer.

Parameters:
+ ARRAY: input array
+ WEIGHT: quantized matrix packed in unsigned integers
+ SCALES: scales to use per GROUP-SIZE elements of WEIGHT
+ BIAS: biases to use per GROUP-SIZE elements of WEIGHT (default `nil').
+ TRANSPOSE: whether to multiply with the transposed WEIGHT or not
  namely whether we are performing x @ w.T or x @ w (default `t').
+ GROUP-SIZE: size of the group in w that shares a scale and bias (default 64).
+ BITS: number of bits occupied by each element in w (default 4)
"
  (declare (type (cl:or mlx-array number) bias!)
           (type (integer 0) group-size bits))
  (with-mlx-op "mlx_quantized_matmul"
    array
    weight
    scales
    bias!
    (transpose! :bool)
    (group-size :int)
    (bits       :int)))

(defmlx-method repeat (array (repeats integer) &key axis)
  "Repeat an array along a specified axis.
Return `mlx-array'.

Parameters:
+ ARRAY: input array
+ REPEATS: repetitions for each element
+ AXIS: axis in which to repeat the array along.
  if unspecified it uses the flattened array of the input
  and repeats along axis 0
"
  (declare (type (cl:or integer null) axis))
  (if axis
      (with-mlx-op "mlx_repeat_axis"
        array
        (repeats :int)
        (axis    :int))
      (with-mlx-op "mlx_repeat"
        array
        (repeats :int))))

(defgeneric ash (array count)
  (:documentation
   "Element-wise left shift.
Return `mlx-array' of ARRAY << count.

Parameters:
+ ARRAY: input array
+ COUNT: shifting integer,
  if positive, shift left;
  if negative, shift right
")
  (:method (array (count integer))
    (ash (mlx-array array) count))
  (:method (array (count mlx-array))
    (ash (mlx-array array) count))
  (:method ((array mlx-array) (count mlx-array))
    (with-mlx-op "mlx_left_shift" array count))
  (:method ((array mlx-array) (count integer))
    (cond ((cl:> count 0)
           (with-mlx-op "mlx_left_shift"
             array count))
          ((cl:< count 0)
           (with-mlx-op "mlx_right_shift"
             array count))
          (t array))))

(defgeneric roll (array shift &key axis axes)
  (:documentation
   "Roll array elements along a given axis.

Elements that are rolled beyond the end of the array are introduced
at the beggining and vice-versa.

If the axis is not provided the array is flattened, rolled and then
the shape is restored.

Parameters:
+ ARRAY: input array
+ SHIFT: number of places by which elements are shifted.
  + If positive the array is rolled to the right,
  + if negative it is rolled to the left.
  + If an int is provided but given multiply AXES,
    then the same value is used for all axes
+ AXIS (AXES): axis or axes along which to roll the elements
")
  (:method (array (shift integer) &key axis axes)
    (roll (mlx-array array) (list shift) :axis axis :axes axes))
  (:method (array (shift sequence) &key axis axes)
    (roll (mlx-array array) shift :axis axis :axes axes))
  (:method ((array mlx-array) (shift integer) &key axis axes)
    (roll array (list shift) :axis axis :axes axes))
  (:method ((array mlx-array) (shift sequence)
            &key axis axes
            &aux (axis! (cl:or axis axes nil)))
  (declare (type (cl:or null integer sequence) axis!))
  (with-foreign<-sequence (shift* shift :int len)
    (etypecase axis!
      (null (with-mlx-op "mlx_roll"
              array
              (shift* :pointer)
              (len    :int)))
      (integer (with-mlx-op "mlx_roll_axis"
                 array
                 (shift* :pointer)
                 (len    :int)
                 (axis!  :int)))
      (sequence (with-foreign<-sequence (axes* axis! :int axes-len)
                  (with-mlx-op "mlx_roll_axes"
                    array
                    (shift*   :pointer)
                    (len      :int)
                    (axes*    :pointer)
                    (axes-len :int))))))))

(defgeneric round (array &optional decimals)
  (:documentation
   "Round to the given number of decimals.

Definition:

    (let ((scalar (expt 10 DECIMALS)))
      (/ (round (* ARRAY scalar)) scalar))

Parameters:
+ ARRAY: input array
+ DECIMALS: number of decimal places to round to (default 0)
")
  (:method (array &optional (decimals 0))
    (round (mlx-array array) decimals))
  (:method ((array mlx-array) &optional (decimals 0))
    (declare (type integer decimals))
    (with-mlx-op "mlx_round"
      array
      (decimals :int))))

(defgeneric scatter (array indices updates axes)
  (:documentation
   "Scatter updates to the given indices.

The parameters indices and axes determine the locations
of a that are updated with the values in updates.
Assuming 1-d indices for simplicity, indices[i] are the indices
on axis axes[i] to which the values in updates will be applied.
Note each array in indices is assigned to a corresponding axis
and hence indices.size() == axes.size(). If an index/axis pair
is not provided then indices along that axis are assumed to be
zero.

Note the rank of updates must be equal to the sum of the rank
of the broadcasted indices and the rank of a. In other words,
assuming the arrays in indices have the same shape,
updates.ndim() == indices[0].ndim() + a.ndim(). The leading
dimensions of updates correspond to the indices, and the remaining
a.ndim() dimensions are the values that will be applied to the
given location in a.

For example:

    (scatter (zeros '(4 4))
             (list (mlx-array '(2)))
             (reshape (arange 1 3 :dtype :float32) '(1 1 2))
             0)
    ; => array([[0, 0, 0, 0],
                [0, 0, 0, 0],
                [1, 2, 0, 0],
                [0, 0, 0, 0]], dtype=float32)
")
  (:method ((array mlx-array) (indices sequence) (updates mlx-array) (axes integer))
    (scatter array indices updates (list axes)))
  (:method ((array mlx-array) (indices sequence) (updates mlx-array) (axes sequence))
    (with-array-vector<-sequence (vec indices)
      (with-foreign<-sequence (axes* axes :int len)
        (with-mlx-op "mlx_scatter"
          array
          (vec     :pointer)
          (updates mlx-array)
          (axes*   :pointer)
          (len     :int))))))

;; TODO: #mlx-cl
;; scatter_*?
;; segmented_mm
;; not sure what to do with them, how to use them

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

;; TODO: #mlx-cl #dev #syntax
;; shape input check, normalize
(defmlx-method reshape (array (shape sequence))
  "Reshape an array while preserving the size.
Return reshaped `mlx-array'.

Parameters:
+ ARRAY: input array
+ SHAPE: shape specification"
  (with-foreign<-sequence (shape* shape :int len)
    (with-mlx-op "mlx_reshape"
      array
      (shape* :pointer)
      (len    :size))))

;; TODO: #mlx-cl #user-friendly
;; if not given SIZE, automatically determine it,
;; rather than raising error
(defgeneric slice (array start axes &optional size)
  (:documentation
   "Get/Set a sub-array from the input array.

Parameter:
+ ARRAY: input array
+ START: index location to start the slice at
+ AXES: axes corresponding to the indices in start_indices.
+ SIZE: size of slice
")
  (:method ((array mlx-array) (start mlx-array) (axes integer) &optional size)
    (declare (type (cl:or integer sequence) size))
    (slice array start (list axes) size))
  (:method (array start (axes integer) &optional size)
    (declare (type (cl:or integer sequence) size))
    (slice (mlx-array array) (mlx-array start) (list axes) size))
  (:method (array start (axes sequence) &optional size)
    (declare (type (cl:or integer sequence) size))
    (slice (mlx-array array) (mlx-array start) axes size))
  (:method ((array mlx-array) (start mlx-array) (axes sequence) &optional size)
    (declare (type (cl:or integer sequence) size))
    (with-foreign<-sequence (axes* axes :int axes-num)
      (with-foreign<-sequence (size* (sequencefy size) :int size-num)
        (with-mlx-op "mlx_slice"
          array
          start
          (axes*    :pointer)
          (axes-num :int)
          (size*    :pointer)
          (size-num :int))))))

(defgeneric (setf slice) (update array start axes &optional size)
  (:documentation
   "Update a sub-array of the input array. ")
  (:method ((update mlx-array) (array mlx-array) (start mlx-array) (axes sequence) &optional size)
    (declare (ignorable size))
    (with-foreign<-sequence (axes* axes :int len)
      (with-mlx-op "mlx_slice_update_dynamic"
        array
        update
        start
        (axes* :pointer)
        (len   :size)))))

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

(defgeneric take (array indices &key axis along-axis-p)
  (:documentation
   "Take elements along an axis.
Return `mlx-array'.

The elements are taken from indices along the specified axis.
If the axis is not specified the array is treated as a flattened
1-D array prior to performing the take.

Parameters:
+ ARRAY: input array
+ INDICES: integer index or input array with integral type
+ AXIS (AXES):
+ ALONG-AXIS-P:

As an example, if the axis=1 this is equivalent to a[:, indices, ...].")
  (:method ((arr mlx-array) indices &key axis along-axis-p)
    (take arr (mlx-array indices) :axis axis :along-axis-p along-axis-p))
  (:method ((arr mlx-array) (indices mlx-array) &key axis along-axis-p)
    (if axis
        (if along-axis-p
            (with-mlx-op "mlx_take_along_axis"
              arr
              indices
              (axis :int))
            (with-mlx-op "mlx_take_axis"
              arr
              indices
              (axis :int)))
        (with-mlx-op "mlx_take" arr indices))))

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
