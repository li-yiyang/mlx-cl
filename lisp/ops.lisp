;;;; ops.lisp --- Operators for MLX

(in-package :mlx)

;;; Dev Note:

;; + operaters should be implemented as generic function,
;;   which allows CLOS method combination and rewritten
;; + all exported operaters should return `mlx-array' as results


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

;; Dev Note:
;; mlx_* bindings should use `with-op-template', `with-mlx-op'
;; and `defmlx-method' to automatically generate mlx operators
;; funcall.

(defmacro with-op-template ((op cffi docs &rest parameters) expr &body body)
  "Generate mlx-cl operator template.
The BODY element should be able to expanded by LAMBDA-LIST,
the expansion rule is defined as EXPR.

Syntax:

    (with-op-template (OP CFFI DOCS
                       (x \"documentation of x\")
                       (y \"documentation of y\")
                       :debug DEBUG)
        EXPR
      &body BODY)

Parameters:
+ OP: variable binded with the name of operator used to expand EXPR
+ CFFI: variable binded with the string of mlx function postfix
  (default to be (string-downcase OP))

  Example: use `mlx::sconc' to concat string
+ DOCS: variable binded with a list of (short &key ...) for
  `mlx::gen-doc' and `mlx::defmlx-method' usage
+ PARAMETERS: elements should be (variable documentation-of-variable)
+ DEBUG: if non-nil, print expressions without evaluating them
+ EXPR: how to expand the op template
+ BODY: elements should be (op . docstring)
"
  (let ((temp    (gensym "TEMP"))
        (op*     (gensym "OP"))
        (short   (gensym "SHORT")))
    (multiple-value-bind (para plist)
        (split-args-keys parameters)
      (let* ((fn `(,temp
                   (,op* &optional ,short &rest ,docs)
                   (destructuring-bind (,op &optional (,cffi (string-downcase ,op)))
                       (listfy ,op*)
                     (declare (ignorable ,op ,cffi))
                     (setf (getf ,docs :parameters)
                           (alist-union ',para (getf ,docs :parameters)))
                     (let ((,docs (cons ,short ,docs)))
                       ,expr)))))
        (if (getf plist :debug)
            `(flet (,fn)
               ,@(loop :for arg :in body :collect `(print (apply #',temp ',arg))))
            `(macrolet (,fn)
               ,@(loop :for arg :in body :collect `(,temp ,@arg))))))))

(defmacro with-mlx-op (name &body arg-type?)
  "Wrap a MLX calling with NAME and ARG-TYPE?.

Syntax:

    (with-mlx-op
      { name | (name &key alloc wrap) }
      { arg | (arg type) }*
      )

Note:
this is equal to calling:

   (with-elem& (res& res :alloc alloc)
     (ensure-success name
       res&
       ...)
     (wrap-as-mlx-array res))
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

(defmacro defmlx-method (name lambda-list &body body)
  "Define a MLX method with name.

Syntax:

    (defmlx-method name lambda-list
      \"short documentation\"     --+
      :return \"balabala\"          +--> use `mlx-cl::gen-doc' to generate documentation
      ... ;; key value pairs      --+
      :methods ((lambda-list . body) ...) -> methods patches
      :aliases (...)                      -> list of function alias
      ,@body)

Parameters:
+ NAME: a symbol as method name
+ LAMBDA-LIST: (arg... &key key... &aux ...)
+ DOCSTRING: a documentation string (MUST given)
+ BODY: method body

Note:
This would define a general method of name, the default method
expects input arguments as `mlx-array' object. By default,
would convert the input arguments via `mlx-array' into `mlx-array'
object and call default method.
"
  (multiple-value-bind (short plist body)
      (split-doc-body body)
    (multiple-value-bind (required optional rest keys others aux)
        (parse-lambda-list lambda-list)
      (let ((call-args `(,@(mapcar (lambda (arg)
                                     (if (listp arg) (car arg) `(mlx-array ,arg)))
                                   required)
                         ,@(mapcar #'atomize optional)))
            (call-keys (loop :for key :in (mapcar #'atomize keys)
                             :collect (intern (symbol-name key) :keyword)
                             :collect key)))
        ;; prepare parameter documents
        (setf (getf plist :parameters)
              (alist-union
               (docollect (arg required)
                 (if (listp arg)
                     (list (first arg) (format nil "`~A'" (second arg)))
                     (list arg "input `mlx-array'")))
               (docollect (arg (append optional keys))
                 (if (listp arg)
                     (list (first arg) (format nil "(default ~A)" (second arg)))
                     (list arg "")))
               (getf plist :parameters)))
        ;; prepare return documents
        (setf (getf plist :return) (cl:or (getf plist :return) "`mlx-array'"))
        `(progn
           (defgeneric ,name (,@(mapcar #'atomize required)
                              ,@(when optional `(&optional ,@(mapcar #'atomize optional)))
                              ,@(when rest     `(&rest     ,rest))
                              ,@(when keys     `(&key      ,@(mapcar #'atomize keys)))
                              ,@(when others   `(&allow-other-keys)))
             (:documentation ,(if (cl:find-if (lambda (key) (getf plist key))
                                              '(:return :note :dev-note :examples
                                                :definition :parameters :syntax
                                                :aliases))
                                  (apply #'gen-doc short plist)
                                  short))
             ,@(loop :for method :in (getf plist :methods) :collect `(:method ,@method))
             (:method (,@(docollect (arg required) (if (listp arg) arg (list arg t)))
                       ,@(when optional `(&optional ,@(mapcar #'len<=2-listfy optional)))
                       ,@(when rest     `(&rest     ,rest))
                       ,@(when keys     `(&key      ,@(mapcar #'len<=2-listfy keys)))
                       ,@(when others   `(&allow-other-keys)))
               ,@(if rest
                     `((declare (ignore ,@(mapcar #'atomize keys)))
                       (apply #',name ,@call-args ,rest))
                     `((,name ,@call-args ,@call-keys))))
             (:method (,@(docollect (arg required) (if (listp arg) arg (list arg 'mlx-array)))
                       ,@(when optional `(&optional ,@optional))
                       ,@(when rest     `(&rest     ,rest))
                       ,@(when keys     `(&key      ,@keys))
                       ,@(when others   `(&allow-other-keys))
                       ,@(when aux `(&aux ,@aux)))
               ,@body))
           ,@(docollect (alias (getf plist :aliases))
               `(defalias ,alias ,name))
           ',name)))))

;; (op ARRAY)
(with-op-template (op cffi docstring
                   (x "input array"))
    `(defmlx-method ,op (x) ,@docstring
       (with-mlx-op ,(sconc "mlx_" cffi) x))
  (abs         "Element-wise abs(X) = | X |. ")
  (erfinv      "Element wise error function erf^-1(X). ")
  (square      "Element wise X^2. ")
  (sqrt        "Element wise √X. ")
  (rsqrt       "Element wise √(1 / x). ")
  (sin         "Element-wise sin(X). ")
  (cos         "Element-wise cos(X). ")
  (tan         "Element-wise tan(X). ")
  (sinh        "Element-wise sinh(X). ")
  (cosh        "Element-wise cosh(X). ")
  (tanh        "Element-wise tanh(X). ")
  (asin        "Element-wise arcsin(X). ")
  (acos        "Element-wise arccos(X). ")
  (atan        "Element-wise arctan(X). ")
  (asinh       "Element-wise arcsinh(X). ")
  (acosh       "Element-wise arccosh(X). ")
  (atanh       "Element-wise arctanh(X). ")
  (expm1       "Element-wise exponential minus 1.")
  (loge        "Element-wise ln(X). ")
  (log10       "Element-wise log10(X). ")
  (log2        "Element-wise log2(X). ")
  (sign        "Element-wise sign(X). ")
  (imagpart    "Element imaginary part of X. ")
  (realpart    "Element real part of X. ")
  (copy        "Copy element. ")
  (degrees     "Convert angles from radians to degrees. ")
  (radians     "Convert angles from degrees to radians. ")

  (finite-p    "Test if element is finite. ")
  (nan-p       "Test if element is NaN. ")
  (inf-p       "Test if element is inf. ")
  (neg-inf-p   "Test if element is negative inf. ")
  (pos-inf-p   "Test if element is positive inf. ")

  ((not "logical_not") "Element-wise logical not. ")

  (negative
   "Element wise -X. "
   :dev-note "`negative' is equal to calling (- X).
See `-' for details. ")
  (reciprocal
   "Element wise 1/X. "
   :dev-note "`reciprocal' is equal to calling (/ X).
See `/' for details. ")

  (erf
   "Element wise error function erf(X). "
   :definition "erf(x) = (2 / x) int(exp(- t^2), {t, 0, x})")
  ((stop-gradient "stop_gradient")
   "Stop gradients of X from being computed. "
   :note "
The operation is the identity but it prevents gradients from flowing
through the array.")
  ((hadamard-transform "hadamard_transform")
   "Perform the Walsh-Hadamard transform along the final axis."
   :definition "
Return (hadamard-matrix(len(X)) @ X) * scale,
where Hadamard Matrix is defined as

          1  / H_{m-1}  H_{m-1}  \
   H_m = --- |                   |
         √2  \ H_{m-1}  -H_{m-1} /

The Hadamard Transformation could be considered as a general Fourier
Transformation.")

  (conjugate
   "Element wise complex conjugate. "
   :definition "conjugate of X is equal to (- (realpart X) (imagpart X)). "
   :note       "See `imagpart' and `realpart'. ")

  (lognot
   "Element-wise bitwise inverse. "
   :aliases (bit-not))
  ((log1+ "log1p") "Element-wise log(A + 1). "
   :dev-note "")
  (sigmoid
   "Element-wise logistic sigmoid(X). "
   :definition "sigmoid(X) = exp(X) / (1 + exp(X))"))

;; (op ARRAY &optional (DTYPE *default-mlx-dtype*))
(with-op-template (op cffi docs
                   (array "input array")
                   (dtype "`mlx-dtype'"))
    `(defmlx-method ,op (array &optional (dtype *default-mlx-dtype* dtype?)
                         &aux (dtype! (if dtype? (ensure-mlx-dtype dtype) dtype)))
       ,@docs
       (with-mlx-op ,(sconc "mlx_" cffi)
         array
         (dtype! mlx-dtype)))
  (as-type
   "Convert ARRAY as a different type. "
   :parameters ((dtype "the `mlx-dtype' to change to"))
   :aliases    (dtype<-))
  (view
   "View the array as a different type. "
   :dev-note "
The `view' does not imply that the input and output arrays
share their underlying data. The view only gaurantees that the binary
representation of each element (or group of elements) is the same."
   :parameters ((dtype "`mlx-dtype' to change to.
The output shape changes along the last axis if the input array's
type and the input dtype do not have the same size."))))

;; (op A B)
(with-op-template (op cffi docs
                   (a "input array")
                   (b "input array"))
    `(defmlx-method ,op (a b) ,@docs (with-mlx-op ,(sconc "mlx_" cffi) a b))
  (add "Element-wise A + B. ")
  ((sub "subtract") "Element-wise A - B. ")
  ((mul "multiply") "Element-wise A * B. "
   :note "See `matmul' or `@' for matrix multiply. ")
  ((div "divide")   "Element-wise A / B. "
   :note "See `inverse' for inverse matrix. ")
  ((mod "divmod")   "Element-wise A % B. "
   :definition "")
  (remainder "Element-wise remainder of A / B. "
             :definition "A = div * B + remainder")
  ((expt "power") "Element-wise A ^ B. ")
  (matmul "Matrix multiplication. ")
  (inner "Inner product of A : B. "
         :definition "")
  (outer "Outter product of A × B. "
         :definition "")
  (kron  "Kronecker product of A ⊗ B"
         :definition "")
  (logaddexp "Element-wise log(exp(A) + exp(B)). ")
  ((atan2 "arctan2") "Element-wise arctan(A / B). "
   :dev-note "")
  ((op2<  "less")          "Element-wise A < B. ")
  ((op2<= "less_equal")    "Element-wise A <= B. ")
  ((op2>  "greater")       "Element-wise A > B. ")
  ((op2>= "greater_equal") "Element-wise A >= B. ")

  ((op2and "logical_and") "Element-wise A ∧ B (A and B). "
   :aliases (logical-and))
  ((op2or  "logical_or")  "Element-wise A ∨ B (A or B). "
   :aliases (logical-or))
  ((logand "bitwise_and") "Element-wise bitwise A & B (A bitand B). "
   :aliases (bit-and))
  ((logxor "bitwise_xor") "Element-wise bitwise A ^ B (A bitxor B). "
   :aliases (bit-xor))
  ((logior "bitwise_ior") "Element-wise bitwise A | B (A bitor B). "
   :aliases (bit-ior bit-or)))

;; (op ELEM &rest MORE-ELEM)
;; => + SINGLE if (endp MORE-ELEM)
;;    + reduce ELEM and MORE-ELEM
(with-op-template (op cffi docs
                   (elem      "input array")
                   (more-elem "can be empty"))
    (destructuring-bind (&key
                           (single        '(copy elem))
                           (initial-value 'elem)
                           (reduce        (intern* cffi))
                         &allow-other-keys)
        (rest docs)
      `(defun ,op (elem &rest more-elem)
         ,(apply #'gen-doc docs)
         (if (endp more-elem) (mlx-array ,single)
             (reduce #',reduce more-elem :initial-value ,initial-value))))
  ((+ add) "Element-wise sum of ELEM and MORE-ELEM. "
   :return "`mlx-array' of sum")
  ((- sub) "Element-wise substract of ELEM and MORE-ELEM."
   :return "`mlx-array' of sum, or `negative' of ELEM if no MORE-ELEM. "
   :single (negative elem))
  ((* mul) "Element-wise multiply of ELEM and MORE-ELEM. "
   :return "`mlx-array' of multiplication")
  ((/ div) "Element-wise ELEM divided by MORE-ELEM. "
   :return "`mlx-array' of division, or `reciprocal' of ELEM if no MORE-ELEM")

  ((and op2and) "Logical `and' all ELEM and MORE-ELEM. "
   :dev-note "This would compute all the arguments (ELEM and MORE-ELEM). ")
  ((or  op2or)  "Logical `or' all ELEM and MORE-ELEM. "
   :dev-note "This would compute all the arguments (ELEM and MORE-ELEM). "))

;; (op ELEM &rest MORE-ELEM)
;; => apply MLX function (CFFI) on every elements
;;    return a list of mlx-array
(with-op-template (op cffi docs
                   (elem "input array")
                   (more-elem "other more input array"))
    `(defun ,op (elem &rest more-elem)
       ,(apply #'gen-doc docs)
       (flet ((f (x) (with-mlx-op ,(sconc "mlx_" cffi) x)))
         (mapcar #'f (cons elem more-elem))))
  ((at-least-1d "at_least_1d")
   "Convert all ELEM and MORE-ELEM to have at least one dimension. ")
  ((at-least-2d "at_least_2d")
   "Convert all ELEM and MORE-ELEM to have at least two dimension. ")
  ((at-least-3d "at_least_3d")
   "Convert all ELEM and MORE-ELEM to have at least three dimension. "))

;; (op ARRAY &key (axis/axes 0) (ddof 0) (keep-dim-p *keep-dim-p*))
(with-op-template (op cffi docs
                   (array      "input array")
                   (axis       "optional axis to reduce over (default 0)")
                   (axes       "alias of axis")
                   (ddof       "delta degrees of freedom (default 0)")
                   (keep-dim-p "keep reduced axes as signelton dimensions (default nil)"))
    `(defmlx-method ,op (array &key axis axes (ddof 0) (keep-dim-p *keep-dim-p*)
                           &aux
                             (axis* (cl:or axes axis 0))
                             (keepdimsp (the boolean (cl:and keep-dim-p t))))
       ,@docs
       (declare (type (or null integer sequence) axis*)
                (type (integer 0) ddof))
       (etypecase axis*
         (null
          (with-mlx-op ,(sconc "mlx_" cffi)
            array
            (keepdimsp :bool)
            (ddof      :int)))
         (integer
          (with-mlx-op ,(sconc "mlx_" cffi "_axis")
            array
            (axis*     :int)
            (keepdimsp :bool)
            (ddof      :int)))
         (sequence
          (with-foreign<-sequence (axes axis* :int len)
            (with-mlx-op ,(sconc "mlx_" cffi "_axes")
              array
              (axes      :pointer)
              (len       :size)
              (keepdimsp :bool)
              (ddof      :int))))))
  (std
   "Compute the standard deviation(s) over the given AXIS(AXES)."
   :definition "standard deviations = sqrt(var(ARRAY))")
  (var
   "Compute the variance(s) over the given AXIS(AXES). "
   :definition "variance = sum((ARRAY - mean(ARRAY))^2) / (dim(ARRAY) - DDOF)"))

;; (op ARRAY &key AXIS/AXES KEEP-DIM-P)
(with-op-template (op cffi docs
                   (array "input array")
                   (axis  "optional axis or axes to reduce over (default 0)
  + `nil': reducing over the entire array
  + integer index: reduce on the specified AXIS
  + sequence of indexs: reduce on the specified AXES")
                   (axes  "alias of axis"))
    `(defmlx-method ,op (array &key axis axes (keep-dim-p *keep-dim-p*)
                         &aux (axis* (cl:or axis axes nil)))
       ,@docs
       (etypecase axis*
         (null
          (with-mlx-op ,(sconc "mlx_" cffi)
            array
            (keep-dim-p :bool)))
         (integer
          (with-mlx-op ,(sconc "mlx_" cffi "_axis")
            array
            (axis :int)
            (keep-dim-p :bool)))
         (sequence
          (with-foreign<-sequence (axes axis* :int len)
            (with-mlx-op ,(sconc "mlx_" cffi "_axes")
              array
              (axes :pointer)
              (len  :size)
              (keep-dim-p :bool))))))
  (all "An `and' reduction over the given axes. ")
  (any "An `or'  reduction over the given axes. ")

  ;; DEV: should keep the API same as Common Lisp `min' and `max'
  ;; or keep the API same as MLX?
  (maximum "A `max' reduction over the given axes. ")
  (minimum "A `min' reduction over the given axes. ")

  (mean "Compute the mean(s) over the given axes.")

  (prod "An product reduction over the given axes.")

  (logsumexp
   "A log-sum-exp reduction over the given axes. "
   :definition "log(sum(exp(ARRAY), axis))")
  (sum
   "Sum reduce the array over the given axes. "
   :return "`mlx-array' with the corresponding axes reduced. "))

;; (op ARRAY &key AXIS/AXES)
(with-op-template (op cffi docs
                   (array "input array")
                   (axes "alias of AXIS"))
    `(defmlx-method ,op (array &key axis axes
                         &aux (axis* (cl:or axes axis
                                            ,(getf (rest docs) :default-axis 0))))
       ,@docs
       (etypecase axis*
         (null
          (with-mlx-op ,(sconc "mlx_" cffi)
            array))
         (integer
          (with-mlx-op ,(sconc "mlx_" cffi "_axis")
            array
            (axis :int)))
         (sequence
          (with-foreign<-sequence (axes axis* :int len)
            (with-mlx-op ,(sconc "mlx_" cffi "_axes")
              array
              (axes :pointer)
              (len  :size))))))
  (softmax
   "Perform the softmax along the given axis. "
   :default-axis 0
   :definition "(exp ARRAY) / (sum (exp ARRAY) :axis AXIS)"
   :parameters ((axis "Optional axis or axes to compute over.
  + `nil':
  + integer index:")))
  (squeeze
   "Remove length one axes from an array. "
   :default-axis nil
   :parameters ((axis "axes to remove (default nil)
  + `nil': all size one axes are removed
  + integer index or sequenc of integer index: axix(axes) to remove"))))


;; (op ARRAY &key AXIS KEEP-DIM-P)
(with-op-template (op cffi docs
                   (axis "")
                   (keep-dim-p "keep reduced axes as singleton dimensions (default `nil')"))
    `(defmlx-method ,op (array &key axis (keep-dim-p *keep-dim-p*))
       ,@docs
       (declare (type (cl:or null integer) axis))
       (if axis
           (with-mlx-op ,(sconc "mlx_" cffi "_axis")
             array
             (axis :int)
             (keep-dim-p :bool))
           (with-mlx-op ,(sconc "mlx_" cffi)
             array
             (keep-dim-p :bool))))
  (argmin "Indices of the minimum values along the axis. ")
  (argmax "Indices of the maximum values along the axis."))

;; (op NAME &key axis)
(with-op-template (op cffi docs)
    `(defmlx-method ,op (array &key (axis ,(getf (rest docs) :default-axis 0)))
       ,@docs
       (declare (type (cl:or null integer) axis))
       (if axis
           (with-mlx-op ,(sconc "mlx_" cffi "_axis")
             array
             (axis :int))
           (with-mlx-op ,(sconc "mlx_" cffi)
             array)))
  (argsort
   "Return the indices that sort the array. "
   :default-axis -1
   :parameters ((axis "optional axis to sort over (default -1 for last axis)
  + `nil' sorts over the flattened array
  + integer index axis to sort over")))
  (sort
   "Stacks the arrays along a new axis."
   :default-axis -1
   :parameters ((axis "")))
  (stack
   "Stacks the arrays along a new axis. "
   :default-axis nil
   :parameters ((axis "Returns a sorted copy of the array. "))))

;; (op (ARRAY dim=2) &key diag/diagonal)
(with-op-template (op cffi docs
                   (diagonal "diagonal of the 2-D array (default 0)")
                   (diag     "alias of DIAGONAL"))
    `(defmlx-method ,op (array &key diag diagonal &aux (k (cl:or diagonal diag 0)))
       ,@docs
       (declare (type integer k))
       (assert (cl:= (dim array) 2))
       (with-mlx-op ,(sconc "mlx_" cffi) array (k :int)))
  (tri-lower
   "Zeros the ARRAY above the given diagonal. "
   :aliases (tril))
  (tri-upper
   "Zeros the ARRAY below the given diagonal. "
   :aliases (triu)))

;; (op array shape &key dtype)
(with-op-template (op cffi docs
                   (shape "shape of output array
if given as `mlx-array', will use shape of `mlx-array'"))
    `(defgeneric ,op (shape &key dtype)
       (:documentation ,(apply #'gen-doc docs))
       (:method ((size integer) &key (dtype *default-mlx-dtype*))
         (,op (list size) :dtype dtype))
       (:method ((shape sequence) &key (dtype *default-mlx-dtype* dtype?)
                 &aux (dtype! (if dtype? (ensure-mlx-dtype dtype) dtype)))
         (with-foreign<-sequence (shape* shape :int len)
           (with-mlx-op ,(sconc "mlx_" cffi)
             (shape* :pointer)
             (len    :size)
             (dtype! mlx-dtype))))
       (:method ((arr mlx-array) &key dtype)
         (declare (ignore dtype))
         (with-mlx-op ,(sconc "mlx_" cffi "_like")
           arr)))
  (ones  "Construct an array of ones. ")
  (zeros "Construct an array of zeros. "))

;; (op ARRAY &key AXIS REVERSE INCLUDSIVE)
(with-op-template (op cffi docs
                   (axis       "optional axis to compute the cumulative result
if unspecified, apply on the flattened array")
                   (reverse    "if cumulative in reverse")
                   (includsive "whether the Nth element of output include the Nth element of input (default t)"))
    `(defmlx-method ,op (array &key axis (reverse nil) (includsive t)
                         &aux
                           (reverse! (cl:and reverse t))
                           (includsive! (cl:and includsive t)))
       ,@docs
       (declare (type (cl:or null integer) axis))
       (let ((array (if axis array (reshape array '(-1))))
             (axis  (cl:or axis 0)))
         (with-mlx-op ,(sconc "mlx_" cffi)
           array
           (axis        :int)
           (reverse!    :bool)
           (includsive! :bool))))
  (cummax       "Return the cumulative maximum of the elements along the given axis.")
  (cummin       "Return the cumulative minimum of the elements along the given axis.")
  (cumprod      "Return the cumulative product of the elements along the given axis.")
  (cumsum       "Return the cumulative sum of the elements along the given axis. ")
  (logcumsumexp "Return the cumulative logsumexp of the elements along the given axis."))

;; (= ELEM &rest MORE-ELEM)
(with-op-template (op cffi docs
                   (elem      "elem for compare")
                   (more-elem "more elem testing used to compare with ELEM"))
    (destructuring-bind (&key combine compare initial-value &allow-other-keys)
        (rest docs)
      `(defun ,op (elem &rest more-elem)
         ,(apply #'gen-doc docs)
         (reduce (lambda (res new)
                   (,combine res (,(intern* compare) elem new)))
                 more-elem
                 :initial-value ,initial-value)))
  (= "Test if ELEM and MORE-ELEM is all equal. "
     :notes         "See `mlx::op2='. "
     :combine       op2and
     :compare       op2=
     :initial-value +mlx-true+))

;; (op &rest ELEMS)
(with-op-template (op cffi docs
                   (elem      "elem to compare")
                   (more-elem "more elem to compare"))
    (destructuring-bind (&key compare
                           (combine 'op2and)
                           (initial-value '+mlx-true+)
                         &allow-other-keys)
        (rest docs)
      `(defun ,op (elem &rest more-elem)
         ,(apply #'gen-doc docs)
         (loop :with res := ,initial-value
               :for (a . rest) :on (cons elem more-elem)
               :if (endp rest)
                 :return res
               :do (setf res (,combine res (,compare a (car rest))))
               :finally (return res))))
  (<= "Test if ELEMS are less or equal in sequence. "
      :compare op2<=)
  (<  "Test if ELEMS are less in sequence. "
      :compare op2<)
  (>= "Test if ELEMS are greater or equal in sequence. "
      :compare op2>=)
  (>  "Test if ELEMS are greater in sequence. "
      :compare op2>))

;; (op ELEM &rest MORE-ELEM)
(with-op-template (op cffi docs
                   (elem "elem to compare")
                   (more-elem "more elem to compare"))
    (destructuring-bind (&key compare
                           (combine       'op2and)
                           (initial-value '+mlx-true+)
                         &allow-other-keys)
        (rest docs)
      `(defun ,op (elem &rest more-elem)
         ,(apply #'gen-doc docs)
         (loop :with res := ,initial-value
               :for (first . rest) :on (cons elem more-elem)
               :do (loop :for then :in rest
                         :do (setf rest (,combine res (,compare first then))))
               :finally (return res))))
  (~= "Approximate comparision of ELEM and MORE-ELEM. "
      :compare op2~=)
  (/= "Test if none ELEM and MORE-ELEM are equal to each other. "
      :compare op2/=))


;;; Manual bindings

;; TODO: a compiler macro for the `+' and `add'
;; Example:
;; + (+ (* alpha a b) (* beta c))
;; + (add (mul alpha a b) (mul beta c))
;; where ALPHA and BETA are const value
;;
(defmlx-method addmm (a b c &key (alpha 1.0) (beta 1.0))
  "Matrix multiplication with addition and optional scaling."
  :return "ALPHA * (A @ B) + BETA * C."
  :parameters ((c     "input constant offset `mlx-array'")
               (alpha "scaling parameter for (A @ B)")
               (beta  "scaling parameter for constant offset C"))
  (with-mlx-op "mlx_addmm" c a b (alpha :float) (beta :float)))

(defmlx-method op2~= (array1 array2
                          &key
                            relative-tolerance rtol
                            absolute-tolerance atol
                            (nan-equal-p *nan-equal-p*)
                          &aux
                            (rtol* (coerce (cl:or relative-tolerance
                                                  rtol
                                                  *relative-tolerance*)
                                           'double-float))
                            (atol* (coerce (cl:or absolute-tolerance
                                                  atol
                                                  *absolute-tolerance*)
                                           'double-float))
                            (equal-nan-p (the boolean (cl:and nan-equal-p t))))
  "Approximate comparison of two arrays."
  :definition "all(abs(a - b) <= (atol + rtol * abs(b)))"
  :parameters ((relative-tolerance "relative tolerance (default `*relative-tolerance*')")
               (absolute-tolerance "absolute toerance  (default `*absolute-tolerance*')")
               (rtol "alias of RELATIVE-TOLERANCE")
               (atol "alias of ABSOLUTE-TOLERANCE")
               (nan-equal-p "if NaN is equal to others (default `*nan-equal-p*')"))
  :aliases    (all-close)
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
  (:method ((shape integer) value &key dtype)
    (full (list shape) value :dtype dtype))
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
