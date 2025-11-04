;;;; ops.lisp --- Operators for MLX -*- mlx-cl-test-file: "api.lisp" -*-

(in-package :mlx)

;;; Dev Note:

;; + operaters should be implemented as generic function,
;;   which allows CLOS method combination and rewritten
;;   (use `defmlx-method', `with-op-template' for convience)
;;   (or modifiy them to support more functionalities)
;; + all exported operaters should return `mlx-array' as results
;; + see `lisp/convert.lisp', mlx ops methods should use
;;   `bool<-', `seq<-', `num<-', `axis<-', `axes<-' functions
;;   to normalize input parameters
;; + the code should be organized in MLX-CL.TEST (mlx-api)
;;   by the test sequence
;; + for Emacs: `mlx-cl-highlight-test-tags' (see dev/mlx-cl-tags.el)


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


;;;; Create MLX array

;;; convert Lisp values into mlx-array

(defgeneric mlx-array (val &key dtype shape &allow-other-keys)
  (:documentation
   "Convert VAL as MLX-ARRAY of DTYPE.

Parameters:
+ VAL:   lisp value
+ DTYPE: MLX-ARRAY data type or lisp type specifier,

  + if not given, would try to determine from VAL via
    `mlx::ensure-mlx-dtype';
  + if given DTYPE is different to VAL type, will try
    to convert VAL into specific DTYPE
")
  ;; TEST: #mlx-array-bool
  (:method (bool
            &key (dtype :bool)
            &aux (dtype! (ensure-mlx-dtype dtype)))
    (declare (type boolean bool)
             (type (member :bool) dtype!)
             (ignorable dtype!))
    (if bool +mlx-true+ +mlx-false+))
  ;; TEST: #mlx-array-scalar
  (:method ((number number)
            &key (dtype (type-of number))
            &aux (dtype! (ensure-mlx-dtype dtype)))
    (wrap-as-mlx-array
     (ecase dtype!
       (:bool
        (mlx_array_new_bool (cl:not (zerop number))))
       ((:uint8 :uint16 :uint32 :uint64
         :int8  :int16  :int32  :int64)
        (mlx_array_new_int (truncate number)))
       (:float32
        (mlx_array_new_float32 (coerce number 'single-float)))
       (:float64
        (mlx_array_new_float64 (coerce number 'double-float)))
       (:complex64
        (mlx_array_new_complex (coerce (cl:realpart number) 'single-float)
                               (coerce (cl:imagpart number) 'single-float))))))
  ;; TEST: #mlx-array-array
  (:method ((array array)
            &key (dtype  (mlx-dtype array) dtype?)
            &aux
              (dtype! (if dtype? (ensure-mlx-dtype dtype) dtype))
              (shape  (shape<- (array-dimensions array))))
    (let ((size  (reduce #'cl:* shape))
          (dtype (if (not (eql (array-element-type array) t))
                     (mlx-dtype<-lisp-type (array-element-type array)))))
      (declare (type integer size))
      (cond ((eql dtype! :complex64)
             (let ((coerce (%mlx-dtype-coerce dtype!)))
               (wrap-as-mlx-array
                (with-foreign-pointer (array* (cl:* 2 (foreign-type-size :float) size))
                  (loop :for i :below size
                        :for idx :from 0 :by 2
                        :for c := (funcall coerce (row-major-aref array i))
                        :do (setf (mem-aref array* :float idx)      (cl:realpart c)
                                  (mem-aref array* :float (1+ idx)) (cl:imagpart c)))
                  (with-pointer-to-vector-data (shape* shape)
                    (mlx_array_new_data array* shape* (length shape) dtype!))))))
            ((and dtype (eq dtype dtype!))
             (let ((array (make-array size
                                      :initial-contents array
                                      :element-type     (array-element-type array))))
               (declare (type simple-array array))
               (wrap-as-mlx-array
                (with-pointer-to-vector-data (array* array)
                  (with-pointer-to-vector-data (shape* shape)
                    (mlx_array_new_data array* shape* (length shape) dtype!))))))
            (t
              (let ((coerce (%mlx-dtype-coerce dtype!)))
                (wrap-as-mlx-array
                 (with-foreign<-array (array* array (cffi-type<-mlx-dtype dtype!)
                                       %shape %size
                                       elem (funcall coerce elem))
                   (with-pointer-to-vector-data (shape* shape)
                     (mlx_array_new_data array* shape* (length shape) dtype!)))))))))
  ;; TEST: #mlx-array-list
  (:method ((list list)
            &key (dtype  (mlx-dtype list))
            &aux (dtype! (ensure-mlx-dtype dtype)))
    "For `nil', wrap as boolean false.
Otherwise, the LIST should be in valid shape. "
    (if (null list)
        ;; for `nil', wrap as boolean false
        +mlx-false+
        (wrap-as-mlx-array
         (let* ((shape (shape list))
                (size   (reduce #'cl:* shape))
                (cffi-t (cffi-type<-mlx-dtype dtype!))
                (dsize  (foreign-type-size cffi-t))
                (coerce (%mlx-dtype-coerce dtype!)))
           (with-foreign-pointer (data (cl:* size dsize))
             (let ((idx -1))
               (labels ((dump (elem)
                          (if (atom elem)
                              (setf (mem-aref data cffi-t (incf idx))
                                    (funcall coerce elem))
                              (map nil #'dump elem))))
                 (dump list)))
             (with-foreign<-sequence (shape* shape :int ndim)
               (mlx_array_new_data data shape* ndim dtype!)))))))
  ;; TEST: #mlx-array-mlx-array
  (:method ((arr mlx-array) &key dtype)
    (if dtype
        (as-type arr dtype)
        arr)))

;; TEST: #arange
(defmlx-func arange (&rest [start]-stop-[step]-&key-step-dtype)
  "Generate ranges of numbers from START to STOP (not included) by STEP."
  :return "1-D `mlx-array' of shape ( floor((START - STOP) / STEP) )."
  :syntax "
    (arange [START] STOP [STEP] &key STEP DTYPE)

    + (arange stop       &key dtype step)
    + (arange start stop &key dtype step)
    + (arange start stop step &key dtype)
    + note that STEP would overwrite the key :step
"
  :definition "
`arange' is equal to calling:

    (mlx-array (loop :for n :from START :below STOP :by STEP
                     :collect n)
               :dtype DTYPE)
"
  :examples (("(arange STOP)"
              (arange 5)
              (mlx-array #(0 1 2 3 4)))
             ("(arange START STOP)"
              (arange 1 5)
              (mlx-array #(1 2 3 4)))
             ("(arange START :step STEP)"
              (arange 5 :step 2)
              (mlx-array #(0 2 4))))
  (multiple-value-bind (args keys)
      (split-args-keys [start]-stop-[step]-&key-step-dtype)
    (declare (ignore args))
    (destructuring-bind (start stop step)
        (reorder~ [start]-stop-[step]-&key-step-dtype)
      (let* ((coerce (%mlx-dtype-coerce :float64))
             (dtype  (getf keys :dtype))
             (dtype! (if dtype
                         (ensure-mlx-dtype dtype)
                         *default-mlx-dtype*)))
        (with-mlx-op "mlx_arange"
          ((funcall coerce (num<- (if (eql start :*) 0 start))) :double)
          ((funcall coerce (num<- stop))                        :double)
          ((funcall coerce (num<- step))                        :double)
          (dtype!                                               mlx-dtype))))))

;; TEST: #zeros, #ones
;; (op shape &key dtype)
(with-op-template (op cffi docs
                   (shape "shape of output array
if given as `mlx-array', will use shape of `mlx-array'")
                   (dtype "return `mlx-array' dtype (default `*default-mlx-dtype*')"))
    `(defmlx-func ,op (shape &key (dtype *default-mlx-dtype* dtype?)
                       &aux
                         (dtype! (if dtype? (ensure-mlx-dtype dtype) dtype))
                         (shape! (shape<- shape)))
       ,@docs
       :note ,(format nil "See `~(~A~)-like'. " op)
       (declare (type (simple-array (signed-byte 32)) shape!))
       (with-pointer-to-vector-data (shape* shape!)
         (with-mlx-op ,(sconc "mlx_" cffi)
           (shape*          :pointer)
           ((length shape!) :size)
           (dtype!          mlx-dtype))))
  (ones  "Construct an array of ones. ")
  (zeros "Construct an array of zeros. "))

;; ones-like, zeros-like
(with-op-template (op cffi docs
                   (array "input array
 the shape of result `mlx-array' will be same as ARRAY. ")
                   (dtype "return `mlx-array' dtype (default as ARRAY's dtype)"))
    (let ((quick (getf (rest docs) :quick)))
      `(defmlx-method ,op (array &key (dtype nil dtype?))
         ,@docs
         :note ,(format nil "See `~(~A~)'. " quick)
         :methods ((((array array) &key (dtype (dtype array)))
                    "Avoid alloc mlx-array. "
                    (,quick (shape array) :dtype dtype))
                   (((list  list)  &key (dtype (dtype list)))
                    "Avoid alloc mlx-array. "
                    (,quick (shape list) :dtype dtype))
                   (((number number) &key (dtype (dtype number)))
                    (,quick () :dtype dtype)))
         (let ((arr (if dtype? (as-type array dtype) array)))
           (with-mlx-op ,(sconc "mlx_" cffi) arr))))
  ((ones-like  "ones_like")
   "Construct an array of ones as shape of ARRAY. "
   :quick ones)
  ((zeros-like "zeros_like")
   "Construct an array of zeros as shape of ARRAY. "
   :quick zeros))

;; TEST: #full
(defmlx-func full (shape
                   &optional (value 0)
                   &key (dtype (mlx-dtype value) dtype?)
                   &aux
                     (shape! (shape<- shape))
                     (dtype! (if (cl:and dtype? dtype)
                                 dtype
                                 (ensure-mlx-dtype dtype))))
  "Construct an array of SHAPE filling with the given VALUE."
  :return "a `mlx-array' of SHAPE filled with VALUE."
  :parameters ((shape "shape of the array")
               (value "value to fill the array (default 0)")
               (dtype "data type of the output array
if unspecified the output type is inferred from vals
or `*default-mlx-dtype*'"))
  :note "See `full-like'. "
  (with-pointer-to-vector-data (shape* shape!)
    (with-mlx-op "mlx_full"
      (shape*          :pointer)
      ((length shape!) :int)
      value
      (dtype! mlx-dtype))))

;; TEST: #id
(defmlx-func id (shape &key (dtype *default-mlx-dtype* dtype?)
                 &aux
                   (dtype! (if dtype? (ensure-mlx-dtype dtype) dtype))
                   (shape! (shape<- shape)))
  "Create a square identity matrix."
  :parameters ((shape "shape of the identity matrix, which should be integer")
               (dtype "`mlx-dtype' of new matrix (default `*default-mlx-dtype*')"))
  (let ((shape (etypecase shape!
                 (integer shape!)
                 (simple-array
                  (ecase (length shape!)
                    (1 (aref shape! 0))
                    (2
                     (assert (cl:= (aref shape! 0) (aref shape! 1)) ()
                             "Expecting a square matrix, but got shape ~A. "
                             shape!)
                     (aref shape! 0)))))))
    (declare (type (integer 0) shape))
    (with-mlx-op "mlx_identity"
      (shape  :int)
      (dtype! mlx-dtype))))

;; TEST: #eye
(defmlx-func eye (shape &key (diag 0 diag?) (dtype *default-mlx-dtype* dtype?)
                  &aux
                    (diag* (if diag? (num<- diag) diag))
                    (shape! (shape<- shape))
                    (dtype! (if dtype? (ensure-mlx-dtype dtype) dtype)))
  "Create an identity matrix or a general diagonal matrix."
  :return "`mlx-array' where all elements are equal to zero,
except for the DIAG diagonal, whose values are equal to one."
  :parameters ((shape "shape of identity matrix")
               (diag  "diagonal index to be one (default 0)")
               (dtype "new matrix `mlx-dtype' (default `*default-mlx-dtype*')"))
  (declare (type integer diag*))
  (let ((n 1) (m 1))
    (declare (type (integer 1) n m))
    (etypecase shape!
      (integer
       (setf n shape!
             m shape!))
      (sequence
       (case (length shape!)
         (1 (setf n (aref shape! 0)
                  m (aref shape! 0)))
         (2 (setf n (aref shape! 0)
                  m (aref shape! 1)))
         (otherwise (error "Expecting a 2-D matrix, but got shape ~A. "
                           shape!)))))
    (with-mlx-op "mlx_eye"
      (n      :int)
      (m      :int)
      (diag*  :int)
      (dtype! mlx-dtype))))

;; (op (ARRAY dim=2) &key diag/diagonal)
(with-op-template (op cffi docs
                   (diagonal "diagonal of the 2-D array (default 0)")
                   (diag     "alias of DIAGONAL"))
    `(defmlx-method ,op (array &key (diag 0 diag?) (diagonal 0 diagonal?)
                         &aux (k (cond (diag?     (num<- diag))
                                       (diagonal? (num<- diagonal))
                                       (t         0))))
       ,@docs
       (declare (type integer k))
       (assert (dim= array 2))
       (with-mlx-op ,(sconc "mlx_" cffi) array (k :int)))
  ((tri-lower "tril")
   "Zeros the ARRAY above the given diagonal. "
   :aliases (tril))
  ((tri-upper "triu")
   "Zeros the ARRAY below the given diagonal. "
   :aliases (triu)))

;; TEST: #tri
(defmlx-func tri (rows &optional (cols rows cols?)
                  &key
                    (diag 0 diag?) (diagonal nil diagonal?)
                    (dtype *default-mlx-dtype* dtype?)
                    (pos :lower)
                  &aux
                    (rows! (num<- rows))
                    (cols! (if cols? (num<- cols) rows!))
                    (k (cond (diag?     (num<- diag))
                             (diagonal? (num<- diagonal))
                             (t         0)))
                    (dtype! (if dtype? (ensure-mlx-dtype dtype) dtype)))
  "Make an array with ones at and POS (`:lower'/`:upper') the given DIAGONAL and zeros elsewhere. "
  :parameters ((rows "number of rows of the triangle array")
               (cols "number of cols of the triangle array (default to ROWS)")
               (diagonal "diagonal of 2-D array")
               (diag     "alias of DIAGONAL")
               (dtype    "`mlx-dtype' or something can convert into `mlx-dtype'")
               (pos      "one of `:lower' and `:upper' for the ones (default `:lower')"))
  (declare (type integer rows! cols! k)
           (type (member :lower :upper) pos))
  (ecase pos
    (:lower
     (with-mlx-op "mlx_tri"
       (rows!  :int)
       (cols!  :int)
       (k      :int)
       (dtype! mlx-dtype)))
    (:upper
     (triu (ones (list rows! cols!) :dtype dtype!) :diagonal k))))

;; TEST: #linspace
(defmlx-func linspace (start stop &optional (num 50 num?)
                       &key (dtype *default-mlx-dtype* dtype?)
                       &aux
                         (num!   (if num? (num<- num) num))
                         (start! (num<- start))
                         (stop!  (num<- stop))
                         (dtype! (if dtype? (ensure-mlx-dtype dtype) dtype)))
  "Generate num evenly spaced numbers over interval [start, stop]."
  :parameters ((start "starting value")
               (stop  "stopping value")
               (num   "number of samples (default 50)")
               (dtype "`mlx-dtype' of the new array (default `*default-mlx-dtype*')"))
  (declare (type (integer 0) num))
  (let ((coerce (%mlx-dtype-coerce :float64)))
    (with-mlx-op "mlx_linspace"
      ((funcall coerce start!) :double)
      ((funcall coerce stop!)  :double)
      (num!                    :int)
      (dtype!                  mlx-dtype))))

;; TEST: #meshgrid
(let ((xy (foreign-string-alloc "xy"))
      (ij (foreign-string-alloc "ij")))
  (defmlx-func meshgrid (array &rest more-arrays-&key-spares-indexing)
    "Generate multidimensional coordinate grids from 1-D coordinate arrays."
    :syntax "(meshgrid array... &key (SPARES nil) (INDEXING :xy))"
    :parameters ((arrays "a sequence of `mlx-array'")
                 (spares "return dense or spare array (default `nil')
  + `t': a sparse grid is returned in which each output array
    has a single non-zero element
  + `nil': a dense grid is returned")
                 (indexing "how to index the output array (default `:xy')
  + `:xy': Cartesian
  + `:ij': Matrix"))
    (multiple-value-bind (more-arr keys)
        (split-args-keys more-arrays-&key-spares-indexing)
      (destructuring-bind (&key (spares nil) (indexing :xy)) keys
        (declare (type (member :xy :ij) indexing))
        (with-array-vector<-sequence (vec (cons array more-arr))
          (with-mlx-op ("mlx_meshgrid" :alloc mlx_vector_array_new
                                       :wrap  wrap-as-mlx-array-list)
            (vec :pointer)
            ((bool<- spares) :bool)
            ((ecase indexing
               (:xy xy)
               (:ij ij))
             :string)))))))


;;; Get Attributes of `mlx-array'
;; see `lisp/array.lisp'

(with-op-template (op cffi docstring
                   (x "input array"))
    (let* ((plist (rest docstring))
           (class (getf plist :cl-wrap-class 'number))
           (fn    (getf plist :cl-wrap)))
      `(defmlx-method ,op (x)
         ,@docstring
         ,@(when fn `(:methods ((((x ,class)) (,fn x)))))
         (with-mlx-op ,(sconc "mlx_" cffi) x)))
  (sign        "Element-wise sign(X). ")
  (degrees     "Convert angles from radians to degrees. ")
  (radians     "Convert angles from degrees to radians. "))

;; TEST: #tr
(defmlx-method tr (array
                   &key
                     (offset 0 offset?)
                     (axis1 0 axis1?) (axis2 1 axis2?)
                     (dtype (mlx-dtype array) dtype?)
                   &aux
                     (offset! (if offset? (num<-  offset) offset))
                     (axis1!  (if axis1?  (axis<- axis1)  axis1))
                     (axis2!  (if axis2?  (axis<- axis2)  axis2))
                     (dtype!  (if dtype?  (ensure-mlx-dtype dtype) dtype)))
  "Return the sum along a specified diagonal in the given array."
  :parameters ((offset "offset of the diagonal from the main diagonal.
Can be positive or negative. (default 0)")
               (axis1 "first axis of the 2-D sub-array of the diagonals")
               (axis2 "second axis of the 2-D sub-array of the diagonals")
               (dtype "data type of the output array. (default `mlx-dtype' of ARRAY)"))
  :definition "
The trace of ARRAY on AXIS1=i, AXIS2=j is defined as:

    tr_{i, j}(A) = sum(A_{a_i=k, a_j=k}, k=0~max(shape(A, i), shape(A, j)))
"
  (declare (type integer offset! axis1! axis2!))
  (with-mlx-op "mlx_trace"
    array
    (offset! :int)
    (axis1!  :int)
    (axis2!  :int)
    (dtype!  mlx-dtype)))

;; TEST: #diag
(defmlx-method diag (array &optional (n 0 n?) &aux (n* (if n? (num<- n) n)))
  "Extract a diagonal or construct a diagonal matrix."
  :return "extracted diagonal or the constructed diagonal matrix."
  :parameters ((array "input array
  + 1-D array: diagonal matrix is constructed with ARRAY on the Nth diagonal
  + 2-D array: Nth diagonal is returned")
               (n     "diagonal to extract or construct (default 0)"))
  (declare (type integer n*))
  (with-mlx-op "mlx_diag"
    array
    (n* :int)))

;; TEST: #finite/nan/inf/neg-inf/pos-inf-p
(with-op-template (op cffi docstring
                   (x "input array"))
    (let* ((plist (rest docstring))
           (class (getf plist :cl-wrap-class 'number))
           (fn    (getf plist :cl-wrap)))
      `(defmlx-method ,op (x)
         ,@docstring
         ,@(when fn `(:methods ((((x ,class)) (,fn x)))))
       (with-mlx-op ,(sconc "mlx_" cffi) x)))
  ((finite-p  "isfinite") "Test if element is finite. ")
  ((nan-p     "isnan")    "Test if element is NaN. ")
  ((inf-p     "isinf")    "Test if element is inf. ")
  ((neg-inf-p "isneginf") "Test if element is negative inf. ")
  ((pos-inf-p "isposinf") "Test if element is positive inf. "))

;; TEST: #num<-nan
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

;; TEST: #std-var
;; (op ARRAY &key (axis/axes 0) (ddof 0) (keep-dim-p *keep-dim-p*))
(with-op-template (op cffi docs
                   (array      "input array")
                   (axis       "optional axis to reduce over (default 0)")
                   (axes       "alias of axis")
                   (ddof       "delta degrees of freedom (default 0)")
                   (keep-dim-p "keep reduced axes as signelton dimensions (default nil)"))
    `(defmlx-method ,op (array
                         &key
                           (axis nil axis?) (axes nil axes?)
                           (ddof 0) (keep-dim-p *keep-dim-p* keep-dim-p?)
                         &aux
                           (ddof* (num<- ddof))
                           (axis* (cond (axis? (axes<- axis))
                                        (axes? (axes<- axes))
                                        (t     0)))
                           (keepdimsp (if keep-dim-p?
                                          (bool<- keep-dim-p)
                                          keep-dim-p)))
       ,@docs
       (declare (type (or null integer sequence mlx-array) axis*)
                (type (integer 0) ddof*))
       (etypecase axis*
         (null
          (with-mlx-op ,(sconc "mlx_" cffi)
            array
            (keepdimsp :bool)
            (ddof*     :int)))
         (mlx-axis                      ; :int
          (with-mlx-op ,(sconc "mlx_" cffi "_axis")
            array
            (axis*     :int)
            (keepdimsp :bool)
            (ddof*     :int)))
         (mlx-axes                      ; (simple-array :int)
          (with-pointer-to-vector-data (axes axis*)
            (with-mlx-op ,(sconc "mlx_" cffi "_axes")
              array
              (axes           :pointer)
              ((length axis*) :size)
              (keepdimsp      :bool)
              (ddof*          :int))))))
  (std
   "Compute the standard deviation(s) over the given AXIS(AXES)."
   :definition "standard deviations = sqrt(var(ARRAY))")
  (var
   "Compute the variance(s) over the given AXIS(AXES). "
   :definition "variance = sum((ARRAY - mean(ARRAY))^2) / (dim(ARRAY) - DDOF)"))


;;; Indexing elements in mlx-array

;; TEST: #at
(defmlx-method at (array &rest indexs)
  "Take INDEXS on ARRAY. "
  :parameters ((array  "input array")
               (indexs "how to index elements or slice subarray of ARRAY

Taking by indexs:
 + integer of index: take element of subarray
 + sequence of indexs: will take elements along the axis

Slicing:
 + nil: take all
 + (~ ...) or (~~ ...): [start] stop [step] &key step
   See `~' and `~~' for detailed documentation
 + rational:
   + positive: (~ 0 (ceiling rational * shape) 1)
   + negative: (~ (floor rational * shape) shape 1)

Syntax sugars:
there's some predefined slicing methods, for example:

    (at array :first)      ; first element
    (at array '(:first 2)) ; first two elements

Use (documentation :first 'mlx:slice) to get documentation
of the slicing methods.

Use `defmlx-slice' to define your own slicing shortcuts.

Dev Note: the slice shortcuts are defined in `mlx::*slice-spec-rules*'. ")
               (*keep-dim-p* "if `*keep-dim-p*' is non-nil,
the output slice would keep dim as the original input ARRAY;
otherwise, the result would be `squeezed'. "))
  :dev-note "
The MLX-C method `mlx_slice' will return the slice as the same shape
of the input ARRAY. But the MLX package of Python would squeeze the
extra shape infomation, which is much more userfriendly. So i take
that API design. "
  (let* ((len  (dim array))
         (size (cl:* len (foreign-type-size :int)))
         (take ()))
    (with-foreign-pointer (start* size)
      (with-foreign-pointer (stop* size)
        (with-foreign-pointer (step* size)
          (loop :for axis :below len
                :for shape :in (shape array)
                :for slice := (evaluate-slice-spec (pop indexs) shape)
                :if (listp slice)
                  :do (destructuring-bind (start stop step) (rest slice)
                        (setf (mem-aref start* :int axis) start
                              (mem-aref stop*  :int axis) stop
                              (mem-aref step*  :int axis) step))
                :else
                  :do (push (cons axis (mlx-array slice)) take)
                      (setf (mem-aref start* :int axis) 0
                            (mem-aref stop*  :int axis) shape
                            (mem-aref step*  :int axis) 1))
          (let ((slice (with-mlx-op "mlx_slice"
                         array
                         (start* :pointer)
                         (len    :size)
                         (stop*  :pointer)
                         (len    :size)
                         (step*  :pointer)
                         (len    :size))))
            (loop :for (axis . idxs) :in take
                  :do (setf slice (with-mlx-op "mlx_take_axis"
                                    slice
                                    idxs
                                    (axis :int))))
            ;; TEST: #at-keep-dim-p
            (if *keep-dim-p*
                slice
                (squeeze slice))))))))

;; TEST: #at-setf
(defmlx-method (setf at) (value array &rest slices)
  "Set SLICES of ARRAY by VALUE. "
  :parameters ((value "value to be updated to ARRAY slices

the value would be boardcast to fit ARRAY, for example:

    (setf (at #(1 2 3 4) (~ 0 3)) 5)
    ; => array([5, 5, 5, 4], dtype=uint64)
")
               (slices "see `at' for the slice rules"))
  :dev-note "
"
  (let* ((len  (dim array))
         (size (cl:* len (foreign-type-size :int))))
    (with-foreign-pointer (start* size)
      (with-foreign-pointer (stop* size)
        (with-foreign-pointer (step* size)
          (loop :for axis :below len
                :for shape :in (shape array)
                :for slice := (evaluate-slice-spec (pop slices) shape)
                :if (listp slice)
                  :do (destructuring-bind (start stop step) (rest slice)
                        (setf (mem-aref start* :int axis) start
                              (mem-aref stop*  :int axis) stop
                              (mem-aref step*  :int axis) step))
                :else
                  ;; TODO: #mlx-cl #missing
                  ;; to (setf (at arr '(0 1 2)) #(0 1 2))
                  :do (error "Taking index ~A is not supported yet. "
                             slice))
          (let ((res (with-mlx-op "mlx_slice_update"
                       array
                       value
                       (start* :pointer)
                       (len    :size)
                       (stop*  :pointer)
                       (len    :size)
                       (step*  :pointer)
                       (len    :size))))
            (%steal-mlx-array-pointer res array)
            array))))))

(defun at* (array &rest indexs)
  "(lisp<- (at ARRAY . INDEXS))"
  (lisp<- (apply #'at array indexs)))

(defun (setf at*) (value array &rest indexs)
  "same as (setf at). "
  (apply #'(setf at) value array indexs))


;;;; Basic Operations

;; TEST: #elementry-operation
;; add, sub, mul, div
(with-op-template (op cffi docs
                   (a "input array")
                   (b "input array"))
    (let* ((plist (rest docs))
           (class (getf plist :cl-wrap-class 'number))
           (fn    (getf plist :cl-wrap)))
      `(defmlx-method ,op (a b)
         ,@docs
         ,@(when fn `(:methods ((((a ,class) (b ,class)) (,fn a b)))))
         (with-mlx-op ,(sconc "mlx_" cffi) a b)))
  ;; TEST: #elementry-operation
  (add
   "Element-wise A + B. "
   :cl-wrap cl:+
   :aliases (op2+))
  ((sub "subtract")
   "Element-wise A - B. "
   :cl-wrap cl:-
   :aliases (op2-))
  ((mul "multiply")
   "Element-wise A * B. "
   :cl-wrap cl:*
   :note "See `matmul' or `@' for matrix multiply. "
   :aliases (op2*))
  ((div "divide")
   "Element-wise A / B. "
   :cl-wrap cl:/
   :note "See `inverse' for inverse matrix. "
   :aliases (op2/)))

;; TEST: #elementry-operation, #single-op-inv
;; +, -, *, /
(with-op-template (op cffi docs
                   (elem      "input array")
                   (more-elem "can be empty"))
    (destructuring-bind (&key
                           (single        '(copy elem))
                           (initial-value 'elem)
                           (reduce        (intern* cffi))
                         &allow-other-keys)
        (rest docs)
      `(defmlx-func ,op (elem &rest more-elem)
         ,@docs
         (if (endp more-elem) ,single
             (reduce #',reduce more-elem :initial-value ,initial-value))))
  ((+ add) "Element-wise sum of ELEM and MORE-ELEM. "
   :return "`mlx-array' of sum")
  ((- sub) "Element-wise substract of ELEM and MORE-ELEM."
   :return "`mlx-array' of sum, or `negative' of ELEM if no MORE-ELEM. "
   :single (negative elem))
  ((* mul) "Element-wise multiply of ELEM and MORE-ELEM. "
   :return "`mlx-array' of multiplication")
  ((/ div) "Element-wise ELEM divided by MORE-ELEM. "
   :single (reciprocal elem)
   :return "`mlx-array' of division, or `reciprocal' of ELEM if no MORE-ELEM"))

;; TEST: #1arg-op-with-cl-wrap
;; (op ARRAY)
(with-op-template (op cffi docstring
                   (x "input array"))
    (let* ((plist (rest docstring))
           (class (getf plist :cl-wrap-class 'number))
           (fn    (getf plist :cl-wrap)))
      `(defmlx-method ,op (x)
         ,@docstring
         ,@(when fn
             (etypecase fn
               (symbol `(:methods ((((x ,class)) (,fn x)))))
               (list   `(:methods ((((x ,class)) ,fn))))))
         (with-mlx-op ,(sconc "mlx_" cffi) x)))
  ;; TEST: #abs
  (abs                  "Element-wise abs(X) = | X |. "      :cl-wrap cl:abs)

  ;; TEST: #exp-log
  (exp                  "Element-wise exp(X). "              :cl-wrap cl:exp)
  ((loge     "log")     "Element-wise ln(X). "               :cl-wrap cl:log)

  ;; TEST: #square-sqrt
  (square               "Element wise X^2. "                 :cl-wrap (cl:* x x))
  (sqrt                 "Element-wise sqrt(X). "             :cl-wrap cl:sqrt)
  (rsqrt                "Element-wise sqrt(1/X). "           :cl-wrap (cl:sqrt (cl:/ x)))

  ;; TEST: #complex-op
  ((imagpart "imag")    "Element-wise imaginary part of X. " :cl-wrap cl:imagpart)
  ((realpart "real")    "Element-wise real part of X. "      :cl-wrap cl:realpart)
  (conjugate
   "Element wise complex conjugate. "
   :cl-wrap cl:conjugate
   :definition "conjugate of X is equal to (- (realpart X) (imagpart X)). "
   :note       "See `imagpart' and `realpart'. ")

  ;; TEST: #trigonometric-functions
  (sin                  "Element-wise sin(X). "              :cl-wrap cl:sin)
  (cos                  "Element-wise cos(X). "              :cl-wrap cl:cos)
  (tan                  "Element-wise tan(X). "              :cl-wrap cl:tan)
  (sinh                 "Element-wise sinh(X). "             :cl-wrap cl:sinh)
  (cosh                 "Element-wise cosh(X). "             :cl-wrap cl:cosh)
  (tanh                 "Element-wise tanh(X). "             :cl-wrap cl:tanh)
  ((asin     "arcsin")  "Element-wise arcsin(X). "           :cl-wrap cl:asin)
  ((acos     "arccos")  "Element-wise arccos(X). "           :cl-wrap cl:acos)
  ((atan     "arctan")  "Element-wise arctan(X). "           :cl-wrap cl:atan)
  ((asinh    "arcsinh") "Element-wise arcsinh(X). "          :cl-wrap cl:asinh)
  ((acosh    "arccosh") "Element-wise arccosh(X). "          :cl-wrap cl:acosh)
  ((atanh    "arctanh") "Element-wise arctanh(X). "          :cl-wrap cl:atanh)

  ;; TEST: #logical-operation
  ((! "logical_not")
   "Element-wise logical not. "
   :cl-wrap cl:not
   :aliaes (¬ logical-not))

  ;; TEST: single-op-inv
  (negative
   "Element wise -X. "
   :cl-wrap cl:-
   :dev-note "`negative' is equal to calling (- X).
See `-' for details. ")
  (reciprocal
   "Element wise 1/X. "
   :cl-wrap cl:/
   :dev-note "`reciprocal' is equal to calling (/ X).
See `/' for details. ")

  ;; TEST: bitwise-ops
  ((lognot "bitwise_invert")
   "Element-wise bitwise inverse. "
   :aliases (bit-not)
   :cl-wrap cl:lognot))

;; TEST: #exp-log
(defmlx-method log (array &optional (base nil base?))
  "Element-wise log(ARRAY) of BASE. "
  :parameters ((array "input array")
               (base  "number of log base (default log_E(ARRAY))"))
  :definition "log_BASE(ARRAY) = log_E(ARRAY) / log_E(BASE)"
  :methods ((((number number) &optional base)
             (if base
                 (cl:log number (num<- base))
                 (cl:log number ))))
  (if base?
      (case base
        (2  (log2  array))
        (10 (log10 array))
        (otherwise (div (loge array) (loge base))))
      (loge array)))

;; TEST: #2args-op-with-cl-wrap
(with-op-template (op cffi docs
                   (a "input array")
                   (b "input array"))
    (let* ((plist (rest docs))
           (class (getf plist :cl-wrap-class 'number))
           (fn    (getf plist :cl-wrap)))
      `(defmlx-method ,op (a b)
         ,@docs
         ,@(when fn `(:methods ((((a ,class) (b ,class)) (,fn a b)))))
         (with-mlx-op ,(sconc "mlx_" cffi) a b)))
  ;; TEST: #remainder
  (remainder
   "Element-wise remainder of A / B. "
   :cl-wrap cl:mod
   :definition "A = div * B + remainder")
  ((expt "power")
   "Element-wise A ^ B. "
   :cl-wrap cl:expt)
  ((op2max "maximum")
   "Element-wise max of max(A, B). "
   :cl-wrap cl:max)
  ((op2min "minimum")
   "Element-wise min of min(A, B). "
   :cl-wrap cl:min))

;; TEST: #mod
(defmlx-method mod (a b)
  "Element-wise A % B. "
  :parameters ((a "input array")
               (b "input array"))
  :return "values are "
  :dev-note "
The fuction divmod(a, b) is equivalent to but faster than (a // b, a % b).
The function uses numpy-style broadcasting semantics.
Either or both input arrays can also be scalars.
"
  :methods ((((a number) (b number))
             (values (cl:truncate a b) (cl:mod a b))))
  (values-list
   (with-mlx-op ("mlx_divmod"
                 :alloc mlx_vector_array_new
                 :wrap  wrap-as-mlx-array-list)
     a
     b)))


;;;; Clipping

;; TEST: #floor
(defmlx-method floor (array &optional divisor)
  #-lispworks "⌊ARRAY / DIVISOR⌋"
  #+lispworks "floor(ARRAY / DIVISOR)"
  :parameters ((array   "input array")
               (divisor "by default, divisor is 1"))
  :methods ((((num number) &optional (divisor 1))
             (etypecase divisor
               (mlx-array (floor (mlx-array num) divisor))
               (number    (cl:floor num divisor)))))
  (if divisor
      (with-mlx-op "mlx_floor_divide"
        array
        ((mlx-array divisor) mlx-array))
      (with-mlx-op "mlx_floor" array)))

;; TEST: #ceiling
(defmlx-method ceiling (array &optional divisor)
  #-lispworks "⌈ARRAY / DIVISOR⌉"
  #+lispworks "ceiling(ARRAY / DIVISOR)"
  :parameters ((array   "input array")
               (divisor "by default, divisor is 1"))
  :methods ((((num number) &optional (divisor 1))
             (etypecase divisor
               (mlx-array (ceiling (div (mlx-array num) divisor)))
               (number    (cl:ceiling num divisor)))))
  (if divisor
      (ceiling (div array divisor))
      (with-mlx-op "mlx_ceil" array)))

;; TEST: #clip
(defmlx-method clip (array &key min max)
  "Clip the values of the ARRAY between the given MIN and MAX."
  :parameters ((array "input array")
               (min   "min range to clip, ignore if not given")
               (max   "max range to clip, ignore if not given"))
  (flet ((ptr (obj) (if obj (mlx-object-pointer obj) (null-pointer))))
    (with-mlx-op "mlx_clip"
      array
      ((ptr min) :pointer)
      ((ptr max) :pointer))))

;; TEST: #round
(defmlx-method round (array &optional (decimals 0 d?)
                      &aux (decimal (if d? (num<- decimals) decimals)))
  "Round to the given number of decimals."
  :definition "
  (let ((scalar (expt 10 DECIMALS)))
    (/ (round (* ARRAY scalar)) scalar))
"
  :parameters ((decimals "number of decimal places to round to (default 0)"))
  (declare (type integer decimals))
  (with-mlx-op "mlx_round"
    array
    (decimals :int)))


;;;; Products Operations

;; TEST: #products
(with-op-template (op cffi docs
                   (a "input array")
                   (b "input array"))
    (let* ((plist (rest docs))
           (class (getf plist :cl-wrap-class 'number))
           (fn    (getf plist :cl-wrap)))
      `(defmlx-method ,op (a b)
         ,@docs
         ,@(when fn `(:methods ((((a ,class) (b ,class)) (,fn a b)))))
         (with-mlx-op ,(sconc "mlx_" cffi) a b)))
  ;; TEST: #matmul
  (matmul
   "Matrix multiplication. "
   :definitino "")
  ;; TEST: #inner
  (inner
   "Inner product of A : B. "
   :definition "")
  ;; TEST: #outter
  (outer
   "Outter product of A × B. "
   :definition "")
  ;; TEST: #kron
  (kron  #-lispworks "Kronecker product of A ⊗ B"
         #+lispworks "Kronecker product of A and B. "
         :definition ""))

(with-op-template (op cffi docs
                   (elem      "input array")
                   (more-elem "can be empty"))
    (destructuring-bind (&key
                           (single        '(copy elem))
                           (initial-value 'elem)
                           (reduce        (intern* cffi))
                         &allow-other-keys)
        (rest docs)
      `(defmlx-func ,op (elem &rest more-elem)
         ,@docs
         (if (endp more-elem) ,single
             (reduce #',reduce more-elem :initial-value ,initial-value))))
  ((@ matmul) "Matrix multiplication of ELEMS. "))

;; TODO: #mlx-cl #optimize
;; a compiler macro for the `+' and `add'
;; Example:
;; + (+ (* alpha a b) (* beta c))
;; + (add (mul alpha a b) (mul beta c))
;; where ALPHA and BETA are const value
;;
(defmlx-method addmm (a b c &key (alpha 1.0 alpha?) (beta 1.0 beta?)
                      &aux
                        (a* (if alpha? (num<- alpha) alpha))
                        (b* (if beta?  (num<- beta)  beta)))
  "Matrix multiplication with addition and optional scaling."
  :return "ALPHA * (A @ B) + BETA * C."
  :parameters ((c     "input constant offset `mlx-array'")
               (alpha "scaling parameter for (A @ B)")
               (beta  "scaling parameter for constant offset C"))
  (with-mlx-op "mlx_addmm" c a b (a* :float) (b* :float)))

;; TEST: #tensordot
(defmlx-method tensordot (array1 array2 &key axes axis
                          &aux (axis* (axes<- axis axes 2)))
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


;;;; Bitwise Operations

;; TEST: #bitwise-ops
(with-op-template (op cffi docs
                   (a "input array")
                   (b "input array"))
    (let* ((plist (rest docs))
           (class (getf plist :cl-wrap-class 'number))
           (fn    (getf plist :cl-wrap)))
      `(defmlx-method ,op (a b)
         ,@docs
         ,@(when fn `(:methods ((((a ,class) (b ,class)) (,fn a b)))))
         (with-mlx-op ,(sconc "mlx_" cffi) a b)))
  ((logand "bitwise_and") "Element-wise bitwise A & B (A bitand B). "
   :cl-wrap cl:logand
   :aliases (bit-and))
  ((logxor "bitwise_xor") "Element-wise bitwise A ^ B (A bitxor B). "
   :cl-wrap cl:logxor
   :aliases (bit-xor))
  ((logior "bitwise_or") "Element-wise bitwise A | B (A bitor B). "
   :cl-wrap cl:logior
   :aliases (bit-ior bit-or)))

;; TEST: #comparing
(with-op-template (op cffi docs
                   (a "input array")
                   (b "input array"))
    (let* ((plist (rest docs))
           (class (getf plist :cl-wrap-class 'number))
           (fn    (getf plist :cl-wrap)))
      `(defmlx-method ,op (a b)
         ,@docs
         ,@(when fn `(:methods ((((a ,class) (b ,class)) (,fn a b)))))
         (with-mlx-op ,(sconc "mlx_" cffi) a b)))
  ((op2<  "less")
   "Element-wise A < B. "
   :cl-wrap cl:<)
  ((op2<= "less_equal")
   "Element-wise A <= B. "
   :cl-wrap cl:<=)
  ((op2>  "greater")
   "Element-wise A > B. "
   :cl-wrap cl:>)
  ((op2>= "greater_equal")
   "Element-wise A >= B. "
   :cl-wrap cl:>=)
  ((op2= "equal")
   "Element-wise A == B. "
   :note "Not equal comparison on two arrays with numpy-style broadcasting semantics.
Either or both input arrays can also be scalars."
   :cl-wrap cl:=)
  ((op2/= "not_equal")
   "Element-wise A != B. "
   :note "Not equal comparison on two arrays with numpy-style broadcasting semantics.
Either or both input arrays can also be scalars."
   :cl-wrap cl:/=))

;; TEST: #compare=
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

;; TEST: #compare-windowed
;; (op ELEM &rest ELEMS)
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

(defmlx-method op2~= (array1 array2
                          &key
                            relative-tolerance rtol
                            absolute-tolerance atol
                            (nan-equal-p *nan-equal-p*)
                          &aux
                            (rtol* (coerce (num<- (cl:or relative-tolerance
                                                         rtol
                                                         *relative-tolerance*))
                                           'double-float))
                            (atol* (coerce (num<- (cl:or absolute-tolerance
                                                         atol
                                                         *absolute-tolerance*))
                                           'double-float))
                            (equal-nan-p (bool<- nan-equal-p)))
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

;; TEST: #compare-every-pair
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
                         :do (setf res (,combine res (,compare first then))))
               :finally (return res))))
  (~= "Approximate comparision of ELEM and MORE-ELEM. "
      :compare op2~=)
  (/= "Test if none ELEM and MORE-ELEM are equal to each other. "
      :compare op2/=))

;; TEST: #ash
(defmlx-method ash (array (count integer))
  "Element-wise left shift."
  :return "`mlx-array' of ARRAY << count."
  :arameters ((array "input array")
              (count "shifting integer,
 + positive, shift left;
 + negative, shift right"))
  :methods ((((array mlx-array) (count mlx-array))
             (with-mlx-op "mlx_left_shift" array count))
            ((arr (count mlx-array))
             (ash arr count))
            (((num number) (count integer))
             (cl:ash num count)))
  :aliases (left-shift)
  (cond ((cl:> count 0)
         (with-mlx-op "mlx_left_shift"
           array
           ((mlx-array count :dtype :uint32) mlx-array)))
        ((cl:< count 0)
         (with-mlx-op "mlx_right_shift"
           array
           ((mlx-array count :dtype :uint32) mlx-array)))
        (t array)))


;;;; Miscs Operations

;; TEST: #miscs
(with-op-template (op cffi docstring
                   (x "input array"))
    (let* ((plist (rest docstring))
           (class (getf plist :cl-wrap-class 'number))
           (fn    (getf plist :cl-wrap)))
      `(defmlx-method ,op (x)
         ,@docstring
         ,@(when fn `(:methods ((((x ,class)) (,fn x)))))
         (with-mlx-op ,(sconc "mlx_" cffi) x)))
  (erf
   "Element wise error function erf(X). "
   :definition "erf(x) = (2 / x) int(exp(- t^2), {t, 0, x})")
  (erfinv      "Element wise error function erf^-1(X). ")

  (expm1       "Element-wise exponential minus 1.")
  ((log1+ "log1p") "Element-wise log(A + 1). ")
  (log2        "Element-wise log2(X). ")
  (log10       "Element-wise log10(X). ")

  (copy
   "Copy element. "
   :cl-wrap cl:identity)

  ;; TODO: #mlx-cl #api-change
  ;; move `stop-gradient' to `mlx.nn' package
  ((stop-gradient "stop_gradient")
   "Stop gradients of X from being computed. "
   :note "
The operation is the identity but it prevents gradients from flowing
through the array.")
  (sigmoid
   "Element-wise logistic sigmoid(X). "
   :definition "sigmoid(X) = exp(X) / (1 + exp(X))")

  ;; TODO: #mlx-cl #api-change
  ;; move `hadamard-transform' to `mlx.fft' package
  ((hadamard-transform "hadamard_transform")
   "Perform the Walsh-Hadamard transform along the final axis."
   :definition "
Return (hadamard-matrix(len(X)) @ X) * scale,
where Hadamard Matrix is defined as

            1     / H_{m-1}  H_{m-1}  \
   H_m = -------  |                   |
         sqrt(2)  \ H_{m-1}  -H_{m-1} /

The Hadamard Transformation could be considered as a general Fourier
Transformation."))

;; TODO: #mlx-cl #missing
;; should have `mode' key, but the mlx-c seems to not support it
(defmlx-method quantize ((weight mlx-array) &key (group-size 64) (bits 4))
  "Quantize the matrix w using bits bits per element."
  :return "values of quantized version of WEIGHT, quantization scales, and bias."
  :parameters ((weight "matrix to be quantized")
               (group-size "size of the group in w that shares a scale and bias (default 64)")
               (bits "number of bits occupied by each element of w in the returned quantized matrix (default 4)"))
  :note "Quantization is a technique to reduce the computational and memory costs of
running inference by representing the weights and activations with
low-precision data types like 8-bit integer (int8) instead of the usual
32-bit floating point (float32)."
  (declare (type (integer 0) group-size bits))
  (with-elem& (res0& res0 :type :pointer :alloc (mlx_array_new))
    (with-elem& (res1& res1 :type :pointer :alloc (mlx_array_new))
      (with-elem& (res2& res2 :type :pointer :alloc (mlx_array_new))
        (ensure-success "mlx_quantize"
          :pointer res0&
          :pointer res1&
          :pointer res2&
          :pointer (mlx-object-pointer weight)
          :int     group-size
          :int     bits)
        (values (wrap-as-mlx-array res0&)
                (wrap-as-mlx-array res1&)
                (wrap-as-mlx-array res2&))))))

;; TODO: #mlx-cl #optimization
;; constant 0 should be preallocated as `+mlx-zero+'
;; (preallocated `mlx-array' of `0')
(defmlx-method quantized-matmul (array weight scales
                                 &key bias (transpose t) (group-size 64) bits
                                 &aux (bias! (cl:or bias 0))
                                   (transpose! (cl:and transpose t)))
  "Perform the matrix multiplication with the quantized matrix WEIGHT."
  :return "a manipulation of `mlx-array'. "
  :note "
The quantization uses one floating point SCALE and BIAS per GROUP-SIZE of elements.
Each element in WEIGHT takes BITS and is packed in an unsigned 32 bit integer."
  :parameters ((weight "quantized matrix packed in unsigned integers")
               (scales "scales to use per GROUP-SIZE elements of WEIGHT")
               (bias   "biases to use per GROUP-SIZE elements of WEIGHT (default `nil').")
               (transpose "whether to multiply with the transposed WEIGHT or not
namely whether we are performing x @ w.T or x @ w (default `t').")
               (group-size "size of the group in w that shares a scale and bias (default 64).")
               (bits "number of bits occupied by each element in w (default 4)"))
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

;; (op ARRAY &key AXIS/AXES KEEP-DIM-P)
(with-op-template (op cffi docs
                   (array "input array")
                   (axis  "optional axis or axes to reduce over (default 0)
  + `nil': reducing over the entire array
  + integer index: reduce on the specified AXIS
  + sequence of indexs: reduce on the specified AXES")
                   (axes  "alias of axis"))
    `(defmlx-method ,op (array
                         &key (axis nil axis?) (axes nil axes?)
                           (keep-dim-p *keep-dim-p* keep-dim-p?)
                         &aux
                           (axis*     (cond (axis? (axes<- axis))
                                            (axes? (axes<- axes))
                                            (t nil)))
                           (keepdimsp (if keep-dim-p?
                                          (bool<- keep-dim-p)
                                          keep-dim-p)))
       ,@docs
       (declare (type (or null integer sequence mlx-array) axis*))
       (etypecase axis*
         (null
          (with-mlx-op ,(sconc "mlx_" cffi)
            array
            (keep-dim-p :bool)))
         (mlx-axis                      ; int
          (with-mlx-op ,(sconc "mlx_" cffi "_axis")
            array
            (axis      :int)
            (keepdimsp :bool)))
         (mlx-axes                      ; (simple-array int)
          (with-pointer-to-vector-data (axes axis*)
            (with-mlx-op ,(sconc "mlx_" cffi "_axes")
              array
              (axes           :pointer)
              ((length axis*) :size)
              (keepdimsp      :bool))))))
  (all "An `and' reduction over the given axes. ")
  (any "An `or'  reduction over the given axes. ")

  ;; DEV: should keep the API same as Common Lisp `min' and `max'
  ;; or keep the API same as MLX?
  ((maximum "max") "A `max' reduction over the given axes. ")
  ((minimum "min") "A `min' reduction over the given axes. ")

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
    `(defmlx-method ,op (array &key (axis nil axis?) (axes nil axes?)
                         &aux (axis* (cond (axis? (axes<- axis))
                                           (axes? (axes<- axes))
                                           (t ,(getf (rest docs) :default-axis 0)))))
       ,@docs
       (declare (type (or null integer sequence mlx-array) axis*))
       (etypecase axis*
         (null
          (with-mlx-op ,(sconc "mlx_" cffi)
            array))
         (mlx-axis                      ; int
          (with-mlx-op ,(sconc "mlx_" cffi "_axis")
            array
            (axis* :int)))
         (mlx-axes                      ; (simple-array int)
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


;;;; Control Flow

(with-op-template (op cffi docs
                   (a "input array")
                   (b "input array"))
    (let* ((plist (rest docs))
           (class (getf plist :cl-wrap-class 'number))
           (fn    (getf plist :cl-wrap)))
      `(defmlx-method ,op (a b)
         ,@docs
         ,@(when fn `(:methods ((((a ,class) (b ,class)) (,fn a b)))))
         (with-mlx-op ,(sconc "mlx_" cffi) a b)))
  ;; TEST: #comparing
  ((op2<  "less")
   "Element-wise A < B. "
   :cl-wrap cl:<)
  ((op2<= "less_equal")
   "Element-wise A <= B. "
   :cl-wrap cl:<=)
  ((op2>  "greater")
   "Element-wise A > B. "
   :cl-wrap cl:>)
  ((op2>= "greater_equal")
   "Element-wise A >= B. "
   :cl-wrap cl:>=)
  ((op2= "equal")
   "Element-wise A == B. "
   :note "Not equal comparison on two arrays with numpy-style broadcasting semantics.
Either or both input arrays can also be scalars."
   :cl-wrap cl:=)
  ((op2/= "not_equal")
   "Element-wise A != B. "
   :note "Not equal comparison on two arrays with numpy-style broadcasting semantics.
Either or both input arrays can also be scalars."
   :cl-wrap cl:/=)

  ((op2and "logical_and") "Element-wise A and B. "
   :cl-wrap cl:and
   :aliases (logical-and))
  ((op2or  "logical_or")  "Element-wise A or B. "
   :cl-wrap cl:or
   :aliases (logical-or)))

;; TEST: #min-max, #argmin-argmax
;; (op ARRAY &key AXIS KEEP-DIM-P)
(with-op-template (op cffi docs
                   (axis "")
                   (keep-dim-p "keep reduced axes as singleton dimensions (default `nil')"))
    `(defmlx-method ,op (array &key (axis nil axis?) (keep-dim-p *keep-dim-p*)
                         &aux (axis* (if axis? (axis<- axis) axis)))
       ,@docs
       (declare (type (cl:or null integer) axis*))
       (if axis*
           (with-mlx-op ,(sconc "mlx_" cffi "_axis")
             array
             (axis* :int)
             (keep-dim-p :bool))
           (with-mlx-op ,(sconc "mlx_" cffi)
             array
             (keep-dim-p :bool))))
  (argmin "Indices of the minimum values along the axis. ")
  (argmax "Indices of the maximum values along the axis."))

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
         (if (endp more-elem) ,single
             (reduce #',reduce more-elem :initial-value ,initial-value))))
  ((min op2min) "Element-wise min. "
   :return "`mlx-array' of element-wise min. ")
  ((max op2max) "Element-wise max. "
   :return "`mlx-array' of element-wise max. ")
  ((&& op2and) "Logical `and' all ELEM and MORE-ELEM. "
   :dev-note "This would compute all the arguments (ELEM and MORE-ELEM). "
   :aliases ( #-lispworks ∧ ))
  ((||  op2or)  "Logical `or' all ELEM and MORE-ELEM. "
   :dev-note "This would compute all the arguments (ELEM and MORE-ELEM). "
   :aliases ( #-lispworks ∨ )))

#+lispworks
(defalias ∧ &&)

#+lispworks
(defalias ∨ ||)


;;;; Operation on Shape

;; TEST: #roll
(defmlx-method roll (array (shift t)
                     &key
                       (axis nil axis?)
                       (axes nil axes?)
                     &aux
                       (axis!  (cond (axis? (axes<- axis))
                                     (axes? (axes<- axes))
                                     (t     nil))))
  "Roll array elements along a given axis."
  :note "
Elements that are rolled beyond the end of the array are introduced
at the beggining and vice-versa.

If the axis is not provided the array is flattened, rolled and then
the shape is restored."
  :parameters ((array "input array")
               (shift "number of places by which elements are shifted.
 + If positive the array is rolled to the right,
 + if negative it is rolled to the left.
 + If an int is provided but given multiply AXES,
   then the same value is used for all axes")
               (axis "axis or axes along which to roll the elements")
               (axes "alias of AXIS"))
  :methods ((((array t) (shift t) &key axis axes)
             (roll (mlx-array array) shift :axis (or axis axes))))

    (etypecase axis!
      (null
       (let ((shift (vec<- shift)))
         (with-pointer-to-vector-data (shift* shift)
           (with-mlx-op "mlx_roll"
             array
             (shift*         :pointer)
             ((length shift) :size)))))
      (integer
       (let ((shift (vec<- shift)))
         (with-pointer-to-vector-data (shift* shift)
           (with-mlx-op "mlx_roll_axis"
             array
             (shift*          :pointer)
             ((length shift) :int)
             (axis!           :int)))))
      (sequence
       (let ((shift (etypecase shift
                      (integer
                       (make-array (length axis!)
                                   :initial-element shift
                                   :element-type    '(signed-byte 32)))
                      (sequence
                       (assert (cl:= (length shift) (length axis!)))
                       (make-array (length axis!)
                                   :initial-contents shift
                                   :element-type     '(signed-byte 32))))))
         (with-pointer-to-vector-data (shift* shift)
           (with-pointer-to-vector-data (axes* axis!)
             (with-mlx-op "mlx_roll_axes"
               array
               (shift*          :pointer)
               ((length shift) :int)
               (axes*           :pointer)
               ((length axis!)  :int))))))))

;; at-least-*d
;; (op ELEM &rest MORE-ELEM)
(with-op-template (op cffi docs
                   (elem "input array")
                   (more-elem "other more input array"))
    `(defun ,op (elem &rest more-elem)
       ,(apply #'gen-doc docs)
       (flet ((f (x) (with-mlx-op ,(sconc "mlx_" cffi) x)))
         (mapcar #'f (cons elem more-elem))))
  ((at-least-1d "atleast_1d")
   "Convert all ELEM and MORE-ELEM to have at least one dimension. ")
  ((at-least-2d "atleast_2d")
   "Convert all ELEM and MORE-ELEM to have at least two dimension. ")
  ((at-least-3d "atleast_3d")
   "Convert all ELEM and MORE-ELEM to have at least three dimension. "))

;; TEST: #atleast
(defmlx-func atleast (array &rest more-arrays-&key-dim)
  "Convert all ARRAY to have at least NDIM dimension. "
  :syntax "(atleast ARRAYS... &key (dim 1))"
  :parameters ((arrays "input arrays")
               (dim    "ensure all ARRAYS are at least DIM dims"))
  (multiple-value-bind (more-arrays keys)
      (split-args-keys more-arrays-&key-dim)
    (let ((dim* (num<- (getf keys :dim 1))))
      (ecase dim*
        (1 (apply #'at-least-1d array more-arrays))
        (2 (apply #'at-least-2d array more-arrays))
        (3 (apply #'at-least-3d array more-arrays))))))

;; TEST: #expand-dims
(defmlx-method expand-dims (array (axis integer) &rest axes)
  "Add a size one dimension at the given AXES."
  :return "`mlx-array' with inserted dimensions."
  :parameters ((array "input array")
               (axes  "index of the inserted dimensions"))
  (if (endp axes)
      (with-mlx-op "mlx_expand_dims"
        array
        (axis :int))
      (let ((axes (vec<- (cons axis axes))))
        (with-pointer-to-vector-data (axes* axes)
          (with-mlx-op "mlx_expand_dims_axes"
            array
            (axes*         :pointer)
            ((length axes) :size))))))

;; TEST: #reshape
(defmlx-method reshape (array (shape t)
                        &aux (shape! (shape<- shape)))
  "Reshape an array while preserving the size."
  :return "reshaped `mlx-array'."
  :parameters ((array "input array")
               (shape "shape specification"))
  (with-pointer-to-vector-data (shape* shape!)
    (with-mlx-op "mlx_reshape"
      array
      (shape*         :pointer)
      ((length shape!) :size))))

;; TEST: #swap-axes
(defmlx-method swap-axes (array (axis1 t) (axis2 t)
                          &aux
                            (axis1! (axis<- axis1))
                            (axis2! (axis<- axis2)))
  "Swap two axes of an ARRAY."
  :return "swaped `mlx-array'."
  :parameters ((array "input array")
               (axis1 "specific axis index to swap with AXIS2")
               (axis2 "specific axis index to swap with AXIS1"))
  (with-mlx-op "mlx_swapaxes"
    array
    (axis1! :int)
    (axis2! :int)))

;; TEST: #transpose
(defmlx-method transpose (array &key (axes nil axes?) (axis nil axis?)
                          &aux (axes! (cond (axes? (axes<- axes))
                                            (axis? (axes<- axis))
                                            (t     nil))))
  "Transpose the dimensions of the ARRAY."
  :return "transposed `mlx-array'"
  :parameters ((axes "the source axis for each axis in the new array.
The given AXES should be a list of axis as a reorder of ARRAY's axes.
By default it is to reverse the axes like (reverse (shape array)). ")
               (axis "alias of AXES"))
  :dev-note "This is like permutation of `swap-axes'. "
  (etypecase axes!
    (null
     (with-mlx-op "mlx_transpose"
       array))
    ((simple-array (signed-byte 32))
     (with-pointer-to-vector-data (axes* axes!)
        (with-mlx-op "mlx_transpose_axes"
          array
          (axes*          :pointer)
          ((length axes!) :int))))
    (integer (error "Expecting a reorder of array axes, but got ~A. "
                    (or axes axis)))))

;; TEST: #moveaxis
(defmlx-method moveaxis (array (from integer) (to integer))
  "Move an axis to a new position."
  :parameters ((from "source axis")
               (to   "destination axis"))
  :methods (((array (from mlx-array) to)
             (moveaxis array (the integer (num<- from)) to))
            ((array from (to mlx-array))
             (moveaxis array from (the integer (num<- to)))))
  (with-mlx-op "mlx_moveaxis"
    array
    (from :int)
    (to   :int)))

;; TEST: #flatten
(defmlx-method flatten (array &key (start 0 start?) (stop -1 stop?)
                        &aux
                          (start*  (if start? (axis<- start) start))
                          (stop*   (if stop?  (axis<- stop)  stop)))
  "Flatten ARRAY between START axis and END axis. "
  :return "flattend `mlx-array'."
  :parameters ((start "the first dimension to flatten (default 0)")
               (stop  "the last dimension to flatten (default -1)"))
  (declare (type integer start* stop*))
  (with-mlx-op "mlx_flatten"
    array
    (start* :int)
    (stop*  :int)))

;; TEST: #unflatten
(defmlx-method unflatten (array (axis integer) (shape t)
                          &aux (shape! (shape<- shape)))
  "Unflatten an axis of an array to a shape."
  :parameters ((array "input array")
               (axis  "axis to unflatten")
               (shape "shape to unflatten to
Note: at most one SHAPE entry can be -1 in which case the corresponding
size will be inferred. "))
  :note "See also `squeeze'. "
  (with-pointer-to-vector-data (shape* shape!)
    (with-mlx-op "mlx_unflatten"
      array
      (axis            :int)
      (shape*          :pointer)
      ((length shape!) :size))))

;; TODO: where


;;;; Conc and Split `mlx-array'

;; TEST: #concat
(defmlx-func concat (array &rest more-arrays-&key-axis)
  "Concatenate a sequence of `mlx-array' along the given AXIS."
  :syntax "(concat ARRAYS... &key (AXIS 0))"
  :parameters ((arrays "input arrays")
               (axis   "axis to concatenate along (default 0)
if AXIS is `nil', will concat as flattened arrays. "))
  (multiple-value-bind (more-arrays plist)
      (split-args-keys more-arrays-&key-axis)
    (let ((axis* (axis<- (getf plist :axis 0))))
      (with-array-vector<-sequence (arrays (cons array more-arrays))
        (if axis*
            (with-mlx-op "mlx_concatenate_axis"
              (arrays :pointer)
              (axis*  :int))
            (with-mlx-op "mlx_concatenate"
              (arrays :pointer)))))))

;; TEST: #stack
(defmlx-func stack (array &rest more-arrays-&key-axis)
  "Stack ARRAYS on AXIS. "
  :syntax "(stack arrays... &key (axis nil))"
  :parameters ((arrays "arrays to be stacked on AXIS")
               (axis   "axis to stack (default 0)"))
  (multiple-value-bind (arrays plist)
      (split-args-keys more-arrays-&key-axis)
    (destructuring-bind (&key (axis 0)) plist
      (let ((axis (axis<- axis)))
        (with-array-vector<-sequence (vec (cons array arrays))
          (if axis
              (with-mlx-op "mlx_stack_axis"
                (vec  :pointer)
                (axis :int))
              (with-mlx-op "mlx_stack"
                (vec  :pointer))))))))

;; TEST: #tile
(defmlx-method tile (array &rest repeats
                     &aux (repeat! (vec<- repeats)))
  "Construct an array by repeating ARRAY the number of times given by REPEAT."
  :syntax "(tile ARRAY REPEATS...)"
  :parameters ((array   "input array")
               (repeats "number of times to repeat ARRAY along each axis"))
  (with-pointer-to-vector-data (reps* repeat!)
    (with-mlx-op "mlx_tile"
      array
      (reps*            :pointer)
      ((length repeat!) :size))))

;; TEST: #repeat
(defmlx-method repeat (array (repeats integer) &key axis)
  "Repeat an array along a specified axis."
  :parameters ((repeats "repetitions for each element")
               (axis    "axis in which to repeat the array along.
if unspecified it uses the flattened array of the input
and repeats along axis 0
"))
  :methods (((array (repeats mlx-array) &key axis)
             (repeat array (the integer (num<- repeats)) :axis axis)))
  (declare (type (cl:or integer null) axis))
  (if axis
      (with-mlx-op "mlx_repeat_axis"
        array
        (repeats :int)
        (axis    :int))
      (with-mlx-op "mlx_repeat"
        array
        (repeats :int))))

;; TEST: #split
(defmlx-method split (array (split-or-indices sequence) &key (axis 0 axis?)
                      &aux (axis! (if axis? (axis<- axis) axis)))
  "Split an array along a given axis."
  :parameters ((split-or-indices "specify how to split ARRAY
 + integer for how many subarray to be splited
 + sequence of integer for each subarray size")
               (axis "axis to split (default 0)"))
  :methods ((((arr mlx-array) (num-splits integer) &key (axis 0 axis?)
              &aux (axis! (if axis? (axis<- axis) axis)))
             (declare (type (integer 0) num-splits)
                      (type integer axis!))
             (with-mlx-op ("mlx_split" :alloc mlx_vector_array_new
                                       :wrap  wrap-as-mlx-array-list)
               arr
               (num-splits :int)
               (axis!      :int)))
            ((arr (num-splits integer) &key (axis 0))
             (split (mlx-array arr) num-splits :axis axis)))
  (declare (type integer axis!))
  (with-foreign<-sequence (indices* split-or-indices :int len)
    (with-mlx-op ("mlx_split_sections" :alloc mlx_vector_array_new
                                       :wrap  wrap-as-mlx-array-list)
      array
      (indices* :pointer)
      (len      :int)
      (axis!    :int))))

;; argpartition, partition
(with-op-template (op cffi docs
                   (nth "element index at NTH position in the output will give the sorted position
All indices before NTH position will be of elements less or equal to
the element at the NTH index and all indices after will be of elements
greater or equal to the element at the NTH index")
                   (axis "optional axis to partition over (default -1, last axis)
  + `nil': partition over the flattened array
  + integer index: partition over axis"))
    `(defmlx-method ,op (array (nth integer) &key (axis -1 axis?)
                         &aux (axis* (if axis? (axis<- axis nil) axis)))
       ,@docs
       :methods (((array (nth mlx-array) &key (axis -1))
                  (,op array (the integer (num<- nth)) :axis axis)))
       (declare (type (cl:or null integer) axis*))
       (if axis*
           (with-mlx-op ,(sconc "mlx_" cffi "_axis")
             array
             (nth   :int)
             (axis* :int))
           (with-mlx-op ,(sconc "mlx_" cffi)
             array
             (nth  :int))))
  (argpartition "Returns the indices that partition the array.")
  (partition    "Returns a partitioned copy of ARRAY such that smaller NTH elements are first."))


;;;; Sorting Operations

;; TEST: #sort
;; (op ARRAY &key axis)
(with-op-template (op cffi docs
                   (axis "optional axis to sort over (default -1 for last axis)
  + `nil' sorts over the flattened array
  + integer index axis to sort over"))
    `(defmlx-method ,op (array &key (axis -1 axis?)
                         &aux (axis* (if axis? (axis<- axis) axis)))
       ,@docs
       (declare (type (cl:or null integer) axis*))
       (if axis*
           (with-mlx-op ,(sconc "mlx_" cffi "_axis")
             array
             (axis* :int))
           (with-mlx-op ,(sconc "mlx_" cffi)
             array)))
  (argsort "Return the indices that sort the array along AXIS. ")
  (sort    "Sort the array along AXIS. "))

;; TEST: #topk
(defmlx-method topk (array (k integer) &key (axis nil axis?) &aux (axis* (axis<- axis)))
  "Return K largest elements in ARRAY along given AXIS. "
  (declare (type (cl:or null integer) axis*))
  (if axis*
      (with-mlx-op "mlx_topk_axis"
        array
        (k     :int)
        (axis* :int))
      (with-mlx-op "mlx_topk"
        array
        (k :int))))


;;;; Conv Operations

(defun parse-pad-width (pad-width dim)
  "Parse PAD-WIDTH as normalized input form.

Rule:
+ integer: pad-width same for all DIM
+ (integer . integer):
+ (...)"
  (flet ((parse-single (pad-width)
           (etypecase pad-width
             (integer                (cons pad-width pad-width))
             ((cons integer integer) pad-width))))
    (etypecase pad-width
      (integer
       (loop :with pad := (cons pad-width pad-width)
             :repeat dim :collect pad))
      ((cons integer integer)
       (loop :repeat dim :collect pad-width))
      (sequence (map 'list #'parse-single pad-width)))))

(let ((const (foreign-string-alloc "constant"))
      (edge  (foreign-string-alloc "edge")))
  (defmlx-method pad (array
                      &optional (pad-width 1)
                      &key (mode :constant) (fill nil fill?)
                      &aux
                        (dim       (dim array))
                        (pw        (parse-pad-width pad-width dim))
                        (pad-value (mlx-array fill))
                        (mode*     (ecase (if (cl:and fill? fill) :const mode)
                                     ((:const :constant) const)
                                     ((:edge :replicate) edge))))
    "Pad an array with a constant value. "
    :parameters ((pad-width "number of padded values to add to the edges of each axis (default 1):
 + integer: ((PAD-WIDTH . PAD-WIDTH) ... )
   all axes are extended by the same number on each side
 + sequence of integer: (pad-width ...)
   all axis are extended by the same (before . after) = PAD-WIDTH
 + ((before_0 . after_0) (before_1 . after_1) ... (before_N . after_N))
   to pad over axis, each axis are defined like above

see `mlx::parse-pad-width'. ")
                 (mode "padding mode (default `:constant')
 + `:constant', `:const': pads with constant value (see FILL)
 + `:edge', `:replicate': pads with edge values of ARRAY

If setting FILL, MODE will be forced set as `:constant'. ")
                 (fill "value to fill the paddings (default `0')"))
    :examples (("pad size 2 on both sides of every axis"
                (pad array 2)
                "...")
               ("pad size 2 before and size 3 after for every axis"
                (pad array '(2 . 3))
                "...")
               ("pad axis 0 by 1, pad axis 1 by 2"
                (pad array '(1 2))
                "...")
               ("pad axis 0 before by 1, after by 2, pad axis 1 before by "))
    (let* ((size (cl:* dim (foreign-type-size :int))))
      (with-foreign-pointer (axes* size)
        (with-foreign-pointer (low* size)
          (with-foreign-pointer (high* size)
            (loop :for axis :from 0
                  :for (low . high) :in pw
                  :do (setf (mem-aref axes* :int axis) axis
                            (mem-aref low*  :int axis) low
                            (mem-aref high* :int axis) high))
            (with-mlx-op "mlx_pad"
              array
              (axes* :pointer)
              (dim   :size)
              (low*  :pointer)
              (dim   :size)
              (high* :pointer)
              (dim   :size)
              pad-value
              (mode* :string))))))))

;; TODO: #mlx-cl
;; use `pad' for padding, which is eazy to use
(with-op-template (op cffi docs
                   (array    "input `mlx-array'
 + (...) shape: will reshape it into 1 ... 1 form
 + (N ... C) shape:
   + N: batch size
   + C: channels
")
                   (weight   "weight `mlx-array' of shape
 + (...) shape: will reshape it into (1 ... 1) form
 + (C_out ... C_in) shape:
   + C_out: output channels
   + C_in: input channels
")
                   (stride   "how far the WEIGHT kernel moves (default 1)

Example:
+ stride=1: dense sampling
+ stride=2: skip one")
                   (padding  "padding of ARRAY (default 0)")
                   (dilation "how far apart the kernel elements are spaced (default 1)

Example:
+ dilation=1: dense sampling, taking input ARRAY as contiguous
+ dilation=2: skip, taking input ARRAY like #_#_#")
                   (groups   "divide input channels and output channels indepent groups (default 1)

Example:
+ group=1: all input channels are connected to output channels
+ group=C_in: one kernel per input channel
+ 1 < group < C_in: split input channels into n GROUP subgroups"))
    (let* ((dim (getf (rest docs) :dim))
           (strides   (loop :repeat (1- dim) :collect (list (gensym "STRIDE")   'stride)))
           (paddings  (loop :repeat (1- dim) :collect (list (gensym "PADDING")  'padding)))
           (dilations (loop :repeat (1- dim) :collect (list (gensym "DILATION") 'dilation))))
      `(defmlx-method ,op (array weight &key (stride 1) (padding 0) (dilation 1) (groups 1))
         ,@docs
         (declare (type (cl:or sequence integer) stride padding dilation)
                  (type integer groups))
         (destructuring-bind (stride &optional ,@strides) (lst<- stride)
           (destructuring-bind (padding &optional ,@paddings) (lst<- padding)
             (destructuring-bind (dilation &optional ,@dilations) (lst<- dilation)
               (with-mlx-op ,(sconc "mlx_" cffi)
                 array
                 weight
                 (stride :int)
                 ,@(loop :for (stride) :in strides
                         :collect `(,stride :int))
                 (padding :int)
                 ,@(loop :for (padding) :in paddings
                         :collect `(,padding :int))
                 (dilation :int)
                 ,@(loop :for (dilation) :in dilations
                         :collect `(,dilation :int))
                 (groups :int)))))))
  (conv1d "1-D convolution over an input with several channels" :dim 1)
  (conv2d "2-D convolution over an input with several channels" :dim 2)
  (conv3d "3-D convolution over an input with several channels" :dim 3))

;; TODO: #mlx-cl #missing
;; conv-transpose1d...

;; (defmlx-method conv (input weight &key (stride 1) (padding 0) (dialation 1) (groups 1))
;;   "")


;;;; Cummulative Operations

;; (op ARRAY &key AXIS REVERSE INCLUDSIVE)
(with-op-template (op cffi docs
                   (axis       "optional axis to compute the cumulative result
if unspecified or `nil', apply on the flattened array")
                   (reverse    "if cumulative in reverse")
                   (includsive "whether the Nth element of output include the Nth element of input (default t)"))
    `(defmlx-method ,op (array &key (axis nil axis?) (reverse nil) (includsive t)
                         &aux
                           (axis*       (if axis? (num<- axis) nil))
                           (reverse!    (bool<- reverse))
                           (includsive! (bool<- includsive)))
       ,@docs
       (declare (type (cl:or null integer) axis*))
       (let ((array (if axis* array (reshape array '(-1))))
             (axis  (cl:or axis* 0)))
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


;;;; Einsum

(defmlx-method einsum ((subscripts string) &rest operands)
  "Perform the Einstein summation convention on the operands."
  :parameters ((subscripts "Einstein summation convention equation")
               (operands   "input arrays
  + if given with zero operands, return a function of lambda list

        (lambda (&rest operands) ...)
  + if given non-zero operands, calculate einsum on operands"))
  (if (endp operands)
      (lambda (&rest operands) (einsum subscripts operands))
      (with-foreign-string (str subscripts)
        (with-array-vector<-sequence (vec operands)
          (with-mlx-op "mlx_einsum"
            (str :string)
            (vec :pointer))))))


;;;; Better for Dev

(defgeneric copy (obj)
  (:documentation "Return a copy of OBJ. ")
  (:method (obj)
    "By default, return OBJ itself without coping. "
    obj)
  (:method ((list list))
    "Recursively copying a LIST. "
    (labels ((copy-list* (list)
               (if (atom list)
                   (copy list)
                   (mapcar #'copy-list list))))
      (copy-list* list)))
  (:method ((array array))
    "Displace new array to the old ARRAY. "
    (make-array (array-dimensions array)
                :element-type (array-element-type array)
                :displaced-to array
                :adjustable   (adjustable-array-p array)))
  (:method ((arr mlx-array))
    "Call mlx_copy. "
    (with-mlx-op "mlx_copy" arr)))

(defmlx-method as-strided
    (array &key shape strides (offset 0)
     &aux
       (shape!   (if shape
                     (shape<- shape)
                     (shape<- (shape array))))
       (strides! (if strides
                     (shape<- strides)
                     (loop :with shape := (cl:reverse (shape array))
                           :with strides := (make-array (length shape)
                                                        :element-type '(signed-byte 32))
                           :for idx :from 0
                           :for elem :in shape
                           :for prod := elem :then (cl:* elem prod)
                           :do (setf (aref strides idx) prod)
                           :finally (return strides)))))
  "Create a view into the array with the given shape and strides."
  :return "`mlx-array' which is the strided view of the input."
  :parameters ((array "input array")
               (shape "shape of resulting array (default `nil')
  + integer index: shape of the 1-d array
  + sequence of integer index: shape of target array
  + `nil': default to (shape array)")
               (strides "strides of the resulting array
  + integer index: shape of the 1-d array
  + sequence of integer index: shape
  + `nil': default to the reverse exclusive cumulative product of (shape ARRAY)")
               (offset "skip that many elements from the beginning of the input array (default 0)"))
  :note "
This function should be used with caution as it changes
the shape and strides of the array directly. This can lead to the
resulting array pointing to invalid memory locations which can
result into crashes."
  (declare (type (integer 0) offset)
           (type (simple-array (signed-byte 32)) shape! strides!))
  (with-pointer-to-vector-data (shape* shape!)
    (with-pointer-to-vector-data (strides* strides!)
      (with-mlx-op "mlx_as_strided"
        array
        (shape*            :pointer)
        ((length shape!)   :int)
        (strides*          :pointer)
        ((length strides!) :int)
        (offset            :size)))))

(defmlx-method broadcast-to (array (shape t)
                             &aux (shape! (vec<- shape)))
  "Broadcast an array to the given shape. "
  :return "`mlx-array' with the new shape."
  :note "The broadcasting semantics are the same as Numpy."
  :parameters ((array "input array")
               (shape "shape to broadcast to"))
  :method (((array (shape mlx-array))
            (broadcast-to array (shape shape))))
  (declare (type (simple-array (signed-byte 32)) shape!))
  (with-pointer-to-vector-data (shape* shape!)
    (with-mlx-op "mlx_broadcast_to"
      array
      (shape*          :pointer)
      ((length shape!) :size))))

(defmlx-method contiguous (array &key (major :row))
  "Force an ARRAY to be MAJOR contiguous. Copy if necessary."
  :return "row or col contiguous output."
  :parameters ((array "input array")
               (major "`:row' or `:col' for row major or col major (default `:row')"))
  (declare (type (member :row :col :column) major))
  (with-mlx-op "mlx_contiguous"
    array
    ((if (cl:eq major :row) nil t) :bool)))

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
  ((as-type "astype")
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

;; (setf (dtype ...) ...)
(with-op-template (op cffi docs)
   `(defgeneric (setf ,op) (dtype array)
      (:documentation ,(apply #'gen-doc docs))
      (:method (dtype (array mlx-array))
        (let ((arr (as-type array (ensure-mlx-dtype dtype))))
          (%steal-mlx-array-pointer arr array)
          array)))
  (mlx-dtype "Set ARRAY's data mlx-dtype as DTYPE. ")
  (dtype     "Set ARRAY's data mlx-dtype as DTYPE. "
             :note "See also (setf mlx-dtype). "))

;; TODO: #mlx-cl
;; scatter_*?
;; segmented_mm
;; not sure what to do with them, how to use them
(defmlx-method scatter (array (indices sequence) updates (axes sequence))
  "Scatter updates to the given indices."
  :note "
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
given location in a."
  :examples (("update subset of array"
              (scatter (zeros '(4 4))
                       (list (mlx-array '(2)))
                       (reshape (arange 1 3 :dtype :float32) '(1 1 2))
                       0)
              (mlx-array #2A((0 0 0 0)
                             (0 0 0 0)
                             (1 2 0 0)
                             (0 0 0 0))
                         :dtype :float32)))
  :methods (((array (indices sequence) updates (axes integer))
             (scatter array indices updates (list axes))))
  (with-array-vector<-sequence (vec indices)
    (with-foreign<-sequence (axes* axes :int len)
      (with-mlx-op "mlx_scatter"
        array
        (vec     :pointer)
        (updates mlx-array)
        (axes*   :pointer)
        (len     :int)))))

(defmlx-method put-along-axis (array indices values &key (axis nil axis?)
                               &aux (axis* (if axis? (axis<- axis) axis)))
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
  (if axis*
      (with-mlx-op "mlx_put_along_axis"
        array
        indices
        values
        (axis* :int))
      (let ((shape (shape array)))
        (reshape (put-along-axis (reshape array '(-1)) indices values :axis 0) shape))))

;; TODO: #mlx-cl #missing
;; (defgeneric depends (inputs dependencies))

(defmethod equal ((arr mlx-array) elem)
  (equal (lisp<- arr) elem))

(defmethod equal (elem (arr mlx-array))
  (equal (lisp<- arr) elem))

(defmethod equal ((arr1 mlx-array) (arr2 mlx-array))
  "Test if ARR1 is equal to ARR2.

See `*nan-equal-p*' for NaN equal to other values. "
  (lisp<- (with-mlx-op "mlx_array_equal"
            arr1 arr2 (*nan-equal-p* :bool))))

(defmethod equal ((arr mlx-array) (num number))
  (cl:and (null (shape arr))
          (cl:= (lisp<- arr) num)))

(defmethod equal ((num number) (arr mlx-array))
  (cl:and (null (shape arr))
          (cl:= (lisp<- arr) num)))

(defmethod equal ((arr1 mlx-array) (arr2 array))
  (equal (lisp<- arr1) arr2))

(defmethod equal ((arr2 array) (arr1 mlx-array))
  (equal (lisp<- arr1) arr2))

;; TODO: #mlx-cl #missing
;; what is gather?
;; (defgeneric gather (array indices axes slice))
;; (defgeneric gather-mm (array1 array2 lhs-indices rhs-indices sorted-indices))
;; (defgeneric gather-qmm (array weight scales biases lhs-indices rhs-indices transpost group-size bits sorted-indices))

;;;; ops.lisp ends here
