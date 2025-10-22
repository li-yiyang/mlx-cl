;;;; array.lisp --- Array for MLX-C

(in-package :mlx-cl)


;;;; Lowlevel

(defctype mlx-array :pointer)

;; MLX-DTYPE

;; +mlx-dtypes+, +supported-mlx-dtypes+
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter +mlx-dtypes+
    '(:bool
      :uint8
      :uint16
      :uint32
      :uint64
      :int8
      :int16
      :int32
      :int64
      :float16
      :float32
      :float64
      :bfloat16
      :complex64)
    "MLX dtype keywords. ")
  (defparameter +supported-mlx-dtypes+
    '(:bool
      :uint8
      :uint16
      :uint32
      :uint64
      :int8
      :int16
      :int32
      :int64
      :float32
      :float64)
    "MLX-CL supported MLX-DTYPES. "))

(macrolet ((define-mlx-dtype ()
             `(progn
                (defcenum mlx-dtype ,@+mlx-dtypes+)
                (deftype  mlx-dtype ()
                  "Names of data type for MLX-DTYPE.

Use `mlx::lisp-type<-mlx-dtype' to convert MLX-DTYPE names to
lisp type specification.

Use `mlx::cffi-type<-mlx-dtype' to convert MLX-DTYPE names to
CFFI type specification.

Use `mlx::ensure-mlx-dtype' to ensure given type specification
as MLX-DTYPE. "
                  '(member ,@+mlx-dtypes+)))))
  (define-mlx-dtype))

;; *default-mlx-{int,uint,float}-dtype*
;; *default-mlx-dtype*
;; {lisp-type,cffi-type}<-mlx-dtype
;; mlx-dtype<-{lisp-type,cffi-type}
(eval-when (:compile-toplevel :load-toplevel :execute)
  (declaim
   (type (member :int8 :int16 :int32 :int64)     *default-mlx-int-dtype*)
   (type (member :uint8 :uint16 :uint32 :uint64) *default-mlx-uint-dtype*)
   (type (member :float32 :float64)              *default-mlx-float-dtype*)
   (type mlx-dtype                               *default-mlx-dtype*))

  (defparameter *default-mlx-int-dtype*  :int32
    "Default MLX-DTYPE for `integer'. ")
  (defparameter *default-mlx-uint-dtype* :uint32
    "Default MLX-DTYPE for `unsigned-integer'. ")
  (defparameter *default-mlx-float-dtype* :float32
    "Default lisp float to MLX float dtype. ")
  (defparameter *default-mlx-dtype*      :float32
    "Default `mlx-dtype'. ")

  ;; TODO: :float16, :bfloat16 :complex64
  (defun lisp-type<-mlx-dtype (dtype)
    "Convert MLX-DTYPE to lisp type specification. "
    (declare (type mlx-dtype dtype))
    (ecase dtype
      (:bool      'boolean)
      (:uint8     '(unsigned-byte 8))
      (:uint16    '(unsigned-byte 16))
      (:uint32    '(unsigned-byte 32))
      (:uint64    '(unsigned-byte 64))
      (:int8      '(signed-byte   8))
      (:int16     '(signed-byte   16))
      (:int32     '(signed-byte   32))
      (:int64     '(signed-byte   64))
      (:float32   'single-float)
      (:float64   'double-float)
      (:complex64 'complex)))

  (defun cffi-type<-mlx-dtype (dtype)
    "Convert MLX-DTYP to CFFI type specification. "
    (declare (type mlx-dtype dtype))
    (ecase dtype
      (:float32 :float)
      (:float64 :double)
      ((:bool
        :uint8 :uint16 :uint32 :uint64
        :int8  :int16  :int32  :int64)
       dtype)))

  (defun mlx-dtype<-cffi-type (cffi-type)
    "Turn CFFI-TYPE into mlx-dtype. "
    (ecase cffi-type
      (:int    *default-mlx-int-dtype*)
      (:uint   *default-mlx-uint-dtype*)
      (:float  :float32)
      (:double :float64)
      ((:bool
        :uint8 :uint16 :uint32 :uint64
        :int8  :int16  :int32  :int64)
       cffi-type)))

  (defun mlx-dtype<-lisp-type (lisp-type)
    "Turn LISP-TYPE into mlx-dtype. "
    (macrolet ((esubtype* (&body cond)
                 `(cond ,@(loop for (type* res) in cond
                                collect `((subtypep lisp-type ',type*) ,res))
                        (t (error "Failed to determine ~A as `mlx-dtype'. "
                                  lisp-type)))))
      (esubtype*
       (boolean            :bool)
       (single-float       :float32)
       (ratio              *default-mlx-float-dtype*)
       (float              *default-mlx-float-dtype*)
       ((unsigned-byte 8)  :uint8)
       ((unsigned-byte 16) :uint16)
       ((unsigned-byte 32) :uint32)
       ((unsigned-byte 64) :uint64)
       (unsigned-byte      *default-mlx-uint-dtype*)
       ((signed-byte 8)    :int8)
       ((signed-byte 16)   :int16)
       ((signed-byte 32)   :int32)
       ((signed-byte 64)   :int64)
       (integer            *default-mlx-int-dtype*)))))

(defun minimal-super-type (type1 type2)
  "Return the minimal common type of TYPE1 and TYPE2. "
  (flet ((float<- (type)
           (cond ((subtypep type 'single-float) 'single-float)
                 ((subtypep type 'float)        'float)
                 (t (error
                     "Cannot convert ~A as `mlx-dtype' compatiable type. "
                     type)))))
    (if (cl:and (subtypep type1 'boolean)
                (subtypep type2 'boolean))
        'boolean
        (let ((int1 (subtypep type1 'integer))
              (int2 (subtypep type2 'integer)))
          (if int1
              (if int2
                  ;; type1 and type2 are all integer
                  (if (subtypep type1 type2) type2 type1)
                  ;; type1 integer, type2 not integer
                  (float<- type2))
              (if int2
                  (float<- type1)
                  (let ((f1 (float<- type1))
                        (f2 (float<- type2)))
                    (if (subtypep f1 f2) f2 f1))))))))

(defun ensure-mlx-dtype (dtype)
  "Ensure DTYPE as `mlx-dtype'.

Rules:
+ MLX-DTYPE  -> MLX-DTYPE
+ CFFI types -> MLX-DTYPE
+ lisp
  + single-float -> :float32
  + double-float -> :float64
  + integer, signed-integer -> calculate size and then :int?
      or `*default-mlx-int-dtype*' as fallback
  + unsigned-integer        -> calculate size and then :uint?
      or `*default-mlx-uint-dtype*' as fallback

Dev Note:
Use `mlx::mlx-dtype<-lisp-type' and `mlx::mlx-dtype<-cffi-type'
if you know how to convert the DTYPE. "
  (etypecase dtype
    (mlx-dtype dtype)
    (keyword   (mlx-dtype<-cffi-type dtype))
    ((cl:or symbol list) (mlx-dtype<-lisp-type dtype))))

(flet ((int<-    (n) (truncate n))
       (float<-  (f) (coerce f 'single-float))
       (double<- (f) (coerce f 'double-float))
       (bool<-   (b) (cl:and b t)))
  (defun %mlx-dtype-coerce (dtype)
    "Return a converter to MLX DTYPE. "
    (declare (type mlx-dtype dtype))
    (ecase dtype
      ((:uint8 :uint16 :uint32 :uint64
        :int8  :int16  :int32  :int64)
       #'int<-)
      (:float32 #'float<-)
      (:float64 #'double<-)
      (:bool    #'bool<-)))

  (defun mlx-dtype-coerce (value dtype)
    "Coerce VALUE to MLX DTYPE.

Dev Note:
Use `%mlx-dtype-coerce' for massive data convertion. "
    (declare (type number    value)
             (type mlx-dtype dtype))
    (ecase dtype
      ((:uint8 :uint16 :uint32 :uint64
        :int8  :int16  :int32  :int64)
       (int<- value))
      (:float32 (float<-  value))
      (:float64 (double<- value))
      (:bool    (bool<-   value)))))

;; size_t mlx_dtype_size(mlx_dtype dtype);
(defcfun (mlx_dtype_size "mlx_dtype_size") :size
  (dtype mlx-dtype))

;;; Allocation and Deallocation

;; mlx_array mlx_array mlx_array_new();
(defcfun (mlx_array_new "mlx_array_new") mlx-array)

;; int mlx_array_free(mlx_array arr);
(defun mlx_array_free (mlx-array)
  (ensure-success "mlx_array_free"
   mlx-array mlx-array))

;; mlx_array mlx_array_new_data(
;;   const void* data, const int* shape, int dim, mlx_dtype dtype);
(defcfun (mlx_array_new_data "mlx_array_new_data") mlx-array
  (data  :pointer)
  (shape :pointer)
  (dim   :int)
  (dtype mlx-dtype))

;; mlx_array mlx_array_new_{ name } ({ type } val);
(macrolet ((mlx_array_new* (&rest name-type)
             `(progn
                ,@(loop :for (name type) :in name-type
                        :collect `(defcfun ,(intern* "MLX_ARRAY_NEW_" name) mlx-array
                                    (val ,type))))))
  (mlx_array_new*
   (:bool    :bool)
   (:float32 :float)
   (:float64 :double)
   (:int     :int)))

;;; Convert to Lisp

;; int mlx_array_item_<dtype> (<dtype>* res, const mlx_array arr);
;; <dtype>* mlx_array_data_<dtype> (const mlx_array arr);
(macrolet ((mlx_array_item/data_* ()
             `(progn
                ,@(loop :for dtype :in +supported-mlx-dtypes+
                        :collect `(defcsetfun ,(format nil "mlx_array_item_~(~A~)" dtype)
                                      ,(cffi-type<-mlx-dtype dtype)
                                    mlx-array mlx-array)
                        :collect `(defcfun (,(intern* 'mlx_array_data_ dtype)
                                            ,(format nil "mlx_array_data_~(~A~)" dtype))
                                      :pointer
                                    (mlx-array mlx-array))))))
  (mlx_array_item/data_*))

;;; Eval

(defun mlx_array_eval (mlx-array)
  (ensure-success "mlx_array_eval"
    mlx-array mlx-array))


;;;; Highlevel

;;; Conditions

(define-condition mlx-array-error (mlx-runtime-error)
  ((array :initarg :array))
  (:report (lambda (cond stream)
   (format stream "MLX Array Error on ~A: ~%~A"
                     (slot-value cond 'array)
                     (slot-value cond 'message)))))

(define-condition mlx-axis-error (mlx-array-error)
  ((axis :initarg :axis))
  (:report (lambda (cond stream)
   (format stream "MLX Axis Error on ~A: ~%~A~%(axis=~D)"
                     (slot-value cond 'array)
                     (slot-value cond 'message)
                     (slot-value cond 'axis)))))

;;; Class

(defclass mlx-array (mlx-object) ()
  (:documentation
   "An N-dimensional array object, on which computations are performed.
The computation on MLX-ARRAY is lazy. "))

(defun wrap-as-mlx-array (pointer)
  (let ((array (make-instance 'mlx-array :pointer pointer)))
    (tg:finalize array (lambda () (mlx_array_free pointer)))
    array))

(defun mlx-array-p (object)
  "Test if OBJECT is `mlx-array' or not. "
  (typep object 'mlx-array))

(defun mlx-scalar-p (object)
  "Test if OBJECT is a `mlx-array' scalar.

Definition:
A `mlx-array' scalar is a `mlx-array' with `nil' shape. "
  (cl:and (mlx-array-p object)
          (null (shape object))))

;; int mlx_array_tostring(mlx_string* str, const mlx_array arr);
(defmethod string<- ((array mlx-array))
  "Convert ARRAY as string.
Note this would perform evaluation on ARRAY. "
  (with-mlx-string& (str str&)
    (ensure-success "mlx_array_tostring"
      :pointer  str&
      mlx-array (mlx-object-pointer array))
    (string<-mlx-string str)))


;;; Constants

;; Dev Note:
;; not sure why this would happen in SBCL, but using
;; float traps mask would avoid this.

#+sbcl
(sb-int:with-float-traps-masked (:overflow)
  (defparameter +mlx-true+
    (wrap-as-mlx-array (mlx_array_new_bool t))
    "True value of `mlx-array'. ")

  (defparameter +mlx-false+
    (wrap-as-mlx-array (mlx_array_new_bool nil))
    "False value of `mlx-array'. "))

#-sbcl
(progn
  (defparameter +mlx-true+
    (wrap-as-mlx-array (mlx_array_new_bool t))
    "True value of `mlx-array'. ")

  (defparameter +mlx-false+
    (wrap-as-mlx-array (mlx_array_new_bool nil))
    "False value of `mlx-array'. "))

;;; MLX-DTYPE

(defun mlx-dtype-size (dtype)
  "Return the size of DTYPE. "
  (mlx_dtype_size (ensure-mlx-dtype dtype)))

(defgeneric mlx-dtype (object)
  (:documentation
   "Return type of OBJECT as `mlx-dtype'.

See `mlx-cl::mlx-dtype<-lisp-type' for converting Lisp type
specification into `mlx-dtype'. ")
  (:method ((number number))
    (mlx-dtype<-lisp-type (type-of number)))
  (:method ((array array))
    "Return `mlx-dtype' of ARRAY.

If `array-element-type' of ARRAY is `t', will determine
the element type by the finding the minimal super type of
each element of ARRAY via `mlx::minimal-super-type'.

Otherwise, the `mlx-dtype' will be determined by the
`array-element-type'. "
    (let ((type (array-element-type array)))
   (if (cl:eq type t)
          (let ((size (reduce #'cl:* (array-dimensions array))))
   (if (zerop size) *default-mlx-dtype*
                (loop :with type := (type-of (row-major-aref array 0))
                      :for idx :from 1 :below size
                      :for elem := (row-major-aref array idx)
                      :do (setf type (minimal-super-type (type-of elem) type))
                      :finally (return (mlx-dtype<-lisp-type type)))))
          (mlx-dtype<-lisp-type type))))
  (:method ((list list))
    "Return `mlx-dtype' of ARRAY.

This will iterate over LIST recursively and find the
minimal super type of each element of LIST via
`mlx::minimal-super-type'.

Note that `()' would be considered as atom (boolean),
but if LIST is empty, the type of LIST would be "
    (labels ((type-of-list (list)
   ;; find the `minimal-super-type' of list elements
   (if (atom list) (type-of list)
                   (let ((list (mapcar #'type-of-list list)))
   (reduce #'minimal-super-type (rest list)
                             :initial-value (first list))))))
   (if (endp list) *default-mlx-dtype*
          (mlx-dtype<-lisp-type (type-of-list list)))))
  (:method ((arr mlx-array))
    (foreign-funcall
     "mlx_array_dtype"
     :pointer (mlx-object-pointer arr)
     mlx-dtype)))

(defalias dtype mlx-dtype
  "Alias of `mlx-dtype'.
Return `mlx-dtype' of given OBJECT. ")

;;; NDIM

(defgeneric dim (object)
  (:documentation
   "Return the dimension of OBJECT.

Examples:
+ ndim of scalar is 0
+ ndim of 1 dimensional array is 1
")
  (:method (object)
    "For Lisp OBJECT, ndim of OBJECT is (length (shape object)). "
    (length (shape object)))
  (:method ((array mlx-array))
    (foreign-funcall "mlx_array_ndim"
                     :pointer (mlx-object-pointer array)
                     :size)))

;; SHAPE

(defgeneric shape (object &key axis)
  (:documentation
   "Return the shape of OBJECT.

Parameter:
+ OBJECT: object to get the shape dimensions
+ AXIS:   integer of array axis dimsnsion,
  if not given, by default return all the shape on each axes as a list
  if given axis larger than OBJECT axis, will throw `mlx-axis-error'.

Examples:
+ scalar:                            shape of scalar is ()
+ empty array (1 dimensional array): shape of empty array is (0)
")
  (:method ((number number) &key axis)
    "Shape of NUMBER (as scalar) is (). "
    (declare (type (cl:or null (integer 0)) axis))
    (when axis
      (error 'mlx-axis-error :axis    axis
                             :array   number
                             :message (format nil "Shape of ~A (scalar) is (). "
                                              number))))
  (:method ((array array) &key axis)
    "See `array-dimensions' and `array-dimension'. "
    (declare (type (cl:or null integer) axis))
    (if axis
        (handler-case
            (if (cl:< axis 0)
                (array-dimension array (+ axis (length (array-dimensions array))))
                (array-dimension array axis))
          (error ()
            (error 'mlx-axis-error :axis    axis
                                   :array   array
                                   :message (format nil
                                                    "Shape of ~A is ~A. "
                                                    array
                                                    (array-dimensions array)))))
        (array-dimensions array)))
  (:method ((list list) &key axis)
    "Shape of LIST.

Shape would test on each axis of LIST to ensure they are
inconsist within same axis.

Example:

    (shape '((1 2) 3))    ; is inconsist
    (shape '((1 2) (3 4)) ; is consist

Note that shape of empty list () is (0);
shape of list of single null (nil) is (1),
where nil is considered as boolean.
"
    (declare (type (cl:or null integer) axis))
    (labels ((err ()
               ;; raise error for invalid shape
               (error 'mlx-array-error
                      :array list
                      :message (format nil "~S is in valid shape. " list)))
             (dim (lst)
               ;; lst : (a1 a2 ...) -> (length lst)
               ;; lst : (lst1 lst2 ...) -> (cons (lengsh lst) (dim lst1))
               (if (atom lst) (err)
                   (let ((first (first lst)))
                     (if (atom first) (list (length lst))
                         (let ((first-dim (dim first)))
                           (if (every (lambda (dim) (equal first-dim dim))
                                      (mapcar #'dim (rest lst)))
                               (cons (length lst) first-dim)
                               (err))))))))
      (let ((shape (if (endp list) '(0) (dim list))))
        (if axis
            (cl:or (if (cl:< axis 0)
                       (unless (cl:< (+ axis (length shape)) 0)
                         (nth (cl:+ axis (length shape)) shape))
                       (nth axis shape))
                   (error 'mlx-axis-error
                          :axis    axis
                          :array   list
                          :message (format nil
                                           "Shape of ~A is ~A. "
                                           list shape)))
            shape))))
  (:method ((array mlx-array) &key axis)
    (declare (type (cl:or null integer) axis))
    (if axis
        (foreign-funcall "mlx_array_dim"
                         :pointer (mlx-object-pointer array)
                         :int     axis
                         :size)
        (sequence<-foreign (foreign-funcall
                            "mlx_array_shape"
                            :pointer (mlx-object-pointer array)
                            :pointer)
                           :int (dim array)))))

(defgeneric size (object)
  (:documentation
   "Number of elements in the OBJECT. ")
  (:method (object)
    (reduce #'cl:* (shape object)))
  (:method ((number number))
    (declare (ignore number))
    1)
  (:method ((arr mlx-array))
    (foreign-funcall
     "mlx_array_size"
     mlx-array (mlx-object-pointer arr)
     :size)))

;;; Convert lisp value as `mlx-array'

(defgeneric mlx-array (val &key dtype)
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
  (:method (bool
            &key (dtype :bool)
            &aux (dtype! (ensure-mlx-dtype dtype)))
    (declare (type boolean bool)
             (type (member :bool) dtype!)
             (ignorable dtype!))
    (if bool +mlx-true+ +mlx-false+))
  (:method ((number number)
            &key (dtype (type-of number))
            &aux (dtype! (ensure-mlx-dtype dtype)))
    (wrap-as-mlx-array
     (ecase dtype!
       ((:uint8 :uint16 :uint32 :uint64
         :int8  :int16  :int32  :int64)
        (mlx_array_new_int (truncate number)))
       (:float32
        (mlx_array_new_float32 (coerce number 'single-float)))
       (:float64
        (mlx_array_new_float64 (coerce number 'double-float))))))
  (:method ((array array)
            &key (dtype  (mlx-dtype array))
            &aux (dtype! (ensure-mlx-dtype dtype)))
    (wrap-as-mlx-array
     (let ((coerce (%mlx-dtype-coerce dtype!)))
       (with-foreign<-array (array* array (cffi-type<-mlx-dtype dtype!) shape size
                             elem (funcall coerce elem))
         (with-foreign<-sequence (shape* shape :int dim)
           (mlx_array_new_data array* shape* dim dtype!))))))
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
  (:method ((arr mlx-array) &key dtype)
    (if dtype
        (as-type arr dtype)
        arr)))

(defmethod mlx-object-pointer (elem)
  (mlx-object-pointer (mlx-array elem)))

;;; Convert `mlx-array' to lisp values

(defgeneric lisp<- (array)
  (:documentation
   "Convert MLX ARRAY into Lisp elements.

The output array would be declared with MLX-DTYPE. ")
  (:method (object)
    "Lisp OBJECT as lisp element"
    object)
  (:method ((arr mlx-array))
    (let ((arr (as-strided arr)))
      (mlx_array_eval  (mlx-object-pointer arr))
      (mlx_synchronize (mlx-object-pointer *mlx-stream*))
      (let ((shape (shape arr))
            (dtype (mlx-dtype arr)))
        (if (endp shape) ;; scalar
            (macrolet ((convert ()
                         `(ecase dtype
                            ,@(loop :for dtype :in +supported-mlx-dtypes+
                                    :collect
                                    `(,dtype (,(intern* 'mlx_array_item_ dtype)
                                              (mlx-object-pointer arr)))))))
              (convert))
            (macrolet ((convert ()
                         `(ecase dtype
                            ,@(loop :for dtype :in +supported-mlx-dtypes+
                                    :collect
                                    `(,dtype (,(intern* 'mlx_array_data_ dtype)
                                              (mlx-object-pointer arr)))))))
              (array<-foreign (convert)
                              (cffi-type<-mlx-dtype dtype)
                              shape
                              (lisp-type<-mlx-dtype dtype))))))))

(defmethod equal ((arr1 mlx-array) (arr2 mlx-array))
  (cl:or (cl:eq arr1 arr2)
         (pointer-eq (mlx-object-pointer arr1)
                     (mlx-object-pointer arr2))
         (lisp<- (= arr1 arr2))))

;;;; array.lisp ends here
