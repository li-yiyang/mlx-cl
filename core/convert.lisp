;;;; convert.lisp ---- Utilies to convert between Lisp and mlx-array (especially parameters)

;;; Commentary:
;; convert between lisp and mlx-array data input and output
;; for more lowlevel conversion, see `lisp/utils.lisp':
;; + `sequence<-foreign'
;; + `array<-foreign'


;;; Utils Sugar


;;; Convert lisp value as `mlx-array'



;;; Convert `mlx-array' to lisp values

(defun lisp<-mlx-array (arr)
  (declare (type mlx-array arr))
  (let ((shape (shape arr))
        (dtype (mlx-dtype arr)))
    (if (endp shape)
        (macrolet ((convert ()
                     `(ecase dtype
                        ,@(loop :for dtype :in +supported-mlx-dtypes+
                                :collect `(,dtype (,(intern* 'mlx_array_item_ dtype)
                                                   (mlx-object-pointer arr)))))))
          (mlx_array_eval  (mlx-object-pointer arr))
          (mlx_synchronize (mlx-object-pointer *mlx-stream*))
          (convert))
        (let ((arr (as-strided (contiguous arr))))
          (macrolet ((convert ()
                       `(ecase dtype
                          ,@(loop :for dtype :in +supported-mlx-dtypes+
                                  :collect
                                  `(,dtype (,(intern* 'mlx_array_data_ dtype)
                                            (mlx-object-pointer arr)))))))
            (mlx_array_eval  (mlx-object-pointer arr))
            (mlx_synchronize (mlx-object-pointer *mlx-stream*))
            (array<-foreign (convert)
                            (cffi-type<-mlx-dtype dtype)
                            shape
                            (lisp-type<-mlx-dtype dtype)))))))

(defgeneric lisp<- (array)
  (:documentation
   "Convert MLX ARRAY into Lisp elements.

The output array would be declared with MLX-DTYPE. ")
  (:method (object)          object)
  (:method ((arr mlx-array)) (lisp<-mlx-array arr)))


;;; Convert for parameters

;; Dev Note:
;; the functions `bool<-', `num<-', `seq<-', `axis<-' are used to
;; convert number / mlx-array into expected values. They should
;; be used to construct generic function (ops) to support values
;; be called samelessly between lisp and mlx-array.

(declaim (inline bool<- num<- seq<- axis<-))

(defun bool<- (obj)
  "Parse input OBJ as boolean.

Dev Note:
this should be used to parse mlx method parameters like
`keep-dim-p', `nan-equal-p'. "
  (the boolean
    (cl:and (typecase obj
              (mlx-array
               (assert (dim= obj 0))    ; scalar
               (lisp<-mlx-array obj))
              (t obj))
            t)))

(defun num<- (num)
  "Parse input NUM as lisp number.

Dev Note:
this should be used to parse mlx method parameters like
`start', `stop' or else. "
  (the number
    (etypecase num
      (number     num)
      (mlx-array
       (assert (dim= num 0))            ; scalar
       (lisp<-mlx-array num)))))

(defun shape<- (shape)
  "Parse input OBJ as shape sequence.
Return value is simple-vector.

Dev Note:
this should be used to parse mlx method parameters like
`shape', `size' or else. "
  (etypecase shape
    ((simple-array (signed-byte 32))
     shape)
    (number
     (make-array 1 :initial-element shape
                   :element-type '(signed-byte 32)))
    (list
     (make-array (length shape)
                 :initial-contents shape
                 :element-type '(signed-byte 32)))
    (simple-array
     (make-array (length shape)
                 :initial-contents shape
                 :element-type '(signed-byte 32)))
    (mlx-array
     (cond ((dim= shape 0)
            (make-array 1 :initial-element (lisp<-mlx-array shape)
                          :element-type    '(signed-byte 32)))
           ((dim= shape 1) (lisp<-mlx-array shape))
           (t (error "Expecting shape to be 0 or 1 dim mlx-arry, but got ~A. "
                     shape))))))

(defun vec<- (seq)
  "Turn SEQ as (simple-array (signed-byte 32)). "
  (etypecase seq
    ((simple-array (signed-byte 32)) seq)
    (integer
     (make-array 1 :initial-element seq
                   :element-type '(signed-byte 32)))
    (list
     (make-array (length seq)
                 :initial-contents seq
                 :element-type '(signed-byte 32)))
    (simple-array
     (make-array (length seq)
                 :initial-contents seq
                 :element-type '(signed-byte 32)))
    (mlx-array
     (cond ((dim= seq 0)
            (make-array 1 :initial-element (lisp<-mlx-array seq)
                          :element-type    '(signed-byte 32)))
           ((dim= seq 1)
            (lisp<-mlx-array seq))
           (t (error "Expecting shape to be 0 or 1 dim mlx-array, but got ~A. "
                     seq))))))

(defun lst<- (obj)
  "Convert OBJ as list.
Return value is a list (first sequence, then force list). "
  (typecase obj
    (list         obj)
    (simple-array (map 'list #'cl:identity obj))
    (integer      (list obj))))


;;; TODO: #mlx-cl #optimize
;; Use compiler-macro to expand the constant axis expression during compile time,
;; for example:
;; + (OP ARRAY :axis :first) => should be expanded into (OP ARRAY :axis 0)
;; + (OP ARRAY :axes '(:last 2)) => should be expanded into (OP ARRAY :axes #(-2 -1))
;; should implement it in `lisp/comp-acc.lisp' in the future.

;;; Convert between axis/axes

(deftype mlx-axis ()
  "CFFI type of :int"
  '(signed-byte 32))

(deftype mlx-axes ()
  "CFFI type of :int*"
  '(simple-array (signed-byte 32)))

(deftype mlx-axis/axes ()
  "(or mlx-axis mlx-axes)"
  '(or
    (singed-byte 32)
    (simple-array (signed-byte 32))))

;;; AXIS/AXES<-
(defparameter *axis-spec-rules*
  (make-hash-table :test 'eql)
  "Rules of mlx axis.

Key: keyword of AXIS/AXES shortcuts
Val: function with lambda list (...) for axis rules. ")

;;; Dev Note:
;; Not sure whether or not to make it user-definable,
;; like `defmlx-slice', maybe defmlx-axis?
;;
(defun axes<- (axis/axes)
  "Convert AXIS as single/multiple axis/axes.
Return:
+ nil: if AXIS/AXES nil
+ integer of single axis
+ sequence (simple-array (signed-byte 32)) for multiple axes
"
  (labels ((apply-rule (rule args)
             (let ((rule (or (gethash rule *axis-spec-rules*)
                             (error "Unknown AXIS/AXES shortcuts ~S. " rule))))
               (normalize (apply rule args))))
           (vec<- (seq)
             (map 'simple-vector (lambda (ax) (assert (integerp ax)) ax) seq))
           (normalize (axis)
             (etypecase axis
               (null nil)
               ((or integer simple-vector) axis)
               (list
                (cond ((keywordp (car axis))
                       (apply-rule (car axis) (cdr axis)))
                      ((cl:= (length axis) 1)
                       (the integer (car axis)))
                      (t (vec<- axis))))
               (sequence (vec<- axis))
               (keyword (apply-rule axis ()))
               (mlx-array
                (cond ((dim= axis 0) (the integer (lisp<-mlx-array axis)))
                      ((dim= axis 1) (vec<- (lisp<-mlx-array axis)))
                      (t "Expecting scalar or 1-D mlx-array for AXIS/AXES, but got ~A. "
                         axis))))))
    (normalize axis/axes)))

(defun axis<- (axis)
  "Convert OBJ as single axis."
  (let ((axes (axes<- axis)))
    (etypecase axes
      (null    nil)
      (integer axes)
      (simple-vector
       (if (cl:= (length axes) 1)
           (svref axes 0)
           (error "Expecting single AXIS, but got ~A. " axes))))))

(defmacro defmlx-axis (name lambda-list &body body)
  "Define a axis shortcut NAME for mlx-array.

Syntax:

    (defmlx-axis :shortcut (...)
      [docstring]
      &body body)
"
  (declare (type keyword name))
  (multiple-value-bind (doc plist body)
      (split-doc-body body)
    `(setf (gethash ,name *axis-spec-rules*)
           (lambda ,lambda-list ,(apply #'gen-doc doc plist) ,@body))))

(defmethod documentation (axis (type (eql 'axis)))
  (declare (type keyword axis))
  (documentation (gethash axis *axis-spec-rules*) 'function))

(defmlx-axis :last (&optional (n 1))
  "Last N(=1) axes. "
  :parameters ((n "N axes (default 1)
 + if N=1, return single axis
 + if N>1, return a simple-vector of (-N, ..., -1)"))
  :note "See also `:last'. "
  (declare (type (integer 1) n))
  (cond ((cl:= n 1) -1)
        ((cl:> n 1)
         (map 'simple-vector #'cl:-
              (loop :for i :from n :downto 1 :by 1 :collect i)))))

(defmlx-axis :first (&optional (n 1))
  "First N(=1) axes. "
  :parameters ((n "N axes (default 1)
 + if N=1, return single axis
 + if N>1, return a simple-vector of (0, ..., N-1)"))
  :note "See also `:first'. "
  (declare (type (integer 1) n))
  (cond ((cl:= n 1) 0)
        ((cl:> n 1)
         (map 'simple-vector #'cl:identity
              (loop :for i :from 0 :below n :by 1 :collect i)))))



;;;; convert.lisp
