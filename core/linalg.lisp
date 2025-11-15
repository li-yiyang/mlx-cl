35;93;31M;;;; linalg.lisp ---- Liner Algebra

(in-package :mlx-cl.linalg)

;; TEST: #matinv
(mlx::defmlx-method matinv (matrix)
  "Compute the inverse of a square matrix. "
  :parameters ((matrix "input `mlx-array'

This function supports arrays with at least 2 dimensions.
When the input has more than two dimensions, the inverse is
computed for each matrix in the last two dimensions of MATRIX."))
  :return "returns a `mlx-array' that (@ A MAT-INV) = (@ MAT-INV A) = (eye (shape A))"
  (mlx::with-mlx-op "mlx_linalg_inv"
    matrix))

;; TEST: #tri-inv
(mlx::defmlx-method tri-inv (matrix &optional (tri :lower))
  "Compute the inverse of a triangular square matrix. "
  :parameters ((matrix "input `mlx-array'

This function supports arrays with at least 2 dimensions.
When the input has more than two dimensions, the inverse is
computed for each matrix in the last two dimensions of MATRIX.")
               (tri  "the array is upper or lower triangular (default `:lower')
 + `:lower': the MATRIX is lower triangle
 + `:upper': the MATRIX is upper triangle"))
  (declare (type (member :lower :upper) tri))
  (mlx::with-mlx-op "mlx_linalg_tri_inv"
    matrix
    ((ecase tri
       (:lower nil)
       (:upper t))
     :bool)))

(defparameter +fro+ (cffi:foreign-string-alloc "fro"))
(defparameter +nuc+ (cffi:foreign-string-alloc "nuc"))

;; TEST: #norm
(mlx::defmlx-method norm (matrix/vector &optional ord
                          &key (axis nil axis?) (axes nil axes?)
                            (keep-dim-p *keep-dim-p* keep-dim-p?)
                          &aux
                            (axis! (cond (axis? (mlx::axes<- axis))
                                         (axes? (mlx::axes<- axes))
                                         (t     nil)))
                            (keepdimp (if keep-dim-p?
                                          (mlx::bool<- keep-dim-p)
                                          keep-dim-p)))
  "Matrix or vector norm. "
  :parameters ((matrix/vector "input `mlx-array'
 + if AXIS is `nil', MATRIX/VECTOR must be 1-D or 2-D, unless ORD is `nil'
 + if both AXIS and ORD are `nil', the 2-norm of (flattern MATERIX/VECTOR) will be returned")
               (ord "order of the norm (default `nil')

  | ORD      | norm for matrices             | norm for vectors             |
  |----------+-------------------------------+------------------------------|
  | nil      | Frobenius norm                | 2-norm                       |
  | :fro     | Frobenius norm                | -                            |
  | :nuc     | nuclear norm                  | -                            |
  | :inf     | max(sum(abs(MATRIX), axis=1)) | max(abs(VECTOR))             |
  | :neg-inf | min(sum(abs(MATRIX), axis=1)) | mix(abs(VECTOR))             |
  | 0        | -                             | sum(VECTOR != 0)             |
  | 1        | max(sum(abs(MATRIX), axis=0)) | as below                     |
  | -1       | min(sum(abs(MATRIX), axis=0)) | as below                     |
  | 2        | 2-norm (largest sing. value)  | as below                     |
  | -2       | smallest singular value       | as below                     |
  | other    | -                             | sum(abs(VECTOR)^ORD)^(1/ORD) |

The Frobenius norm is given by:

    || A ||_F = ( sum_{i,j} abs(a_{i,j})^2 )^{1/2}

The nuclear norm is the sum of the singular values.

Both the Frobenius and nuclear norm orders are only defined for matrices
and raise a ValueError when (dim A) is not equal to 2.
")
               (axis "AXIS/AXES to norm over (default `nil')
 + integer: axis of ARRAY along which to compute the vector norms
 + sequence(dim=2): axes holding 2-D matrices, the matrix norms of these matrices are computed
 + nil: depends on the dim of the ARRAY")
               (axes "alias of AXIS")
               (keep-dim-p "whether the axes are normed over left in the result as dimensions with size one (default `*keep-dim-p*')"))
  :return "`mlx-array' containing the norm(s)"
  :note   "
References:
[1] G.H.Golub and C.F.Van Loan, Matrix Computations, Baltimore, MD, Johns Hopkins University Press, 1985, pg. 15
"
  (declare (type (or null number mlx-array
                     (member :inf :infinity :pos-inf :positive-infinity
                             :neg-inf :negative-infinity
                             :nuc :nuclear
                             :fro :frobenius))
                 ord))
  (labels ((mlx_linalg_norm (ord-double axis-pointer axis-num-size)
             (declare (type double-float ord-double)
                      (type cffi:foreign-pointer axis-pointer)
                      (type integer axis-num-size))
             (mlx::with-mlx-op "mlx_linalg_norm"
               matrix/vector
               (ord-double    :double)
               (axis-pointer  :pointer)
               (axis-num-size :size)
               (keepdimp      :bool)))
           (mlx_linalg_norm_matrix (ord-str-ptr axis-pointer axis-num-size)
             (declare (type cffi:foreign-pointer ord-str-ptr axis-pointer)
                      (type integer axis-num-size))
             (mlx::with-mlx-op "mlx_linalg_norm_matrix"
               matrix/vector
               (ord-str-ptr   :pointer)
               (axis-pointer  :pointer)
               (axis-num-size :size)
               (keepdimp      :bool)))
           (linalg_norm (ord)
             (etypecase axis!
               (null
                (if (numberp ord)
                    (mlx_linalg_norm        ord (cffi:null-pointer) 0)
                    (mlx_linalg_norm_matrix ord (cffi:null-pointer) 0)))
               (integer
                (mlx::with-foreign<-sequence (ptr (make-sequence
                                                   '(vector (signed-byte 32) 1)
                                                   1
                                                   :initial-element axis!)
                                              :int len)
                  (if (numberp ord)
                      (mlx_linalg_norm        ord ptr len)
                      (mlx_linalg_norm_matrix ord ptr len))))
               (sequence
                (assert (cl:= (length axis!) 2))
                (mlx::with-foreign<-sequence (ptr axis! :int len)
                  (if (numberp ord)
                      (mlx_linalg_norm        ord ptr len)
                      (mlx_linalg_norm_matrix ord ptr len)))))))
    (etypecase ord
      (null
       (etypecase axis!
         (null (if (dim< matrix/vector 2)
                   (linalg_norm 2d0)
                   (linalg_norm +fro+)))
         (integer  (linalg_norm 2d0))
         (sequence (linalg_norm +fro+))))
      (keyword
       (ecase ord
         ((:fro :frobenius)
          (assert (dim= matrix/vector 2))
          (linalg_norm +fro+))
         ((:nuc :nuclear)
          (assert (dim= matrix/vector 2))
          (linalg_norm +nuc+))
         ((:inf :infinity :pos-inf :positive-infinity)
          (linalg_norm #+sbcl      sb-ext:double-float-positive-infinity
                       #+lispworks SYSTEM::*PLUS-INFINITY-DOUBLE*
                       #-(or sbcl lispworks)
                       (error "MLX-CL for your lisp implementation does not support `~S' yet.
Please raise issue or help me writing a portable one. "
                              ord)))
         ((:neg-inf :negative-infinity)
          (linalg_norm #+sbcl      sb-ext:double-float-negative-infinity
                       #+lispworks SYSTEM::*MINUS-INFINITY-DOUBLE*
                       #-(or sbcl lispworks)
                       (error "MLX-CL for your lisp implementation does not support `~S' yet.
Please raise issue or help me writing a portable one. "
                              ord)))))
      (number
       (linalg_norm (coerce ord 'double-float)))
      (mlx-array
       (linalg_norm (coerce (mlx::num<- ord) 'double-float))))))

(mlx::defmlx-method cholesky (array &optional (tri :lower))
  "Compute the Cholesky decomposition of a real symmetric positive semi-definite matrix. "
  :parameters ((array "input array (at least 2 dimensions)
 + if given ARRAY is more than 2-D, the CHolesky decomposition is computed
   for each matrix in the last two dimensions of ARRAY
 + if input ARRAY is not symmetric positive semi-definite, behaviour is undefined
")
               (tri   "shape of Cholesky factor (default `:lower')
 this sets the returned Cholesky factor is upper or lower triangular
 + `:lower': the MATRIX is lower triangle
   returns a lower triangluar L matrix such that (@ L (transpose L)) = ARRAY
 + `:upper': the MATRIX is upper triangle
   returns an upper triangluar U matrix such that (@ (transpose U) U) = ARRAY"))
  :return "the Cholesky decomposition factor"
  (declare (type (member :lower :upper) tri))
  (mlx::with-mlx-op "mlx_linalg_cholesky"
    array
    ((ecase tri
       (:lower nil)
       (:upper t))
     :bool)))

(mlx::defmlx-method cholesky-inv (array &optional (tri :lower))
  "Compute the inverse of a real symmetric positive semi-definite matrix using it's cholesky decomposition. "
  :parameters ((array "input triangluar matrix (at least dim=2)
 + if given ARRAY has more than 2-D, the Cholesky inverse is computed for
   each matrix in the last two dimensions of L
 + if the input matrix is not a triangluar matrix, the behaviour is undefined")
               (tri   "the shape of triangular Cholesky (default `:lower'
 + `:lower': return the lower triangular Cholesky factor
 + `:upper': return the upper triangular Cholesky factor"))
  :return "(matinv ARRAY) where ARRAY = (@ L (transpose L))"
  :definition "
Let A be a real symmetric positive semi-definite matrix and L is its Cholesky decomposition such that:

    A = (@ L (transpose L))

This function computes (matinv A)."
  (declare (type (member :lower :upper) tri))
  (mlx::with-mlx-op "mlx_linalg_cholesky_inv"
    array
    ((ecase tri
       (:lower nil)
       (:upper t))
     :bool)))

(mlx::defmlx-method cross (array1 array2 &key (axis -1 axis?)
                           &aux (axis! (if axis? (mlx::axis<- axis) axis)))
  "Compute the cross product of two arrays along a specified axis. "
  :parameters ((array1 "input array")
               (array2 "input array")
               (axis   "the axis for cross product (default `-1')"))
  :note "The cross product is defined for arrays with size 2 or 3 in the specified axis.
If the size is 2 then the third value is assumed to be zero."
  :return "the cross product of ARRAY1 and ARRAY2"
  :definition "
"
  (declare (type integer axis!))
  (mlx::with-mlx-op "mlx_linalg_cross"
    array1
    array2
    (axis! :int)))

(mlx::defmlx-method qr (array)
  "QR factorization of the input ARRAY. "
  :parameters ((array "input array (dim>=2)"))
  :return "values of Q and R matrics that (@ Q R) = ARRAY"
  (assert (dim>= array 2))
  (mlx-cl::with-elem& (Q Q& :type :pointer :alloc (mlx-cl::mlx_array_new))
    (mlx-cl::with-elem& (R R& :type :pointer :alloc (mlx-cl::mlx_array_new))
      (mlx-cl::ensure-success "mlx_linalg_qr"
        :pointer Q&
        :pointer R&
        :pointer (mlx-cl::mlx-object-pointer array)
        :pointer (mlx-cl::mlx-object-pointer (mlx-cl::default-mlx-stream)))
      (values (mlx-cl::wrap-as-mlx-array Q)
              (mlx-cl::wrap-as-mlx-array R)))))

;; (mlx::defmlx-method svd (array)
;;   "The Singular Value Decoposition (SVD) of the input matrix. "
;;   :parameters ((array "input array"))
;;   )

;; (mlx::defmlx-method eigvals (array)
;;   )

;; (mlx::defmlx-method eigh (array)
;;   )

;; (mlx::defmlx-method lu (array)
;;   )

;; (mlx::defmlx-method pinv (array)
;;   )

;; (mlx::defmlx-method solve (array array)
;;   )

;; (mlx::defmlx-method solve-triangular (array array)
;;   )

;;;; linalg.lisp ends here
