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
 + sequence(dim=2): axes holding 2-D matrices,
   the matrix norms of these matrices are computed
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
                (assert (cl:= (cl:length axis!) 2))
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

(mlx::defmlx-method svd (array &key (uv t uv?)
                         &aux (uv! (if uv? (mlx::bool<- uv) uv)))
  "The Singular Value Decoposition (SVD) of the input matrix. "
  :parameters ((array "input array (dim>=2)
if ARRAY has more than 2-D, the function iterates over
all indices of the first (dim ARRAY) SVD is applied to the
last two indices")
               (uv    "whether compute U V decomposition (default `t')
 + if non-nil: return U, S, and Vt components
 + if nil: return only S array"))
  :definition "A = (@ U (diag S) Vt)"
  :return "values of U, S and Vt if UV, otherwise, only S"
  (mlx::with-mlx-op "mlx_linalg_svd"
    array
    (uv! :bool)))

;; TEST: #eigvals
(mlx::defmlx-method eigvals (array)
  "Compute the eigenvalues of a square matrix. "
  :parameters ((array "input array (dim>=2)
if ARRAY has more than 2-D, the eigen values are computed for each
matrix in the last two dimensions"))
  :return "eigen values"
  (mlx::with-mlx-op "mlx_linalg_eigvals"
    array))

(mlx::defmlx-method eig (array)
  "Compute the eigenvalues and eigenvectors of a square matrix. "
  :parameters ((array "input array (dim>=2)
if ARRAY has more than 2-D, the eigen values and eign vectors are
computed for each matrix in the last two dimensions"))
  :return "values of eigen values and normalized right eign vectors
The column (at V :* I) is the eigen vector corresponding to the I-th
eigen value. "
  (mlx-cl::with-elem& (val val& :type :pointer :alloc (mlx-cl::mlx_array_new))
    (mlx-cl::with-elem& (vec vec& :type :pointer :alloc (mlx-cl::mlx_array_new))
      (mlx-cl::ensure-success "mlx_linalg_eig"
        :pointer val&
        :pointer vec&
        :pointer (mlx-cl::mlx-object-pointer array)
        :pointer (mlx-cl::mlx-object-pointer (mlx-cl::default-mlx-stream)))
      (values (mlx-cl::wrap-as-mlx-array val)
              (mlx-cl::wrap-as-mlx-array vec)))))

(defparameter +L+ (cffi:foreign-string-alloc "L"))
(defparameter +U+ (cffi:foreign-string-alloc "U"))

(mlx::defmlx-method eigvalsh (array &optional (tri :lower))
  "Compute the eigenvalues of a complex Hermitian or real symmetric matrix. "
  :parameters ((array "input array (dim>=2)
must be a real symmetric or complex Hermitian matrix")
               (tri   "using which part of ARRAY to use
 + `:upper': upper triangle of the matrix
 + `:lower': lower triangle of the matrix"))
  :return "the eign values in ascending order"
  :note "
The input matrix is assumed to be symmetric (or Hermitian).
Only the selected triangle is used. No checks for symmetry are performed."
  (declare (type (member :lower :upper) tri))
  (mlx::with-mlx-op "mlx_linalg_eigvalsh"
    array
    ((ecase tri
       (:lower +L+)
       (:upper +U+))
     :pointer)))

(mlx::defmlx-method eigh (array &optional (tri :lower))
  "Compute the eigenvalues of a complex Hermitian or real symmetric matrix. "
  :parameters ((array "input array (dim>=2)
must be a real symmetric or complex Hermitian matrix")
               (tri   "using which part of ARRAY to use
 + `:upper': upper triangle of the matrix
 + `:lower': lower triangle of the matrix"))
  :return "values of eign values and eign vectors"
  :note "
The input matrix is assumed to be symmetric (or Hermitian).
Only the selected triangle is used. No checks for symmetry are performed."
  (declare (type (member :lower :upper) tri))
  (mlx-cl::with-elem& (val val& :type :pointer :alloc (mlx-cl::mlx_array_new))
    (mlx-cl::with-elem& (vec vec& :type :pointer :alloc (mlx-cl::mlx_array_new))
      (mlx-cl::ensure-success "mlx_linalg_eigh"
        :pointer val&
        :pointer vec&
        :pointer (mlx-cl::mlx-object-pointer array)
        :pointer (ecase tri
                   (:lower +L+)
                   (:upper +U+))
        :pointer (mlx-cl::mlx-object-pointer (mlx-cl::default-mlx-stream)))
      (values (mlx-cl::wrap-as-mlx-array val)
              (mlx-cl::wrap-as-mlx-array vec)))))

(mlx::defmlx-method lu (array)
  "Compute the LU factorization of the given matrix ARRAY. "
  :parameters ((array "input array (dim>=2)"))
  :return "values of p, L and U `mlx-array', such that A = (@ (at L p :*) U)"
  (mlx::with-mlx-op ("mlx_linalg_lu" :alloc mlx::mlx_vector_array_new
                                     :wrap  mlx::wrap-as-mlx-array-list
                                     :free  mlx::mlx_vector_array_free)
    array))

(mlx::defmlx-method lu-factor (array)
  "Computes a compact representation of the LU factorization. "
  :parameters ((array "input array (dim>=2)"))
  :return "LU matrix and pivots `mlx-array'"
  (mlx::with-elem& (LU LU& :type :pointer :alloc (mlx-cl::mlx_array_new))
    (mlx::with-elem& (pivots pivots& :type :pointer :alloc (mlx-cl::mlx_array_new))
      (mlx-cl::ensure-success "mlx_linalg_lu_factor"
        :pointer LU&
        :pointer pivots&
        :pointer (mlx-cl::mlx-object-pointer array)
        :pointer (mlx-cl::mlx-object-pointer (mlx-cl::default-mlx-stream)))
      (values (mlx-cl::wrap-as-mlx-array LU)
              (mlx-cl::wrap-as-mlx-array pivots)))))

(mlx::defmlx-method pinv (array)
  "Compute the (Moore-Penrose) pseudo-inverse of a matrix. "
  :parameters ((array "input array (dim>=2)"))
  :return "APLUS such that (@ ARRAY APLUS ARRAY) = ARRAY"
  (mlx::with-mlx-op "mlx_linalg_pinv"
    array))

(mlx::defmlx-method solve (A B)
  "Compute the solution to a system of linear equations (@ A X) = B. "
  :parameters ((A "weights array")
               (B "base array"))
  :return "The unique solution to the system A X = B"
  (mlx::with-mlx-op "mlx_linalg_solve"
    A
    B))

(mlx::defmlx-method solve-triangular (A B &optional (tri :lower))
  "Computes the solution of a triangular system of linear equations (@ A X) = B. "
  :parameters ((A "weights triangular array")
               (B "base array")
               (tri "shape of triangular weights array (default `:lower')
 + `:lower': lower triangular
 + `:upper': upper triangular"))
  :return "the unique solution to the system (@ A X) = B"
  (mlx::with-mlx-op "mlx_linalg_solve_triangular"
    A
    B
    ((ecase tri
       (:lower nil)
       (:upper t))
     :bool)))

;;;; linalg.lisp ends here
