;;;; fft.lisp --- FFT bindings -*- mlx-cl-test-file: "fft.lisp" -*-

(in-package :mlx-cl.fft)

;; TEST: #1dfft
(mlx::with-op-template (op cffi docs
                        (axis "axis along which to perform the FFT (default -1)")
                        (size "size of the transformed axis (default (shape ARRAY :axis AXIS))
The corresponding axis in the input is truncated or padded with zeros to match SIZE. "))
    `(mlx::defmlx-method ,op (array &key (axis -1 axis?) (size nil)
                              &aux
                                (axis* (if axis? (mlx::axis<- axis) axis))
                                (shape (if size
                                           (mlx::num<- size)
                                           (shape array :axis axis*))))

       ,@docs
       (declare (type integer axis* shape))
       (mlx::with-mlx-op ,(mlx::sconc "mlx_fft_" cffi)
         array
         (shape :int)
         (axis* :int)))
  ((1dfft  "fft")  "1-D discrete FFT. "
   :return "DFT of the input along the given axis")
  ((1difft "ifft") "1-D inverse discrete FFT. "
   :return "inverse DFT of the input along the given axis"))

;; TEST: #2dfft
(mlx::with-op-template (op cffi docs
                        (size "size of the transformed axes (default is size along AXIS/AXES)
The corresponding axes in the input are truncated or padded with
zeros to match the sizes in SIZE. ")
                        (axis "axes along which to perform the FFT (default '(-2 -1))")
                        (axes "alias of AXIS"))
    `(mlx::defmlx-method ,op
         (array &key (axis nil axis?) (axes nil axes?) size
          &aux
            (axis* (cond (axis? (mlx::axes<- axis))
                         (axes? (mlx::axes<- axes))
                         (t     (mlx::axes<- #(-2 -1)))))
            (size* (or size (mlx::shape<- (shape array :axis axis*)))))
       ,@docs
       (assert (cl:= (length axis*) 2))
       (assert (cl:= (length size*) 2))
       (mlx::with-foreign<-sequence (axis& axis* :int)
         (mlx::with-foreign<-sequence (size& size* :int)
           (mlx::with-mlx-op ,(mlx::sconc "mlx_fft_" cffi)
             array
             (size& :pointer)
             (2     :size)
             (axis& :pointer)
             (2     :size)))))
  ((2dfft  "fft2")  "2-D discrete FFT. ")
  ((2difft "ifft2") "2-D inverse discrete FFT. "))

;; TEST: #fft
(mlx::defmlx-method fft (array &key (axis nil axis?) (axes nil axes?) size dim
                         &aux
                           (axis* (cond (axis? (mlx::axes<- axis))
                                        (axes? (mlx::axes<- axes))
                                        (t     nil))))
  "Discrete Fourier Transform. "
  :parameters ((array "input array")
               (dim   "dim to perform FFT
 + dim=1: perform 1-D FFT on given AXIS/AXES, which should be integer or sequence(len=1)
   See `1dfft'.
 + dim=2: perform 2-D FFT on given AXIS/AXES, which should be sequence(len=2)
   See `2dfft'.
 + dim=N: perform N-D FFT on given AXIS/AXES,
   See `ndfft'.
 + dim=nil (default): the dim would be determined by AXIS/AXES and ARRAY:
   DIM would be the dim of AXIS/AXES
   if AXIS/AXES is `nil' (default), the DIM is determined by ARRAY's dim:
   + ARRAY(dim=0): will raise error
   + ARRAY(dim=1): `1dfft'
   + ARRAY(dim=2): `2dfft'
   + ARRAY(dim=N): N-D")
               (size  "size of the transformed AXIS/AXES (default shape along AXIS/AXES).
The corresponding axes in the input are truncated or padded with zeros to match SIZE. ")
               (axis  "axis along which to perform FFT (default `nil')")
               (axes  "alias of AXIS"))
  :return "DFT of the input along the given axis"
  :note "See also `ifft'. "
  :dev-note "Use `1dfft', `2dfft' for faster calling. "
  (declare (type (or null (integer 1)) dim))
  (etypecase axis*
    (null
     (cond ((null dim)
            (etypecase size
              (null
               (let ((dim (dim array)))
                 (case dim
                   (0 (error "Cannot perform FFT on scalar: ~A. " array))
                   (1 (1dfft array))
                   (2 (2dfft array))
                   (otherwise
                    (fft array
                         :size size
                         :axis (loop :for i :below dim :collect i)
                         :dim  dim)))))
              (integer
               (1dfft array :size size))
              (sequence
               (fft array :size size
                          :dim  (length size)
                          :axis (loop :for i :below (length size)
                                      :collect i)))))
           (t
            (case dim
              (1 (1dfft array :axis -1))
              (2 (2dfft array :axis #(0 1)))
              (otherwise
               (fft array
                    :size size
                    :axis (loop :for i :below dim :collect i)
                    :dim  dim))))))
    (integer
     (1dfft array :axis axis* :size size))
    (sequence
     (let ((adim (length axis)))
       (when (and dim (cl:/= dim adim))
         (error "AXIS/AXES ~A should match with FFT dim ~A. "
                axis* dim))
       (case adim
         (0 (error "Cannot perform FFT on empty axis: ~A. " axis*))
         (1 (1dfft array :axis (mlx::axis<- axis*) :size size))
         (2 (2dfft array :axis axis*               :size size))
         (otherwise
          (let ((size (etypecase size
                        (null
                         (shape array :axes axis*))
                        (integer
                         (loop :for i :below adim :collect size))
                        (sequence
                         (unless (cl:= (length size) adim)
                           (error "SIZE ~A should be same dim ~A as AXIS/AXES. "
                                  size adim))
                         size))))
            (mlx::with-foreign<-sequence (n* size :int nlen)
              (mlx::with-foreign<-sequence (axes* axis* :int alen)
                (mlx::with-mlx-op "mlx_fft_fftn"
                  array
                  (n*     :pointer)
                  (nlen   :size)
                  (axes*  :pointer)
                  (alen   :size)))))))))))

(mlx::defmlx-method ifft (array &key (axis nil axis?) (axes nil axes?) size dim
                          &aux
                            (axis* (cond (axis? (mlx::axes<- axis))
                                         (axes? (mlx::axes<- axes)))))
  "Inverse Discrete Fourier Transform. "
  :parameters ((array "input array")
               (dim   "dim to perform FFT
 + dim=1: perform 1-D FFT on given AXIS/AXES, which should be integer or sequence(len=1)
   See `1dfft'.
 + dim=2: perform 2-D FFT on given AXIS/AXES, which should be sequence(len=2)
   See `2dfft'.
 + dim=N: perform N-D FFT on given AXIS/AXES,
   See `ndfft'.
 + dim=nil (default): the dim would be determined by AXIS/AXES and ARRAY:
   DIM would be the dim of AXIS/AXES
   if AXIS/AXES is `nil' (default), the DIM is determined by ARRAY's dim:
   + ARRAY(dim=0): will raise error
   + ARRAY(dim=1): `1dfft'
   + ARRAY(dim=2): `2dfft'
   + ARRAY(dim=N): N-D")
               (size  "size of the transformed AXIS/AXES (default shape along AXIS/AXES).
The corresponding axes in the input are truncated or padded with zeros to match SIZE. ")
               (axis  "axis along which to perform FFT (default `nil')")
               (axes  "alias of AXIS"))
  :return "The inverse DFT of the input along the given axes. "
  :note "See also `fft'. "
  :dev-note "Use `1difft' and `2difft' for faster calling. "
  (declare (type (or null (integer 1)) dim))
  (etypecase axis*
    (null
     (cond ((null dim)
            (etypecase size
              (null
               (let ((dim (dim array)))
                 (case dim
                   (0 (error "Cannot perform inverse FFT on scalar: ~A. " array))
                   (1 (1dfft array))
                   (2 (2dfft array))
                   (otherwise
                    (ifft array
                          :size size
                          :axis (loop :for i :below dim :collect i)
                          :dim  dim)))))
              (integer
               (1difft array :size size))
              (sequence
               (ifft array :size size
                           :dim  (length size)
                           :axis (loop :for i :below (length size)
                                       :collect i)))))
           (t
            (case dim
              (1 (1difft array :axis -1))
              (2 (2difft array :axis #(-2 -1)))
              (otherwise
               (ifft array
                     :size size
                     :axis (loop :for i :below dim :collect i)
                     :dim  dim))))))
    (integer
     (1difft array :axis axis* :size size))
    (sequence
     (let ((adim (length axis)))
       (when (and dim (cl:/= dim adim))
         (error "AXIS/AXES ~A should match with FFT dim ~A. "
                axis* dim))
       (case adim
         (0 (error "Cannot perform inverse FFT on empty axis: ~A. " axis*))
         (1 (1difft array :axis (mlx::axis<- axis*) :size size))
         (2 (2difft array :axis axis*               :size size))
         (otherwise
          (let ((size (etypecase size
                        (null
                         (shape array :axes axis*))
                        (integer
                         (loop :for i :below adim :collect size))
                        (sequence
                         (unless (cl:= (length size) adim)
                           (error "SIZE ~A should be same dim ~A as AXIS/AXES. "
                                  size adim))
                         size))))
            (mlx::with-foreign<-sequence (n* size :int nlen)
              (mlx::with-foreign<-sequence (axes* axis* :int alen)
                (mlx::with-mlx-op "mlx_fft_ifftn"
                  array
                  (n*     :pointer)
                  (nlen   :size)
                  (axes*  :pointer)
                  (alen   :size)))))))))))

;;;; fft.lisp ends here
