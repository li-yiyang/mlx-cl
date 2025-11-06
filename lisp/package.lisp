;;; package.lisp --- mlx-cl.ffi package definition

;;;

(uiop:define-package #:mlx-cl
  (:use :cl :cffi)
  (:nicknames #:mlx)
  (:shadow
   #:+ #:- #:* #:/
   #:= #:/= #:< #:> #:<= #:>=
   #:sin  #:cos  #:tan  #:asin  #:acos  #:atan
   #:sinh #:cosh #:tanh #:asinh #:acosh #:atanh
   #:mod  #:exp #:log
   #:conjugate
   #:ceiling #:floor
   #:abs #:sqrt
   #:equal
   #:min #:max #:sort
   #:bit-and #:bit-ior #:bit-not #:bit-xor
   #:logand #:logior #:lognot #:logxor
   #:imagpart #:realpart #:ash
   #:expt #:round)
  (:reexport #:common-lisp)
  (:export
   ;;;; utils

   ;;;; error
   #:mlx-error

   ;;;; string

   ;;;; version
   #:mlx-version

   ;;;; device
   #:*mlx-device*
   #:mlx-device
   #:mlx-device-type
   #:mlx-device-index
   #:with-mlx-device

   ;;;; stream
   #:*mlx-stream*
   #:mlx-stream
   #:mlx-cpu-stream
   #:mlx-gpu-stream
   #:mlx-stream-equal
   #:mlx-stream-device
   #:mlx-stream-index
   #:default-mlx-cpu-stream
   #:default-mlx-gpu-stream

   ;;;; array
   #:mlx-array-error
   #:*default-mlx-int-dtype*
   #:*default-mlx-dtype*
   #:*default-mlx-float-dtype*
   #:*default-mlx-uint-dtype*
   #:mlx-array
   #:mlx-dtype
   #:dtype
   #:lisp<-

   #:size
   #:nbytes
   #:dim
   #:shape
   #:strides

   ;;;; ops
   ;;; config
   #:*relative-tolerance*
   #:*absolute-tolerance*
   #:*nan-equal-p*
   #:*keep-dim-p*

   ;;; Create MLX array
   #:mlx-array
   #:arange
   #:zeros  #:zeros-like
   #:ones   #:ones-like
   #:full
   #:id
   #:eye
   #:tri
   #:linspace
   #:meshgrid

   ;;; MLX attributes
   #:shape
   #:sign
   #:degrees
   #:radians
   #:tr
   #:diag
   #:dtype
   #:finite-p
   #:nan-p
   #:inf-p
   #:neg-inf-p
   #:pos-inf-p
   #:num<-nan
   #:std
   #:var

   ;;; Indexing
   #:~
   #:~~
   #:at*
   #:at
   #:defmlx-slice

   ;;; Basic Operations
   #:abs
   #:exp
   #:log
   #:+   #:-   #:*   #:/
   #:add #:sub #:mul #:div
   #:abs
   #:exp
   #:log #:log2 #:log10 #:loge
   #:square
   #:sqrt #:rsqrt
   #:imagpart #:realpart
   #:conjugate
   #:sin  #:cos  #:tan  #:sinh  #:cosh  #:tanh
   #:asin #:acos #:atan #:asinh #:acosh #:atanh
   #:logical-not #:! #:lognot
   #:reciprocal #:negative
   #:divmod
   #:remainder
   #:floor
   #:ceiling
   #:clip
   #:round
   #:matmul #:@
   #:inner
   #:outer
   #:kron
   #:tensordot
   #:lognot
   #:ash
   #:logand #:bit-and
   #:logxor #:bit-xor
   #:logior #:bit-ior #:bit-or
   #:op2< #:op2<= #:op2> #:op2>= #:op2= #:op2/=
   #:< #:<= #:> #:>= #:= #:/=
   #:op2and #:op2or #:logical-and #:logical-or #:logical-not
   #:&& #:|| #:!
   #:~= #:op2~= #:all-close
   #:all #:any
   #:argmin #:argmax
   #:maximum #:minimum
   #:mean #:prod #:logsumexp #:sum
   #:softmax #:squeeze

   ;;; Operation on Shape
   #:roll
   #:atleast
   #:at-least-1d
   #:at-least-2d
   #:at-least-3d
   #:expand-dims
   #:reshape
   #:swap-axes
   #:transpose
   #:flatten
   #:unflatten

   ;;; Conc and Split
   #:concat
   #:stack
   #:tile
   #:repeat
   #:split

   ;;; Sorting Operation
   #:sort
   #:topk

   ;;; Conv Operation
   #:pad
   #:conv

   ;;; Cummulative Operation
   #:cumsum

   ;;; Einsum
   #:einsum

   ;;; Dev
   #:as-type
   #:view
   #:copy

   ;; metal
   #:metal-available-p
   #:metal-device-info
   #:metal-device-architecture
   #:metal-device-max-buffer-length
   #:metal-device-max-recommanded-working-set-size
   #:metal-device-memory-size

   ;;;; sugar
   #:dim= #:dim/= #:dim> #:dim< #:dim<= #:dim>=
   #:->*

   ;;;; io
   #:load-from
   #:save-to
   #:save
   ))

(uiop:define-package #:mlx-cl.fft
  (:use :mlx-cl)
  (:nicknames :mlx.fft)
  (:export
   #:fft
   #:ifft
   #:rfft
   #:irfft
   #:fftshift
   #:ifftshift))

(uiop:define-package #:mlx-cl.linalg
  (:use :mlx-cl)
  (:nicknames :mlx.linalg)
  (:export
   #:inv
   #:tri-inv
   #:norm
   #:cholesky
   #:cholesky-inv
   #:cross
   #:qr
   #:svd
   #:eigvals
   #:eig
   #:eigvalsh
   #:eigh
   #:lu
   #:lu-factor
   #:pinv
   #:solve
   #:solve-triangular))

(uiop:define-package #:mlx-cl.random
  (:use :mlx-cl)
  (:nicknames :mlx.rnd)
  (:export
   #:bernoulli
   #:categorical
   #:gumbel
   #:key
   #:normal
   #:multivariate-normal
   #:randint
   #:speed
   #:split
   #:truncated-normal
   #:uniform
   #:laplace
   #:permutation))

(defpackage #:mlx-user
  (:use :mlx-cl)
  (:local-nicknames
   (:fft    :mlx-cl.fft)
   (:rnd    :mlx-cl.random)
   (:linalg :mlx-cl.linalg)))

;;;; package.lisp ends here
