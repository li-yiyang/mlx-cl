;;;; package.lisp --- mlx-cl.ffi package definition

;;;

(uiop:define-package #:mlx-cl
  (:use :cl :cffi)
  (:nicknames #:mlx)
  (:shadow
   #:+ #:- #:* #:/
   #:= #:/= #:< #:> #:<= #:>=
   #:sin  #:cos  #:tan  #:asin  #:acos  #:atan
   #:sinh #:cosh #:tanh #:asinh #:acosh #:atanh
   #:mod  #:exp
   #:conjugate
   #:ceiling #:floor
   #:abs #:sqrt
   #:equal #:eq
   #:min #:max #:sort
   #:and #:or #:not #:bit-and #:bit-ior #:bit-not #:bit-xor
   #:logand #:logior #:lognot #:logxor
   #:trace #:identity #:imagpart #:realpart #:ash
   #:expt #:round)
  (:reexport #:common-lisp)
  (:export
   ;; utils

   ;; error
   #:mlx-error

   ;; string

   ;; version
   #:mlx-version

   ;; device
   #:*mlx-device*
   #:mlx-device
   #:mlx-device-type
   #:mlx-device-index
   #:with-mlx-device

   ;; stream
   #:*mlx-stream*
   #:mlx-stream
   #:mlx-cpu-stream
   #:mlx-gpu-stream
   #:mlx-stream-equal
   #:mlx-stream-device
   #:mlx-stream-index
   #:synchronize
   #:default-mlx-cpu-stream
   #:default-mlx-gpu-stream

   ;; array
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

   ;; ops
   #:add
   #:sub
   #:div
   #:mul

   #:+
   #:-
   #:*
   #:/

   #:abs
   #:addmm
   #:all-axes
   #:all-axis
   #:all
   #:all-close
   #:any-axes
   #:any-axis
   #:any
   #:arange
   #:arccos
   #:arccosh
   #:arcsin
   #:arcsinh
   #:arctan
   #:arctan2
   #:arctanh
   #:argmax
   #:argmin
   #:argpartition
   #:argsort
   #:=
   #:strided<-
   #:type<-
   #:atleast
   #:bitwise-and
   #:bitwise-invert
   #:bitwise-or
   #:bitwise-xor
   #:block-masked-mm
   #:block-size
   #:broadcast-arrays
   #:broadcast-to
   #:ceil
   #:clip
   #:concatenate
   #:conjugate
   #:contiguous
   #:conv
   #:copy
   #:cos
   #:cosh
   #:cummax
   #:cummin
   #:cumprod
   #:cumsum
   #:degrees
   #:depends
   #:dequantize
   #:diag
   #:diagonal
   #:mod
   #:einsum
   #:equal
   #:erf
   #:erfinv
   #:exp
   #:expand-dims
   #:expm1
   #:eye
   #:flatten
   #:floor
   #:full
   #:gather
   #:>
   #:>=
   #:hadamard-transform
   #:identity
   #:imag
   #:inner
   #:close-p
   #:finite-p
   #:inf-p
   #:nan-p
   #:neg-inf-p
   #:pos-inf-p
   #:kron
   #:left-shift
   #:<
   #:<=
   #:linspace
   #:log
   #:log10
   #:log1p
   #:log2
   #:logaddexp
   #:logcumsumexp
   #:logical-and
   #:logical-not
   #:logsumexp
   #:@
   #:max
   #:maximum
   #:mean
   #:meshgrid
   #:min
   #:minimum
   #:moveaxis
   #:num<-nan
   #:negative
   #:/=
   #:number-of-elements
   #:ones
   #:outer
   #:pad
   #:partition
   #:power
   #:prod
   #:put-along-axis
   #:quantize
   #:quantized-matmul
   #:radians
   #:real
   #:reciprocal
   #:remainder
   #:repeat
   #:reshape
   #:right-shift
   #:roll
   #:round
   #:rsqrt
   #:scatter
   #:scatter+
   #:scatter-max
   #:scatter-min
   #:scatter-prod
   #:segmented-mm
   #:sigmoid
   #:sign
   #:sin
   #:sinh
   #:slice
   #:softmax
   #:sort
   #:split
   #:sqrt
   #:square
   #:squeeze
   #:stack
   #:std
   #:stop-gradient
   #:swap-axes
   #:sum
   #:take
   #:tan
   #:tanh
   #:tensordot
   #:tile
   #:topk
   #:trace
   #:transpose
   #:tri
   #:tri-upper
   #:tri-lower
   #:unflatten
   #:var
   #:view
   #:where
   #:zeros

   ;; metal
   #:metal-available-p
   #:metal-device-info
   #:metal-device-architecture
   #:metal-device-max-buffer-length
   #:metal-device-max-recommanded-working-set-size
   #:metal-device-memory-size
   ))

(defpackage #:mlx-user
  (:use :mlx-cl))

;;;; package.lisp ends here
