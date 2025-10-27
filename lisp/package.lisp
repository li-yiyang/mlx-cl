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
   #:bit-and #:bit-ior #:bit-not #:bit-xor
   #:logand #:logior #:lognot #:logxor
   #:identity #:imagpart #:realpart #:ash
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
   #:~
   #:~~
   #:at*
   #:at

   ;;; config
   #:*relative-tolerance*
   #:*absolute-tolerance*
   #:*nan-equal-p*
   #:*keep-dim-p*

   ;;; wraping
   #:add #:+
   #:sub #:-
   #:div #:*
   #:mul #:/

   ;; (op ARRAY)
   #:abs #:erfinv #:square #:sqrt #:rsqrt
   #:sin #:cos #:tan #:sinh #:cosh #:tanh
   #:asin #:acos #:atan #:asinh #:acosh #:atanh
   #:expm1 #:loge #:log10 #:log2 #:sign
   #:imagpart #:realpart #:copy
   #:degrees #:radians
   #:inf-p #:finite-p #:nan-p #:neg-inf-p #:pos-inf-p
   #:! #:¬ #:negative #:reciprocal #:erf #:stop-gradient
   #:hadamard-transform #:conjugate #:lognot #:log1+
   #:sigmoid

   ;; (op ARRAY &optional (DTYPE *default-mlx-dtype*))
   #:as-type #:view

   ;; (op A B)
   #:add #:sub #:mul #:div #:mod #:remainder #:expt
   #:matmul #:inner #:outer #:kron #:logaddexp
   #:atan2 #:op2< #:op2<= #:op2> #:op2>=
   #:op2and #:logical-and #:op2or #:logical-or
   #:logand #:bit-and #:logxor #:bit-xor #:logior #:bit-ior #:bit-or

   ;; (op ELEM &rest MORE-ELEM)
   #:+ #:- #:* #:/ #:&& #:∧ #:|| #:∨

   ;; (op ELEM &rest MORE-ELEM)
   #:at-least-1d #:at-least-2d #:at-least-3d

   ;; (op ARRAY &key axis/axes (ddof 0) (keep-dim-p *keep-dim-p*))
   #:std #:var

   ;; (op ARRAY &key axis/axes (keep-dim-p *keep-dim-p*))
   #:all #:any #:maximum #:minimum #:mean #:prod #:logsumexp #:sum

   ;; (op ARRAY &key AXIS/AXES)
   #:softmax #:squeeze

   ;; (op ARRAY &key AXIS (KEEP-DIM-P *keep-dim-p*))
   #:argmin #:argmax

   ;; (op ARRAY &key axis)
   #:argsort #:sort #:stack

   ;; (op (ARRAY :dim 2) &key diag/diagonal)
   #:tri-lower #:tril #:tri-upper #:triu

   ;; (op ARRAY SHAPE &key DTYPE)
   #:ones #:zeros

   ;; (op ARRAY &key AXIS REVERSE INCLUDSIVE)
   #:cummax #:cummin #:cumprod #:cumsum #:logsumsumexp

   ;; (op ELEM &rest MORE-ELEM)
   #:=

   ;; (op ELEM &rest MORE-ELEM)
   #:<= #:< #:>= #:>

   ;; (op ELEM &rest MORE-ELEM)
   #:~= #:/=

   ;;; op manual bindings
   #:addmm
   #:op2~=
   #:arange
   #:argpartition
   #:partition
   #:as-strided
   #:atleast
   #:broadcast-to
   #:floor
   #:ceiling
   #:clip
   #:concat
   #:contiguous
   #:diag
   #:einsum
   #:expand-dims
   #:eye
   #:flatten
   #:full
   #:identity
   #:linspace
   #:meshgrid
   #:moveaxis
   #:num<-nan
   #:put-along-axis
   #:quantize
   #:quantized-matmul
   #:repeat
   #:ash
   #:roll
   #:round
   #:scatter
   #:op2=
   #:op2/=
   #:reshape
   #:slice
   #:slice-update
   #:split
   #:swap-axes
   #:take
   #:tensordot
   #:tile
   #:topk
   #:transpose
   #:tr
   #:tri
   #:unflatten

   ;; metal
   #:metal-available-p
   #:metal-device-info
   #:metal-device-architecture
   #:metal-device-max-buffer-length
   #:metal-device-max-recommanded-working-set-size
   #:metal-device-memory-size

   ;;;; io
   #:load-from
   #:save-to
   #:save
   ))

(defpackage #:mlx-user
  (:use :mlx-cl))

;;;; package.lisp ends here
