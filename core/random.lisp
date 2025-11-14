;;;; random.lisp ---- Random

(in-package :mlx-cl.random)


;;; Seed

(mlx::defmlx-func prng-key (seed)
  "Get a PRNG key from a SEED"
  :return "`mlx-array' as PRNG key"
  :parameters ((seed "Seed for the PRNG"))
  (mlx::with-mlx-op "mlx_random_key"
    ((the integer (mlx::num<- seed)) :uint64)))

(mlx::defmlx-func split-prng-key (key &optional (num 2 num?))
  "Split a PRNG key into sub keys. "
  :return "return `mlx-array' with first axis dimension as NUM. "
  :parameters ((key "input PRNG key to split")
               (num "number of sub keys (default `2')"))
  (mlx::with-mlx-op "mlx_random_split_num"
    ((mlx::mlx-object-pointer
      (if (integerp key)
          (prng-key key)
          (mlx-array key :dtype :uint32)))
     :pointer)
    ((the integer (if num? (mlx::num<- num) num)) :int)))

;; TEST: #seed
(mlx::defmlx-func seed (seed &aux (s (the integer (mlx::num<- seed))))
  "Seed the global PRNG. "
  :parameters ((seed "seed for the global PRNG"))
  (mlx::ensure-success "mlx_random_seed"
    :int s))

(declaim (inline key<-))
(defun key<- (key)
  (etypecase key
    (null      (cffi:null-pointer))
    (integer   (mlx::mlx-object-pointer (prng-key key)))
    (mlx-array (mlx::mlx-object-pointer (mlx-array key :dtype :uint32)))))


;;; Distributions

(mlx::defmlx-func bernoulli (&key (p 0.5) shape key
                             &aux (shape! (if shape
                                              (mlx::shape<- shape)
                                              (shape p))))
  "Generate Bernoulli random values. "
  :return "`mlx-array' of random bool"
  :note "
The values are sampled from the bernoulli distribution with parameter P.
The parameter P can be a float or array and must be broadcastable to SHAPE."
  :parameters ((p     "parameter of Bernounlli distribution (default 0.5)")
               (shape "shape of the output (default (shape P))")
               (key   "a PRNG key (default nil)"))
  :definition "
"
  (mlx::with-foreign<-sequence (shape* shape! :int len)
    (mlx::with-mlx-op "mlx_random_bernoulli"
      p
      (shape*         :pointer)
      (len            :size)
      ((key<- key)    :pointer))))

(mlx::defmlx-func categorical (logits
                               &key
                                 (axis    -1  axis?)
                                 (shape   nil shape?)
                                 (samples nil samples?)
                                 key
                               &aux
                                 (axis! (if axis? (mlx::axis<- axis) axis)))
  "Sample from a categorical distribution. "
  :return "SHAPE sized output array with dtype `:uint32'"
  :notes "
The values are sampled from the categorical distribution specified by
the unnormalized values in LOGITS. At most one of SHAPE or SAMPLES can
be specified. If both are `nil', the output has the same shape as LOGITS
with the AXIS dimension removed. "
  :parameters ((logits  "unnormalized categorical distribution(s)")
               (axis    "axis which specifies the distribution (default -1)")
               (shape   "shape of the output. (default `nil')
This must be broadcast compatible with (shape LOGITS) with the axis
dimension removed. ")
               (samples "number of samples to draw from each of the categorical distributions in LOGITS, the output will have SAMPLES in the last dimension. (default `nil')")
               (key     "a PRNG key (default `nil')"))
  (declare (type integer axis!))
  (when (and shape? samples?)
    (error "At most one of SHAPE or SAMPLES can be specified. "))
  (cond (shape?
         (mlx::with-foreign<-sequence (shape* (mlx::shape<- shape) :int len)
           (mlx::with-mlx-op "mlx_random_categorical_shape"
             logits
             (axis!       :int)
             (shape*      :pointer)
             (len         :size)
             ((key<- key) :pointer))))
        (samples?
         (mlx::with-mlx-op "mlx_random_categorical_num_samples"
           logits
           (axis!                              :int)
           ((the integer (mlx::num<- samples)) :int)
           ((key<- key)                        :pointer)))
        (t
         (mlx::with-mlx-op "mlx_random_categorical"
           logits
           (axis!        :int)
           ((key<- key)  :pointer)))))

(mlx::defmlx-func gumbel (&key shape (dtype *default-mlx-dtype* dtype?) key
                          &aux (dtype! (if dtype? (mlx::ensure-mlx-dtype dtype) dtype)))
  "Sample from the standard Gumbel distribution. "
  :note "The values are sampled from a standard Gumbel distribution which CDF exp(-exp(-x))."
  :parameters ((shape "shape of the output")
               (dtype "`mlx-dtype' of the output (default `*default-mlx-dtype*')")
               (key   "PRNG key (default `nil')"))
  (mlx::with-foreign<-sequence (shape* (mlx::shape<- shape) :int len)
    (mlx::with-mlx-op "mlx_random_gumbel"
      (shape*      :pointer)
      (len         :size)
      (dtype!      mlx-dtype)
      ((key<- key) :pointer))))

(mlx::defmlx-func normal (&key shape (dtype *default-mlx-dtype* dtype?)
                            key
                            μ mu    loc   mean
                            σ sigma scale std
                          &aux
                            (dtype! (if dtype?
                                        (mlx::ensure-mlx-dtype dtype)
                                        dtype))
                            (mean!  (or μ mu    loc   mean))
                            (std!   (or σ sigma scale std)))
  "Generate normally distributed random numbers. "
  :return "`mlx-array' of random values. "
  :note "If MU and SIGMA are not provided, the standard normal is used.
That means x ~ N(0, 1) for real numbers and Re(x), Im(x) sim N(0, 1/2) for complex numbers. "
  :aliases (gaussian)
  :parameters ((shape "shape of the output (default `()')")
               (dtype "type of the output (default `*default-mlx-dtype*')")
               #-lispworks
               (μ     "N(mu, sigma) mean of the deistribution (default `nil')")
               #-lispworks
               (σ     "N(mu, sigma) standard deviation of the distribution (default `nil')")
               (key   "PRNG key (default `nil')")
               (mean  "alias of mu, mean")
               (std   "alias of sigma, standard deviation")
               (mu    "alias of mu, mean")
               (sigma "alias of sigma, standard deviation")
               (loc   "alias of mu, mean")
               (scale "alias of sigma, standard deviation"))
  (if (and (typep mean! '(or null number))
           (typep std!  '(or null number)))
      (mlx::with-foreign<-sequence (shape* (mlx::shape<- shape) :int len)
        (mlx::with-mlx-op "mlx_random_normal"
          (shape*                                               :pointer)
          (len                                                  :size)
          (dtype!                                               mlx-dtype)
          ((if (null mean!) 0.0f0 (coerce mean! 'single-float)) :float)
          ((if (null std!)  1.0f0 (coerce std!  'single-float)) :float)
          ((key<- key)                                          :pointer)))
      (mlx::with-foreign<-sequence (shape* (mlx::shape<- shape) :int len)
        (mlx::with-mlx-op "mlx_random_normal_broadcast"
          (shape*                                     :pointer)
          (len                                        :size)
          (dtype!                                     mlx-dtype)
          ((if (null mean!)
               (cffi:null-pointer)
               (mlx::mlx-object-pointer (mlx-array mean!)))
           :pointer)
          ((if (null std!)
               (cffi:null-pointer)
               (mlx::mlx-object-pointer (mlx-array std!)))
           :pointer)
          ((key<- key)                                :pointer)))))

;; TODO: #mlx-cl.random #missing
(mlx::defmlx-func multivariate-normal
    (mean cov &key shape (dtype *default-mlx-dtype* dtype?) key
     &aux (dtype! (if dtype? (mlx::ensure-mlx-dtype dtype) dtype)))
  "Generate jointly-normal random samples given a mean and covariance. "
  :return "`mlx-array' of random values"
  :parameters ((mean "mean of the distribution
should be an `mlx-array' of shape (... n)")
               (cov  "convariance matrix of the distribution
should be an `mlx-array' of shape (... n n).
the batch shape `...' must be broadcast-compatible with MEAN

The matrix cov must be positive semi-definite.
The behavior is undefined if it is not.
The only supported dtype is float32.")
               (shape "result shape (default `()')
 + (): determined by broadcasting the batch shapes of MEAN and COV
 + shape: the output shape must be broadcast-compatiable with
   (shape MEAN :axis (~ :* -1)) and (shape COV :axis (~ :* -2)). ")
               (dtype "output type (default `*default-mlx-dtype*')")
               (key   "PRNG key (default `nil')"))
  (mlx::with-foreign<-sequence (shape* shape :int len)
    (mlx::with-mlx-op "mlx_random_multivariate_normal"
      mean
      cov
      (shape*      :pointer)
      (len         :size)
      (dtype!      mlx-dtype)
      ((key<- key) :pointer))))

(mlx::defmlx-func truncated-normal (lower upper
                                    &key shape key
                                      (dtype *default-mlx-dtype* dtype?)
                                    &aux (dtype! (if dtype?
                                                     (mlx::ensure-mlx-dtype dtype)
                                                     dtype)))
  "Generate values from a truncated normal distribution. "
  :parameters ((lower "lower bound of the domain")
               (upper "upper bound of the domain")
               (shape "shape of the output (default `()')")
               (dtype "data type of the output (default `*default-mlx-dtype*')")
               (key   "PRNG key (default `nil')"))
  :note "
The values are sampled from the truncated normal distribution
on the domain (LOWER, UPPER). The bounds LOWER and UPPER can
be scalars or arrays and must be broadcastable to SHAPE."
  (mlx::with-foreign<-sequence (shape* (mlx::shape<- shape) :int len)
    (mlx::with-mlx-op "mlx_random_truncated_normal"
      lower
      upper
      (shape*      :pointer)
      (len         :size)
      (dtype!      mlx-dtype)
      ((key<- key) :pointer))))

(mlx::defmlx-func uniform (&rest [low]-[high]-&key-shape-key-dtype)
  "Generate uniformly distributed random numbers. "
  :return "`mlx-array' with random values [LOW, HIGH). "
  :syntax "(uniform [low] [high] &key shape dtype key)"
  :examples (("no LOW and HIGH"
              (uniform)
              "=> [0, 1)")
             ("given HIGH"
              (uniform high)
              "=> [0, high)")
             ("given LOW and HIGH"
              (uniform low high)
              "=> [low, high)"))
  :parameters ((low   "lower bound of the distribution (default 0)")
               (high  "upper bound of the distribution (default 1)")
               (shape "shape of the output (default `()')")
               (dtype "`mlx-dtype' of the output (default `*default-mlx-dtype*')")
               (key   "PRNG key (default `nil')"))
  (multiple-value-bind (args keys)
      (mlx::split-args-keys [low]-[high]-&key-shape-key-dtype)
    (destructuring-bind (&key shape key (dtype *default-mlx-dtype* dtype?)) keys
      (let ((arglen (length args))
            (shape! (mlx::shape<- shape))
            (dtype  (if dtype? (mlx::ensure-mlx-dtype dtype) dtype))
            low high)
        (cond ((cl:= arglen 0)
               (setf low  0.0f0
                     high 1.0f0))
              ((cl:= arglen 1)
               (setf low  0.0f0
                     high (first args)))
              ((cl:= arglen 2)
               (setf low  (first  args)
                     high (second args)))
              (t (error "Invalid number of LOW HIGH arguments. ")))
        (mlx::with-foreign<-sequence (shape* shape! :int len)
          (mlx::with-mlx-op "mlx_random_uniform"
            low
            high
            (shape*      :pointer)
            (len         :size)
            (dtype       mlx-dtype)
            ((key<- key) :pointer)))))))

(mlx::defmlx-func randint (&rest [low]-[high]-&key-shape-key-dtype)
  "Generate random integers from the given interval. "
  :return "`mlx-array' of SHAPE whose array falls between [LOW, HIGH). "
  :parameters ((low   "lower bound of the interval (default `0')")
               (high  "upper bound of the interval (default `50')")
               (shape "shape of the output (default `()')")
               (dtype "type of the output (default `*default-mlx-int-dtype*')")
               (key   "PRNG key (default `nil')"))
  :note "the lower and upper bound can be scalars or `mlx-array' and shoud be broadcastable to SHAPE. "
  (multiple-value-bind (args keys)
      (mlx::split-args-keys [low]-[high]-&key-shape-key-dtype)
    (destructuring-bind (&key shape key (dtype *default-mlx-int-dtype* dtype?)) keys
      (let ((arglen (length args))
            (dtype  (if dtype? (mlx::ensure-mlx-dtype dtype) dtype))
            (shape! (mlx::shape<- shape))
            low high)
        (declare (type (member :uint8 :uint16 :uint32 :uint64
                               :int8  :int16  :int32  :int64)
                       dtype))
        (cond ((cl:= arglen 0)
               (setf low  0
                     high 100))
              ((cl:= arglen 1)
               (setf low  0
                     high (first args)))
              ((cl:= arglen 2)
               (setf low  (first  args)
                     high (second args)))
              (t (error "Invalid number of LOW HIGH arguments. ")))
        (mlx::with-foreign<-sequence (shape* shape! :int len)
          (mlx::with-mlx-op "mlx_random_randint"
            low
            high
            (shape*      :pointer)
            (len         :size)
            (dtype       mlx-dtype)
            ((key<- key) :pointer)))))))

(mlx::defmlx-func laplace
    (&key shape key (mean 0.0 mean?) (scale 1.0 scale?)
       (dtype *default-mlx-dtype* dtype?)
     &aux
       (dtype! (if dtype? (mlx::ensure-mlx-dtype dtype) dtype))
       (mean!  (if mean?  (coerce (mlx::num<- mean)  'single-float) mean))
       (scale! (if scale? (coerce (mlx::num<- scale) 'single-float) scale)))
  "Sample numbers from a Laplace distribution. "
  :parameters ((shape "shape of the output (default `()')")
               (dtype "`mlx-dtype' of the output (default `*default-mlx-dtype*')")
               (mean  "mean of the distribution (default `0.0')")
               (scale "scale parameter of Laplace distribution (default `1.0')")
               (key   "PRNG key"))
  (mlx::with-foreign<-sequence (shape* (mlx::shape<- shape) :int len)
    (mlx::with-mlx-op "mlx_random_laplace"
      (shape*      :pointer)
      (len         :size)
      (dtype!      mlx-dtype)
      (mean!       :float)
      (scale!      :float)
      ((key<- key) :pointer))))


;;; Permutation

(mlx::defmlx-method permutation (x &key (axis 0 axis?) key
                                 &aux (axis! (if axis? (mlx::axis<- axis) axis)))
  "Generate a random permutation or permute the entries of an array. "
  :return "`mlx-array' of generated random permutation or randomly permuted input array. "
  :parameters ((x    "input to permutate
 + integer: make `(arange X)'
 + mlx-array: permutate on AXIS")
               (axis "axis to permute along (default `0')
 + if given `nil': would flatten the array first and then reshape to original")
               (key  "PRNG key (default `nil')"))
  :methods ((((x integer) &key axis key)
             "Equal to calling (permutation (arange X)). "
             (declare (ignore axis))
             (mlx::with-mlx-op "mlx_random_permutation_arange"
               (x           :int)
               ((key<- key) :pointer))))
  (if (null axis)
      (let ((shape (shape x)))
        (reshape (permutation (flatten x) :axis axis :key key) shape))
      (mlx::with-mlx-op "mlx_random_permutation"
        x
        (axis!       :int)
        ((key<- key) :pointer))))

;;;; random.lisp ends here
