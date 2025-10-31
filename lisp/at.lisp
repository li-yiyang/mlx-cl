;;;; at.lisp --- Indexing `mlx-array'.

(in-package :mlx-cl)

;;; Dev Note:

(deftype ~ ()
  "Type of ranges. "
  `(cons (eql ~) (cons integer (cons integer (cons integer null)))))

(flet ((reorder~ ([start]-stop-[step]-&key-step)
         (multiple-value-bind (args plist)
             (split-args-keys [start]-stop-[step]-&key-step)
           (let ((step (getf plist :step 1)))
             (case (cl:length args)
               (0 `(:* :* ,step))
               (1 `(:* ,@args ,step))
               (2 `(,@args    ,step))
               (3 args)
               (t (error "Invalid range syntax of ~S. "
                         [start]-stop-[step]-&key-step)))))))

  (defmacro ~ (&rest [start]-stop-[step]-&key-step)
    "Generate a range specification (~ start stop step).
Return a `~' for range.

Syntax:

  (~ [START] STOP [STEP] &key STEP)

Parameters:
+ START: range start,
  if given is `:*', will be 0
+ STOP: range stop,
  if given is `:*', will be -1
+ STEP: range step (default 1)

Examples:
+ range [0, 5), step 1: (~ 5)
+ range [0, 5), in reverse (~ 5 :step -1)
+ all in reverse (~ :step -1)

Note:
+ (~ start stop step :step step0) will overwrites step0,
  return value is (~ start stop step).
"
    `(list '~ ,@(reorder~ [start]-stop-[step]-&key-step)))

  (defmacro ~~ (&rest [start]-stop-[step]-&key-step)
    "Generate a range specification (~~ start stop step).
Return a `~~' for range.

Syntax:

  (~~ [START] STOP [STEP] &key STEP)

Parameters:
+ START: range start,
  if given is `:*', will be 0
+ STOP: range stop,
  if given is `:*', will be -1
+ STEP: range step (default 1)

Note:
+ (~ start stop step :step step0) will overwrites step0,
  return value is (~ start stop step).

See `~'.
"
    `(list '~~ ,@(reorder~ [start]-stop-[step]-&key-step))))

(defparameter *slice-spec-rules*
  (make-hash-table :test 'eql)
  "Rules of slice EXPR.

Key: symbol/keyword of Rule names
Val: function with lambda list (shape &rest args),
     should return (~ START STOP STEP) or sequence of
     integer index for taking

See `mlx::evaluate-slice-spec' and `mlx::defmlx-slice'
for how the rules are applied and defined.
")

(defun evaluate~ (shape start stop step)
  "Evaluate ~ form to MLX acceptable form.
Return ~ expression.

Rules:
+ (~ start stop step>0): the index should be normalized into [0, shape)
  and START < STOP
+ (~ start stop step<0): the index should be normalized into [-shape, 0)
  and START > STOP
"
  (declare (type (integer 0) shape)
           (type (or integer (eql :*)) start stop)
           (type integer step))
  (cond ((cl:> step 0)
         (let ((stop  (cond ((eql stop :*) shape)
                            ((cl:< stop 0) (cl:+ shape stop))
                            (t stop)))
               (start (cond ((eql start :*) 0)
                            ((cl:< start 0) (cl:+ shape start))
                            (t start))))
           (assert (cl:< -1 start stop))
           (list '~ start stop step)))
        ((cl:< step 0)
         (let ((stop*  (cond ((eql stop :*)  (cl:- (cl:1+ shape)))
                             ((cl:< stop 0)  (cl:- stop 1))
                             (t              (cl:- stop shape 1))))
               (start* (cond ((eql start :*) -1)
                             ((cl:< start 0) (cl:- start 1))
                             (t              (cl:- start shape 1)))))
           (assert (cl:> 0 stop* start*))
           (list '~ stop* start* step)))
        (t (error "(~ ~A ~A ~A) should have non-zero step. "
                  start stop step))))

(defun evaluate~~ (shape start stop step)
  "Evaluate ~~ form to MLX acceptable form.
Return ~ expression.

Rules:
see `mlx::evaluate~', the difference is that STOP is included
"
  (declare (type (integer 0) shape)
           (type (or integer (eql :*)) start stop)
           (type integer step))
  (cond ((cl:> step 0)
         (let ((stop  (cond ((eql stop :*) shape)
                            ((cl:< stop 0) (cl:+ shape stop 1))
                            (t (cl:1+ stop))))
               (start (cond ((eql start :*) 0)
                            ((cl:< start 0) (cl:+ shape start))
                            (t start))))
           (assert (cl:< -1 start stop))
           (list '~ start stop step)))
        ((cl:< step 0)
         (let ((stop*  (cond ((eql stop :*)  (cl:- (cl:1+ shape)))
                             ((cl:< stop 0)  (cl:- stop))
                             (t              (cl:- stop shape))))
               (start* (cond ((eql start :*) -1)
                             ((cl:< start 0) (cl:- start 1))
                             (t              (cl:- start shape 1)))))
           (assert (cl:> 0 stop* start*))
           (list '~ stop* start* step)))
        (t (error "(~ ~A ~A ~A) should have non-zero step. "
                  start stop step))))

(defun evaluate-slice-spec (expr shape)
  "Evaluate slice EXPR for `at'.
Return values are { (~ START STOP STEP) | sequence } and takep.

Basic Rules:
+ list:
  + (~  start stop step): see evaluate~
  + (~~ start stop step): see evaluate~~
  + others, see `mlx::*slice-spec-rules*'.
+ integer:  (~ integer (1+ integer) 1)
+ rational:
  + positive: (~ 0 (ceiling rational * shape) 1)
  + negative: (~ (floor rational * shape) shape 1)
+ symbol:
  be turned into (symbol) and equal to list EXPR
  See `mlx::*slice-spec-rules*'
"
  (declare (type (cl:or symbol list integer rational mlx-array) expr)
           (type integer shape))
  (labels ((normalize (expr)
             (etypecase expr
               (keyword  (apply-rule expr ()))
               (integer  `(~ ,expr ,(1+ expr) 1))
               (null     `(~ 0 ,shape 1))
               (list
                (let ((car (car expr)))
                  (etypecase car
                    (keyword (apply-rule (car expr) (cdr expr)))
                    (symbol  (ecase (car expr)
                               (~  (apply #'evaluate~  shape (cdr expr)))
                               (~~ (apply #'evaluate~~ shape (cdr expr)))))
                    (integer (map 'simple-vector (lambda (x) (cl:mod x shape)) expr)))))
               (sequence
                (map 'simple-vector (lambda (x) (cl:mod x shape)) expr))
               (rational
                (cond ((cl:< -1 expr 0)
                       (the ~ `(~ ,(cl:floor (cl:* shape (cl:- expr))) ,shape 1)))
                      ((cl:<  0 expr 1)
                       (the ~ `(~ 0 ,(cl:ceiling (cl:* shape expr)) 1)))
                      (t (error "Rational split ~A should between (-1, 0) and (0, 1)."
                                expr))))
               (mlx-array
                (case (dim expr)
                  (0 (reshape expr '(1)))
                  (1 expr)
                  (t (error "Expecting mlx-array as index or sequence of index, but got ~A"
                            expr))))))
           (apply-rule (rule args)
             (let* ((rule (cl:or (gethash rule *slice-spec-rules*)
                                 (error "Unknown slice rule for ~S. "
                                        rule)))
                    (res  (apply rule shape args)))
               (normalize res))))
    (normalize expr)))

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

Dev Note: the slice shortcuts are defined in `mlx::*slice-spec-rules*'. "))
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
            (when take
              (loop :for (axis . idxs) :in take
                    :do (setf slice (with-mlx-op "mlx_take_axis"
                                      slice
                                      idxs
                                      (axis :int)))))
            (squeeze slice)))))))

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

;; TODO: #mlx-cl #syntax
;; introduce `~' syntax for range:
;; + (arange (~ start stop step))
;; + (arange start stop step)
(defun %arange (start stop step dtype
                &aux
                  (start* (num<- start))
                  (stop*  (num<- stop))
                  (step*  (num<- step))
                  (dtype! (ensure-mlx-dtype dtype)))
  (declare (type real start stop step))
  (let ((coerce (%mlx-dtype-coerce :float64)))
    (with-mlx-op "mlx_arange"
      ((funcall coerce start*) :double)
      ((funcall coerce stop*)  :double)
      ((funcall coerce step*)  :double)
      (dtype!                  mlx-dtype))))

(defmacro arange (&rest range-specification)
  "Generate ranges of numbers from START to STOP by STEP.
Return 1-D `mlx-array' of shape ( ⌊(START - STOP) / STEP⌋ ).

Syntax:

    (arange [START] STOP [STEP] &key STEP DTYPE)

    + (arange stop       &key dtype step)
    + (arange start stop &key dtype step)
    + (arange start stop step &key dtype)

Definition:
`arange' is equal to calling:

    (mlx-array (loop :for n :from START :below STOP :by STEP
                     :collect n)
               :dtype DTYPE)

Examples:

    (arange 5)         ; (arange STOP)               => [0 1 2 3 4]
    (arange 1 5)       ; (arange START STOP)         => [1 2 3 4]
    (arange 5 :step 2) ; (arange START :step STEP)   => [0 2 4]
    (arange 2 :dtype :float32)
                       ; (arange START :dtype DTYPE) => [0.0 1.0]
"
  (multiple-value-bind (range keys)
      (split-args-keys range-specification)
    (let ((len (cl:length range)))
      (cond ((cl:= len 1)
             (destructuring-bind
                 (&key (dtype *default-mlx-dtype*) (step  1))
                 keys
               `(%arange 0 ,@range ,step ,dtype)))
            ((cl:= len 2)
             (destructuring-bind
                 (&key (dtype *default-mlx-dtype*) (step  1))
                 keys
               `(%arange ,@range ,step ,dtype)))
            ((cl:= len 3)
             (destructuring-bind
                 (&key (dtype *default-mlx-dtype*))
                 keys
               `(%arange ,@range ,dtype)))
            (t (error "Unknown `arange' syntax: ~A. " range-specification))))))


;;; Slice Rules

(defmacro defmlx-slice (method (shape &rest lambda-list) &body body)
  "Define a slice METHOD on SHAPE.

Syntax:

    (defmlx-slice :method (shape ...)
      [docstring]
      &body body)

Parameters:
+ METHOD: keyword to be defined as slice method
+ SHAPE: shape of the given axis
+ LAMBDA-LIST: additional parameters to generate slice pattern
+ BODY: to generate slice pattern

Dev Note:
The slice method should return (~ START STOP STEP) or sequence of integer. "
  (declare (type keyword method)
           (type symbol  shape))
  (multiple-value-bind (doc plist body) (split-doc-body body)
    `(progn
       (setf (gethash ',method *slice-spec-rules*)
             (lambda (,shape ,@lambda-list)
               ,(apply #'gen-doc doc plist)
               (declare (type integer ,shape))
               ,@body)))))

(defmethod documentation (method (doc-type (eql 'slice)))
  "Return documentation"
  (declare (type keyword method))
  (documentation (gethash method *slice-spec-rules*) 'function))

(defmlx-slice :first (shape &optional (n 1))
  "Get first N slice of mlx-array. "
  :return "(~ 0 (min N SHAPE) 1/-1) (-1 if N < 0)"
  :parameters ((n "integer of first N elements
 + if N is negative, slice in reverse
 + if N is zero, raise error"))
  (declare (type integer n))
  (cond ((cl:< n 0) `(~ 0 ,(cl:min (cl:- n) shape) -1))
        ((cl:> n 0) `(~ 0 ,(cl:min       n  shape)  1))
        (t (error "N=~A in (:first N) should not be zero. " n))))

(defmlx-slice :last (shape &optional (n 1))
  "Get last N slice of mlx-array. "
  :return "(~ (max 0 (- SHAPE N)) SHAPE 1)"
  :parameters ((n "integer of last N elements
 + if N is negative, slice in reverse
 + if N is zero, raise error"))
  (declare (type integer n))
  (cond ((cl:< n 0) `(~ ,(cl:max 0 (cl:+ shape n)) ,shape -1))
        ((cl:> n 0) `(~ ,(cl:max 0 (cl:- shape n)) ,shape  1))
        (t (error "N=~A in (:last N) should not be zero. " n))))

(macrolet ((nth* (&rest keyword-n)
             `(progn ,@(loop :for (keyword n) :in keyword-n
                             :collect
                             `(defmlx-slice ,keyword (shape)
                                ,(format nil "Get ~Dth element of mlx-array. " n)
                                :return (format nil "(~ ~D ~D 1)" n (cl:1+ n))
                                (declare (ignorable shape))
                                '(~ ,n ,(cl:1+ n) 1))))))
  (nth* (:second 2)
        (:third  3)
        (:fourth 4)
        (:fifth  5)
        (:sixth  6)
        (:seventh 7)
        (:eighth  8)
        (:ninth   9)
        (:tenth   10)))

(defmlx-slice :middle (shape &optional (n 1))
  "Get N (default 1) elements in the middle of mlx-array. "
  :return "(~~ (max 0 middle-half) (max shape middle+half) 1)"
  :parameters ((n "integer of middle N elements
 + if N is negative, slice in reverse
 + if N is zero, raise error"))
  :examples (("Middel element"
              (:middle 1)
              "or :middle")
             ("Middle 2 elements"
              (:middle 2)
              "(~ (- middle 1) (+ middle 1) 1)"))
  (declare (type integer n))
  (let* ((middle (cl:floor shape 2))
         (m      (cl:abs n))
         (left   (cl:floor m 2))
         (right  (cl:- m left)))
    `(~
      ,(cl:- middle left) ,(cl:+ middle right)
      ,(cond ((cl:< n 0) -1)
             ((cl:> n 0)  1)
             (t (error "N=~A in (:middle N) should not be zero. " n))))))

(defmlx-slice :* (shape)
  "Get all elements of axis in mlx-array. "
  :return "(~ 0 SHAPE 1)"
  :notes "See also `:all'. "
  `(~ 0 ,shape 1))

(defmlx-slice :all (shape)
  "Get all elements of axis in mlx-array. "
  :return "(~ 0 SHAPE 1)"
  :notes "See also `:*'. "
  `(~ 0 ,shape 1))

(defmlx-slice :reverse (shape)
  "Get all elements in reverse of axis in mlx-array. "
  :return "(~ 0 SHAPE -1)"
  `(~ 0 ,shape -1))

(defmlx-slice :skip (shape &optional (n 1) &key (start 0))
  "Get elements on axis, but skip every N (1) element. "
  :return "(~ START :* (1+ N))"
  :parameters ((n "skip every N elements (default 1)
 + if N is negative, take N as reversed")
               (start "start position (default 0)"))
  :examples (("Skip every 1, this is equal to `:even'. "
              (:skip 1)
              "equal to `:skip'")
             ("Skip every 1 starting from 1, this is equal to `:odd'. "
              (:skip 1 :start 1)
              "equal to `(:skip :start 1)'"))
  (declare (type integer n start))
  `(~ ,start :* ,(cl:1+ n)))

(defmlx-slice :odd (shape)
  "Get odd index elements on axis, this is equal to (:skip 1 :start 1). "
  :return "(~ 1 SHAPE 2)"
  `(~ 1 ,shape 2))

(defmlx-slice :even (shape)
  "Get even index elements on axis, this is equal to (:skip 0 :start 0). "
  :return "(~ 0 SHAPE 2)"
  `(~ 0 ,shape 2))

;;;; at.lisp ends here
