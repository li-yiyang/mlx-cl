;;;; wrap.lisp ---- Wrapper tools to create mlx ops methods -*- mlx-cl-test-file: "core/ops.lisp" -*-

(in-package :mlx-cl)

(defmacro with-op-template ((op cffi docs &rest parameters) expr &body body)
  "Generate mlx-cl operator template.
The BODY element should be able to expanded by LAMBDA-LIST,
the expansion rule is defined as EXPR.

Syntax:

    (with-op-template (OP CFFI DOCS
                       (x \"documentation of x\")
                       (y \"documentation of y\")
                       :debug DEBUG)
        EXPR
      &body BODY)

Parameters:
+ OP: variable binded with the name of operator used to expand EXPR
+ CFFI: variable binded with the string of mlx function postfix
  (default to be (string-downcase OP))

  Example: use `mlx::sconc' to concat string
+ DOCS: variable binded with a list of (short &key ...) for
  `mlx::gen-doc' and `mlx::defmlx-method' usage
+ PARAMETERS: elements should be (variable documentation-of-variable)
+ DEBUG: if non-nil, print expressions without evaluating them
+ EXPR: how to expand the op template
+ BODY: elements should be (op . docstring)
"
  (let ((temp    (gensym "TEMP"))
        (op*     (gensym "OP"))
        (short   (gensym "SHORT")))
    (multiple-value-bind (para plist)
        (split-args-keys parameters)
      (let* ((fn `(,temp
                   (,op* &optional ,short &rest ,docs)
                   (destructuring-bind (,op &optional (,cffi (string-downcase ,op)))
                       (listfy ,op*)
                     (declare (ignorable ,op ,cffi))
                     (setf (getf ,docs :parameters)
                           (alist-union ',para (getf ,docs :parameters)))
                     (let ((,docs (cons ,short ,docs)))
                       ,expr)))))
        (if (getf plist :debug)
            `(flet (,fn)
               ,@(loop :for arg :in body :collect `(print (apply #',temp ',arg))))
            `(macrolet (,fn)
               ,@(loop :for arg :in body :collect `(,temp ,@arg))))))))

(defmacro with-mlx-op (name &body arg-type?)
  "Wrap a MLX calling with NAME and ARG-TYPE?.

Syntax:

    (with-mlx-op
      { name | (name &key alloc wrap) }
      { arg | (arg type) }*
      )

Note:
this is equal to calling:

   (with-elem& (res& res :alloc alloc)
     (ensure-success name
       res&
       ...)
     (wrap-as-mlx-array res))
"
  (let ((res  (gensym "RES"))
        (res& (gensym "RES&")))
    (destructuring-bind
        (name &key (alloc 'mlx_array_new) (wrap 'wrap-as-mlx-array))
        (listfy name)
      `(with-elem& (,res ,res& :type  :pointer
                               :alloc (,alloc))
         (ensure-success ,name
           :pointer ,res&
           ,@(loop :for arg-type :in arg-type?
                   :for (arg type) := (if (listp arg-type)
                                          arg-type
                                          (list arg-type 'mlx-object))
                   :if (cl:and (cl:not (keywordp type))
                               (subtypep type 'mlx-object))
                     :collect :pointer
                     :and :collect `(mlx-object-pointer ,arg)
                   :else
                     :collect type
                     :and :collect arg)
           :pointer (mlx-object-pointer (default-mlx-stream)))
         (,wrap ,res)))))

;; TODO: #mlx-cl #testing
;; add tests on method `:examples' when gen-doc
(defmacro defmlx-method (name lambda-list &body body)
  "Define a MLX method with name.

Syntax:

    (defmlx-method name lambda-list
      \"short documentation\"     --+
      :return \"balabala\"          +--> use `mlx-cl::gen-doc' to generate documentation
      ... ;; key value pairs      --+
      :methods ((lambda-list . body) ...) -> methods patches
      :aliases (...)                      -> list of function alias
      ,@body)

Parameters:
+ NAME: a symbol as method name,
  if it's a list, should would be like (setf op)
+ LAMBDA-LIST: (arg... &key key... &aux ...)
+ DOCSTRING: a documentation string (MUST given)
+ BODY: method body

Note:
This would define a general method of name, the default method
expects input arguments as `mlx-array' object. By default,
would convert the input arguments via `mlx-array' into `mlx-array'
object and call default method.
"
  (multiple-value-bind (short plist body)
      (split-doc-body body)
    (multiple-value-bind (required optional rest keys others aux)
        (parse-lambda-list lambda-list)
      (let ((call-args `(,@(mapcar (lambda (arg)
                                     (if (listp arg) (car arg) `(mlx-array ,arg)))
                                   required)
                         ,@(mapcar #'atomize optional)))
            (call-keys (loop :for key :in (mapcar #'atomize keys)
                             :collect (intern (symbol-name key) :keyword)
                             :collect key))
            (arrayp    (cl:> (count-if #'symbolp required) 0)))
        ;; prepare parameter documents
        (setf (getf plist :parameters)
              (alist-union
               (docollect (arg required)
                 (if (listp arg)
                     (list (first arg) (format nil "`~A'" (second arg)))
                     (list arg "input `mlx-array'")))
               (docollect (arg (append optional keys))
                 (if (listp arg)
                     (list (first arg) (format nil "(default ~A)" (second arg)))
                     (list arg "")))
               (getf plist :parameters)))
        ;; prepare return documents
        (setf (getf plist :return) (cl:or (getf plist :return) "`mlx-array'"))
        ;; assert (setf name) if needed
        (when (listp name)
          (assert (cl:eq (first name) 'setf)))
        `(progn
           (defgeneric ,name (,@(mapcar #'atomize required)
                              ,@(when optional `(&optional ,@(mapcar #'atomize optional)))
                              ,@(when rest     `(&rest     ,rest))
                              ,@(when keys     `(&key      ,@(mapcar #'atomize keys)))
                              ,@(when others   `(&allow-other-keys)))
             (:documentation ,(if (cl:find-if (lambda (key) (getf plist key))
                                              '(:return :note :dev-note :examples
                                                :definition :parameters :syntax
                                                :aliases))
                                  (apply #'gen-doc short plist)
                                  short))
             ,@(loop :for method :in (getf plist :methods) :collect `(:method ,@method))
             (:method (,@(docollect (arg required) (if (listp arg) arg (list arg t)))
                       ,@(when optional `(&optional ,@(mapcar #'len<=2-listfy optional)))
                       ,@(when rest     `(&rest     ,rest))
                       ,@(when keys     `(&key      ,@(mapcar (if arrayp
                                                                  #'len<=2-listfy
                                                                  #'listfy)
                                                              keys)))
                       ,@(when others   `(&allow-other-keys))
                       ,@(unless arrayp `(&aux      ,@aux)))
               ,@(if arrayp
                     (if rest
                         (if (listp name)
                             `((declare (ignore ,@(mapcar #'atomize keys)))
                               (apply (function ,name) ,@call-args ,rest))
                             `((declare (ignore ,@(mapcar #'atomize keys)))
                               (apply #',name ,@call-args ,rest)))
                         (if (listp name)
                             `((setf (,(second name) ,@(rest call-args) ,@call-keys) ,(first call-args)))
                             `((,name ,@call-args ,@call-keys))))
                     body))
             ,@(when arrayp
                 `((:method (,@(docollect (arg required) (if (listp arg) arg (list arg 'mlx-array)))
                             ,@(when optional `(&optional ,@optional))
                             ,@(when rest     `(&rest     ,rest))
                             ,@(when keys     `(&key      ,@keys))
                             ,@(when others   `(&allow-other-keys))
                             ,@(when aux `(&aux ,@aux)))
                     ,@body))))
           ,@(docollect (alias (getf plist :aliases))
               `(defalias ,alias ,name))
           ',name)))))

(defmacro defmlx-func (name lambda-list &body body)
  "Like `defmlx-method' but defines function. "
  (multiple-value-bind (doc plist body)
      (split-doc-body body)
    `(progn
       (defun ,name ,lambda-list
         ,(apply #'gen-doc doc plist)
         ,@body)
       ,@(docollect (alias (getf plist :aliases))
           `(defalias ,alias ,name))
       ',name)))

;;; Syntax Sugar

(defmacro dtype= (array dtype)
  "Test if ARRAY's mlx-dtype is equal to DTYPE. "
  `(cl:eql (dtype ,array) ,dtype))

(with-op-template (op cffi docs
                   (array "input `mlx-array'")
                   (dim   "size to compare"))
    `(defmacro ,op (array dim)
       ,(apply #'gen-doc `(,(first docs) ,@(rest docs) :return "t or nil"))
       `(the boolean (,',(intern (string-upcase cffi) :cl) (dim ,array) ,dim)))
  ((dim= =) "Test if ARRAY's dim is equal to DIM. ")
  ((dim< <) "Test if ARRAY's dim is less than DIM. ")
  ((dim> >) "Test if ARRAY's dim is greater then DIM. ")
  ((dim<= <=) "Test if ARRAY's dim is less than or equal to DIM. ")
  ((dim>= >=) "Test if ARRAY's dim is greater than or equal to DIM. ")
  ((dim/= /=) "Test if ARRAY's dim is not equal to DIM. "))

(defun lisp<-mlx-array (arr)
  (declare (type mlx-array arr))
  (let ((arr (as-strided (contiguous arr))))
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
             (make-array (length seq)
                         :initial-contents seq
                         :element-type     '(signed-byte 32)))
           (normalize (axis)
             (etypecase axis
               (null nil)
               ((or integer (simple-array (signed-byte 32)))
                axis)
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
                      ((dim= axis 1) (vec<- (lisp<-mlx-array (mlx-array axis :dtype :int32))))
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


;;;; Slice

(deftype ~ ()
  "Type of ranges. "
  `(cons (eql ~) (cons integer (cons integer (cons integer null)))))

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
               (declare (type integer ,shape)
                        (ignorable ,shape))
               ,@body)))))

(defmethod documentation (method (doc-type (eql 'at)))
  "Return documentation"
  (declare (type keyword method))
  (documentation (gethash method *slice-spec-rules*) 'function))

;; TEST: #reorder~
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun reorder~ ([start]-stop-[step]-&key-step)
    "Reorder syntax [start] stop [step] &key step.
Return a list of (START STOP STEP). "
    (multiple-value-bind (args plist)
        (flet ((keyp (k) (and (keywordp k)
                              (not (eql k :*)))))
          (split-args-keys [start]-stop-[step]-&key-step #'keyp))
      (let ((step (getf plist :step 1)))
        (case (cl:length args)
          (0 `(:* :* ,step))
          (1 `(:* ,@args ,step))
          (2 `(,@args    ,step))
          (3 args)
          (t (error "Invalid range syntax of ~S. "
                    [start]-stop-[step]-&key-step)))))))

(with-op-template (op cffi docs
                   (start "range start
 if given is `:*', will be 0")
                   (stop  "range stop
 if given is `:*', will be -1")
                   (step  "range step (default 1)"))
    `(defmacro ,op (&rest [start]-stop-[step]-&key-step)
       ,(apply #'gen-doc docs)
       `(list ',',op ,@(reorder~ [start]-stop-[step]-&key-step)))
  (~  "Generate a range specification (~  start stop step).")
  (~~ "Generate a range specification (~~ start stop step)."))

;; TEST: #evaluate~
(defun evaluate~ (shape start stop step)
  "Evaluate ~ form to MLX acceptable form.
Return ~ expression.

Rules:
+ (~ START STOP step>0)
  + START -> [0, shape):
    + :*  => 0
    + >=0 => min(START, shape)
    + <0  => mod(START, shape)
  + STOP  -> [0, shape]:
    + :*  => shape
    + >=0 => min(STOP,  shape+1)
    + <0  => mod(START, shape)
+ (~ START STOP step<0)
  + START -> [-(shape+1), 0):
    + :*  => -1
    + >=0 => STOP-(shape+1)
    +  <0 => (shape+STOP)-(shape+1)=STOP-1
  + STOP  -> [-shape,     0):
    + :*  => -(shape+1)
    + >=0 => START-(shape+1)
    +  <0 => (shape+START)-(shape+1)=START-1
"
  (declare (type (integer 0) shape)
           (type (or integer (eql :*)) start stop)
           (type integer step))
    (cond ((cl:> step 0)
           (let ((start  (cond ((eql  start :*) 0)
                               ((cl:< start  0) (cl:mod start shape))
                               (t               (cl:min start shape))))
                 (stop   (cond ((eql  stop :*)  shape)
                               ((cl:< stop  0)  (cl:min (cl:mod stop shape)
                                                        (1+ shape)))
                               (t               (cl:min stop shape)))))
             (list '~ start stop step)))
          ((cl:< step 0)
           (let ((start  (cond ((eql  stop :*)  -1)
                               ((cl:< stop  0)  (1- stop))
                               (t               (cl:- stop  (cl:1+ shape)))))
                 (stop   (cond ((eql  start :*) (cl:- (cl:1+ shape)))
                               ((cl:< start  0) (1- start))
                               (t               (cl:- start (cl:1+ shape))))))
             (list '~ start stop step)))
          (t (error "(~~ ~A ~A ~A) should have non-zero step. "
                    start stop step))))

;; TEST: #evaluate~~
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
           (let ((start  (cond ((eql  start :*) 0)
                               ((cl:< start  0) (cl:mod start shape))
                               (t               (cl:min start shape))))
                 (stop   (cond ((eql  stop :*)  shape)
                               ((cl:< stop  0)  (cl:min (1+ (cl:mod stop shape))
                                                        shape))
                               (t               (cl:min (1+ stop) shape)))))
             (list '~ start stop step)))
          ((cl:< step 0)
           (let ((start  (cond ((eql  stop :*)  -1)
                               ((cl:< stop  0)  (1+ (1- stop)))
                               (t               (1+ (cl:- stop  (cl:1+ shape))))))
                 (stop   (cond ((eql  start :*) (cl:- (cl:1+ shape)))
                               ((cl:< start  0) (1- start))
                               (t               (cl:- start (cl:1+ shape))))))
             (list '~ start stop step)))
          (t (error "(~~~~ ~A ~A ~A) should have non-zero step. "
                    start stop step))))

;; Dev Note: for detailed `at' rule `defmlx-slice',
;; see `lisp/at.lisp'
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

;;;; wrap.lisp ends here
