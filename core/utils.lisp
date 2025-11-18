;;;; utils.lisp --- Some functional codes

(in-package :mlx)


;;; Condition

(define-condition mlx-error () ())

(define-condition mlx-runtime-error (mlx-error)
  ((message :initarg :message))
  (:report (lambda (cond stream)
             (write-string "MLX Runtim Error: "       stream)
             (write-string (slot-value cond 'message) stream))))

(define-condition mlx-cffi-error (mlx-error)
  ((function :initarg :message)
   (return   :initarg :return))
  (:report
   (lambda (cond stream)
     (format stream
             "MLX CFFI Calling Error: FFI function `~A' returned error code ~D. "
             (slot-value cond 'function)
             (slot-value cond 'return)))))

(define-condition mlx-debugging-error (mlx-runtime-error) ())


;;; CFFI libraries

;; TODO: build CFFI libraries if cannot load
;; or ship with pre-built binaries (.dylib)

(mlx-cl.lib:load-libmlxc)

;; Design of MLX CFFI Error Callback
;;
;; + msg: raise from MLX C library
;; + data: provided

(defcallback mlx-error :void
    ((msg  :string)
     (data :pointer))
  (declare (ignorable data))
  (error 'mlx-runtime-error :message msg))

(foreign-funcall "mlx_set_error_handler"
                 :pointer (callback mlx-error)
                 :pointer (null-pointer)
                 :pointer (null-pointer)
                 :void)

(defmacro ensure-success (cffi-calling &body {type-arg}*-ignore-error-p?)
  "Ensure CFFI-CALLING form returns zero.
Otherwise, raise ERROR expression.

Syntax:

    (ensure-success \"foreign-function\"
      { type arg }*
      ignore-error-p?)

"
  (declare (type string cffi-calling))
  (let* ((res            (gensym "RES"))
         (err            `(error 'mlx-cffi-error
                                 :function ,cffi-calling
                                 :return   ,res))
         (arg            (if (evenp (cl:length {type-arg}*-ignore-error-p?))
                             {type-arg}*-ignore-error-p?
                             (butlast {type-arg}*-ignore-error-p?)))
         (ignore-error-p (if (oddp (cl:length {type-arg}*-ignore-error-p?))
                             (car (last {type-arg}*-ignore-error-p?))
                             nil)))
    (assert (symbolp ignore-error-p))
    `(let ((,res (foreign-funcall ,cffi-calling ,@arg :int)))
       ,(cond ((null ignore-error-p) `(if (zerop ,res) t ,err))
              ((eq ignore-error-p t) t)
              (t `(if (zerop ,res) t (unless ,ignore-error-p ,err)))))))

(defmacro defcsetfun (cffi-function type-alloc?-free? &body type-args)
  "Define a CFFI function that return :int as success mark.
The CFFI function should change the first element of TYPE.
Defined new function should retrun setted type result.

Syntax:

   (defcsetfun \"cffi_calling\" { type
                                | (type &key alloc free)
                                }
      {type args}*)
"
  (let ((result  (gensym "RESULT"))
        (result& (gensym "RESULT&")))
    (destructuring-bind (type &key alloc free)
        (if (listp type-alloc?-free?) type-alloc?-free?
            (list type-alloc?-free?))
      `(defun ,(intern (string-upcase cffi-function))
           ,(loop for (type arg) on type-args by #'cddr
                  collect arg)
         (with-elem& (,result ,result& :type  ,type
                                       :free  ,free
                                       :alloc ,alloc)
           (ensure-success ,cffi-function
             :pointer ,result&
             ,@type-args)
           ,result)))))


;;; OOP
;; Base class of all the MLX objects

(defclass mlx-object ()
  ((pointer :initarg :pointer
            :reader  mlx-object-pointer))
  (:documentation
   "Base object of MLX objects. "))

(defgeneric string<- (elem)
  (:documentation
   "Turn ELEM into string. ")
  (:method (obj)
    "By default print OBJ as string. "
    (princ-to-string obj)))

(defmethod print-object ((obj mlx-object) stream)
  (write-string (string<- obj) stream))

;; DEV Note:
;; subclasses of mlx-object should define a function
;; called `wrap-as-mlx-*' to convert CFFI pointer
;; to `mlx-object', which should do:
;; 1. make a new `mlx-object' from pointer
;; 2. bind its finalized process to free the pointer
;;    and other possible foreign data
;;


;;; CL API mask

(defgeneric equal (x y)
  (:documentation
   "Test if X and Y is equal to each other.
Returns `t' if X and Y are equal in value, and `nil' if not.

Note:
This calls `cl:equal' to test if two elements are same.
And it would perform enhanced test on the nested data structure
to ensure that the X and Y are equal in value.

Dev Note:
Use `eq' for fast comparing.
")
  (:method (x y)
    (cl:equal x y))
  (:method ((num1 number) (num2 number))
    (cl:= num1 num2))
  (:method ((str1 string) (str2 string))
    (cl:string= str1 str2))
  (:method ((arr1 array) (arr2 array))
    "If two `array' is equal:
+ (shape ARR1) are equal to (shape ARR2)
+ every element in ARR1 and ARR2 are equal to each other"
    (cl:and (cl:equal (array-dimensions arr1)
                      (array-dimensions arr2))
            (loop :for idx :below (size arr1)
                  :if (cl:not (equal (row-major-aref arr1 idx)
                                     (row-major-aref arr2 idx)))
                    :return nil
                  :finally (return t)))))

(defgeneric copy (elem)
  (:documentation
   "Duplicate ELEM.

Dev: should implement `copy' method for each different type. ")
  (:method ((num number))
    num)
  (:method ((list list))
    (copy-tree list))
  (:method ((seq sequence))
    (copy-seq seq))
  (:method ((arr array))
    (make-array (array-dimensions arr)
                :initial-contents arr
                :element-type     (array-element-type arr))))


;;; Dev Shortcuts

;; DEV Note:
;; the `string<-' function should be used to `print-object'
;;

(defun sequencefy (elem)
  "Turn ELEM as a sequence if ELEM is not a sequence. "
  (if (typep elem 'sequence)
      (map 'list #'identity elem)
      (list elem)))

(defun len<=2-listfy (elem)
  "Turn ELEM as a list with length <= 2. "
  (cond ((atom elem) (list elem))
        ((cl:<= (cl:length elem) 2) elem)
        (t (subseq elem 0 2))))

(defun listfy (elem)
  "Turn ELEM as a list if it's atom. "
  (if (atom elem) (list elem) elem))

(defun atomize (elem)
  "Turn ELEM as a ATOM if it's a list. "
  (if (listp elem) (car elem) elem))

(defun intern* (&rest components)
  "Concate COMPONENTS and intern it as a symbol.
Return a symbol of MLX-C.CFFI package. "
  (flet ((string<- (elem)
           (etypecase elem
             (symbol (symbol-name elem))
             (number (format nil "~A" elem))
             (string (string-upcase elem)))))
    (intern (apply #'concatenate 'string (mapcar #'string<- components))
            :mlx-cl)))

(defun sconc (&rest strings)
  "Concat STRINGS. "
  (apply #'concatenate 'string strings))

(defun alist-union (alist &rest alists)
  "Union ALISTS like `cl:union', but keep orders. "
  (let ((alist (reverse alist)))
    (loop :for insert :in alists :do
      (loop :for (car . cdr) :in insert
            :if (assoc car alist)
              :do (setf (cdr (assoc car alist)) cdr)
            :else
              :do (push (cons car cdr) alist)))
    (reverse alist)))

(defun gen-doc (shortline
                &key
                  (return "`mlx-array'")
                  syntax aliases
                  note dev-note examples
                  definition parameters
                &allow-other-keys)
  "Generate documents from DOCSTRING. "
  (flet ((trim (str)
           ;; trivial implementation of string trim
           ;; trim only first character
           (if (cl:and (cl:> (cl:length str) 0)
                       (cl:char= (aref str 0) #\Newline))
               (subseq str 1)
               str)))
    (with-output-to-string (doc)
      (write-line shortline doc)
      (etypecase return
        (symbol              (format doc "Return `~S'. ~%" return))
        ((cl:or null string) (format doc "Return ~A. ~%" return))
        (list                (format doc "Return values are ~{~A~^, ~}. " return)))
      (when syntax
        (format doc "~%Syntax:~%    ~A~%" syntax))
      (when definition
        (format doc "~%Definition:~%~A~%" (trim definition)))
      (when parameters
        (typecase parameters
          (string (format doc "~%Parameters:~%~A~%" parameters))
          (list   (format doc "~%Parameters:~%~{~{+ ~@:(~A~): ~A~}~%~}" parameters))))
      (when note
        (format doc "~%Note:~%~A~%" (trim note)))
      (when dev-note
        (format doc "~%Dev Note:~%~A~%" (trim dev-note)))
      (when examples
        (format doc "~%Examples:~%")
        (loop :for (docstr in out) :in examples :do
          (format doc "~A~%    ~S~%    ;; => ~S~%" docstr in out)))
      (when aliases
        (format doc "~%See also ~{`~A'~^, ~}. ~%" aliases)))))

(defun split-doc-body (progn)
  "Split PROGN like (doc { keyword value }* . body) pattern.
Return doc, plist, body.

Test:
+ (split-doc-body ())                 ;; => nil, nil, nil
+ (split-doc-body '(\"doc\"))           ;; => \"doc\", nil, nil
+ (split-doc-body '(\"doc\" :p 1))      ;; => \"doc\", (:p 1), nil
+ (split-doc-body '(:p 1))            ;; => nil, nil, (:p 1)
+ (split-doc-body '(\"doc\" (not doc))) ;; => \"doc\", nil, (not doc)
"
  (if (endp progn)
      (values nil nil progn)
      (let ((doc (first progn)))
        (if (stringp doc)
            (loop :for (key . val-body) :on (rest progn) :by #'cddr
                  :if (endp val-body)
                    :return (values doc plist (cons key val-body))
                  :if (keywordp key)
                    :collect key :into plist
                    :and :collect (car val-body) :into plist
                  :else
                    :return (values doc plist (cons key val-body))
                  :finally (return (values doc plist val-body)))
            (values nil nil progn)))))

(defun parse-lambda-list (lambda-list
                          &key
                            (normal   #'cl:identity)
                            (optional #'cl:identity)
                            (aux      #'cl:identity)
                            (key      #'cl:identity)
                          &allow-other-keys)
  "Trivial implementation to parse LAMBDA-LIST.
Return values are required, optional, rest, keys, others, aux.

Example:

    (x y (z type) &optional (y 2 y?) &key (bala 3 bala?) &aux ...)

    ;; => (x y (z type)), ((y 2 y?)), ((bala 3 bala?)), nil, (...)
"
  (declare (type (or list function) lambda-list))
  (let (normal* optional* rest* keys* others* aux*)
    (labels ((normal (lst)
               (loop :for (arg . rst) :on lst
                     :if (member arg '(&optional &key &rest &aux &allow-other-keys))
                       :do (setf normal* normals)
                       :and :return (case arg
                                      (&optional (optional rst))
                                      (&rest     (rst      rst))
                                      (&aux      (aux      rst))
                                      (&key      (key      rst))
                                      (&allow-other-keys
                                       (setf others* t)
                                       (assert (endp rst))))
                     :collect (funcall normal arg) :into normals
                     :finally (setf normal* normals)))
             (optional (lst)
               (loop :for (arg . rst) :on lst
                     :if (member arg '(&optional &allow-other-keys))
                       :do (error "Invalid lambda list ~A. " lambda-list)
                     :if (member arg '(&rest &aux &key))
                       :do (setf optional* optionals)
                       :and :return (case arg
                                      (&rest (rst rst))
                                      (&aux  (aux rst))
                                      (&key  (key rst)))
                     :collect (funcall optional arg) :into optionals
                     :finally (setf optional* optionals)))
             (rst (lst)
               (setf rest* (first lst))
               (case (second lst)
                 (&key (key (cddr lst)))
                 (&allow-other-keys
                  (setf others* t)
                  (assert (endp (cddr lst))))
                 (&aux (aux (cddr lst)))))
             (aux (lst)
               (loop :for arg :in lst
                     :if (member arg '(&optional &key &rest &aux &allow-other-keys))
                       :do (error "Invalid lambda list ~A. " lambda-list)
                     :collect (funcall aux arg) :into auxs
                     :finally (setf aux* auxs)))
             (key (lst)
               (loop :for (arg . rst) :on lst
                     :if (member arg '(&optional &rest &key))
                       :do (error "Invalid lambda list ~A. " lambda-list)
                     :if (cl:eq arg '&allow-other-keys)
                       :do (setf others* t)
                       :and :do (assert (endp rst))
                     :if (cl:eq arg '&aux)
                       :do (setf keys* keys)
                       :and :return (aux rst)
                     :collect (funcall key arg) :into keys
                     :finally (setf keys* keys))))
      (etypecase lambda-list
        (list      (normal lambda-list))
        (function  (normal
                    #+sbcl      (sb-introspect:function-lambda-list lambda-list)
                    #+lispworks (lw:function-lambda-list            lambda-list)
                    #-(or sbcl lispworks)
                    (error "Don't know how to parse function ~A's lambda list.
Please add support in mlx-cl/core/utils.lisp like:

  #+sbcl (sb-introspect:function-lambda-list lambda-list)

and pull a request. "
                           lambda-list))))
      (values normal* optional* rest* keys* others* aux*))))

(defun split-args-keys (args &optional (keyp #'keywordp))
  "Split ARGS and return ARGS and KEYS.

Example:

    (split-args-keys '(x y :z z))
    ;; => (x y), (:z z)
"
  (loop :for (arg . rest) :on args
        :if (funcall keyp arg)
          :do (if (oddp (cl:length rest))
                  (return (values arg* (cons arg rest)))
                  (error "Odd number of function key calling arguments ~A. " args))
        :else
          :collect arg :into arg*
        :finally (return (values arg* nil))))

(defun alias-symbol-function (symbol function &optional documentation)
  "See `defalias'. "
  (declare (type (or symbol (cons (eql 'setf) (cons symbol null))) symbol)
           (type (or function symbol (cons (eql 'setf) (cons symbol null))) function)
           (type (cl:or null string) documentation))
  (macrolet ((fn   (sym) `(symbol-function ,sym))
             (ma   (sym) `(macro-function  ,sym))
             (doc  (fn)  `(documentation ,fn 'function)))
    (cond ((functionp function)
           (setf (fn symbol)  function
                 (doc symbol) (cl:or documentation (doc function))))
          ((ignore-errors (fn function))
           (setf (fn  symbol) (fn function)
                 (doc symbol) (cl:or documentation (doc function))))
          ((ignore-errors (ma function))
           (setf (ma  symbol) (ma  function)
                 (doc symbol) (cl:or documentation (doc function))))
          (t (error "Undefined ~S. " function)))))

(defmacro defalias (name function &optional documentation)
  "Define NAME alias for FUNCTION. "
  `(alias-symbol-function ',name ',function ,documentation))

(defmacro docollect ((arg list) &body body)
  "Map over LIST with element as ARG and result as BODY. "
  `(loop :for ,arg :in ,list :collect (progn ,@body)))


;;; CFFI Shortcuts

;; (string<- array) is provided only for `metal-device-info'

(defun string<-c-array (c-array)
  "Turn C-ARRAY into lisp string.
Return string. "
  (with-output-to-string (stream)
    (loop :for code :across c-array
          :if (zerop code)
            :return t
          :do (write-char (code-char code) stream))))

(defmacro with-elem& ((elem elem& &key (type :pointer) free alloc) &body body)
  "With ELEM and pointer to ELEM&.

Syntax:

    (with-elem& (elem elem& :type   cffi-type
                            {:free  free}?
                            {:alloc alloc}?)
       body)

Parameters:
+ ELEM:  variable as elem value
+ ELEM&: variable as pointer to ELEM (like &elem in C)
+ TYPE:  CFFI foreign types
+ FREE:  if given, will free ELEM after execution
+ ALLOC: if given, will alloc ELEM before execution
"
  `(with-foreign-object (,elem& ',type)
     (symbol-macrolet ((,elem (mem-aref ,elem& ',type)))
       ,@(when alloc
           `((setf ,elem ,alloc)))
       ,(if free
            `(unwind-protect (progn ,@body) ,free)
            `(progn ,@body)))))

;; TODO: accelerate the data coping and data reading

#-sbcl
(defmacro with-foreign<-sequence ((pointer sequence type
                                   &optional
                                     (length (gensym "LENGTH"))
                                     (elem (gensym "ELEM"))
                                     elem-convert)
                                  &body body
                                  &aux
                                    (seq   (gensym "SEQUENCE"))
                                    (ctype (gensym "TYPE")))
  "Wrap SEQUENCE as foreign array POINTER of TYPE. "
  `(let* ((,seq    ,sequence)
          (,ctype  ,type)
          (,length (cl:length ,seq)))
     (with-foreign-pointer (,pointer (cl:* ,length (foreign-type-size ,ctype)))
       (let ((idx -1))
         (map nil
              (lambda (,elem)
                (setf (mem-aref ,pointer ,ctype (incf idx))
                      ,(if elem-convert
                           elem-convert
                           elem)))
              ,seq))
       ,@body)))

#+sbcl
(defmacro with-foreign<-sequence ((pointer sequence type
                                   &optional
                                     (length (gensym "LENGTH"))
                                     (elem   (gensym "ELEM"))
                                     elem-convert)
                                  &body body
                                  &aux
                                    (seq   (gensym "SEQUENCE"))
                                    (ctype (gensym "TYPE")))
  "Wrap SEQUENCE as foreign array POINTER of TYPE. "
  `(let* ((,seq    ,sequence)
          (,ctype  ,type)
          (,length (cl:length ,seq)))
     ,(cond (elem-convert
             `(with-foreign-pointer
                  (,pointer (cl:* ,length (foreign-type-size ,ctype)))
                (let ((idx -1))
                  (map nil
                       (lambda (,elem)
                         (setf (mem-aref ,pointer ,ctype (incf idx)) ,elem-convert))
                       ,seq))
                ,@body))
            ;; Use `cffi:pointer-to-vector-data' to avoid data copying.
            ;; Credits:
            ;; https://www.reddit.com/r/Common_Lisp/comments/1ogracb/comment/nlk6l80/
            ((member type *built-in-foreign-types*)
             (let ((fn (gensym "FN")))
               `(flet ((,fn (,pointer ,length)
                         (declare (ignorable ,length))
                         ,@body))
                  (etypecase ,seq
                    ((simple-array ,(ecase type
                                      (:int          '(signed-byte   32))
                                      ((:uint :size) '(unsigned-byte 32))
                                      (:pointer      'foreign-pointer)))
                     (with-pointer-to-vector-data (,pointer ,seq)
                       (,fn  ,pointer ,length)))
                    (sequence
                     (with-foreign-pointer
                         (,pointer (cl:* ,length (foreign-type-size ,ctype)))
                       (let ((idx -1))
                         (map nil
                              (lambda (,elem)
                                (setf (mem-aref ,pointer ,ctype (incf idx))
                                      ,elem))
                              ,seq))
                       (,fn ,pointer ,length)))))))
            (t
             `(with-foreign-pointer
                  (,pointer (cl:* ,length (foreign-type-size ,ctype)))
                (let ((idx -1))
                  (map nil
                       (lambda (,elem)
                         (setf (mem-aref ,pointer ,ctype (incf idx)) ,elem))
                       ,seq))
                ,@body)))))

(defmacro with-foreign<-array ((pointer array type
                                &optional
                                  (shape (gensym "SHAPE"))
                                  (size  (gensym "SIZE"))
                                  (elem  (gensym "ELEM"))
                                  elem-convert)
                               &body body
                               &aux
                                 (arr   (gensym "ARRAY"))
                                 (ctype (gensym "TYPE"))
                                 (idx   (gensym "IDX")))
  "Wrap ARRAY as foreign array POINTER of TYPE. "
  `(let* ((,arr   ,array)
          (,ctype ,type)
          (,shape (array-dimensions ,arr))
          (,size  (cl:reduce #'cl:* ,shape)))
     (with-foreign-pointer (,pointer (cl:* ,size (foreign-type-size ,ctype)))
       (loop :for ,idx :below ,size
             :for ,elem := (row-major-aref ,arr ,idx)
             :do (setf (mem-aref ,pointer ,ctype ,idx)
                       ,(if elem-convert
                            elem-convert
                            elem)))
       ,@body)))

(defun sequence<-foreign (pointer type length &optional (lisp-type 'list))
  "Convert foreign POINTER with TYPE and LENGTH into lisp sequence. "
  (cond ((subtypep lisp-type 'list)
         (loop :for i :below length
               :collect (mem-aref pointer type i)))
        ((subtypep lisp-type 'array)
         (loop :with arr := (make-array length)
               :for i :below length
               :do (setf (aref arr i) (mem-aref pointer type i))
               :finally (return arr)))
        (t
         (error "Invalid LISP-TYPE ~A, should be list, array. " lisp-type))))

(defun array<-foreign (pointer type shape &optional (element-type t))
  "Convert foreign POINTER with TYPE and SHAPE into lisp array. "
  (if (subtypep element-type 'complex)
      (loop :with array := (make-array shape :element-type 'complex
                                             :initial-element #C(0 0))
            :for i :below (reduce #'cl:* shape)
            :for idx :from 0 :by 2
            :do (setf (row-major-aref array i)
                      (complex (mem-aref pointer type idx)
                               (mem-aref pointer type (cl:1+ idx))))
            :finally (return array))
      (loop :with array := (make-array shape :element-type element-type)
            :for i :below (reduce #'cl:* shape)
            :do (setf (row-major-aref array i) (mem-aref pointer type i))
            :finally (return array))))

;;;; utils.lisp ends here
