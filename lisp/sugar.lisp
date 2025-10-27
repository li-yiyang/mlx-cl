;;;; sugar.lisp --- Syntax sugar

(in-package :mlx-cl)

(with-op-template (op cffi docs
                   (array "input `mlx-array'")
                   (dim   "size to compare"))
    `(defmacro ,op (array dim)
       ,(apply #'gen-doc docs)
       (list ',(intern (string-upcase cffi) :cl) (list 'dim array) dim))
  ((dim= =) "Test if ARRAY's dim is equal to DIM. ")
  ((dim< <) "Test if ARRAY's dim is less than DIM. ")
  ((dim> >) "Test if ARRAY's dim is greater then DIM. ")
  ((dim<= <=) "Test if ARRAY's dim is less than or equal to DIM. ")
  ((dim>= >=) "Test if ARRAY's dim is greater than or equal to DIM. ")
  ((dim/= /=) "Test if ARRAY's dim is not equal to DIM. "))

(defmacro ->* (expr &body body)
  "Trivial implementation of arrow macro.

Syntax:

    (->* EXPR
     BODY)

+ EXPR: expression
+ BODY: each expression of BODY would be binded with variable `*',
  the value of `*' is previous expression result

Example:

    (->* expr
      (as-type * :float32))
    ;; => (let* ((* expr)
                 (* (as-type * :float32)))
            *)
"
  `(let* ((* ,expr)
          ,@(loop :for expr :in body
                  :collect `(* ,expr)))
     *))


;;;; sugar.lisp ends here
