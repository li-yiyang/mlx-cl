;;;; sugar.lisp --- Syntax sugar

(in-package :mlx-cl)

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
