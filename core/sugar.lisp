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
      (as-dtype :float32))
    ;; => (let* ((* expr)
                 (* (as-dtype * :float32)))
            *)
"
  (let ((sym* (intern "*")))
    (labels ((*? (elem)
               (if (atom elem)
                   (eql sym* elem)
                   (find-if #'*? (rest elem)))))
      `(let* ((,sym* ,expr)
              ,@(loop :for expr :in body
                      :collect `(,sym*
                                 ,(etypecase expr
                                    (symbol `(,expr ,sym*))
                                    (list
                                     (if (*? expr) expr
                                         `(,(first expr) ,sym* ,@(rest expr))))))))
         ,sym*))))

(defmacro neq (obj1 obj2)
  "Equal to (not (eq OBJ1 OBJ2)). "
  `(cl:not (eq ,obj1 ,obj2)))

(defmacro nequal (obj1 obj2)
  "Test OBJ1 and OBJ2 are NOT equal. "
  `(cl:not (equal ,obj1 ,obj2)))


;;;; TODO:
;; Math reader macro #$ EXPR
;;
;; Example:
;;
;;   #$( (exp[X ^ 2 + Y ^ 2] / 3 - 5 * 6 / 4) < 3 )
;;
;; [ EXPR {, EXPR}* ] -> (list EXPR {EXPR}*)
;; FN [...] -> (FN [...])
;; EXPR op EXPR -> (op EXPR EXPR) iff op is binary-op or op canbe turned into binary-op
;;


;;;; sugar.lisp ends here
