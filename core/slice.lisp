;;;; slice.lisp --- Defines mlx slice shortcuts -*- mlx-cl-test-file: "core/ops.lisp" -*-

(in-package :mlx-cl)

;; TEST: #at-first
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

;; TEST: #at-last
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

;; TEST: #at-butlast
(defmlx-slice :butlast (shape &optional (n 1))
  "Get slice of mlx-array but ignore last N elements. "
  :return "(~ 0 (max 0 (- SHAPE N)) 1)"
  :parameters ((n "integer of last N elements to be ignored"))
  (declare (type (integer 1) n))
  `(~ 0 ,(cl:max 0 (cl:- shape n)) 1))

;; TEST: #at-nth
(macrolet ((nth* (&rest keyword-n)
             `(progn ,@(loop :for (keyword n) :in keyword-n
                             :collect
                             `(defmlx-slice ,keyword (shape)
                                ,(format nil "Get ~Dth element of mlx-array. " n)
                                :return (format nil "(~ ~D ~D 1)" (cl:1- n) n)
                                (declare (ignorable shape))
                                '(~ ,(cl:1- n) ,n 1))))))
  (nth* (:second 2)
        (:third  3)
        (:fourth 4)
        (:fifth  5)
        (:sixth  6)
        (:seventh 7)
        (:eighth  8)
        (:ninth   9)
        (:tenth   10)))

;; TEST: #at-middle
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

;; TEST: #at-all
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

;; TEST: #at-reverse
(defmlx-slice :reverse (shape)
  "Get all elements in reverse of axis in mlx-array. "
  :return "(~ 0 SHAPE -1)"
  `(~ 0 ,shape -1))

;; TEST: #at-skip
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

;; TEST: #at-odd/even
(defmlx-slice :odd (shape)
  "Get odd index elements on axis, this is equal to (:skip 1 :start 1). "
  :return "(~ 1 SHAPE 2)"
  `(~ 1 ,shape 2))

(defmlx-slice :even (shape)
  "Get even index elements on axis, this is equal to (:skip 0 :start 0). "
  :return "(~ 0 SHAPE 2)"
  `(~ 0 ,shape 2))

;;;; slice.lisp ends here
