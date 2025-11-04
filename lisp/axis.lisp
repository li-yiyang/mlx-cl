;;;; axis.lisp --- Defines axis shortcuts

(in-package :mlx-cl)

(defmlx-axis :last (&optional (n 1))
  "Last N(=1) axis. "
  :parameters ((n "number of axis (default 1)"))
  :return "#(-N ... -1)"
  (declare (type (integer 1) n))
  (if (cl:= n 1) -1
      (loop :with axes := (make-array n :element-type '(signed-byte 32))
            :for i :below n
            :for axis :from (cl:- n) :by 1
            :do (setf (aref axes i) axis)
            :finally (return axes))))

(defmlx-axis :first (&optional (n 1))
  "First N(=1) axis. "
  :parameters ((n "number of axis (default 1)"))
  (declare (type (integer 1) n))
  (if (cl:= n 1) -1
      (loop :with axes := (make-array n :element-type '(signed-byte 32))
            :for i :below n
            :for axis :from 1
            :do (setf (aref axes i) axis)
            :finally (return axes))))

;;;; axis.lisp ends here
