;;;; device.lisp --- Test for `mlx:mlx-device'

(in-package :mlx-cl.test)

(def-suite* mlx-device
  :description "Test `mlx:mlx-device'. "
  :in mlx-test)

(test default-mlx-device
  (let ((device *mlx-device*))
    (cond ((metal-available-p)
           (is (equal :gpu   (mlx-device-type device)))
           (is (equal device (mlx-device :type :gpu)))
           (is (equal (princ-to-string device) "Device(gpu, 0)")))
          (t
           (is (equal :cpu (mlx-device-type device)))
           (signals mlx-error
             (setf *mlx-device* (mlx-device :type :gpu)))))))

(test mlx-device
  (with-mlx-device (mlx-device :type :cpu)
    ;; default mlx-cpu-device
    (is (equal :cpu *mlx-device*))
    (is (equal *mlx-device* :cpu))
    (is (equal (princ-to-string *mlx-device*) "Device(cpu, 0)")))

  (with-mlx-device :cpu
    ;; default mlx-cpu-device
    (is (equal :cpu *mlx-device*))
    (is (equal *mlx-device* :cpu))
    (is (equal (princ-to-string *mlx-device*) "Device(cpu, 0)"))))

;;;; device.lisp ends here
