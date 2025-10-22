;;;; device.lisp --- MLX device

;;; Commentary:
;; mlx-device defines the compute unit where operations are performed

(in-package :mlx-cl)


;;;; CFFI Lowlevel

(defctype mlx-device :pointer)

(defcenum mlx-device-type :cpu :gpu)

;;; Allocation and Deallocation

(defcfun (mlx_device_new "mlx_device_new") mlx-device)

(defun mlx_device_free (mlx-device)
  (ensure-success "mlx_device_free"
    mlx-device mlx-device))

;;; Attributes

;; int mlx_device_get_type(mlx_device_type* type, mlx_device dev);
(defcsetfun "mlx_device_get_type" mlx-device-type
  mlx-device mlx-device)

;; int mlx_device_get_index(int* index, mlx_device dev);
(defcsetfun "mlx_device_get_index" :int
  mlx-device mlx-device)

;; bool mlx_device_equal(mlx_device lhs, mlx_device rhs);
(defcfun (mlx_device_equal "mlx_device_equal") :bool
  (lhs mlx-device)
  (rhs mlx-device))

;; mlx_device mlx_device_new_type(mlx_device_type type, int index);
(defcfun (mlx_device_new_type "mlx_device_new_type") mlx-device
  (type  mlx-device-type)
  (index :int))


;;;; Highlevel

(deftype mlx-device-type ()
  "Type of `mlx-device'. "
  '(member :cpu :gpu))

(defclass mlx-device (mlx-object)
  ((type  :reader mlx-device-type)
   (index :reader mlx-device-index))
  (:documentation
   "MLX-DEVICE defines the compute unit where operations are performed.

Use `default-mlx-device' to get/set the default computing device.
Or use `mlx-device' to define a new device. "))

(defmethod initialize-instance :after ((device mlx-device) &key pointer)
  (with-slots (type index) device
    (setf index (mlx_device_get_index pointer)
          type  (mlx_device_get_type  pointer))
    (ecase type
      (:gpu (change-class device 'mlx-gpu-device))
      (:cpu (change-class device 'mlx-cpu-device)))))

(defun wrap-as-mlx-device (pointer)
  (let ((device (make-instance 'mlx-device :pointer pointer)))
    (tg:finalize device (lambda () (mlx_device_free pointer)))))

(defclass mlx-cpu-device (mlx-device)
  ((mlx-device-type :initform :cpu))
  (:documentation
   "CPU device of MLX device. "))

(defclass mlx-gpu-device (mlx-device)
  ((mlx-device-type :initform :gpu))
  (:documentation
   "GPU device of MLX device. "))

;; int mlx_device_tostring(mlx_string* str, mlx_device dev);
(defmethod string<- ((device mlx-device))
  "Get device description. "
  (with-mlx-string& (str str&)
    (ensure-success "mlx_device_tostring"
      :pointer   str&
      mlx-device (mlx-object-pointer device))
    (string<-mlx-string str)))

(let (default-mlx-device)
  (defun default-mlx-device ()
    "Return default `mlx-device'. "
    (if default-mlx-device
        default-mlx-device
        (with-elem& (dev dev& :type  :pointer
                              :alloc (mlx_device_new))
          (ensure-success "mlx_get_default_device"
            :pointer dev&)
          (setf default-mlx-device (wrap-as-mlx-device dev)))))

  (defun (setf default-mlx-device) (mlx-device)
    (declare (type mlx-device mlx-device))
    (unless (equal mlx-device default-mlx-device)
      (ensure-success "mlx_set_default_device"
        mlx-device (mlx-object-pointer mlx-device))
      (setf default-mlx-device mlx-device))))

(define-symbol-macro *mlx-device* (default-mlx-device))

(defun mlx-device (&key type (index 0))
  "Make a `mlx-device' of TYPE and INDEX. "
  (declare (type mlx-device-type type)
           (type integer index))
  (wrap-as-mlx-device (mlx_device_new_type type index)))

;; equal

(defmethod equal ((dev1 mlx-device) (dev2 mlx-device))
  (cl:or (cl:eq dev1 dev2)
         (pointer-eq (mlx-object-pointer dev1)
                     (mlx-object-pointer dev2))
         (foreign-funcall "mlx_device_equal"
                          :pointer (mlx-object-pointer dev1)
                          :pointer (mlx-object-pointer dev2)
                          :bool)))

(defmethod equal (type (dev mlx-device))
  (equal dev type))

(defmethod equal ((dev mlx-device) (type symbol))
  "For testing `mlx-device' DEV with device TYPE. "
  (when (typep type '(cl:or null mlx-device-type))
    (when type (cl:eq (mlx-device-type dev) type))))

(defmethod equal ((dev mlx-device) (idx integer))
  (cl:= (mlx-device-index dev) idx))

;; with-mlx-device

(defmacro with-mlx-device (device &body body)
  "Bind default mlx-device as DEVICE locally within BODY. "
  (let ((prev (gensym "PREV")))
    `(let ((,prev *mlx-device*))
       (unwind-protect (progn
                         (setf *mlx-device* (the mlx-device ,device))
                         ,@body)
         (setf *mlx-device* ,prev)))))

;;;; device.lisp ends here
