;;;; stream.lisp --- MLX stream

;;; Commentary:
;; mlx-stream ingest and execute array operations on a specific device

(in-package :mlx-cl)


;;; Lowlevel

(defctype mlx-stream :pointer)

;; Allocation and Deallocation

;; mlx_stream mlx_stream_new();
(defcfun (mlx_stream_new "mlx_stream_new") mlx-stream)

;; mlx_stream mlx_stream_new_device(mlx_device dev);
(defcfun (mlx_stream_new_device "mlx_stream_new_device") mlx-stream
  (mlx-device mlx-device))

;; int mlx_get_default_stream(mlx_stream* stream, mlx_device dev);
(defcsetfun "mlx_get_default_stream" (mlx-stream :alloc (mlx_stream_new))
  mlx-device mlx-device)

(defun mlx_set_default_stream (mlx-stream)
  (ensure-success "mlx_set_default_stream"
    mlx-stream mlx-stream))

;; mlx_stream mlx_default_cpu_stream_new();
(defcfun (mlx_default_cpu_stream_new "mlx_default_cpu_stream_new") mlx-stream)

;; mlx_stream mlx_default_gpu_stream_new();
(defcfun (mlx_default_gpu_stream_new "mlx_default_gpu_stream_new") mlx-stream)

;; int mlx_stream_free(mlx_stream stream);
(defun mlx_stream_free (mlx-stream)
  (ensure-success "mlx_stream_free"
    mlx-stream mlx-stream))

;; Attributes

(defcsetfun "mlx_stream_get_device" (mlx-device :alloc (mlx_device_new))
  mlx-stream mlx-stream)

(defcsetfun "mlx_stream_get_index" :int
  mlx-stream mlx-stream)

;; Synchronize

;; int mlx_synchronize(mlx_stream stream);
(defun mlx_synchronize (mlx-stream)
  (ensure-success "mlx_synchronize"
    mlx-stream mlx-stream))


;;; Highlevel

(defclass mlx-stream (mlx-object)
  ((device :initarg :device
           :reader  mlx-stream-device)
   (index  :initarg :index
           :reader  mlx-stream-index))
  (:documentation
   "MLX-STREAM ingest and execute array operations on a specific device.

Use `default-mlx-cpu-stream', `default-mlx-gpu-stream' to get the default
`mlx-stream' object.

Use `mlx-stream' to create a new stream on `mlx-device'.
"))

(defclass mlx-cpu-stream (mlx-stream) ()
  (:documentation
   "MLX-STREAM on `mlx-cpu-device'. "))

(defclass mlx-gpu-stream (mlx-stream) ()
  (:documentation
   "MLX-STREAM on `mlx-gpu-device'. "))

(defmethod initialize-instance :after ((stream mlx-stream) &key pointer)
  (with-slots (device index) stream
    (setf device (wrap-as-mlx-device (mlx_stream_get_device pointer))
          index  (mlx_stream_get_index pointer))
    (ecase (mlx-device-type device)
      (:gpu (change-class stream 'mlx-gpu-stream))
      (:cpu (change-class stream 'mlx-cpu-stream)))))

(defun wrap-as-mlx-stream (pointer)
  (let ((stream (make-instance 'mlx-stream :pointer pointer)))
    (tg:finalize stream (lambda () (mlx_stream_free pointer)))
    (the mlx-stream stream)))

(defmethod string<- ((stream mlx-stream))
  (with-mlx-string& (str str&)
    (ensure-success "mlx_stream_tostring"
      :pointer   str&
      mlx-stream (mlx-object-pointer stream))
    (string<-mlx-string str)))

(defmethod equal ((stream1 mlx-stream) (stream2 mlx-stream))
  (cl:or (cl:eq stream1 stream2)
         (pointer-eq (mlx-object-pointer stream1)
                     (mlx-object-pointer stream2))
         (foreign-funcall "mlx_stream_equal"
                          :pointer (mlx-object-pointer stream1)
                          :pointer (mlx-object-pointer stream2)
                          :bool)))

;; TODO: Need more tests

(define-symbol-macro *mlx-stream* (default-mlx-stream))

(let (default-mlx-stream
      default-mlx-device)
  (defun default-mlx-stream (&optional (mlx-device (default-mlx-device)))
    "Return default `mlx-stream' object. "
    (declare (type mlx-device mlx-device))
    (if (cl:and (equal default-mlx-device mlx-device)
                default-mlx-stream)
        default-mlx-stream
        (setf default-mlx-device mlx-device
              default-mlx-stream (wrap-as-mlx-stream
                                  (mlx_get_default_stream
                                   (mlx-object-pointer mlx-device))))))

  (defun (setf default-mlx-stream) (mlx-stream
                                    &optional (mlx-device (default-mlx-device)))
    (declare (type mlx-stream mlx-stream))
    (unless (cl:and (equal mlx-device
                           (cl:or default-mlx-device
                                  (setf default-mlx-device
                                        (default-mlx-device))))
                    (equal mlx-stream (default-mlx-stream)))
      (mlx_set_default_stream (mlx-object-pointer mlx-stream))
      (setf default-mlx-stream mlx-stream))))

(defun default-mlx-cpu-stream ()
  "Return default `mlx-cpu-stream' object. "
  (wrap-as-mlx-stream (mlx_default_cpu_stream_new)))

(defun default-mlx-gpu-stream ()
  "Return default `mlx-gpu-stream' object. "
  (wrap-as-mlx-stream (mlx_default_gpu_stream_new)))

(defun mlx-stream (&key device)
  "Create a `mlx-stream' on `mlx-device'. "
  (declare (type mlx-device device))
  (wrap-as-mlx-stream (mlx_stream_new_device (mlx-object-pointer device))))

(defun synchronize (&optional (mlx-stream (default-mlx-stream)))
  "Synchronize with the given stream.

Parameter:
+ MLX-STREAM: if not given, would be `default-mlx-stream'
"
  (declare (type mlx-stream mlx-stream))
  (mlx_synchronize (mlx-object-pointer mlx-stream)))

;;;; stream.lisp ends here
