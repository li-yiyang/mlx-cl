;;;; metal.lisp --- MLX-C metal

(in-package :mlx-cl)

(defstruct (metal-device-info
            (:conc-name metal-device-))
  "Information about the GPU device and system settings."
  (architecture ""
   :type string
   :read-only t)
  (max-buffer-length 0
   :type unsigned-byte
   :read-only t)
  (max-recommanded-working-set-size 0
   :type unsigned-byte
   :read-only t)
  (memory-size 0
   :type unsigned-byte
   :read-only t))

(defcstruct mlx_metal_device_info_t
  (architecture                      (:array :char 256))
  (max-buffer-length                 :size)
  (max-recommanded-working-set-size  :size)
  (memory-size                       :size))

(defun metal-device-info ()
  "Return a `metal-device-info' structure. "
  (let ((plist (foreign-funcall "mlx_metal_device_info"
                                (:struct mlx_metal_device_info_t))))
    (make-metal-device-info
     :architecture                     (string<-c-array (getf plist 'architecture))
     :max-buffer-length                (getf plist 'max-buffer-length)
     :max-recommanded-working-set-size (getf plist 'max-recommanded-working-set-size)
     :memory-size                      (getf plist 'memory-size))))

(defun metal-available-p ()
  "Test if metal is available. "
  (with-elem& (res res& :type :bool)
    (ensure-success "mlx_metal_is_available"
      :pointer res&)
    res))

;; GPU trace

(defun path-with-extension (path extension)
  "Return pathname modified to have the specified EXTENSION. "
  (declare (type (cl:or pathname string) path)
           (type (cl:or string symbol) extension))
  (pathname (format nil "~A.~A" (uiop:split-name-type path) extension)))

(let ((capture nil))
  (defun metal-start-capture (path &key (if-exists :error) restart)
    "Start a Metal capture.
Return path to capture.

Parameter:
+ PATH: path to save the capture which
  should have the extension `.gputrace'

  if the extension is got given or not `.gputrace',
  will overwrite the extension
+ IF-EXISTS: what to do if trace file at PATH
  already exists, which could be `:error', `:supersede';
+ RESTART: if already have a existing trace,
  stop it and restart if RESTART is `t',
  otherwise, raise error. "
    (declare (type (cl:or pathname string) path)
             (type (member :error :supersede) if-exists))
    (let ((path (path-with-extension path "gputrace")))
      (if capture
          (cond (restart
                 (metal-stop-capture)
                 (metal-start-capture path :if-exists if-exists))
                (t
                 (error "Already an existing metal capture at ~A. " capture)))
          (cond ((uiop:file-exists-p path)
                 (ecase if-exists
                   (:error (error "Capture ~A already exists. " path))
                   (:supersede
                    (uiop:delete-file-if-exists path)
                    (metal-start-capture path :restart restart))))
                (t
                 (handler-case
                     (ensure-success "mlx_metal_start_capture"
                       :string (uiop:native-namestring path))
                   (mlx-runtime-error (err)
                     (error 'mlx-debugging-error
                            :message (format nil "~A

Make sure that `MLX_METAL_DEBUG' option is turned on when building the
libmlxc.dylib.

And Make sure `MLX_CAPTURE_ENABLED' environment variable is set to `1'
before loading `mlx-cl' library. "
                                             (slot-value err 'message)))))
                 (setf capture path))))))

  (defun metal-stop-capture ()
    (unwind-protect (ensure-success "mlx_metal_stop_capture")
      (setf capture nil))))

;; TODO: Metal API

;;;; metal.lisp ends here
