;;;; version.lisp --- MLX-C version

(in-package :mlx-cl)

;; mlx_string version = mlx_string_new();
;; mlx_version(&version);
;; mlx_string_data(version); // => string of MLX version
(defun mlx-version ()
  "Return MLX version. "
  (with-mlx-string& (str str&)
    (ensure-success "mlx_version"
      :pointer str&)
    (string<-mlx-string str)))

;;;; version.lisp ends here
