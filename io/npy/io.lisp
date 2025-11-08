;;;; io.lisp --- IO implementation for npy <-> mlx-array

(in-package :mlx-cl.io.npy)

;; TODO: #optimization
;; make `load-from' and `save-to' faster using MLX CFFI bindings
;;
(mlx:defmlx-extension (:npy "NPY")
  ((:load-path path)
   "Load from NPY file. "
   :parameters ((stream "NPY stream to load from"))
   (mlx:mlx-array (numpy-file-format:load-array path)))
  ((:save-path arr path)
   "Write as NPY file. "
   :parameters ((arr    "`mlx-array' to write")
                (stream "NPY stream to save to"))
   (numpy-file-format:store-array (mlx:lisp<- arr) path)))

;;;; io.lisp ends here
