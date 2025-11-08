;;;; io.lisp --- JPEG I/O for mlx-array

(in-package :mlx-cl.io.jpeg)

(mlx::defmlx-extension (:jpeg "JPEG" "JPG")
  ((:load-path path)
   "Load JPEG image. "
   (multiple-value-bind (buffer h w c)
       (jpeg:decode-image path)
     (mlx:reshape (mlx:mlx-array buffer) (list h w c))))
  ((:save-path img pathname)
   "Write JPEG image. "
   (assert (or (mlx:dim= img 2)
               (mlx:dim= img 3)))
   (jpeg:encode-image pathname
                      (mlx:lisp<- img)
                      (if (mlx:dim= img 3)
                          (mlx:shape img :axis 2)
                          1)
                      (mlx:shape img :axis 0)
                      (mlx:shape img :axis 1))))

;;;; io.lisp
