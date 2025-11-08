;;;; io.lisp --- PNG I/O for mlx-array

(in-package :mlx-cl.io.tiff)

(mlx::defmlx-extension (:tiff "TIF" "TIFF")
  ((:load stream)
   "Load TIFF image. "
   (let* ((img   (tiff:read-tiff-stream stream))
          (w     (tiff:tiff-image-width img))
          (h     (tiff:tiff-image-length img))
          (d     (tiff:tiff-image-bits-per-sample img))
          (c     (tiff:tiff-image-samples-per-pixel img))
          (dtype (mlx::mlx-dtype<-bits (if (arrayp d) (aref d 0) d)))
          (dat   (mlx:mlx-array (tiff:tiff-image-data img) :dtype dtype)))
     (mlx:reshape dat (list h w c))))
  ((:save img stream)
   "Write TIFF image. "
   (let ((img (make-instance 'tiff:tiff-image
                             :length            (mlx:shape img :axis 0)
                             :width             (mlx:shape img :axis 1)
                             :samples-per-pixel (mlx:shape img :axis 2)
                             :bits-per-sample   (ecase (mlx:dtype img)
                                                  (:uint8  8)
                                                  (:uint16 16)
                                                  (:uint32 32))
                             :data (mlx:lisp<- (mlx:reshape img '(-1))))))
     (tiff:write-tiff-stream stream img))))

;;;; io.lisp ends here
